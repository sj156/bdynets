from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import mean_absolute_error, r2_score
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler


BASE_DIR = Path(__file__).resolve().parent
PROCESSED_DIR = BASE_DIR / "processed_real_network"
FLOW_FILE = PROCESSED_DIR / "edge_flow_xijt_nonzero.csv"
LINK_FILE = PROCESSED_DIR / "links_clean.csv"
OUT_FILE = PROCESSED_DIR / "edge_xijt_capacity_estimates.csv"
SUMMARY_FILE = PROCESSED_DIR / "edge_xijt_capacity_estimates_summary.txt"


def make_one_hot_encoder() -> OneHotEncoder:
    try:
        return OneHotEncoder(handle_unknown="ignore", sparse_output=False)
    except TypeError:
        return OneHotEncoder(handle_unknown="ignore", sparse=False)


def main() -> None:
    links = pd.read_csv(LINK_FILE)
    flow = pd.read_csv(FLOW_FILE, usecols=["time_bin", "edge_index", "x_ijt"])

    edge_stats = (
        flow.groupby("edge_index")["x_ijt"]
        .agg(
            observed_mean_xijt="mean",
            observed_p50_xijt="median",
            observed_p90_xijt=lambda s: s.quantile(0.90),
            observed_p95_xijt=lambda s: s.quantile(0.95),
            observed_p99_xijt=lambda s: s.quantile(0.99),
            observed_max_xijt="max",
            active_bins="count",
        )
        .reset_index()
    )

    data = links.merge(edge_stats, on="edge_index", how="left")
    data["active_bins"] = data["active_bins"].fillna(0).astype(int)
    data["observed_max_xijt"] = data["observed_max_xijt"].fillna(0.0)

    length_cap = data["length"].replace([np.inf, -np.inf], np.nan).quantile(0.995)
    data["length_for_model"] = data["length"].clip(lower=0.005, upper=length_cap)
    data["storage_capacity_veh"] = (
        data["length_for_model"].astype(float)
        * data["number_of_lanes"].astype(float).clip(lower=1.0)
        * data["jam_density"].astype(float).clip(lower=1e-6)
    )
    data["log_length"] = np.log1p(data["length_for_model"].astype(float))
    data["log_lane_capacity"] = np.log1p(data["lane_capacity_in_vhc_per_hour"].astype(float).clip(lower=1.0))
    data["log_storage"] = np.log1p(data["storage_capacity_veh"].astype(float).clip(lower=1e-6))
    data["log_observed_max"] = np.log1p(data["observed_max_xijt"])

    train = data[data["active_bins"] >= 12].copy()
    if len(train) < 100:
        raise ValueError("Not enough active edges to estimate x_ijt capacity.")

    numeric_features = [
        "log_length",
        "number_of_lanes",
        "speed_limit",
        "log_lane_capacity",
        "jam_density",
        "log_storage",
    ]
    categorical_features = ["link_type_name"]

    preprocessor = ColumnTransformer(
        transformers=[
            ("num", StandardScaler(), numeric_features),
            ("cat", make_one_hot_encoder(), categorical_features),
        ]
    )
    model = Pipeline(
        steps=[
            ("preprocess", preprocessor),
            (
                "regressor",
                GradientBoostingRegressor(
                    loss="quantile",
                    alpha=0.90,
                    n_estimators=350,
                    learning_rate=0.035,
                    max_depth=3,
                    random_state=42,
                ),
            ),
        ]
    )

    x_train, x_test, y_train, y_test = train_test_split(
        train[numeric_features + categorical_features],
        train["log_observed_max"],
        test_size=0.2,
        random_state=42,
    )
    model.fit(x_train, y_train)
    pred_test = model.predict(x_test)

    model_capacity = np.expm1(model.predict(data[numeric_features + categorical_features]))
    model_capacity = np.maximum(model_capacity, 1.0)
    observed_floor = data["observed_max_xijt"].fillna(0.0).to_numpy(dtype=float)
    capacity = np.maximum(model_capacity, observed_floor)
    capacity = np.maximum(capacity, 1.0)

    data["model_capacity_xijt"] = np.round(model_capacity, 6)
    data["capacity_xijt"] = np.round(capacity, 6)
    data["capacity_basis"] = np.where(observed_floor >= model_capacity, "observed_max_floor", "regression_upper")
    data["observed_max_to_capacity"] = np.divide(
        observed_floor,
        capacity,
        out=np.zeros_like(observed_floor, dtype=float),
        where=capacity > 0,
    )

    out_cols = [
        "edge_index",
        "edge_id",
        "from_node_id",
        "to_node_id",
        "link_type_name",
        "length",
        "number_of_lanes",
        "speed_limit",
        "lane_capacity_in_vhc_per_hour",
        "jam_density",
        "storage_capacity_veh",
        "active_bins",
        "observed_mean_xijt",
        "observed_p50_xijt",
        "observed_p90_xijt",
        "observed_p95_xijt",
        "observed_p99_xijt",
        "observed_max_xijt",
        "model_capacity_xijt",
        "capacity_xijt",
        "capacity_basis",
        "observed_max_to_capacity",
    ]
    for col in out_cols:
        if col not in data.columns:
            data[col] = np.nan
    data[out_cols].to_csv(OUT_FILE, index=False)

    summary = {
        "training_edges": int(len(train)),
        "all_edges": int(len(data)),
        "target": "log1p(edge-level observed max x_ijt)",
        "model": "GradientBoostingRegressor(loss='quantile', alpha=0.90)",
        "test_log_mae": float(mean_absolute_error(y_test, pred_test)),
        "test_log_r2": float(r2_score(y_test, pred_test)),
        "capacity_definition": "max(regression-predicted x_ijt upper envelope, observed edge max x_ijt, 1)",
        "capacity_basis_counts": data["capacity_basis"].value_counts().to_dict(),
        "capacity_xijt_quantiles": {
            str(q): float(data["capacity_xijt"].quantile(q))
            for q in [0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99]
        },
        "output": str(OUT_FILE),
    }
    SUMMARY_FILE.write_text(json.dumps(summary, ensure_ascii=False, indent=2), encoding="utf-8")
    print(f"capacity_csv={OUT_FILE}")
    print(f"summary={SUMMARY_FILE}")


if __name__ == "__main__":
    main()
