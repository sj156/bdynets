#!/usr/bin/env python3
"""Process one week of D11 Station 5-Minute files into a typical-day summary."""

from __future__ import annotations

import csv
from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
RAW = ROOT / "data" / "raw"
PROCESSED = ROOT / "data" / "processed"

FIVE_MIN_COLUMNS = [
    "timestamp",
    "station",
    "district",
    "freeway",
    "direction_of_travel",
    "lane_type",
    "station_length",
    "samples",
    "percentage_observed",
    "total_flow",
    "average_occupancy",
    "average_speed",
]

for lane in range(1, 9):
    FIVE_MIN_COLUMNS.extend(
        [
            f"lane{lane}_samples",
            f"lane{lane}_flow",
            f"lane{lane}_average_occupancy",
            f"lane{lane}_average_speed",
            f"lane{lane}_observed",
        ]
    )


def weighted_mean(numerator: pd.Series, denominator: pd.Series) -> float:
    den = denominator.sum()
    if den == 0:
        return float("nan")
    return numerator.sum() / den


def main() -> int:
    PROCESSED.mkdir(parents=True, exist_ok=True)
    files = sorted(RAW.glob("d11_text_station_5min_2020_10_*.txt"))
    if not files:
        raise SystemExit("No Station 5-Minute 2020-10 files found in data/raw.")

    metadata = pd.read_csv(PROCESSED / "station_metadata_2020_latest_by_station.csv", dtype={"station": str})
    metadata_stations = set(metadata["station"])

    chunks = []
    file_rows = []
    use_cols = ["timestamp", "station", "samples", "total_flow", "average_speed"]
    for path in files:
        df = pd.read_csv(path, header=None, names=FIVE_MIN_COLUMNS, usecols=use_cols)
        df["station"] = df["station"].astype(str)
        ts = pd.to_datetime(df["timestamp"], format="%m/%d/%Y %H:%M:%S")
        df["date"] = ts.dt.strftime("%Y-%m-%d")
        df["time"] = ts.dt.strftime("%H:%M")
        chunks.append(df[["date", "time", "station", "samples", "total_flow", "average_speed"]])
        file_rows.append(
            {
                "file": path.name,
                "rows": len(df),
                "date": df["date"].iloc[0],
                "stations": df["station"].nunique(),
                "first_timestamp": df["timestamp"].iloc[0],
                "last_timestamp": df["timestamp"].iloc[-1],
            }
        )

    raw = pd.concat(chunks, ignore_index=True)
    raw["samples"] = pd.to_numeric(raw["samples"], errors="coerce").fillna(0)
    raw["total_flow"] = pd.to_numeric(raw["total_flow"], errors="coerce")
    raw["average_speed"] = pd.to_numeric(raw["average_speed"], errors="coerce")
    raw["flow_weight_num"] = raw["total_flow"] * raw["samples"]
    raw["speed_weight_num"] = raw["average_speed"] * raw["samples"]

    grouped = (
        raw.groupby(["station", "time"], as_index=False)
        .agg(
            records=("station", "size"),
            days=("date", "nunique"),
            samples_sum=("samples", "sum"),
            total_flow_count=("total_flow", "count"),
            flow_weight_num=("flow_weight_num", "sum"),
            average_speed_count=("average_speed", "count"),
            speed_weight_num=("speed_weight_num", "sum"),
        )
    )
    grouped["metadata_matched"] = grouped["station"].isin(metadata_stations).map({True: "yes", False: "no"})
    grouped["total_flow_weighted_by_samples"] = grouped["flow_weight_num"] / grouped["samples_sum"]
    grouped["average_speed_weighted_by_samples"] = grouped["speed_weight_num"] / grouped["samples_sum"]
    grouped = grouped.drop(columns=["flow_weight_num", "speed_weight_num"])
    grouped.to_csv(PROCESSED / "station_5min_2020_10_05_10_11_time_of_day_summary.csv", index=False)

    pd.DataFrame(file_rows).to_csv(
        PROCESSED / "station_5min_2020_10_05_10_11_file_summary.csv", index=False
    )

    five_stations = set(raw["station"])
    missing = sorted(five_stations - metadata_stations, key=int)
    (PROCESSED / "station_5min_2020_10_05_10_11_missing_metadata_stations.txt").write_text(
        "\n".join(missing) + ("\n" if missing else "")
    )

    with (PROCESSED / "station_5min_2020_10_05_10_11_metadata_coverage.csv").open("w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["metric", "value"])
        writer.writerow(["five_min_stations", len(five_stations)])
        writer.writerow(["metadata_stations", len(metadata_stations)])
        writer.writerow(["five_min_matched_metadata", len(five_stations & metadata_stations)])
        writer.writerow(["five_min_missing_metadata", len(missing)])

    print(f"Station 5-Minute files: {len(files)}")
    print(f"Raw rows: {len(raw)}")
    print(f"Summary rows: {len(grouped)}")
    print("Wrote Station 5-Minute processed outputs.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
