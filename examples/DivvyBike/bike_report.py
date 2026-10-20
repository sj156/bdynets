"""Reusable Python helpers for the Divvy multi-scale R Markdown report.

The module reads the processed node and sparse hourly OD tables, validates their
keys, prepares descriptive summaries and draws reproducible static figures.
It deliberately does not download map tiles or modify the input CSV files.
"""

from __future__ import annotations

from pathlib import Path
from typing import Dict, Iterable, Mapping, Tuple

import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.path import Path as MplPath
from matplotlib.patches import FancyArrowPatch
import numpy as np
import pandas as pd


SCALE_LABELS = {
    "raw": "Raw stations",
    "r500": "500 m grid",
    "r1000": "1,000 m grid",
}

NODE_COLUMNS = {
    "node_idx",
    "node_id",
    "latitude",
    "longitude",
    "n_raw_stations",
    "display_name",
    "coordinate_available",
}
FLOW_COLUMNS = {
    "timestamp",
    "origin_node_idx",
    "destination_node_idx",
    "trip_count",
}

# The same central-Chicago extent used by the existing interactive prototype.
# Coordinates are west, east, south, north.
CENTRAL_CHICAGO_BOUNDS = (-87.730, -87.595, 41.800, 41.950)
CENTRAL_CHICAGO_POLYGON = (
    (41.945, -87.705),
    (41.950, -87.630),
    (41.920, -87.605),
    (41.860, -87.595),
    (41.805, -87.615),
    (41.800, -87.700),
    (41.830, -87.720),
    (41.890, -87.730),
    (41.925, -87.720),
)

FLOW_CLASSES = (
    (1, "1", "#64a9d9"),
    (3, "2–3", "#9fc37d"),
    (5, "4–5", "#edc45d"),
    (7, "6–7", "#f49a51"),
    (np.inf, "≥ 8", "#eb6547"),
)


def _require_columns(frame: pd.DataFrame, required: set[str], label: str) -> None:
    missing = sorted(required.difference(frame.columns))
    if missing:
        raise ValueError(f"{label} is missing columns: {', '.join(missing)}")


def _flow_file(data_dir: Path, scale: str) -> Path:
    matches = sorted(data_dir.glob(f"od_flow_*_{scale}.csv"))
    if len(matches) != 1:
        raise FileNotFoundError(
            f"Expected exactly one OD file for {scale} in {data_dir}; found {len(matches)}"
        )
    return matches[0]


def read_divvy_scale(
    data_dir: str | Path,
    scale: str,
    timezone: str = "America/Chicago",
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Read one node/OD pair and attach a timezone to source-local timestamps."""
    data_dir = Path(data_dir)
    if scale not in SCALE_LABELS:
        raise ValueError(f"Unsupported scale {scale!r}; choose from {list(SCALE_LABELS)}")

    node_path = data_dir / f"nodes_{scale}.csv"
    flow_path = _flow_file(data_dir, scale)
    if not node_path.exists():
        raise FileNotFoundError(node_path)

    nodes = pd.read_csv(node_path)
    flows = pd.read_csv(flow_path)
    _require_columns(nodes, NODE_COLUMNS, node_path.name)
    _require_columns(flows, FLOW_COLUMNS, flow_path.name)

    nodes = nodes.copy()
    flows = flows.copy()
    nodes["node_idx"] = pd.to_numeric(nodes["node_idx"], errors="raise").astype("int64")
    nodes["latitude"] = pd.to_numeric(nodes["latitude"], errors="coerce")
    nodes["longitude"] = pd.to_numeric(nodes["longitude"], errors="coerce")
    nodes["n_raw_stations"] = pd.to_numeric(
        nodes["n_raw_stations"], errors="coerce"
    ).fillna(0).astype("int64")
    nodes["coordinate_available"] = (
        nodes["coordinate_available"].astype(str).str.lower().eq("true")
    )

    parsed_time = pd.to_datetime(flows["timestamp"], errors="coerce")
    if parsed_time.isna().any():
        raise ValueError(f"{flow_path.name} contains invalid timestamps")
    if parsed_time.dt.tz is None:
        parsed_time = parsed_time.dt.tz_localize(
            timezone, ambiguous="raise", nonexistent="raise"
        )
    else:
        parsed_time = parsed_time.dt.tz_convert(timezone)
    flows["timestamp"] = parsed_time
    flows["origin_node_idx"] = pd.to_numeric(
        flows["origin_node_idx"], errors="raise"
    ).astype("int64")
    flows["destination_node_idx"] = pd.to_numeric(
        flows["destination_node_idx"], errors="raise"
    ).astype("int64")
    flows["trip_count"] = pd.to_numeric(flows["trip_count"], errors="raise")

    validate_divvy_data(nodes, flows, scale)
    nodes.attrs.update(scale=scale, source_file=node_path.name)
    flows.attrs.update(scale=scale, source_file=flow_path.name, timezone=timezone)
    return nodes, flows


def validate_divvy_data(
    nodes: pd.DataFrame,
    flows: pd.DataFrame,
    scale: str = "dataset",
) -> Mapping[str, int]:
    """Validate identities, coordinates, sparse keys and count measurements."""
    duplicate_nodes = int(nodes["node_idx"].duplicated().sum())
    if duplicate_nodes:
        raise ValueError(f"{scale}: duplicate node_idx values: {duplicate_nodes}")

    valid_ids = set(nodes["node_idx"].tolist())
    unknown_origin = int((~flows["origin_node_idx"].isin(valid_ids)).sum())
    unknown_destination = int((~flows["destination_node_idx"].isin(valid_ids)).sum())
    if unknown_origin or unknown_destination:
        raise ValueError(
            f"{scale}: OD table has unknown endpoints "
            f"(origin={unknown_origin}, destination={unknown_destination})"
        )

    duplicate_keys = int(
        flows.duplicated(
            ["timestamp", "origin_node_idx", "destination_node_idx"]
        ).sum()
    )
    if duplicate_keys:
        raise ValueError(f"{scale}: duplicate timestamp/OD keys: {duplicate_keys}")

    counts = flows["trip_count"]
    if counts.isna().any() or (counts < 0).any() or not np.allclose(counts, np.round(counts)):
        raise ValueError(f"{scale}: trip_count must contain nonnegative integers")

    bad_coordinates = int(
        (
            nodes["coordinate_available"]
            & (
                nodes["latitude"].isna()
                | nodes["longitude"].isna()
                | ~nodes["latitude"].between(-90, 90)
                | ~nodes["longitude"].between(-180, 180)
            )
        ).sum()
    )
    if bad_coordinates:
        raise ValueError(f"{scale}: invalid available coordinates: {bad_coordinates}")

    return {
        "duplicate_nodes": duplicate_nodes,
        "duplicate_od_time_keys": duplicate_keys,
        "unknown_origins": unknown_origin,
        "unknown_destinations": unknown_destination,
        "invalid_available_coordinates": bad_coordinates,
    }


def load_report_scales(
    data_dir: str | Path,
    scales: Iterable[str] = ("raw", "r500", "r1000"),
    timezone: str = "America/Chicago",
) -> Dict[str, Tuple[pd.DataFrame, pd.DataFrame]]:
    """Load all scales used by the report."""
    return {
        scale: read_divvy_scale(data_dir, scale, timezone=timezone)
        for scale in scales
    }


def summarize_divvy_scale(
    scale: str,
    nodes: pd.DataFrame,
    flows: pd.DataFrame,
) -> dict:
    """Return one descriptive summary row for a scale."""
    loop = flows["origin_node_idx"].eq(flows["destination_node_idx"])
    nonloop_pairs = flows.loc[~loop, ["origin_node_idx", "destination_node_idx"]].drop_duplicates()
    n_nodes = len(nodes)
    possible_edges = n_nodes * max(n_nodes - 1, 0)
    total_trips = float(flows["trip_count"].sum())
    loop_trips = float(flows.loc[loop, "trip_count"].sum())
    timestamps = flows["timestamp"].drop_duplicates().sort_values()

    return {
        "scale": scale,
        "label": SCALE_LABELS[scale],
        "nodes": n_nodes,
        "nodes_with_coordinates": int(nodes["coordinate_available"].sum()),
        "source_stations_represented": int(nodes["n_raw_stations"].sum()),
        "hourly_bins": int(len(timestamps)),
        "active_directed_edges": int(len(nonloop_pairs)),
        "network_density_pct": 100 * len(nonloop_pairs) / possible_edges if possible_edges else np.nan,
        "total_trips": int(total_trips),
        "within_node_trips": int(loop_trips),
        "within_node_share_pct": 100 * loop_trips / total_trips if total_trips else 0.0,
        "mean_trips_per_observed_od_hour": float(flows["trip_count"].mean()),
        "median_trips_per_observed_od_hour": float(flows["trip_count"].median()),
        "single_trip_row_share_pct": float(100 * flows["trip_count"].eq(1).mean()),
        "first_time": timestamps.iloc[0] if len(timestamps) else pd.NaT,
        "last_time": timestamps.iloc[-1] if len(timestamps) else pd.NaT,
    }


def compare_scales(
    datasets: Mapping[str, Tuple[pd.DataFrame, pd.DataFrame]]
) -> pd.DataFrame:
    """Build the raw/500 m/1,000 m comparison table."""
    rows = [summarize_divvy_scale(scale, *datasets[scale]) for scale in datasets]
    return pd.DataFrame(rows)


def validation_table(
    datasets: Mapping[str, Tuple[pd.DataFrame, pd.DataFrame]]
) -> pd.DataFrame:
    """Return compact validation evidence for the report."""
    rows = []
    for scale, (nodes, flows) in datasets.items():
        result = dict(validate_divvy_data(nodes, flows, scale))
        result.update(
            scale=SCALE_LABELS[scale],
            flow_rows=len(flows),
            zero_trip_rows=int(flows["trip_count"].eq(0).sum()),
            missing_trip_counts=int(flows["trip_count"].isna().sum()),
        )
        rows.append(result)
    return pd.DataFrame(rows)


def _scale_colors(scales: Iterable[str]) -> dict[str, str]:
    palette = {"raw": "#4477AA", "r500": "#EE8844", "r1000": "#228833"}
    return {scale: palette.get(scale, "#667085") for scale in scales}


def plot_scale_comparison(summary: pd.DataFrame) -> plt.Figure:
    """Compare network compression and observed OD-hour concentration."""
    colors = [_scale_colors(summary["scale"])[s] for s in summary["scale"]]
    fig, axes = plt.subplots(1, 3, figsize=(14, 4.2))
    specs = (
        ("nodes", "Network nodes", "Count", "{:,.0f}"),
        ("active_directed_edges", "Distinct active directed OD pairs", "Count", "{:,.0f}"),
        (
            "mean_trips_per_observed_od_hour",
            "Mean trips per observed OD-hour",
            "Trips",
            "{:.2f}",
        ),
    )
    x = np.arange(len(summary))
    for ax, (column, title, ylabel, formatter) in zip(axes, specs):
        values = summary[column].to_numpy(dtype=float)
        bars = ax.bar(x, values, color=colors, width=0.66)
        ax.set_title(title, loc="left", fontsize=11, weight="bold")
        ax.set_ylabel(ylabel)
        ax.set_xticks(x, summary["label"], rotation=16, ha="right")
        ax.grid(axis="y", color="#dfe5ec", linewidth=0.8)
        ax.set_axisbelow(True)
        for bar, value in zip(bars, values):
            ax.text(
                bar.get_x() + bar.get_width() / 2,
                bar.get_height(),
                formatter.format(value),
                ha="center",
                va="bottom",
                fontsize=8.5,
                color="#243447",
            )
    fig.suptitle(
        "Spatial aggregation compresses the network and concentrates observed flows",
        x=0.02,
        ha="left",
        fontsize=14,
        weight="bold",
        color="#172033",
    )
    fig.tight_layout(rect=(0, 0, 1, 0.91))
    return fig


def plot_edge_weight_distributions(
    datasets: Mapping[str, Tuple[pd.DataFrame, pd.DataFrame]]
) -> plt.Figure:
    """Plot empirical distributions of positive sparse OD-hour counts."""
    fig, ax = plt.subplots(figsize=(10.5, 5.2))
    colors = _scale_colors(datasets)
    for scale, (_, flows) in datasets.items():
        values = np.sort(flows.loc[flows["trip_count"] > 0, "trip_count"].to_numpy(dtype=float))
        probability = np.arange(1, len(values) + 1) / len(values)
        ax.step(
            values,
            probability,
            where="post",
            label=SCALE_LABELS[scale],
            color=colors[scale],
            linewidth=2,
        )
    ax.set_xscale("log")
    ax.set_xlabel("Trips in an observed OD-hour (log scale)")
    ax.set_ylabel("Empirical cumulative probability")
    ax.set_title("OD-hour trip-count distributions", loc="left", fontsize=13, weight="bold")
    ax.grid(True, which="both", color="#dfe5ec", linewidth=0.75)
    ax.legend(frameon=False, ncol=3, loc="lower right")
    fig.tight_layout()
    return fig


def hourly_profile(
    datasets: Mapping[str, Tuple[pd.DataFrame, pd.DataFrame]]
) -> pd.DataFrame:
    """Calculate average daily hourly totals and concentration by scale."""
    rows = []
    for scale, (_, flows) in datasets.items():
        hourly = (
            flows.groupby("timestamp", as_index=False)
            .agg(total_trips=("trip_count", "sum"), active_od_rows=("trip_count", "size"))
            .assign(
                hour=lambda x: x["timestamp"].dt.hour,
                mean_trips_per_active_od=lambda x: x["total_trips"] / x["active_od_rows"],
            )
        )
        profile = hourly.groupby("hour", as_index=False).agg(
            mean_total_trips=("total_trips", "mean"),
            mean_active_od_rows=("active_od_rows", "mean"),
            mean_trips_per_active_od=("mean_trips_per_active_od", "mean"),
        )
        profile["scale"] = scale
        rows.append(profile)
    return pd.concat(rows, ignore_index=True)


def plot_hourly_profiles(profile: pd.DataFrame) -> plt.Figure:
    """Draw average hour-of-day patterns for volume and OD concentration."""
    fig, axes = plt.subplots(1, 2, figsize=(13.5, 4.5), sharex=True)
    colors = _scale_colors(profile["scale"].unique())
    for scale, frame in profile.groupby("scale", sort=False):
        axes[0].plot(
            frame["hour"],
            frame["mean_total_trips"],
            marker="o",
            markersize=3.5,
            linewidth=2,
            color=colors[scale],
            label=SCALE_LABELS[scale],
        )
        axes[1].plot(
            frame["hour"],
            frame["mean_trips_per_active_od"],
            marker="o",
            markersize=3.5,
            linewidth=2,
            color=colors[scale],
            label=SCALE_LABELS[scale],
        )
    axes[0].set_title("Average total trips by hour", loc="left", weight="bold")
    axes[0].set_ylabel("Trips")
    axes[1].set_title("Average flow per observed OD by hour", loc="left", weight="bold")
    axes[1].set_ylabel("Trips per observed OD")
    for ax in axes:
        ax.set_xlabel("Hour in America/Chicago")
        ax.set_xticks(range(0, 24, 3))
        ax.grid(True, color="#dfe5ec", linewidth=0.75)
        ax.legend(frameon=False)
    fig.tight_layout()
    return fig


def _flow_style(value: float) -> tuple[str, str, float, str]:
    for upper, label, color in FLOW_CLASSES:
        if value <= upper:
            width = 0.8 if value <= 1 else min(3.1, 0.9 + 0.28 * value)
            linestyle = "--" if value <= 1 else "-"
            return label, color, width, linestyle
    raise AssertionError("unreachable")


def prepare_map_frame(
    nodes: pd.DataFrame,
    flows: pd.DataFrame,
    start_time: str | pd.Timestamp,
    end_time: str | pd.Timestamp,
    bounds: tuple[float, float, float, float] = CENTRAL_CHICAGO_BOUNDS,
) -> tuple[pd.DataFrame, pd.DataFrame, dict]:
    """Aggregate one half-open local-time window and join endpoint coordinates."""
    timezone = str(flows["timestamp"].dt.tz)
    start = pd.Timestamp(start_time)
    end = pd.Timestamp(end_time)
    if start.tzinfo is None:
        start = start.tz_localize(timezone)
    else:
        start = start.tz_convert(timezone)
    if end.tzinfo is None:
        end = end.tz_localize(timezone)
    else:
        end = end.tz_convert(timezone)
    if end <= start:
        raise ValueError("end_time must be after start_time")

    west, east, south, north = bounds
    coordinate_nodes = nodes.loc[
        nodes["coordinate_available"]
        & nodes["longitude"].between(west, east)
        & nodes["latitude"].between(south, north)
    ].copy()
    polygon_xy = np.array([(lng, lat) for lat, lng in CENTRAL_CHICAGO_POLYGON])
    inside = MplPath(polygon_xy).contains_points(
        coordinate_nodes[["longitude", "latitude"]].to_numpy()
    )
    map_nodes = coordinate_nodes.loc[inside].copy()
    keep_ids = set(map_nodes["node_idx"])
    window = flows.loc[
        flows["timestamp"].ge(start)
        & flows["timestamp"].lt(end)
        & flows["origin_node_idx"].isin(keep_ids)
        & flows["destination_node_idx"].isin(keep_ids)
    ].copy()
    loop_trips = int(
        window.loc[
            window["origin_node_idx"].eq(window["destination_node_idx"]), "trip_count"
        ].sum()
    )
    routes = (
        window.loc[window["origin_node_idx"].ne(window["destination_node_idx"])]
        .groupby(["origin_node_idx", "destination_node_idx"], as_index=False)["trip_count"]
        .sum()
    )

    endpoint = map_nodes.set_index("node_idx")[["longitude", "latitude", "display_name"]]
    routes = routes.join(endpoint.add_prefix("origin_"), on="origin_node_idx")
    routes = routes.join(endpoint.add_prefix("destination_"), on="destination_node_idx")
    routes = routes.sort_values("trip_count", ascending=False).reset_index(drop=True)
    metadata = {
        "start": start,
        "end": end,
        "total_trips_in_bounds": int(window["trip_count"].sum()),
        "within_node_trips": loop_trips,
        "active_nonloop_routes": int(len(routes)),
    }
    return map_nodes, routes, metadata


def plot_map_comparison(
    datasets: Mapping[str, Tuple[pd.DataFrame, pd.DataFrame]],
    start_time: str,
    end_time: str,
    max_routes: int = 180,
    bounds: tuple[float, float, float, float] = CENTRAL_CHICAGO_BOUNDS,
) -> plt.Figure:
    """Compare directed central-Chicago flows at three spatial resolutions."""
    scales = list(datasets)
    fig, axes = plt.subplots(len(scales), 1, figsize=(15, 19), squeeze=False)
    west, east, south, north = bounds

    for ax, scale in zip(axes[:, 0], scales):
        nodes, flows = datasets[scale]
        map_nodes, all_routes, meta = prepare_map_frame(
            nodes, flows, start_time, end_time, bounds=bounds
        )
        routes = all_routes.head(max_routes)

        ax.set_facecolor("#f2f4f6")
        # A restrained coordinate grid provides geographic context without
        # requiring remote tiles, so the report remains self-contained.
        ax.grid(True, color="#d8dde3", linewidth=0.55, zorder=0)
        outline = np.vstack([polygon_xy := np.array([(lng, lat) for lat, lng in CENTRAL_CHICAGO_POLYGON]), polygon_xy[0]])
        ax.plot(
            outline[:, 0],
            outline[:, 1],
            color="#9aa8b5",
            linewidth=1,
            linestyle=(0, (4, 4)),
            zorder=1,
        )
        ax.scatter(
            map_nodes["longitude"],
            map_nodes["latitude"],
            s=9 if scale == "raw" else 18,
            facecolor="#ffffff",
            edgecolor="#314d68",
            linewidth=0.65,
            alpha=0.9,
            zorder=4,
        )

        for route in routes.itertuples(index=False):
            _, color, width, linestyle = _flow_style(float(route.trip_count))
            pair_direction = 1 if route.origin_node_idx < route.destination_node_idx else -1
            rad = 0.035 * pair_direction
            arrow = FancyArrowPatch(
                (route.origin_longitude, route.origin_latitude),
                (route.destination_longitude, route.destination_latitude),
                arrowstyle="-|>",
                mutation_scale=6.5 + min(float(route.trip_count), 12) * 0.22,
                linewidth=width,
                linestyle=linestyle,
                color=color,
                alpha=0.76,
                connectionstyle=f"arc3,rad={rad}",
                shrinkA=2.2,
                shrinkB=2.2,
                zorder=2,
            )
            ax.add_patch(arrow)

        ax.set_xlim(west, east)
        ax.set_ylim(south, north)
        # A landscape panel keeps individual routes legible in the HTML report.
        ax.set_aspect("auto")
        ax.set_title(SCALE_LABELS[scale], loc="left", fontsize=13, weight="bold")
        ax.text(
            0.02,
            0.975,
            (
                f"{meta['active_nonloop_routes']:,} active routes · "
                f"{meta['total_trips_in_bounds']:,} trips\n"
                f"showing top {len(routes):,} routes; self-loops excluded from arrows"
            ),
            transform=ax.transAxes,
            va="top",
            ha="left",
            fontsize=8.5,
            color="#334155",
            bbox=dict(boxstyle="round,pad=0.45", facecolor="white", edgecolor="#d4dae1", alpha=0.94),
            zorder=10,
        )
        ax.tick_params(labelsize=8, colors="#667085")
        ax.set_xlabel("Longitude", fontsize=8.5)
        ax.set_ylabel("Latitude", fontsize=8.5)

    legend_handles = [
        Line2D([0], [0], color=color, lw=2.8, linestyle="--" if label == "1" else "-", label=label)
        for _, label, color in FLOW_CLASSES
    ]
    fig.legend(
        handles=legend_handles,
        title="Directed trips in selected window",
        loc="lower center",
        ncol=5,
        frameon=False,
        bbox_to_anchor=(0.5, 0.012),
    )
    fig.suptitle(
        f"Central Chicago directed OD flows · {start_time} to {end_time}",
        x=0.015,
        y=0.995,
        ha="left",
        fontsize=15,
        weight="bold",
        color="#172033",
    )
    fig.text(
        0.985,
        0.012,
        "Straight/curved display links connect aggregate endpoints; they are not observed bicycle routes.",
        ha="right",
        fontsize=8.5,
        color="#667085",
    )
    fig.tight_layout(rect=(0, 0.045, 1, 0.965), h_pad=2.2)
    return fig


def format_summary_for_report(summary: pd.DataFrame) -> pd.DataFrame:
    """Select and format columns for an HTML report table."""
    out = summary[
        [
            "label",
            "nodes",
            "active_directed_edges",
            "total_trips",
            "within_node_share_pct",
            "mean_trips_per_observed_od_hour",
            "single_trip_row_share_pct",
        ]
    ].copy()
    out.columns = [
        "Scale",
        "Nodes",
        "Active directed OD pairs",
        "Total trips",
        "Within-node trips (%)",
        "Mean trips / observed OD-hour",
        "Rows with one trip (%)",
    ]
    out["Nodes"] = out["Nodes"].map("{:,.0f}".format)
    out["Active directed OD pairs"] = out["Active directed OD pairs"].map("{:,.0f}".format)
    out["Total trips"] = out["Total trips"].map("{:,.0f}".format)
    for column in (
        "Within-node trips (%)",
        "Mean trips / observed OD-hour",
        "Rows with one trip (%)",
    ):
        out[column] = out[column].map("{:.2f}".format)
    return out


def dataframe_html(frame: pd.DataFrame, index: bool = False) -> str:
    """Return compact accessible table markup for an as-is R Markdown chunk."""
    return frame.to_html(
        index=index,
        border=0,
        classes=["table", "table-striped", "table-hover", "report-table"],
        justify="left",
        escape=True,
    )

