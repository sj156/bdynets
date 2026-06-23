#!/usr/bin/env python3
"""Build an OSM-based dynamic congestion sample for city-traffic-M.

This script keeps the original large speed file remote. It downloads only the
Parquet footer plus the speed column chunks needed for the selected road
segments, then writes a compact JSON/JS payload for an interactive Leaflet map.
"""

from __future__ import annotations

import json
import math
import os
import struct
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import requests


ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = ROOT / "data" / "city-traffic-benchmarks"
OUT_DIR = ROOT / "visualizations"

STATIC_PATH = DATA_DIR / "city-traffic-M-raw-static-features.parquet"
VOLUME_PATH = DATA_DIR / "city-traffic-M-raw-volume.parquet"

SPEED_FILE_NAME = "city-traffic-M-raw-speed.parquet"
SPEED_REMOTE_URL = (
    "https://www.kaggle.com/api/v1/datasets/download/"
    f"mightyneghbor/city-traffic-benchmarks/{SPEED_FILE_NAME}?datasetVersionNumber=4"
)
SPEED_FILE_SIZE = 11_026_339_676

JSON_OUT = OUT_DIR / "city_traffic_M_osm_congestion_sample.json"
JS_OUT = OUT_DIR / "city_traffic_M_osm_congestion_sample.js"
HTML_OUT = OUT_DIR / "city_traffic_M_osm_congestion_sample.html"

TARGET_DYNAMIC_SEGMENTS = 720
GRID_X = 34
GRID_Y = 22
FRAME_FREQ = "15min"
MERGE_GAP_BYTES = 65_536
MAX_RANGE_WORKERS = 4
RANGE_RETRIES = 5

SPEED_LIMIT_LABELS = {
    0: "未知",
    1: "5 km/h",
    2: "20 km/h",
    3: "30 km/h",
    4: "40 km/h",
    5: "50 km/h",
    6: "60 km/h",
    7: "70 km/h",
    8: "80 km/h",
    9: "90 km/h",
    10: "100 km/h",
    11: "110 km/h",
}


def require_sources() -> None:
    missing = [str(path) for path in [STATIC_PATH, VOLUME_PATH] if not path.exists()]
    if missing:
        raise FileNotFoundError("Missing required data file(s): " + ", ".join(missing))


def load_static_features() -> pd.DataFrame:
    static = pd.read_parquet(STATIC_PATH).reset_index(names="node_id")
    coord_cols = [
        "x_coordinate_start",
        "y_coordinate_start",
        "x_coordinate_end",
        "y_coordinate_end",
    ]
    static = static.dropna(subset=coord_cols).copy()
    static["x_mid"] = (static["x_coordinate_start"] + static["x_coordinate_end"]) / 2
    static["y_mid"] = (static["y_coordinate_start"] + static["y_coordinate_end"]) / 2
    return static


def load_volume_column_stats() -> pd.DataFrame:
    parquet = pq.ParquetFile(VOLUME_PATH)
    row_group = parquet.metadata.row_group(0)
    rows: list[dict[str, float | int | str]] = []

    for column_index, column_name in enumerate(parquet.schema.names):
        if column_name == "timestamp":
            continue
        stats = row_group.column(column_index).statistics
        if stats is None:
            continue
        max_volume = float(stats.max)
        if max_volume <= 0:
            continue
        rows.append(
            {
                "node_id": int(column_name.removeprefix("node_")),
                "volume_column": column_name,
                "volume_min": float(stats.min),
                "volume_max": max_volume,
                "volume_nulls": int(stats.null_count),
            }
        )

    return pd.DataFrame(rows)


def select_dynamic_segments(static: pd.DataFrame, stats: pd.DataFrame) -> pd.DataFrame:
    candidates = static.merge(stats, on="node_id", how="inner")
    x_span = candidates["x_mid"].max() - candidates["x_mid"].min()
    y_span = candidates["y_mid"].max() - candidates["y_mid"].min()
    candidates["grid_x"] = np.floor(
        (candidates["x_mid"] - candidates["x_mid"].min()) / x_span * GRID_X
    ).clip(0, GRID_X - 1)
    candidates["grid_y"] = np.floor(
        (candidates["y_mid"] - candidates["y_mid"].min()) / y_span * GRID_Y
    ).clip(0, GRID_Y - 1)

    candidates = candidates.sort_values("volume_max", ascending=False)
    diverse = (
        candidates.groupby(["grid_x", "grid_y"], as_index=False, sort=False)
        .head(1)
        .sort_values("volume_max", ascending=False)
    )
    selected_ids = set(diverse.head(TARGET_DYNAMIC_SEGMENTS)["node_id"].astype(int))

    if len(selected_ids) < TARGET_DYNAMIC_SEGMENTS:
        for node_id in candidates["node_id"].astype(int):
            selected_ids.add(node_id)
            if len(selected_ids) >= TARGET_DYNAMIC_SEGMENTS:
                break

    selected = candidates[candidates["node_id"].isin(selected_ids)].copy()
    selected = selected.sort_values(["volume_max", "node_id"], ascending=[False, True])
    selected["speed_column"] = "node_" + selected["node_id"].astype(str)
    return selected.head(TARGET_DYNAMIC_SEGMENTS).reset_index(drop=True)


def read_volume_timeseries(selected: pd.DataFrame) -> pd.DataFrame:
    columns = selected["volume_column"].tolist() + ["timestamp"]
    table = pq.read_table(VOLUME_PATH, columns=columns)
    frame = table.to_pandas()
    if not isinstance(frame.index, pd.DatetimeIndex):
        frame = frame.set_index("timestamp")
    frame.index = pd.to_datetime(frame.index)
    return frame.sort_index()[selected["volume_column"].tolist()]


def choose_sample_day(volume: pd.DataFrame) -> pd.Timestamp:
    complete_counts = volume.resample("D").size()
    complete_days = complete_counts[complete_counts >= 288].index
    daily_total = volume.resample("D").sum(min_count=1).sum(axis=1).loc[complete_days]
    if daily_total.empty:
        raise ValueError("No complete 24-hour day found in sampled volume data.")
    return pd.Timestamp(daily_total.idxmax()).normalize()


def frame_timeseries(frame: pd.DataFrame, sample_day: pd.Timestamp) -> pd.DataFrame:
    day_end = sample_day + pd.Timedelta(days=1)
    day = frame.loc[(frame.index >= sample_day) & (frame.index < day_end)]
    frames = day.resample(FRAME_FREQ).mean().interpolate(limit_direction="both")
    return frames.fillna(0.0)


def get_signed_speed_url() -> str:
    response = requests.get(SPEED_REMOTE_URL, allow_redirects=False, timeout=30)
    response.raise_for_status()
    signed_url = response.headers.get("Location")
    if not signed_url:
        raise RuntimeError("Kaggle did not return a signed speed-file URL.")
    return signed_url


def download_speed_footer(signed_url: str, cache_dir: Path) -> tuple[bytes, int]:
    cache_dir.mkdir(parents=True, exist_ok=True)
    footer_tail_path = cache_dir / "city-traffic-M-raw-speed.footer.bin"

    if footer_tail_path.exists():
        footer_tail = footer_tail_path.read_bytes()
        footer_len = struct.unpack("<i", footer_tail[-8:-4])[0]
        if len(footer_tail) == footer_len + 8 and footer_tail[-4:] == b"PAR1":
            return footer_tail, SPEED_FILE_SIZE - footer_len - 8

    tail = range_get(signed_url, SPEED_FILE_SIZE - 8, SPEED_FILE_SIZE - 1)
    footer_len = struct.unpack("<i", tail[:4])[0]
    footer_start = SPEED_FILE_SIZE - footer_len - 8
    footer_tail = range_get(signed_url, footer_start, SPEED_FILE_SIZE - 1)
    footer_tail_path.write_bytes(footer_tail)
    return footer_tail, footer_start


def range_get(url: str, start: int, end: int) -> bytes:
    headers = {"Range": f"bytes={start}-{end}"}
    last_error: Exception | None = None
    expected = end - start + 1
    for attempt in range(RANGE_RETRIES):
        try:
            response = requests.get(url, headers=headers, timeout=120)
            if response.status_code not in (200, 206):
                raise RuntimeError(f"Range request failed: {response.status_code} {start}-{end}")
            if len(response.content) != expected:
                raise RuntimeError(
                    f"Range request returned {len(response.content)} bytes, expected {expected}"
                )
            return response.content
        except Exception as error:  # Network transport can be flaky for many signed range requests.
            last_error = error
            time.sleep(min(2 ** attempt, 12))
    raise RuntimeError(f"Range request failed after retries: {start}-{end}") from last_error


def make_sparse_metadata_file(footer_tail: bytes, footer_start: int, path: Path) -> None:
    with path.open("wb") as file:
        file.write(b"PAR1")
        file.seek(footer_start)
        file.write(footer_tail)
        file.truncate(SPEED_FILE_SIZE)


def needed_column_ranges(metadata_path: Path, columns: list[str]) -> list[tuple[int, int]]:
    parquet = pq.ParquetFile(metadata_path)
    row_group = parquet.metadata.row_group(0)
    ranges = []
    for column in columns:
        column_index = parquet.schema.names.index(column)
        chunk = row_group.column(column_index)
        offsets = [
            offset
            for offset in [chunk.dictionary_page_offset, chunk.data_page_offset]
            if offset is not None and offset > 0
        ]
        start = min(offsets)
        end = start + chunk.total_compressed_size - 1
        ranges.append((start, end))
    return ranges


def merge_ranges(ranges: list[tuple[int, int]]) -> list[tuple[int, int]]:
    merged: list[list[int]] = []
    for start, end in sorted(ranges):
        if not merged or start - merged[-1][1] - 1 > MERGE_GAP_BYTES:
            merged.append([start, end])
        else:
            merged[-1][1] = max(merged[-1][1], end)
    return [(start, end) for start, end in merged]


def write_ranges_to_sparse_file(
    signed_url: str,
    ranges: list[tuple[int, int]],
    footer_tail: bytes,
    footer_start: int,
    out_path: Path,
) -> None:
    merged = merge_ranges(ranges)
    print(
        f"Downloading {len(merged)} speed byte ranges "
        f"({sum(end - start + 1 for start, end in merged) / 1_000_000:.1f} MB)."
    )

    with out_path.open("wb") as file:
        file.write(b"PAR1")

    def fetch(item: tuple[int, int]) -> tuple[int, bytes]:
        start, end = item
        return start, range_get(signed_url, start, end)

    completed = 0
    with ThreadPoolExecutor(max_workers=MAX_RANGE_WORKERS) as executor:
        futures = [executor.submit(fetch, item) for item in merged]
        with out_path.open("r+b") as file:
            for future in as_completed(futures):
                start, content = future.result()
                file.seek(start)
                file.write(content)
                completed += 1
                if completed % 50 == 0 or completed == len(merged):
                    print(f"  speed ranges {completed}/{len(merged)}")

            file.seek(footer_start)
            file.write(footer_tail)
            file.truncate(SPEED_FILE_SIZE)


def read_speed_subset(selected: pd.DataFrame) -> pd.DataFrame:
    signed_url = get_signed_speed_url()
    cache_dir = DATA_DIR / "_cache"
    footer_tail, footer_start = download_speed_footer(signed_url, cache_dir)

    with tempfile.TemporaryDirectory(prefix="city_traffic_speed_") as temp_dir:
        temp_path = Path(temp_dir)
        metadata_path = temp_path / "speed_meta.parquet"
        sparse_path = temp_path / "speed_subset.parquet"
        make_sparse_metadata_file(footer_tail, footer_start, metadata_path)

        columns = selected["speed_column"].tolist() + ["timestamp"]
        ranges = needed_column_ranges(metadata_path, columns)
        write_ranges_to_sparse_file(signed_url, ranges, footer_tail, footer_start, sparse_path)

        speed = pq.read_table(sparse_path, columns=columns).to_pandas()
        if not isinstance(speed.index, pd.DatetimeIndex):
            speed = speed.set_index("timestamp")
        speed.index = pd.to_datetime(speed.index)
        return speed.sort_index()[selected["speed_column"].tolist()]


def round_or_none(value: float | int | bool | None, digits: int = 4):
    if value is None:
        return None
    if isinstance(value, (bool, np.bool_)):
        return bool(value)
    if isinstance(value, (int, np.integer)):
        return int(value)
    if isinstance(value, (float, np.floating)):
        if math.isnan(value):
            return None
        return round(float(value), digits)
    return value


def segment_record(row: pd.Series) -> dict:
    speed_code = int(row["speed_limit"]) if not pd.isna(row["speed_limit"]) else 0
    return {
        "id": int(row["node_id"]),
        "lat1": round_or_none(row["y_coordinate_start"], 6),
        "lon1": round_or_none(row["x_coordinate_start"], 6),
        "lat2": round_or_none(row["y_coordinate_end"], 6),
        "lon2": round_or_none(row["x_coordinate_end"], 6),
        "length": round_or_none(row["length"], 1),
        "region": int(row["region_id"]) if not pd.isna(row["region_id"]) else None,
        "category": int(row["category"]) if not pd.isna(row["category"]) else None,
        "edgeType": int(row["edge_type"]) if not pd.isna(row["edge_type"]) else None,
        "speedLimitCode": speed_code,
        "speedLimit": SPEED_LIMIT_LABELS.get(speed_code, str(speed_code)),
        "freeFlowSpeed": round_or_none(row["free_flow_speed"], 2),
        "maxVolume": round_or_none(row["volume_max"], 1),
    }


def build_payload(
    static: pd.DataFrame,
    selected: pd.DataFrame,
    volume_frames: pd.DataFrame,
    speed_frames: pd.DataFrame,
    full_speed: pd.DataFrame,
    sample_day: pd.Timestamp,
) -> dict:
    volume_values = np.nan_to_num(volume_frames.to_numpy(dtype=float), nan=0.0)
    speed_values = np.nan_to_num(speed_frames.to_numpy(dtype=float), nan=0.0)
    free_flow = np.nanpercentile(full_speed.replace(0, np.nan).to_numpy(dtype=float), 90, axis=0)
    free_flow = np.nan_to_num(free_flow, nan=0.0, posinf=0.0, neginf=0.0)

    volume_p95 = max(1.0, float(np.percentile(volume_values, 95)))
    volume_weight = np.clip(np.log1p(volume_values) / math.log1p(volume_p95), 0.0, 1.0)
    with np.errstate(divide="ignore", invalid="ignore"):
        slowdown = 1.0 - (speed_values / free_flow.reshape(1, -1))
    slowdown = np.nan_to_num(slowdown, nan=0.0, posinf=0.0, neginf=0.0)
    slowdown = np.clip(slowdown, 0.0, 1.0)
    congestion = np.clip(slowdown * volume_weight, 0.0, 1.0)

    selected = selected.copy()
    selected["free_flow_speed"] = free_flow
    segments = [segment_record(row) for _, row in selected.iterrows()]

    frame_records = []
    for i, timestamp in enumerate(volume_frames.index):
        congestion_row = congestion[i]
        volume_row = volume_values[i]
        speed_row = speed_values[i]
        active = volume_row > 0
        frame_records.append(
            {
                "i": i,
                "time": timestamp.strftime("%Y-%m-%d %H:%M"),
                "congestionMean": round(float(np.mean(congestion_row)), 3),
                "congestionP95": round(float(np.percentile(congestion_row, 95)), 3),
                "meanSpeed": round(float(np.mean(speed_row[active])) if active.any() else 0.0, 2),
                "meanVolume": round(float(np.mean(volume_row)), 2),
                "active": int(active.sum()),
                "congestion": [round(float(v), 3) for v in congestion_row],
                "volume": [round(float(v), 2) for v in volume_row],
                "speed": [round(float(v), 2) for v in speed_row],
            }
        )

    bounds = {
        "latMin": round(float(static[["y_coordinate_start", "y_coordinate_end"]].min().min()), 6),
        "latMax": round(float(static[["y_coordinate_start", "y_coordinate_end"]].max().max()), 6),
        "lonMin": round(float(static[["x_coordinate_start", "x_coordinate_end"]].min().min()), 6),
        "lonMax": round(float(static[["x_coordinate_start", "x_coordinate_end"]].max().max()), 6),
    }
    peak_frame_index = int(np.argmax(congestion.sum(axis=1)))

    return {
        "metadata": {
            "title": "city-traffic-M OSM 动态拥堵样本",
            "sourceDataset": "mightyneghbor/city-traffic-benchmarks",
            "sourceVersion": 4,
            "dynamicFiles": [VOLUME_PATH.name, SPEED_FILE_NAME],
            "staticFile": STATIC_PATH.name,
            "nativeTimeStepMinutes": 5,
            "frameIntervalMinutes": 15,
            "frameAggregation": "mean of three 5-minute observations",
            "sampleDay": sample_day.strftime("%Y-%m-%d"),
            "frameCount": len(frame_records),
            "dynamicSegmentCount": len(segments),
            "selectionMethod": (
                "Road segments with nonzero volume, selected by highest volume and "
                "grid-based spatial thinning. Speed is range-fetched only for these segments."
            ),
            "congestionDefinition": (
                "max(0, 1 - speed / segment_p90_speed) multiplied by a clipped log-volume weight"
            ),
            "osmUse": (
                "OpenStreetMap is used as the visual basemap for road context, place labels, "
                "and orientation. It is not used to calculate congestion in this artifact."
            ),
            "peakFrameIndex": peak_frame_index,
            "peakFrame": frame_records[peak_frame_index]["time"],
            "volumeP95": round(volume_p95, 2),
            "congestionP95": round(float(np.percentile(congestion, 95)), 3),
        },
        "bounds": bounds,
        "segments": segments,
        "frames": frame_records,
    }


def write_outputs(payload: dict) -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    JSON_OUT.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    JS_OUT.write_text(
        "window.CITY_TRAFFIC_OSM_SAMPLE = "
        + json.dumps(payload, ensure_ascii=False, separators=(",", ":"))
        + ";\n",
        encoding="utf-8",
    )
    HTML_OUT.write_text(HTML, encoding="utf-8")


HTML = r"""<!doctype html>
<html lang="zh-CN">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>city-traffic-M OSM 动态拥堵样本</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <script src="city_traffic_M_osm_congestion_sample.js"></script>
  <style>
    :root {
      --ink: #20252b;
      --muted: #64717a;
      --line: #d8dedb;
      --panel: #ffffff;
      --paper: #f7f8f5;
      --green: #00a651;
      --amber: #ffb000;
      --red: #e02424;
      --blue: #326b91;
      --shadow: 0 12px 30px rgba(32, 37, 43, 0.14);
    }

    * { box-sizing: border-box; }

    html, body, .app {
      height: 100%;
      margin: 0;
    }

    body {
      background: var(--paper);
      color: var(--ink);
      font-family: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      letter-spacing: 0;
    }

    .app {
      display: grid;
      grid-template-rows: auto minmax(0, 1fr);
    }

    header {
      z-index: 5;
      display: grid;
      grid-template-columns: minmax(0, 1fr) auto;
      gap: 18px;
      align-items: end;
      padding: 16px 18px 12px;
      border-bottom: 1px solid var(--line);
      background: rgba(255, 255, 255, 0.96);
    }

    h1 {
      margin: 0;
      font-size: 21px;
      line-height: 1.2;
      font-weight: 760;
    }

    .subtitle {
      margin-top: 5px;
      color: var(--muted);
      font-size: 13px;
      line-height: 1.35;
    }

    .metrics {
      display: grid;
      grid-template-columns: repeat(4, minmax(86px, 1fr));
      gap: 9px;
      min-width: 500px;
    }

    .metric {
      border: 1px solid var(--line);
      border-radius: 8px;
      padding: 8px 9px;
      background: #fff;
    }

    .metric span {
      display: block;
    }

    .label {
      color: var(--muted);
      font-size: 11px;
      line-height: 1.15;
      white-space: normal;
    }

    .value {
      margin-top: 4px;
      font-variant-numeric: tabular-nums;
      font-size: 17px;
      line-height: 1.1;
      font-weight: 740;
    }

    main {
      position: relative;
      min-height: 0;
    }

    #map {
      height: 100%;
      min-height: 600px;
    }

    #map .leaflet-tile-pane {
      filter: saturate(0.62) contrast(0.9) brightness(1.08);
    }

    .control-panel {
      position: absolute;
      z-index: 600;
      left: 16px;
      right: 16px;
      bottom: 16px;
      display: grid;
      grid-template-columns: auto auto minmax(220px, 1fr) auto;
      gap: 10px;
      align-items: center;
      max-width: 920px;
      padding: 11px;
      border: 1px solid rgba(216, 222, 219, 0.9);
      border-radius: 8px;
      background: rgba(255, 255, 255, 0.94);
      box-shadow: var(--shadow);
      backdrop-filter: blur(10px);
    }

    button, select {
      height: 36px;
      border: 1px solid #c7d0cd;
      border-radius: 8px;
      background: #fff;
      color: var(--ink);
      font: inherit;
      font-size: 13px;
    }

    button {
      min-width: 72px;
      padding: 0 12px;
      font-weight: 720;
      cursor: pointer;
    }

    select {
      min-width: 124px;
      padding: 0 10px;
    }

    input[type="range"] {
      width: 100%;
      accent-color: var(--blue);
    }

    .time {
      min-width: 138px;
      text-align: right;
      font-variant-numeric: tabular-nums;
      font-size: 13px;
      font-weight: 720;
    }

    .side-panel {
      position: absolute;
      z-index: 550;
      top: 16px;
      right: 16px;
      display: grid;
      gap: 12px;
      width: 320px;
    }

    .panel {
      border: 1px solid var(--line);
      border-radius: 8px;
      padding: 12px;
      background: rgba(255, 255, 255, 0.95);
      box-shadow: var(--shadow);
      backdrop-filter: blur(8px);
    }

    .panel h2 {
      margin: 0 0 10px;
      font-size: 13px;
      line-height: 1.2;
      font-weight: 760;
    }

    .ramp {
      height: 12px;
      border: 1px solid #cfd6d3;
      border-radius: 8px;
      background: linear-gradient(90deg, var(--green), var(--amber), var(--red));
    }

    .legend-row {
      display: flex;
      justify-content: space-between;
      margin-top: 7px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.25;
    }

    .detail-grid {
      display: grid;
      gap: 8px;
    }

    .detail-row {
      display: grid;
      grid-template-columns: 78px minmax(0, 1fr);
      gap: 10px;
      align-items: baseline;
      font-size: 12px;
      line-height: 1.35;
    }

    .detail-row span {
      color: var(--muted);
    }

    .detail-row strong {
      min-width: 0;
      font-weight: 700;
      font-variant-numeric: tabular-nums;
      overflow-wrap: anywhere;
    }

    #trendCanvas {
      display: block;
      width: 100%;
      height: 142px;
    }

    .note {
      color: var(--muted);
      font-size: 12px;
      line-height: 1.35;
    }

    .leaflet-tooltip.traffic-tooltip {
      border: 1px solid var(--line);
      border-radius: 8px;
      box-shadow: var(--shadow);
      color: var(--ink);
      font-size: 12px;
      line-height: 1.35;
    }

    .mono { font-variant-numeric: tabular-nums; }

    @media (max-width: 900px) {
      header {
        grid-template-columns: 1fr;
      }

      .metrics {
        min-width: 0;
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }

      .side-panel {
        left: 12px;
        right: 12px;
        width: auto;
      }

      .control-panel {
        grid-template-columns: 1fr;
      }

      .time {
        text-align: left;
      }
    }
  </style>
</head>
<body>
  <div class="app">
    <header>
      <div>
        <h1>city-traffic-M OSM 动态拥堵样本</h1>
        <div class="subtitle" id="subtitle"></div>
      </div>
      <div class="metrics">
        <div class="metric"><span class="label" id="m1Label">拥堵均值</span><span class="value" id="m1Value">0</span></div>
        <div class="metric"><span class="label" id="m2Label">拥堵 95 分位</span><span class="value" id="m2Value">0</span></div>
        <div class="metric"><span class="label">平均速度 km/h</span><span class="value" id="speedValue">0</span></div>
        <div class="metric"><span class="label">有流量道路</span><span class="value" id="activeValue">0</span></div>
      </div>
    </header>

    <main>
      <div id="map"></div>
      <section class="side-panel">
        <div class="panel">
          <h2 id="legendTitle">拥堵颜色刻度</h2>
          <div class="ramp"></div>
          <div class="legend-row" id="legendLabels">
            <span>通畅</span><span>变慢</span><span>严重</span>
          </div>
        </div>
        <div class="panel">
          <h2 id="trendTitle">拥堵趋势</h2>
          <canvas id="trendCanvas" width="580" height="260"></canvas>
        </div>
        <div class="panel">
          <h2>当前时段</h2>
          <div class="detail-grid">
            <div class="detail-row"><span>时间窗口</span><strong id="windowValue"></strong></div>
            <div class="detail-row"><span>帧序号</span><strong id="frameValue"></strong></div>
            <div class="detail-row"><span>原始粒度</span><strong id="grainValue"></strong></div>
            <div class="detail-row"><span>突出路段</span><strong id="hotSegmentValue"></strong></div>
          </div>
        </div>
        <div class="panel">
          <h2>指标说明</h2>
          <div class="note" id="metricNote"></div>
        </div>
        <div class="panel">
          <h2>数据来源</h2>
          <div class="note" id="sourceNote"></div>
        </div>
      </section>
      <section class="control-panel">
        <button type="button" id="playButton">播放</button>
        <select id="metricSelect">
          <option value="congestion">拥堵</option>
          <option value="volume">流量</option>
        </select>
        <input id="frameSlider" type="range" min="0" max="0" value="0">
        <div class="time" id="timeReadout"></div>
      </section>
    </main>
  </div>

  <script>
    const data = window.CITY_TRAFFIC_OSM_SAMPLE;
    const meta = data.metadata;
    const slider = document.getElementById("frameSlider");
    const playButton = document.getElementById("playButton");
    const metricSelect = document.getElementById("metricSelect");
    const timeReadout = document.getElementById("timeReadout");
    const subtitle = document.getElementById("subtitle");
    const m1Label = document.getElementById("m1Label");
    const m2Label = document.getElementById("m2Label");
    const m1Value = document.getElementById("m1Value");
    const m2Value = document.getElementById("m2Value");
    const speedValue = document.getElementById("speedValue");
    const activeValue = document.getElementById("activeValue");
    const legendTitle = document.getElementById("legendTitle");
    const legendLabels = document.getElementById("legendLabels");
    const trendTitle = document.getElementById("trendTitle");
    const metricNote = document.getElementById("metricNote");
    const sourceNote = document.getElementById("sourceNote");
    const windowValue = document.getElementById("windowValue");
    const frameValue = document.getElementById("frameValue");
    const grainValue = document.getElementById("grainValue");
    const hotSegmentValue = document.getElementById("hotSegmentValue");
    const trendCanvas = document.getElementById("trendCanvas");
    const trendCtx = trendCanvas.getContext("2d");

    let frameIndex = meta.peakFrameIndex || 0;
    let metric = "congestion";
    let playing = false;
    let lastStep = 0;
    const layers = [];
    const haloLayers = [];
    const volumeScale = Math.max(1, meta.volumeP95);

    slider.max = String(data.frames.length - 1);
    subtitle.textContent = `${meta.sampleDay} 样本日 | ${meta.frameCount} 个 15 分钟时间帧 | ${meta.dynamicSegmentCount} 条抽样道路 | 底图来自 OpenStreetMap`;
    sourceNote.innerHTML = `静态几何：<span class="mono">${meta.staticFile}</span><br>
      动态流量：<span class="mono">${meta.dynamicFiles[0]}</span><br>
      动态速度：<span class="mono">${meta.dynamicFiles[1]}</span>（只按需读取抽样路段列）<br>
      OSM：仅用作地图瓦片、道路/地名标签和空间参照。`;

    const map = L.map("map", { preferCanvas: true, zoomControl: true });
    map.createPane("trafficHalo");
    map.getPane("trafficHalo").style.zIndex = 430;
    map.getPane("trafficHalo").style.pointerEvents = "none";
    map.createPane("trafficLines");
    map.getPane("trafficLines").style.zIndex = 440;
    L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
      maxZoom: 19,
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);

    const bounds = L.latLngBounds(
      [data.bounds.latMin, data.bounds.lonMin],
      [data.bounds.latMax, data.bounds.lonMax]
    );
    map.fitBounds(bounds, { padding: [24, 24] });

    function clamp(value, min, max) {
      return Math.max(min, Math.min(max, value));
    }

    function colorRamp(t) {
      t = clamp(t, 0, 1);
      const green = [0, 166, 81];
      const amber = [255, 176, 0];
      const red = [224, 36, 36];
      const left = t < 0.52;
      const local = left ? t / 0.52 : (t - 0.52) / 0.48;
      const a = left ? green : amber;
      const b = left ? amber : red;
      const rgb = a.map((v, i) => Math.round(v + (b[i] - v) * local));
      return `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`;
    }

    function metricValue(frame, index) {
      if (metric === "volume") return frame.volume[index] / volumeScale;
      return frame.congestion[index];
    }

    function rawValue(frame, index) {
      return metric === "volume" ? frame.volume[index] : frame.congestion[index];
    }

    function styleFor(frame, index) {
      const value = metricValue(frame, index);
      const intensity = clamp(value, 0, 1);
      const lineWeight = metric === "volume" ? 2.2 + intensity * 6.2 : 2.6 + intensity * 7.4;
      return {
        line: {
          color: colorRamp(intensity),
          weight: lineWeight,
          opacity: 0.74 + intensity * 0.24,
          lineCap: "round",
          lineJoin: "round",
        },
        halo: {
          color: "#151a1f",
          weight: lineWeight + 4,
          opacity: 0.34 + intensity * 0.3,
          lineCap: "round",
          lineJoin: "round",
        },
      };
    }

    function tooltipText(segment, frame, index) {
      const speedLimitText = segment.speedLimit === "unknown" ? "未知" : segment.speedLimit;
      return `<strong>道路段 #${segment.id}</strong><br>
        拥堵指数：<span class="mono">${frame.congestion[index].toFixed(3)}</span><br>
        流量值：<span class="mono">${frame.volume[index].toFixed(2)}</span><br>
        当前速度：<span class="mono">${frame.speed[index].toFixed(1)} km/h</span><br>
        自由流速度：<span class="mono">${segment.freeFlowSpeed} km/h</span><br>
        静态限速：${speedLimitText}，区域 ${segment.region}<br>
        长度：<span class="mono">${segment.length} m</span>`;
    }

    function pad2(value) {
      return String(value).padStart(2, "0");
    }

    function parseFrameDate(timeText) {
      const [datePart, clockPart] = timeText.split(" ");
      const [year, month, day] = datePart.split("-").map(Number);
      const [hour, minute] = clockPart.split(":").map(Number);
      return new Date(year, month - 1, day, hour, minute);
    }

    function formatClock(date) {
      return `${pad2(date.getHours())}:${pad2(date.getMinutes())}`;
    }

    function formatZhDateTime(date) {
      return `${date.getFullYear()}年${pad2(date.getMonth() + 1)}月${pad2(date.getDate())}日 ${formatClock(date)}`;
    }

    function formatWindow(timeText) {
      const start = parseFrameDate(timeText);
      const end = new Date(start.getTime() + meta.frameIntervalMinutes * 60 * 1000);
      const sameDay = start.toDateString() === end.toDateString();
      return `${formatZhDateTime(start)}-${sameDay ? formatClock(end) : formatZhDateTime(end)}`;
    }

    function hottestSegment(frame) {
      let bestIndex = 0;
      let bestValue = -Infinity;
      for (let i = 0; i < data.segments.length; i++) {
        const value = rawValue(frame, i);
        if (value > bestValue) {
          bestValue = value;
          bestIndex = i;
        }
      }
      const segment = data.segments[bestIndex];
      if (metric === "volume") {
        return `#${segment.id}，流量 ${frame.volume[bestIndex].toFixed(2)}，速度 ${frame.speed[bestIndex].toFixed(1)} km/h`;
      }
      return `#${segment.id}，拥堵 ${frame.congestion[bestIndex].toFixed(3)}，速度 ${frame.speed[bestIndex].toFixed(1)} km/h`;
    }

    for (let i = 0; i < data.segments.length; i++) {
      const segment = data.segments[i];
      const styles = styleFor(data.frames[frameIndex], i);
      const halo = L.polyline(
        [[segment.lat1, segment.lon1], [segment.lat2, segment.lon2]],
        { ...styles.halo, pane: "trafficHalo", interactive: false }
      ).addTo(map);
      const polyline = L.polyline(
        [[segment.lat1, segment.lon1], [segment.lat2, segment.lon2]],
        { ...styles.line, pane: "trafficLines" }
      ).addTo(map);
      polyline.bindTooltip("", { className: "traffic-tooltip", sticky: true });
      polyline.on("mouseover", () => {
        polyline.setTooltipContent(tooltipText(segment, data.frames[frameIndex], i));
      });
      haloLayers.push(halo);
      layers.push(polyline);
    }

    function drawTrend() {
      const rect = trendCanvas.getBoundingClientRect();
      const dpr = window.devicePixelRatio || 1;
      trendCanvas.width = Math.max(1, Math.round(rect.width * dpr));
      trendCanvas.height = Math.max(1, Math.round(rect.height * dpr));
      trendCtx.setTransform(dpr, 0, 0, dpr, 0, 0);
      const values = data.frames.map((frame) => metric === "volume" ? frame.meanVolume : frame.congestionMean);
      const maxValue = Math.max(...values, 0.001);
      const pad = { left: 34, right: 14, top: 16, bottom: 26 };
      const w = rect.width - pad.left - pad.right;
      const h = rect.height - pad.top - pad.bottom;

      trendCtx.clearRect(0, 0, rect.width, rect.height);
      trendCtx.fillStyle = "#ffffff";
      trendCtx.fillRect(0, 0, rect.width, rect.height);
      trendCtx.strokeStyle = "#e2e7e4";
      trendCtx.lineWidth = 1;
      for (let i = 0; i <= 4; i++) {
        const y = pad.top + h * i / 4;
        trendCtx.beginPath();
        trendCtx.moveTo(pad.left, y);
        trendCtx.lineTo(pad.left + w, y);
        trendCtx.stroke();
      }

      trendCtx.strokeStyle = metric === "volume" ? "#0057b8" : "#e02424";
      trendCtx.lineWidth = 2;
      trendCtx.beginPath();
      values.forEach((value, i) => {
        const x = pad.left + w * i / (values.length - 1);
        const y = pad.top + h * (1 - value / maxValue);
        if (i === 0) trendCtx.moveTo(x, y);
        else trendCtx.lineTo(x, y);
      });
      trendCtx.stroke();

      const cursorX = pad.left + w * frameIndex / (values.length - 1);
      trendCtx.strokeStyle = "#20252b";
      trendCtx.lineWidth = 1.4;
      trendCtx.beginPath();
      trendCtx.moveTo(cursorX, pad.top);
      trendCtx.lineTo(cursorX, pad.top + h);
      trendCtx.stroke();

      trendCtx.fillStyle = "#64717a";
      trendCtx.font = "12px ui-sans-serif, system-ui, sans-serif";
      trendCtx.fillText("00:00", pad.left, rect.height - 8);
      trendCtx.fillText("24:00", rect.width - pad.right - 40, rect.height - 8);
    }

    function updateFrame() {
      const frame = data.frames[frameIndex];
      for (let i = 0; i < layers.length; i++) {
        const styles = styleFor(frame, i);
        haloLayers[i].setStyle(styles.halo);
        layers[i].setStyle(styles.line);
      }

      timeReadout.textContent = formatWindow(frame.time);
      windowValue.textContent = formatWindow(frame.time);
      frameValue.textContent = `${frameIndex + 1} / ${data.frames.length}`;
      grainValue.textContent = `原始 ${meta.nativeTimeStepMinutes} 分钟，显示为 ${meta.frameIntervalMinutes} 分钟均值`;
      hotSegmentValue.textContent = hottestSegment(frame);
      slider.value = String(frameIndex);
      speedValue.textContent = frame.meanSpeed.toFixed(1);
      activeValue.textContent = String(frame.active);

      if (metric === "volume") {
        m1Label.textContent = "平均流量";
        m2Label.textContent = "流量 95 分位";
        m1Value.textContent = frame.meanVolume.toFixed(2);
        m2Value.textContent = meta.volumeP95.toFixed(2);
        legendTitle.textContent = "流量颜色刻度";
        legendLabels.innerHTML = "<span>低流量</span><span>中等</span><span>高流量</span>";
        trendTitle.textContent = "流量趋势";
        metricNote.textContent = "流量模式直接使用数据集中的 volume 值。颜色从绿色到红色表示从低流量到高流量，线越粗代表该时间段流量越高。";
      } else {
        m1Label.textContent = "拥堵均值";
        m2Label.textContent = "拥堵 95 分位";
        m1Value.textContent = frame.congestionMean.toFixed(3);
        m2Value.textContent = frame.congestionP95.toFixed(3);
        legendTitle.textContent = "拥堵颜色刻度";
        legendLabels.innerHTML = "<span>通畅</span><span>变慢</span><span>严重</span>";
        trendTitle.textContent = "拥堵趋势";
        metricNote.textContent = "拥堵指数 = 相对该路段 p90 速度的降速程度 × 对数流量权重。这样能突出“车多且变慢”的道路，降低低速但几乎没车的道路影响。";
      }
      drawTrend();
    }

    function step(timestamp) {
      if (!playing) return;
      if (!lastStep || timestamp - lastStep > 260) {
        frameIndex = (frameIndex + 1) % data.frames.length;
        lastStep = timestamp;
        updateFrame();
      }
      requestAnimationFrame(step);
    }

    playButton.addEventListener("click", () => {
      playing = !playing;
      playButton.textContent = playing ? "暂停" : "播放";
      if (playing) requestAnimationFrame(step);
    });

    slider.addEventListener("input", () => {
      frameIndex = Number(slider.value);
      updateFrame();
    });

    metricSelect.addEventListener("change", () => {
      metric = metricSelect.value;
      updateFrame();
    });

    window.addEventListener("resize", drawTrend);
    updateFrame();
  </script>
</body>
</html>
"""


def main() -> None:
    require_sources()
    static = load_static_features()
    stats = load_volume_column_stats()
    selected = select_dynamic_segments(static, stats)
    volume = read_volume_timeseries(selected)
    sample_day = choose_sample_day(volume)
    volume_frames = frame_timeseries(volume, sample_day)
    speed = read_speed_subset(selected)
    speed_frames = frame_timeseries(speed, sample_day)
    payload = build_payload(static, selected, volume_frames, speed_frames, speed, sample_day)
    write_outputs(payload)

    print(f"Wrote {JSON_OUT.relative_to(ROOT)}")
    print(f"Wrote {JS_OUT.relative_to(ROOT)}")
    print(f"Wrote {HTML_OUT.relative_to(ROOT)}")
    print(
        "Sample:",
        payload["metadata"]["sampleDay"],
        payload["metadata"]["frameCount"],
        "frames,",
        payload["metadata"]["dynamicSegmentCount"],
        "segments, peak",
        payload["metadata"]["peakFrame"],
    )


if __name__ == "__main__":
    # Avoid surprises if a shell has proxy variables set for unrelated work.
    os.environ.setdefault("NO_PROXY", "localhost,127.0.0.1")
    main()
