#!/usr/bin/env python3
"""Build a lightweight dynamic traffic-volume visualization sample.

The source Parquet files are large, so this script reads only a selected set of
road-segment columns from the dynamic volume table and writes browser-friendly
artifacts under visualizations/.
"""

from __future__ import annotations

import json
import math
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq


ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = ROOT / "data" / "city-traffic-benchmarks"
OUT_DIR = ROOT / "visualizations"

STATIC_PATH = DATA_DIR / "city-traffic-M-raw-static-features.parquet"
VOLUME_PATH = DATA_DIR / "city-traffic-M-raw-volume.parquet"

JSON_OUT = OUT_DIR / "city_traffic_M_volume_sample.json"
JS_OUT = OUT_DIR / "city_traffic_M_volume_sample.js"
HTML_OUT = OUT_DIR / "city_traffic_M_volume_dynamic_sample.html"

TARGET_DYNAMIC_SEGMENTS = 720
TARGET_BACKGROUND_SEGMENTS = 6400
GRID_X = 34
GRID_Y = 22
FRAME_FREQ = "15min"

SPEED_LIMIT_LABELS = {
    0: "unknown",
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
    return selected.head(TARGET_DYNAMIC_SEGMENTS).reset_index(drop=True)


def read_volume_timeseries(selected: pd.DataFrame) -> pd.DataFrame:
    columns = selected["volume_column"].tolist() + ["timestamp"]
    table = pq.read_table(VOLUME_PATH, columns=columns)
    frame = table.to_pandas()

    if not isinstance(frame.index, pd.DatetimeIndex):
        if "timestamp" in frame.columns:
            frame = frame.set_index("timestamp")
        else:
            raise ValueError("Volume table did not expose a timestamp index or column.")

    frame.index = pd.to_datetime(frame.index)
    frame = frame.sort_index()
    return frame[selected["volume_column"].tolist()]


def choose_sample_day(volume: pd.DataFrame) -> pd.Timestamp:
    complete_counts = volume.resample("D").size()
    complete_days = complete_counts[complete_counts >= 288].index
    daily_total = volume.resample("D").sum(min_count=1).sum(axis=1)
    daily_total = daily_total.loc[complete_days]
    if daily_total.empty:
        raise ValueError("No complete 24-hour day found in sampled volume data.")
    return pd.Timestamp(daily_total.idxmax()).normalize()


def make_frames(volume: pd.DataFrame, sample_day: pd.Timestamp) -> pd.DataFrame:
    day_end = sample_day + pd.Timedelta(days=1)
    day = volume.loc[(volume.index >= sample_day) & (volume.index < day_end)]
    frames = day.resample(FRAME_FREQ).mean().interpolate(limit_direction="both")
    return frames.fillna(0.0)


def select_background_segments(static: pd.DataFrame, dynamic_nodes: set[int]) -> pd.DataFrame:
    dynamic_background = static[static["node_id"].isin(dynamic_nodes)]
    long_roads = static.sort_values("length", ascending=False).head(TARGET_BACKGROUND_SEGMENTS)
    background = pd.concat([dynamic_background, long_roads], ignore_index=True)
    background = background.drop_duplicates("node_id")
    return background.head(TARGET_BACKGROUND_SEGMENTS).reset_index(drop=True)


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


def segment_record(row: pd.Series, include_volume: bool = False) -> dict:
    speed_code = int(row["speed_limit"]) if not pd.isna(row["speed_limit"]) else 0
    record = {
        "id": int(row["node_id"]),
        "x1": round_or_none(row["x_coordinate_start"], 6),
        "y1": round_or_none(row["y_coordinate_start"], 6),
        "x2": round_or_none(row["x_coordinate_end"], 6),
        "y2": round_or_none(row["y_coordinate_end"], 6),
        "length": round_or_none(row["length"], 1),
        "region": int(row["region_id"]) if not pd.isna(row["region_id"]) else None,
        "category": int(row["category"]) if not pd.isna(row["category"]) else None,
        "edgeType": int(row["edge_type"]) if not pd.isna(row["edge_type"]) else None,
        "speedLimitCode": speed_code,
        "speedLimit": SPEED_LIMIT_LABELS.get(speed_code, str(speed_code)),
        "isPaved": bool(row["is_paved"]),
        "crosswalkEnd": bool(row["ends_with_crosswalk"]),
    }
    if include_volume:
        record["maxVolume"] = round_or_none(row["volume_max"], 1)
        record["meanVolume"] = round_or_none(row["mean_volume"], 2)
        record["peakFrame"] = int(row["peak_frame"])
    return record


def build_payload(
    static: pd.DataFrame,
    selected: pd.DataFrame,
    background: pd.DataFrame,
    frames: pd.DataFrame,
    sample_day: pd.Timestamp,
) -> dict:
    values = frames.to_numpy(dtype=float)
    values = np.nan_to_num(values, nan=0.0, posinf=0.0, neginf=0.0)

    selected = selected.copy()
    selected["mean_volume"] = values.mean(axis=0)
    selected["peak_frame"] = values.argmax(axis=0)

    dynamic_segments = [segment_record(row, include_volume=True) for _, row in selected.iterrows()]
    background_segments = [
        [
            round_or_none(row["x_coordinate_start"], 6),
            round_or_none(row["y_coordinate_start"], 6),
            round_or_none(row["x_coordinate_end"], 6),
            round_or_none(row["y_coordinate_end"], 6),
        ]
        for _, row in background.iterrows()
    ]

    frame_records = []
    for i, (timestamp, row_values) in enumerate(zip(frames.index, values)):
        active = int((row_values > 0).sum())
        frame_records.append(
            {
                "i": i,
                "time": timestamp.strftime("%Y-%m-%d %H:%M"),
                "mean": round(float(np.mean(row_values)), 2),
                "p95": round(float(np.percentile(row_values, 95)), 2),
                "active": active,
                "values": [round(float(v), 2) for v in row_values],
            }
        )

    global_p95 = float(np.percentile(values, 95))
    global_p99 = float(np.percentile(values, 99))
    bounds = {
        "xMin": round(float(static[["x_coordinate_start", "x_coordinate_end"]].min().min()), 6),
        "xMax": round(float(static[["x_coordinate_start", "x_coordinate_end"]].max().max()), 6),
        "yMin": round(float(static[["y_coordinate_start", "y_coordinate_end"]].min().min()), 6),
        "yMax": round(float(static[["y_coordinate_start", "y_coordinate_end"]].max().max()), 6),
    }

    peak_frame_index = int(np.argmax(values.sum(axis=1)))
    return {
        "metadata": {
            "title": "city-traffic-M volume sample",
            "sourceDataset": "mightyneghbor/city-traffic-benchmarks",
            "sourceVersion": 4,
            "dynamicFile": VOLUME_PATH.name,
            "staticFile": STATIC_PATH.name,
            "dynamicVariable": "traffic volume",
            "nativeTimeStepMinutes": 5,
            "frameAggregation": "mean of three 5-minute observations",
            "frameIntervalMinutes": 15,
            "sampleDay": sample_day.strftime("%Y-%m-%d"),
            "frameCount": len(frame_records),
            "dynamicSegmentCount": len(dynamic_segments),
            "backgroundSegmentCount": len(background_segments),
            "selectionMethod": (
                "Road segments with nonzero volume, selected by highest Parquet max "
                "volume with grid-based spatial thinning."
            ),
            "peakFrameIndex": peak_frame_index,
            "peakFrame": frame_records[peak_frame_index]["time"],
            "globalP95": round(global_p95, 2),
            "globalP99": round(global_p99, 2),
        },
        "bounds": bounds,
        "backgroundSegments": background_segments,
        "segments": dynamic_segments,
        "frames": frame_records,
    }


def write_payload(payload: dict) -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    json_text = json.dumps(payload, ensure_ascii=False, separators=(",", ":"))
    JSON_OUT.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    JS_OUT.write_text("window.CITY_TRAFFIC_SAMPLE = " + json_text + ";\n", encoding="utf-8")


HTML = r"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>city-traffic-M Dynamic Volume Sample</title>
  <script src="city_traffic_M_volume_sample.js"></script>
  <style>
    :root {
      --ink: #20252b;
      --muted: #66717d;
      --panel: #f7f8f6;
      --line: #d9dedc;
      --blue: #31688e;
      --gold: #d4a23a;
      --rust: #b9543d;
      --olive: #697e48;
      --paper: #fffdf8;
      --shadow: 0 12px 32px rgba(32, 37, 43, 0.12);
    }

    * { box-sizing: border-box; }

    body {
      margin: 0;
      min-height: 100vh;
      background: var(--paper);
      color: var(--ink);
      font-family: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      letter-spacing: 0;
    }

    .app {
      display: grid;
      grid-template-rows: auto minmax(0, 1fr);
      min-height: 100vh;
    }

    header {
      display: grid;
      grid-template-columns: minmax(0, 1fr) auto;
      gap: 20px;
      align-items: end;
      padding: 18px 22px 14px;
      border-bottom: 1px solid var(--line);
      background: rgba(255, 253, 248, 0.96);
    }

    h1 {
      margin: 0;
      font-size: 22px;
      line-height: 1.18;
      font-weight: 720;
    }

    .subtitle {
      margin-top: 5px;
      color: var(--muted);
      font-size: 13px;
      line-height: 1.35;
    }

    .header-metrics {
      display: grid;
      grid-template-columns: repeat(3, minmax(96px, 1fr));
      gap: 10px;
      min-width: 340px;
    }

    .metric {
      border: 1px solid var(--line);
      border-radius: 8px;
      padding: 9px 10px;
      background: #ffffff;
    }

    .metric-label {
      display: block;
      color: var(--muted);
      font-size: 11px;
      line-height: 1.2;
      white-space: nowrap;
    }

    .metric-value {
      display: block;
      margin-top: 4px;
      font-variant-numeric: tabular-nums;
      font-size: 17px;
      line-height: 1.1;
      font-weight: 720;
    }

    main {
      display: grid;
      grid-template-columns: minmax(0, 1fr) 330px;
      min-height: 0;
    }

    .map-wrap {
      position: relative;
      min-height: 560px;
      overflow: hidden;
      background: #fbfaf5;
    }

    #mapCanvas {
      width: 100%;
      height: 100%;
      display: block;
    }

    .map-footer {
      position: absolute;
      left: 18px;
      right: 18px;
      bottom: 16px;
      display: grid;
      grid-template-columns: auto minmax(180px, 1fr) auto;
      gap: 12px;
      align-items: center;
      padding: 12px;
      border: 1px solid rgba(217, 222, 220, 0.86);
      border-radius: 8px;
      background: rgba(255, 253, 248, 0.92);
      box-shadow: var(--shadow);
      backdrop-filter: blur(10px);
    }

    button,
    select {
      height: 36px;
      border: 1px solid #cbd3d1;
      border-radius: 8px;
      background: #ffffff;
      color: var(--ink);
      font: inherit;
      font-size: 13px;
    }

    button {
      min-width: 72px;
      padding: 0 12px;
      font-weight: 700;
      cursor: pointer;
    }

    select {
      min-width: 86px;
      padding: 0 10px;
    }

    input[type="range"] {
      width: 100%;
      accent-color: var(--blue);
    }

    .time-readout {
      min-width: 138px;
      text-align: right;
      font-variant-numeric: tabular-nums;
      font-size: 13px;
      font-weight: 700;
    }

    aside {
      display: grid;
      grid-template-rows: auto auto minmax(0, 1fr);
      gap: 16px;
      min-height: 0;
      padding: 18px;
      border-left: 1px solid var(--line);
      background: var(--panel);
      overflow: auto;
    }

    .panel {
      border: 1px solid var(--line);
      border-radius: 8px;
      padding: 13px;
      background: #ffffff;
    }

    .panel h2 {
      margin: 0 0 10px;
      font-size: 13px;
      line-height: 1.2;
      font-weight: 760;
    }

    #trendCanvas {
      width: 100%;
      height: 170px;
      display: block;
    }

    .legend {
      display: grid;
      gap: 8px;
    }

    .ramp {
      height: 12px;
      border: 1px solid #cfd6d3;
      border-radius: 8px;
      background: linear-gradient(90deg, #d9e8ef 0%, #d4a23a 52%, #b9543d 100%);
    }

    .legend-row {
      display: flex;
      justify-content: space-between;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.25;
    }

    .segment-table {
      display: grid;
      gap: 7px;
    }

    .segment-row {
      display: grid;
      grid-template-columns: 56px 1fr 54px;
      gap: 8px;
      align-items: center;
      font-size: 12px;
      line-height: 1.25;
      border-bottom: 1px solid #edf0ef;
      padding-bottom: 7px;
    }

    .segment-row:last-child { border-bottom: 0; padding-bottom: 0; }
    .mono { font-variant-numeric: tabular-nums; }
    .muted { color: var(--muted); }

    .tooltip {
      position: absolute;
      pointer-events: none;
      z-index: 4;
      display: none;
      max-width: 230px;
      padding: 9px 10px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: rgba(255, 255, 255, 0.96);
      box-shadow: var(--shadow);
      font-size: 12px;
      line-height: 1.35;
    }

    @media (max-width: 900px) {
      header,
      main {
        grid-template-columns: 1fr;
      }

      .header-metrics {
        min-width: 0;
      }

      aside {
        border-left: 0;
        border-top: 1px solid var(--line);
      }

      .map-footer {
        grid-template-columns: 1fr;
      }

      .time-readout {
        text-align: left;
      }
    }
  </style>
</head>
<body>
  <div class="app">
    <header>
      <div>
        <h1>city-traffic-M Dynamic Volume Sample</h1>
        <div class="subtitle" id="subtitle"></div>
      </div>
      <div class="header-metrics">
        <div class="metric">
          <span class="metric-label">Mean volume</span>
          <span class="metric-value" id="meanMetric">0</span>
        </div>
        <div class="metric">
          <span class="metric-label">P95 volume</span>
          <span class="metric-value" id="p95Metric">0</span>
        </div>
        <div class="metric">
          <span class="metric-label">Active segments</span>
          <span class="metric-value" id="activeMetric">0</span>
        </div>
      </div>
    </header>

    <main>
      <section class="map-wrap" id="mapWrap">
        <canvas id="mapCanvas"></canvas>
        <div class="tooltip" id="tooltip"></div>
        <div class="map-footer">
          <button id="playButton" type="button">Play</button>
          <input id="frameSlider" type="range" min="0" max="0" value="0">
          <div class="time-readout" id="timeReadout"></div>
        </div>
      </section>

      <aside>
        <section class="panel">
          <h2>Volume Scale</h2>
          <div class="legend">
            <div class="ramp"></div>
            <div class="legend-row">
              <span>low</span>
              <span id="scaleMid">p95</span>
              <span id="scaleHigh">high</span>
            </div>
          </div>
        </section>

        <section class="panel">
          <h2>Sample Trend</h2>
          <canvas id="trendCanvas" width="580" height="300"></canvas>
        </section>

        <section class="panel">
          <h2>Top Peak Segments</h2>
          <div class="segment-table" id="topSegments"></div>
        </section>
      </aside>
    </main>
  </div>

  <script>
    const data = window.CITY_TRAFFIC_SAMPLE;
    const mapCanvas = document.getElementById("mapCanvas");
    const trendCanvas = document.getElementById("trendCanvas");
    const mapWrap = document.getElementById("mapWrap");
    const tooltip = document.getElementById("tooltip");
    const playButton = document.getElementById("playButton");
    const slider = document.getElementById("frameSlider");
    const timeReadout = document.getElementById("timeReadout");
    const meanMetric = document.getElementById("meanMetric");
    const p95Metric = document.getElementById("p95Metric");
    const activeMetric = document.getElementById("activeMetric");
    const subtitle = document.getElementById("subtitle");
    const scaleMid = document.getElementById("scaleMid");
    const scaleHigh = document.getElementById("scaleHigh");
    const topSegments = document.getElementById("topSegments");

    const mapCtx = mapCanvas.getContext("2d");
    const trendCtx = trendCanvas.getContext("2d");
    const bounds = data.bounds;
    const meta = data.metadata;
    const maxScale = Math.max(1, meta.globalP99);
    let frameIndex = meta.peakFrameIndex || 0;
    let playing = false;
    let lastStep = 0;

    slider.max = String(data.frames.length - 1);
    subtitle.textContent = `${meta.sampleDay}, ${meta.frameCount} frames, ${meta.dynamicSegmentCount} dynamic road segments, ${meta.frameAggregation}.`;
    scaleMid.textContent = `p95 ${meta.globalP95}`;
    scaleHigh.textContent = `p99 ${meta.globalP99}`;

    const topRows = data.segments
      .slice()
      .sort((a, b) => b.maxVolume - a.maxVolume)
      .slice(0, 8)
      .map((segment) => {
        return `<div class="segment-row">
          <strong class="mono">#${segment.id}</strong>
          <span class="muted">${segment.speedLimit}, region ${segment.region}</span>
          <span class="mono">${segment.maxVolume}</span>
        </div>`;
      })
      .join("");
    topSegments.innerHTML = topRows;

    function resizeCanvas(canvas, ctx) {
      const rect = canvas.getBoundingClientRect();
      const dpr = window.devicePixelRatio || 1;
      canvas.width = Math.max(1, Math.round(rect.width * dpr));
      canvas.height = Math.max(1, Math.round(rect.height * dpr));
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      return rect;
    }

    function project(x, y, rect) {
      const pad = 36;
      const width = Math.max(1, rect.width - pad * 2);
      const height = Math.max(1, rect.height - pad * 2);
      const xNorm = (x - bounds.xMin) / (bounds.xMax - bounds.xMin);
      const yNorm = (y - bounds.yMin) / (bounds.yMax - bounds.yMin);
      return {
        x: pad + xNorm * width,
        y: pad + (1 - yNorm) * height,
      };
    }

    function lerp(a, b, t) {
      return Math.round(a + (b - a) * t);
    }

    function colorFor(value) {
      const t = Math.max(0, Math.min(1, value / maxScale));
      const blue = [217, 232, 239];
      const gold = [212, 162, 58];
      const rust = [185, 84, 61];
      const left = t < 0.55;
      const local = left ? t / 0.55 : (t - 0.55) / 0.45;
      const a = left ? blue : gold;
      const b = left ? gold : rust;
      return `rgb(${lerp(a[0], b[0], local)}, ${lerp(a[1], b[1], local)}, ${lerp(a[2], b[2], local)})`;
    }

    function drawMap() {
      const rect = resizeCanvas(mapCanvas, mapCtx);
      mapCtx.clearRect(0, 0, rect.width, rect.height);
      mapCtx.fillStyle = "#fbfaf5";
      mapCtx.fillRect(0, 0, rect.width, rect.height);

      mapCtx.lineCap = "round";
      mapCtx.strokeStyle = "rgba(117, 128, 132, 0.17)";
      mapCtx.lineWidth = 0.65;
      mapCtx.beginPath();
      for (const line of data.backgroundSegments) {
        const p1 = project(line[0], line[1], rect);
        const p2 = project(line[2], line[3], rect);
        mapCtx.moveTo(p1.x, p1.y);
        mapCtx.lineTo(p2.x, p2.y);
      }
      mapCtx.stroke();

      const values = data.frames[frameIndex].values;
      for (let i = 0; i < data.segments.length; i++) {
        const segment = data.segments[i];
        const value = values[i];
        if (value <= 0) continue;
        const p1 = project(segment.x1, segment.y1, rect);
        const p2 = project(segment.x2, segment.y2, rect);
        mapCtx.strokeStyle = colorFor(value);
        mapCtx.globalAlpha = 0.42 + Math.min(0.5, value / maxScale * 0.5);
        mapCtx.lineWidth = 0.8 + Math.min(5.2, value / maxScale * 5.2);
        mapCtx.beginPath();
        mapCtx.moveTo(p1.x, p1.y);
        mapCtx.lineTo(p2.x, p2.y);
        mapCtx.stroke();
      }
      mapCtx.globalAlpha = 1;
    }

    function drawTrend() {
      const rect = resizeCanvas(trendCanvas, trendCtx);
      const means = data.frames.map((frame) => frame.mean);
      const maxMean = Math.max(...means, 1);
      const pad = { left: 34, right: 14, top: 18, bottom: 28 };
      const w = rect.width - pad.left - pad.right;
      const h = rect.height - pad.top - pad.bottom;

      trendCtx.clearRect(0, 0, rect.width, rect.height);
      trendCtx.fillStyle = "#ffffff";
      trendCtx.fillRect(0, 0, rect.width, rect.height);

      trendCtx.strokeStyle = "#e4e8e6";
      trendCtx.lineWidth = 1;
      for (let i = 0; i <= 4; i++) {
        const y = pad.top + h * i / 4;
        trendCtx.beginPath();
        trendCtx.moveTo(pad.left, y);
        trendCtx.lineTo(pad.left + w, y);
        trendCtx.stroke();
      }

      trendCtx.strokeStyle = "#31688e";
      trendCtx.lineWidth = 2;
      trendCtx.beginPath();
      means.forEach((value, i) => {
        const x = pad.left + w * i / (means.length - 1);
        const y = pad.top + h * (1 - value / maxMean);
        if (i === 0) trendCtx.moveTo(x, y);
        else trendCtx.lineTo(x, y);
      });
      trendCtx.stroke();

      const cursorX = pad.left + w * frameIndex / (means.length - 1);
      trendCtx.strokeStyle = "#b9543d";
      trendCtx.lineWidth = 1.5;
      trendCtx.beginPath();
      trendCtx.moveTo(cursorX, pad.top);
      trendCtx.lineTo(cursorX, pad.top + h);
      trendCtx.stroke();

      trendCtx.fillStyle = "#66717d";
      trendCtx.font = "12px ui-sans-serif, system-ui, sans-serif";
      trendCtx.fillText("00:00", pad.left, rect.height - 8);
      trendCtx.fillText("24:00", rect.width - pad.right - 40, rect.height - 8);
    }

    function updateMetrics() {
      const frame = data.frames[frameIndex];
      meanMetric.textContent = frame.mean.toFixed(2);
      p95Metric.textContent = frame.p95.toFixed(2);
      activeMetric.textContent = String(frame.active);
      timeReadout.textContent = frame.time;
      slider.value = String(frameIndex);
    }

    function render() {
      drawMap();
      drawTrend();
      updateMetrics();
    }

    function step(timestamp) {
      if (!playing) return;
      if (!lastStep || timestamp - lastStep > 220) {
        frameIndex = (frameIndex + 1) % data.frames.length;
        lastStep = timestamp;
        render();
      }
      requestAnimationFrame(step);
    }

    function distanceToSegment(px, py, ax, ay, bx, by) {
      const dx = bx - ax;
      const dy = by - ay;
      if (dx === 0 && dy === 0) return Math.hypot(px - ax, py - ay);
      const t = Math.max(0, Math.min(1, ((px - ax) * dx + (py - ay) * dy) / (dx * dx + dy * dy)));
      return Math.hypot(px - (ax + t * dx), py - (ay + t * dy));
    }

    mapWrap.addEventListener("mousemove", (event) => {
      const rect = mapCanvas.getBoundingClientRect();
      const mx = event.clientX - rect.left;
      const my = event.clientY - rect.top;
      const values = data.frames[frameIndex].values;
      let best = null;
      let bestDistance = 12;

      for (let i = 0; i < data.segments.length; i++) {
        const segment = data.segments[i];
        const p1 = project(segment.x1, segment.y1, rect);
        const p2 = project(segment.x2, segment.y2, rect);
        const distance = distanceToSegment(mx, my, p1.x, p1.y, p2.x, p2.y);
        if (distance < bestDistance) {
          bestDistance = distance;
          best = { segment, value: values[i] };
        }
      }

      if (!best) {
        tooltip.style.display = "none";
        return;
      }

      tooltip.style.display = "block";
      tooltip.style.left = `${Math.min(rect.width - 248, mx + 16)}px`;
      tooltip.style.top = `${Math.max(12, my - 20)}px`;
      tooltip.innerHTML = `<strong>Road #${best.segment.id}</strong><br>
        Volume: <span class="mono">${best.value.toFixed(2)}</span><br>
        Length: <span class="mono">${best.segment.length} m</span><br>
        ${best.segment.speedLimit}, region ${best.segment.region}`;
    });

    mapWrap.addEventListener("mouseleave", () => {
      tooltip.style.display = "none";
    });

    playButton.addEventListener("click", () => {
      playing = !playing;
      playButton.textContent = playing ? "Pause" : "Play";
      if (playing) requestAnimationFrame(step);
    });

    slider.addEventListener("input", () => {
      frameIndex = Number(slider.value);
      render();
    });

    window.addEventListener("resize", render);
    render();
  </script>
</body>
</html>
"""


def write_html() -> None:
    HTML_OUT.write_text(HTML, encoding="utf-8")


def main() -> None:
    require_sources()
    static = load_static_features()
    stats = load_volume_column_stats()
    selected = select_dynamic_segments(static, stats)
    volume = read_volume_timeseries(selected)
    sample_day = choose_sample_day(volume)
    frames = make_frames(volume, sample_day)
    background = select_background_segments(static, set(selected["node_id"].astype(int)))
    payload = build_payload(static, selected, background, frames, sample_day)
    write_payload(payload)
    write_html()

    print(f"Wrote {JSON_OUT.relative_to(ROOT)}")
    print(f"Wrote {JS_OUT.relative_to(ROOT)}")
    print(f"Wrote {HTML_OUT.relative_to(ROOT)}")
    print(
        "Sample:",
        payload["metadata"]["sampleDay"],
        payload["metadata"]["frameCount"],
        "frames,",
        payload["metadata"]["dynamicSegmentCount"],
        "dynamic segments",
    )


if __name__ == "__main__":
    main()
