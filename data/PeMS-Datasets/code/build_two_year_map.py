#!/usr/bin/env python3
"""Build the July 2024 through June 2026 LA-area traffic map."""

from __future__ import annotations

import base64
import gzip
import json
import subprocess
import time
from pathlib import Path
from typing import Callable, Sequence

import numpy as np
import pandas as pd

from monthly_pipeline import UF_DATALESS


ROOT = Path(__file__).resolve().parents[1]
from traffic_context import (
    assign_links_to_weather_stations,
    build_incident_buckets,
    compute_weekslot_baseline,
    describe_chp_incident,
    expand_chp_note,
)


CLEAN_DIR = ROOT / "clean_data"
NETWORK_DIR = CLEAN_DIR / "network"
TRAFFIC_DIR = CLEAN_DIR / "traffic"
INCIDENT_DIR = CLEAN_DIR / "incidents"
WEATHER_DIR = CLEAN_DIR / "weather"
SUMMARY_DIR = CLEAN_DIR / "summary"
MAP_DIR = ROOT / "map"
ASSET_DIR = MAP_DIR / "data"
OUTPUT_HTML = MAP_DIR / "la_area_freeway_speeds_2024_07_to_2026_06.html"


def file_is_dataless(path: Path) -> bool:
    try:
        return bool(getattr(path.stat(), "st_flags", 0) & UF_DATALESS)
    except OSError:
        return True


def ensure_local_files(
    paths: Sequence[Path],
    timeout_seconds: int = 20 * 60,
    runner: Callable[..., object] = subprocess.run,
    is_dataless: Callable[[Path], bool] = file_is_dataless,
    sleeper: Callable[[float], None] = time.sleep,
) -> None:
    pending = [Path(path) for path in paths if is_dataless(Path(path))]
    for path in pending:
        result = runner(
            ["brctl", "download", str(path)],
            check=False,
            capture_output=True,
            text=True,
        )
        if getattr(result, "returncode", 0) != 0:
            message = (
                getattr(result, "stderr", "").strip()
                or getattr(result, "stdout", "").strip()
            )
            raise OSError(f"Could not download {path}: {message}")
    deadline = time.monotonic() + timeout_seconds
    while pending:
        pending = [path for path in pending if is_dataless(path)]
        if not pending:
            return
        if time.monotonic() >= deadline:
            raise TimeoutError(f"Timed out waiting for map input: {pending[0]}")
        sleeper(2)


def _encode_uint8(values: np.ndarray) -> str:
    return base64.b64encode(np.asarray(values, dtype=np.uint8).tobytes()).decode("ascii")


def _quantize(values: np.ndarray, scale: float = 1.0, offset: float = 0.0) -> np.ndarray:
    array = np.asarray(values, dtype=float)
    result = np.full(array.shape, 255, dtype=np.uint8)
    valid = np.isfinite(array)
    result[valid] = np.clip(np.rint((array[valid] + offset) * scale), 0, 254).astype(np.uint8)
    return result


def prepare_weather_payload(
    weather: pd.DataFrame,
    time_index: pd.DatetimeIndex,
) -> tuple[list[dict], pd.DataFrame]:
    frame = weather.copy()
    frame["time"] = pd.to_datetime(frame["time"], errors="coerce")
    frame = frame.dropna(subset=["time", "latitude", "longitude"])
    stations = frame[["station", "station_name", "latitude", "longitude"]].drop_duplicates("station")

    payload: list[dict] = []
    for station in stations.itertuples(index=False):
        group = frame[frame["station"] == station.station].copy()
        regular = group.set_index("time").reindex(time_index)

        payload.append(
            {
                "station": str(station.station),
                "name": str(station.station_name),
                "lat": round(float(station.latitude), 6),
                "lng": round(float(station.longitude), 6),
                "temperature": _encode_uint8(_quantize(regular["temperature_c"].to_numpy(), 2, 30)),
                "wind": _encode_uint8(_quantize(regular["wind_speed_ms"].to_numpy(), 5)),
                "visibility": _encode_uint8(_quantize(regular["visibility_km"].to_numpy(), 2)),
                "rainHour": _encode_uint8(_quantize(regular["rain_hour_mm"].to_numpy(), 5)),
            }
        )
    return payload, stations


def _number_or_none(value: object) -> float | None:
    number = pd.to_numeric(pd.Series([value]), errors="coerce").iloc[0]
    return round(float(number), 1) if pd.notna(number) else None


def _notes_from_csv(value: object) -> list[str]:
    try:
        parsed = json.loads(str(value))
    except (TypeError, ValueError, json.JSONDecodeError):
        return []
    return [str(note) for note in parsed if str(note).strip()] if isinstance(parsed, list) else []


def _as_bool(value: object) -> bool:
    return str(value).strip().lower() in {"1", "true", "yes"}


def _reported_time_index(value: object, time_index: pd.DatetimeIndex) -> int | None:
    timestamp = pd.to_datetime(value, errors="coerce")
    if pd.isna(timestamp) or time_index.empty:
        return None
    index = int(time_index.searchsorted(timestamp, side="right") - 1)
    return index if 0 <= index < len(time_index) else None


def prepare_incident_payload(
    incidents: pd.DataFrame,
    time_index: pd.DatetimeIndex,
) -> tuple[list[dict], list[list[int]]]:
    clean = incidents.copy().reset_index(drop=True)
    records: list[dict] = []
    for idx, row in clean.iterrows():
        has_link = pd.notna(row["link_id"])
        raw_description = str(row["description"])
        raw_notes = _notes_from_csv(row.get("chp_notes"))
        records.append(
            {
                "id": idx,
                "incidentId": str(row["incident_id"]),
                "timestamp": pd.Timestamp(row["timestamp"]).strftime("%Y-%m-%d %H:%M"),
                "endTime": pd.Timestamp(row["end_time"]).strftime("%Y-%m-%d %H:%M"),
                "description": raw_description,
                "displayDescription": describe_chp_incident(raw_description),
                "displayNotes": [expand_chp_note(note) for note in raw_notes],
                "reportedIndex": _reported_time_index(row["timestamp"], time_index),
                "category": str(row["category"]),
                "location": str(row["location"]),
                "area": str(row["area"]),
                "lat": round(float(row["latitude"]), 6) if pd.notna(row["latitude"]) else None,
                "lng": round(float(row["longitude"]), 6) if pd.notna(row["longitude"]) else None,
                "district": str(row["district"]),
                "freeway": str(row["freeway"]),
                "direction": str(row["direction"]),
                "duration": round(float(row["duration_min"]), 1) if pd.notna(row["duration_min"]) else None,
                "durationDefault": _as_bool(row.get("duration_default", False)),
                "detailMessageCount": int(row.get("detail_message_count", 0)),
                "speedBefore": _number_or_none(row.get("speed_before_mph")),
                "speedDuring": _number_or_none(row.get("speed_during_mph")),
                "speedAfter": _number_or_none(row.get("speed_after_mph")),
                "speedDrop": _number_or_none(row.get("speed_drop_mph")),
                "speedDropPct": _number_or_none(row.get("speed_drop_pct")),
                "normalDuring": _number_or_none(row.get("normal_during_mph")),
                "deficitVsNormal": _number_or_none(row.get("deficit_vs_normal_mph")),
                "samplesBefore": int(row.get("samples_before", 0)),
                "samplesDuring": int(row.get("samples_during", 0)),
                "samplesAfter": int(row.get("samples_after", 0)),
                "linkId": int(row["link_id"]) if has_link else None,
                "matchMethod": str(row["match_method"]),
                "postmileGap": round(float(row["postmile_gap"]), 2) if pd.notna(row["postmile_gap"]) else None,
                "coordinateGapM": round(float(row["coordinate_gap_m"]), 1)
                if pd.notna(row["coordinate_gap_m"])
                else None,
                "sourceIncomplete": _as_bool(row.get("source_incomplete", False)),
            }
        )
    return records, build_incident_buckets(clean, time_index)


def write_gzip_bytes(path: Path, values: np.ndarray) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".part")
    with temporary.open("wb") as raw:
        with gzip.GzipFile(filename="", mode="wb", fileobj=raw, compresslevel=9, mtime=0) as handle:
            handle.write(np.asarray(values, dtype=np.uint8).tobytes(order="C"))
    temporary.replace(path)


def write_gzip_text(path: Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".part")
    with temporary.open("wb") as raw:
        with gzip.GzipFile(filename="", mode="wb", fileobj=raw, compresslevel=9, mtime=0) as handle:
            handle.write(value.encode("utf-8"))
    temporary.replace(path)


def load_network() -> tuple[list[dict[str, object]], list[dict[str, object]]]:
    ensure_local_files([NETWORK_DIR / "links.csv", NETWORK_DIR / "stations.csv"])
    links_frame = pd.read_csv(
        NETWORK_DIR / "links.csv",
        dtype={"district": "string", "freeway": "string", "direction": "string"},
    )
    links: list[dict[str, object]] = []
    for row in links_frame.itertuples(index=False):
        links.append(
            {
                "id": int(row.link_id),
                "district": str(row.district).zfill(2),
                "freeway": str(row.freeway),
                "direction": str(row.direction),
                "from_station": str(row.from_station),
                "to_station": str(row.to_station),
                "from_abs_pm": round(float(row.from_abs_pm), 3),
                "to_abs_pm": round(float(row.to_abs_pm), 3),
                "coords": [
                    [round(float(row.from_lat), 7), round(float(row.from_lon), 7)],
                    [round(float(row.to_lat), 7), round(float(row.to_lon), 7)],
                ],
            }
        )
    stations_frame = pd.read_csv(
        NETWORK_DIR / "stations.csv",
        dtype={"station": "string", "district": "string", "freeway": "string", "direction": "string"},
    )
    stations = [
        {
            "station": str(row.station),
            "lat": round(float(row.latitude), 7),
            "lng": round(float(row.longitude), 7),
            "district": str(row.district).zfill(2),
            "freeway": str(row.freeway),
            "direction": str(row.direction),
            "name": str(row.name) if pd.notna(row.name) else "",
        }
        for row in stations_frame.itertuples(index=False)
    ]
    return links, stations


def build_month_assets(
    year: int,
    month: int,
    links: list[dict[str, object]],
) -> dict[str, object]:
    key = f"{year:04d}_{month:02d}"
    speed_path = TRAFFIC_DIR / f"speed_{key}.csv.gz"
    incident_path = INCIDENT_DIR / f"incidents_{key}.csv.gz"
    weather_path = WEATHER_DIR / f"weather_{key}.csv.gz"
    context_summary_path = SUMMARY_DIR / f"context_{key}.json"
    traffic_summary_path = SUMMARY_DIR / f"month_{key}.json"
    ensure_local_files(
        [
            speed_path,
            incident_path,
            weather_path,
            context_summary_path,
            traffic_summary_path,
        ]
    )
    header = pd.read_csv(speed_path, nrows=0).columns.tolist()
    expected = ["time"] + [f"link_{link['id']}" for link in links]
    if header != expected:
        raise ValueError(f"{speed_path.name} does not match the fixed network")
    dtype = {column: "float32" for column in header[1:]}
    frame = pd.read_csv(speed_path, dtype=dtype)
    times = pd.to_datetime(frame.pop("time"), errors="raise")
    speeds = frame.to_numpy(dtype=np.float32, copy=False)
    encoded = np.full(speeds.shape, 255, dtype=np.uint8)
    valid = np.isfinite(speeds) & (speeds >= 1) & (speeds <= 100)
    encoded[valid] = np.rint(speeds[valid]).astype(np.uint8)
    baseline = compute_weekslot_baseline(speeds, times)
    encoded_baseline = np.full(baseline.shape, 255, dtype=np.uint8)
    baseline_valid = np.isfinite(baseline)
    encoded_baseline[baseline_valid] = np.clip(
        np.rint(baseline[baseline_valid]), 0, 254
    ).astype(np.uint8)
    speed_asset = ASSET_DIR / f"speed_{key}.binz"
    baseline_asset = ASSET_DIR / f"baseline_{key}.binz"
    write_gzip_bytes(speed_asset, encoded)
    write_gzip_bytes(baseline_asset, encoded_baseline)

    incidents = pd.read_csv(incident_path, dtype={"incident_id": "string", "district": "string"})
    if not incidents.empty:
        incidents["timestamp"] = pd.to_datetime(incidents["timestamp"], errors="coerce")
        incidents["end_time"] = pd.to_datetime(incidents["end_time"], errors="coerce")
    incident_payload, incident_buckets = prepare_incident_payload(incidents, pd.DatetimeIndex(times))
    weather = pd.read_csv(weather_path, dtype={"station": "string"})
    weather_payload, weather_stations = prepare_weather_payload(weather, pd.DatetimeIndex(times))
    assignments = assign_links_to_weather_stations(links, weather_stations)

    context_summary = json.loads(context_summary_path.read_text(encoding="utf-8"))
    traffic_summary = json.loads(traffic_summary_path.read_text(encoding="utf-8"))
    context = {
        "timeLabels": [timestamp.strftime("%Y-%m-%d %H:%M") for timestamp in times],
        "initialIndex": min(len(times) - 1, 15 * 288 + 16 * 12),
        "incidents": incident_payload,
        "incidentsByTime": incident_buckets,
        "weather": weather_payload,
        "linkWeather": [assignments.get(int(link["id"])) for link in links],
        "summary": {
            "incidents": len(incidents),
            "matchedIncidents": int(incidents["link_id"].notna().sum()) if "link_id" in incidents else 0,
            "weatherStations": len(weather_payload),
            "trafficMissing": traffic_summary["inventory"]["missing"],
            "incidentSourceAvailable": context_summary["incidents"]["source_available"],
            "weatherSourceAvailable": context_summary["weather"]["source_available"],
        },
    }
    context_path = ASSET_DIR / f"context_{key}.jsonz"
    write_gzip_text(
        context_path,
        json.dumps(context, separators=(",", ":"), ensure_ascii=False),
    )
    return {
        "key": key,
        "label": f"{year:04d}-{month:02d}",
        "speed": f"data/{speed_asset.name}",
        "baseline": f"data/{baseline_asset.name}",
        "context": f"data/{context_path.name}",
        "timeSteps": len(times),
    }


HTML_TEMPLATE = r'''<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="icon" href="data:,">
  <title>LA Freeway Speeds, Incidents, and Weather · 2024–2026</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <style>
    html, body { height: 100%; margin: 0; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; color: #172033; }
    .page { display: grid; grid-template-columns: minmax(0, 1fr) 370px; height: 100vh; width: 100vw; overflow: hidden; background: #eef2f6; }
    .map-wrap { position: relative; height: 100vh; min-width: 0; overflow: hidden; }
    #map { height: 100%; width: 100%; }
    aside { background: #fff; border-left: 1px solid #d8dee8; padding: 20px; overflow: auto; box-sizing: border-box; }
    h1 { margin: 0 0 6px; font-size: 24px; line-height: 1.15; color: #18243c; letter-spacing: 0; }
    .sub { color: #667085; font-size: 13px; margin-bottom: 16px; }
    .field { margin: 13px 0; }
    .field-title { display: block; font-size: 12px; font-weight: 700; color: #475569; margin-bottom: 6px; }
    input[type="range"] { width: 100%; box-sizing: border-box; }
    button, select { height: 34px; border: 1px solid #bdc7d6; background: #f8fafc; border-radius: 4px; padding: 0 10px; color: #172033; }
    #monthSelect, #incidentFilter { width: 100%; box-sizing: border-box; }
    #loadState { display: block; margin: 6px 0 0; }
    button { cursor: pointer; }
    button:hover { background: #eef3f8; }
    .time-row { display: flex; gap: 8px; align-items: center; }
    #timeText { font-weight: 750; font-size: 17px; color: #172033; margin-top: 4px; }
    .stats { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin: 14px 0 16px; }
    .stat { border: 1px solid #d8dee8; border-radius: 4px; padding: 9px; background: #f8fafc; min-height: 48px; }
    .stat b { display: block; font-size: 20px; color: #172033; }
    .stat span { font-size: 11px; color: #64748b; }
    label { display: flex; gap: 8px; align-items: center; color: #334155; font-size: 14px; margin: 8px 0; }
    .weather-box { border: 1px solid #d8dee8; border-radius: 4px; padding: 10px; background: #f8fafc; font-size: 13px; line-height: 1.55; }
    .weather-box b { color: #172033; }
    .weather-key { display: flex; gap: 14px; flex-wrap: wrap; margin-top: 7px; color: #64748b; font-size: 11px; }
    .weather-key span { display: inline-flex; align-items: center; gap: 5px; }
    .weather-dot { width: 9px; height: 9px; border-radius: 50%; border: 1px solid #526075; background: #f8fafc; }
    .weather-dot.rain { border-color: #0754a6; background: #1687e8; }
    .event-list { max-height: 180px; overflow: auto; border-top: 1px solid #e2e8f0; }
    .event-row { padding: 8px 2px; border-bottom: 1px solid #e2e8f0; font-size: 12px; cursor: pointer; }
    .event-row:hover { background: #f8fafc; }
    .event-row b { display: block; color: #172033; }
    details { border-top: 1px solid #d8dee8; padding: 10px 0; font-size: 12px; color: #526075; line-height: 1.5; }
    summary { cursor: pointer; font-weight: 700; color: #334155; }
    .gap-note { display: none; margin: 10px 0; padding: 8px; border: 1px solid #e2a43a; background: #fff8e8; border-radius: 4px; font-size: 12px; color: #6f4b08; }
    .legend, .ranking { position: absolute; z-index: 700; background: #fff; border: 1px solid rgba(15,23,42,.35); border-radius: 4px; box-shadow: 0 2px 8px rgba(15,23,42,.18); }
    .map-wrap.popup-open .legend, .map-wrap.popup-open .ranking, .map-wrap.popup-open .leaflet-control-zoom { opacity: 0; pointer-events: none; }
    .legend { left: 16px; bottom: 16px; padding: 9px; min-width: 356px; }
    .legend-row { display: flex; gap: 6px; align-items: center; }
    .swatch { width: 43px; height: 25px; border-radius: 4px; border: 1px solid rgba(15,23,42,.25); display: flex; align-items: center; justify-content: center; font-size: 11px; font-weight: 800; }
    .swatch.red { color: #fff; }
    .legend-note { font-size: 12px; font-weight: 700; color: #334155; margin-top: 5px; }
    .legend-info { display: grid; grid-template-columns: 1fr 1fr; gap: 4px 10px; margin-top: 8px; font-size: 12px; color: #475569; }
    .legend-info b { color: #172033; }
    .ranking { right: 16px; bottom: 16px; width: 310px; max-height: 330px; overflow: hidden; }
    .ranking-head { padding: 10px 11px 7px; border-bottom: 1px solid #d8dee8; }
    .ranking-head b { display: block; font-size: 14px; }
    .ranking-head span { color: #64748b; font-size: 11px; }
    .rank-list { max-height: 274px; overflow: auto; }
    .rank-row { display: grid; grid-template-columns: 25px minmax(0,1fr) auto; gap: 7px; align-items: center; padding: 7px 10px; border-bottom: 1px solid #edf0f4; cursor: pointer; }
    .rank-row:hover { background: #f4f7fa; }
    .rank-num { width: 22px; height: 22px; border-radius: 50%; background: #e8edf3; display: grid; place-items: center; font-size: 11px; font-weight: 800; }
    .rank-road { min-width: 0; font-size: 12px; }
    .rank-road b { display: block; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
    .rank-road span { color: #64748b; }
    .rank-value { text-align: right; font-size: 11px; color: #64748b; }
    .rank-value b { display: block; font-size: 13px; color: #b42318; }
    .incident-diamond { width: 18px; height: 18px; background: #c5372d; border: 2px solid #fff; transform: rotate(45deg); box-shadow: 0 1px 4px rgba(0,0,0,.4); display: grid; place-items: center; }
    .incident-diamond span { transform: rotate(-45deg); color: #fff; font-size: 11px; font-weight: 900; line-height: 1; }
    .popup { font-size: 13px; line-height: 1.4; min-width: 210px; }
    .leaflet-popup-pane { z-index: 1100; }
    .incident-popup { width: 350px; max-width: calc(100vw - 88px); max-height: calc(100vh - 110px); overflow-x: hidden; overflow-y: auto; padding-right: 3px; }
    .incident-description { margin: 3px 0 2px; font-weight: 650; color: #172033; }
    .popup-grid { display: grid; grid-template-columns: auto auto; gap: 2px 12px; margin-top: 6px; }
    .incident-notes { margin-top: 7px; padding-top: 6px; border-top: 1px solid #d8dee8; }
    .incident-notes ul { margin: 4px 0 0; padding-left: 18px; }
    .incident-source { margin-top: 7px; padding: 6px 0 0; }
    .incident-source summary { font-size: 11px; }
    .incident-source div { margin-top: 4px; overflow-wrap: anywhere; }
    .speed-chart { margin-top: 9px; padding-top: 8px; border-top: 1px solid #d8dee8; }
    .speed-chart-title { font-size: 12px; font-weight: 750; color: #172033; }
    .speed-chart svg { display: block; width: 100%; height: auto; margin-top: 3px; overflow: visible; }
    .speed-chart-note, .speed-chart-empty { color: #64748b; font-size: 10px; line-height: 1.35; }
    .speed-chart-empty { padding: 8px 0 2px; }
    .association-note { margin-top: 7px; color: #64748b; font-size: 11px; }
    .leaflet-container { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; }
    @media (max-width: 900px) {
      .page { grid-template-columns: 1fr; grid-template-rows: 68vh 32vh; }
      .map-wrap { height: 68vh; }
      aside { height: 32vh; border-left: 0; border-top: 1px solid #d8dee8; }
      .ranking { width: min(280px, calc(100vw - 32px)); bottom: 92px; max-height: 220px; }
      .rank-list { max-height: 165px; }
      .legend { left: 10px; right: 10px; bottom: 10px; min-width: 0; }
      .swatch { flex: 1; width: auto; font-size: 9px; }
      .legend-info { display: none; }
      .incident-popup { max-height: calc(68vh - 90px); }
    }
  </style>
</head>
<body>
<div class="page">
  <div class="map-wrap">
    <div id="map"></div>
    <div class="legend">
      <div class="legend-row">
        <div class="swatch" style="background:#16a43a">60+</div><div class="swatch" style="background:#7ad151">55-59</div><div class="swatch" style="background:#f7ea2a">50-54</div><div class="swatch" style="background:#ffc928">45-49</div><div class="swatch" style="background:#f5a623">40-44</div><div class="swatch" style="background:#f36b2b">35-39</div><div class="swatch red" style="background:#f01818">≤35</div>
      </div>
      <div class="legend-note">Speed, mph</div>
      <div class="legend-info">
        <div>Time: <b id="legendTime"></b></div><div>Average: <b id="legendAvg"></b></div><div>Active incidents: <b id="legendIncidents"></b></div><div>Highest 1h rain: <b id="legendRain"></b></div>
      </div>
    </div>
    <div class="ranking" id="congestionRanking">
      <div class="ranking-head"><b>Most congested now</b><span>Normal speed minus current speed</span></div>
      <div class="rank-list" id="rankList"></div>
    </div>
  </div>
  <aside>
    <h1>LA Freeway Speeds and Incidents</h1>
    <div class="sub">PeMS 5-minute speeds · CHP incidents · NOAA weather · July 2024–June 2026</div>
    <div class="field">
      <span class="field-title">Month</span>
      <select id="monthSelect"></select>
      <span id="loadState" class="sub"></span>
    </div>
    <div class="field">
      <span class="field-title">Time</span>
      <div class="time-row"><button id="playBtn">Play</button><button id="resetBtn">Reset</button></div>
      <input id="timeSlider" type="range" min="0" max="__MAX_INDEX__" step="1" value="__INITIAL_INDEX__">
      <div id="timeText"></div>
    </div>
    <div id="gapNote" class="gap-note"></div>
    <div class="stats">
      <div class="stat"><b id="segmentCount"></b><span>links with speed / fixed links</span></div>
      <div class="stat"><b id="avgSpeed"></b><span>average speed</span></div>
      <div class="stat"><b id="activeIncidentCount"></b><span>active incidents</span></div>
      <div class="stat"><b id="rainStationCount"></b><span>weather stations reporting rain</span></div>
    </div>
    <div class="field">
      <span class="field-title">Weather</span>
      <div class="weather-box" id="weatherSummary"></div>
      <div class="weather-key"><span><i class="weather-dot rain"></i>rain in previous hour</span><span><i class="weather-dot"></i>no rain / unavailable</span></div>
    </div>
    <div class="field">
      <span class="field-title">Layers and incident filter</span>
      <label><input id="toggleIncidents" type="checkbox" checked> active incidents</label>
      <label><input id="toggleWeatherStations" type="checkbox" checked> NOAA weather stations</label>
      <label><input id="toggleStations" type="checkbox"> PeMS station points</label>
      <label><input id="toggleCasing" type="checkbox"> road casing</label>
      <select id="incidentFilter">
        <option value="All">All incident types</option><option value="Collision">Collisions</option><option value="Disabled vehicle">Disabled vehicles</option><option value="Traffic hazard">Traffic hazards</option><option value="Fire">Fire</option><option value="Construction / closure">Construction / closure</option><option value="Other">Other</option>
      </select>
    </div>
    <div class="field">
      <span class="field-title">Active incidents</span>
      <div class="event-list" id="eventList"></div>
    </div>
    <details>
      <summary>Data and indicators</summary>
      <p>Road speed uses PeMS Station 5-Minute records from D7, D8, and D12. Only mainline stations are included.</p>
      <p>Normal speed is the median for the same link, weekday, and five-minute time within the selected month. The ranking uses normal speed minus current speed and excludes missing or zero values.</p>
      <p>CHP incidents are matched by district, freeway, direction, and postmile. Coordinates are used when postmile matching is unavailable. CHP does not identify the exact lane.</p>
      <p>Incident speed compares the matched link during the event with the 30 minutes before, the 30 minutes after, and the usual speed for the same weekday and time. These are observed associations, not causal estimates.</p>
      <p>Weather comes from seven NOAA GHCNh airport stations. Each road link uses its nearest station. Rain is reported only when the previous-hour window is complete or NOAA supplies a separate reported total.</p>
      <p>Months or district-days missing from the downloaded archives are identified beside the time controls.</p>
    </details>
  </aside>
</div>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
const config = __CONFIG_JSON__;
const data = { ...config, timeLabels: [], incidents: [], incidentsByTime: [], weather: [], linkWeather: [], speed: null, baseline: null, summary: {} };
function decodeBytes(value) { const raw = atob(value); const bytes = new Uint8Array(raw.length); for (let i = 0; i < raw.length; i += 1) bytes[i] = raw.charCodeAt(i); return bytes; }
async function fetchGzipBytes(url) { const response = await fetch(url); if (!response.ok) throw new Error(`${response.status} ${url}`); const compressed = response.body; if (!compressed || typeof DecompressionStream === "undefined") throw new Error("This browser cannot read gzip map data."); const stream = compressed.pipeThrough(new DecompressionStream("gzip")); return new Uint8Array(await new Response(stream).arrayBuffer()); }

const colors = { high: "#16a43a", c55: "#7ad151", c50: "#f7ea2a", c45: "#ffc928", c40: "#f5a623", c35: "#f36b2b", low: "#f01818", missing: "#94a3b8", weatherRain: "#1687e8", weatherDry: "#f8fafc" };
const map = L.map("map", { preferCanvas: true, zoomSnap: 0.25, zoomDelta: 0.25 });
L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", { maxZoom: 19, attribution: "&copy; OpenStreetMap contributors" }).addTo(map);
map.setView(data.center, data.zoom);

const casingLayer = L.layerGroup();
const speedLayer = L.layerGroup().addTo(map);
const stationLayer = L.layerGroup();
const incidentLayer = L.layerGroup().addTo(map);
const incidentHighlightLayer = L.layerGroup().addTo(map);
const weatherStationLayer = L.layerGroup().addTo(map);
const linkById = new Map(data.links.map(link => [link.id, link]));
const weatherById = new Map(data.weather.map(station => [station.station, station]));
const lineById = new Map();
const markerByIncidentId = new Map();
let activeTime = 0;
let playing = false;
let timer = null;

function speedColor(value) { if (value == null || value === 255) return colors.missing; if (value >= 60) return colors.high; if (value >= 55) return colors.c55; if (value >= 50) return colors.c50; if (value >= 45) return colors.c45; if (value >= 40) return colors.c40; if (value >= 35) return colors.c35; return colors.low; }
function speedAt(link, index) { if (!data.speed) return null; const value = data.speed[index * data.links.length + link.id]; return value === 255 ? null : value; }
function weekslot(index) { const label = data.timeLabels[index]; const date = new Date(label.replace(" ", "T")); const weekday = (date.getDay() + 6) % 7; return weekday * 288 + date.getHours() * 12 + Math.floor(date.getMinutes() / 5); }
function normalAt(link, index) { if (!data.baseline) return null; const value = data.baseline[weekslot(index) * data.links.length + link.id]; return value === 255 ? null : value; }
function weatherValue(bytes, index, scale, offset = 0) { const value = bytes[index]; return value === 255 ? null : value / scale - offset; }
function weatherAt(link, index) { const station = weatherById.get(link.weatherStation); if (!station) return null; return { station, temperature: weatherValue(station.temperature, index, 2, 30), wind: weatherValue(station.wind, index, 5), visibility: weatherValue(station.visibility, index, 2), rain: weatherValue(station.rainHour, index, 5) }; }
function weatherAtStation(station, index) { return { station, temperature: weatherValue(station.temperature, index, 2, 30), wind: weatherValue(station.wind, index, 5), visibility: weatherValue(station.visibility, index, 2), rain: weatherValue(station.rainHour, index, 5) }; }
function directionSide(direction) { const dir = String(direction || "").toUpperCase(); if (dir === "N" || dir === "E") return 1; if (dir === "S" || dir === "W") return -1; return 0; }
function canonicalAxis(direction) { const dir = String(direction || "").toUpperCase(); if (dir === "N" || dir === "S") return [0, -1]; return [1, 0]; }
function styleForZoom() { const z = map.getZoom(); if (z < 8.8) return { offset: .4, flowWeight: 2, casingWeight: 0, opacity: .84, incidentSize: 13 }; if (z < 9.6) return { offset: 2.4, flowWeight: 2.8, casingWeight: 0, opacity: .88, incidentSize: 15 }; if (z < 10.6) return { offset: 4.8, flowWeight: 3.5, casingWeight: 4.8, opacity: .91, incidentSize: 17 }; if (z < 11.6) return { offset: 6.2, flowWeight: 4.3, casingWeight: 5.8, opacity: .94, incidentSize: 18 }; return { offset: 8, flowWeight: 5.2, casingWeight: 6.8, opacity: .95, incidentSize: 20 }; }
function drawCoordsFor(link) { const style = styleForZoom(); if (!link.coords || link.coords.length < 2 || style.offset === 0) return link.coords; const start = map.latLngToLayerPoint(link.coords[0]); const end = map.latLngToLayerPoint(link.coords[link.coords.length - 1]); let dx = end.x - start.x; let dy = end.y - start.y; const len = Math.hypot(dx, dy); if (!len) return link.coords; let nx = -dy / len; let ny = dx / len; const axis = canonicalAxis(link.direction); if (dx * axis[0] + dy * axis[1] < 0) { nx *= -1; ny *= -1; } const offset = style.offset * directionSide(link.direction); return link.coords.map(coord => { const point = map.latLngToLayerPoint(coord); return map.layerPointToLatLng(L.point(point.x + nx * offset, point.y + ny * offset)); }); }
function routeLabel(link) { return `${link.freeway} ${link.direction}`; }
function escapeHtml(value) { return String(value ?? "").replace(/[&<>"']/g, character => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#039;" }[character])); }
function metric(value) { return value == null ? "NA" : `${Number(value).toFixed(1)} mph`; }
function observedChange(incident) { if (incident.speedDrop == null) return "NA"; if (incident.speedDrop > 0) return `${incident.speedDrop.toFixed(1)} mph (${incident.speedDropPct == null ? "NA" : incident.speedDropPct.toFixed(1) + "%"})`; if (incident.speedDrop < 0) return `${Math.abs(incident.speedDrop).toFixed(1)} mph faster`; return "No change"; }
function normalDifference(incident) { if (incident.deficitVsNormal == null) return "NA"; if (incident.deficitVsNormal > 0) return `${incident.deficitVsNormal.toFixed(1)} mph slower`; if (incident.deficitVsNormal < 0) return `${Math.abs(incident.deficitVsNormal).toFixed(1)} mph faster`; return "At normal speed"; }

function incidentSpeedChart(incident) {
  const link = linkById.get(incident.linkId);
  const center = Number(incident.reportedIndex);
  if (!link || !Number.isInteger(center) || !data.speed) return '<div class="speed-chart"><div class="speed-chart-title">Matched-link speed, ±2 hours</div><div class="speed-chart-empty">No matched five-minute speed series is available for this incident.</div></div>';

  const observations = [];
  for (let step = -24; step <= 24; step += 1) {
    const index = center + step;
    if (index < 0 || index >= data.timeLabels.length) continue;
    const speed = speedAt(link, index);
    observations.push({ minute: step * 5, speed });
  }
  const valid = observations.filter(point => point.speed != null);
  if (valid.length < 2) return '<div class="speed-chart"><div class="speed-chart-title">Matched-link speed, ±2 hours</div><div class="speed-chart-empty">Not enough valid five-minute speeds to draw the event window.</div></div>';

  const width = 350;
  const height = 186;
  const margin = { top: 12, right: 10, bottom: 38, left: 44 };
  const plotWidth = width - margin.left - margin.right;
  const plotHeight = height - margin.top - margin.bottom;
  const maxObserved = Math.max(...valid.map(point => point.speed));
  const yMax = Math.max(60, Math.min(100, Math.ceil(maxObserved / 10) * 10));
  const x = minute => margin.left + ((minute + 120) / 240) * plotWidth;
  const y = speed => margin.top + (1 - speed / yMax) * plotHeight;

  let path = "";
  let previousMinute = null;
  for (const point of observations) {
    if (point.speed == null) { previousMinute = null; continue; }
    const command = previousMinute === point.minute - 5 ? "L" : "M";
    path += `${command}${x(point.minute).toFixed(1)},${y(point.speed).toFixed(1)} `;
    previousMinute = point.minute;
  }

  const xTicks = [[-120, "-2h"], [-60, "-1h"], [0, "Event"], [60, "+1h"], [120, "+2h"]];
  const yTicks = [0, yMax / 2, yMax];
  const grid = yTicks.map(value => `<line x1="${margin.left}" y1="${y(value)}" x2="${width - margin.right}" y2="${y(value)}" stroke="#d8dee8" stroke-width="1"/><text x="${margin.left - 7}" y="${y(value) + 3}" text-anchor="end" fill="#64748b" font-size="9">${Math.round(value)}</text>`).join("");
  const labels = xTicks.map(([minute, label]) => `<line x1="${x(minute)}" y1="${height - margin.bottom}" x2="${x(minute)}" y2="${height - margin.bottom + 4}" stroke="#64748b"/><text x="${x(minute)}" y="${height - margin.bottom + 15}" text-anchor="middle" fill="${minute === 0 ? "#b42318" : "#64748b"}" font-size="9" font-weight="${minute === 0 ? "700" : "400"}">${label}</text>`).join("");
  const duration = incident.duration == null ? 0 : Math.max(0, Math.min(120, Number(incident.duration)));
  const durationBand = duration > 0 ? `<rect x="${x(0)}" y="${margin.top}" width="${Math.max(1, x(duration) - x(0))}" height="${plotHeight}" fill="#f04438" opacity=".08"/>` : "";
  const partial = observations.length < 49 ? " The window is shorter at the edge of a month." : "";
  return `<div class="speed-chart"><div class="speed-chart-title">Matched-link speed, ±2 hours</div><svg viewBox="0 0 ${width} ${height}" role="img" aria-label="Average speed (mph) by minutes from report">${durationBand}${grid}<line x1="${margin.left}" y1="${height - margin.bottom}" x2="${width - margin.right}" y2="${height - margin.bottom}" stroke="#64748b"/><line x1="${x(0)}" y1="${margin.top}" x2="${x(0)}" y2="${height - margin.bottom}" stroke="#b42318" stroke-width="1.5" stroke-dasharray="3 3"/><path d="${path.trim()}" fill="none" stroke="#155eef" stroke-width="2.25" stroke-linecap="round" stroke-linejoin="round"/>${labels}<text x="${margin.left + plotWidth / 2}" y="${height - 3}" text-anchor="middle" fill="#475569" font-size="9">Minutes from report</text><text x="11" y="${margin.top + plotHeight / 2}" text-anchor="middle" fill="#475569" font-size="9" transform="rotate(-90 11 ${margin.top + plotHeight / 2})">Average speed (mph)</text></svg><div class="speed-chart-note">Five-minute average on the matched directional road link.${partial}</div></div>`;
}

function linkPopup(link) { const speed = speedAt(link, activeTime); const normal = normalAt(link, activeTime); const deficit = speed != null && normal != null ? Math.max(0, normal - speed) : null; const weather = weatherAt(link, activeTime); const activeIds = data.incidentsByTime[activeTime] || []; const eventCount = activeIds.filter(id => data.incidents[id].linkId === link.id).length; return `<div class="popup"><b>${routeLabel(link)}</b><br>${link.from_station} → ${link.to_station}<br>${data.timeLabels[activeTime]}<div class="popup-grid"><span>Speed</span><b>${speed == null ? "NA" : speed + " mph"}</b><span>Normal</span><b>${normal == null ? "NA" : normal + " mph"}</b><span>Deficit</span><b>${deficit == null ? "NA" : deficit + " mph"}</b><span>Active incidents</span><b>${eventCount}</b><span>Nearest weather</span><b>${weather ? weather.station.name : "NA"}</b><span>Rain, previous hour</span><b>${weather && weather.rain != null ? weather.rain.toFixed(1) + " mm" : "NA"}</b></div></div>`; }

function drawMap() { casingLayer.clearLayers(); speedLayer.clearLayers(); lineById.clear(); const style = styleForZoom(); for (const link of data.links) { const coords = drawCoordsFor(link); if (style.casingWeight > 0) L.polyline(coords, { color: "#1f3329", weight: style.casingWeight, opacity: .32, lineCap: "round", lineJoin: "round", interactive: false }).addTo(casingLayer); const value = speedAt(link, activeTime); const line = L.polyline(coords, { color: speedColor(value), weight: style.flowWeight, opacity: style.opacity, lineCap: "round", lineJoin: "round" }).addTo(speedLayer); line._link = link; line.bindPopup(() => linkPopup(link)); lineById.set(link.id, line); } updateTime(false); }
function drawStations() { stationLayer.clearLayers(); for (const station of data.stations) L.circleMarker([station.lat, station.lng], { radius: 2.1, color: "#fff", weight: 1, fillColor: "#334155", fillOpacity: .8 }).bindPopup(`<div class="popup"><b>Station ${station.station}</b><br>${station.freeway} ${station.direction}<br>D${station.district}<br>${station.name || ""}</div>`).addTo(stationLayer); }
function weatherStationPopup(station) { const weather = weatherAtStation(station, activeTime); return `<div class="popup"><b>${station.name}</b><br>NOAA ${station.station}<br>${data.timeLabels[activeTime]}<div class="popup-grid"><span>Temperature</span><b>${weather.temperature == null ? "NA" : weather.temperature.toFixed(1) + " °C"}</b><span>Wind</span><b>${weather.wind == null ? "NA" : weather.wind.toFixed(1) + " m/s"}</b><span>Visibility</span><b>${weather.visibility == null ? "NA" : weather.visibility.toFixed(1) + " km"}</b><span>Rain, previous hour</span><b>${weather.rain == null ? "NA" : weather.rain.toFixed(1) + " mm"}</b></div></div>`; }
function drawWeatherStations() { weatherStationLayer.clearLayers(); for (const station of data.weather) { const weather = weatherAtStation(station, activeTime); const rain = weather.rain; const raining = rain != null && rain > 0; L.circleMarker([station.lat, station.lng], { radius: raining ? 8 : 6, color: raining ? "#0754a6" : "#526075", weight: raining ? 2 : 1.5, fillColor: rain != null && rain > 0 ? colors.weatherRain : colors.weatherDry, fillOpacity: .96 }).bindTooltip(station.name, { direction: "top", offset: [0, -7], opacity: .94 }).bindPopup(() => weatherStationPopup(station)).addTo(weatherStationLayer); } }

function incidentPopup(incident) {
  const link = linkById.get(incident.linkId);
  const weather = link ? weatherAt(link, activeTime) : null;
  const roadMatch = link ? `${escapeHtml(incident.freeway)} ${escapeHtml(incident.direction)}` : "No reliable speed link";
  const association = link ? "Observed association on the matched road link; it does not establish that the incident caused the speed change." : "No speed comparison is calculated because the event could not be matched to a nearby observed road link.";
  const notes = incident.displayNotes && incident.displayNotes.length
    ? `<div class="incident-notes"><b>Incident details</b><ul>${incident.displayNotes.map(note => `<li>${escapeHtml(note)}</li>`).join("")}</ul></div>`
    : "";
  const source = incident.description && incident.description !== incident.displayDescription
    ? `<details class="incident-source"><summary>Original CHP description</summary><div>${escapeHtml(incident.description)}</div></details>`
    : "";
  return `<div class="popup incident-popup"><b>${escapeHtml(incident.category)}</b><div class="incident-description">${escapeHtml(incident.displayDescription || incident.description)}</div><div>${escapeHtml(incident.location || incident.area)}</div>${notes}<div class="popup-grid"><span>Reported</span><b>${incident.timestamp}</b><span>Ended</span><b>${incident.endTime}</b><span>Duration</span><b>${incident.duration == null ? "NA" : incident.duration + " min"}${incident.durationDefault ? " (default)" : ""}</b><span>Matched road</span><b>${roadMatch}</b><span>30 min before</span><b>${metric(incident.speedBefore)}</b><span>During incident</span><b>${metric(incident.speedDuring)}</b><span>30 min after</span><b>${metric(incident.speedAfter)}</b><span>Observed slowdown</span><b>${observedChange(incident)}</b><span>Normal at event time</span><b>${metric(incident.normalDuring)}</b><span>Compared with normal</span><b>${normalDifference(incident)}</b><span>Samples, before/during/after</span><b>${incident.samplesBefore}/${incident.samplesDuring}/${incident.samplesAfter}</b><span>Rain, previous hour</span><b>${weather && weather.rain != null ? weather.rain.toFixed(1) + " mm" : "NA"}</b></div>${incidentSpeedChart(incident)}<div class="association-note">${association}</div>${source}</div>`;
}
function drawIncidents() { incidentLayer.clearLayers(); incidentHighlightLayer.clearLayers(); markerByIncidentId.clear(); const enabled = document.getElementById("toggleIncidents").checked; const category = document.getElementById("incidentFilter").value; const activeIds = data.incidentsByTime[activeTime] || []; const visible = activeIds.map(id => data.incidents[id]).filter(event => category === "All" || event.category === category); if (enabled) { for (const incident of visible) { const link = linkById.get(incident.linkId); if (link) L.polyline(drawCoordsFor(link), { color: "#5b1621", weight: styleForZoom().flowWeight + 4, opacity: .72, dashArray: "3 7", interactive: false }).addTo(incidentHighlightLayer); if (incident.lat == null || incident.lng == null) continue; const size = styleForZoom().incidentSize; const icon = L.divIcon({ className: "", html: '<div class="incident-diamond"><span>!</span></div>', iconSize: [size, size], iconAnchor: [size / 2, size / 2] }); const marker = L.marker([incident.lat, incident.lng], { icon }).bindPopup(() => incidentPopup(incident), { maxWidth: 390, minWidth: 280 }).addTo(incidentLayer); markerByIncidentId.set(incident.id, marker); } } renderEventList(visible); return visible; }
function renderEventList(events) { const list = document.getElementById("eventList"); if (!events.length) { list.innerHTML = '<div class="event-row">No active incidents in the selected layer.</div>'; return; } list.innerHTML = events.slice(0, 20).map(event => `<div class="event-row" data-incident="${event.id}"><b>${event.freeway} ${event.direction} · ${event.category}</b>${event.location || event.area}</div>`).join(""); for (const row of list.querySelectorAll("[data-incident]")) row.addEventListener("click", () => { const marker = markerByIncidentId.get(Number(row.dataset.incident)); if (marker) { map.setView(marker.getLatLng(), Math.max(map.getZoom(), 12)); marker.openPopup(); } }); }

function updateRanking() { const rows = []; for (const link of data.links) { const speed = speedAt(link, activeTime); const normal = normalAt(link, activeTime); if (speed == null || speed <= 0 || normal == null) continue; const deficit = Math.max(0, normal - speed); if (deficit < 3 || speed >= 60) continue; const weather = weatherAt(link, activeTime); const incident = (data.incidentsByTime[activeTime] || []).some(id => data.incidents[id].linkId === link.id); rows.push({ link, speed, normal, deficit, rain: weather ? weather.rain : null, incident }); } rows.sort((a, b) => b.deficit - a.deficit || a.speed - b.speed || a.link.id - b.link.id); const top = rows.slice(0, 8); const list = document.getElementById("rankList"); if (!top.length) { list.innerHTML = '<div class="rank-row"><div class="rank-road">No material speed deficit</div></div>'; return; } list.innerHTML = top.map((row, i) => `<div class="rank-row" data-link="${row.link.id}"><div class="rank-num">${i + 1}</div><div class="rank-road"><b>${routeLabel(row.link)} · ${row.link.from_station}–${row.link.to_station}</b><span>${row.incident ? "incident · " : ""}${row.rain == null ? "weather unavailable" : row.rain > 0 ? row.rain.toFixed(1) + " mm rain" : "dry report"}</span></div><div class="rank-value"><b>−${Math.round(row.deficit)} mph</b>${row.speed} mph now</div></div>`).join(""); for (const item of list.querySelectorAll("[data-link]")) item.addEventListener("click", () => { const line = lineById.get(Number(item.dataset.link)); if (line) { map.fitBounds(line.getBounds(), { maxZoom: 13, padding: [60, 60] }); line.openPopup(); } }); }

function updateWeatherSummary() { const observations = data.weather.map(station => ({ station, temperature: weatherValue(station.temperature, activeTime, 2, 30), wind: weatherValue(station.wind, activeTime, 5), visibility: weatherValue(station.visibility, activeTime, 2), rain: weatherValue(station.rainHour, activeTime, 5) })); const validTemps = observations.filter(item => item.temperature != null).map(item => item.temperature); const validWinds = observations.filter(item => item.wind != null).map(item => item.wind); const validVisibility = observations.filter(item => item.visibility != null).map(item => item.visibility); const validRain = observations.filter(item => item.rain != null).map(item => item.rain); const rainCount = observations.filter(item => item.rain != null && item.rain > 0).length; const median = values => { if (!values.length) return null; const sorted = [...values].sort((a,b) => a-b); return sorted[Math.floor(sorted.length / 2)]; }; const temperature = median(validTemps); const wind = median(validWinds); const visibility = validVisibility.length ? Math.min(...validVisibility) : null; const rain = validRain.length ? Math.max(...validRain) : null; document.getElementById("rainStationCount").textContent = rainCount; document.getElementById("weatherSummary").innerHTML = `<b>${rainCount} of ${data.weather.length}</b> stations reporting rain<br>Temperature: <b>${temperature == null ? "NA" : temperature.toFixed(1) + " °C"}</b><br>Median wind: <b>${wind == null ? "NA" : wind.toFixed(1) + " m/s"}</b><br>Lowest visibility: <b>${visibility == null ? "NA" : visibility.toFixed(1) + " km"}</b><br>Highest 1h rain: <b>${rain == null ? "NA" : rain.toFixed(1) + " mm"}</b>`; document.getElementById("legendRain").textContent = rain == null ? "NA" : rain.toFixed(1) + " mm"; }

function updateTime(restyle = true) {
  if (!data.speed || !data.timeLabels.length) return;
  activeTime = Number(document.getElementById("timeSlider").value);
  let total = 0;
  let count = 0;
  if (restyle) speedLayer.eachLayer(line => {
    const value = speedAt(line._link, activeTime);
    line.setStyle({ color: speedColor(value) });
    if (value != null) { total += value; count += 1; }
  });
  else for (const link of data.links) {
    const value = speedAt(link, activeTime);
    if (value != null) { total += value; count += 1; }
  }
  const avg = count ? Math.round(total / count) : null;
  const label = data.timeLabels[activeTime];
  const active = drawIncidents();
  document.getElementById("timeText").textContent = label;
  document.getElementById("segmentCount").textContent = `${count.toLocaleString()} / ${data.links.length.toLocaleString()}`;
  document.getElementById("avgSpeed").textContent = avg == null ? "NA" : `${avg} mph`;
  document.getElementById("activeIncidentCount").textContent = active.length;
  document.getElementById("legendTime").textContent = label;
  document.getElementById("legendAvg").textContent = avg == null ? "NA" : `${avg} mph`;
  document.getElementById("legendIncidents").textContent = active.length;
  updateWeatherSummary();
  drawWeatherStations();
  updateRanking();
}

function updateGapNote() {
  const messages = [];
  if (data.summary.trafficMissing && data.summary.trafficMissing.length) messages.push(`Missing Station 5-Minute files: ${data.summary.trafficMissing.join(", ")}`);
  if (data.summary.incidentSourceAvailable === false) messages.push("CHP incident archive is unavailable for this month.");
  if (data.summary.weatherSourceAvailable === false) messages.push("NOAA weather is unavailable for this month.");
  const note = document.getElementById("gapNote");
  note.textContent = messages.join(" ");
  note.style.display = messages.length ? "block" : "none";
}

async function loadMonth(key) {
  const month = config.months.find(item => item.key === key);
  if (!month) return;
  playing = false;
  clearInterval(timer);
  document.getElementById("playBtn").textContent = "Play";
  const state = document.getElementById("loadState");
  state.textContent = "Loading month…";
  document.getElementById("monthSelect").disabled = true;
  try {
    const [speed, baseline, contextBytes] = await Promise.all([
      fetchGzipBytes(month.speed),
      fetchGzipBytes(month.baseline),
      fetchGzipBytes(month.context),
    ]);
    const context = JSON.parse(new TextDecoder().decode(contextBytes));
    if (speed.length !== month.timeSteps * data.links.length) throw new Error("Monthly speed dimensions do not match the fixed network.");
    if (baseline.length !== 7 * 288 * data.links.length) throw new Error("Monthly baseline dimensions do not match the fixed network.");
    Object.assign(data, context, { speed, baseline });
    weatherById.clear();
    for (const station of data.weather) {
      station.temperature = decodeBytes(station.temperature);
      station.wind = decodeBytes(station.wind);
      station.visibility = decodeBytes(station.visibility);
      station.rainHour = decodeBytes(station.rainHour);
      weatherById.set(station.station, station);
    }
    data.links.forEach((link, index) => { link.weatherStation = data.linkWeather[index]; });
    const slider = document.getElementById("timeSlider");
    slider.max = data.timeLabels.length - 1;
    slider.value = data.initialIndex;
    activeTime = data.initialIndex;
    updateGapNote();
    drawMap();
    state.textContent = `${month.label} · ${data.timeLabels.length.toLocaleString()} time steps`;
  } catch (error) {
    state.textContent = `Could not load ${month.label}: ${error.message}`;
    throw error;
  } finally {
    document.getElementById("monthSelect").disabled = false;
  }
}

document.getElementById("timeSlider").addEventListener("input", () => updateTime(true));
document.getElementById("monthSelect").addEventListener("change", event => loadMonth(event.target.value));
document.getElementById("toggleStations").addEventListener("change", event => event.target.checked ? stationLayer.addTo(map) : stationLayer.remove());
document.getElementById("toggleWeatherStations").addEventListener("change", event => event.target.checked ? weatherStationLayer.addTo(map) : weatherStationLayer.remove());
document.getElementById("toggleCasing").addEventListener("change", event => event.target.checked ? casingLayer.addTo(map) : casingLayer.remove());
document.getElementById("toggleIncidents").addEventListener("change", () => updateTime(false));
document.getElementById("incidentFilter").addEventListener("change", () => updateTime(false));
document.getElementById("resetBtn").addEventListener("click", () => { document.getElementById("timeSlider").value = data.initialIndex; updateTime(true); });
document.getElementById("playBtn").addEventListener("click", () => { playing = !playing; document.getElementById("playBtn").textContent = playing ? "Pause" : "Play"; if (playing) timer = setInterval(() => { const slider = document.getElementById("timeSlider"); slider.value = Math.min(Number(slider.value) + 1, Number(slider.max)); if (slider.value === slider.max) { playing = false; document.getElementById("playBtn").textContent = "Play"; clearInterval(timer); } updateTime(true); }, 350); else clearInterval(timer); });
map.on("zoomend", () => { if (data.speed) drawMap(); });
map.on("popupopen", () => document.querySelector(".map-wrap").classList.add("popup-open"));
map.on("popupclose", () => document.querySelector(".map-wrap").classList.remove("popup-open"));
drawStations();
const monthSelect = document.getElementById("monthSelect");
monthSelect.innerHTML = config.months.map(month => `<option value="${month.key}">${month.label}</option>`).join("");
monthSelect.value = config.initialMonth;
loadMonth(config.initialMonth);
</script>
</body>
</html>
'''


def build_html(config: dict[str, object]) -> str:
    config_json = json.dumps(config, separators=(",", ":"), ensure_ascii=False)
    return (
        HTML_TEMPLATE.replace("__CONFIG_JSON__", config_json)
        .replace("__MAX_INDEX__", "0")
        .replace("__INITIAL_INDEX__", "0")
    )


def main() -> int:
    MAP_DIR.mkdir(parents=True, exist_ok=True)
    ASSET_DIR.mkdir(parents=True, exist_ok=True)
    periods = list(pd.period_range("2024-07", "2026-06", freq="M"))
    required = []
    for period in periods:
        key = f"{period.year:04d}_{period.month:02d}"
        required.extend(
            [
                TRAFFIC_DIR / f"speed_{key}.csv.gz",
                INCIDENT_DIR / f"incidents_{key}.csv.gz",
                WEATHER_DIR / f"weather_{key}.csv.gz",
                SUMMARY_DIR / f"month_{key}.json",
                SUMMARY_DIR / f"context_{key}.json",
            ]
        )
    missing = [path for path in required if not path.exists()]
    if missing:
        raise FileNotFoundError(
            "Monthly clean data is incomplete. Missing: " + ", ".join(str(path) for path in missing[:10])
        )

    links, stations = load_network()
    months = []
    for period in periods:
        print(f"MAP ASSETS {period.year:04d}-{period.month:02d}", flush=True)
        months.append(build_month_assets(period.year, period.month, links))
    config = {
        "source": "PeMS Station 5-Minute, CHP Incidents, NOAA GHCNh",
        "center": [34.03, -117.82],
        "zoom": 9.4,
        "links": links,
        "stations": stations,
        "months": months,
        "initialMonth": months[-1]["key"],
    }
    OUTPUT_HTML.write_text(build_html(config), encoding="utf-8")

    print(f"Speed links: {len(links):,}")
    print(f"Months: {len(months)}")
    print(f"Wrote {OUTPUT_HTML}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
