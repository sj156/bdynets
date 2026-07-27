#!/usr/bin/env python3
"""Build the April 2026 LA speed map with incidents, weather, and rankings."""

from __future__ import annotations

import base64
import json
from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
from traffic_context import build_incident_buckets


START = pd.Timestamp("2026-04-01 00:00:00")
END = pd.Timestamp("2026-04-30 23:55:00")
TIME_INDEX = pd.date_range(START, END, freq="5min")

CLEAN_DIR = ROOT / "clean_data"
TRAFFIC_CLEAN = CLEAN_DIR / "traffic_clean.json"
INCIDENTS_CLEAN = CLEAN_DIR / "incidents_clean.csv"
WEATHER_CLEAN = CLEAN_DIR / "weather_clean.csv"
PROCESSED = ROOT / "processed_incident_weather"
MAP_DIR = ROOT / "map"

OUTPUT_HTML = MAP_DIR / "la_area_freeway_speeds_incidents_weather_2026_04.html"
OUTPUT_JSON = PROCESSED / "la_link_context_2026_04.json"


def _encode_uint8(values: np.ndarray) -> str:
    return base64.b64encode(np.asarray(values, dtype=np.uint8).tobytes()).decode("ascii")


def _quantize(values: np.ndarray, scale: float = 1.0, offset: float = 0.0) -> np.ndarray:
    array = np.asarray(values, dtype=float)
    result = np.full(array.shape, 255, dtype=np.uint8)
    valid = np.isfinite(array)
    result[valid] = np.clip(np.rint((array[valid] + offset) * scale), 0, 254).astype(np.uint8)
    return result


def load_clean_traffic() -> dict:
    if not TRAFFIC_CLEAN.exists():
        raise FileNotFoundError(f"Run prepare_clean_data.py first: {TRAFFIC_CLEAN}")
    return json.loads(TRAFFIC_CLEAN.read_text(encoding="utf-8"))


def prepare_weather_payload(weather: pd.DataFrame) -> tuple[list[dict], pd.DataFrame]:
    frame = weather.copy()
    frame["time"] = pd.to_datetime(frame["time"], errors="coerce")
    frame = frame.dropna(subset=["time", "latitude", "longitude"])
    stations = frame[["station", "station_name", "latitude", "longitude"]].drop_duplicates("station")

    payload: list[dict] = []
    for station in stations.itertuples(index=False):
        group = frame[frame["station"] == station.station].copy()
        regular = group.set_index("time").reindex(TIME_INDEX)

        payload.append(
            {
                "station": str(station.station),
                "name": str(station.station_name),
                "lat": round(float(station.latitude), 6),
                "lng": round(float(station.longitude), 6),
                "temperature": _encode_uint8(_quantize(regular["temperature_c"].to_numpy(), 2, 30)),
                "wind": _encode_uint8(_quantize(regular["wind_speed_ms"].to_numpy(), 10)),
                "visibility": _encode_uint8(_quantize(regular["visibility_km"].to_numpy(), 10)),
                "rainHour": _encode_uint8(_quantize(regular["rain_hour_mm"].to_numpy(), 10)),
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


def prepare_incident_payload(incidents: pd.DataFrame) -> tuple[list[dict], list[list[int]]]:
    clean = incidents.copy().reset_index(drop=True)
    records: list[dict] = []
    for idx, row in clean.iterrows():
        has_link = pd.notna(row["link_id"])
        records.append(
            {
                "id": idx,
                "incidentId": str(row["incident_id"]),
                "timestamp": pd.Timestamp(row["timestamp"]).strftime("%Y-%m-%d %H:%M"),
                "endTime": pd.Timestamp(row["end_time"]).strftime("%Y-%m-%d %H:%M"),
                "description": str(row["description"]),
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
                "chpNotes": _notes_from_csv(row.get("chp_notes")),
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
                "sourceIncomplete": _as_bool(row["source_incomplete"]),
            }
        )
    return records, build_incident_buckets(clean, TIME_INDEX)


HTML_TEMPLATE = r'''<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>LA Freeway Speeds, Incidents, and Weather · April 2026</title>
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
    .popup-grid { display: grid; grid-template-columns: auto auto; gap: 2px 12px; margin-top: 6px; }
    .incident-notes { margin-top: 7px; padding-top: 6px; border-top: 1px solid #d8dee8; }
    .incident-notes ul { margin: 4px 0 0; padding-left: 18px; }
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
    <div class="sub">PeMS 5-minute speeds · CHP incidents · NOAA weather · April 2026</div>
    <div class="field">
      <span class="field-title">Time</span>
      <div class="time-row"><button id="playBtn">Play</button><button id="resetBtn">Reset</button></div>
      <input id="timeSlider" type="range" min="0" max="__MAX_INDEX__" step="1" value="__INITIAL_INDEX__">
      <div id="timeText"></div>
    </div>
    <div id="gapNote" class="gap-note">CHP incident archive is incomplete for 2026-04-03. Speed and weather remain available.</div>
    <div class="stats">
      <div class="stat"><b id="segmentCount"></b><span>road links</span></div>
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
        <option value="All">All incident types</option><option value="Collision">Collisions</option><option value="Traffic hazard">Traffic hazards</option><option value="Fire">Fire</option><option value="Construction / closure">Construction / closure</option><option value="Other">Other</option>
      </select>
    </div>
    <div class="field">
      <span class="field-title">Active incidents</span>
      <div class="event-list" id="eventList"></div>
    </div>
    <details>
      <summary>Data and indicators</summary>
      <p>Road speed uses PeMS Station 5-Minute records from D7, D8, and D12. Only mainline stations are included.</p>
      <p>Normal speed is the median for the same link, weekday, and five-minute time across April. The ranking uses normal speed minus current speed and excludes missing or zero values.</p>
      <p>CHP incidents are matched by district, freeway, direction, and postmile. Coordinates are used when postmile matching is unavailable. CHP does not identify the exact lane.</p>
      <p>Incident speed compares the matched link during the event with the 30 minutes before, the 30 minutes after, and the usual speed for the same weekday and time. These are observed associations, not causal estimates.</p>
      <p>Weather comes from seven NOAA GHCNh airport stations. Each road link uses its nearest station. Rain is reported only when the previous-hour window is complete or NOAA supplies a separate reported total.</p>
      <p>The PeMS CHP archive contains only one statewide summary record on 2026-04-03. That date is treated as incomplete rather than incident-free.</p>
    </details>
  </aside>
</div>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
const data = __PAYLOAD_JSON__;
function decodeBytes(value) { const raw = atob(value); const bytes = new Uint8Array(raw.length); for (let i = 0; i < raw.length; i += 1) bytes[i] = raw.charCodeAt(i); return bytes; }
for (const link of data.links) { link.values = decodeBytes(link.values); link.baseline = decodeBytes(link.baseline); }
for (const station of data.weather) { station.temperature = decodeBytes(station.temperature); station.wind = decodeBytes(station.wind); station.visibility = decodeBytes(station.visibility); station.rainHour = decodeBytes(station.rainHour); }

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
let activeTime = data.initialIndex;
let playing = false;
let timer = null;

function speedColor(value) { if (value == null || value === 255) return colors.missing; if (value >= 60) return colors.high; if (value >= 55) return colors.c55; if (value >= 50) return colors.c50; if (value >= 45) return colors.c45; if (value >= 40) return colors.c40; if (value >= 35) return colors.c35; return colors.low; }
function speedAt(link, index) { const value = link.values[index]; return value === 255 ? null : value; }
function weekslot(index) { const label = data.timeLabels[index]; const date = new Date(label.replace(" ", "T")); const weekday = (date.getDay() + 6) % 7; return weekday * 288 + date.getHours() * 12 + Math.floor(date.getMinutes() / 5); }
function normalAt(link, index) { const value = link.baseline[weekslot(index)]; return value === 255 ? null : value; }
function weatherValue(bytes, index, scale, offset = 0) { const value = bytes[index]; return value === 255 ? null : value / scale - offset; }
function weatherAt(link, index) { const station = weatherById.get(link.weatherStation); if (!station) return null; return { station, temperature: weatherValue(station.temperature, index, 2, 30), wind: weatherValue(station.wind, index, 10), visibility: weatherValue(station.visibility, index, 10), rain: weatherValue(station.rainHour, index, 10) }; }
function weatherAtStation(station, index) { return { station, temperature: weatherValue(station.temperature, index, 2, 30), wind: weatherValue(station.wind, index, 10), visibility: weatherValue(station.visibility, index, 10), rain: weatherValue(station.rainHour, index, 10) }; }
function directionSide(direction) { const dir = String(direction || "").toUpperCase(); if (dir === "N" || dir === "E") return 1; if (dir === "S" || dir === "W") return -1; return 0; }
function canonicalAxis(direction) { const dir = String(direction || "").toUpperCase(); if (dir === "N" || dir === "S") return [0, -1]; return [1, 0]; }
function styleForZoom() { const z = map.getZoom(); if (z < 8.8) return { offset: .4, flowWeight: 2, casingWeight: 0, opacity: .84, incidentSize: 13 }; if (z < 9.6) return { offset: 2.4, flowWeight: 2.8, casingWeight: 0, opacity: .88, incidentSize: 15 }; if (z < 10.6) return { offset: 4.8, flowWeight: 3.5, casingWeight: 4.8, opacity: .91, incidentSize: 17 }; if (z < 11.6) return { offset: 6.2, flowWeight: 4.3, casingWeight: 5.8, opacity: .94, incidentSize: 18 }; return { offset: 8, flowWeight: 5.2, casingWeight: 6.8, opacity: .95, incidentSize: 20 }; }
function drawCoordsFor(link) { const style = styleForZoom(); if (!link.coords || link.coords.length < 2 || style.offset === 0) return link.coords; const start = map.latLngToLayerPoint(link.coords[0]); const end = map.latLngToLayerPoint(link.coords[link.coords.length - 1]); let dx = end.x - start.x; let dy = end.y - start.y; const len = Math.hypot(dx, dy); if (!len) return link.coords; let nx = -dy / len; let ny = dx / len; const axis = canonicalAxis(link.direction); if (dx * axis[0] + dy * axis[1] < 0) { nx *= -1; ny *= -1; } const offset = style.offset * directionSide(link.direction); return link.coords.map(coord => { const point = map.latLngToLayerPoint(coord); return map.layerPointToLatLng(L.point(point.x + nx * offset, point.y + ny * offset)); }); }
function routeLabel(link) { return `${link.freeway} ${link.direction}`; }
function escapeHtml(value) { return String(value ?? "").replace(/[&<>"']/g, character => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#039;" }[character])); }
function metric(value) { return value == null ? "NA" : `${Number(value).toFixed(1)} mph`; }
function observedChange(incident) { if (incident.speedDrop == null) return "NA"; if (incident.speedDrop > 0) return `${incident.speedDrop.toFixed(1)} mph (${incident.speedDropPct == null ? "NA" : incident.speedDropPct.toFixed(1) + "%"})`; if (incident.speedDrop < 0) return `${Math.abs(incident.speedDrop).toFixed(1)} mph faster`; return "No change"; }
function normalDifference(incident) { if (incident.deficitVsNormal == null) return "NA"; if (incident.deficitVsNormal > 0) return `${incident.deficitVsNormal.toFixed(1)} mph slower`; if (incident.deficitVsNormal < 0) return `${Math.abs(incident.deficitVsNormal).toFixed(1)} mph faster`; return "At normal speed"; }

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
  const notes = incident.chpNotes && incident.chpNotes.length
    ? `<div class="incident-notes"><b>CHP notes</b><ul>${incident.chpNotes.map(note => `<li>${escapeHtml(note)}</li>`).join("")}</ul></div>`
    : "";
  return `<div class="popup"><b>${escapeHtml(incident.category)}</b><br>${escapeHtml(incident.description)}<br>${escapeHtml(incident.location || incident.area)}${notes}<div class="popup-grid"><span>Reported</span><b>${incident.timestamp}</b><span>Ended</span><b>${incident.endTime}</b><span>Duration</span><b>${incident.duration == null ? "NA" : incident.duration + " min"}${incident.durationDefault ? " (default)" : ""}</b><span>Matched road</span><b>${roadMatch}</b><span>30 min before</span><b>${metric(incident.speedBefore)}</b><span>During incident</span><b>${metric(incident.speedDuring)}</b><span>30 min after</span><b>${metric(incident.speedAfter)}</b><span>Observed slowdown</span><b>${observedChange(incident)}</b><span>Normal at event time</span><b>${metric(incident.normalDuring)}</b><span>Compared with normal</span><b>${normalDifference(incident)}</b><span>Samples, before/during/after</span><b>${incident.samplesBefore}/${incident.samplesDuring}/${incident.samplesAfter}</b><span>Rain, previous hour</span><b>${weather && weather.rain != null ? weather.rain.toFixed(1) + " mm" : "NA"}</b></div><div class="association-note">${association}</div></div>`;
}
function drawIncidents() { incidentLayer.clearLayers(); incidentHighlightLayer.clearLayers(); markerByIncidentId.clear(); const enabled = document.getElementById("toggleIncidents").checked; const category = document.getElementById("incidentFilter").value; const activeIds = data.incidentsByTime[activeTime] || []; const visible = activeIds.map(id => data.incidents[id]).filter(event => category === "All" || event.category === category); if (enabled) { for (const incident of visible) { const link = linkById.get(incident.linkId); if (link) L.polyline(drawCoordsFor(link), { color: "#5b1621", weight: styleForZoom().flowWeight + 4, opacity: .72, dashArray: "3 7", interactive: false }).addTo(incidentHighlightLayer); if (incident.lat == null || incident.lng == null) continue; const size = styleForZoom().incidentSize; const icon = L.divIcon({ className: "", html: '<div class="incident-diamond"><span>!</span></div>', iconSize: [size, size], iconAnchor: [size / 2, size / 2] }); const marker = L.marker([incident.lat, incident.lng], { icon }).bindPopup(() => incidentPopup(incident)).addTo(incidentLayer); markerByIncidentId.set(incident.id, marker); } } renderEventList(visible); return visible; }
function renderEventList(events) { const list = document.getElementById("eventList"); if (!events.length) { list.innerHTML = '<div class="event-row">No active incidents in the selected layer.</div>'; return; } list.innerHTML = events.slice(0, 20).map(event => `<div class="event-row" data-incident="${event.id}"><b>${event.freeway} ${event.direction} · ${event.category}</b>${event.location || event.area}</div>`).join(""); for (const row of list.querySelectorAll("[data-incident]")) row.addEventListener("click", () => { const marker = markerByIncidentId.get(Number(row.dataset.incident)); if (marker) { map.setView(marker.getLatLng(), Math.max(map.getZoom(), 12)); marker.openPopup(); } }); }

function updateRanking() { const rows = []; for (const link of data.links) { const speed = speedAt(link, activeTime); const normal = normalAt(link, activeTime); if (speed == null || speed <= 0 || normal == null) continue; const deficit = Math.max(0, normal - speed); if (deficit < 3 || speed >= 60) continue; const weather = weatherAt(link, activeTime); const incident = (data.incidentsByTime[activeTime] || []).some(id => data.incidents[id].linkId === link.id); rows.push({ link, speed, normal, deficit, rain: weather ? weather.rain : null, incident }); } rows.sort((a, b) => b.deficit - a.deficit || a.speed - b.speed || a.link.id - b.link.id); const top = rows.slice(0, 8); const list = document.getElementById("rankList"); if (!top.length) { list.innerHTML = '<div class="rank-row"><div class="rank-road">No material speed deficit</div></div>'; return; } list.innerHTML = top.map((row, i) => `<div class="rank-row" data-link="${row.link.id}"><div class="rank-num">${i + 1}</div><div class="rank-road"><b>${routeLabel(row.link)} · ${row.link.from_station}–${row.link.to_station}</b><span>${row.incident ? "incident · " : ""}${row.rain == null ? "weather unavailable" : row.rain > 0 ? row.rain.toFixed(1) + " mm rain" : "dry report"}</span></div><div class="rank-value"><b>−${Math.round(row.deficit)} mph</b>${row.speed} mph now</div></div>`).join(""); for (const item of list.querySelectorAll("[data-link]")) item.addEventListener("click", () => { const line = lineById.get(Number(item.dataset.link)); if (line) { map.fitBounds(line.getBounds(), { maxZoom: 13, padding: [60, 60] }); line.openPopup(); } }); }

function updateWeatherSummary() { const observations = data.weather.map(station => ({ station, temperature: weatherValue(station.temperature, activeTime, 2, 30), wind: weatherValue(station.wind, activeTime, 10), visibility: weatherValue(station.visibility, activeTime, 10), rain: weatherValue(station.rainHour, activeTime, 10) })); const validTemps = observations.filter(item => item.temperature != null).map(item => item.temperature); const validWinds = observations.filter(item => item.wind != null).map(item => item.wind); const validVisibility = observations.filter(item => item.visibility != null).map(item => item.visibility); const validRain = observations.filter(item => item.rain != null).map(item => item.rain); const rainCount = observations.filter(item => item.rain != null && item.rain > 0).length; const median = values => { if (!values.length) return null; const sorted = [...values].sort((a,b) => a-b); return sorted[Math.floor(sorted.length / 2)]; }; const temperature = median(validTemps); const wind = median(validWinds); const visibility = validVisibility.length ? Math.min(...validVisibility) : null; const rain = validRain.length ? Math.max(...validRain) : null; document.getElementById("rainStationCount").textContent = rainCount; document.getElementById("weatherSummary").innerHTML = `<b>${rainCount} of ${data.weather.length}</b> stations reporting rain<br>Temperature: <b>${temperature == null ? "NA" : temperature.toFixed(1) + " °C"}</b><br>Median wind: <b>${wind == null ? "NA" : wind.toFixed(1) + " m/s"}</b><br>Lowest visibility: <b>${visibility == null ? "NA" : visibility.toFixed(1) + " km"}</b><br>Highest 1h rain: <b>${rain == null ? "NA" : rain.toFixed(1) + " mm"}</b>`; document.getElementById("legendRain").textContent = rain == null ? "NA" : rain.toFixed(1) + " mm"; }

function updateTime(restyle = true) { activeTime = Number(document.getElementById("timeSlider").value); let total = 0; let count = 0; if (restyle) speedLayer.eachLayer(line => { const value = speedAt(line._link, activeTime); line.setStyle({ color: speedColor(value) }); if (value != null) { total += value; count += 1; } }); else for (const link of data.links) { const value = speedAt(link, activeTime); if (value != null) { total += value; count += 1; } } const avg = count ? Math.round(total / count) : null; const label = data.timeLabels[activeTime]; const active = drawIncidents(); document.getElementById("timeText").textContent = label; document.getElementById("segmentCount").textContent = count.toLocaleString(); document.getElementById("avgSpeed").textContent = avg == null ? "NA" : `${avg} mph`; document.getElementById("activeIncidentCount").textContent = active.length; document.getElementById("legendTime").textContent = label; document.getElementById("legendAvg").textContent = avg == null ? "NA" : `${avg} mph`; document.getElementById("legendIncidents").textContent = active.length; document.getElementById("gapNote").style.display = label.startsWith("2026-04-03") ? "block" : "none"; updateWeatherSummary(); drawWeatherStations(); updateRanking(); }

document.getElementById("timeSlider").addEventListener("input", () => updateTime(true));
document.getElementById("toggleStations").addEventListener("change", event => event.target.checked ? stationLayer.addTo(map) : stationLayer.remove());
document.getElementById("toggleWeatherStations").addEventListener("change", event => event.target.checked ? weatherStationLayer.addTo(map) : weatherStationLayer.remove());
document.getElementById("toggleCasing").addEventListener("change", event => event.target.checked ? casingLayer.addTo(map) : casingLayer.remove());
document.getElementById("toggleIncidents").addEventListener("change", () => updateTime(false));
document.getElementById("incidentFilter").addEventListener("change", () => updateTime(false));
document.getElementById("resetBtn").addEventListener("click", () => { document.getElementById("timeSlider").value = data.initialIndex; updateTime(true); });
document.getElementById("playBtn").addEventListener("click", () => { playing = !playing; document.getElementById("playBtn").textContent = playing ? "Pause" : "Play"; if (playing) timer = setInterval(() => { const slider = document.getElementById("timeSlider"); slider.value = Math.min(Number(slider.value) + 1, Number(slider.max)); if (slider.value === slider.max) { playing = false; document.getElementById("playBtn").textContent = "Play"; clearInterval(timer); } updateTime(true); }, 350); else clearInterval(timer); });
map.on("zoomend", drawMap);
drawStations();
drawMap();
</script>
</body>
</html>
'''


def build_html(payload: dict) -> str:
    payload_json = json.dumps(payload, separators=(",", ":"), ensure_ascii=False)
    return (
        HTML_TEMPLATE.replace("__PAYLOAD_JSON__", payload_json)
        .replace("__MAX_INDEX__", str(max(0, len(payload["timeLabels"]) - 1)))
        .replace("__INITIAL_INDEX__", str(payload["initialIndex"]))
    )


def main() -> int:
    PROCESSED.mkdir(parents=True, exist_ok=True)
    MAP_DIR.mkdir(parents=True, exist_ok=True)
    required = [TRAFFIC_CLEAN, INCIDENTS_CLEAN, WEATHER_CLEAN]
    missing = [path for path in required if not path.exists()]
    if missing:
        raise FileNotFoundError(
            "Run prepare_clean_data.py first. Missing: " + ", ".join(str(path) for path in missing)
        )

    print("Loading clean PeMS traffic data")
    payload = load_clean_traffic()

    print("Loading clean CHP incident data")
    incidents = pd.read_csv(INCIDENTS_CLEAN, dtype={"incident_id": "string", "district": "string"})
    incidents["timestamp"] = pd.to_datetime(incidents["timestamp"], errors="coerce")
    incidents["end_time"] = pd.to_datetime(incidents["end_time"], errors="coerce")
    incident_payload, incident_buckets = prepare_incident_payload(incidents)

    print("Loading clean NOAA weather data")
    weather = pd.read_csv(WEATHER_CLEAN, dtype={"station": "string"})
    weather_payload, _ = prepare_weather_payload(weather)

    matched_count = int(incidents["link_id"].notna().sum())
    payload["source"] = "PeMS Station 5-Minute, CHP Incidents, NOAA GHCNh"
    payload["incidents"] = incident_payload
    payload["incidentsByTime"] = incident_buckets
    payload["weather"] = weather_payload
    payload["summary"].update(
        {
            "incidents": len(incidents),
            "matchedIncidents": matched_count,
            "weatherStations": len(weather_payload),
            "incidentGapDates": ["2026-04-03"],
        }
    )

    OUTPUT_JSON.write_text(json.dumps(payload, separators=(",", ":"), ensure_ascii=False), encoding="utf-8")
    OUTPUT_HTML.write_text(build_html(payload), encoding="utf-8")

    print(f"Speed links: {len(payload['links']):,}")
    print(f"CHP incidents in D7/D8/D12: {len(incidents):,}")
    print(f"Matched incidents loaded: {matched_count:,}")
    print(f"NOAA weather stations: {len(weather_payload)}")
    print(f"Wrote {OUTPUT_HTML}")
    print(f"Wrote {OUTPUT_JSON}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
