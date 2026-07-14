#!/usr/bin/env python3
"""Build a 5-minute PeMS speed map for the Los Angeles area."""

from __future__ import annotations

import base64
import csv
import json
import math
import re
from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
RAW = ROOT / "data" / "raw"
APRIL = ROOT / "data" / "April"
PROCESSED = ROOT / "processed"
MAP = ROOT / "map"

START = pd.Timestamp("2026-04-01 00:00:00")
END = pd.Timestamp("2026-04-30 23:55:00")
INITIAL_TIME = pd.Timestamp("2026-04-01 16:00:00")
LANE_TYPE = "ML"
DISTRICTS = ("07", "08", "12")

OUTPUT_HTML = MAP / "la_area_freeway_speeds_2026_04_5min.html"
OUTPUT_JSON = PROCESSED / "la_area_freeway_speeds_2026_04_5min.json"
OUTPUT_SUMMARY = PROCESSED / "la_area_freeway_speeds_2026_04_5min_summary.csv"
OUTPUT_STATIONS = PROCESSED / "la_area_stations_2026_04_5min.csv"
OUTPUT_LINKS = PROCESSED / "la_area_speed_links_2026_04_5min.csv"

META_PATTERN = re.compile(r"^d(?P<district>\d{2})_text_meta_(?P<date>\d{4}_\d{2}_\d{2})\.txt$")
FIVE_MIN_PATTERN = re.compile(
    r"^d(?P<district>\d{2})_text_station_5min_(?P<year>\d{4})_(?P<month>\d{2})_(?P<day>\d{2})\.txt$"
)

METADATA_COLUMNS = [
    "ID",
    "Fwy",
    "Dir",
    "District",
    "County",
    "City",
    "State_PM",
    "Abs_PM",
    "Latitude",
    "Longitude",
    "Length",
    "Type",
    "Lanes",
    "Name",
    "User_ID_1",
    "User_ID_2",
    "User_ID_3",
    "User_ID_4",
]

FIVE_MIN_COLUMNS = [
    "timestamp",
    "station",
    "district",
    "freeway",
    "direction",
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

USE_FIVE_MIN_COLUMNS = [
    "timestamp",
    "station",
    "district",
    "freeway",
    "direction",
    "lane_type",
    "average_speed",
]


def ensure_dirs() -> None:
    PROCESSED.mkdir(parents=True, exist_ok=True)
    MAP.mkdir(parents=True, exist_ok=True)


def normalize_station(value: object) -> str:
    text = str(value).strip()
    return text[:-2] if text.endswith(".0") else text


def normalize_metadata_row(row: list[str]) -> list[str]:
    if len(row) < 18:
        return row + [""] * (18 - len(row))
    if len(row) == 18:
        return row
    fixed = row[:13]
    fixed.append(" ".join(part for part in row[13:-4] if part))
    fixed.extend(row[-4:])
    return fixed


def read_metadata() -> pd.DataFrame:
    rows: list[dict[str, str]] = []
    for path in sorted(RAW.glob("d*_text_meta_*.txt")):
        match = META_PATTERN.match(path.name)
        if not match:
            continue
        metadata_date = match.group("date").replace("_", "-")
        with path.open(newline="", encoding="utf-8", errors="replace") as f:
            reader = csv.reader(f, delimiter="\t")
            next(reader, None)
            for raw_row in reader:
                row = normalize_metadata_row(raw_row)
                record = dict(zip(METADATA_COLUMNS, row))
                record["metadata_date"] = metadata_date
                record["source_file"] = path.name
                rows.append(record)

    if not rows:
        raise FileNotFoundError("No metadata files found.")

    meta = pd.DataFrame(rows)
    meta = meta.rename(
        columns={
            "ID": "station",
            "Fwy": "freeway",
            "Dir": "direction",
            "District": "district",
            "Abs_PM": "abs_pm",
            "Latitude": "latitude",
            "Longitude": "longitude",
            "Type": "type",
            "Lanes": "lanes",
            "Name": "name",
        }
    )
    meta["station"] = meta["station"].map(normalize_station)
    meta["district"] = meta["district"].astype(str).str.zfill(2)
    meta["freeway"] = meta["freeway"].astype(str).str.strip()
    meta["direction"] = meta["direction"].astype(str).str.strip()
    meta["type"] = meta["type"].astype(str).str.strip()
    meta["county"] = meta["County"].astype(str).str.strip()
    meta["abs_pm"] = pd.to_numeric(meta["abs_pm"], errors="coerce")
    meta["latitude"] = pd.to_numeric(meta["latitude"], errors="coerce")
    meta["longitude"] = pd.to_numeric(meta["longitude"], errors="coerce")
    meta["lanes"] = pd.to_numeric(meta["lanes"], errors="coerce")
    meta = meta.dropna(subset=["latitude", "longitude", "abs_pm"])
    meta = meta[meta["type"] == LANE_TYPE].copy()
    meta = meta.sort_values(["station", "metadata_date"]).drop_duplicates("station", keep="last")
    return meta


def distance_m(lat1: float, lon1: float, lat2: float, lon2: float) -> float:
    radius = 6_371_000.0
    phi1 = math.radians(lat1)
    phi2 = math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dlambda = math.radians(lon2 - lon1)
    a = math.sin(dphi / 2) ** 2 + math.cos(phi1) * math.cos(phi2) * math.sin(dlambda / 2) ** 2
    return radius * 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))


def speed_color(value: float | None) -> str:
    if value is None or not np.isfinite(value):
        return "#94a3b8"
    if value >= 60:
        return "#16a43a"
    if value >= 55:
        return "#7ad151"
    if value >= 50:
        return "#f7ea2a"
    if value >= 45:
        return "#ffc928"
    if value >= 40:
        return "#f5a623"
    if value >= 35:
        return "#f36b2b"
    return "#f01818"


def encode_speed_values(values: np.ndarray) -> str:
    encoded = bytearray()
    for value in values:
        if not np.isfinite(value):
            encoded.append(255)
        else:
            encoded.append(max(0, min(100, int(round(float(value))))))
    return base64.b64encode(encoded).decode("ascii")


def build_speed_matrix(meta: pd.DataFrame) -> tuple[list[str], np.ndarray]:
    time_index = pd.date_range(START, END, freq="5min")
    station_ids = sorted(meta["station"].unique(), key=lambda value: int(value))
    station_pos = {station: i for i, station in enumerate(station_ids)}
    speed = np.full((len(time_index), len(station_ids)), np.nan, dtype=np.float32)

    valid_stations = set(station_ids)
    start_ns = START.value
    five_min_ns = pd.Timedelta(minutes=5).value
    max_idx = len(time_index)

    for path in sorted(APRIL.glob("D*/d*_text_station_5min_2026_04_*.txt")):
        if not FIVE_MIN_PATTERN.match(path.name):
            continue
        print(f"Reading {path.name}")
        for chunk in pd.read_csv(
            path,
            header=None,
            names=FIVE_MIN_COLUMNS,
            usecols=USE_FIVE_MIN_COLUMNS,
            dtype={
                "timestamp": "string",
                "station": "string",
                "district": "string",
                "freeway": "string",
                "direction": "string",
                "lane_type": "string",
            },
            chunksize=300_000,
        ):
            chunk["station"] = chunk["station"].map(normalize_station)
            chunk = chunk[
                (chunk["lane_type"].str.strip() == LANE_TYPE)
                & (chunk["station"].isin(valid_stations))
            ].copy()
            if chunk.empty:
                continue

            chunk["average_speed"] = pd.to_numeric(chunk["average_speed"], errors="coerce")
            chunk = chunk[chunk["average_speed"].between(1, 100)].copy()
            if chunk.empty:
                continue

            times = pd.to_datetime(chunk["timestamp"], format="%m/%d/%Y %H:%M:%S", errors="coerce")
            valid_times = times.notna()
            if not valid_times.any():
                continue

            chunk = chunk.loc[valid_times].copy()
            times = times.loc[valid_times]
            time_pos = ((times.astype("int64") - start_ns) // five_min_ns).astype(int)
            in_range = (time_pos >= 0) & (time_pos < max_idx)
            if not in_range.any():
                continue

            chunk = chunk.loc[in_range].copy()
            time_pos = time_pos.loc[in_range]
            station_idx = chunk["station"].map(station_pos)
            keep = station_idx.notna()
            if not keep.any():
                continue

            speed[time_pos.loc[keep].to_numpy(), station_idx.loc[keep].astype(int).to_numpy()] = (
                chunk.loc[keep, "average_speed"].astype(float).to_numpy(dtype=np.float32)
            )

    return station_ids, speed


def build_links(meta: pd.DataFrame, station_ids: list[str], speed_matrix: np.ndarray) -> tuple[list[dict], pd.DataFrame]:
    station_pos = {station: i for i, station in enumerate(station_ids)}
    meta = meta[meta["station"].isin(station_pos)].copy()
    has_speed = np.isfinite(speed_matrix).any(axis=0)
    stations_with_speed = {station for station, idx in station_pos.items() if has_speed[idx]}
    meta = meta[meta["station"].isin(stations_with_speed)].copy()

    links: list[dict] = []
    rows: list[dict] = []
    max_gap_m = 8_500.0
    max_postmile_gap = 8.0

    for (district, freeway, direction), group in meta.groupby(["district", "freeway", "direction"], sort=False):
        ordered = group.sort_values(["abs_pm", "station"]).drop_duplicates("abs_pm", keep="first")
        records = list(ordered.itertuples(index=False))
        for a, b in zip(records, records[1:]):
            gap_m = distance_m(float(a.latitude), float(a.longitude), float(b.latitude), float(b.longitude))
            pm_gap = abs(float(b.abs_pm) - float(a.abs_pm))
            if gap_m > max_gap_m or pm_gap > max_postmile_gap:
                continue

            ai = station_pos[str(a.station)]
            bi = station_pos[str(b.station)]
            stacked = np.vstack([speed_matrix[:, ai], speed_matrix[:, bi]])
            counts = np.isfinite(stacked).sum(axis=0)
            values = np.full(speed_matrix.shape[0], np.nan, dtype=np.float32)
            np.divide(np.nansum(stacked, axis=0), counts, out=values, where=counts > 0)
            if not np.isfinite(values).any():
                continue

            link = {
                "id": len(links),
                "district": str(district),
                "freeway": str(freeway),
                "direction": str(direction),
                "from_station": str(a.station),
                "to_station": str(b.station),
                "from_abs_pm": round(float(a.abs_pm), 3),
                "to_abs_pm": round(float(b.abs_pm), 3),
                "coords": [
                    [round(float(a.latitude), 7), round(float(a.longitude), 7)],
                    [round(float(b.latitude), 7), round(float(b.longitude), 7)],
                ],
                "values": encode_speed_values(values),
            }
            links.append(link)
            rows.append({k: v for k, v in link.items() if k not in {"coords", "values"}})

    return links, pd.DataFrame(rows)


def build_station_markers(meta: pd.DataFrame, station_ids: list[str], speed_matrix: np.ndarray) -> list[dict]:
    station_pos = {station: i for i, station in enumerate(station_ids)}
    markers = []
    for row in meta.itertuples(index=False):
        idx = station_pos.get(str(row.station))
        if idx is None or not np.isfinite(speed_matrix[:, idx]).any():
            continue
        markers.append(
            {
                "station": str(row.station),
                "lat": round(float(row.latitude), 7),
                "lng": round(float(row.longitude), 7),
                "district": str(row.district),
                "freeway": str(row.freeway),
                "direction": str(row.direction),
                "name": str(row.name),
            }
        )
    return markers


def build_summary(time_labels: list[str], links: list[dict]) -> list[dict]:
    decoded_values = [base64.b64decode(link["values"]) for link in links]
    rows = []
    for idx, label in enumerate(time_labels):
        values = [values[idx] for values in decoded_values if values[idx] != 255]
        rows.append(
            {
                "time": label,
                "segments_with_speed": len(values),
                "average_speed": round(float(np.mean(values)), 1) if values else "",
                "median_speed": round(float(np.median(values)), 1) if values else "",
            }
        )
    return rows


def build_html(payload: dict) -> str:
    payload_json = json.dumps(payload, separators=(",", ":"))
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>LA Area Freeway Speeds 2026 April 5-Minute</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <style>
    html, body {{ height: 100%; margin: 0; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; color: #172033; }}
    .page {{ display: grid; grid-template-columns: minmax(0, 1fr) 360px; height: 100vh; width: 100vw; overflow: hidden; background: #eef2f6; }}
    #map {{ height: 100vh; min-width: 0; }}
    aside {{ background: #fff; border-left: 1px solid #d8dee8; padding: 20px; overflow: auto; box-sizing: border-box; }}
    h1 {{ margin: 0 0 6px; font-size: 25px; line-height: 1.15; color: #18243c; }}
    .sub {{ color: #667085; font-size: 13px; margin-bottom: 18px; }}
    .field {{ margin: 13px 0; }}
    .field span {{ display: block; font-size: 12px; font-weight: 700; color: #475569; margin-bottom: 6px; }}
    input[type="range"] {{ width: 100%; box-sizing: border-box; }}
    button {{ height: 34px; border: 1px solid #bdc7d6; background: #f8fafc; border-radius: 4px; padding: 0 12px; cursor: pointer; }}
    button:hover {{ background: #eef3f8; }}
    .time-row {{ display: flex; gap: 8px; align-items: center; }}
    #timeText {{ font-weight: 750; font-size: 17px; color: #172033; margin-top: 4px; }}
    .stats {{ display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin: 14px 0 18px; }}
    .stat {{ border: 1px solid #d8dee8; border-radius: 4px; padding: 10px; background: #f8fafc; }}
    .stat b {{ display: block; font-size: 21px; color: #172033; }}
    .stat span {{ font-size: 12px; color: #64748b; }}
    label {{ display: flex; gap: 8px; align-items: center; color: #334155; font-size: 14px; margin: 8px 0; }}
    .legend {{
      position: absolute;
      left: 16px;
      bottom: 16px;
      z-index: 700;
      background: #fff;
      border: 1px solid rgba(15,23,42,.35);
      border-radius: 4px;
      padding: 9px;
      box-shadow: 0 2px 8px rgba(15,23,42,.18);
      min-width: 356px;
    }}
    .legend-row {{ display: flex; gap: 6px; align-items: center; }}
    .swatch {{
      width: 43px;
      height: 25px;
      border-radius: 5px;
      border: 1px solid rgba(15,23,42,.25);
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 11px;
      font-weight: 800;
      color: #172033;
    }}
    .swatch.red {{ color: #fff; }}
    .legend-note {{ font-size: 12px; font-weight: 700; color: #334155; margin-top: 5px; }}
    .legend-info {{ display: grid; grid-template-columns: 1fr 1fr; gap: 4px 10px; margin-top: 8px; font-size: 12px; color: #475569; }}
    .legend-info b {{ color: #172033; }}
    .popup {{ font-size: 13px; line-height: 1.35; }}
    .leaflet-container {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; }}
    @media (max-width: 900px) {{
      .page {{ grid-template-columns: 1fr; grid-template-rows: 68vh 32vh; }}
      #map {{ height: 68vh; }}
      aside {{ height: 32vh; border-left: 0; border-top: 1px solid #d8dee8; }}
      .legend {{ min-width: 290px; }}
      .swatch {{ width: 34px; font-size: 10px; }}
    }}
  </style>
</head>
<body>
<div class="page">
  <div id="map"></div>
  <aside>
    <h1>LA Area Freeway Speeds</h1>
    <div class="sub">PeMS Station 5-Minute · April 2026 · D7/D8/D12</div>
    <div class="field">
      <span>Time</span>
      <div class="time-row">
        <button id="playBtn">Play</button>
        <button id="resetBtn">Reset</button>
      </div>
      <input id="timeSlider" type="range" min="0" max="{len(payload["timeLabels"]) - 1}" step="1" value="{payload["initialIndex"]}">
      <div id="timeText"></div>
    </div>
    <div class="stats">
      <div class="stat"><b id="segmentCount"></b><span>road links</span></div>
      <div class="stat"><b id="avgSpeed"></b><span>avg speed</span></div>
      <div class="stat"><b id="stationCount">{payload["summary"]["stations"]:,}</b><span>ML stations</span></div>
      <div class="stat"><b id="districtText">D7/D8/D12</b><span>districts</span></div>
    </div>
    <div class="field">
      <span>Layers</span>
      <label><input id="toggleStations" type="checkbox"> station points</label>
      <label><input id="toggleCasing" type="checkbox"> road casing</label>
    </div>
  </aside>
</div>
<div class="legend">
  <div class="legend-row">
    <div class="swatch" style="background:#16a43a">60+</div>
    <div class="swatch" style="background:#7ad151">55-59</div>
    <div class="swatch" style="background:#f7ea2a">50-54</div>
    <div class="swatch" style="background:#ffc928">45-49</div>
    <div class="swatch" style="background:#f5a623">40-44</div>
    <div class="swatch" style="background:#f36b2b">35-39</div>
    <div class="swatch red" style="background:#f01818">≤35</div>
  </div>
  <div class="legend-note">Speed, mph</div>
  <div class="legend-info">
    <div>Time: <b id="legendTime"></b></div>
    <div>Average: <b id="legendAvg"></b></div>
    <div>Road links: <b id="legendSegments"></b></div>
    <div>Data: <b>Station 5-Minute</b></div>
  </div>
</div>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
const data = {payload_json};
for (const link of data.links) {{
  const raw = atob(link.values);
  const values = new Uint8Array(raw.length);
  for (let i = 0; i < raw.length; i += 1) values[i] = raw.charCodeAt(i);
  link.values = values;
}}
const colors = {{
  high: "#16a43a",
  c55: "#7ad151",
  c50: "#f7ea2a",
  c45: "#ffc928",
  c40: "#f5a623",
  c35: "#f36b2b",
  low: "#f01818",
  missing: "#94a3b8"
}};

const map = L.map("map", {{ preferCanvas: true, zoomSnap: 0.25, zoomDelta: 0.25 }});
L.tileLayer("https://{{s}}.tile.openstreetmap.org/{{z}}/{{x}}/{{y}}.png", {{
  maxZoom: 19,
  attribution: "&copy; OpenStreetMap contributors"
}}).addTo(map);
map.setView(data.center, data.zoom);

const casingLayer = L.layerGroup();
const speedLayer = L.layerGroup().addTo(map);
const stationLayer = L.layerGroup();
let activeTime = data.initialIndex;
let playing = false;
let timer = null;

function speedColor(value) {{
  if (value == null || value === 255 || Number.isNaN(value)) return colors.missing;
  if (value >= 60) return colors.high;
  if (value >= 55) return colors.c55;
  if (value >= 50) return colors.c50;
  if (value >= 45) return colors.c45;
  if (value >= 40) return colors.c40;
  if (value >= 35) return colors.c35;
  return colors.low;
}}

function speedAt(link, index) {{
  const value = link.values[index];
  return value === 255 ? null : value;
}}

function directionSide(direction) {{
  const dir = String(direction || "").toUpperCase();
  if (dir === "N" || dir === "E") return 1;
  if (dir === "S" || dir === "W") return -1;
  return 0;
}}

function canonicalAxis(direction) {{
  const dir = String(direction || "").toUpperCase();
  if (dir === "N" || dir === "S") return [0, -1];
  if (dir === "E" || dir === "W") return [1, 0];
  return [1, 0];
}}

function styleForZoom() {{
  const z = map.getZoom();
  if (z < 8.8) return {{ offset: 0.4, flowWeight: 2.0, casingWeight: 0, opacity: 0.84 }};
  if (z < 9.6) return {{ offset: 2.4, flowWeight: 2.8, casingWeight: 0, opacity: 0.88 }};
  if (z < 10.6) return {{ offset: 4.8, flowWeight: 3.5, casingWeight: 4.8, opacity: 0.91 }};
  if (z < 11.6) return {{ offset: 6.2, flowWeight: 4.3, casingWeight: 5.8, opacity: 0.94 }};
  return {{ offset: 8.0, flowWeight: 5.2, casingWeight: 6.8, opacity: 0.95 }};
}}

function drawCoordsFor(link) {{
  const style = styleForZoom();
  if (!link.coords || link.coords.length < 2 || style.offset === 0) return link.coords;
  const start = map.latLngToLayerPoint(link.coords[0]);
  const end = map.latLngToLayerPoint(link.coords[link.coords.length - 1]);
  let dx = end.x - start.x;
  let dy = end.y - start.y;
  const len = Math.hypot(dx, dy);
  if (!len) return link.coords;
  let nx = -dy / len;
  let ny = dx / len;
  const axis = canonicalAxis(link.direction);
  if (dx * axis[0] + dy * axis[1] < 0) {{
    nx *= -1;
    ny *= -1;
  }}
  const offset = style.offset * directionSide(link.direction);
  return link.coords.map(coord => {{
    const point = map.latLngToLayerPoint(coord);
    return map.layerPointToLatLng(L.point(point.x + nx * offset, point.y + ny * offset));
  }});
}}

function drawMap() {{
  casingLayer.clearLayers();
  speedLayer.clearLayers();
  const style = styleForZoom();
  for (const link of data.links) {{
    const coords = drawCoordsFor(link);
    if (style.casingWeight > 0) {{
      L.polyline(coords, {{ color: "#1f3329", weight: style.casingWeight, opacity: 0.32, lineCap: "round", lineJoin: "round", interactive: false }}).addTo(casingLayer);
    }}
    const value = speedAt(link, activeTime);
    const line = L.polyline(coords, {{ color: speedColor(value), weight: style.flowWeight, opacity: style.opacity, lineCap: "round", lineJoin: "round" }}).addTo(speedLayer);
    line._link = link;
    line.bindPopup(() => {{
      const popupValue = speedAt(link, activeTime);
      return `<div class="popup"><b>${{link.freeway}} ${{link.direction}}</b><br>${{link.from_station}} → ${{link.to_station}}<br>${{data.timeLabels[activeTime]}}<br>Speed: ${{popupValue ?? "NA"}} mph</div>`;
    }});
  }}
  updateTime(false);
}}

function drawStations() {{
  stationLayer.clearLayers();
  for (const station of data.stations) {{
    L.circleMarker([station.lat, station.lng], {{
      radius: 2.1,
      color: "#fff",
      weight: 1,
      fillColor: "#334155",
      fillOpacity: 0.8
    }}).bindPopup(`<div class="popup"><b>Station ${{station.station}}</b><br>${{station.freeway}} ${{station.direction}}<br>D${{station.district}}<br>${{station.name || ""}}</div>`).addTo(stationLayer);
  }}
}}

function updateTime(restyle = true) {{
  activeTime = Number(document.getElementById("timeSlider").value);
  let total = 0;
  let count = 0;
  if (restyle) {{
    speedLayer.eachLayer(line => {{
      const value = speedAt(line._link, activeTime);
      line.setStyle({{ color: speedColor(value) }});
      if (value != null && Number.isFinite(value)) {{ total += value; count += 1; }}
    }});
  }} else {{
    for (const link of data.links) {{
      const value = speedAt(link, activeTime);
      if (value != null && Number.isFinite(value)) {{ total += value; count += 1; }}
    }}
  }}
  const avg = count ? Math.round(total / count) : null;
  const label = data.timeLabels[activeTime];
  document.getElementById("timeText").textContent = label;
  document.getElementById("segmentCount").textContent = count.toLocaleString();
  document.getElementById("avgSpeed").textContent = avg == null ? "NA" : `${{avg}} mph`;
  document.getElementById("legendTime").textContent = label;
  document.getElementById("legendAvg").textContent = avg == null ? "NA" : `${{avg}} mph`;
  document.getElementById("legendSegments").textContent = count.toLocaleString();
}}

document.getElementById("timeSlider").addEventListener("input", () => updateTime(true));
document.getElementById("toggleStations").addEventListener("change", event => event.target.checked ? stationLayer.addTo(map) : stationLayer.remove());
document.getElementById("toggleCasing").addEventListener("change", event => event.target.checked ? casingLayer.addTo(map) : casingLayer.remove());
document.getElementById("resetBtn").addEventListener("click", () => {{ document.getElementById("timeSlider").value = data.initialIndex; updateTime(true); }});
document.getElementById("playBtn").addEventListener("click", () => {{
  playing = !playing;
  document.getElementById("playBtn").textContent = playing ? "Pause" : "Play";
  if (playing) {{
    timer = setInterval(() => {{
      const slider = document.getElementById("timeSlider");
      slider.value = Math.min(Number(slider.value) + 1, Number(slider.max));
      if (slider.value === slider.max) {{
        playing = false;
        document.getElementById("playBtn").textContent = "Play";
        clearInterval(timer);
      }}
      updateTime(true);
    }}, 300);
  }} else {{
    clearInterval(timer);
  }}
}});
map.on("zoomend", drawMap);

drawStations();
drawMap();
</script>
</body>
</html>
"""


def main() -> int:
    ensure_dirs()
    time_index = pd.date_range(START, END, freq="5min")
    time_labels = [ts.strftime("%Y-%m-%d %H:%M") for ts in time_index]
    initial_index = int((INITIAL_TIME - START) / pd.Timedelta(minutes=5))

    meta = read_metadata()
    station_ids, speed_matrix = build_speed_matrix(meta)
    links, links_df = build_links(meta, station_ids, speed_matrix)
    stations = build_station_markers(meta, station_ids, speed_matrix)

    payload = {
        "source": "PeMS Station 5-Minute",
        "timeLabels": time_labels,
        "initialIndex": initial_index,
        "center": [34.03, -117.82],
        "zoom": 9.4,
        "summary": {
            "stations": len(stations),
            "links": len(links),
            "timeSteps": len(time_labels),
        },
        "links": links,
        "stations": stations,
    }

    OUTPUT_JSON.write_text(json.dumps(payload, separators=(",", ":")), encoding="utf-8")
    OUTPUT_HTML.write_text(build_html(payload), encoding="utf-8")
    links_df.to_csv(OUTPUT_LINKS, index=False)
    pd.DataFrame(stations).to_csv(OUTPUT_STATIONS, index=False)
    pd.DataFrame(build_summary(time_labels, links)).to_csv(OUTPUT_SUMMARY, index=False)

    print(f"ML metadata stations: {len(meta):,}")
    print(f"Stations with speed data: {len(stations):,}")
    print(f"5-minute time steps: {len(time_labels):,}")
    print(f"Speed links: {len(links):,}")
    print(f"Wrote {OUTPUT_HTML}")
    print(f"Wrote {OUTPUT_JSON}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
