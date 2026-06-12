#!/usr/bin/env python3
"""Build an OSM/Leaflet HTML map for D11 Station Hour 2020 outputs."""

from __future__ import annotations

import json
import math
import re
from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
PROCESSED = ROOT / "data" / "processed"
OSM_CACHE = ROOT / "data" / "osm" / "d11_highways_overpass.json"
OUTPUT = ROOT / "outputs" / "maps" / "station_hour_2020_osm_map.html"


def as_number(value):
    if pd.isna(value) or value == "":
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def build_station_records() -> list[dict]:
    metadata = pd.read_csv(PROCESSED / "station_metadata_2020_latest_by_station.csv")
    monthly = pd.read_csv(PROCESSED / "station_hour_2020_station_month_basic_summary.csv")

    metadata["station"] = metadata["station"].astype(str)
    monthly["station"] = monthly["station"].astype(str)
    monthly["month"] = monthly["month"].astype(str).str.zfill(2)

    monthly["flow"] = pd.to_numeric(monthly["total_flow_weighted_by_samples"], errors="coerce")
    monthly["samples_sum"] = pd.to_numeric(monthly["samples_sum"], errors="coerce").fillna(0)

    records: list[dict] = []
    for _, meta in metadata.dropna(subset=["latitude", "longitude"]).iterrows():
        station = meta["station"]
        station_months = monthly[monthly["station"] == station]
        flows: dict[str, float | None] = {}
        weighted_num = 0.0
        weighted_den = 0.0
        records_count = 0

        for _, row in station_months.iterrows():
            month = row["month"]
            flow = as_number(row["flow"])
            flows[month] = None if flow is None else round(flow, 3)
            if flow is not None and row["samples_sum"] > 0:
                weighted_num += flow * float(row["samples_sum"])
                weighted_den += float(row["samples_sum"])
            records_count += int(row["records"]) if not pd.isna(row["records"]) else 0

        annual_flow = weighted_num / weighted_den if weighted_den else None
        records.append(
            {
                "station": station,
                "lat": float(meta["latitude"]),
                "lon": float(meta["longitude"]),
                "freeway": str(meta.get("freeway", "")).strip(),
                "direction": str(meta.get("direction", "")).strip(),
                "type": str(meta.get("type", "")).strip() or "Unknown",
                "lanes": None if pd.isna(meta.get("lanes")) else int(meta["lanes"]),
                "name": "" if pd.isna(meta.get("name")) else str(meta["name"]).strip(),
                "metadata_date": str(meta.get("metadata_date", "")),
                "annual_flow": None if annual_flow is None else round(annual_flow, 3),
                "records": records_count,
                "flows": flows,
            }
        )

    return records


def local_projector(records: list[dict]):
    lat0 = sum(item["lat"] for item in records) / len(records)
    cos0 = math.cos(math.radians(lat0))

    def project(lat: float, lon: float) -> tuple[float, float]:
        return lon * 111_320 * cos0, lat * 110_540

    return project


def route_numbers(tags: dict) -> set[str]:
    text = " ".join(str(tags.get(key, "")) for key in ("ref", "name", "alt_name", "official_name"))
    return set(re.findall(r"(?<!\d)(\d{1,3})(?!\d)", text))


def point_segment_distance(
    point: tuple[float, float],
    start: tuple[float, float],
    end: tuple[float, float],
) -> tuple[float, float]:
    px, py = point
    ax, ay = start
    bx, by = end
    dx = bx - ax
    dy = by - ay
    denom = dx * dx + dy * dy
    if denom == 0:
        return math.hypot(px - ax, py - ay), 0.0
    t = max(0.0, min(1.0, ((px - ax) * dx + (py - ay) * dy) / denom))
    qx = ax + t * dx
    qy = ay + t * dy
    return math.hypot(px - qx, py - qy), t


def interpolate_coord(
    coord_a: tuple[float, float],
    coord_b: tuple[float, float],
    t: float,
) -> list[float]:
    lat = coord_a[0] + (coord_b[0] - coord_a[0]) * t
    lon = coord_a[1] + (coord_b[1] - coord_a[1]) * t
    return [lat, lon]


def coord_at_distance(way: dict, target: float) -> list[float]:
    cum = way["cum"]
    coords = way["coords"]
    if target <= 0:
        return [coords[0][0], coords[0][1]]
    if target >= cum[-1]:
        return [coords[-1][0], coords[-1][1]]
    for i in range(len(cum) - 1):
        if cum[i] <= target <= cum[i + 1]:
            span = cum[i + 1] - cum[i]
            t = 0 if span == 0 else (target - cum[i]) / span
            return interpolate_coord(coords[i], coords[i + 1], t)
    return [coords[-1][0], coords[-1][1]]


def subline_around_match(way: dict, distance_along: float, half_window_m: float = 260.0) -> list[list[float]]:
    cum = way["cum"]
    coords = way["coords"]
    start_d = max(0.0, distance_along - half_window_m)
    end_d = min(cum[-1], distance_along + half_window_m)

    line = [coord_at_distance(way, start_d)]
    for i, dist in enumerate(cum):
        if start_d < dist < end_d:
            line.append([coords[i][0], coords[i][1]])
    line.append(coord_at_distance(way, end_d))
    return line


def load_osm_ways(records: list[dict]) -> tuple[list[dict], dict[str, list[tuple]], list[tuple]]:
    if not OSM_CACHE.exists():
        return [], {}, []

    project = local_projector(records)
    raw = json.loads(OSM_CACHE.read_text(encoding="utf-8"))
    nodes = {
        item["id"]: (float(item["lat"]), float(item["lon"]))
        for item in raw.get("elements", [])
        if item.get("type") == "node"
    }

    ways: list[dict] = []
    segments_by_number: dict[str, list[tuple]] = {}
    all_segments: list[tuple] = []

    for item in raw.get("elements", []):
        if item.get("type") != "way":
            continue
        coords = [nodes[node_id] for node_id in item.get("nodes", []) if node_id in nodes]
        if len(coords) < 2:
            continue

        points = [project(lat, lon) for lat, lon in coords]
        cumulative = [0.0]
        for i in range(len(points) - 1):
            cumulative.append(
                cumulative[-1]
                + math.hypot(points[i + 1][0] - points[i][0], points[i + 1][1] - points[i][1])
            )

        tags = item.get("tags", {})
        numbers = route_numbers(tags)
        way_index = len(ways)
        ways.append(
            {
                "coords": coords,
                "points": points,
                "cum": cumulative,
                "tags": tags,
                "numbers": numbers,
            }
        )

        for seg_index in range(len(points) - 1):
            segment = (way_index, seg_index, points[seg_index], points[seg_index + 1])
            all_segments.append(segment)
            for number in numbers:
                segments_by_number.setdefault(number, []).append(segment)

    return ways, segments_by_number, all_segments


def build_road_segments(records: list[dict]) -> list[dict]:
    ways, segments_by_number, all_segments = load_osm_ways(records)
    if not ways:
        return []

    project = local_projector(records)
    road_segments: list[dict] = []
    max_match_distance_m = 250.0

    for record in records:
        freeway = str(record.get("freeway", "")).strip()
        candidates = segments_by_number.get(freeway, all_segments)
        if not candidates:
            candidates = all_segments

        station_xy = project(record["lat"], record["lon"])
        best = None
        for way_index, seg_index, start, end in candidates:
            dist_m, t = point_segment_distance(station_xy, start, end)
            if best is None or dist_m < best[0]:
                best = (dist_m, t, way_index, seg_index)

        if best is None:
            continue

        dist_m, t, way_index, seg_index = best
        if dist_m > max_match_distance_m:
            continue

        way = ways[way_index]
        seg_start_d = way["cum"][seg_index]
        seg_end_d = way["cum"][seg_index + 1]
        distance_along = seg_start_d + (seg_end_d - seg_start_d) * t
        tags = way["tags"]

        road_segments.append(
            {
                "station": record["station"],
                "freeway": record["freeway"],
                "direction": record["direction"],
                "type": record["type"],
                "annual_flow": record["annual_flow"],
                "flows": record["flows"],
                "coords": subline_around_match(way, distance_along),
                "match_distance_m": round(dist_m, 2),
                "osm_highway": tags.get("highway", ""),
                "osm_ref": tags.get("ref", ""),
                "osm_name": tags.get("name", ""),
            }
        )

    return road_segments


def build_html(records: list[dict], road_segments: list[dict]) -> str:
    center_lat = sum(item["lat"] for item in records) / len(records)
    center_lon = sum(item["lon"] for item in records) / len(records)
    data_json = json.dumps(records, ensure_ascii=False, separators=(",", ":"))
    roads_json = json.dumps(road_segments, ensure_ascii=False, separators=(",", ":"))

    html = r"""<!doctype html>
<html lang="zh-CN">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>PeMS D11 Station Hour 2020 OSM Map</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <style>
    /* Critical Leaflet layout fallback. If the CDN stylesheet is blocked,
       these rules keep tiles, SVG overlays, controls, and popups positioned. */
    .leaflet-container {
      overflow: hidden;
      background: #ddd;
      outline-offset: 1px;
      font: 12px/1.5 "Helvetica Neue", Arial, Helvetica, sans-serif;
    }
    .leaflet-pane,
    .leaflet-tile,
    .leaflet-marker-icon,
    .leaflet-marker-shadow,
    .leaflet-tile-container,
    .leaflet-pane > svg,
    .leaflet-pane > canvas,
    .leaflet-zoom-box,
    .leaflet-image-layer,
    .leaflet-layer {
      position: absolute;
      left: 0;
      top: 0;
    }
    .leaflet-tile,
    .leaflet-marker-icon,
    .leaflet-marker-shadow {
      user-select: none;
      -webkit-user-drag: none;
    }
    .leaflet-pane { z-index: 400; }
    .leaflet-tile-pane { z-index: 200; }
    .leaflet-overlay-pane { z-index: 400; }
    .leaflet-shadow-pane { z-index: 500; }
    .leaflet-marker-pane { z-index: 600; }
    .leaflet-tooltip-pane { z-index: 650; }
    .leaflet-popup-pane { z-index: 700; }
    .leaflet-map-pane canvas { z-index: 100; }
    .leaflet-map-pane svg { z-index: 200; }
    .leaflet-control {
      position: relative;
      z-index: 800;
      pointer-events: auto;
    }
    .leaflet-top,
    .leaflet-bottom {
      position: absolute;
      z-index: 1000;
      pointer-events: none;
    }
    .leaflet-top { top: 0; }
    .leaflet-right { right: 0; }
    .leaflet-bottom { bottom: 0; }
    .leaflet-left { left: 0; }
    .leaflet-control { float: left; clear: both; }
    .leaflet-right .leaflet-control { float: right; }
    .leaflet-top .leaflet-control { margin-top: 10px; }
    .leaflet-bottom .leaflet-control { margin-bottom: 10px; }
    .leaflet-left .leaflet-control { margin-left: 10px; }
    .leaflet-right .leaflet-control { margin-right: 10px; }
    .leaflet-control-zoom a {
      display: block;
      width: 26px;
      height: 26px;
      line-height: 26px;
      text-align: center;
      text-decoration: none;
      color: #111827;
      background: #fff;
      border-bottom: 1px solid #ccc;
    }
    .leaflet-control-zoom a:last-child { border-bottom: 0; }
    .leaflet-control-attribution {
      background: rgba(255,255,255,.82);
      margin: 0;
      padding: 0 5px;
      color: #333;
    }
    .leaflet-popup {
      position: absolute;
      text-align: center;
      margin-bottom: 20px;
    }
    .leaflet-popup-content-wrapper {
      padding: 1px;
      text-align: left;
      border-radius: 6px;
      background: white;
      box-shadow: 0 3px 14px rgba(0,0,0,.25);
    }
    .leaflet-popup-content {
      margin: 10px 12px;
      line-height: 1.35;
    }
    .leaflet-popup-tip-container {
      width: 40px;
      height: 20px;
      position: absolute;
      left: 50%;
      margin-left: -20px;
      overflow: hidden;
      pointer-events: none;
    }
    .leaflet-popup-tip {
      width: 17px;
      height: 17px;
      padding: 1px;
      margin: -10px auto 0;
      transform: rotate(45deg);
      background: white;
      box-shadow: 0 3px 14px rgba(0,0,0,.25);
    }
    .leaflet-container a.leaflet-popup-close-button {
      position: absolute;
      top: 0;
      right: 0;
      border: none;
      text-align: center;
      width: 24px;
      height: 24px;
      font: 16px/24px Tahoma, Verdana, sans-serif;
      color: #757575;
      text-decoration: none;
      background: transparent;
    }
    html, body {
      height: 100%;
      margin: 0;
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      color: #172033;
      background: #f4f6f8;
    }
    .app {
      display: grid;
      grid-template-columns: 340px minmax(0, 1fr);
      height: 100%;
    }
    aside {
      overflow: auto;
      border-right: 1px solid #d9dee7;
      background: #ffffff;
      padding: 18px;
    }
    #map {
      min-height: 100%;
    }
    h1 {
      font-size: 20px;
      line-height: 1.2;
      margin: 0 0 8px;
    }
    .sub {
      font-size: 13px;
      line-height: 1.45;
      color: #5e697c;
      margin-bottom: 18px;
    }
    .field {
      margin-bottom: 14px;
    }
    label {
      display: block;
      font-size: 12px;
      font-weight: 700;
      color: #354052;
      margin-bottom: 6px;
    }
    select, input {
      width: 100%;
      box-sizing: border-box;
      border: 1px solid #c9d1dd;
      border-radius: 6px;
      padding: 8px 9px;
      font-size: 14px;
      background: #fff;
      color: #172033;
    }
    .checks {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 7px 10px;
      margin-top: 2px;
    }
    .check {
      display: flex;
      align-items: center;
      gap: 6px;
      font-size: 13px;
      color: #2d3748;
    }
    .check input {
      width: auto;
    }
    .stats {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 8px;
      margin: 16px 0;
    }
    .stat {
      border: 1px solid #d9dee7;
      border-radius: 6px;
      padding: 10px;
      background: #fafbfc;
    }
    .stat .value {
      font-size: 20px;
      font-weight: 800;
    }
    .stat .name {
      font-size: 11px;
      color: #647083;
      margin-top: 2px;
    }
    .legend {
      font-size: 12px;
      line-height: 1.5;
      color: #4b5568;
      border-top: 1px solid #e4e8ef;
      padding-top: 14px;
      margin-top: 14px;
    }
    .swatch {
      display: inline-block;
      width: 10px;
      height: 10px;
      border-radius: 50%;
      margin-right: 6px;
      border: 1px solid rgba(0,0,0,.18);
    }
    .popup-title {
      font-weight: 800;
      font-size: 14px;
      margin-bottom: 4px;
    }
    .popup-row {
      font-size: 12px;
      margin: 2px 0;
    }
    @media (max-width: 760px) {
      .app {
        grid-template-columns: 1fr;
        grid-template-rows: auto 1fr;
      }
      aside {
        max-height: 42vh;
        border-right: none;
        border-bottom: 1px solid #d9dee7;
      }
      #map {
        min-height: 58vh;
      }
    }
  </style>
</head>
<body>
  <div class="app">
    <aside>
      <h1>PeMS D11 Station Hour 2020</h1>
      <div class="sub">
        OSM 底图上的 District 11 station。圆点颜色可按月度流量或 station 类型显示；数据来自当前已整理的 Station Hour 与 Metadata。
      </div>

      <div class="field">
        <label for="month">月份</label>
        <select id="month">
          <option value="annual">全年加权均值</option>
          <option value="01">2020-01</option>
          <option value="02">2020-02</option>
          <option value="03">2020-03</option>
          <option value="04">2020-04</option>
          <option value="05">2020-05</option>
          <option value="06">2020-06</option>
          <option value="07">2020-07</option>
          <option value="08">2020-08</option>
          <option value="09">2020-09</option>
          <option value="10">2020-10</option>
          <option value="11">2020-11</option>
          <option value="12">2020-12</option>
        </select>
      </div>

      <div class="field">
        <label for="colorMode">颜色方式</label>
        <select id="colorMode">
          <option value="flow">按流量</option>
          <option value="type">按 station 类型</option>
        </select>
      </div>

      <div class="field">
        <label class="check">
          <input type="checkbox" id="showRoads" checked>
          <span>显示匹配的 OSM 路段颜色</span>
        </label>
      </div>

      <div class="field">
        <label>Station 类型</label>
        <div id="typeFilters" class="checks"></div>
      </div>

      <div class="field">
        <label for="freeway">Freeway 过滤</label>
        <input id="freeway" placeholder="例如 5、15、805，留空为全部">
      </div>

      <div class="field">
        <label for="stationSearch">Station ID 搜索</label>
        <input id="stationSearch" placeholder="输入 station ID">
      </div>

      <div class="stats">
        <div class="stat">
          <div id="visibleCount" class="value">0</div>
          <div class="name">当前显示 station</div>
        </div>
        <div class="stat">
          <div id="meanFlow" class="value">-</div>
          <div class="name">显示点平均流量</div>
        </div>
        <div class="stat">
          <div id="roadCount" class="value">0</div>
          <div class="name">当前显示路段</div>
        </div>
      </div>

      <div id="legend" class="legend"></div>
    </aside>
    <main id="map"></main>
  </div>

  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <script>
    const stationData = __DATA_JSON__;
    const roadSegments = __ROAD_SEGMENTS_JSON__;
    const map = L.map("map", { preferCanvas: true }).setView([__CENTER_LAT__, __CENTER_LON__], 9);

    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      maxZoom: 19,
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);

    const typeColors = {
      ML: "#2F6DB3",
      HV: "#8C5FBF",
      FR: "#C76D3B",
      OR: "#4F9360",
      FF: "#D1A12C",
      Unknown: "#6B7280"
    };
    const flowColors = ["#E9F2FB", "#BFD9EF", "#85B7DD", "#4B8CC7", "#1F5B99", "#0B2F5B"];
    const markers = [];
    const roadLines = [];

    const types = Array.from(new Set(stationData.map(d => d.type))).sort();
    const typeFilters = document.getElementById("typeFilters");
    types.forEach(type => {
      const id = "type-" + type;
      const label = document.createElement("label");
      label.className = "check";
      label.innerHTML = '<input type="checkbox" id="' + id + '" value="' + type + '" checked> <span>' + type + '</span>';
      typeFilters.appendChild(label);
    });

    function selectedTypes() {
      return new Set(Array.from(typeFilters.querySelectorAll("input:checked")).map(input => input.value));
    }

    function flowValue(d) {
      const month = document.getElementById("month").value;
      if (month === "annual") return d.annual_flow;
      return d.flows[month] ?? null;
    }

    function roadFlowValue(d) {
      const month = document.getElementById("month").value;
      if (month === "annual") return d.annual_flow;
      return d.flows[month] ?? null;
    }

    function flowBreaks(values) {
      const sorted = values.filter(v => Number.isFinite(v)).sort((a, b) => a - b);
      if (!sorted.length) return [];
      return [0.2, 0.4, 0.6, 0.8, 0.95].map(q => sorted[Math.min(sorted.length - 1, Math.floor(q * sorted.length))]);
    }

    function colorForFlow(value, breaks) {
      if (!Number.isFinite(value)) return "#9CA3AF";
      let idx = 0;
      while (idx < breaks.length && value > breaks[idx]) idx++;
      return flowColors[idx];
    }

    function radiusForFlow(value) {
      if (!Number.isFinite(value)) return 4;
      return Math.max(4, Math.min(15, 3 + Math.sqrt(value) / 4));
    }

    function popupHtml(d, value) {
      const flowText = Number.isFinite(value) ? Math.round(value).toLocaleString() : "No data";
      return [
        '<div class="popup-title">Station ' + d.station + '</div>',
        '<div class="popup-row"><b>Name:</b> ' + (d.name || "-") + '</div>',
        '<div class="popup-row"><b>Freeway:</b> ' + d.freeway + ' ' + d.direction + '</div>',
        '<div class="popup-row"><b>Type:</b> ' + d.type + '</div>',
        '<div class="popup-row"><b>Lanes:</b> ' + (d.lanes ?? "-") + '</div>',
        '<div class="popup-row"><b>Flow:</b> ' + flowText + '</div>',
        '<div class="popup-row"><b>Metadata:</b> ' + d.metadata_date + '</div>'
      ].join("");
    }

    function roadPopupHtml(d, value) {
      const flowText = Number.isFinite(value) ? Math.round(value).toLocaleString() : "No data";
      return [
        '<div class="popup-title">Matched OSM road</div>',
        '<div class="popup-row"><b>Station:</b> ' + d.station + '</div>',
        '<div class="popup-row"><b>PeMS freeway:</b> ' + d.freeway + ' ' + d.direction + '</div>',
        '<div class="popup-row"><b>OSM:</b> ' + (d.osm_ref || "-") + ' ' + (d.osm_name || "") + '</div>',
        '<div class="popup-row"><b>Highway:</b> ' + (d.osm_highway || "-") + '</div>',
        '<div class="popup-row"><b>Match distance:</b> ' + d.match_distance_m + ' m</div>',
        '<div class="popup-row"><b>Flow:</b> ' + flowText + '</div>'
      ].join("");
    }

    function renderLegend(mode, breaks) {
      const legend = document.getElementById("legend");
      if (mode === "type") {
        legend.innerHTML = "<b>Station 类型</b><br>" + types.map(type =>
          '<span class="swatch" style="background:' + (typeColors[type] || typeColors.Unknown) + '"></span>' + type
        ).join("<br>");
      } else {
        const labels = [];
        for (let i = 0; i < flowColors.length; i++) {
          let label;
          if (i === 0) label = "≤ " + Math.round(breaks[0] || 0);
          else if (i === flowColors.length - 1) label = "> " + Math.round(breaks[breaks.length - 1] || 0);
          else label = Math.round(breaks[i - 1] || 0) + " - " + Math.round(breaks[i] || 0);
          labels.push('<span class="swatch" style="background:' + flowColors[i] + '"></span>' + label);
        }
        legend.innerHTML = "<b>平均小时流量</b><br>" + labels.join("<br>") + '<br><br>圆点越大表示流量越高。';
      }
    }

    function update() {
      markers.forEach(marker => map.removeLayer(marker));
      markers.length = 0;
      roadLines.forEach(line => map.removeLayer(line));
      roadLines.length = 0;

      const mode = document.getElementById("colorMode").value;
      const showRoads = document.getElementById("showRoads").checked;
      const freeway = document.getElementById("freeway").value.trim();
      const search = document.getElementById("stationSearch").value.trim();
      const allowedTypes = selectedTypes();

      const filtered = stationData.filter(d => {
        if (!allowedTypes.has(d.type)) return false;
        if (freeway && d.freeway !== freeway) return false;
        if (search && !d.station.includes(search)) return false;
        return true;
      });

      const values = filtered.map(flowValue).filter(v => Number.isFinite(v));
      const breaks = flowBreaks(values);
      const visibleStations = new Set(filtered.map(d => d.station));
      let flowTotal = 0;
      let flowCount = 0;

      if (showRoads) {
        roadSegments
          .filter(d => visibleStations.has(d.station))
          .forEach(d => {
            const value = roadFlowValue(d);
            const color = mode === "type" ? (typeColors[d.type] || typeColors.Unknown) : colorForFlow(value, breaks);
            const line = L.polyline(d.coords, {
              color,
              weight: mode === "flow" ? Math.max(4, Math.min(10, radiusForFlow(value) * 0.72)) : 5,
              opacity: 0.88,
              lineCap: "round",
              lineJoin: "round",
              smoothFactor: 0.8
            }).bindPopup(roadPopupHtml(d, value));
            line.addTo(map);
            roadLines.push(line);
          });
      }

      filtered.forEach(d => {
        const value = flowValue(d);
        if (Number.isFinite(value)) {
          flowTotal += value;
          flowCount++;
        }
        const color = mode === "type" ? (typeColors[d.type] || typeColors.Unknown) : colorForFlow(value, breaks);
        const marker = L.circleMarker([d.lat, d.lon], {
          radius: mode === "flow" ? radiusForFlow(value) : 6,
          color: "#1f2937",
          weight: 0.6,
          fillColor: color,
          fillOpacity: 0.78
        }).bindPopup(popupHtml(d, value));
        marker.addTo(map);
        markers.push(marker);
      });

      document.getElementById("visibleCount").textContent = filtered.length.toLocaleString();
      document.getElementById("meanFlow").textContent = flowCount ? Math.round(flowTotal / flowCount).toLocaleString() : "-";
      document.getElementById("roadCount").textContent = roadLines.length.toLocaleString();
      renderLegend(mode, breaks);
    }

    ["month", "colorMode", "showRoads", "freeway", "stationSearch"].forEach(id => {
      document.getElementById(id).addEventListener("input", update);
      document.getElementById(id).addEventListener("change", update);
    });
    typeFilters.addEventListener("change", update);
    update();
  </script>
</body>
</html>
"""
    return (
        html.replace("__DATA_JSON__", data_json)
        .replace("__ROAD_SEGMENTS_JSON__", roads_json)
        .replace("__CENTER_LAT__", f"{center_lat:.6f}")
        .replace("__CENTER_LON__", f"{center_lon:.6f}")
    )


def main() -> int:
    records = build_station_records()
    road_segments = build_road_segments(records)
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    OUTPUT.write_text(build_html(records, road_segments), encoding="utf-8")
    print(f"Wrote {OUTPUT}")
    print(f"Stations on map: {len(records)}")
    print(f"Matched road segments: {len(road_segments)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
