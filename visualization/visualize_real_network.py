from __future__ import annotations

import argparse
import json
import math
import os
from pathlib import Path

import cv2
import numpy as np
import pandas as pd


BASE_DIR = Path(__file__).resolve().parent
PROCESSED_DIR = BASE_DIR / "processed_real_network"
DEFAULT_OUT_DIR = PROCESSED_DIR / "visualizations"
MPL_CACHE_DIR = PROCESSED_DIR / ".matplotlib"
MPL_CACHE_DIR.mkdir(parents=True, exist_ok=True)
os.environ.setdefault("MPLCONFIGDIR", str(MPL_CACHE_DIR))

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection


def out_of_china(lon: float, lat: float) -> bool:
    return lon < 72.004 or lon > 137.8347 or lat < 0.8293 or lat > 55.8271


def _transform_lat(x: float, y: float) -> float:
    ret = -100.0 + 2.0 * x + 3.0 * y + 0.2 * y * y + 0.1 * x * y
    ret += 0.2 * math.sqrt(abs(x))
    ret += (20.0 * math.sin(6.0 * x * math.pi) + 20.0 * math.sin(2.0 * x * math.pi)) * 2.0 / 3.0
    ret += (20.0 * math.sin(y * math.pi) + 40.0 * math.sin(y / 3.0 * math.pi)) * 2.0 / 3.0
    ret += (160.0 * math.sin(y / 12.0 * math.pi) + 320 * math.sin(y * math.pi / 30.0)) * 2.0 / 3.0
    return ret


def _transform_lon(x: float, y: float) -> float:
    ret = 300.0 + x + 2.0 * y + 0.1 * x * x + 0.1 * x * y
    ret += 0.1 * math.sqrt(abs(x))
    ret += (20.0 * math.sin(6.0 * x * math.pi) + 20.0 * math.sin(2.0 * x * math.pi)) * 2.0 / 3.0
    ret += (20.0 * math.sin(x * math.pi) + 40.0 * math.sin(x / 3.0 * math.pi)) * 2.0 / 3.0
    ret += (150.0 * math.sin(x / 12.0 * math.pi) + 300.0 * math.sin(x / 30.0 * math.pi)) * 2.0 / 3.0
    return ret


def wgs84_to_gcj02(lon: float, lat: float) -> tuple[float, float]:
    if out_of_china(lon, lat):
        return lon, lat

    a = 6378245.0
    ee = 0.00669342162296594323
    dlat = _transform_lat(lon - 105.0, lat - 35.0)
    dlon = _transform_lon(lon - 105.0, lat - 35.0)
    radlat = lat / 180.0 * math.pi
    magic = math.sin(radlat)
    magic = 1 - ee * magic * magic
    sqrt_magic = math.sqrt(magic)
    dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrt_magic) * math.pi)
    dlon = (dlon * 180.0) / (a / sqrt_magic * math.cos(radlat) * math.pi)
    return lon + dlon, lat + dlat


def uses_gcj02(tile: str) -> bool:
    return tile.startswith("amap")


def load_network(tile: str) -> tuple[pd.DataFrame, pd.DataFrame, dict[int, tuple[float, float]], list[list[tuple[float, float]]]]:
    nodes = pd.read_csv(PROCESSED_DIR / "nodes_clean.csv")
    links = pd.read_csv(PROCESSED_DIR / "links_clean.csv")

    if uses_gcj02(tile):
        converted = nodes[["x_84", "y_84"]].apply(
            lambda r: wgs84_to_gcj02(float(r["x_84"]), float(r["y_84"])),
            axis=1,
            result_type="expand",
        )
        nodes["map_lon"] = converted[0]
        nodes["map_lat"] = converted[1]
        coord_cols = ["map_lon", "map_lat"]
    else:
        nodes["map_lon"] = nodes["x_84"]
        nodes["map_lat"] = nodes["y_84"]
        coord_cols = ["map_lon", "map_lat"]

    coords = {
        int(row.node_id): (float(getattr(row, coord_cols[0])), float(getattr(row, coord_cols[1])))
        for row in nodes[["node_id", *coord_cols]].dropna().itertuples(index=False)
    }

    segments: list[list[tuple[float, float]]] = []
    valid_edge_indices: list[int] = []
    for row in links[["edge_index", "from_node_id", "to_node_id"]].itertuples(index=False):
        u = int(row.from_node_id)
        v = int(row.to_node_id)
        if u in coords and v in coords:
            segments.append([coords[u], coords[v]])
            valid_edge_indices.append(int(row.edge_index))

    valid_edges = pd.DataFrame({"edge_index": valid_edge_indices})
    links = links.merge(valid_edges, on="edge_index", how="inner")
    return nodes, links, coords, segments


def map_bounds(nodes: pd.DataFrame) -> tuple[float, float, float, float]:
    return (
        float(nodes["map_lat"].min()),
        float(nodes["map_lon"].min()),
        float(nodes["map_lat"].max()),
        float(nodes["map_lon"].max()),
    )


def tile_config(tile: str) -> dict[str, object]:
    if tile == "amap":
        return {
            "name": "AMap vector tiles (GCJ-02 overlay)",
            "layers": [
                {
                    "url": "https://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}",
                    "subdomains": ["1", "2", "3", "4"],
                    "opacity": 1.0,
                }
            ],
            "attribution": "AMap",
            "satellite": False,
        }

    if tile in {"amap-satellite", "amap-satellite-labels"}:
        layers: list[dict[str, object]] = [
            {
                "url": "https://webst0{s}.is.autonavi.com/appmaptile?style=6&x={x}&y={y}&z={z}",
                "subdomains": ["1", "2", "3", "4"],
                "opacity": 1.0,
            }
        ]
        if tile == "amap-satellite-labels":
            layers.append(
                {
                    "url": "https://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}",
                    "subdomains": ["1", "2", "3", "4"],
                    "opacity": 0.46,
                }
            )
        return {
            "name": "AMap satellite tiles (GCJ-02 overlay)",
            "layers": layers,
            "attribution": "AMap",
            "satellite": True,
        }

    return {
        "name": "OpenStreetMap tiles (WGS84 overlay)",
        "layers": [
            {
                "url": "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                "subdomains": ["a", "b", "c"],
                "opacity": 1.0,
            }
        ],
        "attribution": "OpenStreetMap contributors",
        "satellite": False,
    }


def network_features(links: pd.DataFrame, coords: dict[int, tuple[float, float]]) -> list[dict[str, object]]:
    features: list[dict[str, object]] = []
    cols = [
        "edge_index",
        "edge_id",
        "from_node_id",
        "to_node_id",
        "length",
        "number_of_lanes",
        "speed_limit",
        "link_type_name",
    ]
    for row in links[cols].itertuples(index=False):
        u = int(row.from_node_id)
        v = int(row.to_node_id)
        if u not in coords or v not in coords:
            continue
        features.append(
            {
                "type": "Feature",
                "geometry": {
                    "type": "LineString",
                    "coordinates": [
                        [round(coords[u][0], 7), round(coords[u][1], 7)],
                        [round(coords[v][0], 7), round(coords[v][1], 7)],
                    ],
                },
                "properties": {
                    "edge_index": int(row.edge_index),
                    "edge_id": str(row.edge_id),
                    "from_node_id": u,
                    "to_node_id": v,
                    "length_km": None if pd.isna(row.length) else round(float(row.length), 4),
                    "lanes": None if pd.isna(row.number_of_lanes) else int(row.number_of_lanes),
                    "speed_limit": None if pd.isna(row.speed_limit) else float(row.speed_limit),
                    "link_type_name": None if pd.isna(row.link_type_name) else str(row.link_type_name),
                },
            }
        )
    return features


def flow_features(
    links: pd.DataFrame,
    coords: dict[int, tuple[float, float]],
    time_bin: int,
) -> list[dict[str, object]]:
    flow = pd.read_csv(PROCESSED_DIR / "edge_flow_xijt_nonzero.csv")
    flow = flow[flow["time_bin"] == time_bin]
    if flow.empty:
        return []

    flow = flow.merge(
        links[["edge_index", "from_node_id", "to_node_id", "link_type_name", "length"]],
        on=["edge_index", "from_node_id", "to_node_id"],
        how="inner",
    )

    features: list[dict[str, object]] = []
    for row in flow.itertuples(index=False):
        u = int(row.from_node_id)
        v = int(row.to_node_id)
        if u not in coords or v not in coords:
            continue
        x_ijt = float(row.x_ijt)
        features.append(
            {
                "type": "Feature",
                "geometry": {
                    "type": "LineString",
                    "coordinates": [
                        [round(coords[u][0], 7), round(coords[u][1], 7)],
                        [round(coords[v][0], 7), round(coords[v][1], 7)],
                    ],
                },
                "properties": {
                    "edge_index": int(row.edge_index),
                    "edge_id": str(row.edge_id),
                    "from_node_id": u,
                    "to_node_id": v,
                    "x_ijt": x_ijt,
                    "link_type_name": None if pd.isna(row.link_type_name) else str(row.link_type_name),
                    "length_km": None if pd.isna(row.length) else round(float(row.length), 4),
                    "start_min": int(row.start_min),
                    "end_min": int(row.end_min),
                },
            }
        )
    return features


def write_leaflet_html(
    out_path: Path,
    title: str,
    tile: str,
    bounds: tuple[float, float, float, float],
    features: list[dict[str, object]],
    flow_layer: bool,
) -> None:
    config = tile_config(tile)
    geojson = json.dumps({"type": "FeatureCollection", "features": features}, ensure_ascii=False)
    bounds_json = json.dumps([[bounds[0], bounds[1]], [bounds[2], bounds[3]]])
    tile_layers_json = json.dumps(config["layers"])
    satellite_json = json.dumps(config["satellite"])
    panel_class = "panel satellite-panel" if config["satellite"] else "panel"

    style_fn = """
function styleFeature(feature) {
  const p = feature.properties || {};
  if (p.x_ijt !== undefined) {
    const v = Math.max(0, Number(p.x_ijt || 0));
    const t = Math.min(1, Math.log1p(v) / Math.log1p(maxFlow || 1));
    const hue = satellite ? 185 - 185 * t : 215 - 215 * t;
    return {
      color: `hsl(${hue}, 92%, 48%)`,
      weight: (satellite ? 1.35 : 1.0) + (satellite ? 6.25 : 5.5) * t,
      opacity: satellite ? 0.96 : 0.9,
      lineCap: "round"
    };
  }
  if (satellite) {
    return {
      color: "#00e5ff",
      weight: majorRoads.has(p.link_type_name) ? 1.8 : 0.85,
      opacity: majorRoads.has(p.link_type_name) ? 0.9 : 0.58,
      lineCap: "round"
    };
  }
  return {
    color: "#20639b",
    weight: majorRoads.has(p.link_type_name) ? 1.35 : 0.65,
    opacity: majorRoads.has(p.link_type_name) ? 0.7 : 0.36
  };
}
"""

    html = f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{title}</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <style>
    html, body, #map {{ height: 100%; margin: 0; }}
    body {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; }}
    .panel {{
      position: absolute;
      z-index: 1000;
      top: 12px;
      left: 12px;
      max-width: min(420px, calc(100vw - 32px));
      padding: 10px 12px;
      background: rgba(255, 255, 255, 0.92);
      border: 1px solid rgba(0, 0, 0, 0.15);
      border-radius: 6px;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.12);
      font-size: 13px;
      line-height: 1.4;
    }}
    .panel h1 {{ font-size: 15px; margin: 0 0 4px; }}
    .panel .muted {{ color: #555; }}
    .satellite-panel {{
      background: rgba(7, 12, 14, 0.78);
      border-color: rgba(255, 255, 255, 0.28);
      color: #f5f7f8;
      box-shadow: 0 2px 14px rgba(0, 0, 0, 0.35);
    }}
    .satellite-panel .muted {{ color: rgba(245, 247, 248, 0.76); }}
  </style>
</head>
<body>
  <div id="map"></div>
  <div class="{panel_class}">
    <h1>{title}</h1>
    <div class="muted">{config["name"]}</div>
    <div class="muted">{len(features):,} line features</div>
  </div>
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <script>
    const data = {geojson};
    const bounds = {bounds_json};
    const satellite = {satellite_json};
    const tileLayers = {tile_layers_json};
    const majorRoads = new Set(["快速路", "主干路", "次干路", "一级公路", "二级公路", "收费高速路"]);
    const maxFlow = data.features.reduce((m, f) => Math.max(m, Number((f.properties || {{}}).x_ijt || 0)), 0);
    const map = L.map("map", {{ preferCanvas: true }});
    tileLayers.forEach((tileLayer, index) => {{
      L.tileLayer(tileLayer.url, {{
        maxZoom: 19,
        subdomains: tileLayer.subdomains,
        attribution: index === 0 ? "{config["attribution"]}" : "",
        opacity: tileLayer.opacity
      }}).addTo(map);
    }});
    map.fitBounds(bounds, {{ padding: [18, 18] }});
{style_fn}
    const layer = L.geoJSON(data, {{
      style: styleFeature,
      onEachFeature: (feature, layer) => {{
        const p = feature.properties || {{}};
        const flowLine = p.x_ijt !== undefined ? `<b>x_ijt:</b> ${{p.x_ijt}}<br>` : "";
        layer.bindPopup(
          `${{flowLine}}<b>edge:</b> ${{p.edge_id}}<br>` +
          `<b>from-to:</b> ${{p.from_node_id}} -> ${{p.to_node_id}}<br>` +
          `<b>type:</b> ${{p.link_type_name || ""}}<br>` +
          `<b>length km:</b> ${{p.length_km ?? ""}}`
        );
      }}
    }}).addTo(map);
  </script>
</body>
</html>
"""
    out_path.write_text(html, encoding="utf-8")


def compact_flow_value(value: float) -> int | float:
    value = float(value)
    if value.is_integer():
        return int(value)
    return round(value, 3)


def write_dynamic_leaflet_html(
    out_path: Path,
    title: str,
    tile: str,
    bounds: tuple[float, float, float, float],
    links: pd.DataFrame,
    coords: dict[int, tuple[float, float]],
    initial_time_bin: int,
) -> None:
    config = tile_config(tile)
    features = network_features(links, coords)
    valid_edges = set(int(e) for e in links["edge_index"])

    flow = pd.read_csv(
        PROCESSED_DIR / "edge_flow_xijt_nonzero.csv",
        usecols=["time_bin", "start_min", "end_min", "edge_index", "x_ijt"],
    )
    flow = flow[flow["edge_index"].isin(valid_edges)]

    time_bins = [int(v) for v in sorted(flow["time_bin"].unique())]
    if initial_time_bin not in time_bins and time_bins:
        initial_time_bin = time_bins[0]
    initial_index = time_bins.index(initial_time_bin) if initial_time_bin in time_bins else 0
    max_flow = compact_flow_value(flow["x_ijt"].max()) if not flow.empty else 1

    flow_by_bin: dict[str, list[list[int | float]]] = {}
    time_meta: dict[str, dict[str, int | float]] = {}
    for time_bin, group in flow.groupby("time_bin", sort=True):
        key = str(int(time_bin))
        values = group[["edge_index", "x_ijt"]].to_numpy()
        flow_by_bin[key] = [[int(edge_index), compact_flow_value(x_ijt)] for edge_index, x_ijt in values]
        time_meta[key] = {
            "start_min": int(group["start_min"].iloc[0]),
            "end_min": int(group["end_min"].iloc[0]),
            "active_edges": int(len(group)),
            "total_xijt": compact_flow_value(group["x_ijt"].sum()),
        }

    network_json = json.dumps({"type": "FeatureCollection", "features": features}, ensure_ascii=False, separators=(",", ":"))
    flow_json = json.dumps(flow_by_bin, ensure_ascii=False, separators=(",", ":"))
    meta_json = json.dumps(time_meta, ensure_ascii=False, separators=(",", ":"))
    bounds_json = json.dumps([[bounds[0], bounds[1]], [bounds[2], bounds[3]]])
    tile_layers_json = json.dumps(config["layers"], separators=(",", ":"))
    satellite_json = json.dumps(config["satellite"])
    panel_class = "panel satellite-panel" if config["satellite"] else "panel"

    html = f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{title}</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <style>
    html, body, #map {{ height: 100%; margin: 0; }}
    body {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; }}
    .panel {{
      position: absolute;
      z-index: 1000;
      top: 12px;
      left: 12px;
      width: min(520px, calc(100vw - 32px));
      padding: 10px 12px;
      background: rgba(255, 255, 255, 0.92);
      border: 1px solid rgba(0, 0, 0, 0.15);
      border-radius: 6px;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.12);
      font-size: 13px;
      line-height: 1.4;
    }}
    .panel h1 {{ font-size: 15px; margin: 0 0 5px; }}
    .panel .muted {{ color: #555; }}
    .satellite-panel {{
      background: rgba(7, 12, 14, 0.80);
      border-color: rgba(255, 255, 255, 0.28);
      color: #f5f7f8;
      box-shadow: 0 2px 14px rgba(0, 0, 0, 0.35);
    }}
    .satellite-panel .muted {{ color: rgba(245, 247, 248, 0.76); }}
    .controls {{
      display: grid;
      grid-template-columns: auto 1fr;
      gap: 8px 10px;
      align-items: center;
      margin-top: 8px;
    }}
    button {{
      min-width: 64px;
      height: 30px;
      border: 1px solid rgba(255, 255, 255, 0.36);
      border-radius: 5px;
      color: inherit;
      background: rgba(255, 255, 255, 0.13);
      cursor: pointer;
      font: inherit;
    }}
    input[type="range"] {{ width: 100%; }}
    .readout {{
      grid-column: 1 / -1;
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      font-variant-numeric: tabular-nums;
    }}
  </style>
</head>
<body>
  <div id="map"></div>
  <div class="{panel_class}">
    <h1>{title}</h1>
    <div class="muted">{config["name"]}</div>
    <div class="controls">
      <button id="playButton" type="button">Play</button>
      <input id="timeSlider" type="range" min="0" max="{max(0, len(time_bins) - 1)}" value="{initial_index}" step="1">
      <div class="readout">
        <span id="binLabel"></span>
        <span id="timeLabel"></span>
        <span id="activeLabel"></span>
        <span id="totalLabel"></span>
      </div>
    </div>
  </div>
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <script>
    const network = {network_json};
    const flowByBin = {flow_json};
    const timeMeta = {meta_json};
    const timeBins = {json.dumps(time_bins, separators=(",", ":"))};
    const bounds = {bounds_json};
    const satellite = {satellite_json};
    const maxFlow = {max_flow};
    const tileLayers = {tile_layers_json};
    const majorRoads = new Set(["快速路", "主干路", "次干路", "一级公路", "二级公路", "收费高速路"]);
    const map = L.map("map", {{ preferCanvas: true }});

    tileLayers.forEach((tileLayer, index) => {{
      L.tileLayer(tileLayer.url, {{
        maxZoom: 19,
        subdomains: tileLayer.subdomains,
        attribution: index === 0 ? "{config["attribution"]}" : "",
        opacity: tileLayer.opacity
      }}).addTo(map);
    }});
    map.fitBounds(bounds, {{ padding: [18, 18] }});

    const edgeLayers = new Map();
    let activeValues = new Map();
    let activeIndex = {initial_index};
    let timer = null;

    function minuteText(startMin, endMin) {{
      const pad = (v) => String(v).padStart(2, "0");
      return `${{pad(Math.floor(startMin / 60))}}:${{pad(startMin % 60)}}-${{pad(Math.floor(endMin / 60))}}:${{pad(endMin % 60)}}`;
    }}

    function baseStyle(properties) {{
      if (satellite) {{
        return {{
          color: "#00e5ff",
          weight: majorRoads.has(properties.link_type_name) ? 1.65 : 0.72,
          opacity: majorRoads.has(properties.link_type_name) ? 0.78 : 0.36,
          lineCap: "round"
        }};
      }}
      return {{
        color: "#20639b",
        weight: majorRoads.has(properties.link_type_name) ? 1.25 : 0.55,
        opacity: majorRoads.has(properties.link_type_name) ? 0.58 : 0.28,
        lineCap: "round"
      }};
    }}

    function flowStyle(value, properties) {{
      if (!value) return baseStyle(properties);
      const t = Math.min(1, Math.log1p(value) / Math.log1p(maxFlow || 1));
      const hue = satellite ? 185 - 185 * t : 215 - 215 * t;
      return {{
        color: `hsl(${{hue}}, 92%, 48%)`,
        weight: (satellite ? 1.4 : 1.05) + (satellite ? 6.4 : 5.5) * t,
        opacity: satellite ? 0.98 : 0.9,
        lineCap: "round"
      }};
    }}

    function popupHtml(properties) {{
      const value = activeValues.get(properties.edge_index) || 0;
      const bin = timeBins[activeIndex];
      return (
        `<b>time_bin:</b> ${{bin}}<br>` +
        `<b>x_ijt:</b> ${{value}}<br>` +
        `<b>edge:</b> ${{properties.edge_id}}<br>` +
        `<b>from-to:</b> ${{properties.from_node_id}} -> ${{properties.to_node_id}}<br>` +
        `<b>type:</b> ${{properties.link_type_name || ""}}<br>` +
        `<b>length km:</b> ${{properties.length_km ?? ""}}`
      );
    }}

    L.geoJSON(network, {{
      style: (feature) => baseStyle(feature.properties || {{}}),
      onEachFeature: (feature, layer) => {{
        const properties = feature.properties || {{}};
        edgeLayers.set(properties.edge_index, layer);
        layer.on("click", () => layer.bindPopup(popupHtml(properties)).openPopup());
      }}
    }}).addTo(map);

    const slider = document.getElementById("timeSlider");
    const playButton = document.getElementById("playButton");
    const binLabel = document.getElementById("binLabel");
    const timeLabel = document.getElementById("timeLabel");
    const activeLabel = document.getElementById("activeLabel");
    const totalLabel = document.getElementById("totalLabel");

    function setFrame(index) {{
      activeIndex = Math.max(0, Math.min(timeBins.length - 1, Number(index)));
      const bin = timeBins[activeIndex];
      const entries = flowByBin[String(bin)] || [];
      const nextValues = new Map(entries);
      const touched = new Set([...activeValues.keys(), ...nextValues.keys()]);

      touched.forEach((edgeIndex) => {{
        const layer = edgeLayers.get(edgeIndex);
        if (!layer) return;
        const properties = layer.feature.properties || {{}};
        layer.setStyle(flowStyle(nextValues.get(edgeIndex) || 0, properties));
      }});

      activeValues = nextValues;
      slider.value = String(activeIndex);
      const meta = timeMeta[String(bin)] || {{ start_min: bin * 5, end_min: bin * 5 + 5, active_edges: 0, total_xijt: 0 }};
      binLabel.textContent = `bin ${{bin}}`;
      timeLabel.textContent = minuteText(meta.start_min, meta.end_min);
      activeLabel.textContent = `active ${{Number(meta.active_edges).toLocaleString()}}`;
      totalLabel.textContent = `total ${{Number(meta.total_xijt).toLocaleString()}}`;
    }}

    function stopPlayback() {{
      if (timer) window.clearInterval(timer);
      timer = null;
      playButton.textContent = "Play";
    }}

    function startPlayback() {{
      stopPlayback();
      playButton.textContent = "Pause";
      timer = window.setInterval(() => {{
        const nextIndex = (activeIndex + 1) % timeBins.length;
        setFrame(nextIndex);
      }}, 520);
    }}

    slider.addEventListener("input", () => {{
      stopPlayback();
      setFrame(Number(slider.value));
    }});
    playButton.addEventListener("click", () => {{
      if (timer) stopPlayback();
      else startPlayback();
    }});

    setFrame(activeIndex);
  </script>
</body>
</html>
"""
    out_path.write_text(html, encoding="utf-8")


def write_static_preview(out_dir: Path, nodes: pd.DataFrame, segments: list[list[tuple[float, float]]], tile: str) -> Path:
    out_path = out_dir / f"road_network_{tile}_coordinates_preview.png"
    fig, ax = plt.subplots(figsize=(10, 10))
    lc = LineCollection(segments, linewidths=0.25, alpha=0.48, colors="#1f77b4")
    ax.add_collection(lc)
    ax.scatter(nodes["map_lon"], nodes["map_lat"], s=1, alpha=0.25, color="#1f77b4")
    ax.autoscale()
    ax.set_aspect("equal", adjustable="box")
    ax.set_title(f"Road network in {'GCJ-02' if uses_gcj02(tile) else 'WGS84'} coordinates")
    ax.set_xlabel("longitude")
    ax.set_ylabel("latitude")
    fig.tight_layout()
    fig.savefig(out_path, dpi=220)
    plt.close(fig)
    return out_path


def minute_label(start_min: int, end_min: int) -> str:
    return f"{start_min // 60:02d}:{start_min % 60:02d}-{end_min // 60:02d}:{end_min % 60:02d}"


def render_flow_frames(
    out_dir: Path,
    links: pd.DataFrame,
    segments: list[list[tuple[float, float]]],
    tile: str,
    frame_step: int,
    max_frames: int | None,
    video_fps: int,
) -> tuple[Path, Path | None]:
    frames_dir = out_dir / f"xijt_frames_{tile}_step{frame_step}"
    frames_dir.mkdir(parents=True, exist_ok=True)

    flow = pd.read_csv(PROCESSED_DIR / "edge_flow_xijt_nonzero.csv")
    time_bins = sorted(flow["time_bin"].unique())
    selected_bins = time_bins[::frame_step]
    if max_frames is not None:
        selected_bins = selected_bins[:max_frames]

    link_order = links.reset_index(drop=True)[["edge_index"]].copy()
    link_order["segment_pos"] = np.arange(len(link_order))
    flow = flow.merge(link_order, on="edge_index", how="inner")
    max_flow = float(flow["x_ijt"].max()) if not flow.empty else 1.0

    all_segments = np.asarray(segments, dtype=float)
    x_min = float(all_segments[:, :, 0].min())
    x_max = float(all_segments[:, :, 0].max())
    y_min = float(all_segments[:, :, 1].min())
    y_max = float(all_segments[:, :, 1].max())
    x_pad = (x_max - x_min) * 0.035
    y_pad = (y_max - y_min) * 0.035

    frame_paths: list[Path] = []
    grouped = {int(k): v for k, v in flow.groupby("time_bin")}
    norm = plt.Normalize(vmin=0, vmax=np.log1p(max_flow))
    cmap = plt.get_cmap("turbo")

    for frame_no, time_bin in enumerate(selected_bins):
        frame_flow = grouped.get(int(time_bin))
        fig, ax = plt.subplots(figsize=(10, 10))
        base = LineCollection(segments, linewidths=0.18, alpha=0.20, colors="#8a9aa8")
        ax.add_collection(base)

        start_min = int(time_bin) * 5
        end_min = start_min + 5
        active_count = 0
        total_xijt = 0.0
        if frame_flow is not None and not frame_flow.empty:
            frame_segments = [segments[int(pos)] for pos in frame_flow["segment_pos"]]
            values = frame_flow["x_ijt"].astype(float).to_numpy()
            colors = cmap(norm(np.log1p(values)))
            widths = 0.45 + 4.0 * (np.log1p(values) / np.log1p(max_flow))
            active = LineCollection(frame_segments, linewidths=widths, colors=colors, alpha=0.86)
            ax.add_collection(active)
            active_count = len(frame_flow)
            total_xijt = float(values.sum())
            start_min = int(frame_flow["start_min"].iloc[0])
            end_min = int(frame_flow["end_min"].iloc[0])

        ax.set_xlim(x_min - x_pad, x_max + x_pad)
        ax.set_ylim(y_min - y_pad, y_max + y_pad)
        ax.set_aspect("equal", adjustable="box")
        ax.set_xticks([])
        ax.set_yticks([])
        ax.set_title(
            f"x_ijt road occupancy | bin {int(time_bin)} | {minute_label(start_min, end_min)} | "
            f"active edges {active_count:,} | total {total_xijt:,.0f}"
        )
        fig.tight_layout()

        out_path = frames_dir / f"frame_{frame_no:04d}_bin_{int(time_bin):03d}.png"
        fig.savefig(out_path, dpi=160)
        plt.close(fig)
        frame_paths.append(out_path)

    video_path: Path | None = None
    if frame_paths:
        first = cv2.imread(str(frame_paths[0]))
        height, width = first.shape[:2]
        video_path = out_dir / f"xijt_animation_{tile}_step{frame_step}_{len(frame_paths)}frames.mp4"
        writer = cv2.VideoWriter(str(video_path), cv2.VideoWriter_fourcc(*"mp4v"), video_fps, (width, height))
        for frame_path in frame_paths:
            img = cv2.imread(str(frame_path))
            if img.shape[1] != width or img.shape[0] != height:
                img = cv2.resize(img, (width, height))
            writer.write(img)
        writer.release()

    return frames_dir, video_path


def main() -> None:
    parser = argparse.ArgumentParser(description="Visualize the processed Beijing road network and x_ijt flows.")
    parser.add_argument("--tile", choices=["osm", "amap", "amap-satellite", "amap-satellite-labels"], default="amap")
    parser.add_argument("--time-bin", type=int, default=107)
    parser.add_argument("--frame-step", type=int, default=12, help="Use every Nth 5-minute bin for video frames.")
    parser.add_argument("--max-frames", type=int, default=24, help="Limit rendered frames. Use 0 for all selected bins.")
    parser.add_argument("--video-fps", type=int, default=6)
    parser.add_argument("--skip-animation", action="store_true", help="Only write the HTML map outputs.")
    parser.add_argument("--dynamic-html", action="store_true", help="Write a time-slider HTML map for all x_ijt bins.")
    parser.add_argument("--out-dir", type=Path, default=DEFAULT_OUT_DIR)
    args = parser.parse_args()

    args.out_dir.mkdir(parents=True, exist_ok=True)
    max_frames = None if args.max_frames == 0 else args.max_frames

    nodes, links, coords, segments = load_network(args.tile)
    bounds = map_bounds(nodes)

    static_png = write_static_preview(args.out_dir, nodes, segments, args.tile)

    network_html = args.out_dir / f"road_network_leaflet_{args.tile}.html"
    write_leaflet_html(
        network_html,
        "Beijing Road Network",
        args.tile,
        bounds,
        network_features(links, coords),
        flow_layer=False,
    )

    flow_html = args.out_dir / f"xijt_leaflet_time_bin_{args.time_bin}_{args.tile}.html"
    write_leaflet_html(
        flow_html,
        f"x_ijt Flow, Time Bin {args.time_bin}",
        args.tile,
        bounds,
        flow_features(links, coords, args.time_bin),
        flow_layer=True,
    )

    dynamic_html = None
    if args.dynamic_html:
        dynamic_html = args.out_dir / f"xijt_dynamic_leaflet_{args.tile}.html"
        write_dynamic_leaflet_html(
            dynamic_html,
            "Dynamic x_ijt Flow",
            args.tile,
            bounds,
            links,
            coords,
            args.time_bin,
        )

    frames_dir = None
    video_path = None
    if not args.skip_animation:
        frames_dir, video_path = render_flow_frames(
            args.out_dir,
            links,
            segments,
            args.tile,
            args.frame_step,
            max_frames,
            args.video_fps,
        )

    print(f"static_png={static_png}")
    print(f"network_html={network_html}")
    print(f"flow_html={flow_html}")
    if dynamic_html:
        print(f"dynamic_html={dynamic_html}")
    if frames_dir:
        print(f"frames_dir={frames_dir}")
    if video_path:
        print(f"video={video_path}")


if __name__ == "__main__":
    main()
