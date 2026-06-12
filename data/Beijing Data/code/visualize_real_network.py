from __future__ import annotations

import argparse
import json
import math
import os
from datetime import datetime
from pathlib import Path

import cv2
import numpy as np
import pandas as pd


BASE_DIR = Path(__file__).resolve().parent
PROCESSED_DIR = BASE_DIR / "processed_real_network"
DEFAULT_OUT_DIR = PROCESSED_DIR / "visualizations"
DEFAULT_RUNS_DIR = DEFAULT_OUT_DIR / "runs"
DEFAULT_OSM_GEOMETRY_FILE = PROCESSED_DIR / "osm_matched_edge_geometry.csv"
TIME_WEIGHTED_OCCUPANCY_FILE = PROCESSED_DIR / "edge_time_weighted_occupancy_nonzero.csv"
XIJT_CAPACITY_FILE = PROCESSED_DIR / "edge_xijt_capacity_estimates.csv"
MPL_CACHE_DIR = PROCESSED_DIR / ".matplotlib"
TIME_BIN_MINUTES = 5
MPL_CACHE_DIR.mkdir(parents=True, exist_ok=True)
os.environ.setdefault("MPLCONFIGDIR", str(MPL_CACHE_DIR))

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection


METRIC_CONFIG = {
    "xijt": {
        "label": "x_ijt",
        "title": "x_ijt observed vehicles",
        "unit": "veh",
        "description": "observed/occupied vehicles on this edge during the time bin",
        "style_max": None,
    },
    "density": {
        "label": "density",
        "title": "Vehicle Density",
        "unit": "veh/km/lane",
        "description": "time-weighted average vehicles divided by edge length and lane count",
        "style_max": None,
    },
    "congestion": {
        "label": "congestion",
        "title": "Time-weighted Congestion Ratio",
        "unit": "ratio",
        "description": "time-weighted average occupancy divided by modeled jam storage; 1.0 means near modeled jam storage",
        "style_max": 1.0,
    },
    "saturation": {
        "label": "x_ijt load",
        "title": "x_ijt Relative Load Ratio",
        "unit": "ratio",
        "description": "x_ijt divided by a regression-calibrated capacity estimate for the same x_ijt sampling method",
        "style_max": 1.0,
    },
}

CONGESTION_COLOR_STOPS = [
    {"min": 0.0, "label": "0%", "color": "#2c7bb6"},
    {"min": 0.2, "label": "20%", "color": "#00a6ca"},
    {"min": 0.4, "label": "40%", "color": "#28a745"},
    {"min": 0.6, "label": "60%", "color": "#fdd349"},
    {"min": 0.8, "label": "80%", "color": "#f46d43"},
    {"min": 1.0, "label": "100%+", "color": "#b2182b"},
]

RELATIVE_LOAD_COLOR_STOPS = [
    {"min": 0.0, "label": "0%", "color": "#2c7bb6"},
    {"min": 0.2, "label": "20%", "color": "#00a6ca"},
    {"min": 0.4, "label": "40%", "color": "#28a745"},
    {"min": 0.6, "label": "60%", "color": "#f46d43"},
    {"min": 0.8, "label": "80%+", "color": "#b2182b"},
]


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


def convert_lonlat_for_tile(lon: float, lat: float, tile: str) -> tuple[float, float]:
    if uses_gcj02(tile):
        return wgs84_to_gcj02(lon, lat)
    return lon, lat


def load_matched_geometry(path: Path, tile: str) -> dict[int, list[tuple[float, float]]]:
    if not path.exists():
        return {}

    matched = pd.read_csv(path)
    geometries: dict[int, list[tuple[float, float]]] = {}
    for row in matched[["edge_index", "geometry_wgs84"]].dropna().itertuples(index=False):
        try:
            raw_points = json.loads(row.geometry_wgs84)
        except Exception:
            continue
        points: list[tuple[float, float]] = []
        for point in raw_points:
            if len(point) < 2:
                continue
            lon, lat = convert_lonlat_for_tile(float(point[0]), float(point[1]), tile)
            points.append((lon, lat))
        if len(points) >= 2:
            geometries[int(row.edge_index)] = points
    return geometries


def load_network(
    tile: str,
    geometry_source: str = "model",
    osm_geometry_file: Path = DEFAULT_OSM_GEOMETRY_FILE,
    unmatched_geometry: str = "fallback",
    exclude_link_types: set[str] | None = None,
) -> tuple[pd.DataFrame, pd.DataFrame, dict[int, tuple[float, float]], list[list[tuple[float, float]]]]:
    nodes = pd.read_csv(PROCESSED_DIR / "nodes_clean.csv")
    links = pd.read_csv(PROCESSED_DIR / "links_clean.csv")
    if exclude_link_types:
        links = links[~links["link_type_name"].astype(str).isin(exclude_link_types)].copy()
    matched_geometry = load_matched_geometry(osm_geometry_file, tile) if geometry_source == "osm-matched" else {}

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
    valid_edges: list[dict[str, object]] = []
    for row in links[["edge_index", "from_node_id", "to_node_id"]].itertuples(index=False):
        edge_index = int(row.edge_index)
        u = int(row.from_node_id)
        v = int(row.to_node_id)
        if u in coords and v in coords:
            if geometry_source == "osm-matched" and edge_index not in matched_geometry and unmatched_geometry == "hide":
                continue
            geometry = matched_geometry.get(edge_index, [coords[u], coords[v]])
            segments.append(geometry)
            used_source = "osm-matched" if edge_index in matched_geometry else "model"
            valid_edges.append({"edge_index": edge_index, "map_geometry": geometry, "geometry_source_used": used_source})

    links = links.merge(pd.DataFrame(valid_edges), on="edge_index", how="inner")
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
        "map_geometry",
    ]
    for row in links[cols].itertuples(index=False):
        u = int(row.from_node_id)
        v = int(row.to_node_id)
        if u not in coords or v not in coords:
            continue
        geometry = getattr(row, "map_geometry", None)
        if not geometry:
            geometry = [coords[u], coords[v]]
        features.append(
            {
                "type": "Feature",
                "geometry": {
                    "type": "LineString",
                    "coordinates": [[round(float(lon), 7), round(float(lat), 7)] for lon, lat in geometry],
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


def write_leaflet_html(
    out_path: Path,
    title: str,
    tile: str,
    bounds: tuple[float, float, float, float],
    features: list[dict[str, object]],
    flow_layer: bool,
    metric: str,
) -> None:
    config = tile_config(tile)
    cfg = metric_config(metric)
    geojson = json.dumps({"type": "FeatureCollection", "features": features}, ensure_ascii=False)
    bounds_json = json.dumps([[bounds[0], bounds[1]], [bounds[2], bounds[3]]])
    tile_layers_json = json.dumps(config["layers"])
    satellite_json = json.dumps(config["satellite"])
    style_max = cfg["style_max"]
    style_max_json = "null" if style_max is None else json.dumps(style_max)
    panel_class = "panel satellite-panel" if config["satellite"] else "panel"

    style_fn = """
function styleFeature(feature) {
  const p = feature.properties || {};
  if (p.metric_value !== undefined) {
    const v = Math.max(0, Number(p.metric_value || 0));
    const t = fixedStyleMax ? Math.min(1, v / fixedStyleMax) : Math.min(1, Math.log1p(v) / Math.log1p(maxValue || 1));
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
    <div class="muted">Metric: {cfg["label"]} ({cfg["description"]})</div>
    <div class="muted">{len(features):,} line features</div>
  </div>
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <script>
    const data = {geojson};
    const bounds = {bounds_json};
    const satellite = {satellite_json};
    const tileLayers = {tile_layers_json};
    const majorRoads = new Set(["快速路", "主干路", "次干路", "一级公路", "二级公路", "收费高速路"]);
    const fixedStyleMax = {style_max_json};
    const maxValue = data.features.reduce((m, f) => Math.max(m, Number((f.properties || {{}}).metric_value || 0)), 0);
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
        const metricLine = p.metric_value !== undefined ? `<b>{cfg["label"]}:</b> ${{p.metric_value}} {cfg["unit"]}<br>` : "";
        const vehicleLabel = p.flow_basis === "time_weighted_occupancy" ? "avg vehicles" : "x_ijt";
        layer.bindPopup(
          `${{metricLine}}<b>${{vehicleLabel}}:</b> ${{p.x_ijt ?? ""}}<br>` +
          `<b>density:</b> ${{p.density_veh_per_km_lane ?? ""}} veh/km/lane<br>` +
          `<b>congestion:</b> ${{p.congestion_index ?? ""}}<br>` +
          `<b>edge:</b> ${{p.edge_id}}<br>` +
          `<b>from-to:</b> ${{p.from_node_id}} -> ${{p.to_node_id}}<br>` +
          `<b>type:</b> ${{p.link_type_name || ""}}<br>` +
          `<b>lanes:</b> ${{p.lanes ?? ""}}<br>` +
          `<b>jam density:</b> ${{p.jam_density ?? ""}}<br>` +
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
    if pd.isna(value):
        return 0
    value = float(value)
    if value.is_integer():
        return int(value)
    return round(value, 3)


def compact_metric_value(value: float, metric: str) -> int | float:
    if pd.isna(value):
        return 0
    if metric == "congestion":
        return round(float(value), 4)
    return compact_flow_value(value)


def metric_uses_time_weighted_occupancy(metric: str) -> bool:
    return metric in {"density", "congestion"} and TIME_WEIGHTED_OCCUPANCY_FILE.exists()


def read_metric_flow(metric: str) -> tuple[pd.DataFrame, str]:
    if metric_uses_time_weighted_occupancy(metric):
        flow = pd.read_csv(TIME_WEIGHTED_OCCUPANCY_FILE)
        flow["x_ijt"] = flow["avg_vehicles"].astype(float)
        return flow, "time_weighted_occupancy"

    flow = pd.read_csv(PROCESSED_DIR / "edge_flow_xijt_nonzero.csv")
    return flow, "period_count"


def flow_basis_labels(metric: str, flow_basis: str) -> dict[str, str]:
    if metric == "saturation":
        return {
            "total_label": "x_ijt 合计",
            "vehicle_label": "x_ijt",
            "summary_label": "x_ijt",
        }
    if flow_basis == "time_weighted_occupancy":
        return {
            "total_label": "平均占用合计",
            "vehicle_label": "平均在途车辆",
            "summary_label": "平均占用",
        }
    return {
        "total_label": "总车辆数 x_ijt",
        "vehicle_label": "x_ijt",
        "summary_label": "动态流量",
    }


def metric_config(metric: str) -> dict[str, object]:
    return METRIC_CONFIG[metric]


def flow_output_prefix(metric: str) -> str:
    return "xijt" if metric == "xijt" else metric


def enrich_flow_metrics(flow: pd.DataFrame, links: pd.DataFrame) -> pd.DataFrame:
    metric_cols = [
        "edge_index",
        "from_node_id",
        "to_node_id",
        "link_type_name",
        "length",
        "number_of_lanes",
        "jam_density",
        "lane_capacity_in_vhc_per_hour",
        "speed_limit",
    ]
    if "map_geometry" in links.columns:
        metric_cols.append("map_geometry")
    flow = flow.merge(
        links[metric_cols],
        on=["edge_index", "from_node_id", "to_node_id"],
        how="inner",
    )
    if XIJT_CAPACITY_FILE.exists():
        capacity = pd.read_csv(
            XIJT_CAPACITY_FILE,
            usecols=["edge_index", "capacity_xijt", "model_capacity_xijt", "observed_max_xijt", "capacity_basis"],
        )
        flow = flow.merge(capacity, on="edge_index", how="left")

    if "avg_vehicles" in flow.columns:
        vehicle_count = flow["avg_vehicles"].astype(float)
        flow["x_ijt"] = vehicle_count
    else:
        vehicle_count = flow["x_ijt"].astype(float)
        flow["avg_vehicles"] = vehicle_count

    length = flow["length"].astype(float).clip(lower=1e-6)
    lanes = flow["number_of_lanes"].astype(float).clip(lower=1.0)
    jam_density = flow["jam_density"].astype(float).clip(lower=1e-6)
    storage_capacity = length * lanes * jam_density

    flow["density_veh_per_km_lane"] = vehicle_count / (length * lanes)
    flow["storage_capacity_veh"] = storage_capacity
    flow["congestion_index"] = vehicle_count / storage_capacity
    flow_for_rate = flow["entry_vehicles"].astype(float) if "entry_vehicles" in flow.columns else flow["x_ijt"].astype(float)
    flow["flow_rate_vph_from_xijt"] = flow_for_rate * (60.0 / TIME_BIN_MINUTES)
    flow["capacity_ratio_from_xijt"] = (
        flow["flow_rate_vph_from_xijt"]
        / flow["lane_capacity_in_vhc_per_hour"].astype(float).clip(lower=1e-6)
    )
    if "capacity_xijt" in flow.columns:
        xijt_capacity = flow["capacity_xijt"].astype(float).clip(lower=1.0)
        raw_xijt = flow["x_ijt"].astype(float)
        if "avg_vehicles" in flow.columns and "entry_vehicles" in flow.columns:
            raw_xijt = flow["entry_vehicles"].astype(float)
        flow["xijt_saturation"] = raw_xijt / xijt_capacity
    else:
        flow["capacity_xijt"] = np.nan
        flow["model_capacity_xijt"] = np.nan
        flow["observed_max_xijt"] = np.nan
        flow["capacity_basis"] = ""
        flow["xijt_saturation"] = flow["capacity_ratio_from_xijt"]
    return flow


def metric_column(metric: str) -> str:
    if metric == "xijt":
        return "x_ijt"
    if metric == "density":
        return "density_veh_per_km_lane"
    if metric == "congestion":
        return "congestion_index"
    if metric == "saturation":
        return "xijt_saturation"
    raise ValueError(f"Unsupported metric: {metric}")


def metric_value(row: object, metric: str) -> float:
    value = getattr(row, metric_column(metric))
    if pd.isna(value):
        return 0.0
    return float(value)


def write_dynamic_leaflet_html(
    out_path: Path,
    title: str,
    tile: str,
    bounds: tuple[float, float, float, float],
    links: pd.DataFrame,
    coords: dict[int, tuple[float, float]],
    metric: str,
) -> None:
    config = tile_config(tile)
    cfg = metric_config(metric)
    features = network_features(links, coords)
    visible_edge_count = len(features)
    matched_edge_count = (
        int((links["geometry_source_used"] == "osm-matched").sum()) if "geometry_source_used" in links.columns else 0
    )
    model_edge_count = max(0, visible_edge_count - matched_edge_count)
    if matched_edge_count and model_edge_count == 0:
        geometry_display_note = "仅绘制已获得 OSM 几何的路段；未匹配路段不显示。"
    elif matched_edge_count:
        geometry_display_note = f"OSM 几何 {matched_edge_count:,} 条，模型直线回退 {model_edge_count:,} 条。"
    else:
        geometry_display_note = f"使用模型几何绘制 {visible_edge_count:,} 条路段。"
    valid_edges = set(int(e) for e in links["edge_index"])

    flow, flow_basis = read_metric_flow(metric)
    flow_labels = flow_basis_labels(metric, flow_basis)
    total_value_label = flow_labels["total_label"]
    vehicle_value_label = flow_labels["vehicle_label"]
    summary_value_label = flow_labels["summary_label"]
    flow = flow[flow["edge_index"].isin(valid_edges)]
    flow = enrich_flow_metrics(flow, links)

    time_bins = [int(v) for v in sorted(flow["time_bin"].unique())]
    initial_index = 0
    metric_col = metric_column(metric)
    max_value = compact_metric_value(flow[metric_col].max(), metric) if not flow.empty else 1
    style_max = cfg["style_max"]
    style_max_json = "null" if style_max is None else json.dumps(style_max)
    if metric == "congestion":
        display_metric_name = "储车占用率"
        legend_title = "储车占用率色阶"
        legend_subtitle = "平均占用 / 拥堵储车能力"
    elif metric == "saturation":
        display_metric_name = "相对负载"
        legend_title = "相对负载色阶"
        legend_subtitle = "x_ijt / 回归估计经验容量"
    else:
        display_metric_name = str(cfg["label"])
        legend_title = f"{display_metric_name} 色阶"
        legend_subtitle = str(cfg["unit"])
    color_stops = RELATIVE_LOAD_COLOR_STOPS if metric == "saturation" else CONGESTION_COLOR_STOPS
    color_scale_max = 0.8 if metric == "saturation" else 1.0
    color_stops_json = json.dumps(color_stops, separators=(",", ":"))
    if metric in {"congestion", "saturation"}:
        legend_segment_width = 100 / len(color_stops)
        legend_gradient = "linear-gradient(to right, " + ", ".join(
            f"{stop['color']} {idx * legend_segment_width:.4g}% {(idx + 1) * legend_segment_width:.4g}%"
            for idx, stop in enumerate(color_stops)
        ) + ")"
        legend_tick_html = "".join(f"<span>{stop['label']}</span>" for stop in color_stops)
    else:
        legend_gradient = "linear-gradient(to right, hsl(215, 92%, 48%), hsl(155, 92%, 48%), hsl(70, 92%, 48%), hsl(0, 92%, 48%))"
        legend_tick_html = f"<span>0</span><span>{max_value} {cfg['unit']}</span>"
    high_load_threshold = 0.8 if metric == "saturation" else 1.0
    high_load_label = "高负载 ≥80%" if metric == "saturation" else "超过 100%"
    high_load_phrase = "达到高负载" if metric == "saturation" else "超过 100%"
    duration_title = "持续高负载" if metric == "saturation" else "持续拥堵"
    duration_subtitle = "连续达到 80% 相对负载的时间" if metric == "saturation" else "连续超过 100% 的时间"

    flow_by_bin: dict[str, list[list[int | float]]] = {}
    duration_by_bin: dict[str, list[list[int]]] = {}
    time_meta: dict[str, dict[str, int | float]] = {}
    duration_counter: dict[int, int] = {}
    threshold_col = metric_col if metric in {"congestion", "saturation"} else "congestion_index"
    for time_bin, group in flow.groupby("time_bin", sort=True):
        key = str(int(time_bin))
        values = group[["edge_index", metric_col]].to_numpy()
        congested_edges = int((group[threshold_col] >= high_load_threshold).sum())
        congested_edge_ids = set(
            int(edge_index)
            for edge_index in group.loc[group[threshold_col] >= high_load_threshold, "edge_index"]
        )
        duration_counter = {
            edge_index: duration + 1
            for edge_index, duration in duration_counter.items()
            if edge_index in congested_edge_ids
        }
        for edge_index in congested_edge_ids:
            duration_counter.setdefault(edge_index, 1)
        flow_by_bin[key] = [
            [int(edge_index), compact_metric_value(metric_value, metric)]
            for edge_index, metric_value in values
        ]
        duration_by_bin[key] = [[edge_index, duration] for edge_index, duration in duration_counter.items()]
        total_vehicle_col = "entry_vehicles" if metric == "saturation" and "entry_vehicles" in group.columns else "x_ijt"
        time_meta[key] = {
            "start_min": int(group["start_min"].iloc[0]),
            "end_min": int(group["end_min"].iloc[0]),
            "active_edges": int(len(group)),
            "active_share": compact_metric_value(len(group) / visible_edge_count if visible_edge_count else 0, "congestion"),
            "congested_edges": congested_edges,
            "congested_share": compact_metric_value(
                congested_edges / visible_edge_count if visible_edge_count else 0,
                "congestion",
            ),
            "total_xijt": compact_flow_value(group[total_vehicle_col].sum()),
            "mean_metric": compact_metric_value(group[metric_col].mean(), metric),
            "p95_metric": compact_metric_value(group[metric_col].quantile(0.95), metric),
            "max_metric": compact_metric_value(group[metric_col].max(), metric),
        }

    network_json = json.dumps({"type": "FeatureCollection", "features": features}, ensure_ascii=False, separators=(",", ":"))
    flow_json = json.dumps(flow_by_bin, ensure_ascii=False, separators=(",", ":"))
    duration_json = json.dumps(duration_by_bin, ensure_ascii=False, separators=(",", ":"))
    meta_json = json.dumps(time_meta, ensure_ascii=False, separators=(",", ":"))
    bounds_json = json.dumps([[bounds[0], bounds[1]], [bounds[2], bounds[3]]])
    tile_layers_json = json.dumps(config["layers"], separators=(",", ":"))
    satellite_json = json.dumps(config["satellite"])
    panel_class = "panel satellite-panel" if config["satellite"] else "panel"
    legend_class = "legend-panel satellite-panel" if config["satellite"] else "legend-panel"
    hotspot_class = "hotspot-panel satellite-panel" if config["satellite"] else "hotspot-panel"

    html = f"""<!doctype html>
<html lang="zh-CN">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{title}</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <style>
    html, body, #map {{ height: 100%; margin: 0; }}
    body {{
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", "PingFang SC", "Microsoft YaHei", sans-serif;
      color: #17202a;
    }}
    .panel,
    .legend-panel,
    .hotspot-panel {{
      position: absolute;
      z-index: 1000;
      background: rgba(255, 255, 255, 0.92);
      border: 1px solid rgba(0, 0, 0, 0.15);
      border-radius: 6px;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.12);
      font-size: 13px;
      line-height: 1.4;
    }}
    .panel {{
      top: 12px;
      left: 12px;
      width: min(460px, calc(100vw - 32px));
      padding: 10px 12px 12px;
    }}
    .legend-panel {{
      left: 12px;
      bottom: 20px;
      width: min(360px, calc(100vw - 32px));
      padding: 10px 12px;
    }}
    .hotspot-panel {{
      top: 12px;
      right: 12px;
      width: min(340px, calc(100vw - 32px));
      max-height: min(540px, calc(100vh - 40px));
      overflow: auto;
      padding: 10px 12px;
    }}
    .panel h1 {{
      font-size: 15px;
      margin: 0 0 5px;
      line-height: 1.25;
    }}
    .panel .muted,
    .legend-panel .muted,
    .hotspot-panel .muted,
    .stat .label {{
      color: #55606b;
    }}
    .satellite-panel {{
      background: rgba(7, 12, 14, 0.80);
      border-color: rgba(255, 255, 255, 0.28);
      color: #f5f7f8;
      box-shadow: 0 2px 14px rgba(0, 0, 0, 0.35);
    }}
    .satellite-panel .muted,
    .satellite-panel .stat .label {{
      color: rgba(245, 247, 248, 0.76);
    }}
    .mode-tabs {{
      display: grid;
      grid-template-columns: repeat(3, minmax(0, 1fr));
      gap: 6px;
      margin-top: 10px;
    }}
    .mode-button {{
      min-width: 0;
      padding: 0 8px;
      font-size: 12px;
      white-space: nowrap;
    }}
    .mode-button.active {{
      border-color: rgba(25, 96, 196, 0.58);
      background: #1960c4;
      color: #fff;
      font-weight: 650;
    }}
    .satellite-panel .mode-button.active {{
      border-color: rgba(255, 255, 255, 0.5);
      background: rgba(40, 166, 202, 0.78);
    }}
    .summary {{
      margin-top: 8px;
      padding: 8px 9px;
      border-radius: 5px;
      background: rgba(25, 96, 196, 0.08);
      color: #243241;
      font-size: 12px;
    }}
    .satellite-panel .summary {{
      background: rgba(255, 255, 255, 0.10);
      color: #f5f7f8;
    }}
    .info-grid,
    .readout {{
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 6px 10px;
      font-variant-numeric: tabular-nums;
    }}
    .info-grid {{
      margin-top: 8px;
      padding-top: 8px;
      border-top: 1px solid rgba(0, 0, 0, 0.10);
    }}
    .satellite-panel .info-grid {{
      border-top-color: rgba(255, 255, 255, 0.18);
    }}
    .stat {{
      min-width: 0;
    }}
    .stat .label,
    .stat .value {{
      display: block;
    }}
    .stat .label {{
      font-size: 11px;
    }}
    .stat .value {{
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
      font-weight: 650;
    }}
    .controls {{
      display: grid;
      grid-template-columns: 76px 1fr;
      gap: 8px 10px;
      align-items: center;
      margin-top: 10px;
    }}
    button {{
      height: 30px;
      border: 1px solid rgba(0, 0, 0, 0.18);
      border-radius: 5px;
      color: #17202a;
      background: rgba(255, 255, 255, 0.88);
      cursor: pointer;
      font: inherit;
    }}
    .satellite-panel button {{
      border-color: rgba(255, 255, 255, 0.36);
      color: #f5f7f8;
      background: rgba(255, 255, 255, 0.13);
    }}
    input[type="range"] {{ width: 100%; }}
    .readout {{
      grid-column: 1 / -1;
      padding-top: 2px;
    }}
    .legend-title {{
      display: flex;
      justify-content: space-between;
      gap: 12px;
      align-items: baseline;
      font-weight: 650;
      margin-bottom: 7px;
    }}
    .legend-bar {{
      height: 12px;
      border-radius: 6px;
      background: {legend_gradient};
      border: 1px solid rgba(0, 0, 0, 0.16);
    }}
    .satellite-panel .legend-bar {{
      border-color: rgba(255, 255, 255, 0.24);
    }}
    .legend-ticks {{
      display: flex;
      justify-content: space-between;
      gap: 4px;
      margin-top: 5px;
      font-size: 11px;
      color: #55606b;
      font-variant-numeric: tabular-nums;
    }}
    .satellite-panel .legend-ticks {{
      color: rgba(245, 247, 248, 0.76);
    }}
    .hotspot-header {{
      display: flex;
      align-items: baseline;
      justify-content: space-between;
      gap: 12px;
      margin-bottom: 8px;
      font-weight: 650;
    }}
    .hotspot-list {{
      list-style: none;
      display: grid;
      gap: 6px;
      padding: 0;
      margin: 0;
    }}
    .hotspot-item {{
      display: grid;
      grid-template-columns: 22px 1fr auto;
      gap: 8px;
      align-items: center;
      padding: 7px 8px;
      border: 1px solid rgba(0, 0, 0, 0.08);
      border-radius: 5px;
      background: rgba(255, 255, 255, 0.56);
      cursor: pointer;
    }}
    .hotspot-item:hover {{
      border-color: rgba(25, 96, 196, 0.45);
      background: rgba(25, 96, 196, 0.08);
    }}
    .satellite-panel .hotspot-item {{
      border-color: rgba(255, 255, 255, 0.14);
      background: rgba(255, 255, 255, 0.08);
    }}
    .hotspot-rank {{
      color: #55606b;
      font-variant-numeric: tabular-nums;
    }}
    .satellite-panel .hotspot-rank {{
      color: rgba(245, 247, 248, 0.70);
    }}
    .hotspot-name {{
      min-width: 0;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
      font-weight: 600;
    }}
    .hotspot-sub {{
      font-size: 11px;
      color: #66717d;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }}
    .satellite-panel .hotspot-sub {{
      color: rgba(245, 247, 248, 0.70);
    }}
    .hotspot-value {{
      font-weight: 700;
      font-variant-numeric: tabular-nums;
      white-space: nowrap;
    }}
    @media (max-width: 900px) {{
      .hotspot-panel {{
        top: auto;
        right: 12px;
        bottom: 92px;
        max-height: 34vh;
      }}
    }}
    @media (max-width: 640px) {{
      .panel {{
        width: calc(100vw - 32px);
      }}
      .hotspot-panel {{
        width: calc(100vw - 32px);
      }}
      .legend-panel {{
        width: calc(100vw - 32px);
        bottom: 16px;
      }}
    }}
  </style>
</head>
<body>
  <div id="map"></div>
  <div class="{panel_class}">
    <h1>{title}</h1>
    <div class="muted">{config["name"]}</div>
    <div class="muted">{geometry_display_note}</div>
    <div class="info-grid">
      <div class="stat"><span class="label">可视化路段</span><span class="value">{visible_edge_count:,}</span></div>
      <div class="stat"><span class="label">OSM 几何</span><span class="value">{matched_edge_count:,}</span></div>
      <div class="stat"><span class="label">时间片</span><span class="value">{len(time_bins):,} x {TIME_BIN_MINUTES} min</span></div>
      <div class="stat"><span class="label">颜色指标</span><span class="value">{display_metric_name}</span></div>
    </div>
    <div class="mode-tabs" role="group" aria-label="地图视图模式">
      <button class="mode-button active" type="button" data-mode="current">{display_metric_name}</button>
      <button class="mode-button" type="button" data-mode="delta">较上一片变化</button>
      <button class="mode-button" type="button" data-mode="duration">{duration_title}</button>
    </div>
    <div class="summary" id="autoSummary"></div>
    <div class="controls">
      <button id="playButton" type="button">播放</button>
      <input id="timeSlider" type="range" min="0" max="{max(0, len(time_bins) - 1)}" value="{initial_index}" step="1">
      <div class="readout">
        <div class="stat"><span class="label">当前时间片</span><span class="value" id="binLabel"></span></div>
        <div class="stat"><span class="label">时段</span><span class="value" id="timeLabel"></span></div>
        <div class="stat"><span class="label">动态路段</span><span class="value" id="activeLabel"></span></div>
        <div class="stat"><span class="label">{total_value_label}</span><span class="value" id="totalLabel"></span></div>
        <div class="stat"><span class="label">平均{display_metric_name}</span><span class="value" id="meanLabel"></span></div>
        <div class="stat"><span class="label">最大{display_metric_name}</span><span class="value" id="maxLabel"></span></div>
        <div class="stat"><span class="label">{high_load_label}</span><span class="value" id="jamLabel"></span></div>
        <div class="stat"><span class="label">95 分位</span><span class="value" id="p95Label"></span></div>
      </div>
    </div>
  </div>
  <div class="{hotspot_class}">
    <div class="hotspot-header">
      <span>热点榜单</span>
      <span class="muted" id="hotspotModeLabel"></span>
    </div>
    <ol class="hotspot-list" id="hotspotList"></ol>
  </div>
  <div class="{legend_class}">
    <div class="legend-title">
      <span id="legendTitle">{legend_title}</span>
      <span class="muted" id="legendSubtitle">{legend_subtitle}</span>
    </div>
    <div class="legend-bar" id="legendBar" aria-hidden="true"></div>
    <div class="legend-ticks" id="legendTicks">{legend_tick_html}</div>
  </div>
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <script>
    const network = {network_json};
    const flowByBin = {flow_json};
    const durationByBin = {duration_json};
    const timeMeta = {meta_json};
    const timeBins = {json.dumps(time_bins, separators=(",", ":"))};
    const bounds = {bounds_json};
    const satellite = {satellite_json};
    const maxValue = {max_value};
    const fixedStyleMax = {style_max_json};
    const metricKey = {json.dumps(metric)};
    const metricLabel = {json.dumps(cfg["label"])};
    const metricUnit = {json.dumps(cfg["unit"])};
    const displayMetricName = {json.dumps(display_metric_name, ensure_ascii=False)};
    const vehicleValueLabel = {json.dumps(vehicle_value_label, ensure_ascii=False)};
    const summaryValueLabel = {json.dumps(summary_value_label, ensure_ascii=False)};
    const highLoadThreshold = {json.dumps(high_load_threshold)};
    const highLoadLabel = {json.dumps(high_load_label, ensure_ascii=False)};
    const highLoadPhrase = {json.dumps(high_load_phrase, ensure_ascii=False)};
    const totalEdges = {visible_edge_count};
    const congestionStops = {color_stops_json};
    const colorScaleMax = {json.dumps(color_scale_max)};
    const tileLayers = {tile_layers_json};
    const majorRoads = new Set(["快速路", "主干路", "次干路", "一级公路", "二级公路", "收费高速路"]);
    const timeBinMinutes = {TIME_BIN_MINUTES};
    const ratioMetricKeys = new Set(["congestion", "saturation"]);
    const modeDetails = {{
      current: {{
        label: {json.dumps(display_metric_name, ensure_ascii=False)},
        listLabel: "最高" + {json.dumps(display_metric_name, ensure_ascii=False)},
        legendTitle: {json.dumps(legend_title, ensure_ascii=False)},
        legendSubtitle: {json.dumps(legend_subtitle, ensure_ascii=False)},
        legendGradient: {json.dumps(legend_gradient)},
        legendTicks: {json.dumps([stop["label"] for stop in color_stops], ensure_ascii=False)}
      }},
      delta: {{
        label: "较上一片变化",
        listLabel: "恶化最快",
        legendTitle: "变化色阶",
        legendSubtitle: "较上一时间片，蓝色缓解 / 红色加重",
        legendGradient: "linear-gradient(to right, #2166ac 0%, #67a9cf 30%, #f7f7f7 50%, #f46d43 70%, #b2182b 100%)",
        legendTicks: ["-50pp", "0", "+50pp+"]
      }},
      duration: {{
        label: {json.dumps(duration_title, ensure_ascii=False)},
        listLabel: "持续最久",
        legendTitle: {json.dumps(duration_title, ensure_ascii=False)},
        legendSubtitle: {json.dumps(duration_subtitle, ensure_ascii=False)},
        legendGradient: "linear-gradient(to right, #fdd349 0%, #f46d43 52%, #b2182b 100%)",
        legendTicks: ["5min", "30min", "60min+"]
      }}
    }};
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
    const edgeProperties = new Map();
    let activeValues = new Map();
    let currentFrameValues = new Map();
    let previousFrameValues = new Map();
    let durationFrameValues = new Map();
    let activeIndex = {initial_index};
    let viewMode = "current";
    let timer = null;

    function minuteText(startMin, endMin) {{
      const pad = (v) => String(v).padStart(2, "0");
      return `${{pad(Math.floor(startMin / 60))}}:${{pad(startMin % 60)}}-${{pad(Math.floor(endMin / 60))}}:${{pad(endMin % 60)}}`;
    }}

    function baseStyle(properties) {{
      if (satellite) {{
        return {{
          color: "#b8c4ca",
          weight: majorRoads.has(properties.link_type_name) ? 1.15 : 0.55,
          opacity: majorRoads.has(properties.link_type_name) ? 0.34 : 0.16,
          lineCap: "round"
        }};
      }}
      return {{
        color: "#6f7d8a",
        weight: majorRoads.has(properties.link_type_name) ? 1.05 : 0.48,
        opacity: majorRoads.has(properties.link_type_name) ? 0.30 : 0.13,
        lineCap: "round"
      }};
    }}

    function formatMetric(value) {{
      const v = Number(value || 0);
      if (ratioMetricKeys.has(metricKey)) {{
        const pct = v * 100;
        const digits = pct >= 10 ? 0 : 1;
        return `${{pct.toLocaleString(undefined, {{ maximumFractionDigits: digits }})}}%`;
      }}
      return `${{v.toLocaleString(undefined, {{ maximumFractionDigits: 3 }})}} ${{metricUnit}}`;
    }}

    function formatDelta(value) {{
      const pctPoints = Number(value || 0) * 100;
      const sign = pctPoints > 0 ? "+" : "";
      const digits = Math.abs(pctPoints) >= 10 ? 0 : 1;
      return `${{sign}}${{pctPoints.toLocaleString(undefined, {{ maximumFractionDigits: digits }})}} pp`;
    }}

    function formatDuration(value) {{
      return `${{Number(value || 0) * timeBinMinutes}} min`;
    }}

    function formatModeValue(value, mode = viewMode) {{
      if (mode === "delta") return formatDelta(value);
      if (mode === "duration") return formatDuration(value);
      return formatMetric(value);
    }}

    function congestionColor(value) {{
      const v = Number(value || 0);
      for (let i = congestionStops.length - 1; i >= 0; i -= 1) {{
        if (v >= congestionStops[i].min) return congestionStops[i].color;
      }}
      return congestionStops[0].color;
    }}

    function metricIntensity(value) {{
      const v = Math.max(0, Number(value || 0));
      if (ratioMetricKeys.has(metricKey)) return Math.min(1, v / colorScaleMax);
      return fixedStyleMax ? Math.min(1, v / fixedStyleMax) : Math.min(1, Math.log1p(v) / Math.log1p(maxValue || 1));
    }}

    function currentColor(value) {{
      if (ratioMetricKeys.has(metricKey)) return congestionColor(value);
      const t = metricIntensity(value);
      const hue = satellite ? 185 - 185 * t : 215 - 215 * t;
      return `hsl(${{hue}}, 92%, 48%)`;
    }}

    function deltaStyle(value) {{
      const v = Number(value || 0);
      if (Math.abs(v) < 0.0001) return null;
      const t = Math.min(1, Math.abs(v) / 0.5);
      return {{
        color: v > 0 ? "#b2182b" : "#2166ac",
        weight: (satellite ? 1.25 : 0.95) + (satellite ? 5.4 : 4.8) * t,
        opacity: satellite ? 0.98 : 0.92,
        lineCap: "round"
      }};
    }}

    function durationStyle(value) {{
      const bins = Number(value || 0);
      if (!bins) return null;
      const t = Math.min(1, bins / 12);
      return {{
        color: t > 0.7 ? "#b2182b" : (t > 0.35 ? "#f46d43" : "#fdd349"),
        weight: (satellite ? 1.35 : 1.0) + (satellite ? 6.1 : 5.2) * t,
        opacity: satellite ? 0.98 : 0.92,
        lineCap: "round"
      }};
    }}

    function flowStyle(value, properties) {{
      if (!value) return baseStyle(properties);
      if (viewMode === "delta") return deltaStyle(value) || baseStyle(properties);
      if (viewMode === "duration") return durationStyle(value) || baseStyle(properties);
      const t = metricIntensity(value);
      return {{
        color: currentColor(value),
        weight: (satellite ? 1.35 : 1.0) + (satellite ? 6.4 : 5.3) * t,
        opacity: satellite ? 0.98 : 0.92,
        lineCap: "round"
      }};
    }}

    function entriesForIndex(index) {{
      const bin = timeBins[index];
      return new Map(flowByBin[String(bin)] || []);
    }}

    function durationForIndex(index) {{
      const bin = timeBins[index];
      return new Map(durationByBin[String(bin)] || []);
    }}

    function valuesForMode(index, mode) {{
      if (mode === "duration") return durationForIndex(index);
      const current = entriesForIndex(index);
      if (mode !== "delta") return current;
      const previous = index > 0 ? entriesForIndex(index - 1) : new Map();
      const edgeIds = new Set([...current.keys(), ...previous.keys()]);
      const delta = new Map();
      edgeIds.forEach((edgeIndex) => {{
        const value = Number(current.get(edgeIndex) || 0) - Number(previous.get(edgeIndex) || 0);
        if (Math.abs(value) >= 0.0001) delta.set(edgeIndex, Number(value.toFixed(4)));
      }});
      return delta;
    }}

    function popupHtml(properties) {{
      const value = activeValues.get(properties.edge_index) || 0;
      const currentValue = currentFrameValues.get(properties.edge_index) || 0;
      const bin = timeBins[activeIndex];
      return (
        `<b>time_bin:</b> ${{bin}}<br>` +
        `<b>${{modeDetails[viewMode].label}}:</b> ${{formatModeValue(value)}}<br>` +
        `<b>当前${{displayMetricName}}:</b> ${{formatMetric(currentValue)}}<br>` +
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
        edgeProperties.set(properties.edge_index, properties);
        layer.on("click", () => layer.bindPopup(popupHtml(properties)).openPopup());
      }}
    }}).addTo(map);

    const slider = document.getElementById("timeSlider");
    const playButton = document.getElementById("playButton");
    const binLabel = document.getElementById("binLabel");
    const timeLabel = document.getElementById("timeLabel");
    const activeLabel = document.getElementById("activeLabel");
    const totalLabel = document.getElementById("totalLabel");
    const meanLabel = document.getElementById("meanLabel");
    const maxLabel = document.getElementById("maxLabel");
    const jamLabel = document.getElementById("jamLabel");
    const p95Label = document.getElementById("p95Label");
    const modeButtons = Array.from(document.querySelectorAll(".mode-button"));
    const autoSummary = document.getElementById("autoSummary");
    const hotspotList = document.getElementById("hotspotList");
    const hotspotModeLabel = document.getElementById("hotspotModeLabel");
    const legendTitleEl = document.getElementById("legendTitle");
    const legendSubtitleEl = document.getElementById("legendSubtitle");
    const legendBarEl = document.getElementById("legendBar");
    const legendTicksEl = document.getElementById("legendTicks");

    function updateLegend() {{
      const detail = modeDetails[viewMode];
      legendTitleEl.textContent = detail.legendTitle;
      legendSubtitleEl.textContent = detail.legendSubtitle;
      legendBarEl.style.background = detail.legendGradient;
      legendTicksEl.innerHTML = detail.legendTicks.map((label) => `<span>${{label}}</span>`).join("");
    }}

    function updateModeButtons() {{
      modeButtons.forEach((button) => {{
        button.classList.toggle("active", button.dataset.mode === viewMode);
      }});
    }}

    function hotspotEntries() {{
      const entries = [...activeValues.entries()].filter(([, value]) => Number(value) !== 0);
      if (viewMode === "delta") {{
        return entries
          .filter(([, value]) => Number(value) > 0)
          .sort((a, b) => Number(b[1]) - Number(a[1]))
          .slice(0, 10);
      }}
      return entries
        .sort((a, b) => Number(b[1]) - Number(a[1]))
        .slice(0, 10);
    }}

    function focusEdge(edgeIndex) {{
      const layer = edgeLayers.get(edgeIndex);
      const properties = edgeProperties.get(edgeIndex);
      if (!layer || !properties) return;
      const bounds = layer.getBounds ? layer.getBounds() : null;
      if (bounds && bounds.isValid && bounds.isValid()) {{
        map.fitBounds(bounds, {{ maxZoom: 16, padding: [80, 80] }});
      }}
      layer.bindPopup(popupHtml(properties)).openPopup();
    }}

    function updateHotspots() {{
      const detail = modeDetails[viewMode];
      hotspotModeLabel.textContent = detail.listLabel;
      const items = hotspotEntries();
      if (!items.length) {{
        hotspotList.innerHTML = `<li class="hotspot-item"><span></span><span class="hotspot-name">当前时间片暂无可排序路段</span><span></span></li>`;
        return;
      }}
      hotspotList.innerHTML = "";
      items.forEach(([edgeIndex, value], index) => {{
        const properties = edgeProperties.get(edgeIndex) || {{}};
        const item = document.createElement("li");
        item.className = "hotspot-item";
        item.innerHTML = (
          `<span class="hotspot-rank">${{index + 1}}</span>` +
          `<span><span class="hotspot-name">${{properties.edge_id || edgeIndex}}</span>` +
          `<span class="hotspot-sub">${{properties.link_type_name || ""}} | ${{properties.from_node_id || ""}} -> ${{properties.to_node_id || ""}}</span></span>` +
          `<span class="hotspot-value">${{formatModeValue(value)}}</span>`
        );
        item.addEventListener("click", () => focusEdge(edgeIndex));
        hotspotList.appendChild(item);
      }});
    }}

    function updateSummary(meta, bin) {{
      const timeText = minuteText(meta.start_min, meta.end_min);
      if (viewMode === "delta") {{
        if (activeIndex === 0) {{
          autoSummary.textContent = `${{timeText}} 是首个时间片，暂无上一片可比较；当前可先看拥堵率分布。`;
          return;
        }}
        const threshold = 0.05;
        const values = [...activeValues.values()].map(Number);
        const worsened = values.filter((value) => value >= threshold).length;
        const improved = values.filter((value) => value <= -threshold).length;
        const maxRise = values.length ? Math.max(...values) : 0;
        const maxDrop = values.length ? Math.min(...values) : 0;
        autoSummary.textContent = `${{timeText}} 较上一时间片有 ${{worsened.toLocaleString()}} 条路段明显加重、${{improved.toLocaleString()}} 条明显缓解；最大上升 ${{formatDelta(maxRise)}}，最大下降 ${{formatDelta(maxDrop)}}。`;
        return;
      }}
      if (viewMode === "duration") {{
        const durations = [...activeValues.values()].map(Number);
        const maxDuration = durations.length ? Math.max(...durations) : 0;
        const longEdges = durations.filter((value) => value >= 6).length;
        autoSummary.textContent = `${{timeText}} 当前有 ${{Number(meta.congested_edges).toLocaleString()}} 条路段${{highLoadPhrase}}；最长连续${{highLoadLabel}} ${{formatDuration(maxDuration)}}，其中 ${{longEdges.toLocaleString()}} 条已持续 30 分钟以上。`;
        return;
      }}
      autoSummary.textContent = `${{timeText}} 有 ${{Number(meta.active_edges).toLocaleString()}} 条路段出现 ${{summaryValueLabel}}，其中 ${{Number(meta.congested_edges).toLocaleString()}} 条${{highLoadPhrase}}；平均${{displayMetricName}} ${{formatMetric(meta.mean_metric)}}，95 分位 ${{formatMetric(meta.p95_metric)}}。`;
    }}

    function setFrame(index) {{
      activeIndex = Math.max(0, Math.min(timeBins.length - 1, Number(index)));
      const bin = timeBins[activeIndex];
      currentFrameValues = entriesForIndex(activeIndex);
      previousFrameValues = activeIndex > 0 ? entriesForIndex(activeIndex - 1) : new Map();
      durationFrameValues = durationForIndex(activeIndex);
      const nextValues = valuesForMode(activeIndex, viewMode);
      const touched = new Set([...activeValues.keys(), ...nextValues.keys()]);

      touched.forEach((edgeIndex) => {{
        const layer = edgeLayers.get(edgeIndex);
        if (!layer) return;
        const properties = layer.feature.properties || {{}};
        layer.setStyle(flowStyle(nextValues.get(edgeIndex) || 0, properties));
      }});

      activeValues = nextValues;
      slider.value = String(activeIndex);
      const meta = timeMeta[String(bin)] || {{
        start_min: bin * 5,
        end_min: bin * 5 + 5,
        active_edges: 0,
        active_share: 0,
        congested_edges: 0,
        congested_share: 0,
        total_xijt: 0,
        mean_metric: 0,
        p95_metric: 0,
        max_metric: 0
      }};
      const activePct = Number(meta.active_share || 0) * 100;
      const congestedPct = Number(meta.congested_share || 0) * 100;
      binLabel.textContent = `bin ${{bin}}`;
      timeLabel.textContent = minuteText(meta.start_min, meta.end_min);
      activeLabel.textContent = `${{Number(meta.active_edges).toLocaleString()}} / ${{totalEdges.toLocaleString()}} (${{activePct.toFixed(1)}}%)`;
      totalLabel.textContent = Number(meta.total_xijt).toLocaleString();
      meanLabel.textContent = formatMetric(meta.mean_metric);
      maxLabel.textContent = formatMetric(meta.max_metric);
      jamLabel.textContent = `${{Number(meta.congested_edges).toLocaleString()}} (${{congestedPct.toFixed(1)}}%)`;
      p95Label.textContent = formatMetric(meta.p95_metric);
      updateModeButtons();
      updateLegend();
      updateSummary(meta, bin);
      updateHotspots();
    }}

    function stopPlayback() {{
      if (timer) window.clearInterval(timer);
      timer = null;
      playButton.textContent = "播放";
    }}

    function startPlayback() {{
      stopPlayback();
      playButton.textContent = "暂停";
      timer = window.setInterval(() => {{
        const nextIndex = (activeIndex + 1) % timeBins.length;
        setFrame(nextIndex);
      }}, 520);
    }}

    slider.addEventListener("input", () => {{
      stopPlayback();
      setFrame(Number(slider.value));
    }});
    modeButtons.forEach((button) => {{
      button.addEventListener("click", () => {{
        stopPlayback();
        viewMode = button.dataset.mode || "current";
        setFrame(activeIndex);
      }});
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


def safe_name(text: str) -> str:
    keep: list[str] = []
    for ch in text.lower():
        if ch.isalnum() or ch in {"-", "_"}:
            keep.append(ch)
        elif ch in {" ", ".", "/"}:
            keep.append("-")
    name = "".join(keep).strip("-_")
    while "--" in name:
        name = name.replace("--", "-")
    return name or "run"


def make_run_dir(args: argparse.Namespace) -> Path:
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    dynamic = "dynamic" if args.dynamic_html else "static"
    geometry = "model"
    if args.geometry_source == "osm-matched":
        geometry = f"osmgeom-{args.unmatched_geometry}"
    filtering = "filtered" if args.exclude_link_types else "alllinks"
    animation = "with-video" if not args.skip_animation else "html-only"
    default_name = f"{timestamp}_{args.tile}_{args.metric}_{geometry}_{filtering}_{dynamic}_{animation}"
    run_name = safe_name(args.run_name) if args.run_name else safe_name(default_name)
    out_dir = DEFAULT_RUNS_DIR / run_name
    suffix = 2
    while out_dir.exists():
        out_dir = DEFAULT_RUNS_DIR / f"{run_name}_{suffix}"
        suffix += 1
    out_dir.mkdir(parents=True, exist_ok=False)
    return out_dir


def write_run_readme(
    out_dir: Path,
    args: argparse.Namespace,
    output_paths: dict[str, Path | None],
    link_count: int,
    used_osm_geometry_count: int,
) -> Path:
    cfg = metric_config(args.metric)
    geometry_note = "原始模型起终点直线"
    if args.geometry_source == "osm-matched":
        geometry_note = "优先使用 OSM 匹配曲线；匹配缺失路段回退为模型起终点直线"

    lines = [
        "# Visualization Run",
        "",
        "## Output Features",
        "",
        f"- Tile/base map: `{args.tile}`",
        f"- Metric: `{args.metric}` - {cfg['description']}",
        f"- Dynamic HTML: `{bool(args.dynamic_html)}`",
        f"- Animation/video generated: `{not args.skip_animation}`",
        f"- Geometry source: `{args.geometry_source}` ({geometry_note})",
        f"- Unmatched geometry handling: `{args.unmatched_geometry}`",
        f"- Excluded link types: `{args.exclude_link_types or 'none'}`",
        f"- OSM geometry file: `{args.osm_geometry_file}`",
        f"- Links drawn: `{link_count}`",
        f"- Links using OSM matched geometry: `{used_osm_geometry_count}`",
        "",
        "## Files",
        "",
    ]
    for label, path in output_paths.items():
        if path is None:
            continue
        rel = path.relative_to(out_dir) if path.is_relative_to(out_dir) else path
        lines.append(f"- {label}: `{rel}`")
    lines.extend(
        [
            "",
            "## Notes",
            "",
            "- `congestion` is calculated as time-weighted average vehicles divided by `(length_km * number_of_lanes * jam_density)` when `edge_time_weighted_occupancy_nonzero.csv` is available.",
            "- `saturation` is calculated as `x_ijt / capacity_xijt`, where `capacity_xijt` comes from `edge_xijt_capacity_estimates.csv` and is calibrated to the same interval-presence sampling method as `x_ijt`.",
            "- This keeps the original `x_ijt` signal while avoiding division by an incompatible hourly flow capacity or instantaneous storage capacity.",
            "- If `geometry_source` is `osm-matched`, unmatched roads are either hidden or drawn as original straight model segments depending on `unmatched_geometry`.",
            "- HTML map files need network access for Leaflet and online map tiles.",
            "",
        ]
    )
    readme = out_dir / "README.md"
    readme.write_text("\n".join(lines), encoding="utf-8")
    return readme


def write_latest_pointer(out_dir: Path, readme: Path, output_paths: dict[str, Path | None]) -> Path:
    DEFAULT_OUT_DIR.mkdir(parents=True, exist_ok=True)
    index = DEFAULT_OUT_DIR / "README.md"
    index.write_text(
        "\n".join(
            [
                "# Visualizations",
                "",
                "New outputs are organized as one folder per run under `runs/`.",
                "Open `LATEST_OUTPUT.md` to find the most recent generated run.",
                "",
                "Files directly in this folder were produced before the per-run organization was added and are kept as legacy outputs.",
                "",
            ]
        ),
        encoding="utf-8",
    )

    lines = [
        "# Latest Visualization Output",
        "",
        f"Latest run directory: `{out_dir}`",
        f"Run README: `{readme}`",
        "",
        "## Key Files",
        "",
    ]
    for label, path in output_paths.items():
        if path is not None:
            lines.append(f"- {label}: `{path}`")
    lines.append("")
    latest = DEFAULT_OUT_DIR / "LATEST_OUTPUT.md"
    latest.write_text("\n".join(lines), encoding="utf-8")
    return latest


def parse_link_types(text: str | None) -> set[str]:
    if not text:
        return set()
    return {part.strip() for part in text.replace("，", ",").split(",") if part.strip()}


def main() -> None:
    parser = argparse.ArgumentParser(description="Visualize the processed Beijing road network and x_ijt flows.")
    parser.add_argument("--tile", choices=["osm", "amap", "amap-satellite", "amap-satellite-labels"], default="amap")
    parser.add_argument("--frame-step", type=int, default=12, help="Use every Nth 5-minute bin for video frames.")
    parser.add_argument("--max-frames", type=int, default=24, help="Limit rendered frames. Use 0 for all selected bins.")
    parser.add_argument("--video-fps", type=int, default=6)
    parser.add_argument("--skip-animation", action="store_true", help="Only write the HTML map outputs.")
    parser.add_argument("--dynamic-html", action="store_true", help="Write a time-slider HTML map for all x_ijt bins.")
    parser.add_argument("--metric", choices=sorted(METRIC_CONFIG), default="xijt", help="Traffic metric to draw on flow maps.")
    parser.add_argument("--geometry-source", choices=["model", "osm-matched"], default="model")
    parser.add_argument("--unmatched-geometry", choices=["fallback", "hide"], default="fallback", help="How to draw edges without matched OSM geometry.")
    parser.add_argument("--exclude-link-types", default="", help="Comma-separated link_type_name values to omit, for example 小区引线.")
    parser.add_argument("--osm-geometry-file", type=Path, default=DEFAULT_OSM_GEOMETRY_FILE)
    parser.add_argument("--run-name", default=None, help="Optional name for the per-run output folder.")
    parser.add_argument("--out-dir", type=Path, default=None, help="Explicit output directory. If omitted, a new folder is created under processed_real_network/visualizations/runs/.")
    args = parser.parse_args()

    if args.out_dir is None:
        args.out_dir = make_run_dir(args)
    else:
        args.out_dir.mkdir(parents=True, exist_ok=True)

    args.out_dir.mkdir(parents=True, exist_ok=True)
    max_frames = None if args.max_frames == 0 else args.max_frames

    excluded_link_types = parse_link_types(args.exclude_link_types)
    nodes, links, coords, segments = load_network(
        args.tile,
        args.geometry_source,
        args.osm_geometry_file,
        args.unmatched_geometry,
        excluded_link_types,
    )
    bounds = map_bounds(nodes)
    used_osm_geometry_count = int((links.get("geometry_source_used", pd.Series(dtype=str)) == "osm-matched").sum())

    geometry_suffix = "_osmgeom" if args.geometry_source == "osm-matched" else ""
    prefix = flow_output_prefix(args.metric)
    dynamic_html = None
    if args.dynamic_html:
        dynamic_html = args.out_dir / f"{prefix}_dynamic_leaflet_{args.tile}{geometry_suffix}.html"
        write_dynamic_leaflet_html(
            dynamic_html,
            f"Dynamic {metric_config(args.metric)['title']}",
            args.tile,
            bounds,
            links,
            coords,
            args.metric,
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

    if dynamic_html:
        print(f"dynamic_html={dynamic_html}")
    if frames_dir:
        print(f"frames_dir={frames_dir}")
    if video_path:
        print(f"video={video_path}")

    output_paths = {
        "dynamic metric HTML": dynamic_html,
        "animation frames directory": frames_dir,
        "animation video": video_path,
    }
    readme = write_run_readme(args.out_dir, args, output_paths, len(links), used_osm_geometry_count)
    latest = write_latest_pointer(args.out_dir, readme, output_paths)
    print(f"run_readme={readme}")
    print(f"latest_pointer={latest}")


if __name__ == "__main__":
    main()
