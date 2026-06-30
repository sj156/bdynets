#!/usr/bin/env python3
"""Build an interactive LargeST traffic-flow map with a 5-minute timeline."""

from __future__ import annotations

import json
import math
from pathlib import Path

import h5py
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "data"
PROCESSED = ROOT / "processed"
MAP = ROOT / "map"

YEAR = 2019
DATE = "2019-07-31"
INITIAL_TIME = "16:10"
H5_PATH = DATA / f"ca_his_raw_{YEAR}.h5"
META_PATH = DATA / "ca_meta.csv"
OUTPUT_HTML = MAP / f"largest_la_bay_area_flow_{DATE.replace('-', '_')}.html"
REGION_DATA_JSON = PROCESSED / f"largest_la_bay_area_flow_{DATE.replace('-', '_')}.json"
SUMMARY_CSV = PROCESSED / f"largest_la_bay_area_flow_{DATE.replace('-', '_')}_summary.csv"

REGIONS = {
    "Los Angeles Area": {
        "counties": {"Los Angeles", "Orange", "Riverside", "San Bernardino", "Ventura"},
        "center": [34.03, -117.82],
        "zoom": 9.4,
    },
    "Bay Area": {
        "counties": {
            "Alameda",
            "Contra Costa",
            "Marin",
            "Napa",
            "San Francisco",
            "San Mateo",
            "Santa Clara",
            "Solano",
            "Sonoma",
        },
        "center": [37.68, -122.12],
        "zoom": 9.2,
    },
}


def ensure_dirs() -> None:
    PROCESSED.mkdir(parents=True, exist_ok=True)
    MAP.mkdir(parents=True, exist_ok=True)


def distance_m(lat1: float, lon1: float, lat2: float, lon2: float) -> float:
    radius = 6_371_000.0
    phi1 = math.radians(lat1)
    phi2 = math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dlambda = math.radians(lon2 - lon1)
    a = math.sin(dphi / 2) ** 2 + math.cos(phi1) * math.cos(phi2) * math.sin(dlambda / 2) ** 2
    return radius * 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))


def flow_color(value: float | None, breaks: list[float]) -> str:
    if value is None or not np.isfinite(value):
        return "#94a3b8"
    if value <= breaks[0]:
        return "#16a43a"
    if value <= breaks[1]:
        return "#7ad151"
    if value <= breaks[2]:
        return "#f7ea2a"
    if value <= breaks[3]:
        return "#ffc928"
    if value <= breaks[4]:
        return "#f5a623"
    if value <= breaks[5]:
        return "#f36b2b"
    return "#f01818"


def load_day_matrix() -> tuple[list[str], np.ndarray]:
    target = pd.Timestamp(DATE)
    next_day = target + pd.Timedelta(days=1)
    with h5py.File(H5_PATH, "r") as h5:
        times = pd.to_datetime(h5["t/axis1"][:], unit="ns")
        mask = (times >= target) & (times < next_day)
        if mask.sum() != 288:
            raise ValueError(f"Expected 288 five-minute frames for {DATE}, found {mask.sum()}")
        first = int(np.where(mask)[0][0])
        last = first + int(mask.sum())
        values = h5["t/block0_values"][first:last, :].astype("float32")
        axis0 = [item.decode() for item in h5["t/axis0"][:]]
    return axis0, values


def initial_time_index() -> int:
    hour, minute = [int(part) for part in INITIAL_TIME.split(":")]
    return (hour * 60 + minute) // 5


def build_region_payload(meta: pd.DataFrame, values: np.ndarray, region_name: str, config: dict) -> dict:
    region_meta = meta[meta["County"].isin(config["counties"])].copy()
    region_meta = region_meta[region_meta["ID2"].between(0, values.shape[1] - 1)].copy()
    region_meta = region_meta.sort_values(["Fwy", "ID2"])

    sensor_indices = region_meta["ID2"].to_numpy(dtype=int)
    sensor_day = values[:, sensor_indices]
    finite_values = sensor_day[np.isfinite(sensor_day)]
    breaks = np.nanquantile(finite_values, [0.15, 0.30, 0.45, 0.60, 0.75, 0.90]).round(1).tolist()

    sensors = []
    for row in region_meta.itertuples(index=False):
        idx = int(row.ID2)
        series = values[:, idx]
        sensors.append(
            {
                "id": str(row.ID),
                "idx": idx,
                "lat": round(float(row.Lat), 7),
                "lng": round(float(row.Lng), 7),
                "county": str(row.County),
                "fwy": str(row.Fwy),
                "direction": str(row.Direction),
                "values": [None if not np.isfinite(v) else int(round(float(v))) for v in series],
            }
        )

    links = []
    max_gap_m = 8_500
    for (fwy, direction), group in region_meta.groupby(["Fwy", "Direction"], sort=False):
        ordered = group.sort_values("ID2")
        rows = list(ordered.itertuples(index=False))
        for a, b in zip(rows, rows[1:]):
            gap = distance_m(float(a.Lat), float(a.Lng), float(b.Lat), float(b.Lng))
            if gap > max_gap_m:
                continue
            va = values[:, int(a.ID2)]
            vb = values[:, int(b.ID2)]
            stacked = np.vstack([va, vb])
            valid_counts = np.isfinite(stacked).sum(axis=0)
            link_values = np.full(va.shape, np.nan, dtype="float32")
            np.divide(np.nansum(stacked, axis=0), valid_counts, out=link_values, where=valid_counts > 0)
            links.append(
                {
                    "from": str(a.ID),
                    "to": str(b.ID),
                    "fwy": str(fwy),
                    "direction": str(direction),
                    "county": str(a.County),
                    "coords": [
                        [round(float(a.Lat), 7), round(float(a.Lng), 7)],
                        [round(float(b.Lat), 7), round(float(b.Lng), 7)],
                    ],
                    "values": [None if not np.isfinite(v) else int(round(float(v))) for v in link_values],
                }
            )

    return {
        "name": region_name,
        "center": config["center"],
        "zoom": config["zoom"],
        "breaks": breaks,
        "sensors": sensors,
        "links": links,
        "summary": {
            "sensors": len(sensors),
            "links": len(links),
            "counties": sorted(region_meta["County"].unique().tolist()),
            "freeways": int(region_meta["Fwy"].nunique()),
        },
    }


def build_html(payload: dict) -> str:
    payload_json = json.dumps(payload, separators=(",", ":"))
    initial_index = initial_time_index()
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>LargeST Traffic Flow - {DATE}</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <style>
    html, body {{ height: 100%; margin: 0; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; color: #172033; }}
    .page {{ display: grid; grid-template-columns: minmax(0, 1fr) 360px; height: 100vh; width: 100vw; overflow: hidden; background: #eef2f6; }}
    #map {{ height: 100vh; min-width: 0; }}
    aside {{ background: #fff; border-left: 1px solid #d8dee8; padding: 20px; overflow: auto; box-sizing: border-box; }}
    h1 {{ margin: 0 0 6px; font-size: 25px; line-height: 1.15; color: #18243c; }}
    .sub {{ color: #667085; font-size: 13px; margin-bottom: 18px; }}
    .hint {{ color: #667085; font-size: 12px; line-height: 1.35; margin-top: 8px; }}
    .field {{ margin: 13px 0; }}
    .field span {{ display: block; font-size: 12px; font-weight: 700; color: #475569; margin-bottom: 6px; }}
    select, input[type="range"] {{ width: 100%; box-sizing: border-box; }}
    select {{ height: 34px; border: 1px solid #cfd7e3; border-radius: 4px; padding: 0 8px; background: #fff; }}
    button {{ height: 34px; border: 1px solid #bdc7d6; background: #f8fafc; border-radius: 4px; padding: 0 12px; cursor: pointer; }}
    button:hover {{ background: #eef3f8; }}
    .time-row {{ display: flex; gap: 8px; align-items: center; }}
    #timeText {{ font-weight: 750; font-size: 17px; color: #172033; margin-top: 4px; }}
    .stats {{ display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin: 14px 0 18px; }}
    .stat {{ border: 1px solid #d8dee8; border-radius: 4px; padding: 10px; background: #f8fafc; }}
    .stat b {{ display: block; font-size: 21px; color: #172033; }}
    .stat span {{ font-size: 12px; color: #64748b; }}
    label {{ display: flex; gap: 8px; align-items: center; color: #334155; font-size: 14px; margin: 8px 0; }}
    .legend {{ position: absolute; left: 16px; bottom: 16px; z-index: 700; background: #fff; border: 1px solid rgba(15,23,42,.35); border-radius: 4px; padding: 8px; box-shadow: 0 2px 8px rgba(15,23,42,.18); }}
    .legend-row {{ display: flex; gap: 6px; align-items: center; }}
    .swatch {{ width: 38px; height: 24px; border-radius: 5px; border: 1px solid rgba(15,23,42,.25); }}
    .legend-note {{ font-size: 12px; font-weight: 700; color: #334155; margin-top: 5px; }}
    .popup {{ font-size: 13px; line-height: 1.35; }}
    .leaflet-container {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; }}
    @media (max-width: 900px) {{ .page {{ grid-template-columns: 1fr; grid-template-rows: 68vh 32vh; }} #map {{ height: 68vh; }} aside {{ height: 32vh; border-left: 0; border-top: 1px solid #d8dee8; }} }}
  </style>
</head>
<body>
<div class="page">
  <div id="map"></div>
  <aside>
    <h1>LargeST Traffic Flow</h1>
    <div class="sub">California PeMS sensors · 5-minute flow · {DATE}</div>

    <div class="field">
      <span>Region</span>
      <select id="regionSelect"></select>
    </div>

    <div class="field">
      <span>Time</span>
      <div class="time-row">
        <button id="playBtn">Play</button>
        <button id="resetBtn">Reset</button>
      </div>
      <input id="timeSlider" type="range" min="0" max="287" step="1" value="{initial_index}">
      <div id="timeText"></div>
    </div>

    <div class="stats">
      <div class="stat"><b id="sensorCount"></b><span>sensors</span></div>
      <div class="stat"><b id="linkCount"></b><span>road segments</span></div>
      <div class="stat"><b id="avgFlow"></b><span>avg flow</span></div>
      <div class="stat"><b id="freewayCount"></b><span>freeways</span></div>
    </div>

    <div class="field">
      <span>Layers</span>
      <label><input id="toggleSensors" type="checkbox"> station points</label>
      <label><input id="toggleCasing" type="checkbox"> road casing</label>
      <div class="hint">Direction spacing increases as you zoom in.</div>
    </div>

    <div class="field">
      <span>Counties</span>
      <div id="countyText" class="sub"></div>
    </div>
  </aside>
</div>
<div class="legend">
  <div class="legend-row">
    <div class="swatch" style="background:#16a43a"></div>
    <div class="swatch" style="background:#7ad151"></div>
    <div class="swatch" style="background:#f7ea2a"></div>
    <div class="swatch" style="background:#ffc928"></div>
    <div class="swatch" style="background:#f5a623"></div>
    <div class="swatch" style="background:#f36b2b"></div>
    <div class="swatch" style="background:#f01818"></div>
  </div>
  <div class="legend-note">5-minute flow, low to high</div>
</div>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
const data = {payload_json};
const initialTimeIndex = {initial_index};
const colors = ["#16a43a","#7ad151","#f7ea2a","#ffc928","#f5a623","#f36b2b","#f01818"];
const times = Array.from({{length: 288}}, (_, i) => {{
  const h = String(Math.floor(i * 5 / 60)).padStart(2, "0");
  const m = String((i * 5) % 60).padStart(2, "0");
  return `${{h}}:${{m}}`;
}});

const map = L.map("map", {{ preferCanvas: true, zoomSnap: 0.25, zoomDelta: 0.25 }});
L.tileLayer("https://{{s}}.tile.openstreetmap.org/{{z}}/{{x}}/{{y}}.png", {{
  maxZoom: 19,
  attribution: "&copy; OpenStreetMap contributors"
}}).addTo(map);

const regionSelect = document.getElementById("regionSelect");
Object.keys(data.regions).forEach(name => {{
  const opt = document.createElement("option");
  opt.value = name;
  opt.textContent = name;
  regionSelect.appendChild(opt);
}});

const casingLayer = L.layerGroup();
const flowLayer = L.layerGroup().addTo(map);
const sensorLayer = L.layerGroup();
let activeRegion = regionSelect.value;
let activeTime = initialTimeIndex;
let playing = false;
let timer = null;

function colorFor(value, breaks) {{
  if (value == null || Number.isNaN(value)) return "#94a3b8";
  let idx = 0;
  while (idx < breaks.length && value > breaks[idx]) idx++;
  return colors[idx];
}}

function currentRegion() {{ return data.regions[activeRegion]; }}

function styleForZoom() {{
  const z = map.getZoom();
  if (z < 8.8) return {{ offset: 0.5, flowWeight: 2.0, casingWeight: 0, flowOpacity: 0.82 }};
  if (z < 9.6) return {{ offset: 2.6, flowWeight: 2.8, casingWeight: 0, flowOpacity: 0.86 }};
  if (z < 10.6) return {{ offset: 4.8, flowWeight: 3.6, casingWeight: 4.8, flowOpacity: 0.9 }};
  if (z < 11.6) return {{ offset: 6.4, flowWeight: 4.4, casingWeight: 5.8, flowOpacity: 0.93 }};
  return {{ offset: 8.0, flowWeight: 5.2, casingWeight: 6.8, flowOpacity: 0.94 }};
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

function drawCoordsFor(link) {{
  if (!link.coords || link.coords.length < 2) return link.coords;
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
  const side = directionSide(link.direction);
  const offset = styleForZoom().offset * side;
  return link.coords.map(coord => {{
    const point = map.latLngToLayerPoint(coord);
    return map.layerPointToLatLng(L.point(point.x + nx * offset, point.y + ny * offset));
  }});
}}

function drawRegion(resetView = true) {{
  casingLayer.clearLayers();
  flowLayer.clearLayers();
  sensorLayer.clearLayers();
  const region = currentRegion();
  if (resetView) map.setView(region.center, region.zoom);
  const style = styleForZoom();

  region.links.forEach(link => {{
    const drawCoords = drawCoordsFor(link);
    if (style.casingWeight > 0) {{
      L.polyline(drawCoords, {{ color: "#1f3329", weight: style.casingWeight, opacity: 0.34, lineCap: "round", lineJoin: "round", interactive: false }}).addTo(casingLayer);
    }}
    const value = link.values[activeTime];
    const line = L.polyline(drawCoords, {{ color: colorFor(value, region.breaks), weight: style.flowWeight, opacity: style.flowOpacity, lineCap: "round", lineJoin: "round" }}).addTo(flowLayer);
    line._flowValues = link.values;
    line._link = link;
    line.bindPopup(() => `<div class="popup"><b>${{link.fwy}} ${{link.direction}}</b><br>${{link.from}} → ${{link.to}}<br>Flow: ${{link.values[activeTime] ?? "NA"}}</div>`);
  }});

  region.sensors.forEach(sensor => {{
    const value = sensor.values[activeTime];
    const marker = L.circleMarker([sensor.lat, sensor.lng], {{
      radius: 2.2,
      color: "#fff",
      weight: 1,
      fillColor: colorFor(value, region.breaks),
      fillOpacity: 0.9
    }}).bindPopup(() => `<div class="popup"><b>Station ${{sensor.id}}</b><br>${{sensor.fwy}}<br>${{sensor.county}}<br>Flow: ${{sensor.values[activeTime] ?? "NA"}}</div>`);
    marker._flowValues = sensor.values;
    sensorLayer.addLayer(marker);
  }});

  document.getElementById("sensorCount").textContent = region.summary.sensors.toLocaleString();
  document.getElementById("linkCount").textContent = region.summary.links.toLocaleString();
  document.getElementById("freewayCount").textContent = region.summary.freeways.toLocaleString();
  document.getElementById("countyText").textContent = region.summary.counties.join(", ");
  updateTime();
}}

function updateTime() {{
  activeTime = Number(document.getElementById("timeSlider").value);
  const region = currentRegion();
  let total = 0;
  let count = 0;
  flowLayer.eachLayer(line => {{
    const value = line._flowValues[activeTime];
    line.setStyle({{ color: colorFor(value, region.breaks) }});
    if (value != null && Number.isFinite(value)) {{ total += value; count += 1; }}
  }});
  sensorLayer.eachLayer(marker => {{
    const value = marker._flowValues[activeTime];
    marker.setStyle({{ fillColor: colorFor(value, region.breaks) }});
  }});
  document.getElementById("timeText").textContent = `${{data.date}} ${{times[activeTime]}}`;
  document.getElementById("avgFlow").textContent = count ? Math.round(total / count).toLocaleString() : "NA";
}}

regionSelect.addEventListener("change", () => {{ activeRegion = regionSelect.value; drawRegion(); }});
map.on("zoomend", () => drawRegion(false));
document.getElementById("timeSlider").addEventListener("input", updateTime);
document.getElementById("toggleSensors").addEventListener("change", e => e.target.checked ? sensorLayer.addTo(map) : sensorLayer.remove());
document.getElementById("toggleCasing").addEventListener("change", e => e.target.checked ? casingLayer.addTo(map) : casingLayer.remove());
document.getElementById("resetBtn").addEventListener("click", () => {{ document.getElementById("timeSlider").value = initialTimeIndex; updateTime(); }});
document.getElementById("playBtn").addEventListener("click", () => {{
  playing = !playing;
  document.getElementById("playBtn").textContent = playing ? "Pause" : "Play";
  if (playing) {{
    timer = setInterval(() => {{
      const slider = document.getElementById("timeSlider");
      slider.value = (Number(slider.value) + 1) % 288;
      updateTime();
    }}, 350);
  }} else {{
    clearInterval(timer);
  }}
}});

drawRegion();
</script>
</body>
</html>
"""


def main() -> int:
    ensure_dirs()
    meta = pd.read_csv(META_PATH)
    axis0, values = load_day_matrix()
    id_to_pos = {int(sensor_id): i for i, sensor_id in enumerate(axis0)}
    meta = meta[meta["ID"].isin(id_to_pos)].copy()
    meta["ID2"] = meta["ID"].map(id_to_pos).astype(int)

    payload = {"date": DATE, "source": "LargeST CA 2019", "regions": {}}
    summary_rows = []
    for region_name, config in REGIONS.items():
      region_payload = build_region_payload(meta, values, region_name, config)
      payload["regions"][region_name] = region_payload
      summary_rows.append({
          "region": region_name,
          "sensors": region_payload["summary"]["sensors"],
          "links": region_payload["summary"]["links"],
          "freeways": region_payload["summary"]["freeways"],
          "counties": "; ".join(region_payload["summary"]["counties"]),
      })

    REGION_DATA_JSON.write_text(json.dumps(payload), encoding="utf-8")
    pd.DataFrame(summary_rows).to_csv(SUMMARY_CSV, index=False)
    OUTPUT_HTML.write_text(build_html(payload), encoding="utf-8")
    print(f"Wrote {OUTPUT_HTML}")
    print(f"Wrote {REGION_DATA_JSON}")
    print(f"Wrote {SUMMARY_CSV}")
    for row in summary_rows:
        print(f"{row['region']}: sensors={row['sensors']:,}, links={row['links']:,}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
