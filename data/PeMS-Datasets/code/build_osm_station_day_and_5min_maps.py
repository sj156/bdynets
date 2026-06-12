#!/usr/bin/env python3
"""Build OSM/Leaflet maps for Station Day and Station 5-Minute outputs."""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd

from build_osm_station_hour_map import build_road_segments


ROOT = Path(__file__).resolve().parents[1]
PROCESSED = ROOT / "data" / "processed"
MAPS = ROOT / "outputs" / "maps"


def safe_float(value):
    if pd.isna(value) or value == "":
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def load_metadata() -> pd.DataFrame:
    metadata = pd.read_csv(PROCESSED / "station_metadata_2020_latest_by_station.csv")
    metadata["station"] = metadata["station"].astype(str)
    return metadata.dropna(subset=["latitude", "longitude"]).copy()


def base_record(meta: pd.Series) -> dict:
    return {
        "station": str(meta["station"]),
        "lat": float(meta["latitude"]),
        "lon": float(meta["longitude"]),
        "freeway": str(meta.get("freeway", "")).strip(),
        "direction": str(meta.get("direction", "")).strip(),
        "type": str(meta.get("type", "")).strip() or "Unknown",
        "lanes": None if pd.isna(meta.get("lanes")) else int(meta["lanes"]),
        "name": "" if pd.isna(meta.get("name")) else str(meta["name"]).strip(),
        "metadata_date": str(meta.get("metadata_date", "")),
    }


def build_station_day_records() -> tuple[list[dict], list[str]]:
    metadata = load_metadata()
    daily = pd.read_csv(PROCESSED / "station_day_2020_daily_flow.csv")
    daily["station"] = daily["station"].astype(str)
    daily["total_flow"] = pd.to_numeric(daily["total_flow"], errors="coerce")
    daily["samples"] = pd.to_numeric(daily["samples"], errors="coerce").fillna(0)
    keys = sorted(daily["date"].unique())

    by_station = {station: df for station, df in daily.groupby("station")}
    records: list[dict] = []
    for _, meta in metadata.iterrows():
        record = base_record(meta)
        station_data = by_station.get(record["station"])
        flows: dict[str, float | None] = {}
        weighted_num = 0.0
        weighted_den = 0.0
        if station_data is not None:
            for _, row in station_data.iterrows():
                flow = safe_float(row["total_flow"])
                flows[row["date"]] = None if flow is None else round(flow, 3)
                samples = safe_float(row["samples"]) or 0
                if flow is not None and samples > 0:
                    weighted_num += flow * samples
                    weighted_den += samples
        record["annual_flow"] = None if weighted_den == 0 else round(weighted_num / weighted_den, 3)
        record["flows"] = flows
        records.append(record)
    return records, keys


def build_station_5min_records() -> tuple[list[dict], list[str]]:
    metadata = load_metadata()
    five = pd.read_csv(PROCESSED / "station_5min_2020_10_05_10_11_time_of_day_summary.csv")
    five["station"] = five["station"].astype(str)
    five["time"] = five["time"].astype(str)
    five["total_flow_weighted_by_samples"] = pd.to_numeric(
        five["total_flow_weighted_by_samples"], errors="coerce"
    )
    five["samples_sum"] = pd.to_numeric(five["samples_sum"], errors="coerce").fillna(0)
    keys = sorted(five["time"].unique())

    by_station = {station: df for station, df in five.groupby("station")}
    records: list[dict] = []
    for _, meta in metadata.iterrows():
        record = base_record(meta)
        station_data = by_station.get(record["station"])
        flows: dict[str, float | None] = {}
        weighted_num = 0.0
        weighted_den = 0.0
        if station_data is not None:
            for _, row in station_data.iterrows():
                flow = safe_float(row["total_flow_weighted_by_samples"])
                flows[row["time"]] = None if flow is None else round(flow, 3)
                samples = safe_float(row["samples_sum"]) or 0
                if flow is not None and samples > 0:
                    weighted_num += flow * samples
                    weighted_den += samples
        record["annual_flow"] = None if weighted_den == 0 else round(weighted_num / weighted_den, 3)
        record["flows"] = flows
        records.append(record)
    return records, keys


def options_script(control_kind: str) -> str:
    if control_kind == "range":
        return """
    const valueRange = document.getElementById("valueRange");
    const valueLabel = document.getElementById("valueLabel");
    valueRange.max = valueKeys.length - 1;
    const defaultIndex = Math.max(0, valueKeys.indexOf("08:00"));
    valueRange.value = defaultIndex;
    function selectedKey() {
      const key = valueKeys[Number(valueRange.value)];
      valueLabel.textContent = key;
      return key;
    }
"""
    return """
    const valueSelect = document.getElementById("valueSelect");
    valueKeys.forEach(key => {
      const option = document.createElement("option");
      option.value = key;
      option.textContent = key;
      valueSelect.appendChild(option);
    });
    function selectedKey() {
      return valueSelect.value;
    }
"""


def control_html(control_kind: str, control_label: str) -> str:
    if control_kind == "range":
        return f"""
      <div class="field">
        <label for="valueRange">{control_label}</label>
        <div class="range-row">
          <input id="valueRange" type="range" min="0" step="1">
          <output id="valueLabel"></output>
        </div>
      </div>
"""
    return f"""
      <div class="field">
        <label for="valueSelect">{control_label}</label>
        <select id="valueSelect">
          <option value="annual">全年加权均值</option>
        </select>
      </div>
"""


def build_html(
    *,
    title: str,
    subtitle: str,
    records: list[dict],
    road_segments: list[dict],
    value_keys: list[str],
    control_label: str,
    control_kind: str,
    default_label: str,
) -> str:
    center_lat = sum(item["lat"] for item in records) / len(records)
    center_lon = sum(item["lon"] for item in records) / len(records)
    data_json = json.dumps(records, ensure_ascii=False, separators=(",", ":"))
    roads_json = json.dumps(road_segments, ensure_ascii=False, separators=(",", ":"))
    keys_json = json.dumps(value_keys, ensure_ascii=False, separators=(",", ":"))
    html = f"""<!doctype html>
<html lang="zh-CN">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{title}</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <style>
    .leaflet-container {{ overflow: hidden; background: #ddd; font: 12px/1.5 Arial, sans-serif; }}
    .leaflet-pane,.leaflet-tile,.leaflet-marker-icon,.leaflet-marker-shadow,.leaflet-tile-container,.leaflet-pane>svg,.leaflet-pane>canvas,.leaflet-image-layer,.leaflet-layer {{ position:absolute; left:0; top:0; }}
    .leaflet-tile,.leaflet-marker-icon,.leaflet-marker-shadow {{ user-select:none; -webkit-user-drag:none; }}
    .leaflet-pane {{ z-index:400; }} .leaflet-tile-pane {{ z-index:200; }} .leaflet-overlay-pane {{ z-index:400; }} .leaflet-marker-pane {{ z-index:600; }} .leaflet-popup-pane {{ z-index:700; }}
    .leaflet-control {{ position:relative; z-index:800; pointer-events:auto; }} .leaflet-top,.leaflet-bottom {{ position:absolute; z-index:1000; pointer-events:none; }}
    .leaflet-top {{ top:0; }} .leaflet-bottom {{ bottom:0; }} .leaflet-left {{ left:0; }} .leaflet-right {{ right:0; }} .leaflet-control {{ float:left; clear:both; }}
    .leaflet-right .leaflet-control {{ float:right; }} .leaflet-top .leaflet-control {{ margin-top:10px; }} .leaflet-left .leaflet-control {{ margin-left:10px; }} .leaflet-right .leaflet-control {{ margin-right:10px; }} .leaflet-bottom .leaflet-control {{ margin-bottom:10px; }}
    .leaflet-control-zoom a {{ display:block; width:26px; height:26px; line-height:26px; text-align:center; text-decoration:none; color:#111827; background:#fff; border-bottom:1px solid #ccc; }}
    .leaflet-control-attribution {{ background:rgba(255,255,255,.82); padding:0 5px; }}
    .leaflet-popup {{ position:absolute; text-align:center; margin-bottom:20px; }} .leaflet-popup-content-wrapper {{ padding:1px; text-align:left; border-radius:6px; background:white; box-shadow:0 3px 14px rgba(0,0,0,.25); }} .leaflet-popup-content {{ margin:10px 12px; line-height:1.35; }}
    .leaflet-popup-tip-container {{ width:40px; height:20px; position:absolute; left:50%; margin-left:-20px; overflow:hidden; pointer-events:none; }} .leaflet-popup-tip {{ width:17px; height:17px; margin:-10px auto 0; transform:rotate(45deg); background:white; box-shadow:0 3px 14px rgba(0,0,0,.25); }}
    .leaflet-container a.leaflet-popup-close-button {{ position:absolute; top:0; right:0; width:24px; height:24px; font:16px/24px Tahoma,Verdana,sans-serif; color:#757575; text-align:center; text-decoration:none; }}
    html, body {{ height:100%; margin:0; font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif; color:#172033; background:#f4f6f8; }}
    .app {{ display:grid; grid-template-columns:340px minmax(0,1fr); height:100%; }}
    aside {{ overflow:auto; border-right:1px solid #d9dee7; background:white; padding:18px; }}
    #map {{ min-height:100%; }}
    h1 {{ font-size:20px; line-height:1.2; margin:0 0 8px; }}
    .sub {{ font-size:13px; line-height:1.45; color:#5e697c; margin-bottom:18px; }}
    .field {{ margin-bottom:14px; }}
    label {{ display:block; font-size:12px; font-weight:700; color:#354052; margin-bottom:6px; }}
    select,input {{ width:100%; box-sizing:border-box; border:1px solid #c9d1dd; border-radius:6px; padding:8px 9px; font-size:14px; background:#fff; color:#172033; }}
    .range-row {{ display:grid; grid-template-columns:1fr 58px; gap:10px; align-items:center; }}
    output {{ font-weight:800; text-align:right; }}
    .checks {{ display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:7px 10px; }}
    .check {{ display:flex; align-items:center; gap:6px; font-size:13px; color:#2d3748; }}
    .check input {{ width:auto; }}
    .stats {{ display:grid; grid-template-columns:1fr 1fr; gap:8px; margin:16px 0; }}
    .stat {{ border:1px solid #d9dee7; border-radius:6px; padding:10px; background:#fafbfc; }}
    .stat .value {{ font-size:20px; font-weight:800; }}
    .stat .name {{ font-size:11px; color:#647083; margin-top:2px; }}
    .legend {{ font-size:12px; line-height:1.5; color:#4b5568; border-top:1px solid #e4e8ef; padding-top:14px; margin-top:14px; }}
    .swatch {{ display:inline-block; width:10px; height:10px; border-radius:50%; margin-right:6px; border:1px solid rgba(0,0,0,.18); }}
    .popup-title {{ font-weight:800; font-size:14px; margin-bottom:4px; }}
    .popup-row {{ font-size:12px; margin:2px 0; }}
    @media (max-width:760px) {{ .app {{ grid-template-columns:1fr; grid-template-rows:auto 1fr; }} aside {{ max-height:42vh; border-right:none; border-bottom:1px solid #d9dee7; }} #map {{ min-height:58vh; }} }}
  </style>
</head>
<body>
  <div class="app">
    <aside>
      <h1>{title}</h1>
      <div class="sub">{subtitle}</div>
      {control_html(control_kind, control_label)}
      <div class="field">
        <label><input type="checkbox" id="showRoads" checked> 显示匹配的 OSM 路段颜色</label>
      </div>
      <div class="field"><label>Station 类型</label><div id="typeFilters" class="checks"></div></div>
      <div class="field"><label for="freeway">Freeway 过滤</label><input id="freeway" placeholder="例如 5、15、805，留空为全部"></div>
      <div class="field"><label for="stationSearch">Station ID 搜索</label><input id="stationSearch" placeholder="输入 station ID"></div>
      <div class="stats">
        <div class="stat"><div id="visibleCount" class="value">0</div><div class="name">当前显示 station</div></div>
        <div class="stat"><div id="meanFlow" class="value">-</div><div class="name">显示点平均流量</div></div>
        <div class="stat"><div id="roadCount" class="value">0</div><div class="name">当前显示路段</div></div>
      </div>
      <div id="legend" class="legend"></div>
    </aside>
    <main id="map"></main>
  </div>
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <script>
    const stationData = {data_json};
    const roadSegments = {roads_json};
    const valueKeys = {keys_json};
    const defaultLabel = {json.dumps(default_label, ensure_ascii=False)};
    const map = L.map("map", {{ preferCanvas: true }}).setView([{center_lat:.6f}, {center_lon:.6f}], 9);
    L.tileLayer("https://{{s}}.tile.openstreetmap.org/{{z}}/{{x}}/{{y}}.png", {{
      maxZoom: 19,
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }}).addTo(map);

    const typeColors = {{ ML:"#2F6DB3", HV:"#8C5FBF", FR:"#C76D3B", OR:"#4F9360", FF:"#D1A12C", CH:"#8A8F98", CD:"#6B7280", Unknown:"#6B7280" }};
    const flowColors = ["#E9F2FB","#BFD9EF","#85B7DD","#4B8CC7","#1F5B99","#0B2F5B"];
    const markers = [], roadLines = [];
    const types = Array.from(new Set(stationData.map(d => d.type))).sort();
    const typeFilters = document.getElementById("typeFilters");
    types.forEach(type => {{
      const label = document.createElement("label");
      label.className = "check";
      label.innerHTML = '<input type="checkbox" value="' + type + '" checked> <span>' + type + '</span>';
      typeFilters.appendChild(label);
    }});
    {options_script(control_kind)}
    function allowedTypes() {{ return new Set(Array.from(typeFilters.querySelectorAll("input:checked")).map(i => i.value)); }}
    function valueFor(d) {{
      const key = selectedKey();
      if (key === "annual") return d.annual_flow;
      return d.flows[key] ?? null;
    }}
    function breaks(values) {{
      const sorted = values.filter(v => Number.isFinite(v)).sort((a,b)=>a-b);
      if (!sorted.length) return [];
      return [0.2,0.4,0.6,0.8,0.95].map(q => sorted[Math.min(sorted.length-1, Math.floor(q*sorted.length))]);
    }}
    function colorFor(value, br) {{
      if (!Number.isFinite(value)) return "#9CA3AF";
      let i=0; while (i<br.length && value>br[i]) i++;
      return flowColors[i];
    }}
    function radiusFor(value) {{ return Number.isFinite(value) ? Math.max(4, Math.min(14, 3 + Math.sqrt(value)/18)) : 4; }}
    function popupStation(d, v) {{
      const flow = Number.isFinite(v) ? Math.round(v).toLocaleString() : "No data";
      return '<div class="popup-title">Station ' + d.station + '</div>' +
        '<div class="popup-row"><b>Name:</b> ' + (d.name || "-") + '</div>' +
        '<div class="popup-row"><b>Freeway:</b> ' + d.freeway + ' ' + d.direction + '</div>' +
        '<div class="popup-row"><b>Type:</b> ' + d.type + '</div>' +
        '<div class="popup-row"><b>Flow:</b> ' + flow + '</div>';
    }}
    function popupRoad(d, v) {{
      const flow = Number.isFinite(v) ? Math.round(v).toLocaleString() : "No data";
      return '<div class="popup-title">Matched OSM road</div>' +
        '<div class="popup-row"><b>Station:</b> ' + d.station + '</div>' +
        '<div class="popup-row"><b>OSM:</b> ' + (d.osm_ref || "-") + ' ' + (d.osm_name || "") + '</div>' +
        '<div class="popup-row"><b>Match distance:</b> ' + d.match_distance_m + ' m</div>' +
        '<div class="popup-row"><b>Flow:</b> ' + flow + '</div>';
    }}
    function renderLegend(br) {{
      const rows = flowColors.map((color, i) => {{
        let label = i === 0 ? "≤ " + Math.round(br[0] || 0) : (i === flowColors.length - 1 ? "> " + Math.round(br[br.length-1] || 0) : Math.round(br[i-1] || 0) + " - " + Math.round(br[i] || 0));
        return '<span class="swatch" style="background:' + color + '"></span>' + label;
      }});
      document.getElementById("legend").innerHTML = "<b>" + defaultLabel + "</b><br>" + rows.join("<br>") + "<br><br>线段和圆点颜色越深表示流量越高。";
    }}
    function update() {{
      markers.forEach(m => map.removeLayer(m)); markers.length = 0;
      roadLines.forEach(l => map.removeLayer(l)); roadLines.length = 0;
      const typesWanted = allowedTypes();
      const freeway = document.getElementById("freeway").value.trim();
      const search = document.getElementById("stationSearch").value.trim();
      const filtered = stationData.filter(d => typesWanted.has(d.type) && (!freeway || d.freeway === freeway) && (!search || d.station.includes(search)));
      const br = breaks(filtered.map(valueFor));
      const visible = new Set(filtered.map(d => d.station));
      let total=0, count=0;
      if (document.getElementById("showRoads").checked) {{
        roadSegments.filter(d => visible.has(d.station)).forEach(d => {{
          const v = valueFor(d), color = colorFor(v, br);
          const line = L.polyline(d.coords, {{ color, weight: Math.max(4, Math.min(10, radiusFor(v)*0.8)), opacity:0.88, lineCap:"round", lineJoin:"round" }}).bindPopup(popupRoad(d, v));
          line.addTo(map); roadLines.push(line);
        }});
      }}
      filtered.forEach(d => {{
        const v = valueFor(d);
        if (Number.isFinite(v)) {{ total += v; count++; }}
        const marker = L.circleMarker([d.lat, d.lon], {{ radius: radiusFor(v), color:"#1f2937", weight:0.6, fillColor:colorFor(v, br), fillOpacity:0.78 }}).bindPopup(popupStation(d, v));
        marker.addTo(map); markers.push(marker);
      }});
      document.getElementById("visibleCount").textContent = filtered.length.toLocaleString();
      document.getElementById("meanFlow").textContent = count ? Math.round(total/count).toLocaleString() : "-";
      document.getElementById("roadCount").textContent = roadLines.length.toLocaleString();
      renderLegend(br);
    }}
    ["showRoads","freeway","stationSearch"].forEach(id => {{
      document.getElementById(id).addEventListener("input", update);
      document.getElementById(id).addEventListener("change", update);
    }});
    typeFilters.addEventListener("change", update);
    if (document.getElementById("valueSelect")) document.getElementById("valueSelect").addEventListener("change", update);
    if (document.getElementById("valueRange")) document.getElementById("valueRange").addEventListener("input", update);
    update();
  </script>
</body>
</html>
"""
    return html


def write_map(path: Path, **kwargs) -> None:
    MAPS.mkdir(parents=True, exist_ok=True)
    path.write_text(build_html(**kwargs), encoding="utf-8")
    print(f"Wrote {path}")


def main() -> int:
    day_records, day_keys = build_station_day_records()
    day_roads = build_road_segments(day_records)
    write_map(
        MAPS / "station_day_2020_osm_map.html",
        title="PeMS D11 Station Day 2020",
        subtitle="OSM 道路颜色表示指定日期的 station daily total flow；圆点保留检测站位置。",
        records=day_records,
        road_segments=day_roads,
        value_keys=day_keys,
        control_label="日期",
        control_kind="select",
        default_label="Daily total flow",
    )
    print(f"Station Day road segments: {len(day_roads)}")

    five_records, five_keys = build_station_5min_records()
    five_roads = build_road_segments(five_records)
    write_map(
        MAPS / "station_5min_2020_10_week_osm_map.html",
        title="PeMS D11 Station 5-Minute Typical Day",
        subtitle="2020-10-05 到 2020-10-11 一周数据按 5 分钟时刻汇总；OSM 路段颜色表示典型 5 分钟流量。",
        records=five_records,
        road_segments=five_roads,
        value_keys=five_keys,
        control_label="5 分钟时刻",
        control_kind="range",
        default_label="Typical 5-minute flow",
    )
    print(f"Station 5-Minute road segments: {len(five_roads)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
