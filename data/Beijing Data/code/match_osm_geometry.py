from __future__ import annotations

import argparse
import json
import math
import time
import urllib.parse
import urllib.request
import urllib.error
from collections import defaultdict
from pathlib import Path
from typing import Any

import pandas as pd


BASE_DIR = Path(__file__).resolve().parent
PROCESSED_DIR = BASE_DIR / "processed_real_network"
DEFAULT_OSM_JSON = PROCESSED_DIR / "osm_highway_ways.json"
DEFAULT_OUTPUT = PROCESSED_DIR / "osm_matched_edge_geometry.csv"
DEFAULT_ENDPOINTS = [
    "https://overpass-api.de/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
    "https://overpass.openstreetmap.ru/api/interpreter",
]

EXCLUDED_HIGHWAYS = {
    "bridleway",
    "bus_stop",
    "construction",
    "corridor",
    "cycleway",
    "elevator",
    "footway",
    "path",
    "pedestrian",
    "platform",
    "proposed",
    "raceway",
    "steps",
}


def project_lonlat(lon: float, lat: float, center_lon: float, center_lat: float) -> tuple[float, float]:
    radius = 6_371_008.8
    x = math.radians(lon - center_lon) * radius * math.cos(math.radians(center_lat))
    y = math.radians(lat - center_lat) * radius
    return x, y


def distance(a: tuple[float, float], b: tuple[float, float]) -> float:
    return math.hypot(a[0] - b[0], a[1] - b[1])


def point_segment_projection(
    p: tuple[float, float],
    a: tuple[float, float],
    b: tuple[float, float],
) -> tuple[float, float, tuple[float, float]]:
    ax, ay = a
    bx, by = b
    px, py = p
    dx = bx - ax
    dy = by - ay
    denom = dx * dx + dy * dy
    if denom <= 1e-12:
        return distance(p, a), 0.0, a
    t = ((px - ax) * dx + (py - ay) * dy) / denom
    t = max(0.0, min(1.0, t))
    proj = (ax + t * dx, ay + t * dy)
    return distance(p, proj), t, proj


def fetch_osm_highways(
    south: float,
    west: float,
    north: float,
    east: float,
    out_json: Path,
    endpoint: str,
    timeout_seconds: int,
) -> None:
    query = f"""
[out:json][timeout:{timeout_seconds}];
(
  way["highway"]({south:.7f},{west:.7f},{north:.7f},{east:.7f});
);
out tags geom;
"""
    data = urllib.parse.urlencode({"data": query}).encode("utf-8")
    req = urllib.request.Request(endpoint, data=data, headers={"User-Agent": "codex-road-network-visualizer"})
    with urllib.request.urlopen(req, timeout=timeout_seconds + 30) as response:
        payload = response.read()
    out_json.write_bytes(payload)


def fetch_osm_highways_with_fallback(
    south: float,
    west: float,
    north: float,
    east: float,
    out_json: Path,
    endpoints: list[str],
    timeout_seconds: int,
) -> str:
    errors: list[str] = []
    for endpoint in endpoints:
        try:
            print(f"Trying Overpass endpoint: {endpoint}")
            fetch_osm_highways(south, west, north, east, out_json, endpoint, timeout_seconds)
            return endpoint
        except (urllib.error.URLError, TimeoutError, OSError) as exc:
            errors.append(f"{endpoint}: {exc}")
            print(f"Endpoint failed: {endpoint} ({exc})")
    raise RuntimeError("All Overpass endpoints failed:\n" + "\n".join(errors))


def load_osm_ways(osm_json: Path, center_lon: float, center_lat: float) -> list[dict[str, Any]]:
    raw = json.loads(osm_json.read_text(encoding="utf-8"))
    ways: list[dict[str, Any]] = []

    for element in raw.get("elements", []):
        if element.get("type") != "way":
            continue
        tags = element.get("tags") or {}
        highway = tags.get("highway")
        if not highway or highway in EXCLUDED_HIGHWAYS:
            continue
        geometry = element.get("geometry") or []
        if len(geometry) < 2:
            continue

        lonlat = [(float(p["lon"]), float(p["lat"])) for p in geometry]
        xy = [project_lonlat(lon, lat, center_lon, center_lat) for lon, lat in lonlat]
        chain = [0.0]
        for a, b in zip(xy, xy[1:]):
            chain.append(chain[-1] + distance(a, b))

        ways.append(
            {
                "id": int(element["id"]),
                "tags": tags,
                "highway": highway,
                "name": tags.get("name", ""),
                "oneway": tags.get("oneway", ""),
                "lonlat": lonlat,
                "xy": xy,
                "chain": chain,
            }
        )
    return ways


def build_segment_index(ways: list[dict[str, Any]], cell_size_m: float) -> dict[tuple[int, int], list[tuple[int, int]]]:
    grid: dict[tuple[int, int], list[tuple[int, int]]] = defaultdict(list)
    for way_idx, way in enumerate(ways):
        xy = way["xy"]
        for seg_idx, (a, b) in enumerate(zip(xy, xy[1:])):
            minx = min(a[0], b[0])
            maxx = max(a[0], b[0])
            miny = min(a[1], b[1])
            maxy = max(a[1], b[1])
            ix0 = math.floor(minx / cell_size_m)
            ix1 = math.floor(maxx / cell_size_m)
            iy0 = math.floor(miny / cell_size_m)
            iy1 = math.floor(maxy / cell_size_m)
            for ix in range(ix0, ix1 + 1):
                for iy in range(iy0, iy1 + 1):
                    grid[(ix, iy)].append((way_idx, seg_idx))
    return grid


def candidate_segments(
    points: list[tuple[float, float]],
    grid: dict[tuple[int, int], list[tuple[int, int]]],
    cell_size_m: float,
    radius_cells: int,
) -> set[tuple[int, int]]:
    candidates: set[tuple[int, int]] = set()
    for x, y in points:
        ix = math.floor(x / cell_size_m)
        iy = math.floor(y / cell_size_m)
        for dx in range(-radius_cells, radius_cells + 1):
            for dy in range(-radius_cells, radius_cells + 1):
                candidates.update(grid.get((ix + dx, iy + dy), []))
    return candidates


def nearest_on_way(
    point: tuple[float, float],
    way: dict[str, Any],
    segment_indices: set[int] | None = None,
) -> dict[str, Any]:
    xy = way["xy"]
    chain = way["chain"]
    best: dict[str, Any] | None = None
    indices = segment_indices if segment_indices else set(range(len(xy) - 1))
    for seg_idx in indices:
        if seg_idx < 0 or seg_idx >= len(xy) - 1:
            continue
        dist_m, t, proj = point_segment_projection(point, xy[seg_idx], xy[seg_idx + 1])
        seg_len = max(1e-9, chain[seg_idx + 1] - chain[seg_idx])
        pos = chain[seg_idx] + t * seg_len
        if best is None or dist_m < best["dist_m"]:
            best = {"dist_m": dist_m, "seg_idx": seg_idx, "t": t, "pos": pos, "proj_xy": proj}
    if best is None:
        return {"dist_m": float("inf"), "seg_idx": 0, "t": 0.0, "pos": 0.0, "proj_xy": xy[0]}
    return best


def interpolate_lonlat(way: dict[str, Any], seg_idx: int, t: float) -> tuple[float, float]:
    lonlat = way["lonlat"]
    lon1, lat1 = lonlat[seg_idx]
    lon2, lat2 = lonlat[min(seg_idx + 1, len(lonlat) - 1)]
    return lon1 + (lon2 - lon1) * t, lat1 + (lat2 - lat1) * t


def extract_subline(
    way: dict[str, Any],
    start_match: dict[str, Any],
    end_match: dict[str, Any],
    fallback_start: tuple[float, float],
    fallback_end: tuple[float, float],
) -> list[list[float]]:
    start_pos = float(start_match["pos"])
    end_pos = float(end_match["pos"])
    reverse = start_pos > end_pos
    if reverse:
        start_match, end_match = end_match, start_match

    start_pt = interpolate_lonlat(way, int(start_match["seg_idx"]), float(start_match["t"]))
    end_pt = interpolate_lonlat(way, int(end_match["seg_idx"]), float(end_match["t"]))
    start_seg = int(start_match["seg_idx"])
    end_seg = int(end_match["seg_idx"])

    coords: list[tuple[float, float]] = [start_pt]
    for vertex_idx in range(start_seg + 1, end_seg + 1):
        coords.append(way["lonlat"][vertex_idx])
    coords.append(end_pt)

    deduped: list[tuple[float, float]] = []
    for lon, lat in coords:
        if not deduped or abs(deduped[-1][0] - lon) > 1e-9 or abs(deduped[-1][1] - lat) > 1e-9:
            deduped.append((lon, lat))

    if reverse:
        deduped.reverse()

    if len(deduped) < 2:
        deduped = [fallback_start, fallback_end]

    return [[round(lon, 7), round(lat, 7)] for lon, lat in deduped]


def match_edges(
    nodes: pd.DataFrame,
    links: pd.DataFrame,
    ways: list[dict[str, Any]],
    max_endpoint_distance_m: float,
    max_midpoint_distance_m: float,
    cell_size_m: float,
) -> pd.DataFrame:
    center_lon = float(nodes["x_84"].mean())
    center_lat = float(nodes["y_84"].mean())
    coords_lonlat = {
        int(row.node_id): (float(row.x_84), float(row.y_84))
        for row in nodes[["node_id", "x_84", "y_84"]].dropna().itertuples(index=False)
    }
    coords_xy = {
        node_id: project_lonlat(lon, lat, center_lon, center_lat)
        for node_id, (lon, lat) in coords_lonlat.items()
    }

    grid = build_segment_index(ways, cell_size_m)
    radius_cells = max(1, math.ceil(max_endpoint_distance_m / cell_size_m))
    records: list[dict[str, Any]] = []

    for row in links[["edge_index", "edge_id", "from_node_id", "to_node_id"]].itertuples(index=False):
        u = int(row.from_node_id)
        v = int(row.to_node_id)
        if u not in coords_xy or v not in coords_xy:
            continue
        start_xy = coords_xy[u]
        end_xy = coords_xy[v]
        mid_xy = ((start_xy[0] + end_xy[0]) / 2.0, (start_xy[1] + end_xy[1]) / 2.0)
        candidates = candidate_segments([start_xy, end_xy, mid_xy], grid, cell_size_m, radius_cells)

        by_way: dict[int, set[int]] = defaultdict(set)
        for way_idx, seg_idx in candidates:
            by_way[way_idx].add(seg_idx)

        best: dict[str, Any] | None = None
        for way_idx, seg_indices in by_way.items():
            way = ways[way_idx]
            # Once a candidate way is found, allow all its segments for endpoint projection.
            start_match = nearest_on_way(start_xy, way)
            end_match = nearest_on_way(end_xy, way)
            mid_match = nearest_on_way(mid_xy, way)
            if start_match["dist_m"] > max_endpoint_distance_m or end_match["dist_m"] > max_endpoint_distance_m:
                continue
            if mid_match["dist_m"] > max_midpoint_distance_m:
                continue

            edge_len = distance(start_xy, end_xy)
            osm_len = abs(float(end_match["pos"]) - float(start_match["pos"]))
            length_penalty = abs(osm_len - edge_len) / max(edge_len, 1.0)
            score = (
                max(float(start_match["dist_m"]), float(end_match["dist_m"]))
                + 0.45 * float(mid_match["dist_m"])
                + 18.0 * min(length_penalty, 4.0)
            )
            if best is None or score < best["score"]:
                best = {
                    "way_idx": way_idx,
                    "score": score,
                    "start": start_match,
                    "end": end_match,
                    "mid": mid_match,
                    "osm_len": osm_len,
                }

        if best is None:
            continue

        way = ways[int(best["way_idx"])]
        geometry = extract_subline(way, best["start"], best["end"], coords_lonlat[u], coords_lonlat[v])
        records.append(
            {
                "edge_index": int(row.edge_index),
                "edge_id": str(row.edge_id),
                "from_node_id": u,
                "to_node_id": v,
                "osm_way_id": int(way["id"]),
                "osm_highway": way["highway"],
                "osm_name": way["name"],
                "osm_oneway": way["oneway"],
                "match_score_m": round(float(best["score"]), 3),
                "start_distance_m": round(float(best["start"]["dist_m"]), 3),
                "end_distance_m": round(float(best["end"]["dist_m"]), 3),
                "mid_distance_m": round(float(best["mid"]["dist_m"]), 3),
                "osm_subline_length_m": round(float(best["osm_len"]), 3),
                "geometry_wgs84": json.dumps(geometry, ensure_ascii=False, separators=(",", ":")),
            }
        )
    return pd.DataFrame.from_records(records)


def main() -> None:
    parser = argparse.ArgumentParser(description="Fetch OSM highway geometry and match it to processed edge_index rows.")
    parser.add_argument("--osm-json", type=Path, default=DEFAULT_OSM_JSON)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--endpoint", action="append", default=None, help="Overpass endpoint. Can be repeated.")
    parser.add_argument("--timeout-seconds", type=int, default=180)
    parser.add_argument("--bbox-padding-deg", type=float, default=0.01)
    parser.add_argument("--max-endpoint-distance-m", type=float, default=90.0)
    parser.add_argument("--max-midpoint-distance-m", type=float, default=160.0)
    parser.add_argument("--cell-size-m", type=float, default=220.0)
    parser.add_argument("--reuse-osm-json", action="store_true")
    args = parser.parse_args()

    nodes = pd.read_csv(PROCESSED_DIR / "nodes_clean.csv")
    links = pd.read_csv(PROCESSED_DIR / "links_clean.csv")

    south = float(nodes["y_84"].min()) - args.bbox_padding_deg
    north = float(nodes["y_84"].max()) + args.bbox_padding_deg
    west = float(nodes["x_84"].min()) - args.bbox_padding_deg
    east = float(nodes["x_84"].max()) + args.bbox_padding_deg

    args.osm_json.parent.mkdir(parents=True, exist_ok=True)
    if not args.reuse_osm_json or not args.osm_json.exists():
        started = time.time()
        print(f"Fetching OSM highways for bbox south={south:.6f}, west={west:.6f}, north={north:.6f}, east={east:.6f}")
        endpoint = fetch_osm_highways_with_fallback(
            south,
            west,
            north,
            east,
            args.osm_json,
            args.endpoint or DEFAULT_ENDPOINTS,
            args.timeout_seconds,
        )
        print(f"Saved {args.osm_json} from {endpoint} in {time.time() - started:.1f}s")

    center_lon = float(nodes["x_84"].mean())
    center_lat = float(nodes["y_84"].mean())
    ways = load_osm_ways(args.osm_json, center_lon, center_lat)
    print(f"Loaded {len(ways):,} OSM highway ways after filtering.")

    matched = match_edges(
        nodes,
        links,
        ways,
        args.max_endpoint_distance_m,
        args.max_midpoint_distance_m,
        args.cell_size_m,
    )
    matched.to_csv(args.output, index=False)
    print(f"Matched {len(matched):,} / {len(links):,} edges ({len(matched) / max(len(links), 1):.1%}).")
    print(f"Saved {args.output}")


if __name__ == "__main__":
    main()
