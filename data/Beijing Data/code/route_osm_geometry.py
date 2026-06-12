from __future__ import annotations

import argparse
import heapq
import json
import math
from collections import defaultdict
from pathlib import Path
from typing import Any

import pandas as pd


BASE_DIR = Path(__file__).resolve().parent
PROCESSED_DIR = BASE_DIR / "processed_real_network"
DEFAULT_OSM_JSON = PROCESSED_DIR / "osm_highway_ways.json"
DEFAULT_OUTPUT = PROCESSED_DIR / "osm_routed_edge_geometry.csv"
DEFAULT_REPORT = PROCESSED_DIR / "osm_routed_edge_geometry_report.txt"

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


def load_osm_ways(osm_json: Path) -> list[dict[str, Any]]:
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
        ways.append(
            {
                "id": int(element["id"]),
                "highway": highway,
                "name": tags.get("name", ""),
                "lonlat": [(float(p["lon"]), float(p["lat"])) for p in geometry],
            }
        )
    return ways


def build_graph(
    ways: list[dict[str, Any]],
    center_lon: float,
    center_lat: float,
) -> tuple[list[tuple[float, float]], list[tuple[float, float]], list[list[tuple[int, float]]], dict[tuple[int, int], list[tuple[int, str]]]]:
    node_index: dict[tuple[float, float], int] = {}
    lonlat_nodes: list[tuple[float, float]] = []
    xy_nodes: list[tuple[float, float]] = []
    adjacency: list[list[tuple[int, float]]] = []
    edge_ways: dict[tuple[int, int], list[tuple[int, str]]] = defaultdict(list)

    def get_node(lon: float, lat: float) -> int:
        key = (round(lon, 7), round(lat, 7))
        if key in node_index:
            return node_index[key]
        idx = len(lonlat_nodes)
        node_index[key] = idx
        lonlat_nodes.append(key)
        xy_nodes.append(project_lonlat(key[0], key[1], center_lon, center_lat))
        adjacency.append([])
        return idx

    for way in ways:
        node_ids = [get_node(lon, lat) for lon, lat in way["lonlat"]]
        for a, b in zip(node_ids, node_ids[1:]):
            if a == b:
                continue
            length = distance(xy_nodes[a], xy_nodes[b])
            if length <= 0:
                continue
            adjacency[a].append((b, length))
            adjacency[b].append((a, length))
            edge_ways[(min(a, b), max(a, b))].append((int(way["id"]), str(way["highway"])))
    return lonlat_nodes, xy_nodes, adjacency, edge_ways


def build_grid(xy_nodes: list[tuple[float, float]], cell_size_m: float) -> dict[tuple[int, int], list[int]]:
    grid: dict[tuple[int, int], list[int]] = defaultdict(list)
    for idx, (x, y) in enumerate(xy_nodes):
        grid[(math.floor(x / cell_size_m), math.floor(y / cell_size_m))].append(idx)
    return grid


def nearest_node(
    xy: tuple[float, float],
    xy_nodes: list[tuple[float, float]],
    grid: dict[tuple[int, int], list[int]],
    cell_size_m: float,
    max_radius_m: float,
) -> tuple[int | None, float]:
    ix = math.floor(xy[0] / cell_size_m)
    iy = math.floor(xy[1] / cell_size_m)
    max_cells = max(1, math.ceil(max_radius_m / cell_size_m))
    best_idx: int | None = None
    best_dist = float("inf")
    for radius in range(max_cells + 1):
        for dx in range(-radius, radius + 1):
            for dy in range(-radius, radius + 1):
                if max(abs(dx), abs(dy)) != radius:
                    continue
                for idx in grid.get((ix + dx, iy + dy), []):
                    dist_m = distance(xy, xy_nodes[idx])
                    if dist_m < best_dist:
                        best_idx = idx
                        best_dist = dist_m
        if best_idx is not None and best_dist <= max(radius * cell_size_m, cell_size_m):
            break
    if best_dist > max_radius_m:
        return None, best_dist
    return best_idx, best_dist


def shortest_path(
    adjacency: list[list[tuple[int, float]]],
    source: int,
    target: int,
    max_cost_m: float,
) -> tuple[list[int], float]:
    if source == target:
        return [source], 0.0
    dist = {source: 0.0}
    parent: dict[int, int] = {}
    heap = [(0.0, source)]
    settled: set[int] = set()
    while heap:
        cost, node = heapq.heappop(heap)
        if node in settled:
            continue
        settled.add(node)
        if node == target:
            path = [target]
            while path[-1] != source:
                path.append(parent[path[-1]])
            path.reverse()
            return path, cost
        if cost > max_cost_m:
            break
        for nxt, weight in adjacency[node]:
            new_cost = cost + weight
            if new_cost > max_cost_m:
                continue
            if new_cost < dist.get(nxt, float("inf")):
                dist[nxt] = new_cost
                parent[nxt] = node
                heapq.heappush(heap, (new_cost, nxt))
    return [], float("inf")


def path_highways(path: list[int], edge_ways: dict[tuple[int, int], list[tuple[int, str]]]) -> tuple[str, str]:
    way_ids: list[str] = []
    highways: list[str] = []
    for a, b in zip(path, path[1:]):
        ways = edge_ways.get((min(a, b), max(a, b)), [])
        if not ways:
            continue
        way_id, highway = ways[0]
        way_ids.append(str(way_id))
        highways.append(highway)
    return ";".join(dict.fromkeys(way_ids)), ";".join(dict.fromkeys(highways))


def route_edges(
    nodes: pd.DataFrame,
    links: pd.DataFrame,
    lonlat_nodes: list[tuple[float, float]],
    xy_nodes: list[tuple[float, float]],
    adjacency: list[list[tuple[int, float]]],
    edge_ways: dict[tuple[int, int], list[tuple[int, str]]],
    snap_radius_m: float,
    search_factor: float,
    search_extra_m: float,
    max_route_m: float,
    cell_size_m: float,
    strict_max_ratio: float,
    strict_max_snap_m: float,
) -> pd.DataFrame:
    center_lon = float(nodes["x_84"].mean())
    center_lat = float(nodes["y_84"].mean())
    model_lonlat = {
        int(row.node_id): (float(row.x_84), float(row.y_84))
        for row in nodes[["node_id", "x_84", "y_84"]].dropna().itertuples(index=False)
    }
    model_xy = {
        node_id: project_lonlat(lon, lat, center_lon, center_lat)
        for node_id, (lon, lat) in model_lonlat.items()
    }
    grid = build_grid(xy_nodes, cell_size_m)
    records: list[dict[str, Any]] = []

    for row in links[["edge_index", "edge_id", "from_node_id", "to_node_id", "length", "link_type_name"]].itertuples(index=False):
        u = int(row.from_node_id)
        v = int(row.to_node_id)
        if u not in model_xy or v not in model_xy:
            continue
        start_node, start_snap = nearest_node(model_xy[u], xy_nodes, grid, cell_size_m, snap_radius_m)
        end_node, end_snap = nearest_node(model_xy[v], xy_nodes, grid, cell_size_m, snap_radius_m)
        if start_node is None or end_node is None:
            continue

        euclidean_m = distance(model_xy[u], model_xy[v])
        model_length_m = max(float(row.length) * 1000.0, euclidean_m, 1.0)
        budget_m = min(max_route_m, max(model_length_m * search_factor + search_extra_m, 700.0))
        path, route_m = shortest_path(adjacency, start_node, end_node, budget_m)
        if not path:
            continue

        coords = [[round(lonlat_nodes[idx][0], 7), round(lonlat_nodes[idx][1], 7)] for idx in path]
        if len(coords) == 1:
            coords = [
                [round(model_lonlat[u][0], 7), round(model_lonlat[u][1], 7)],
                [round(model_lonlat[v][0], 7), round(model_lonlat[v][1], 7)],
            ]

        ratio = route_m / model_length_m
        confidence = "high"
        if max(start_snap, end_snap) > strict_max_snap_m or ratio > strict_max_ratio:
            confidence = "low"
        elif len(coords) <= 2:
            confidence = "medium"

        way_ids, highways = path_highways(path, edge_ways)
        records.append(
            {
                "edge_index": int(row.edge_index),
                "edge_id": str(row.edge_id),
                "from_node_id": u,
                "to_node_id": v,
                "model_link_type_name": str(row.link_type_name),
                "osm_way_ids": way_ids,
                "osm_highways": highways,
                "route_confidence": confidence,
                "start_snap_m": round(start_snap, 3),
                "end_snap_m": round(end_snap, 3),
                "route_length_m": round(route_m, 3),
                "model_length_m": round(model_length_m, 3),
                "route_to_model_length_ratio": round(ratio, 4),
                "geometry_point_count": len(coords),
                "geometry_wgs84": json.dumps(coords, ensure_ascii=False, separators=(",", ":")),
            }
        )
    return pd.DataFrame.from_records(records)


def write_report(path: Path, routed: pd.DataFrame, link_count: int) -> None:
    lines = [
        "OSM routed edge geometry report",
        "",
        f"total_model_edges: {link_count}",
        f"routed_edges: {len(routed)}",
        f"unrouted_edges: {link_count - len(routed)}",
        f"routed_rate: {len(routed) / max(link_count, 1):.3%}",
        "",
    ]
    if not routed.empty:
        lines.append("confidence_counts:")
        lines.append(routed["route_confidence"].value_counts().to_string())
        lines.append("")
        lines.append("geometry_point_count:")
        lines.append(routed["geometry_point_count"].describe(percentiles=[0.5, 0.75, 0.9, 0.95, 0.99]).to_string())
        lines.append("")
        lines.append("route_to_model_length_ratio:")
        lines.append(routed["route_to_model_length_ratio"].describe(percentiles=[0.5, 0.75, 0.9, 0.95, 0.99]).to_string())
        lines.append("")
        lines.append("start_snap_m/end_snap_m:")
        lines.append(routed[["start_snap_m", "end_snap_m"]].describe(percentiles=[0.5, 0.75, 0.9, 0.95, 0.99]).to_string())
        lines.append("")
        lines.append("model_link_type_name:")
        lines.append(routed["model_link_type_name"].value_counts().to_string())
    path.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description="Route every model edge along the downloaded OSM highway network.")
    parser.add_argument("--osm-json", type=Path, default=DEFAULT_OSM_JSON)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--report", type=Path, default=DEFAULT_REPORT)
    parser.add_argument("--snap-radius-m", type=float, default=260.0)
    parser.add_argument("--cell-size-m", type=float, default=180.0)
    parser.add_argument("--search-factor", type=float, default=8.0)
    parser.add_argument("--search-extra-m", type=float, default=900.0)
    parser.add_argument("--max-route-m", type=float, default=18_000.0)
    parser.add_argument("--strict-max-ratio", type=float, default=4.5)
    parser.add_argument("--strict-max-snap-m", type=float, default=90.0)
    args = parser.parse_args()

    nodes = pd.read_csv(PROCESSED_DIR / "nodes_clean.csv")
    links = pd.read_csv(PROCESSED_DIR / "links_clean.csv")
    ways = load_osm_ways(args.osm_json)
    center_lon = float(nodes["x_84"].mean())
    center_lat = float(nodes["y_84"].mean())
    lonlat_nodes, xy_nodes, adjacency, edge_ways = build_graph(ways, center_lon, center_lat)
    print(f"Loaded {len(ways):,} ways, {len(lonlat_nodes):,} graph nodes.")

    routed = route_edges(
        nodes,
        links,
        lonlat_nodes,
        xy_nodes,
        adjacency,
        edge_ways,
        args.snap_radius_m,
        args.search_factor,
        args.search_extra_m,
        args.max_route_m,
        args.cell_size_m,
        args.strict_max_ratio,
        args.strict_max_snap_m,
    )
    routed.to_csv(args.output, index=False)
    write_report(args.report, routed, len(links))
    print(f"Routed {len(routed):,} / {len(links):,} edges ({len(routed) / max(len(links), 1):.1%}).")
    print(f"Saved {args.output}")
    print(f"Saved {args.report}")


if __name__ == "__main__":
    main()
