"""Build an offline vector background for a declared extent from OpenFreeMap tiles."""
import argparse
from concurrent.futures import ThreadPoolExecutor
import gzip
import hashlib
import json
import math
import zlib
from pathlib import Path
from tempfile import NamedTemporaryFile
from urllib.request import Request, urlopen

from mapbox_vector_tile import decode
from shapely.geometry import box, shape

USER_AGENT = "bdynets-openstreetmap-preparation/1.0"
SOURCE = "https://tiles.openfreemap.org/planet"
PROVIDER = "OpenStreetMap via OpenFreeMap/OpenMapTiles"
LATITUDE_LIMIT = 85.0511287798066
ATTRIBUTION = "OpenFreeMap · © OpenMapTiles · © OpenStreetMap contributors (ODbL)"
MAIN = {"motorway", "trunk", "primary", "secondary", "tertiary"}
LOCAL = {"minor", "service"}
PLACE = {"city", "town", "village", "suburb", "borough"}


def download(url):
    request = Request(url, headers={"User-Agent": USER_AGENT})
    with urlopen(request, timeout=45) as response:
        return response.read(10_000_001)


def mercator_y(latitude):
    return (1 - math.asinh(math.tan(math.radians(latitude))) / math.pi) / 2


def tile(longitude, latitude, zoom):
    n = 1 << zoom
    return (min(n - 1, int(n * (longitude + 180) / 360)),
            max(0, min(n - 1, int(n * mercator_y(latitude)))))


def tile_keys(bounds, zoom):
    west, east, south, north = bounds
    n = 1 << zoom
    first_x = math.floor(n * (west + 180) / 360)
    last_x = math.floor(n * (math.nextafter(east, west) + 180) / 360)
    first_y = tile(0, north, zoom)[1]
    last_y = tile(0, south, zoom)[1]
    columns = min(n, last_x - first_x + 1)
    return [(x % n, y) for x in range(first_x, first_x + columns)
            for y in range(first_y, last_y + 1)]


def zoom_for(bounds):
    west, east, south, north = bounds
    span = max((east - west) / 360, mercator_y(south) - mercator_y(north))
    return max(0, min(14, math.floor(math.log2(6 / span))))


def validate_bounds(bounds):
    if (not isinstance(bounds, (tuple, list)) or len(bounds) != 4 or
            not all(type(v) in (int, float) and math.isfinite(v) for v in bounds)):
        raise ValueError("Supply finite bounds in west, east, south, north order.")
    west, east, south, north = bounds
    if not (0 < east - west <= 360 and -LATITUDE_LIMIT <= south < north <= LATITUDE_LIMIT):
        raise ValueError("Use increasing, nonempty bounds within Web Mercator limits.")
    shift = 360 * math.floor((west + 180) / 360)
    return [west - shift, east - shift, south, north]


def valid_position(position):
    return (isinstance(position, list) and len(position) == 2
            and all(type(value) in (int, float) for value in position)
            and abs(position[0]) <= 180 and abs(position[1]) <= LATITUDE_LIMIT)


def valid_line(positions, minimum):
    return (isinstance(positions, list) and len(positions) >= minimum
            and all(valid_position(position) for position in positions))


def valid_feature(feature):
    if not isinstance(feature, dict) or feature.get("type") != "Feature":
        return False
    properties = feature.get("properties")
    if properties is not None:
        if not isinstance(properties, dict):
            return False
        if any(properties.get(key) is not None and not isinstance(properties[key], str)
               for key in ("label", "context")):
            return False
    geometry = feature.get("geometry")
    if not isinstance(geometry, dict):
        return False
    kind = geometry.get("type")
    positions = geometry.get("coordinates")
    if kind == "Point":
        return valid_position(positions)
    if kind == "LineString":
        return valid_line(positions, 2)
    return (kind == "Polygon" and isinstance(positions, list) and bool(positions)
            and all(valid_line(ring, 4) and ring[0] == ring[-1] for ring in positions))


def valid_collection(cache):
    if not isinstance(cache, dict) or cache.get("type") != "FeatureCollection":
        return False
    features = cache.get("features")
    attribution = cache.get("attribution")
    return ((attribution is None or isinstance(attribution, str) and bool(attribution))
            and isinstance(features, list) and all(valid_feature(feature) for feature in features))


def read_cache(path):
    with gzip.open(path, "rt", encoding="utf-8") as stream:
        return json.load(stream)


def covering_cache(cache_dir, bounds, zoom):
    west, east, south, north = bounds
    best = None
    for candidate in sorted(cache_dir.glob("*.geojson.gz")):
        try:
            cache = read_cache(candidate)
            provenance = cache["provenance"]
            if provenance["provider"] != PROVIDER or "bounds" not in provenance:
                continue
            w, e, s, n = validate_bounds(provenance["bounds"])
            detail = provenance["zoom"]
            if not (type(detail) is int and zoom <= detail <= 14 and s <= south <= north <= n
                    and any(w <= west + offset <= east + offset <= e for offset in (-360, 0, 360))):
                continue
            rank = (detail, (e - w) * (n - s), candidate)
            if (best is None or rank < best) and valid_collection(cache):
                best = rank
        except (OSError, EOFError, ValueError, KeyError, TypeError, OverflowError, zlib.error):
            continue
    return best[2] if best is not None else None


def geometry_parts(geometry, kind):
    if geometry.is_empty:
        return
    if geometry.geom_type == kind:
        yield geometry
    elif hasattr(geometry, "geoms"):
        for part in geometry.geoms:
            yield from geometry_parts(part, kind)


def features_from_tile(blob, tile_x, tile_y, zoom):
    levels = decode(gzip.decompress(blob) if blob[:2] == b"\x1f\x8b" else blob,
                    default_options={"y_coord_down": True})
    n = 1 << zoom
    layers = {"transportation": "LineString", "waterway": "LineString",
              "water": "Polygon", "place": "Point"}
    for layer, kind in layers.items():
        if layer not in levels:
            continue
        level = levels[layer]
        extent = level["extent"]
        footprint = box(0, 0, extent, extent)

        def convert(position):
            x_coord, y_coord = position[:2]
            longitude = 360 * (tile_x + x_coord / extent) / n - 180
            latitude = math.degrees(math.atan(math.sinh(math.pi * (1 - 2 * (tile_y + y_coord / extent) / n))))
            return [longitude, latitude]

        for item in level["features"]:
            prop = item["properties"]
            category = prop.get("class")
            if layer == "transportation" and category not in (MAIN | LOCAL if zoom >= 11 else MAIN):
                continue
            if layer == "waterway" and category not in {"river", "canal"}:
                continue
            if layer == "place" and (category not in PLACE or not prop.get("name:en", prop.get("name"))):
                continue
            original = shape(item["geometry"])
            if original.is_empty:
                continue
            if kind == "Polygon" and not original.is_valid:
                from shapely import make_valid
                original = make_valid(original)
            local = original.intersection(footprint)
            for part in geometry_parts(local, kind):
                if kind == "Polygon":
                    coordinates = [[convert(vertex) for vertex in ring.coords]
                                   for ring in (part.exterior, *part.interiors)]
                    if part.area < 120:
                        continue
                elif kind == "LineString":
                    coordinates = [convert(vertex) for vertex in part.coords]
                else:
                    coordinates = convert(part.coords[0])
                name = prop.get("name:en", prop.get("name"))
                properties = {"context": layer}
                if name and kind == "Point":
                    properties["label"] = name
                    properties["label_rank"] = {"city": 0, "town": 1, "suburb": 2, "borough": 2, "village": 3}[category]
                yield {"type": "Feature", "geometry": dict(type=kind, coordinates=coordinates),
                       "properties": properties}


def collect(bounds, zoom, output):
    keys = tile_keys(bounds, zoom)
    source = json.loads(download(SOURCE))["tiles"][0]

    def read(key):
        x, y = key
        path = source.format(z=zoom, x=x, y=y)
        blob = download(path)
        return key, hashlib.sha256(blob).hexdigest(), list(features_from_tile(blob, x, y, zoom))

    with ThreadPoolExecutor(max_workers=4) as workers:
        parts = list(workers.map(read, keys))
    features = [feature for part in parts for feature in part[2]]
    result = {"type": "FeatureCollection", "features": ordered_features(features), "attribution": ATTRIBUTION,
        "provenance": {"provider": PROVIDER, "tile_template": source,
            "bounds": bounds, "zoom": zoom, "tiles": [{"x": x, "y": y, "sha256": digest}
                for (x, y), digest, _ in parts]}}
    write_cache(result, output)


def ordered_features(features):
    order = {"water": 0, "waterway": 1, "transportation": 2, "place": 3}

    def layer(feature):
        properties = feature.get("properties") or {}
        rank = properties.get("label_rank")
        return order.get(properties.get("context"), 2), rank if type(rank) is int else 0

    return sorted(features, key=layer)


def write_cache(result, output):
    with NamedTemporaryFile(dir=output.parent, suffix=".part", delete=False) as temporary:
        staged = Path(temporary.name)
    try:
        with gzip.open(staged, "wt", encoding="utf-8") as handle:
            json.dump(result, handle, ensure_ascii=False, separators=(",", ":"))
        staged.replace(output)
    finally:
        staged.unlink(missing_ok=True)


def prepare_scope(bounds, cache_dir):
    zoom = zoom_for(bounds)
    cached = covering_cache(cache_dir, bounds, zoom)
    if cached is not None:
        return cached
    identity = hashlib.sha256(json.dumps([bounds, zoom]).encode()).hexdigest()[:16]
    output = cache_dir / f"openfreemap-{zoom}-{identity}.geojson.gz"
    cache_dir.mkdir(parents=True, exist_ok=True)
    collect(bounds, zoom, output)
    return output


def combine_scopes(paths, cache_dir):
    if len(paths) == 1:
        return paths[0]

    sources = {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in paths}
    identity = hashlib.sha256(json.dumps(sources, sort_keys=True).encode()).hexdigest()[:16]
    output = cache_dir / f"regional-{identity}.geojson.gz"
    provenance = {"provider": PROVIDER, "source_caches": sources}
    if output.exists():
        try:
            previous = read_cache(output)
            if valid_collection(previous) and previous.get("provenance") == provenance:
                return output
        except (OSError, EOFError, ValueError, zlib.error):
            pass

    unique = {}
    for path in paths:
        for feature in read_cache(path)["features"]:
            unique.setdefault(json.dumps(feature, sort_keys=True, separators=(",", ":")), feature)
    result = {"type": "FeatureCollection", "features": ordered_features(unique.values()),
              "attribution": ATTRIBUTION, "provenance": provenance}
    write_cache(result, output)
    return output


def prepare(extents, cache_dir):
    if not isinstance(extents, (list, tuple)) or not extents:
        raise ValueError("Supply one extent or a nonempty list of extents.")
    requested = extents if isinstance(extents[0], (list, tuple)) else [extents]
    bounds = sorted({tuple(validate_bounds(extent)) for extent in requested})
    paths = sorted({prepare_scope(list(extent), cache_dir) for extent in bounds})
    return combine_scopes(paths, cache_dir)


def write_path_file(destination, cache_path):
    with NamedTemporaryFile(dir=destination.parent, delete=False) as temporary:
        staged = Path(temporary.name)
    try:
        staged.write_text(str(cache_path) + "\n", encoding="utf-8")
        staged.replace(destination)
    finally:
        staged.unlink(missing_ok=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("extent", type=Path, help="JSON extent or list of west, east, south, north extents")
    parser.add_argument("cache_dir", type=Path)
    parser.add_argument("--path-file", type=Path, help="Save the absolute GeoJSON path after preparation succeeds")
    args = parser.parse_args()
    if args.path_file is not None:
        destination = args.path_file.resolve()
        if destination == args.extent.resolve():
            parser.error("--path-file must be separate from the extent input.")
        if any(path.name.lower().endswith((".geojson", ".geojson.gz")) for path in (args.path_file, destination)):
            parser.error("--path-file must be a text pointer separate from the GeoJSON cache.")
        if destination == args.cache_dir.resolve() or destination.is_dir():
            parser.error("--path-file must name a text file, not a directory.")
    path = prepare(json.loads(args.extent.read_text(encoding="utf-8")), args.cache_dir).resolve()
    if args.path_file is not None:
        write_path_file(args.path_file, path)
    print(path)
