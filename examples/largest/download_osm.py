"""OSM-derived roads, water and place labels for the PeMS display extent."""
import datetime
import functools
import gzip
import hashlib
import json
import math
from pathlib import Path

import mapbox_vector_tile
import requests
from pmtiles.reader import Reader

ROOT = Path(__file__).resolve().parent
URL = 'https://build.protomaps.com/20261020.pmtiles'
ZOOM = 9
OUTPUT = ROOT / 'data/pems-osm.json.gz'


def build_cache():
    if OUTPUT.exists():
        raise FileExistsError('OSM cache already exists')
    with gzip.open(ROOT / 'data/pems-flow.json.gz', 'rt') as stream:
        payload = json.load(stream)['payload']
    points = [p for link in payload['links'] for p in link['coords']]
    south = min(p[0] for p in points) - .03
    west = min(p[1] for p in points) - .03
    north = max(p[0] for p in points) + .03
    east = max(p[1] for p in points) + .03
    session = requests.Session()

    @functools.lru_cache(maxsize=512)
    def get_bytes(offset, length):
        end = offset + length - 1
        response = session.get(URL, headers={'Range': f'bytes={offset}-{end}'}, timeout=25)
        response.raise_for_status()
        expected = f'bytes {offset}-{end}/'
        if response.status_code != 206 or len(response.content) != length or not response.headers.get('Content-Range', '').startswith(expected):
            raise RuntimeError('PMTiles byte-range response does not match the request')
        return response.content

    reader = Reader(get_bytes)
    metadata = reader.metadata()

    def tile(lon, lat):
        return (int((lon + 180) / 360 * 2**ZOOM),
                int((1 - math.asinh(math.tan(math.radians(lat))) / math.pi) / 2 * 2**ZOOM))

    def geographic(point, tx, ty, extent):
        lon = (tx + point[0] / extent) / 2**ZOOM * 360 - 180
        lat = math.degrees(math.atan(math.sinh(math.pi * (1 - 2 * (ty + point[1] / extent) / 2**ZOOM))))
        return {'lon': lon, 'lat': lat}

    xmin, ymax = tile(west, south)
    xmax, ymin = tile(east, north)
    elements, tiles, places = [], [], set()
    total = (xmax - xmin + 1) * (ymax - ymin + 1)
    for tx in range(xmin, xmax + 1):
        for ty in range(ymin, ymax + 1):
            raw = reader.get(ZOOM, tx, ty)
            if raw is None:
                raise RuntimeError(f'PMTiles tile absent: {ZOOM}/{tx}/{ty}')
            tiles.append({'z': ZOOM, 'x': tx, 'y': ty, 'sha256': hashlib.sha256(raw).hexdigest()})
            decoded = mapbox_vector_tile.decode(gzip.decompress(raw) if raw[:2] == b'\x1f\x8b' else raw,
                                                default_options={'y_coord_down': True})
            for layer in ['roads', 'places', 'water']:
                data = decoded.get(layer, {})
                extent = data.get('extent', 4096)
                for feature in data.get('features', []):
                    props = feature['properties']
                    geom = feature['geometry']
                    coords = geom['coordinates']
                    kind = props.get('kind', '')
                    if layer == 'roads':
                        if kind not in ['highway', 'major_road', 'medium_road', 'minor_road']:
                            continue
                        lines = [coords] if geom['type'] == 'LineString' else coords if geom['type'] == 'MultiLineString' else []
                        for line in lines:
                            elements.append({'type': 'way', 'tags': {'highway': props.get('kind_detail', kind), 'name': props.get('name', '')},
                                             'geometry': [geographic(p, tx, ty, extent) for p in line]})
                    elif layer == 'places' and geom['type'] == 'Point' and kind == 'locality':
                        name = props.get('name:en', props.get('name', ''))
                        if name and name not in places:
                            elements.append({'type': 'node', **geographic(coords, tx, ty, extent),
                                             'tags': {'name': name}, 'population': props.get('population', 0)})
                            places.add(name)
                    elif layer == 'water':
                        polygons = [coords] if geom['type'] == 'Polygon' else coords if geom['type'] == 'MultiPolygon' else []
                        for polygon in polygons:
                            elements.append({'type': 'water', 'rings': [[geographic(p, tx, ty, extent) for p in ring] for ring in polygon]})
            print(f'OSM tile {len(tiles)}/{total}: {ZOOM}/{tx}/{ty}', flush=True)
    geometry = {'schema': 'protomaps-display-extract-v1', 'source_url': URL, 'zoom': ZOOM, 'elements': elements}
    geometry_hash = hashlib.sha256(json.dumps(geometry, separators=(',', ':'), ensure_ascii=False).encode()).hexdigest()
    artifact = {**geometry, 'provenance': {
        'source_url': URL, 'build_date': '2026-10-20',
        'retrieved_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
        'bbox_south_west_north_east': [south, west, north, east], 'zoom': ZOOM, 'tiles': tiles,
        'geometry_sha256': geometry_hash, 'source_metadata': metadata,
        'attribution': 'OpenStreetMap contributors; Protomaps',
        'license': 'OSM-derived Protomaps basemap, ODbL Produced Work',
        'copyright': 'https://www.openstreetmap.org/copyright',
        'limitations': 'Generalized zoom-9 display geometry split at tile boundaries; not routing topology or a historical road-network reconstruction.'}}
    with OUTPUT.open('wb') as stream:
        with gzip.GzipFile(filename='', mode='wb', fileobj=stream, mtime=0) as compressed:
            compressed.write(json.dumps(artifact, separators=(',', ':'), ensure_ascii=False).encode())
    print(json.dumps({'tiles': len(tiles), 'elements': len(elements), 'bytes': OUTPUT.stat().st_size}), flush=True)


if __name__ == '__main__':
    build_cache()
