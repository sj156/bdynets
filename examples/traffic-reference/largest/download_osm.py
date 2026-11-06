"""Cache regional OSM-derived display geometry from a fixed Protomaps build.
One-time refresh only: pip install pmtiles mapbox-vector-tile requests
Normal R Markdown rendering is offline and does not need Python.
"""
import datetime, functools, gzip, hashlib, json, math, pathlib
import requests, mapbox_vector_tile
from pmtiles.reader import Reader
ROOT=pathlib.Path(__file__).resolve().parent
URL='https://build.protomaps.com/20261012.pmtiles'
ZOOM=9
session=requests.Session()
@functools.lru_cache(maxsize=512)
def get_bytes(offset,length):
    response=session.get(URL,headers={'Range':f'bytes={offset}-{offset+length-1}'},timeout=60)
    response.raise_for_status()
    if response.status_code!=206 or len(response.content)!=length:
        raise RuntimeError('Server did not honor byte range')
    return response.content
reader=Reader(get_bytes)
metadata=reader.metadata()
x=json.load(gzip.open(ROOT/'data/largest-visualization.json.gz'))
def tile(lon,lat):
    return int((lon+180)/360*2**ZOOM),int((1-math.asinh(math.tan(math.radians(lat)))/math.pi)/2*2**ZOOM)
def geographic(p,tx,ty,extent):
    lon=(tx+p[0]/extent)/2**ZOOM*360-180
    lat=math.degrees(math.atan(math.sinh(math.pi*(1-2*(ty+p[1]/extent)/2**ZOOM))))
    return {'lon':lon,'lat':lat}
for name,region in x['payload']['data']['regions'].items():
    slug='bay-area' if name=='Bay Area' else 'los-angeles'
    dest=ROOT/'data'/f'{slug}-osm.json.gz'
    if dest.exists(): continue
    points=[p for link in region['links'] for p in link['coords']]
    south,west,north,east=min(p[0] for p in points)-.03,min(p[1] for p in points)-.03,max(p[0] for p in points)+.03,max(p[1] for p in points)+.03
    xmin,ymax=tile(west,south); xmax,ymin=tile(east,north)
    elements=[]; tiles=[]; places=set()
    for tx in range(xmin,xmax+1):
        for ty in range(ymin,ymax+1):
            raw=reader.get(ZOOM,tx,ty)
            if raw is None: raise RuntimeError(f'Missing tile {tx},{ty}')
            tiles.append({'z':ZOOM,'x':tx,'y':ty,'sha256':hashlib.sha256(raw).hexdigest()})
            decoded=mapbox_vector_tile.decode(gzip.decompress(raw),default_options={'y_coord_down':True})
            for layer in ['roads','places','water']:
                data=decoded.get(layer,{}); extent=data.get('extent',4096)
                for feature in data.get('features',[]):
                    props=feature['properties']; geom=feature['geometry']; coords=geom['coordinates']; kind=props.get('kind','')
                    if layer=='roads':
                        if kind not in ['highway','major_road','medium_road','minor_road']: continue
                        lines=[coords] if geom['type']=='LineString' else coords if geom['type']=='MultiLineString' else []
                        for line in lines:
                            elements.append({'type':'way','tags':{'highway':props.get('kind_detail',kind),'name':props.get('name','')},'geometry':[geographic(p,tx,ty,extent) for p in line]})
                    elif layer=='places' and geom['type']=='Point' and kind=='locality':
                        label=props.get('name:en',props.get('name','')); p=geographic(coords,tx,ty,extent)
                        if label and label not in places:
                            elements.append({'type':'node',**p,'tags':{'name':label},'population':props.get('population',0)})
                            places.add(label)
                    elif layer=='water':
                        polygons=[coords] if geom['type']=='Polygon' else coords if geom['type']=='MultiPolygon' else []
                        for polygon in polygons:
                            elements.append({'type':'water','rings':[[geographic(p,tx,ty,extent) for p in ring] for ring in polygon]})
            print(slug,tx,ty,flush=True)
    artifact={'schema':'protomaps-display-extract-v1','source_url':URL,'zoom':ZOOM,'elements':elements}
    encoded=json.dumps(artifact,separators=(',',':')).encode()
    with gzip.open(dest,'wb') as f: f.write(encoded)
    provenance={'source_url':URL,'build_date':'2026-10-12','retrieved_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'bbox_south_west_north_east':[south,west,north,east],'zoom':ZOOM,'tiles':tiles,'sha256_uncompressed':hashlib.sha256(encoded).hexdigest(),'source_metadata':metadata,'attribution':'© OpenStreetMap contributors; Protomaps','license':'OSM-derived Protomaps basemap, ODbL Produced Work','copyright':'https://www.openstreetmap.org/copyright','limitations':'Generalized display geometry, clipped by vector tile boundaries. Not raw OSM ways or routing topology.'}
    (ROOT/'data'/f'{slug}-osm-provenance.json').write_text(json.dumps(provenance,indent=2))
    print(slug,'saved',len(elements),'features',flush=True)
