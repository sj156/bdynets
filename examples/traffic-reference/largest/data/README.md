# Offline inputs for the LargeST traffic maps

These files belong in Git. The tutorial reads them locally, without a web service.

- `largest-visualization.json.gz`: complete saved traffic display snapshot, copied
  byte-for-byte from `data/LargeST Data/processed/largest-visualization.json.gz`.
  It contains the two regions and all 288 five-minute frames for July 31, 2019.
  Original traffic-source identity is embedded in this file.
- `los-angeles-osm.json.gz`, `bay-area-osm.json.gz`: display geometry for roads,
  water polygons and place labels, extracted at zoom 9 from Protomaps' October 12,
  2026 OpenStreetMap-derived basemap. These are generalized map features, not raw
  OSM ways, road topology or a historical reconstruction of the 2019 network.
- `*-osm-provenance.json`: source URL, build date, retrieval time, bounding box,
  tile coordinates and hashes, source metadata and uncompressed extract checksum.
- `SHA256SUMS`: checksums of the saved compressed inputs and provenance files.

Map data © OpenStreetMap contributors; basemap by Protomaps.
OSM: https://www.openstreetmap.org/copyright
Protomaps distribution: https://docs.protomaps.com/basemaps/downloads
Protomaps distributes the basemap as an ODbL Produced Work. Retain the attribution
when reusing these map figures or extracts. This notice concerns the basemap;
traffic-source provenance is recorded separately in the traffic snapshot.

The extract script is `../download_osm.py`. It is only needed for explicit
refreshes, not knitting. Upstream daily archives may expire; these saved extracts
and checksums preserve the exact inputs used here. All required rendering inputs
are committed directly rather than stored as Git LFS pointers.
