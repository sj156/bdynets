# Standalone beijing visualization data

`beijing-visualization.json.gz` contains real saved visualization data extracted losslessly from
`../map/saturation_dynamic_leaflet_osm_osmgeom.html`. It is not simulated data. Raw input files are not reconstructed.
The version-1 JSON document has `source`, `provenance` and `payload` fields.
Payload keys retain the original JSON data declarations; JavaScript and map tiles
are excluded. Open with `bdynets::read_traffic_data()`.

Source SHA-256: `62ea476665faf640f146b5998583e4418378ccc04788b30c3e1db9e4b81b8a4e`.
Extracted file SHA-256: `23c72e3c35a6a4da81937c56a4f68f6a9cf58e51fc396bd576f839426fa0fe0b`.

All decoded payload values were compared with the source JSON. Numbers retain
the saved display precision. Beijing absent entries follow the viewer's zero
convention. LargeST nulls remain missing and its links are display connections,
not asserted model adjacency. The source HTML remains unchanged.
