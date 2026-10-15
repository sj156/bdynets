# Standalone largest visualization data

`largest-visualization.json.gz` contains real saved visualization data extracted losslessly from
`../map/largest_la_bay_area_flow_2019_07_31.html`. It is not simulated data. Raw input files are not reconstructed.
The version-1 JSON document has `source`, `provenance` and `payload` fields.
Payload keys retain the original JSON data declarations; JavaScript and map tiles
are excluded. Open with `bdynets::read_traffic_data()`.

Source SHA-256: `8da4eeb1058ab57bb32943dc7c87bf2bf668ba4c43ccf3d54867e6060760b60b`.
Extracted file SHA-256: `28377d00b44803d9d2165ac57df2ef61e400bca72266b57cb1a6e37a11ff4f09`.

All decoded payload values were compared with the source JSON. Numbers retain
the saved display precision. Beijing absent entries follow the viewer's zero
convention. LargeST nulls remain missing and its links are display connections,
not asserted model adjacency. The source HTML remains unchanged.
