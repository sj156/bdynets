# OpenStreetMap-derived graph data

This directory contains a minimized public export of the approved 100-location
central-Beijing road design:

- `central_beijing_100_nodes.csv`: the 100 selected road vertices and the seven
  fields needed to identify the fixed spatially balanced sample;
- `central_beijing_q4_edges.csv`: 242 undirected q=4 statistical graph edges,
  with road distance and final symmetrized weight;
- `central_beijing_100_graph_input.rds`: the same locations plus `D_km`, `W`,
  `A`, `L`, `Phi`, `q`, `h`, group boundaries and the minimum metadata required
  by the included method-input constructors.

Selection used seed 2026090201: four road vertices were sampled uniformly
without replacement in every cell of a fixed 5-by-5 local-square grid. Groups
are five equal-size sets (20 each) ordered by average directed road-network
distance from the snapped center. The qNN graph uses `q=4`; its 242-edge
support is connected and has no isolates. The bandwidth is the median fourth
nearest road distance, `h=3.8978354 km`.

The full OpenStreetMap road graph, its 81,452-row plotting subset, directed
distance matrix and local source paths are deliberately excluded. The public
RDS is therefore not byte-identical to the original approved evidence RDS. The
original evidence SHA-256 is retained in metadata for lineage, while the public
export's own file hash is recorded in `provenance/CHECKSUMS.sha256`.

The frozen function `countdlm_road_load_approved_context()` checks the hash of
the excluded original evidence file and will correctly reject this minimized
export. Use `scripts/check_inputs.R` to validate the public export and then
`readRDS()` as demonstrated in the reproduction script. This keeps the executed
scientific source byte-identical while making the handoff self-contained.

## License and attribution

These graph data are derived from OpenStreetMap and are not covered by the code
license in `LICENSE-CODE.md`.

**Data © OpenStreetMap contributors, available under the Open Database License
(ODbL) 1.0.**

- OpenStreetMap copyright and attribution: <https://www.openstreetmap.org/copyright/>
- ODbL 1.0 legal text: <https://opendatacommons.org/licenses/odbl/1-0/>

If this derived database is redistributed or adapted, preserve the attribution,
license notices and applicable ODbL share-alike obligations.

