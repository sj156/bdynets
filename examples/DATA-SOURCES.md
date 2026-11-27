# Traffic example source files

| Case | What the supplied source contains | Case inventory |
| --- | --- | --- |
| Beijing | Simulation day 10 from the project link CSV. The gzip preserves the complete original-format day. The project HTML contains the road geometries. Redistribution permission for project-supplied records is undocumented. | [Source manifest](beijing/data/raw/source-manifest.csv) · [Report](beijing/beijing-traffic-report.html) |
| PeMS Flow | Sampled detector and CHP records for 16 June 2026, plus dated project references and a case register for Los Angeles and the Bay Area. Archival access requires a Caltrans account. Redistribution rights for the project-enriched bundle are unverified. | [Source file guide](pems-flow/data/README.md) · [Source manifest](pems-flow/data/raw/source-manifest.csv) |
| PeMS Speed | The source detector speeds and incident starts for the same day, with this example’s own road and detector references. The same access and redistribution conditions apply. | [Source file guide](pems-speed/data/README.md) · [Source manifest](pems-speed/data/raw/source-manifest.csv) |
| City-traffic-M | Original-format Parquet samples for the selected Yekaterinburg roads. The example uses the complete day of volume observations for 18 October 2024. The source leaves the physical volume unit and timezone unspecified. | [Source manifest](metropolis/data/raw/source-manifest.csv) · [Report](metropolis/metropolis-traffic-report.html) |
| Divvy | The 1 January 2026 Chicago departure sample contains 1,950 rows and 13 source columns. Reuse is governed by the [Divvy data license](https://divvybikes.com/data-license-agreement). | [Source manifest](DivvyBike/data/raw/source-manifest.csv) · [Report](DivvyBike/divvy-od-report.html) |

The reports explain the steps from the source samples to the Flow, Speed and
OD drawing objects. The bundled GeoJSON background files supply attributed
OpenStreetMap context from OpenFreeMap. The [example index](README.md) lists
the rendered maps and the commands to reproduce them.
