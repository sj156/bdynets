# PeMS Flow source files

| Supplied file or directory | Purpose in this example |
| --- | --- |
| [`raw/caltrans/5min/`](raw/caltrans/5min/) | The original-format daily station files for districts 04, 07, 08 and 12. A row gives its five-minute clock in column 1 and detector ID in column 2. The example reads counts in column 10. |
| [`raw/caltrans/metadata/`](raw/caltrans/metadata/) | The complete dated D04 station table supplies detector IDs, coordinates, freeway numbers and directions for road geometry. |
| [`case-register.rds`](case-register.rds) | Supplied settings carried forward from the 21 November handover for the 16 June 2026 case. The register records the LA travel-order choices, 2,203 directed D04 pairs, two region bounds, 52 event category mappings and 24 source-hour starts. The stored `origin` records the earlier source and its SHA-256. The report reads this reference as part of its source setup. |
| [`raw/reference/PeMS_2020_2026_maps/map/`](raw/reference/PeMS_2020_2026_maps/map/) | The project map HTML supplies the LA road vertices and detector IDs. The June flow manifest identifies active positions. The supplied-file manifest lists the paths in this example. |
| [`raw/reference/processing_runtime/`](raw/reference/processing_runtime/) | The dated D04 station-map reference identifies the selected stations used in the Bay Area analysis. |
| [`raw/caltrans/chp/`](raw/caltrans/chp/) | Original-format incident starts and messages for the selected date and districts. The map uses reported incident start locations and clocks. |
| [`raw/reference/CHP_full_English_2026_10_21/`](raw/reference/CHP_full_English_2026_10_21/) | The supplied English descriptions for LA join CHP incident IDs. |
| [`basemap-cache/`](basemap-cache/) | Two independent, attributed OpenStreetMap vector layers covering the full regional map extents. The [example README](../README.md#prepare-a-background-for-another-extent) links to the background preparation steps. |

The [supplied-file manifest](raw/source-manifest.csv) gives the paths, contents, byte sizes and checksums for the files in `data/raw/`. The case register is supplied in `data/` and the two backgrounds in `data/basemap-cache/`. The [upstream register](original-sources.csv) records provenance.
