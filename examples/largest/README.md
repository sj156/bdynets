# PeMS hourly flow maps

Los Angeles-area traffic flow on 16 June 2026, with 24 hourly rows, 4,112
active source columns and 4,435 display links. Link values are means of
available endpoint-station flows. The initial hour is 16:00.

The HTML supports pan, zoom, playback, time selection and road-information
popups. The “Data, methods and statistics” panel contains the field reference,
preparation method, reference figures and statistical tables.

| File | Contents |
|---|---|
| `largest-traffic-maps.html` | Self-contained map and documentation, including data, offline basemap and browser runtime. |
| `largest-traffic-maps.Rmd` | Documentation source, data-preparation code, reference figures and statistical calculations. |
| `map-template.html` | Map interface, offline-basemap renderer and embedded Leaflet 1.9.4 script, styles and license. |
| `run.R` | HTML build and figure-export entrypoint. |
| `pems-flow.R` | Data-reading, metric and static-figure functions. |
| `data/pems-flow.json.gz` | Traffic observations, geometry, display metadata and source provenance. |
| `data/pems-osm.json.gz` | Offline OSM roads, water, place labels and extraction provenance. |
| `download_osm.py` | Basemap extraction code. |

## Runtime

Viewing the HTML requires a current browser. Map data and browser assets are
embedded in the file. The template and R Markdown are build sources.

The HTML build entrypoint is `Rscript run.R` within the example directory.
It reads the packaged JSON files and generates `largest-traffic-maps.html`.
The documentation render uses a temporary directory. Build dependencies are
R packages `jsonlite`, `rmarkdown` and `knitr`, plus Pandoc for HTML
conversion. Data preparation also uses R package `digest`.

The figure-export entrypoint has two arguments:
`Rscript run.R data/pems-flow.json.gz new-figure-directory`.
It creates static PDF/PNG figures and source records in the named new
directory. The R Markdown describes frame-index conventions and export
function arguments.

## Data preparation

Traffic-snapshot preparation uses the source export identified in the data
provenance. The R Markdown contains its extraction code and field definitions.
This stage is separate from building the example with the packaged snapshot.
Basemap preparation uses Python packages `requests`, `pmtiles` and
`mapbox-vector-tile`. The HTML build and figure export read the local cache.

Map data: [OpenStreetMap contributors](https://www.openstreetmap.org/copyright),
via [Protomaps](https://docs.protomaps.com/basemaps/downloads), ODbL Produced Work.
The basemap archive is dated 20 October 2026; extraction metadata and tile
checksums are stored in the cache.
