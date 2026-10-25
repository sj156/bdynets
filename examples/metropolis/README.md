# City-traffic-M maps

720 roads on 18 October 2024, with 96 fifteen-minute intervals. The map
displays congestion or volume, with speed and static attributes in road
popups. A daily trend accompanies the map. The initial interval starts at
13:30.

The HTML supports pan, zoom, playback, time selection and road-information
popups. The “Data, methods and statistics” panel contains the field reference,
preparation method, reference figures and statistical tables.

| File | Contents |
|---|---|
| `metropolis-traffic-maps.html` | Self-contained map and documentation, including data, offline basemap and browser runtime. |
| `metropolis-traffic-maps.Rmd` | Documentation source, data-preparation code, reference figures and statistical calculations. |
| `map-template.html` | Map interface, offline-basemap renderer and embedded Leaflet 1.9.4 script, styles and license. |
| `run.R` | HTML build and figure-export entrypoint. |
| `osm-figures.R` | Data-reading, metric and static-figure functions. |
| `data/metropolis-real.json.gz` | Traffic observations, geometry, display metadata and source provenance. |
| `data/osm-basemap.json.gz` | Offline OSM roads, water, place labels and extraction provenance. |
| `download_osm.py` | Basemap extraction code. |
| `bdynets_0.1.0.tar.gz` | R source package and license for the traffic-reading and figure workflow. |

## Runtime

Viewing the HTML requires a current browser. Map data and browser assets are
embedded in the file. The template and R Markdown are build sources.

The HTML build entrypoint is `Rscript run.R` within the example directory.
It reads the packaged JSON files and generates `metropolis-traffic-maps.html`.
The documentation render uses a temporary directory. Build dependencies are
R packages `jsonlite`, `rmarkdown`, `knitr`, `magrittr` and `bdynets`, plus
Pandoc for HTML conversion. The bundled `bdynets_0.1.0.tar.gz` contains the
version 0.1.0 source package and license. The entrypoint uses this archive in
a temporary R library when bdynets is absent.

The figure-export entrypoint has two arguments:
`Rscript run.R data/metropolis-real.json.gz new-figure-directory`.
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
