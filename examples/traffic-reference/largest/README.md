# LargeST traffic maps with offline OpenStreetMap context

Install `bdynets`, `jsonlite`, `rmarkdown` and `knitr`, then from the repository root:

```sh
Rscript -e 'rmarkdown::render("examples/largest/largest-traffic-maps.Rmd")'
```

Open `largest-traffic-maps.html` for both regional maps at 08:00. Rendering is
offline. The local `data/` directory contains all 288 traffic frames, OSM-derived
road/city extracts and download provenance. `osm-figures.R` draws both layers in
Web Mercator. The OSM background is contemporary; traffic is from July 31, 2019.
Display links are retained rather than map-matched to OSM roads.

The R Markdown creates `output/largest-0800/` with PDF/PNG figures, selected frame
RDS, input checksums and session information. Re-rendering replaces these named
exports. Set `time_index` and `output_dir` render parameters for other runs.

`download_osm.py` refreshes missing OSM extracts explicitly; it is never run by
the tutorial. Data © OpenStreetMap contributors, ODbL 1.0:
https://www.openstreetmap.org/copyright. Source archive, tile checksums and retrieval dates are
stored in `data/*-osm-provenance.json`.

For batch plotting, `run.R` accepts a snapshot and output directory and uses
this same OSM plotting workflow at 08:00.
