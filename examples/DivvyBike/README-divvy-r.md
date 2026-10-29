# Divvy Bike R aggregation and OD map

This example uses exported functions from the `bdynets` R package to rebuild an
arbitrary-radius aggregation directly from the raw Divvy node and hourly OD
tables, validate trip-count conservation, and draw a static north-up OD map.

From the repository root, install the package and render the report:

```bash
R CMD INSTALL bdynets
Rscript -e 'rmarkdown::render("examples/DivvyBike/divvy-od-map.Rmd")'
```

The package imports `data.table`, `ggplot2`, `grid`, and `jsonlite`. Rendering
also requires `rmarkdown` and `knitr`.

The R Markdown file resolves all example resources relative to
`examples/DivvyBike/`:

- raw input tables: `data/nodes_raw.csv` and `data/od_flow_*_raw.csv`;
- static street background: `data/chicago_streets_divvy.rds`;
- generated aggregation bundle: `output/aggregated/`;
- rendered map: `divvy_od_r2500_20260105_0600_0900.png`.

The bundled HTML is self-contained and contains the displayed R code followed
by its results. The example uses a 2,500 m radius and the half-open local-time
window `[2026-01-05 06:00, 2026-01-05 09:00)` in `America/Chicago`.

No user-specific absolute paths are stored in the report or aggregation
metadata. The map background is cached locally, so rendering requires no map
tile download.
