# Generic OD aggregation and mapping with Divvy

This example demonstrates two exported `bdynets` functions:

- `aggregate_od_data()` validates and aggregates spatial OD data;
- `traffic_od_map()` reuses the basemap method in `trafficmap.R` and draws
  directed flow arrows and node symbols in one call.

Both functions use explicit column mappings. Data providers may use different
column names or include unrelated columns without requiring a new algorithm.

From the repository root:

```bash
R CMD INSTALL bdynets
Rscript -e 'rmarkdown::render("examples/DivvyBike/divvy-od-map.Rmd")'
Rscript examples/DivvyBike/run.R /tmp/new-divvy-output
```

Required example resources:

- `data/nodes_r2000.csv`;
- `data/od_flow_2026_01_dt01h_r2000.csv`;
- `data/chicago_streets_divvy.rds`.

The report reproduces the supplied 2 km aggregation and pools clock hours
`00:00-03:00` across every January 2026 date. It displays 53 active nodes,
206 directed nonloop OD pairs, and 969 nonloop trips. Fixed flow classes are
`1`, `2-3`, `4-5`, `6-7`, and `>= 8`. Each code block is followed by its
result, the report has no table of contents, and it stores no
contributor-specific absolute path.
