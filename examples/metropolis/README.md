# City-traffic-M source method

Open the [08:00 map](maps/metropolis-flow.png).

Install the accompanying `bdynets` R package once. Run the commands and report blocks from this example directory. All case scripts and source samples are included here. `prepare.R` converts this case’s source data. `scripts/prepare-input.R` aligns the road and time tables and writes the prepared RDS files. `run.R` builds the report and handles exports.

The report converts original-format City Traffic Benchmarks v4 Parquet records for Yekaterinburg into a prepared Flow series of 96 frames. The visible R blocks run in order from a fresh session in this directory and trace road 21263 from five-minute observations to its 08:00 map value. [City-traffic-M report](metropolis-traffic-report.html)

## Prepare the source and map input

Use the two Parquets in `data/raw/`. The static file gives the original road attributes and coordinates. The supplied volume file keeps all source columns for the 288 timestamps on 18 October 2024. The reader checks the date and uses every timestamp. The supplied `data/selected-roads.csv` carries the fixed 720-road list from the original Yekaterinburg case. A `road_id` gives a zero-based static row and the suffix of the matching `node_<id>` column. [Supplied-file manifest](data/raw/source-manifest.csv)

Preparation requires the static and volume Parquets and complete five-minute
volume observations for the selected roads. It preserves observed zeros and
produces quarter-hour means in source volume units. The `trends` table in
`input.rds` averages the unrounded quarter-hour values across the selected
roads, then rounds each mean to two decimals. The report’s chart averages the
two-decimal road values used for the map. Run the following command in this
example directory and choose a new output directory.

```sh
Rscript prepare.R prepared
```

Preparation uses Python 3 with NumPy, pandas and PyArrow. Preparation writes `source.rds`, the tables in `input.rds`, the aligned `flow-frames.rds`, readable tables and geometry, provenance, and checksums. The [upstream register](data/original-sources.csv) is a provenance record. Its volume-file size describes the upstream file. The supplied-file manifest describes what this example reads.

## Render the report and map

Run these commands from this example directory. Rendering uses the installed `bdynets` package, its R dependencies, rmarkdown, knitr, Pandoc, and the Python libraries listed above.

```sh
Rscript run.R
Rscript run.R --no-basemap
Rscript run.R 17:00
Rscript run.R 08:00 --export
```

Edit `plot_time` in `metropolis-traffic-report.Rmd` to change the default from
08:00. A clock argument overrides it for that run. Use an exact quarter-hour
start on the source clock. The report uses the bundled OpenStreetMap
background locally by default. Use `--no-basemap` to omit it.

`run.R` rebuilds `metropolis-traffic-report.html`. The report draws the selected
time once using the full prepared road extent and a narrow margin. With
`--export`, it saves `source.rds`, `input.rds` and `flow-frames.rds` under
`output/prepared-data/` and copies the map to `output/maps/metropolis-flow.png`.
Use `--output-dir` to select a new export directory.

## Use the prepared drawing input

Draw the 08:00 frame from the full series produced by `prepare.R`.

```r
bdynets::traffic_flow_map(
  "prepared/flow-frames.rds", time_label = "08:00",
  basemap = "data/basemap-cache/yekaterinburg.geojson.gz",
  output_file = "metropolis-flow.png", draw = FALSE, full_extent = TRUE
)
```

The [prepared input guide](../../bdynets/inst/INPUT-FORMAT.md) describes the
same drawing format for your own data.

In an interactive R session, run the visible report blocks in order to draw on the current graphics device. Preparation uses a temporary directory. To run from a terminal and retain the report, maps and prepared inputs, use `Rscript run.R --export`.

## Prepare a background for another extent

The [prepared input guide](../../bdynets/inst/INPUT-FORMAT.md#prepare-an-openstreetmap-background) shows how this example’s `scripts/acquire-osm.py` prepares and reuses OpenStreetMap context from your drawing data.
