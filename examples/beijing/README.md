# Beijing simulated entry flow

Open the [08:00 map](maps/beijing-flow.png).

Install the accompanying `bdynets` R package once. Run the commands and report blocks from this example directory. All case scripts and source samples are included here. `prepare.R` converts this case’s source data. `scripts/prepare-input.R` aligns the road and time tables and writes the prepared RDS files. `run.R` builds the report and handles exports.

The [report](beijing-traffic-report.html) follows the project simulator's original CSV and network HTML through to a Flow map. The supplied day keeps 12,287 directed pairs × 1,440 minutes and all 22 original fields. The simulator records the all-lane entry rate for each directed road in vehicles/hour, once per minute. Its time column is elapsed time since simulation midnight.

## Start from the supplied day

Run the [report's R blocks](beijing-traffic-report.Rmd) in order from a fresh session in this example directory. They use the installed `bdynets` package and the source conversion in `prepare.R`.

1. Read the 17,693,280 observations for simulation day 10 (00:00–23:59) from `data/raw/output_LinkTDMOE.csv.gz` and the directed road geometries from `data/raw/saturation_dynamic_leaflet_osm_osmgeom.html`.
2. Join by directed node pair. Save the native minute matrix and geometries as `source.rds`. Average the rates over five-minute intervals and write the 11,742 mapped roads and 288 ordered starts to the preparation tables in `input.rds`.
3. Align the observations by time and road ID in `flow-frames.rds`, the standard drawing input. Pass the full series to `traffic_flow_map()` and select the frame to draw. The report uses 08:00 by default.

The [supplied-file manifest](data/raw/source-manifest.csv) records the source files, their sizes and checksums.

## Prepare the drawing input

From this example directory, run the conversion on the supplied day. Allow at least 3 GB of free temporary space.

```sh
Rscript prepare.R prepared
```

The drawing input is `prepared/flow-frames.rds`.

The [source register](data/original-sources.csv) traces the supplied files to their original sources. The installed package’s `INPUT-FORMAT.md` documents the Flow drawing input.

## Render or export

```sh
Rscript run.R
Rscript run.R 08:00 --export
Rscript run.R 08:05 --export --no-basemap --output-dir output-0805
```

Edit `plot_time` in `beijing-traffic-report.Rmd` to change the default from
08:00. A clock argument overrides it for that run. Use an exact five-minute
start on the simulation clock, such as 08:05. Rendering uses the installed
package, rmarkdown, knitr and Pandoc. It reads the bundled OpenStreetMap
background locally by default. Use `--no-basemap` to omit it.

`run.R` rebuilds `beijing-traffic-report.html`. The report draws the selected
time once using the full prepared road extent and a narrow margin. With
`--export`, it saves the complete series under `output/prepared-data/` and
copies that figure to `output/maps/beijing-flow.png`. Use `--output-dir` to
select a new export directory.

The preparation export includes `roads.csv` (road attributes),
`road-vertices.csv` (ordered coordinates) and `flow.csv` (five-minute observations).
`data-dictionary.csv` explains their fields. `metadata.json` records the clock
and units, `source-files.csv` lists the inputs, and `checksums.csv` records the
generated files.

## Use the prepared drawing input

Draw the 08:00 frame from the full series produced by `prepare.R`.

```r
bdynets::traffic_flow_map(
  "prepared/flow-frames.rds", time_label = "08:00",
  basemap = "data/basemap-cache/beijing.geojson.gz",
  output_file = "beijing-flow.png", draw = FALSE, full_extent = TRUE
)
```

The [prepared input guide](../../bdynets/inst/INPUT-FORMAT.md) describes the
same drawing format for your own data.

In an interactive R session, run the visible report blocks in order to draw on the current graphics device. Preparation uses a temporary directory. To run from a terminal and retain the report, maps and prepared inputs, use `Rscript run.R --export`.

## Prepare a background for another extent

The [prepared input guide](../../bdynets/inst/INPUT-FORMAT.md#prepare-an-openstreetmap-background) shows how this example’s `scripts/acquire-osm.py` prepares and reuses OpenStreetMap context from your drawing data.
