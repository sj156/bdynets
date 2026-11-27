# Divvy individual station departures

Open the [00:00–03:00 map](maps/divvy-od.png) of Chicago departures.

Install the accompanying `bdynets` R package once. Run the commands and report blocks from this example directory. All case scripts and source samples are included here. `prepare.R` converts this case’s source data. `scripts/prepare-input.R` assembles the OD pairs and writes the station and group tables. `run.R` builds the report and handles exports.

The report uses `data/raw/202601-divvy-tripdata.csv`, the supplied file of 1,950 departures on 1 January 2026, for its worked ride. The file keeps its supplied name and column layout. The report follows one trip through the station-level OD input, the 1,500 m grouping and its arrow in the default 00:00–03:00 Chicago-time map. The prepared drawing input includes all eight three-hour windows. Run the displayed blocks in order from this example directory. [Divvy station departures report](divvy-od-report.html)

## Clean the supplied day and build the station input

The report and standalone preparation read every row in the supplied one-day CSV. Both use `prepare_divvy_day()` to check that all departures are on the requested day, remove trailing ASCII spaces from the two station-name columns, and deduplicate exact rows after cleaning. The bundled day has 13 origin name changes and 11 destination name changes. The other source values are preserved, including fractional seconds in the departure clocks. The input file and its checksum are recorded in `data/raw/source-manifest.csv`.

`divvy_fields` maps the source roles. Station conversion uses the median coordinates of valid endpoint pairs and the most frequent nonblank recorded name for each station. It retains 1,230 departures across 512 located stations, including 85 within-station rides, and records exclusions by missing endpoint. In the saved station RDS, each departure's `origin_id` and `destination_id` join `nodes$node_id`.

## Group the whole departure day

`prepare_spatial_example()` groups the retained departures within a 1,500 m station-to-representative bound. Its 62 groups retain all 1,230 trips, including 435 within-group trips. Each group uses the observed coordinates of its representative. Grouped departures retain their source timestamps, and provenance records the membership of every station. The grouped day is written as `spatial-1500m/input.rds` inside the new preparation or report export directory.

## Prepare the input to the map

The report aggregates departures within each local half-open interval
`[start, end)`. It sums trips by directed pair, attaches representative
coordinates, and counts trips within a group separately. Each prepared
frame stores its own local bounds and source provenance.

The report applies the same aggregation to each three-hour window, retaining any empty intervals. Together the eight frames account for all 1,230 trips, including 435 within groups. The 00:00–03:00 frame shows 201 cross-group trips on 129 directed routes. It also contains 87 within-group trips. The OD function calculates colour classes from positive drawable counts across the complete input. Curves show direction between endpoints.

## Reports and prepared output

`run.R` renders `divvy-od-report.html` from `divvy-od-report.Rmd`. VS Code **Run Code** on `run.R` invokes the same entry point as `Rscript run.R`. Dependencies are R, the package imports, `rmarkdown`, `knitr` and Pandoc.

```sh
Rscript run.R
Rscript run.R --no-basemap
Rscript run.R 00:00 --export
```

Run `Rscript prepare.R prepared-day` to prepare the stations, grouped day and all eight three-hour map inputs from the supplied original-format trip CSV without rendering the report. The ordered list is `prepared-day/spatial-1500m/od-frames.rds`. Supply both `--start` and `--end` to write a one-frame list at the same path, including when that window has no trips.

For another one-day CSV with the same Divvy source columns and Chicago local timestamp format, run `Rscript prepare.R --raw TRIP_CSV prepared-day`. Use `--day YYYY-MM-DD` to declare the date in that file. The reader expects every departure to be on that date.

Edit `plot_time` in `divvy-od-report.Rmd` to change the default from 00:00
Chicago time. A clock argument overrides it for that run. Use the exact start
of a prepared window, such as 03:00. The report and `prepare.R` share the
default duration from `divvy_report_windows()`. To use a different duration
in the report, pass `window_minutes` to that function in the preparation
block and choose a start on the new grid.

The report draws one portrait map focused on the nodes in the selected
window. Its trip-count scale uses all prepared windows. The locally
bundled background covers all 62 station groups and contains streets, water
and place labels. Use `--no-basemap` to omit it.
[Chicago GeoJSON](data/basemap-cache/chicago.geojson.gz)

The two preparation directories each contain `nodes.csv`, `flows.csv`, a
field dictionary, metadata, a source inventory and checksums. The grouped
`input.rds` records station-to-representative membership under
`provenance$spatial_aggregation$membership`.

With `--export`, `run.R` saves the complete ordered drawing input at
`output/prepared-data/od-frames.rds`, the station and grouped data under
`output/prepared-data/`, and copies the report’s map to `output/maps/divvy-od.png`. The export includes source inventories. The original trip CSV stays in `data/raw/`.
Use `--output-dir` to select a new export directory.

## Use the prepared drawing input

This direct API call draws the 00:00–03:00 window from the complete input
generated by `prepare.R`. It uses the standard drawing dimensions and the
shared extent across all prepared windows. Pass `extent = "frame"` to focus
the extent on the selected window while keeping the complete series’ colour
scale. Use `Rscript run.R 00:00 --export` for the report’s portrait layout
and labels.

```r
bdynets::traffic_od_map(
  "prepared-day/spatial-1500m/od-frames.rds", time_label = "00:00",
  basemap = "data/basemap-cache/chicago.geojson.gz",
  output_file = "divvy-od.png", draw = FALSE
)
```

The [prepared input guide](../../bdynets/inst/INPUT-FORMAT.md) describes the
same drawing format for OD data from any source.

In an interactive R session, run the visible report blocks in order to draw on the current graphics device. Preparation uses a temporary directory. To run from a terminal and retain the report, maps and prepared inputs, use `Rscript run.R --export`.

## Prepare a background for another extent

The [prepared input guide](../../bdynets/inst/INPUT-FORMAT.md#prepare-an-openstreetmap-background) shows how this example’s `scripts/acquire-osm.py` prepares and reuses OpenStreetMap context from your drawing data.
