# Prepared inputs for Flow, Speed and OD maps

These examples use the installed `bdynets` package. If you have the handover
folder, run `R CMD INSTALL bdynets` from its root after installing the package
dependencies listed in `bdynets/DESCRIPTION`. The handover’s `bdynets/README.md`
gives the installation requirements.

Supply the complete prepared series and choose one frame to draw. Choose how to
interpret source fields, group records, set intervals and handle missing source
values during preparation. The handover’s five example reports show how those
choices were made for their own data. In an installed R session, see
`help("traffic_flow_map", package = "bdynets")`, `help("traffic_speed_map", package = "bdynets")`
or `help("traffic_od_map", package = "bdynets")` for function arguments.

| Map | Drawing object or RDS containing it | Drawing function |
|---|---|---|
| Flow | `trafficmap_data` with `input_type = "flow"` | `traffic_flow_map()` |
| Speed | `trafficmap_data` with `input_type = "speed"` | `traffic_speed_map()` |
| OD | Ordered, nonempty list of `bdynets_od_pairs` frames, or one such frame | `traffic_od_map()` |

The file names below are conventions used by the examples. An RDS made with
`saveRDS()` can have any name. The case exports called `input.rds` contain
intermediate tables for inspecting preparation. The `*-frames.rds` files hold
the objects in the table above.

Choose one selector, `time_label` or a one-based `index`. With `time_label`,
enter the start of the prepared frame or OD window. For a minute-aligned
series, use `"08:00"` if that start occurs once. For an absolute clock with
repeated clock times, use a date and time such as `"2026-01-01 08:00"`. For
starts with nonzero seconds, include seconds and any fractional part. For
example, enter `"08:00:30"` for a start 30 seconds past 08:00. An explicit
UTC offset can distinguish repeated local times. For example, use
`"2026-11-01 01:30 -0500"` to select the occurrence with that offset. Use the
index whenever a time label is still ambiguous. Select OD windows by
their prepared starts, such as `"08:00"` for the interval 08:00–09:00.

Use `output_file` ending in `.png` or `.pdf` to save the result. Set
`draw = FALSE` to skip drawing on the current R graphics device when saving
a file. PDF output uses Quartz on supported macOS builds, Cairo where it is
available, or R’s standard PDF device. Chinese and other non-Latin labels
depend on the fonts available to that device.

## Flow and Speed drawing objects

A prepared `trafficmap_data` object has these fields.

| Field | Contents |
|---|---|
| `input_type` | `"flow"` or `"speed"`, matching the drawing function. |
| `unit` | The supplied Flow unit, or `"mph"` or `"km/h"` for Speed. |
| `roads` | A table with unique character `road_id` values and a `geometry` list-column. Each geometry is a numeric matrix of at least two WGS84 vertices in travel order, with longitude in column 1 and latitude in column 2. The optional `region` column holds the names of regional views as character values or a factor. |
| `values` | A numeric matrix with one column per road in table order and one row per time. Use nonnegative finite values and `NA` for missing cells. |
| `times` | A finite, strictly increasing **numeric** vector of frame starts in seconds. |
| `interval_seconds` | A numeric vector of one positive duration in seconds per frame. The intervals do not overlap. |
| `clock` | `"source_clock"` for elapsed source seconds or encoded source wall-clock times, or `"absolute"` for Unix timestamps that represent actual instants. Convert parsed absolute times with `as.numeric()` during preparation. |

Choose the clock to match the numeric values in `times`:

- For elapsed seconds, use `clock = "source_clock"` with the default
  `time_label_format`. Select by elapsed hours, including values such as
  `"30:00"` if the series runs past 24 hours.
- For absolute timestamps, use `clock = "absolute"`. Set `timezone` to an IANA
  name such as `"America/Chicago"` for local clock display and selection. The
  default is UTC. An optional `time_label_format` changes how the map prints
  each time.
- PeMS and City-traffic-M keep source wall-clock times as numeric keys encoded
  in UTC. They use `clock = "source_clock"` and a `time_label_format` such as
  `"%Y-%m-%d %H:%M"` to display and select by the source calendar clock.

Use the actual frame start even when the printed label shows fewer digits.

You can omit `events` entirely. If you supply events, use a table with
unique `id`, numeric `time` on the same clock, WGS84 `lon` and `lat`, and an integer
`frame` pointing to the prepared frame (one-based). An event belongs to the
prepared interval that includes its start and excludes its end. The static
map marks event locations with a symbol. An optional `region` column assigns
events to regional views. It can be a character column or a factor. Missing
and empty region values leave events unassigned. Keep event categories and
descriptions in the prepared table if you need them for your own summaries.

`title` and `description` are optional strings. The map supplies its own title
and default measurement label from `input_type`. With no explicit frame
selection, the default is the first frame or the prepared object’s `initial`
index. Choosing a frame for drawing leaves the full prepared series available.

The blocks below start with values and geometry that are already prepared. Run
them in an R session with the package installed, in a working directory where
you want to save your RDS and map figures. The handover’s `examples/README.md`
links to the five source-to-drawing reports.

The prepared road object also works with `traffic_report()` for a static HTML
report. By default, each road map fits every road vertex and event location in
the selected area. Use `full_extent = FALSE` to focus on the main cluster.
For both road functions, `padding` controls the extra space at each edge as a
fraction of the larger projected span. The default is `0.08`. Frame statistics
include all records in the selected area regardless of the view. Pass the
object to `traffic_trend()` to summarise its frames.

### Flow

The package divides the range from zero to the maximum across the complete
series into seven equally spaced classes on the `log1p` scale.

```r
library(bdynets)

roads <- data.frame(road_id = "A")
roads$geometry <- list(rbind(c(-87.65, 41.88), c(-87.63, 41.89)))
flow <- structure(list(input_type = "flow", unit = "vehicles/hour",
  roads = roads, values = matrix(c(0, NA, 24), ncol = 1L),
  times = c(0, 300, 600), interval_seconds = rep(300, 3),
  clock = "source_clock"), class = "trafficmap_data")
flow_file <- "flow-frames.rds"
saveRDS(flow, flow_file)
traffic_flow_map(flow_file, time_label = "00:10",
  output_file = "flow.png", draw = FALSE)
```

### Speed

The package uses fixed boundaries at 35, 40, 45, 50, 55 and 60 mph. In km/h the
boundaries are the same physical speeds multiplied by 1.609344.

```r
library(bdynets)

roads <- data.frame(road_id = "A")
roads$geometry <- list(rbind(c(-87.65, 41.88), c(-87.63, 41.89)))
speed <- structure(list(input_type = "speed", unit = "km/h",
  roads = roads, values = matrix(c(0, NA, 72), ncol = 1L),
  times = c(0, 300, 600), interval_seconds = rep(300, 3),
  clock = "source_clock"), class = "trafficmap_data")
speed_file <- "speed-frames.rds"
saveRDS(speed, speed_file)
traffic_speed_map(speed_file, time_label = "00:10",
  output_file = "speed.png", draw = FALSE)
```

## OD drawing object

Each prepared frame carries `nodes` for the nodes shown in that frame and
`all_nodes` for the locations used to set the extent. Both tables use
`node_id`, `longitude` and `latitude`. Coordinates are WGS84.

The `routes` table uses `origin_id`, `destination_id`, `origin_longitude`,
`origin_latitude`, `destination_longitude`, `destination_latitude` and
`flow`. Include one row per displayed pair of distinct node IDs with a
positive integer trip count. Keep these columns even when a frame has no
routes. Zero counts and trips within one node do not represent arrows.

Each frame also supplies `selected_flow` (total trips), `within_node_flow`
(trips within one node), `start_time`, `end_time` and the display
`timezone`. Keep every window you want in the map, in display order.

- If records are present but their total is zero, leave `routes` empty, set
  `selected_flow = 0` and `within_node_flow = 0`, and set `selected_records`
  to the positive number of records. The map displays “0 trips in this window.”
- If no records are present, keep an empty `routes` table and set
  `selected_records = 0`, `selected_flow = 0`, `within_node_flow = 0` and
  `status = "no_records"`. Keep the study locations in `all_nodes` so an
  empty window can also be drawn independently. The map displays “No records
  in this window.”

The default colour classes use the positive counts on drawable routes across
the complete supplied series. They use up to seven `log1p` classes. Drawing a
second window from the same input uses the same scale. To set common thresholds
across separate inputs, pass your own breaks in `flow_style`, for example
`flow_style = list(breaks = c(10, 50, 100))`.

By default, the OD map uses the study locations in `all_nodes` for one prepared
window or their union across all supplied windows. Pass `extent = "frame"` to
focus the map on the selected window. The extent includes the drawn routes, nodes and event
locations, and falls back to study locations when the window has none.
Explicit `bounds` take precedence. The scale still uses the complete supplied
series.

The following example starts with already prepared nodes and counts for two
five-minute windows and uses the declared timestamps to select the second
window. Preparation determines any grouping, window boundaries and the handling
of trips within one node.

```r
library(bdynets)

nodes <- data.frame(node_id = c("A", "B"),
  longitude = c(-87.65, -87.63), latitude = c(41.88, 41.89))
route <- data.frame(origin_id = "A", destination_id = "B",
  origin_longitude = -87.65, origin_latitude = 41.88,
  destination_longitude = -87.63, destination_latitude = 41.89,
  flow = 3L)
zone <- "America/Chicago"
start <- as.POSIXct("2026-01-01 08:00:00", tz = zone)
frame <- structure(list(nodes = nodes, all_nodes = nodes, routes = route,
  start_time = start, end_time = start + 300, timezone = zone, unit = "trips",
  selected_flow = 3L, selected_records = 1L, within_node_flow = 0L,
  active_pairs = 1L, status = "ok"), class = "bdynets_od_pairs")
next_frame <- frame
next_frame$start_time <- start + 300
next_frame$end_time <- start + 600
next_frame$routes$flow <- 120L
next_frame$selected_flow <- 120L
frames <- list(frame, next_frame)
od_file <- "od-frames.rds"
saveRDS(frames, od_file)
traffic_od_map(od_file, time_label = "08:05",
  output_file = "od.png", draw = FALSE)
```

Distinct node IDs sharing coordinates retain their directed counts and are
shown with a ring at the shared location.

## Prepare an OpenStreetMap background

The supplied maps use local GeoJSON backgrounds with roads, water and place
names from OpenStreetMap via OpenFreeMap. For new data, prepare the background
once. This is optional. The three drawing functions accept a local GeoJSON or
gzipped GeoJSON path as `basemap`. They use the local file during drawing.
Supply a WGS84 FeatureCollection with two-dimensional `Point`, `LineString`
or `Polygon` geometry. Split multipart geometries into individual features
when preparing a custom background.

**1. Derive the extents in R.** Use the directory containing your prepared
RDS as your working directory for all three steps. Choose one of the blocks
below and replace its RDS filename with yours. Coordinates use WGS84
longitude and latitude within Web Mercator limits (about ±85.05113° latitude).

For Flow or Speed, `traffic_road_extents()` prepares the area that will actually
be visible on the static map, including the space added to preserve the map's
aspect ratio. Its defaults match the drawing functions. If you draw with a
different `width`, `height`, `padding` or `full_extent`, pass the same values
when calculating the extents.

With its default settings, the function returns one extent per named region
and an additional one for any unassigned roads or events. Use these extents
with separate regional maps. To prepare for one named region, pass the same
`region` to the extent and drawing functions. For one map spanning all regions,
set `split_regions = FALSE` here and draw with `region = NULL`. Data without
region labels produce one extent either way. The JSON file contains one
`[west, east, south, north]` array per extent. See
`help("traffic_road_extents", package = "bdynets")` for its arguments.

```r
# Flow
flow_file <- "flow-frames.rds"
extents <- bdynets::traffic_road_extents(flow_file)
jsonlite::write_json(lapply(extents, unname), "extents.json", digits = NA)
```

```r
# Speed
speed_file <- "speed-frames.rds"
extents <- bdynets::traffic_road_extents(speed_file)
jsonlite::write_json(lapply(extents, unname), "extents.json", digits = NA)
```

For OD, use the study locations in `all_nodes` from every frame. The RDS may
contain a single prepared frame or an ordered list. The block writes a shared
extent for the default `extent = "series"` map with its usual 11 by 9 inch
output. Match the `width`, `height` and `padding` used to draw if you customize
them. If you supply explicit map `bounds`, pass the same bounds when calculating
the background coverage.

```r
# OD
od_file <- "od-frames.rds"
od <- readRDS(od_file)
if (inherits(od, "bdynets_od_pairs")) od <- list(od)
coords <- do.call(rbind, lapply(od, function(frame) {
  frame$all_nodes[c("longitude", "latitude")]
}))
extent <- bdynets::traffic_map_extent(coords, width = 11, height = 9)
jsonlite::write_json(list(unname(extent)), "extents.json", digits = NA)
```

**2. Prepare or reuse the cache in a terminal.** Change to the same directory
you used in R. Replace the example paths below with the locations of your data
directory and the script in your copy of the handover. Each case includes
the same `scripts/acquire-osm.py`, which works for all three map types. The
script chooses a tile zoom for each extent. It reuses a readable cache that covers that extent at the
required detail and downloads only the extents without a usable cache. When
several files are needed, it combines their features in a single GeoJSON for
drawing. The source caches stay available for other runs. The
`--path-file` option writes the absolute GeoJSON path to `background-path.txt`
after preparation succeeds. Use a text file path separate from the extent
JSON and the GeoJSON cache. The script rejects the extent input and names
ending in `.geojson` or `.geojson.gz` as pointer paths. If preparation fails,
an existing path file stays available.

```sh
cd "/absolute/path/to/your/data"
python3 -m pip install mapbox-vector-tile==2.2.0
python3 "/absolute/path/to/handover/examples/DivvyBike/scripts/acquire-osm.py" extents.json basemap-cache --path-file background-path.txt
```

**3. Draw in R with the local file.** Use the same data directory in R and
run the block for your type. Use the RDS filename from step 1. Pass the path
read from `background-path.txt` as `basemap`. To draw without a background,
pass `basemap = NULL`. These calls select the first prepared frame. For a
regional road map, also pass the region you prepared. For a combined road map,
prepare the combined extent in step 1.

```r
# Flow
flow_file <- "flow-frames.rds"
background <- readLines("background-path.txt", n = 1L)
bdynets::traffic_flow_map(flow_file, index = 1L, basemap = background,
  output_file = "flow.png", draw = FALSE)
```

```r
# Speed
speed_file <- "speed-frames.rds"
background <- readLines("background-path.txt", n = 1L)
bdynets::traffic_speed_map(speed_file, index = 1L, basemap = background,
  output_file = "speed.png", draw = FALSE)
```

```r
# OD
od_file <- "od-frames.rds"
background <- readLines("background-path.txt", n = 1L)
bdynets::traffic_od_map(od_file, index = 1L, basemap = background,
  output_file = "od.png", draw = FALSE)
```

## Case exports

The handover’s `examples/README.md` links to the five source walkthroughs.
The reports and source samples are part of the handover folder. Each `.Rmd`
sets a default `plot_time` that the CLI clock argument can override. The
report export saves the full drawing RDS and copies the PNG figures drawn
for the report. Each case documents its own intermediate files and working
directory.
