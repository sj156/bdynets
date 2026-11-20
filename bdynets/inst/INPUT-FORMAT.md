# Prepared inputs for Flow and OD maps

Flow and OD have separate map inputs and plotting functions. Example preparation determines the observations, values, and time scope before drawing. Use the bundled package and run these examples from the bundle root.

| Map | Direct input | Bundled example |
|---|---|---|
| Flow | A single-frame `trafficmap_data` object | `examples/beijing/data/map-frame-0800.rds`, also supplied for PeMS and City-traffic-M |
| OD | A prepared `bdynets_od_pairs` object | `examples/DivvyBike/data/spatial-1500m/window-0000-0300.rds` |

```r
source("bdynets/inst/preparation-session.R")
preparation_session("examples/beijing")
library(bdynets)
frame <- readRDS("examples/beijing/data/map-frame-0800.rds")
traffic_flow_map(frame, basemap = NULL)

pairs <- readRDS("examples/DivvyBike/data/spatial-1500m/window-0000-0300.rds")
traffic_od_map(pairs, basemap = NULL)
```

## Flow map input

The frame contains the directed road IDs and longitude–latitude geometry, one row of aligned road values, a frame start in `times`, its `interval_seconds`, the measurement unit, and source provenance. Optional event starts and additional road metrics belong to that frame. A row value of zero is an observation and `NA` is missing.

Each road example prepares a frame for the map and uses the complete day for trends. The saved frame retains the actual time and a display format in `time_label_format`. The map generates the label from those fields when drawn. Beijing uses simulation-day seconds. PeMS and City-traffic-M retain their source clock encodings, with the source timezone unspecified where appropriate.

The three road examples publish the default 08:00 frame as `data/map-frame-0800.rds`. A report export also writes `output/figures/map-input.rds` for the selected frame. The existing `selected-frame.rds` is an export snapshot for inspection. The PeMS ready frame includes both regions. Pass `region = "Los Angeles Area"` or `region = "San Francisco Bay Area"` to reproduce one regional view.

Beijing prepares five-minute means of minute-sampled hourly entry rates in vehicles/hour. PeMS uses the existing hourly station and endpoint means and includes reported CHP starts. City-traffic-M uses fifteen-minute source volume, additional speed and derived index values, and the existing native coverage information.

## OD map input

The ready object contains `nodes` with the stations or groups to draw and `routes` with one row per drawn directed pair. Each route includes `origin_id`, `destination_id`, both endpoint coordinates, and the prepared trip count in `flow`. It also carries the grouped node register, directed totals and within-group count. Divvy supplies the actual start and end, timezone, unit, grouping radius and provenance.

Apply the time window, group assignments, route limits, loop policy and field mappings during preparation. The prepared input can be drawn with a basemap, bounds, map text and style settings. Passing a time window, route limit, loop or active-node option, or node/flow mapping alongside an already prepared object raises an error. For example, change the selection in Divvy's `prepare.R` and read the resulting RDS to draw another window.

The original station-level `data/input.rds` retains individual Chicago departures. The separate 1.5 km spatial derivative keeps the full day at grouped station resolution. Within-group trips are retained in the totals. An omitted observation remains missing unless the source supports an observed zero. Curved arrows identify ordered endpoints rather than observed travel paths.

## Compatibility and full-day working inputs

The road day files `data/input.rds` contain `roads` and a long `flow` table with `road_id`, `time` and `flow`, plus ordered frame times, intervals, units and any event or metric tables. Use `read_traffic()` for a full-day plotting view and trends. Published road day files carry `bdynets-traffic-input-v1`. `read_traffic()` and `traffic_flow_map()` also accept the older untagged named road lists. Compatibility calls can select a frame from the full day with `index` or `time_label`.

The legacy OD call still accepts station and directed flow tables (`nodes` and `flows`) or their supported adapters. That route can filter and aggregate the supplied records. The ready `bdynets_od_pairs` form is the direct map input used by the Divvy example.

Use `traffic_map_html()` to export an interactive road map.

## Original files and intermediate data

| Example | Original reader | Intermediate data |
|---|---|---|
| Beijing | `read_beijing_raw()` | Native directed pairs, minute-rate matrix, source clock, road geometry and source fields, saved as `source.rds`. |
| PeMS | `read_pems_archives()` | Native hourly positions, five-minute detector counts, station mapping, CHP summaries and display references, saved as `source.rds`. |
| City-traffic-M | `read_metropolis_raw()` and `read_parquet.py` | Native five-minute volume/speed, static road rows, calibration, source summaries and coverage, saved as `source.rds`. |
| Divvy | `read_divvy_source()` and `extract_divvy_day()` | Selected-day CSV with all 13 original fields. Only the stated trailing station-name spaces are removed. |

The reports execute original-format import, save/read intermediate samples in temporary locations, show the road or OD conversion and compare it with the supplied day. The original CSV/Parquet files, intermediate full-day snapshots and source identity records remain read only during rendering.

## Export and readback

`write_traffic_input()` publishes the working preparation tables to a new directory. It accepts the full-day Flow `roads` and `flow` tables or the OD `nodes` and `flows` tables, with their associated metadata. Save the ready single-frame or prepared-pairs object with `saveRDS()` for direct plotting.

The export contains `input.rds`, applicable CSV tables, a dictionary, validation records, metadata, source identities and checksums. Road exports contain `roads.csv`, ordered `road-vertices.csv` and `flow.csv`, with optional event and metric tables. OD exports contain `nodes.csv` and `flows.csv`. Metadata records `input_type` as `road` or `od` and retains the relevant timing and units. Native snapshots can be exported separately as `source.rds`.

RDS retains R types and nested source metadata. CSV identifiers must be read as character, double values use 17 significant digits, and POSIXct timestamps are written as numeric Unix seconds with timezone in metadata. The missing token is declared in `metadata.json`. The reports verify values, IDs, time precision, missingness, road vertex order and grouping membership as applicable. Existing output directories and protected source paths are rejected.

## Existing network interfaces

The [Beijing report](../../examples/beijing/beijing-traffic-report.html#the-shared-network-and-panel-interfaces) also demonstrates `network_data()` and `network_panel()` using explicit source endpoint IDs. A road ID and polyline alone do not define an underlying network topology. These interfaces remain separate from road and OD plotting inputs, and their processing examples do not fit models.
