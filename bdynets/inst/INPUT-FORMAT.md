# Standard plotting input

`traffic_input()` creates an R list with schema `bdynets-traffic-input-v1`. RDS preserves table types, ordered geometry and nested source metadata. The source conversion declares the measurement and interval; the plotting input does not automatically aggregate observations or infer a missing unit, timezone, route or event end.

| Component | Required fields when present | Meaning |
|---|---|---|
| `roads` | Character `road_id`; ordered numeric longitude/latitude matrices in `geometry` | Stable directed display-road IDs and supplied paths |
| `flow` | `road_id`, `time`, `flow` | Unique road-time primary value, nonnegative finite or missing |
| Road timing | `times`, `time_labels`, `interval_seconds`, `unit` | Source starts, labels, widths and actual measurement unit |
| `nodes` | Character `node_id`, `longitude`, `latitude` | Unique located OD endpoints |
| `od` | Character `origin_id`, `destination_id`, integer-valued nonnegative `flow`; optional `time`, `ride_id` | Directed counts or individual departures, joining node IDs |
| OD timing | `od_time_kind`; applicable `od_interval_seconds`, `timezone` | Individual instant, interval start, untimed or explicitly unspecified |
| `events` | `time`, `lon`, `lat`; optional ID, category, description, region and end | Reported point events with only supplied timing information |
| `measurements` | `road_id`, `time`, `metric`, `value` | Other source or derived metrics, defined in `metric_info` |
| `trends` | `time`, `metric`, `value`, definition | Supplied descriptive summaries, with their source calculation meaning |
| `provenance` | Source and preparation records | Source identities, native coverage, calibration, exclusions and transformations |

Unused components are `NULL`. Road units and OD units are separate. Source-specific attributes can accompany the common fields. Recorded zero, missing and imputed are separate states. Missing source records are not filled with zero without an established observation domain. Display links do not create sensor adjacency, causal ties or observed travel paths.

## Time and units in the four inputs

| Input | Stored observation time | Reference and interval | Measurement unit |
|---|---|---|---|
| Beijing | Numeric seconds | Simulation midnight on day 10; 300-second frame starts; calendar date and timezone unspecified | `vehicles/hour`, a mean entry rate |
| PeMS / CHP | Numeric seconds | Source date and clock encoded with UTC arithmetic; timezone unspecified; 3,600-second road frames; events retain their reported starts | `vehicles/hour`, endpoint means of station-hour values |
| City-traffic-M | Numeric seconds | Source date and clock encoded with UTC arithmetic; timezone unspecified; 900-second frame starts | `source volume`; source speed and the dimensionless derived index are defined in `metric_info` |
| Divvy | POSIXct instants | Fractional departure seconds in `America/Chicago`; `od_time_kind='instant'` and no aggregated OD interval | `od_unit='trips'`; one per retained departure |

`time_labels` preserves the supplied road clock labels. UTC arithmetic gives a reproducible numeric encoding for unspecified wall clocks; it does not identify a source UTC offset. Divvy's POSIXct values identify instants in its declared timezone. CSV timestamps are numeric seconds and their interpretation is retained in metadata.

The road measurement uses `unit`; OD counts use `od_unit`. Components that are absent have no corresponding measurement to interpret. `interval_seconds` describes the prepared road intervals, and `od_interval_seconds` describes an explicitly aggregated OD interval when present. The common format accepts the declared observation timing rather than imposing one interval on all sources.

For counts, an interval total sums complete eligible observations. For a sampled rate, a mean retains its rate unit; the source's sampling or weighting rule determines that mean. The four lessons keep their stated source methods. Zero, missing and filled observations remain identifiable. An absent OD pair is left absent where the eligible dyad and observation domain have not been established; observed self-loops remain in the OD table.

## Original files, intermediate data and the common input

The original-format teaching files are in each `data/raw` directory and are identified by `source-manifest.csv`. The source readers validate declared IDs and clocks, select the stated source window and return a source-specific intermediate representation. This is separate from the standard plotting schema.

| Case | Source reader | Intermediate representation |
|---|---|---|
| Beijing | `read_beijing_raw()` | `beijing-native-minute-v1`: directed pairs, source minutes, minute-rate matrix, geometry and source field map |
| PeMS | `read_pems_archives()` | `pems-station-events-v1`: original hourly position values and station map, five-minute station counts, CHP summaries, explicit display references and requested hours |
| City-traffic-M | `read_metropolis_raw()` / `read_parquet.py` | `metropolis-native-v1`: source IDs/static rows, unrounded five-minute volume/speed, historical calibration, source clocks and file facts |
| Divvy | `read_divvy_source()` / `extract_divvy_day()` | Original-column day CSV: every selected source row and all 13 original fields, before station/OD processing |

Each road report saves the imported native object into a new temporary `source.rds`, reads it back exactly, and then constructs the standard tables. Divvy persists the selected original-column day CSV after explicitly removing trailing ASCII spaces from the two mapped station-name columns. It checks every other field against the selected monthly rows and verifies the cleaned CSV by exact typed readback. The complete supplied `data/source.rds` snapshots and `data/input.rds` objects remain read-only references during reports. Small source windows are checked against matching records in the supplied day.

The raw fixture manifests identify ordinary source excerpts separately from derived reference inputs, such as a supplied road register or full-selection calibration. Original-file commands reconstruct the existing full source scope; the teaching workflow does not resample roads or choose a new time aggregation.

## Source conversion and constructor

The supplied case methods are in their `prepare.R`. They read named source columns, validate keys and clocks, calculate the established case measurements, and call `traffic_input()`. The reports show actual raw rows, those calculations, output rows and the standard object's structure. The mapping values identify source headers; the role names identify the common meaning. The executed remapped-CSV export records this correspondence as the named `provenance$column_mapping` object in both RDS and JSON metadata.

For an already prepared road table and its time-value table, the constructor call has the following form using objects from the supplied input:

```r
input <- readRDS('data/input.rds')
standard <- traffic_input(
  roads=input$roads, flow=input$flow, events=input$events,
  times=input$times, time_labels=input$time_labels,
  interval_seconds=input$interval_seconds, unit=input$unit,
  measurements=input$measurements, metric_info=input$metric_info,
  trends=input$trends, provenance=input$provenance)
```

The OD form is `traffic_input(nodes=input$nodes, od=input$od, timezone=input$timezone, od_time_kind=input$od_time_kind, od_interval_seconds=input$od_interval_seconds, provenance=input$provenance)`. Individual departure data retain ride IDs and fractional source seconds. Optional interval metadata describes an already established aggregation; creating a standard object does not itself choose an aggregation.

The recommended static entry point is `traffic_map(input)`, where `input` is the standard object or its RDS path. The same interface handles road, OD and mixed tables. `read_traffic()` remains available for lower-level road workflows, and `traffic_map_html()` writes the earlier interactive road HTML output. The static road display uses seven log1p classes in green–yellow–red; OD uses the Divvy five trip-count classes. Source conversion and drawing retain separate responsibilities.

```r
road <- readRDS('examples/beijing/data/input.rds')
traffic_map(road, index=1, basemap=NULL)
traffic_map('examples/DivvyBike/data/input.rds', basemap=NULL)
```

In mixed inputs with absolute road times and timed OD, the selected road frame supplies a half-open OD selection window unless an explicit window is given. This selects existing records and leaves the original intervals unchanged. Untimed OD stays an explicitly untimed aggregate overlay; no road timestamp is attached to its records. An explicit timestamp filter on untimed OD stops with an explanatory error. A road frame with no matching OD records still draws its road data and reports OD status `no_records`, `selected_records=0` and flow totals `NA`. It does not add zero-flow records or infer an eligible OD observation domain.

Without roads, a combined OD/event view derives one extent from the selected OD stations and events unless explicit bounds are supplied. Data outside explicit bounds are clipped. An empty OD selection has status `no_records`, zero selected records and unknown (`NA`) flow totals; supplied station locations provide reference context. Recorded zero flow remains zero, and a selection with no drawable nonloop routes has status `no_routes`. No missing OD records or timestamps are created. Invalid identifiers, coordinates, timestamps and style parameters still produce errors. Numeric source-clock events may be shown without temporal alignment; absolute timestamp filtering requires explicitly aligned event timestamps.

Automatic context uses an embedded basemap when one is present, then a matching verified cache or a network request when required. `basemap=NULL` draws data without background streets. `layers='auto'` is the supported default for the supplied examples. The existing `layers` argument selects the principal road/OD content; independent selective node/event rendering is not established by these lessons.

## Safe export and readback

`write_traffic_input(input, 'new-prepared-directory', source_files='original.csv')` exports an existing standard object. The destination must be new. It publishes `input.rds`, readable tables, `road-vertices.csv` when applicable, a field dictionary, validation records, source-file SHA-256 values, metadata and a checksum manifest. Source files are read only; declared source directories also protect descendants.

CSV numeric values use 17 significant digits. Identifiers stay character, boolean fields stay logical, and POSIXct timestamps use numeric Unix seconds with timezone recorded separately. The missing token is defined in `metadata.json`. Typed `data.table::fread()` readback in each report verifies every applicable table and ordered geometry. RDS readback compares the complete object fields exactly.

## Relation to the existing network interfaces

| Interface | Responsibility | Source information needed |
|---|---|---|
| `traffic_input()` | Geographic road, event and OD tables with their timing and measurement metadata | Supplied geometry or located endpoints and explicit observation meanings |
| `network_data()` | Source nodes and directed/undirected edges | Unique `node_id`; unique `edge_id` and explicit `from`/`to` node references |
| `network_panel()` | Numeric time-by-unit observations linked to that network | Ordered times, stable node/edge unit IDs, values, units and aggregation meaning |

The [Beijing report](../../examples/beijing/beijing-traffic-report.html#the-shared-network-and-panel-interfaces) executes the conversion on the actual tiny source fixture. `road_id` becomes `edge_id`, the supplied `from_node_id` and `to_node_id` define the endpoints, and `flow` becomes the time-by-edge values matrix. `panel_long()` confirms that IDs, source times, values and observed zeros agree. The source road geometry remains in the geographic input.

An ID and a polyline alone do not establish the source's node/edge topology. PeMS display links retain their stated endpoint-estimation meaning; they do not establish detector adjacency. Station OD counts describe ordered trips. An edge-panel representation needs an explicit source network and observation domain appropriate to that task. These interfaces preserve preparation data; none of the lessons fits a model.

