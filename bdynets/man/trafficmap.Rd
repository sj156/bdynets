\name{traffic_data}
\alias{extract_traffic_data}
\alias{traffic_data}
\alias{read_traffic}
\alias{flow_scale}
\alias{traffic_summary}
\alias{traffic_trend}
\alias{traffic_map}
\alias{traffic_report}
\alias{traffic_export}
\alias{plot.trafficmap_data}
\alias{plot.traffic_trend}
\title{Traffic Measurements and Reported Events on One Geographic Interface}
\description{Construct road measurements and timestamped geographic events, draw
static or interactive maps and write self-contained reports with one visual rule.}
\usage{
extract_traffic_data(path, file = NULL, assignment = NULL, format = "auto")
traffic_data(roads = NULL, flow = NULL, events = NULL, times = NULL,
  time_labels = NULL, interval_seconds = NULL, unit = "unit unspecified",
  title = "Traffic map", basemap = NULL, description = "",
  preparation = character(), notes = character(), provenance = list(),
  measurements = NULL, metric_info = list(), trends = NULL,
  road_columns = NULL, flow_columns = NULL, event_columns = NULL,
  crs = "EPSG:4326")
read_traffic(file)
flow_scale(values)
traffic_summary(x, index = x$initial, region = NULL, mode = "flow",
  reference = NULL, threshold = NULL, comparison = NULL,
  metric = NULL, duration_rule = NULL)
traffic_trend(x, region = NULL, mode = "flow", reference = NULL,
  threshold = NULL, comparison = NULL, metric = NULL,
  duration_rule = NULL, method = "mean")
traffic_map(x, file, mode = "flow", reference = NULL, threshold = NULL,
  comparison = NULL, metric = NULL, duration_rule = NULL,
  trend_method = "mean")
traffic_report(x, file, index = NULL, region = NULL, mode = "flow",
  reference = NULL, threshold = NULL, comparison = NULL,
  metric = NULL, duration_rule = NULL, trend_method = "mean")
traffic_export(x, output_dir, index = NULL, region = NULL,
  input_files = character(), mode = "flow", reference = NULL,
  threshold = NULL, comparison = NULL, metric = NULL,
  duration_rule = NULL, trend_method = "mean")
\method{plot}{trafficmap_data}(x, index = x$initial, region = NULL,
  mode = "flow", reference = NULL, threshold = NULL, comparison = NULL,
  metric = NULL, duration_rule = NULL, ...)
\method{plot}{traffic_trend}(x, metric = NULL, index = NULL, ...)
}
\arguments{
\item{path}{One existing local source file.}
\item{assignment}{Explicit dotted JavaScript identifier holding a JSON literal in JS/HTML. No code is executed.}
\item{format}{Source format: auto, rds, json, csv, js or html. Auto uses the extension; gzip text files are supported.}
\item{road_columns,flow_columns,event_columns}{Optional named character vectors from canonical field keys to source column names, for example \code{c(road_id="segment", time="seconds", flow="volume")}. Omission uses canonical names. Road keys are \code{road_id}, \code{geometry}, \code{region} and \code{opposite_id}; flow keys are \code{road_id}, \code{time} and \code{flow}; event keys are \code{time}, \code{lon}, \code{lat}, \code{id}, \code{category}, \code{description}, \code{region} and \code{time_label}. Only fields being renamed need mappings. Mapped source columns must exist; unknown or duplicate keys fail. Geometry remains an ordered list-column of numeric matrices and is not decoded from CSV text.}
\item{crs}{Declared coordinate reference system: \code{"EPSG:4326"}, \code{"WGS84"} or \code{"OGC:CRS84"}. Coordinates must already be longitude/latitude degrees. No CRS detection or coordinate transformation is performed.}
\item{measurements}{Optional long table with road_id, time, metric and nonnegative numeric value. Keys are unique and refer to known roads and frame starts. Primary flow is reserved.}
\item{metric_info}{Named lists of supplied unit and definition, with optional label, title, kind and scale. Duration metrics require kind="duration" and time_alignment set to interval_start or interval_end. Scale kinds are explicitly selected log1p, gradient with matching stops/palette, or classes with breaks, palette, labels and explicit right boundary inclusion. No scale is inferred for additional metrics. Primary flow metadata must retain the primary unit.}
\item{trends}{Optional source series with time, metric, value, definition and label, plus region (empty means all records). Unique keys identify source frame summaries.}
\item{duration_rule}{Explicit computation list with definition, interval_meaning, unit (minutes or seconds), missing_policy="break" and optional supplied scale. Rendering requires that scale.}
\item{method,trend_method}{Mean computes an available-road arithmetic mean. Provided uses the exact saved metric/scope series, including its precision and missing values, and fails when that scope has no unambiguous series.}

\item{roads}{Optional data frame or CSV/CSV.GZ/RDS table path with unique character \code{road_id} values and
an ordered \code{geometry} list-column of numeric longitude/latitude matrices.
Each matrix has at least two rows ordered in travel direction. An optional \code{region} column groups
geographic areas. An empty or omitted region is unassigned. Optional
\code{opposite_id} values reference existing, distinct road IDs and form reciprocal
pairs. Empty or NA values use automatic matching. Declared relationships restrict
matching to that partner without reversing source geometry. Other scalar columns describe the
roads without controlling pairing, colors or widths. Source polylines are retained;
opposing geometry receives display separation when crowded.}
\item{flow}{Optional long data frame or CSV/CSV.GZ/RDS table path with \code{road_id}, \code{time} and numeric
\code{flow}. Values are nonnegative and finite or \code{NA}. Road-time pairs
are unique and refer to the supplied roads. An absent pair is missing.}
\item{events}{Optional data frame or CSV/CSV.GZ/RDS table path with \code{time}, numeric longitude \code{lon}
and numeric latitude \code{lat}. Optional columns are \code{id}, \code{category},
\code{description}, \code{region} and \code{time_label}. Supplied IDs are unique.
Events without region labels belong only to the all-record scope. Named
region selections match supplied labels exactly.}
\item{times}{Optional strictly increasing frame starts. Clocks are consistently
numeric seconds or \code{POSIXct} instants across flow, events and frames.
Numeric values retain their source-clock interpretation. Absolute timestamps
have UTC display labels by default. Flow timestamps must match frame starts.
Event-only clocks default to consecutive bins containing the observations.}
\item{time_labels}{Optional character labels matching the frame clock. Labels
alone do not establish a timezone.}
\item{interval_seconds}{Positive nonoverlapping interval widths, one value or
one per frame. Default widths use consecutive frame differences, or one hour
for a single frame and for event-only binning. Event starts belong to half-open
intervals \code{[start, end)}; supplied events outside these intervals fail.}
\item{unit}{One caller-supplied measurement-unit label. Omission displays \code{unit unspecified}; no physical unit or unit conversion is inferred.}
\item{title}{One descriptive title.}
\item{basemap}{Optional offline GeoJSON FeatureCollection. LineString coordinates
are numeric longitude/latitude matrices, Polygon coordinates are lists of such
rings, and Point coordinates are numeric longitude/latitude pairs. Feature
properties may contain an explicit Point \code{label}. Other properties do
not control context rendering. Geometry uses a common neutral style. An
optional \code{attribution} string supplies the source text; omission is
shown as unspecified. Providers and licenses are not inferred.}
\item{description,preparation,notes}{Character descriptions for the report.}
\item{provenance}{List of source identities and processing metadata.}
\item{file}{Optional new RDS/JSON/JSON.GZ output for extraction; input RDS bundle for \code{read_traffic}; output HTML path for map or
report functions. Output parent directories exist. Existing output files are replaced.}
\item{values}{Nonnegative numeric vector or matrix, with \code{NA} for missing.}
\item{x}{A \code{trafficmap_data} object, or an input-bundle file for the HTML writers.}
\item{output_dir}{New directory for a reproducible static export. Existing directories are refused.}
\item{input_files}{Optional named character paths of the actual input and source files. Basenames, sizes and MD5 checksums are recorded without personal paths.}
\item{index}{One-based frame index. NULL in the static report selects the first frame.}
\item{region}{One exact geographic region label, or \code{NULL} for all records.}
\item{mode}{One common measurement mode: \code{"flow"}, \code{"change"}, or \code{"duration"}. The default retains source measurements and the existing flow-map style.}
\item{reference}{Optional one-based fixed reference frame for change mode. Omission compares each frame with its immediately preceding contiguous interval. The first frame and gaps have no implicit zero baseline.}
\item{threshold}{One caller-supplied finite threshold for duration mode, expressed in the source measurement unit. No physical threshold is inferred.}
\item{comparison}{Threshold relation: \code{">="}, \code{">"}, \code{"<="}, or \code{"<"}.}
\item{metric}{Source metric name for map, summary, trend construction and writers; omission selects primary flow. For plot.traffic_trend only, this selects the returned field \code{"mean"} or \code{"events"}.}
\item{...}{Additional graphical arguments for the trend method; reserved for the map method.}
}
\details{
At the input boundary, numeric character fields and factor labels for primary
flow and event coordinates are parsed as numbers. Factor integer codes are never
used as measurements. Malformed text and nonfinite values fail; actual missing
primary flow values remain missing. Numeric character time columns become source
seconds; absolute road/event times must be supplied as POSIXct values. The clock,
unit, coordinate system and road geometry are caller declarations.

All primary road-flow inputs use one green-to-red seven-color palette and one log1p
algorithm. Six exact thresholds span zero through the global maximum across
all times and records. They remain fixed during interaction; labels are
rounded for display. Missing observations are gray, zero is observed, and
all-zero and entirely missing inputs have explicit legends.

Source road coordinates are retained, and their vertex order denotes travel
direction. Declared reciprocal opposite_id relationships take priority. Without a
declaration, unique exact reverse paths are accepted first. Other segments use
at least three probes, at most 40 ground metres apart, reverse headings within
15 degrees, and lateral distance within 30 ground metres. Projected counterparts
must be mutual nearest matches. Competing paths within 2 metres of the best
distance are ambiguous, except for contiguous aligned segments at a shared
endpoint. At least 60 percent of an original segment's probes need consistent
support after reciprocal coverage checks. Substantial lateral side changes are
excluded from automatic matching. Ambiguous records retain source geometry.
Explicit partners still need overlapping reversed segments but are not subject
to the automatic proximity and ambiguity criteria. Relation identifiers do not
reverse geometry or establish legal travel direction.

One common layout uses visible road length per screen area, clipped to the view
plus one grid cell. A 96-unit grid and its 3-by-3 neighborhoods smooth density;
segment samples are at most 24 units apart. Per-road density is length weighted.
A continuous zoom baseline is capped by a local ink-area budget. Widths range
from 0.85 to 4.5 units. The opposing edge gap ranges from 0.55 to 1.8 units and
varies continuously with width. White casing also narrows with the stroke.
HTML uses pixels; static figures use 1/96-inch graphics line-width units.
Panning, zooming and resizing recompute layout. Flow values, optional attributes
and source identities do not choose drawing styles.

Crowded opposing segments receive a centerline gap of width plus edge gap. Existing
lateral distance contributes to this gap. Noncoincident paths move apart;
coincident paths use the right side of each travel-ordered line as a drawing
convention. Adjacent segment shifts are averaged at vertices. Screen offsets are
recomputed on zoom and do not change source coordinates, measurements or event
locations. Direction, lanes, speed limits and other descriptive attributes do
not control this calculation or trigger alternative styles. Event symbols
are always red diamonds with white borders and exclamation marks. Categories
are explicit filters and do not change symbols. Unknown physical units,
timezones, durations and road assignments are not inferred.

Roads, events, basemap and road casing have independent visibility controls.
Unavailable input components have disabled controls. Road casing is also removed
when roads are off. Layer visibility changes presentation without changing record
selection, statistics, frame membership or geographic extents. Road and event
popups retain their supplied information.

The first displayed frame is always the first supplied frame. All records are
selected by default. Region selections match labels exactly; unassigned records
remain in the all-record scope. Static reports accept an explicit frame and region, show whole-period coverage separately from selected-interval distributions, and include each applicable flow and event trend. Executable case reports organize regional sections explicitly.

Road and event locations both contribute to focus. Reset view uses the central
15--85 percent spatial range, or the full range for fewer than five locations.
Full extent includes all road vertices and event locations in the selection.
The visible map extent does not alter statistical scope.

Browser projections, bounds and region/frame membership are cached. A spatial
index selects view-plus-margin road candidates. Layers are created lazily and
reused; offscreen layers are detached without removing data records. Unchanged
views and display geometries reuse cached results. Statistics and Full extent
continue to use all records in the selected scope. Caches create no extra files.


The trafficmap-input-v1 RDS bundle contains the named constructor arguments
and a schema field. Input descriptions and source identities are rendered as
provided. Interactive map HTML embeds observations, context and Leaflet and uses a modern browser with DecompressionStream support. Static report HTML embeds map and trend figures, whole-period coverage and selected-interval statistics, without browser map controls. Both outputs are offline. Only R and jsonlite are required for generation.
}
\section{Shared trends and measurement views}{
\code{plot(traffic_trend(x))} draws the same mean or event-start trend used by reports and exports. Optional \code{index} marks a supplied frame; data columns remain directly accessible. All-missing and single-frame series have defined axes. Missing road values are excluded from each interval mean.

Change mode computes signed differences after road/time alignment. Omitted \code{reference} uses the previous contiguous interval; missing values propagate, and no change is inferred across a time gap. A fixed reference compares every frame with that explicitly selected frame. Differences retain the source unit. Blue denotes decrease, neutral denotes zero and red denotes increase, without magnitude bins. The nonnegative source input contract and original values are unchanged.

Duration mode prefers provided metrics declared with kind="duration", their source unit, definition, time alignment and supplied scale. If exactly one exists, it is selected automatically; multiple metrics require an explicit metric name. Computation parameters cannot override a provided duration view. Computation from primary flow in an object that also contains durations requires metric="flow".

Without a provided duration metric, computation requires explicit interval_seconds, threshold, comparison and a complete duration_rule. Each value must represent its full supplied interval according to that declared rule. Confirmed qualifying intervals accumulate through each interval end, in the declared minutes or seconds. Nonqualifying values reset to zero. Missing values return NA and interrupt continuity, as do time gaps. Onsets before the observed window or during a gap are unknown. No percentile threshold, physical congestion standard or duration display palette is inferred. Instantaneous values without interval meaning are unsuitable.

The same \code{mode}, \code{reference}, \code{threshold} and \code{comparison} arguments are accepted by map plots, summary/trend calculation, HTML writers and static exports. Modes are explicit options rather than source-specific branches. Events remain reported starts and are not transformed by road measurement modes. Source objects are not modified.
}
\section{Reproducible exports}{
\code{traffic_export} uses the same plot method and regional/frame selection for all inputs. It writes PNG/PDF maps and each applicable flow/event trend, summary.csv, trend.csv, selected-frame.rds, provenance.json, inputs.csv, checksums.csv and session-info.txt. Output is staged and finalized as a new directory. Invalid selections and missing input files fail before output creation.

The selection snapshot has schema trafficmap-selection-v1 and contains the exact selected road rows, geometry, values and event starts, title, clock interpretation, source-clock label, interval, unit and the complete input's frozen scale. It is an inspection artifact rather than a trafficmap-input-v1 bundle; plotting a newly constructed subset could otherwise recompute the scale or opposing relationships. Derived selections also retain the original selected road values, reference values for changes or source history for computed durations, and the full measurement definition. The output checksum table covers every exported file except itself. Input and output identity tables use MD5 checksums.
}
\value{
\code{extract_traffic_data} returns a traffic-source-v1 container with the decoded payload and basename, size, MD5 and format. Re-reading its RDS container retains the original identity and adds the container identity. JSON preserves JSON values; RDS preserves R types. The extractor reads formats without inferring schema meaning or executing scripts. JS/HTML requires one unambiguous complete JSON literal assignment starting on its own statement line (or at the start of an inline script). Comments and string/template contents are excluded from assignment matching. It refuses existing extraction outputs.
\code{traffic_data} and \code{read_traffic} return \code{trafficmap_data}.
\code{flow_scale} returns computed boundaries and the fixed palette.
Summary functions return data frames. See \code{\link{traffic_coverage}} for separate whole-period primary-flow coverage. The trend function returns a data frame with class \code{traffic_trend} and unit metadata, supporting the shared plot method. HTML writers and the export function return the output
path invisibly; the plot method returns the selected summary invisibly.
}
\examples{
roads <- data.frame(road_id = c("a", "b"))
roads$geometry <- list(
  rbind(c(-118.25, 34.05), c(-118.24, 34.06)),
  rbind(c(-118.24, 34.06), c(-118.23, 34.07)))
flow <- data.frame(road_id = c("a", "b", "a", "b"),
  time = c(0, 0, 3600, 3600), flow = c(0, 300, 180, NA))
events <- data.frame(time = 1800, lon = -118.24, lat = 34.06)
x <- traffic_data(roads, flow, events, interval_seconds = 3600)
traffic_summary(x)
trend <- traffic_trend(x)
plot(trend, metric = "mean", index = 1)
traffic_summary(x, index = 2, mode = "change")
flow_scale(c(0, 100, 1000, NA))
}

\section{Coordinate-derived backgrounds}{
When no context is supplied, rendering obtains OpenStreetMap road geometry through
traffic_basemap, using the same coordinate and cache rules for all inputs.
Statistics and data construction do not download map data. The initial request
needs network access. A matching checksum-protected cache in data/basemap-cache
is reused without a request; offline=TRUE in traffic_basemap refuses a missing
cache. options(bdynets.basemap.auto=FALSE) explicitly renders data layers only.
Background acquisition changes no traffic values, units, source road geometry or
selection statistics. Labelled spatial regions retain separate request bounds.
}

\section{Input road types and runtime context}{
Optional \code{roads$road_class} values declare OSM highway types for background acquisition. Declared classes, including minor roads, extend the common scale hierarchy at any zoom. Missing, empty or unmapped classes on observed roads request all highway types. The background rule never removes traffic road geometry or measurements. OD endpoints alone do not establish a travelled road type.
}
