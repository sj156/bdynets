\name{traffic_data}
\alias{traffic_data}
\alias{read_traffic}
\alias{flow_scale}
\alias{traffic_summary}
\alias{traffic_trend}
\alias{traffic_map}
\alias{traffic_report}
\alias{plot.trafficmap_data}
\title{Traffic Measurements and Reported Events on One Geographic Interface}
\description{Construct road measurements and timestamped geographic events, draw
static or interactive maps and write self-contained reports with one visual rule.}
\usage{
traffic_data(roads = NULL, flow = NULL, events = NULL, times = NULL,
  time_labels = NULL, interval_seconds = NULL, unit = "unit unspecified",
  title = "Traffic map", basemap = NULL, description = "",
  preparation = character(), notes = character(), provenance = list())
read_traffic(file)
flow_scale(values)
traffic_summary(x, index = x$initial, region = NULL)
traffic_trend(x, region = NULL)
traffic_map(x, file)
traffic_report(x, file)
\method{plot}{trafficmap_data}(x, index = x$initial, region = NULL, ...)
}
\arguments{
\item{roads}{Optional data frame with unique character \code{road_id} values and
an ordered \code{geometry} list-column of numeric longitude/latitude matrices.
Each matrix has at least two rows ordered in travel direction. An optional \code{region} column groups
geographic areas. An empty or omitted region is unassigned. Optional
\code{opposite_id} values reference existing, distinct road IDs and form reciprocal
pairs. Empty or NA values use automatic matching. Declared relationships restrict
matching to that partner without reversing source geometry. Other scalar columns describe the
roads without controlling pairing, colors or widths. Source polylines are retained;
opposing geometry receives display separation when crowded.}
\item{flow}{Optional long data frame with \code{road_id}, \code{time} and numeric
\code{flow}. Values are nonnegative and finite or \code{NA}. Road-time pairs
are unique and refer to the supplied roads. An absent pair is missing.}
\item{events}{Optional data frame with \code{time}, numeric longitude \code{lon}
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
\item{file}{Input RDS bundle for \code{read_traffic}; output HTML path for map or
report functions. Output parent directories exist. Existing output files are replaced.}
\item{values}{Nonnegative numeric vector or matrix, with \code{NA} for missing.}
\item{x}{A \code{trafficmap_data} object, or an input-bundle file for the HTML writers.}
\item{index}{One-based frame index.}
\item{region}{One exact geographic region label, or \code{NULL} for all records.}
\item{...}{Reserved graphical arguments.}
}
\details{
All flow inputs use one green-to-red seven-color palette and one log1p
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
remain in the all-record scope. The report uses all records and includes each
applicable flow and event trend without automatic regional chapters.

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
provided. HTML embeds observations, context, Leaflet and report figures. A
modern browser with DecompressionStream support displays it offline. Map generation uses jsonlite alongside base R graphics.
}
\value{
\code{traffic_data} and \code{read_traffic} return \code{trafficmap_data}.
\code{flow_scale} returns computed boundaries and the fixed palette.
Summary and trend functions return data frames. HTML writers return the output
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
traffic_trend(x)
flow_scale(c(0, 100, 1000, NA))
}
