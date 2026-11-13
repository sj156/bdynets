\name{traffic_map}
\alias{traffic_map}
\alias{traffic_map_html}
\title{Draw a Unified Static Traffic Map}
\description{
Draw road flow, directed origin-destination flow, or both from the standard
\code{bdynets-traffic-input-v1} representation. Road measurements use the
package road scale; OD observations use directed curved arrows. A geographic
background can be supplied, loaded from cache, or obtained automatically.
}
\usage{
traffic_map(input = NULL, file = NULL, index = NULL, region = NULL,
  start_time = NULL, end_time = NULL, layers = "auto", mode = "flow",
  reference = NULL, threshold = NULL, comparison = NULL, metric = NULL,
  duration_rule = NULL, basemap = "auto", bounds = NULL, padding = 0.045,
  top_n = Inf, include_loops = FALSE, active_nodes_only = TRUE,
  title = NULL, subtitle = NULL, caption = NULL, flow_style = list(),
  od_style = list(), node_style = list(), event_style = list(),
  north_arrow = TRUE, scale_bar = TRUE, output_file = NULL,
  width = 11, height = 9, dpi = 220, draw = TRUE,
  trend_method = "mean", x = NULL)

traffic_map_html(x, file, mode = "flow", reference = NULL,
  threshold = NULL, comparison = NULL, metric = NULL,
  duration_rule = NULL, trend_method = "mean")
}
\arguments{
\item{input}{A standard \code{bdynets-traffic-input-v1} list, its RDS path,
or an already constructed \code{trafficmap_data} or \code{traffic_od_data}
object.}
\item{x}{For \code{traffic_map}, a compatibility alias for \code{input};
supply only one of these arguments. For \code{traffic_map_html}, a
\code{trafficmap_data} object or compatible RDS bundle.}
\item{file}{For \code{traffic_map}, an optional PNG or PDF path. For backward
compatibility, an HTML destination delegates to \code{traffic_map_html},
including earlier named and positional road HTML calls. For
\code{traffic_map_html}, the required HTML output path.}
\item{index}{One-based road frame. The input's initial frame is used when omitted. In a
combined absolute-time input with timed OD, its half-open frame interval also
selects OD observations unless \code{start_time} and \code{end_time} are
supplied. Untimed OD remains an untimed aggregate overlay.}
\item{region}{One exact road-region label, or \code{NULL} for all records.}
\item{start_time,end_time}{Optional half-open OD timestamp window
\code{[start_time,end_time)}. Supply both or neither. Explicit timestamp filtering
is rejected for untimed OD. This selects records without resampling their
source intervals.}
\item{layers}{\code{"auto"}, or a character subset of \code{"flow"},
\code{"od"}, \code{"nodes"}, and \code{"events"}. At least one of flow or OD
must be drawable. The supplied examples use \code{"auto"}. Independent
node/event layer switching is not established by this workflow.}
\item{mode}{Road measurement view: \code{"flow"}, \code{"change"}, or
\code{"duration"}.}
\item{reference}{Optional one-based fixed reference frame for change mode.}
\item{threshold,comparison,duration_rule}{Explicit road duration-view
parameters. See \code{\link{traffic_data}}.}
\item{metric}{Road metric name. Omission selects primary flow.}
\item{basemap}{\code{"auto"}, \code{NULL}, or an offline GeoJSON
FeatureCollection. Automatic mode uses an embedded basemap when available.
Otherwise context is requested only when drawing or writing an image; matching verified cached
context is reused. New or missing requests require network access.}
\item{bounds}{Optional OD extent as \code{c(west,east,south,north)}. Without roads, the default combined OD/event extent contains selected stations and events. Data are clipped to the plot window.}
\item{padding}{Nonnegative fractional padding applied to node-derived OD
bounds.}
\item{top_n}{Maximum number of nonloop directed OD pairs drawn.}
\item{include_loops}{Whether within-node OD flow is retained in the returned
summary. Loops are not drawn as routes.}
\item{active_nodes_only}{Whether an OD layer shows only endpoints of displayed
routes.}
\item{title,subtitle,caption}{Optional map text.}
\item{flow_style}{Named overrides for the road scale: \code{palette},
\code{breaks}, \code{labels}, or \code{right}.}
\item{od_style}{Named OD overrides: \code{flow_breaks}, \code{flow_labels},
\code{flow_palette}, \code{single_flow_dashed}, \code{curvature}, and
\code{arrow_length}.}
\item{node_style}{Named node overrides: \code{node_fill},
\code{node_colour}, \code{node_size}, and \code{scale_node_size}.}
\item{event_style}{Reserved. Standard events currently retain the package
event style, so this must be an empty list.}
\item{north_arrow,scale_bar}{Whether to add these annotations to road and
combined maps.}
\item{output_file}{Optional PNG or PDF path. This takes precedence over
\code{file}.}
\item{width,height}{Output dimensions in inches.}
\item{dpi}{PNG resolution.}
\item{draw}{Whether to draw on the current graphics device. Set this to
\code{FALSE} when only writing \code{output_file}.}
\item{trend_method}{Legacy HTML trend method, \code{"mean"} or
\code{"provided"}. It is ignored by the static renderer.}
}
\details{
Road values use one seven-level log1p green--yellow--red scale computed across
all frames, so maps remain comparable through time. OD uses the five default
trip-count classes: 1, 2--3, 4--5, 6--7 and at least 8. Zero is observed flow
and missing values are drawn separately.
OD records are aggregated by directed endpoint within the selected window.
Curves and arrowheads communicate direction but are not inferred travelled
paths. In a mixed input, the OD layer is overlaid on the road view and both
layers share the same projected map window. A selected road frame without
matching OD records still draws its roads. Its OD result has status
\code{"no_records"}, \code{selected_records=0}, and \code{NA} flow totals; no
zero-flow observations are created. Untimed OD is labeled as an aggregate
overlay and is never assigned the selected road timestamp.

Without roads, OD and events share an extent derived from selected data unless
explicit bounds are supplied. Pure OD windows without records return
\code{"no_records"}, zero selected records and unknown flow totals. Provided
stations are displayed as reference locations. Actual recorded zeros remain
zero; no drawable nonloop routes gives \code{"no_routes"}. Source rows and
self-loops are retained, without invented routes or observation times.
Malformed IDs, coordinates, timestamps and styles still raise errors.
Numeric source-clock events remain unaligned when no time filter is requested;
absolute timestamp filtering requires explicitly aligned event timestamps.

\code{traffic_map_html} preserves the earlier self-contained interactive road
map writer. New code should use \code{traffic_map} for static standard-input
maps and call \code{traffic_map_html} explicitly when HTML interaction is
required.
}
\value{
Invisibly, a list describing the rendered type, selections, summaries,
basemap, and output path. A pure OD map returns standardized nodes and routes
with its OD summary and status. An OD/event composition also returns the selected event overlay. In both standalone and mixed maps, an empty OD selection is recorded
explicitly as \code{"no_records"}, with unknown flow totals rather than
observed zeros.
\code{traffic_map_html} returns its output path invisibly.
}
\examples{
roads <- data.frame(road_id = c("a", "b"))
roads$geometry <- list(
  rbind(c(116.38, 39.90), c(116.39, 39.91)),
  rbind(c(116.39, 39.91), c(116.40, 39.90)))
flow <- data.frame(road_id = rep(c("a", "b"), 2),
  time = rep(c(0, 3600), each = 2), flow = c(2, 5, 4, NA))
input <- traffic_input(roads = roads, flow = flow,
  times = c(0, 3600), interval_seconds = 3600)
result <- traffic_map(input, index = 1, basemap = NULL, draw = FALSE)
result$type
}
