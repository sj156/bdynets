\name{traffic_flow_map}
\alias{traffic_flow_map}
\title{Draw a Static Road-Flow Map}
\description{
Draw one road-measurement frame from a standard road input. Road geometry is
coloured by the selected value while reported point events are overlaid for the
same frame.
}
\usage{
traffic_flow_map(input, index = NULL, time_label = NULL, region = NULL, mode = "flow",
  reference = NULL, threshold = NULL, comparison = NULL, metric = NULL,
  duration_rule = NULL, basemap = NULL, title = NULL,
  subtitle = NULL, caption = NULL, flow_style = list(),
  north_arrow = TRUE, scale_bar = TRUE, output_file = NULL,
  width = 11, height = 9, dpi = 220, draw = TRUE)
}
\arguments{
\item{input}{A prepared single-frame \code{trafficmap_data} object or its
RDS path. Compatibility inputs include full-day \code{trafficmap_data},
road-only \code{bdynets-traffic-input-v1} lists, older untagged named
road arguments, and their RDS paths.}
\item{index}{One-based frame index. The first frame is used when omitted.}
\item{time_label}{Optional unique frame label such as \code{"08:00"}. Supply
this or \code{index}, not both. The selected label is applied during mapping
and need not be stored in the prepared input.}
\item{region}{One exact road-region label, or \code{NULL} for all roads.}
\item{mode}{Measurement view: \code{"flow"}, \code{"change"}, or
\code{"duration"}.}
\item{reference}{Optional one-based reference frame for change mode.}
\item{threshold,comparison,duration_rule}{Explicit duration-view parameters.}
\item{metric}{Primary flow or one supplied metric name.}
\item{basemap}{\code{NULL} or a GeoJSON FeatureCollection. Use
\code{traffic_basemap(input, ...)} explicitly to obtain and cache context.}
\item{title,subtitle,caption}{Optional map text.}
\item{flow_style}{Uniquely named scale overrides for \code{palette}, \code{breaks},
\code{labels}, and \code{right}.}
\item{north_arrow,scale_bar}{Whether to draw the shared map annotations.}
\item{output_file}{Optional PNG or PDF path.}
\item{width,height}{Output dimensions in inches.}
\item{dpi}{PNG resolution.}
\item{draw}{Whether to draw on the current graphics device.}
}
\details{
Road values use a seven-level log1p green--yellow--red scale computed across
the complete input, so frames remain comparable. Missing values are grey and
observed zeros remain zero. The road renderer uses source polylines, separates
opposing lines when needed, and draws a white halo under the coloured line.
A saved frame with \code{time_label_format} can derive its display label
from the frame time in place of a stored \code{time_labels} value.

\code{traffic_flow_map} and \code{traffic_od_map} share the static canvas,
projection, basemap, output device, north arrow, scale bar and text helpers.
Only their data selection and line renderers differ. There is no combined
flow-and-OD static-map interface.
}
\value{
Invisibly, a list containing the selected frame and region, road summary,
basemap and output path.
}
\seealso{\code{\link{traffic_od_map}}, \code{\link{traffic_data}}}
\examples{
roads <- data.frame(road_id = c("a", "b"))
roads$geometry <- list(
  rbind(c(116.38, 39.90), c(116.39, 39.91)),
  rbind(c(116.39, 39.91), c(116.40, 39.90)))
flow <- data.frame(road_id = c("a", "b"), time = 0, flow = c(2, 5))
frame <- traffic_data(roads = roads, flow = flow, times = 0, interval_seconds = 3600)
result <- traffic_flow_map(frame, basemap = NULL, draw = FALSE)
result$type
}
