\name{traffic_flow_map}
\alias{traffic_flow_map}
\alias{traffic_speed_map}
\title{Draw a Static Flow or Speed Map}
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
  width = 11, height = 9, dpi = 220, draw = TRUE, full_extent = TRUE, padding = 0.08)
traffic_speed_map(input, index = NULL, time_label = NULL, region = NULL,
  basemap = NULL, title = NULL, subtitle = NULL, caption = NULL,
  north_arrow = TRUE, scale_bar = TRUE, output_file = NULL,
  width = 11, height = 9, dpi = 220, draw = TRUE, full_extent = TRUE, padding = 0.08)
}
\arguments{
\item{input}{The complete prepared \code{trafficmap_data} object or its RDS
path. Use \code{traffic_flow_map()} for \code{input_type = "flow"} and
\code{traffic_speed_map()} for \code{input_type = "speed"}.}
\item{index}{One-based frame index. Omit to use the prepared \code{initial} index if present, or frame 1.}
\item{time_label}{An exact prepared start. Use \code{"08:00"} for a start
at second zero, or include seconds and any fraction, such as
\code{"08:00:30.125"}. For calendar clocks, include the date when needed and
add the UTC offset of a repeated local time, or use \code{index}. A numeric
elapsed clock uses elapsed hours, which can exceed 24. Supply at most one
of \code{time_label} and \code{index}.}
\item{region}{One exact road-region label, or \code{NULL} for all roads.}
\item{mode,reference,threshold,comparison,duration_rule}{Keep their defaults.
Supply any additional measurements and their display metadata in the prepared object.}
\item{metric}{The primary Flow measurement or one supplied metric name.}
\item{basemap}{A GeoJSON FeatureCollection, local GeoJSON file or \code{NULL}.
Omit to keep attached context. For a local RDS cache, \code{"cache"} requires
coverage of the rendered viewport and \code{"auto"} uses matching context when
available. A cache options list can select the directory. To prepare coverage
for a map combining several regions, use \code{split_regions = FALSE} in
\code{traffic_road_extents()}. See \file{INPUT-FORMAT.md} for GeoJSON preparation.}
\item{title,subtitle,caption}{Optional map text.}
\item{flow_style}{Uniquely named scale overrides for \code{palette}, \code{breaks},
\code{labels}, and \code{right}.}
\item{north_arrow,scale_bar}{Whether to draw the shared map annotations.}
\item{output_file}{Optional PNG or PDF path.}
\item{width,height}{Output dimensions in inches.}
\item{dpi}{PNG resolution.}
\item{draw}{Whether to draw on the current graphics device.}
\item{full_extent}{The default includes every road vertex and event location
in the selected area. Set to \code{FALSE} to focus on the main cluster.
Statistics cover all records within the selected area.}
\item{padding}{Nonnegative margin on each side, as a fraction of the larger
projected span. The default is \code{0.08}. A collapsed dimension receives a
minimal positive span when \code{padding = 0}.}
}
\details{
Flow uses a seven-level log1p green--yellow--red scale computed across
the complete input. Speed uses fixed class boundaries at 35, 40, 45, 50, 55
and 60 mph, or their equivalents in km/h. Other supplied metrics use their
prepared scale. Missing values are grey and
observed zeros remain zero. The road renderer uses source polylines, separates
opposing lines when needed, and draws a white halo under the coloured line.
The display label is derived from the frame time. Supply
\code{time_label_format} in preparation to control how the time is shown.
It does not change the start used for selection. A numeric source clock
already encoded as calendar seconds and supplied with a format uses calendar
clock matching in its declared timezone.

\code{traffic_flow_map}, \code{traffic_speed_map} and \code{traffic_od_map} share the static canvas,
projection, basemap, output device, north arrow, scale bar and text helpers.
Road and OD inputs use their corresponding line renderers. PDF uses Quartz on macOS when available, or Cairo if available. Unicode text
needs a suitable installed font. The fallback PDF device has limited Unicode
support.
}
\value{
Invisibly, a list containing the selected frame and region, road summary,
basemap and output path. The named WGS84 \code{extent} is the requested
plotting extent. The \code{viewport} includes any expansion required by aspect
ratio, or is \code{NULL} when no device drew a map.
}
\seealso{\code{\link{traffic_od_map}}, \code{\link{traffic_trend}}}
