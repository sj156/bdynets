\name{traffic_od_map}
\alias{traffic_od_map}
\title{Draw one prepared directed OD frame}
\description{Draw the supplied directed pairs, endpoints and optional prepared street and event layers. Rings mark directed pairs whose distinct nodes share a location.}
\usage{
traffic_od_map(
  nodes,
  flows = NULL,
  basemap = NULL,
  node_columns = c(id = "node_id", longitude = "longitude", latitude = "latitude", size =
    "member_weight"),
  flow_columns = c(origin = "origin_id", destination = "destination_id", count = "flow",
    time = "time"),
  basemap_columns = c(longitude = "longitude", latitude = "latitude", group =
    "street_group", class = "street_class"),
  start_time = NULL,
  end_time = NULL,
  hour_window = NULL,
  timezone = "UTC",
  top_n = Inf,
  include_loops = FALSE,
  active_nodes_only = TRUE,
  bounds = NULL,
  padding = 0.045,
  title = "Directed OD flows",
  subtitle = NULL,
  caption = "Curved arrows connect aggregate endpoints; they are not observed paths.",
  flow_breaks = NULL,
  flow_labels = NULL,
  flow_palette = NULL,
  flow_right = TRUE,
  single_flow_dashed = TRUE,
  flow_style = list(),
  curvature = 0.1,
  arrow_length = 0.065,
  node_fill = "white",
  node_colour = "#17324d",
  node_size = 0.82,
  scale_node_size = FALSE,
  north_arrow = TRUE,
  scale_bar = TRUE,
  output_file = NULL,
  width = 11,
  height = 9,
  dpi = 220,
  draw = TRUE,
  events = NULL,
  event_columns = NULL,
  index = NULL,
  time_label = NULL,
  extent = c("series", "frame")
)
}
\arguments{
\item{nodes}{An ordered list of prepared \code{bdynets_od_pairs} windows, one
prepared window, or an RDS path containing either form.}
\item{index}{One-based index into the supplied windows, defaulting to the first.}
\item{time_label}{Exact start of a prepared window. Use \code{"08:00"} for
a start at second zero, or supply seconds and any fraction as in
\code{"2026-01-01 08:00:30.125"}. Include the date and, for repeated local
times, the UTC offset when needed. Use \code{index} for a pooled clock window
without an absolute start or when more than one window matches. Supply at most
one of \code{index} and \code{time_label}.}
\item{flows, node_columns, flow_columns, start_time, end_time, hour_window,
  timezone, top_n, include_loops, active_nodes_only, event_columns}{Preparation
  arguments. Leave these at their defaults. If supplied, \code{timezone} must match the prepared frame.}
\item{basemap, basemap_columns}{Prepared street context and its optional display-field mapping.
For a local RDS cache, \code{"cache"} requires coverage of the rendered viewport
and \code{"auto"} uses matching context when available.}
\item{bounds, padding}{Explicit map extent or padding around the default extent.
The series extent uses the study locations in \code{all_nodes} across the
supplied windows, including when the input contains one window.}
\item{extent}{With \code{"series"}, use the extent of the supplied series.
With \code{"frame"}, focus on the selected frame's visible routes, nodes and
events, including the curve bends. In this mode the margin is a fraction of the
largest projected span. If there is nothing to draw, use the available study
locations. Explicit \code{bounds} take precedence.}
\item{title, subtitle, caption}{Map text.}
\item{flow_style}{Named appearance overrides: \code{breaks}, \code{labels},
\code{palette}, \code{right} and \code{single_flow_dashed}.}
\item{flow_breaks, flow_labels, flow_palette, flow_right, single_flow_dashed}{Route appearance arguments. Defaults are calculated from the input.}
\item{curvature, arrow_length}{Arrow geometry.}
\item{node_fill, node_colour, node_size, scale_node_size}{Node styling.}
\item{north_arrow, scale_bar}{Whether to draw map annotations.}
\item{output_file, width, height, dpi, draw}{Graphics output settings.}
\item{events}{Prepared overlay with numeric \code{lon} and \code{lat} columns.}
}
\details{
The renderer draws one prepared window. With an ordered list, it calculates up
to seven logarithmic count classes from positive drawable pairs across all
windows so the legend is comparable between separate plots. Override the
boundaries with \code{flow_style = list(breaks = ...)}. The \code{extent}
choice does not change these classes. A zero-trip window with
records and a window with no records are labelled separately. PDF uses Quartz on macOS when available, or Cairo if available. Unicode text
needs a suitable installed font. The fallback PDF device has limited Unicode
support.
}
\value{Invisibly, the selected index and label, plotted routes and nodes,
display counts, basemap and output path. The named WGS84 \code{extent} is the
requested plotting extent. The \code{viewport} includes any expansion required
by aspect ratio, or is \code{NULL} when no device drew a map.}
\seealso{\code{\link{traffic_flow_map}}, \code{\link{traffic_map_extent}}}
