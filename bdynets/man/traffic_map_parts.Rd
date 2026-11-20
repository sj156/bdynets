\name{traffic_map_parts}
\alias{traffic_od_select}
\alias{traffic_od_pairs}
\alias{traffic_map_extent}
\alias{traffic_map_base}
\alias{traffic_map_canvas}
\alias{traffic_od_layer}
\alias{traffic_map_caption}
\alias{traffic_map_north_arrow}
\alias{traffic_map_scale_bar}
\alias{traffic_map_output}
\title{Prepare Directed OD Pairs and Compose Map Layers}
\description{
Select and aggregate directed OD records during preparation. The remaining
helpers compose static map output from prepared locations and routes.
}
\usage{
traffic_od_select(nodes, flows = NULL,
  node_columns = c(id = "node_id", longitude = "longitude",
    latitude = "latitude", size = "member_weight"),
  flow_columns = c(origin = "origin_id", destination = "destination_id",
    count = "flow", time = "time"),
  start_time = NULL, end_time = NULL, hour_window = NULL,
  time_label = NULL, timezone = "UTC")

traffic_od_pairs(selection, top_n = Inf, include_loops = FALSE,
  active_nodes_only = TRUE)

traffic_map_extent(nodes, bounds = NULL, padding = 0.045)

traffic_map_base(basemap = "auto", extent,
  basemap_columns = c(longitude = "longitude", latitude = "latitude",
    group = "street_group", class = "street_class"),
  cache_dir = file.path("data", "basemap-cache"), offline = FALSE,
  refresh = FALSE, draws = TRUE)

traffic_map_canvas(extent, basemap = NULL, margins = c(3.6, 1.2, 5.1, 1.2))

traffic_od_layer(pairs, flow_style = list(), legend = TRUE)

traffic_map_caption(title, subtitle = NULL, caption = NULL, window_label = NULL)

traffic_map_north_arrow()

traffic_map_scale_bar()

traffic_map_output(draw_function, output_file = NULL, width = 11,
  height = 9, dpi = 220, draw = TRUE)
}
\arguments{
\item{nodes, flows}{Node and flow tables or supported CSV/RDS paths.
For \code{traffic_od_select()}, supply only \code{nodes} when passing a
combined OD object. For \code{traffic_map_extent()}, supply a table with
\code{longitude} and \code{latitude}.}
\item{node_columns, flow_columns}{Named role-to-column mappings.
Optional roles can be omitted. Each flow record counts as one when the
count role is omitted.}
\item{start_time, end_time}{Start inclusive and end exclusive timestamps.
Supply both for an explicit window.}
\item{hour_window}{Two integer clock hours, pooled across dates.}
\item{time_label}{Whole-hour shorthand such as \code{"08:00"}, pooled
across dates in the declared timezone. Use \code{start_time} and
\code{end_time} for windows with minute-level bounds.}
\item{timezone}{IANA timezone for reading and selecting source timestamps.}
\item{selection}{An object from \code{traffic_od_select()}.}
\item{top_n}{Maximum number of nonloop routes. \code{Inf} retains all of them.}
\item{include_loops}{Allow a selection with only within-node trips. These
trips remain in the totals and do not produce arrows.}
\item{active_nodes_only}{When routes are available, display only their endpoints.}
\item{bounds}{Optional numeric bounds in west, east, south, north order.}
\item{padding}{Fractional padding around the node extent.}
\item{basemap}{Basemap specification or result of \code{traffic_map_base()}.
A specification can be \code{NULL}, \code{"auto"}, \code{"cache"}, a
cache options list, a FeatureCollection, a grouped line table or its path.}
\item{extent}{Numeric bounds in west, east, south, north order.}
\item{basemap_columns}{Named role-to-column mappings for line tables.}
\item{cache_dir}{Directory for cached geographic context.}
\item{offline}{Use cached data for automatic geographic context.}
\item{refresh}{Request a new basemap instead of reusing a cached one.}
\item{draws}{Whether an automatic basemap is needed for drawing.}
\item{margins}{Base graphics margins in lines.}
\item{pairs}{Prepared pairs from \code{traffic_od_pairs()}.}
\item{flow_style}{Named style settings for the OD layer. These include
\code{breaks}, \code{labels}, \code{palette}, \code{right},
\code{single_flow_dashed}, \code{curvature}, \code{arrow_length},
\code{node_fill}, \code{node_colour}, \code{node_size} and \code{scale_node_size}.}
\item{legend}{Draw the OD legend.}
\item{title, subtitle, caption}{Map titles and caption.}
\item{window_label}{Window label appended to the title when no subtitle is supplied.}
\item{draw_function}{Function with no arguments that draws the map.}
\item{output_file}{Optional PNG or PDF path.}
\item{width, height}{Output size in inches.}
\item{dpi}{PNG resolution.}
\item{draw}{Also draw on the current device.}
}
\details{
Use one time selector at a time. The preparation result separates the
within-node total from the directed routes and attaches the endpoint
coordinates. Pass the result to \code{\link{traffic_od_map}} to draw it.

For manual composition, open the map canvas before drawing layers, captions
or annotations. \code{traffic_map_base()} obtains geographic context for a
declared extent and can skip automatic acquisition when \code{draws = FALSE}.
}
\value{
\code{traffic_od_select()} returns an OD selection with standardised nodes
and selected records. \code{traffic_od_pairs()} returns a
\code{bdynets_od_pairs} object with routes, nodes, totals and counts.
The map helpers return named bounds, a basemap or a canvas as appropriate.
Layer, caption, annotation and output helpers return their result invisibly.
}
\seealso{\code{\link{traffic_od_map}}, \code{\link{traffic_flow_map}}}
