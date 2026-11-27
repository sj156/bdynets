\name{traffic_map_parts}
\alias{traffic_map_extent}
\alias{traffic_map_base}
\alias{traffic_map_canvas}
\alias{traffic_od_layer}
\alias{traffic_map_caption}
\alias{traffic_map_north_arrow}
\alias{traffic_map_scale_bar}
\alias{traffic_map_output}
\title{Compose Map Layers for Prepared OD Frames}
\description{Compose static map output from prepared locations and routes.}
\usage{
traffic_map_extent(nodes, bounds = NULL, padding = 0.045,
  width = NULL, height = NULL)

traffic_map_base(basemap = "auto", extent,
  basemap_columns = c(longitude = "longitude", latitude = "latitude",
    group = "street_group", class = "street_class"),
  cache_dir = file.path("data", "basemap-cache"), offline = TRUE,
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
\item{nodes}{Prepared nodes with longitude and latitude columns.}
\item{bounds}{Optional numeric bounds in west, east, south, north order.}
\item{padding}{Fractional padding around the node extent.}
\item{basemap}{Basemap specification or result of \code{traffic_map_base()}.
A specification can be \code{NULL}, \code{"auto"}, \code{"cache"}, a
cache options list, a FeatureCollection or local GeoJSON file, or a grouped line table or its path.
\code{"auto"} uses local context when a matching cache exists. \code{"cache"}
requires a matching local cache.}
\item{extent}{Numeric bounds in west, east, south, north order.}
\item{basemap_columns}{Named role-to-column mappings for line tables.
Longitude, latitude and group are required. The default \code{street_class}
column is optional.}
\item{cache_dir}{Directory for cached geographic context.}
\item{offline}{Keep \code{TRUE}. Drawing reads local context.}
\item{refresh}{Keep \code{FALSE}. See \file{INPUT-FORMAT.md} to prepare a local background before drawing.}
\item{draws}{Whether an automatic basemap is needed for drawing.}
\item{margins}{Base graphics margins in lines.}
\item{pairs}{Prepared OD frame with routes and endpoint locations.}
\item{flow_style}{Named style settings for the OD layer. These include
\code{breaks}, \code{labels}, \code{palette}, \code{right},
\code{single_flow_dashed}, \code{curvature}, \code{arrow_length},
\code{node_fill}, \code{node_colour}, \code{node_size} and \code{scale_node_size}.}
\item{legend}{Draw the OD legend.}
\item{title, subtitle, caption}{Map titles and caption.}
\item{window_label}{Window label appended to the title when no subtitle is supplied.}
\item{draw_function}{Function with no arguments that draws the map.}
\item{output_file}{Optional PNG or PDF path.}
\item{width, height}{Output size in inches. Supply both to
\code{traffic_map_extent()} to plan the background of a static OD series
map at the output aspect ratio.}
\item{dpi}{PNG resolution.}
\item{draw}{Also draw on the current device.}
}
\details{These helpers compose graphics for already prepared OD frames.}
\seealso{\code{\link{traffic_od_map}}, \code{\link{traffic_flow_map}}}
