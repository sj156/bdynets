% Public interface in R/traffic-map.R
\name{traffic_od_map}
\alias{traffic_od_map}
\title{Draw stations and directed origin-destination flows}
\usage{
traffic_od_map(
  nodes,
  flows = NULL,
  basemap = "auto",
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
  flow_breaks = c(1, 3, 5, 7),
  flow_labels = c("1", "2-3", "4-5", "6-7", ">= 8"),
  flow_palette = c("#69afe0", "#a1c181", "#e9c46a", "#f4a261", "#e76f51"),
  single_flow_dashed = TRUE,
  curvature = 0.1,
  arrow_length = 0.065,
  node_fill = "white",
  node_colour = "#17324d",
  node_size = 0.82,
  scale_node_size = FALSE,
  output_file = NULL,
  width = 11,
  height = 9,
  dpi = 220,
  draw = TRUE,
  events = NULL,
  event_columns = NULL
)
}
\arguments{
\item{nodes}{Node table, CSV/CSV.GZ/RDS path, \code{traffic_od_data} object, or \code{od_aggregation} object.}

\item{flows}{Flow table or CSV/CSV.GZ/RDS path; omit for a canonical or aggregation object.}

\item{basemap}{A trafficmap GeoJSON FeatureCollection, grouped line table,
CSV/RDS path, or \code{NULL}. The default \code{auto} obtains context from the
displayed coordinate bounds and caches it in data/basemap-cache.}

\item{node_columns}{Named canonical-key-to-source-column mappings for \code{id}, \code{longitude}, \code{latitude}, and optional \code{size}. Missing optional size in the default mapping is ignored. An explicit mapping must refer to existing columns or use \code{NA} for an omitted optional field.}

\item{flow_columns}{Named canonical-key-to-source-column mappings for \code{origin}, \code{destination}, optional \code{count} and \code{time}. Counts must be nonnegative integer trips; omitted count means one trip per row. Missing optional columns in the default mapping are ignored. Explicit missing source columns fail. Use \code{NA} to omit an optional field. Canonical objects supply their stored mappings when these arguments are omitted.}

\item{basemap_columns}{Mappings for line \code{longitude}, \code{latitude}, \code{group},
and optional \code{class}.}

\item{start_time, end_time}{Optional half-open timestamp window.}

\item{hour_window}{Optional two-element clock-hour window such as \code{c(0,3)}.
It pools the matching hours across every date and is mutually exclusive
with \code{start_time} and \code{end_time}.}

\item{timezone}{IANA timezone used to parse timestamps.}

\item{top_n}{Maximum nonloop OD pairs drawn; use \code{Inf} for all.}

\item{include_loops}{Whether within-node flows are retained in the summary.}

\item{active_nodes_only}{Whether to draw only endpoints of displayed routes.}

\item{bounds}{Optional \code{c(west,east,south,north)} longitude/latitude extent.}

\item{padding}{Fractional padding when bounds are derived from nodes.}

\item{title, subtitle, caption}{Map text.}

\item{flow_breaks, flow_labels, flow_palette}{Fixed flow classes, labels and colours.}

\item{single_flow_dashed}{Whether flow-one routes use dashed lines.}

\item{curvature}{Curve magnitude as a fraction of line length.}

\item{arrow_length}{Arrowhead length in inches.}

\item{node_fill, node_colour, node_size}{Station-symbol styling.}

\item{scale_node_size}{Whether the mapped node-size column controls symbol size.}

\item{output_file}{Optional PNG or PDF output path.}

\item{width, height}{Output dimensions in inches.}

\item{dpi}{PNG resolution.}

\item{draw}{Whether to also draw on the current graphics device.}

\item{events}{Optional event data frame or CSV/CSV.GZ/RDS path with \code{time}, \code{lon} and \code{lat}, and optional unique \code{id}, \code{category}, \code{description}, \code{region} and \code{time_label}. Timestamps must be explicit ISO character timestamps or POSIXt instants, using \code{timezone} for unzoned character times. Event starts use the same half-open timestamp or clock-hour filter as OD flows.}

\item{event_columns}{Optional named canonical-key-to-source-column mapping for the event fields. Omission uses canonical names; unknown keys or missing mapped source columns fail.}
}
\value{
Invisibly, standardized nodes, plotted routes, summary and basemap. With events, \code{event_overlay} also records selected event rows, projected locations and the drawn viewport (NULL when no plot is drawn).
}
\description{
Input column names are supplied through mappings, so unrelated columns are
ignored and the function is not tied to Divvy data.  A grouped line table is
converted to trafficmap's GeoJSON basemap format before it is drawn.
}

\details{
Normalization occurs before the reference directed-OD drawing algorithm. Coordinates
and sizes accept numeric text or factor labels, with malformed or nonfinite
values rejected. Coordinates must already be WGS84 longitude/latitude degrees;
no CRS or unit is guessed. Weighted OD values are unsupported.

The default five trip-count classes are 1, 2--3, 4--5, 6--7 and at least 8, with
colours \code{#69afe0}, \code{#a1c181}, \code{#e9c46a}, \code{#f4a261} and
\code{#e76f51}. The reference curve construction, arrow placement, ordering,
widths and singleton dashes are retained. Curves connect endpoints and are not
observed road paths.

Event overlays use red diamonds with white borders and exclamation marks. The
OD node-derived or explicitly supplied viewport is preserved: events do not
expand it and locations outside it are clipped. Event categories do not select
alternative styles. Overlay composition leaves OD routes, counts and selection
statistics unchanged. Omitted events retain the reference OD rendering.
}
\seealso{\code{\link{traffic_od_data}}, \code{\link{aggregate_od_data}}}
