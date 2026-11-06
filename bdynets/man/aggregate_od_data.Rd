% Public interface in R/traffic-data.R
\name{aggregate_od_data}
\alias{aggregate_od_data}
\title{Validate and aggregate spatial origin-destination data}
\usage{
aggregate_od_data(
  nodes,
  flows = NULL,
  radius_m,
  node_columns = c(id = "node_id", longitude = "longitude", latitude = "latitude", name =
    NA_character_, weight = NA_character_),
  flow_columns = c(origin = "origin_id", destination = "destination_id", count = "flow",
    time = NA_character_),
  timezone = "UTC",
  time_format = NULL,
  output_dir = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{nodes}{A node data frame/data.table, a path to a CSV, CSV.GZ or RDS, or a \code{traffic_od_data} object.}

\item{flows}{An OD data frame/data.table, or a path to a CSV, CSV.GZ or RDS. Omit when \code{nodes} is a \code{traffic_od_data} object.}

\item{radius_m}{Positive aggregation radius in metres.}

\item{node_columns}{Named character vector mapping canonical keys to source columns, with \code{id}, \code{longitude}, \code{latitude}
and optional \code{name} and \code{weight} mappings. Aggregation uses unit node weights by default. Explicit weights must be strictly positive and finite; they are separate from display sizes.}

\item{flow_columns}{Named character vector mapping canonical keys to source columns, with \code{origin}, \code{destination} and
optional \code{count} and \code{time} mappings. Use \code{NA} for \code{count} when every row is
one trip, or for \code{time} when the table has no time dimension.}

\item{timezone}{IANA timezone used when character timestamps are parsed.}

\item{time_format}{Optional format passed to \code{\link[=as.POSIXct]{as.POSIXct()}}.}

\item{output_dir}{Optional directory for standardized CSV and JSON outputs.}

\item{overwrite}{Whether existing generated files may be replaced.}
}
\value{
An \code{od_aggregation} list containing standardized \code{nodes}, \code{flows},
\code{membership}, \code{metadata}, and optionally \code{files}.
}
\description{
It accepts data frames or CSV/RDS paths, validates identifiers, coordinates,
times and flow counts, aggregates nearby nodes at an arbitrary radius, remaps
both OD endpoints, and verifies that the total flow is conserved. Column names
are supplied through named mappings, so unrelated columns are ignored and the
function is not tied to a particular data provider.
}
\details{
The public input boundary normalizes numeric text and factor labels before the
unchanged aggregation algorithm. Malformed or nonfinite numeric fields fail.
Flow counts must be nonnegative integers; weighted OD is unsupported. Coordinates
must already be WGS84 longitude/latitude degrees, with no CRS or unit guessing.
A canonical OD object supplies its stored tables, identifier and coordinate
mappings, flow mappings and timezone when these are omitted. Its display size
is not used as an aggregation weight: the default is one per node, including
nodes with zero display size. To use positive canonical display sizes as
aggregation weights, explicitly supply
\code{node_columns=c(id="node_id", longitude="longitude", latitude="latitude", weight="member_weight")}.
This opt-in requires every mapped weight to be strictly positive and finite.
Missing
optional fields in default mappings are ignored; explicitly mapped source
columns must exist. The aggregation algorithm and flow-conservation check are
unchanged by normalization.

Character times with Z or numeric offsets preserve absolute instants; unzoned
times use timezone. Exported UTC CSV timestamps use the same parser on re-read.
}

\seealso{\code{\link{traffic_od_data}}, \code{\link{traffic_od_map}}}
