\name{traffic_od_data}
\alias{traffic_od_data}
\alias{plot.traffic_od_data}
\title{Construct canonical integer trip-count OD input}
\description{Normalize explicitly mapped node and trip tables into a reusable OD
object for mapping and spatial aggregation.}
\usage{
traffic_od_data(nodes, flows = NULL,
  node_columns = c(id = "node_id", longitude = "longitude",
    latitude = "latitude", size = "member_weight"),
  flow_columns = c(origin = "origin_id", destination = "destination_id",
    count = "flow", time = "time"),
  unit = "trips", crs = "EPSG:4326", timezone = "UTC")
\method{plot}{traffic_od_data}(x, timezone = x$timezone, ...)
}
\arguments{
\item{nodes}{Node data frame or CSV/CSV.GZ/RDS path, or an
\code{od_aggregation} object. Node IDs must be unique and nonmissing.}
\item{flows}{Flow data frame or CSV/CSV.GZ/RDS path. Omit when \code{nodes}
is an \code{od_aggregation} object. Every endpoint must match a node ID.}
\item{node_columns}{Named character vector mapping canonical keys \code{id},
\code{longitude}, \code{latitude} and optional \code{size} to source column
names. Coordinates must be finite longitude/latitude degrees, with longitude
in [-180,180] and latitude in [-85.0511,85.0511]. Sizes, if supplied, must be
nonnegative and finite; zero is valid. Size controls optional display scaling, not aggregation weights. Missing size in the default mapping uses one per node.}
\item{flow_columns}{Named character vector mapping canonical keys \code{origin},
\code{destination} and optional \code{count} and \code{time} to source column
names. Counts must be nonnegative integers. Omitted count uses one trip per
row. Omitted time means no time dimension. Missing optional fields in the
default mapping are ignored; an explicit mapping must name existing source
columns or use \code{NA} for omitted optional fields.}
\item{unit}{Exactly \code{"trips"}. Fractional flow counts and other OD units are
unsupported. No unit conversion is performed.}
\item{crs}{Declared WGS84 longitude/latitude coordinates: \code{"EPSG:4326"},
\code{"WGS84"} or \code{"OGC:CRS84"}. No CRS detection or transformation is
performed.}
\item{timezone}{Recognized IANA timezone for parsing unzoned character times.
The plot method defaults to the object's stored timezone.}
\item{x}{A \code{traffic_od_data} object.}
\item{...}{Arguments passed to \code{\link{traffic_od_map}}, such as
\code{basemap=NULL}, time filters, event overlays and output options.}
}
\details{
Mappings run from canonical field keys to source column names, for example
\code{c(origin="from", destination="to", count="trips")}. Numeric character
fields and factor labels are parsed as numbers; factor integer codes are never
used as measurements. Malformed text, nonfinite values and invalid references
fail before rendering or aggregation.

Optional times are POSIXt instants or explicit ISO character timestamps.
Character timestamps containing Z or a numeric offset retain their absolute
instant; unzoned timestamps use the declared timezone. Numeric clocks are not
accepted as OD absolute timestamps.

The constructor retains normalized canonical node and flow tables, canonical
mappings, original mappings in \code{source_columns}, and declared unit, CRS
and timezone. It validates input without aggregating repeated OD rows or
changing the reference directed-OD drawing algorithm or its five default
trip-count colours. \code{plot(x)} calls \code{traffic_od_map(x)};
\code{aggregate_od_data(x, radius_m=...)} uses the same canonical input with
unit aggregation weights by default. Canonical \code{member_weight} stores
display sizes, including zeros; it becomes an aggregation weight only when
explicitly mapped as \code{weight="member_weight"} in aggregation's
\code{node_columns}. Such explicit weights must be strictly positive and finite.
}
\value{\code{traffic_od_data} returns a list of class \code{traffic_od_data}.
Its canonical nodes contain \code{node_id}, \code{longitude}, \code{latitude}
and \code{member_weight}; flows contain \code{origin_id}, \code{destination_id},
\code{flow} and, when supplied, \code{time}. The plot method invisibly returns
the result of \code{traffic_od_map}.}
\seealso{\code{\link{traffic_od_map}}, \code{\link{aggregate_od_data}}}
\examples{
nodes <- data.frame(station = c("a", "b"),
  lon = c(-87.63, -87.62), lat = c(41.88, 41.89))
flows <- data.frame(from = "a", to = "b", trips = 3L)
x <- traffic_od_data(nodes, flows,
  node_columns = c(id = "station", longitude = "lon", latitude = "lat"),
  flow_columns = c(origin = "from", destination = "to", count = "trips"))
plot(x, basemap = NULL, draw = FALSE)
}
