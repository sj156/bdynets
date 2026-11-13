\name{traffic_input}
\alias{traffic_input}
\title{Prepare standard road, event and origin-destination tables}
\description{Construct and validate a named list for exchanging data between case preparation and plotting. Table attributes and source identifiers remain available after saving with saveRDS.}
\usage{
traffic_input(roads = NULL, flow = NULL, events = NULL, nodes = NULL,
  od = NULL, times = NULL, time_labels = NULL, interval_seconds = NULL,
  unit = "unit unspecified", timezone = NULL, od_interval_seconds = NULL,
  od_time_kind = NULL, measurements = NULL, metric_info = list(),
  trends = NULL, provenance = list(), title = "Traffic map",
  description = "", preparation = character(), notes = character(),
  basemap = NULL, crs = "EPSG:4326")
}
\arguments{
\item{roads,flow,events}{Canonical road, observation and event tables, as in \code{\link{traffic_data}}.}
\item{nodes,od}{Node table with node_id, longitude and latitude. Directed trip table with origin_id, destination_id, integer flow, and optional time. Both tables may include source columns.}
\item{times,time_labels,interval_seconds,unit}{Road frame starts, source labels, widths in seconds and the primary road measurement unit.}
\item{timezone}{IANA timezone for OD clock text. NULL preserves an unspecified road clock.}
\item{od_interval_seconds,od_time_kind}{Optional width in seconds for OD period counts. Time interpretation is instant, interval_start, untimed, or unspecified.}
\item{measurements,metric_info,trends}{Additional road measurements, definitions and supplied time series as in \code{\link{traffic_data}}.}
\item{provenance}{Source names and preparation counts.}
\item{title,description,preparation,notes,basemap,crs}{Descriptive fields, optional map context and the declared longitude/latitude coordinate system, as in \code{\link{traffic_data}}.}
}
\value{A list with schema bdynets-traffic-input-v1. Absent optional components are NULL. Read the complete saved object with readRDS. The legacy read_traffic reader provides its road/event view.}
\examples{
nodes <- data.frame(node_id = c("A", "B"), longitude = c(-87.62, -87.64),
                    latitude = c(41.88, 41.89))
od <- data.frame(origin_id = "A", destination_id = "B", flow = 4L)
input <- traffic_input(nodes = nodes, od = od)
input$od
}
