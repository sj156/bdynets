\name{traffic_coverage}
\alias{traffic_coverage}
\title{Summarize whole-period primary traffic coverage}
\description{Count roads, frames, primary road-flow observations and reported
event starts over the full supplied period, optionally within one named region.}
\usage{
traffic_coverage(x, region = NULL)
}
\arguments{
\item{x}{A \code{trafficmap_data} object from \code{\link{traffic_data}}.}
\item{region}{One exact supplied geographic region label, or \code{NULL} for
all records. Unassigned records belong to the all-record scope.}
}
\details{
Coverage uses every supplied frame and the selected roads' primary source-flow
matrix. It is independent of the displayed frame, map viewport, measurement
mode and layer visibility. Zero is observed; absent road-time pairs and explicit
missing values count as missing. Event starts are counted across the entire
selected region and period. No map context is acquired.

Use \code{\link{traffic_summary}} for selected-interval distributions and
\code{\link{traffic_trend}} for per-frame series. Coverage is not a duration,
rate, unique-event-category count or aggregation of additional metrics.
}
\value{A one-row data frame with columns \code{roads}, \code{frames},
\code{road_time_cells}, \code{observed}, \code{missing}, \code{zero} and
\code{event_starts}. The cell count is roads times frames; observed plus
missing equals the cell count.}
\examples{
events <- data.frame(time = 0, lon = -87.63, lat = 41.88)
x <- traffic_data(events = events, interval_seconds = 3600)
traffic_coverage(x)
}
