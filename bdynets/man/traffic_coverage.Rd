\name{traffic_coverage}
\alias{traffic_coverage}
\title{Summarize whole-period primary traffic coverage}
\description{Count roads, frames, primary Flow or Speed observations and reported
event starts over the full supplied period, optionally within one named region.}
\usage{
traffic_coverage(x, region = NULL)
}
\arguments{
\item{x}{A prepared \code{trafficmap_data} road series, such as the result of \code{read_traffic()} on a drawing RDS.}
\item{region}{One exact supplied geographic region label, or \code{NULL} for
all records. Unassigned records belong to the all-record scope.}
}
\details{
Coverage uses every supplied frame and the selected roads' primary measurement
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
