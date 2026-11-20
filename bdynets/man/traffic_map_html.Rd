\name{traffic_map_html}
\alias{traffic_map_html}
\title{Write a Self-Contained Interactive Road Map}
\description{
Write the existing self-contained HTML road-map viewer. Static maps use
\code{traffic_flow_map} or \code{traffic_od_map} instead.
}
\usage{
traffic_map_html(x, file, mode = "flow", reference = NULL,
  threshold = NULL, comparison = NULL, metric = NULL,
  duration_rule = NULL, trend_method = "mean")
}
\arguments{
\item{x}{A \code{trafficmap_data} object or compatible RDS input bundle.}
\item{file}{HTML output path.}
\item{mode}{Road measurement mode.}
\item{reference}{Optional reference frame for change mode.}
\item{threshold,comparison,duration_rule}{Explicit duration-view parameters.}
\item{metric}{Optional supplied road metric.}
\item{trend_method}{\code{"mean"} or \code{"provided"}.}
}
\value{The output path, invisibly.}
\seealso{\code{\link{traffic_flow_map}}}
