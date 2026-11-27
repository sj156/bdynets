\name{read_traffic}
\alias{read_traffic}
\alias{flow_scale}
\alias{traffic_summary}
\alias{traffic_trend}
\alias{traffic_report}
\alias{traffic_export}
\alias{plot.trafficmap_data}
\alias{plot.traffic_trend}
\title{Read and display prepared road frames}
\description{Read a prepared road series, display an interval, or export a visualization.}
\usage{
read_traffic(file)
flow_scale(values)
traffic_summary(x, index = NULL, region = NULL, mode = "flow",
  reference = NULL, threshold = NULL, comparison = NULL,
  metric = NULL, duration_rule = NULL)
traffic_trend(x, region = NULL, mode = "flow", reference = NULL,
  threshold = NULL, comparison = NULL, metric = NULL,
  duration_rule = NULL, method = "mean")
traffic_report(x, file, index = NULL, region = NULL, mode = "flow",
  reference = NULL, threshold = NULL, comparison = NULL,
  metric = NULL, duration_rule = NULL, trend_method = "mean")
traffic_export(x, output_dir, index = NULL, region = NULL,
  input_files = character(), mode = "flow", reference = NULL,
  threshold = NULL, comparison = NULL, metric = NULL,
  duration_rule = NULL, trend_method = "mean")
\method{plot}{trafficmap_data}(x, index = NULL, region = NULL,
  mode = "flow", reference = NULL, threshold = NULL, comparison = NULL,
  metric = NULL, duration_rule = NULL, ...)
\method{plot}{traffic_trend}(x, metric = NULL, index = NULL, ...)
}
\arguments{
\item{file}{For reading, an RDS path or a ready \code{trafficmap_data} object. For \code{traffic_report()}, the destination HTML file.}
\item{x}{A prepared \code{trafficmap_data} object, or a \code{traffic_trend} object for its plot method.}
\item{values}{Numeric values for the default Flow colour scale.}
\item{index}{One-based frame index. Omit to use the prepared initial frame if present, or frame 1.}
\item{region}{Optional exact region label for roads and associated events.}
\item{mode,reference,threshold,comparison,duration_rule}{Use the defaults. Prepare derived measurements first and pass the supplied name as \code{metric}.}
\item{metric}{A supplied metric name. The trend plot selects a column from its prepared summary.}
\item{method,trend_method}{Use \code{"mean"} for the mean of available road values or \code{"provided"} for the supplied trend series.}
\item{output_dir}{Directory for exported figures and display records.}
\item{input_files}{Optional source file paths to list in the export.}
\item{...}{Additional graphics options.}
}
\details{
\code{read_traffic()} returns the ready frames with their time values. Maps
format times for display. Source table alignment,
aggregation and derived measurements belong to preparation.
\code{traffic_summary()} and \code{traffic_trend()} describe the supplied
values. \code{traffic_report()} saves a static HTML summary and
\code{traffic_export()} writes rendered figures and their data records.
With no attached background, these helpers look for local context after
fitting the map to its graphics device. The export provenance records the
background used by each map file under \code{map_basemaps}.
Data exports use numeric times, with timestamp interpretation recorded in provenance.
Use the argument \code{metric} to display a metric already present in the
prepared frames.
}
\seealso{\code{\link{traffic_flow_map}}, \code{\link{traffic_speed_map}}, \code{\link{traffic_od_map}}}
