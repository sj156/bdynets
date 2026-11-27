\name{gls_forecast_summary}
\alias{gls_forecast_summary}
\title{Compact posterior predictive count summaries}
\usage{
gls_forecast_summary(prediction, Y, level = 0.95, quantile_type = 1L)
}
\arguments{
\item{prediction}{A GLS prediction containing intensity and count draws, forecast rows, and a label.}
\item{Y}{Full observed time-by-series count matrix.}
\item{quantile_type}{Quantile convention, with integer count limits from type 1 and legacy interpolated limits from type 7.}
\item{level}{Central predictive interval probability.}
}
\description{Aggregates forecast draws to pointwise predictive means, count intervals and log predictive probabilities.}
\value{One row per forecast time and series, including observed count, mean intensity, interval bounds and log score.}
