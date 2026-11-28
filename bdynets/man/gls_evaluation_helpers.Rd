\name{gls_evaluation_helpers}
\alias{gls_lag_predictors}
\alias{gls_forecast_metrics}
\alias{gls_trajectory_metrics}
\alias{gls_poisson_mixture_forecast}
\alias{poisson_deviance}
\title{Common predictors and evaluation measures for count networks}
\description{Construct lag predictors from training counts, summarize posterior draws, and compare forecasts or coefficient trajectories on matched observations.}
\usage{
gls_lag_predictors(Y, train_rows, pre_history = NULL)
gls_forecast_metrics(observed, forecast, intensity = NULL, windows = list())
gls_trajectory_metrics(truth, estimate, selected,
  groups = list(all = rep(TRUE, ncol(truth))))
gls_poisson_mixture_forecast(lambda, observed, draws = NULL, seed = NULL)
poisson_deviance(y, mu)
}
\arguments{
\item{Y, observed}{Matrices with times in rows and series in columns.}
\item{train_rows}{Distinct row indices used for centering and scaling the one-bin log-count lags.}
\item{pre_history}{Optional preceding count matrix. If absent, the first observation provides the initial lag.}
\item{forecast}{A list with a forecast mean matrix named \code{mean}, optional log predictive probabilities \code{logprob} and optional 95 percent count limits \code{lower} and \code{upper}.}
\item{intensity}{Optional matrix of true generating means.}
\item{windows}{Named list of logical masks matching the observations. The entire panel is always scored.}
\item{truth, estimate}{Matching finite time-by-pair coefficient matrices on a common predictor scale.}
\item{selected}{Logical support indicators for the columns of the coefficient matrices.}
\item{groups}{Named logical masks selecting groups of ordered pairs.}
\item{lambda, draws}{Time-by-series-by-draw arrays of Poisson intensities and optional predictive count draws.}
\item{seed}{Optional seed for drawing predictive counts when draws are omitted.}
\item{y, mu}{Observed counts and corresponding predicted means.}
}
\value{Lag prediction returns \code{X}, \code{center} and \code{scale}. Mixture prediction returns matrices of means, log predictive probabilities and interval bounds. The two metric functions return window, metric, value and cell count. Empty denominators give missing values. Poisson deviance returns twice the per-cell log-likelihood difference from a saturated model.}
