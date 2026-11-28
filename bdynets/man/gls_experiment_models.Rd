\name{gls_experiment_models}
\alias{gls_pnar_forecast}
\alias{gls_experiment_dss}
\alias{gls_static_forecast}
\title{Static count-network benchmarks and paired GLS projection selection}
\usage{
gls_pnar_forecast(Y, graph, covariates, train_rows, test_rows,
  order = 1L, pooled = FALSE, standardize = FALSE, pre_history = NULL)
gls_experiment_dss(fit, validation_rows, block_length = 8L,
  tolerance = .007, control = fit$control)
gls_static_forecast(Y, X, covariates, pairs, train_rows, test_rows,
  control = gls_control(), checkpoint_dir = NULL, chunk_size = 100L,
  checkpoint_every = 100L, resume = FALSE, cores = 1L)
}
\arguments{
\item{Y}{Nonnegative integer time-by-series matrix.}
\item{graph}{Graph object with series-aligned adjacency and weight matrices.}
\item{covariates}{Finite matrix with one row per observation time.}
\item{train_rows, test_rows, validation_rows}{Ordered row indices for chronological evaluation.}
\item{order}{One or two temporal lags.}
\item{pooled}{Fit a single coefficient vector shared by all targets.}
\item{standardize}{Center and scale logged lags using the training block.}
\item{pre_history}{Rows of counts preceding the observed series.}
\item{fit}{A fitted general GLS model.}
\item{block_length}{Length of circular validation blocks for standard errors of paired loss differences.}
\item{tolerance}{Allowed relative validation deviance increase over the continuous fit for the alternative selection rule.}
\item{control}{A GLS control object.}
\item{checkpoint_dir, chunk_size, checkpoint_every, resume}{Directory, retained samples per chunk, checkpoint interval, and whether to resume a matching static run. A temporary directory is used when omitted.}
\item{cores}{Maximum concurrent chains when parallel chains are enabled.}
\item{X}{Matrix of standardized lagged log counts for static regression.}
\item{pairs}{Candidate dictionary with target, source and positive affinity.}
}
\description{PNAR fits Poisson log-linear regressions with source-network averages and own-history terms. Static Bayesian regression uses the dynamic model's finite-shape observation approximation with signed persistent coefficients, fixed affinities and global-local half-Cauchy shrinkage. The projection selector evaluates all available candidate supports on validation using common random numbers and paired time-block uncertainty.}
\value{PNAR returns test means, log probabilities of the observed test counts, 95 percent count intervals, and coefficients. The static sampler returns test intensity draws, coefficient draws, and chain indices. Projection selection returns multipliers under the paired one-standard-error and relative-tolerance rules plus the scored validation path.}
