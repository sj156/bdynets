\name{gmde_discount_guide}
\alias{gmde_discount_guide}
\title{Replay the graphMoDE Poisson discount guide}
\usage{
gmde_discount_guide(sums, exposure, design, transition, initial_mean,
  initial_covariance, baseline, adjustment, tolerance = 1e-12,
  max_iterations = 200L)
}
\arguments{
\item{sums}{Nonnegative integer aggregate counts of length T.}
\item{exposure}{Nonnegative integer class size; empty classes require zero counts.}
\item{design}{Finite T-by-p predictor design.}
\item{transition}{Constant finite p-by-p evolution matrix.}
\item{initial_mean}{Finite length-p initial mean specified before observations.}
\item{initial_covariance}{Symmetric positive definite p-by-p initial covariance.}
\item{baseline}{Baseline discount in (0,1).}
\item{adjustment}{Positive fixed Poisson protection constant c.}
\item{tolerance}{Log-scale root tolerance between 1e-14 and 1e-6.}
\item{max_iterations}{Positive root-solver iteration limit.}
}
\description{Deterministic Gamma log-moment matching and multivariate
linear-Bayes feedback for the graphMoDE discount model. This is neither a
Laplace filter nor an exact Poisson filter or a posterior sampler. The
information proxy uses undiscounted propagation.}
\value{A list of T-by-p means, p-by-p-by-T covariance arrays, effective
 discounts, information proxies, predictor moments and matched Gamma parameters.
 All T transitions from the time-zero prior are included.}
\details{No scientific prior or adjustment default is selected. One constant
 evolution matrix and one class size are accepted for the complete history.
 Numerical failures are explicit; there is no covariance jitter or clipping.
 The guide's predictive likelihood must not be added to the posterior target.}
