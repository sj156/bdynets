\name{gls_sa_pgss}
\alias{gls_sa_pgss}
\title{Forecast independent count series with a state-augmented Poisson--Gamma filter}
\usage{
gls_sa_pgss(Y, train, validation, test,
  grid = expand.grid(discount = c(.70, .75, .80, .90, .95, .98, .995),
    phi = c(.5, .8, .95), sigma = c(.02, .05, .15, .30)),
  tuning_particles = 300L, particles = 2000L, seed = 1L)
}
\arguments{
\item{Y}{Time-by-series count matrix.}
\item{train, validation, test}{Consecutive row indices partitioning Y in order.}
\item{grid}{Discount medians, AR(1) persistence parameters and innovation standard deviations.}
\item{tuning_particles, particles}{Particle counts for model selection and final filtering.}
\item{seed}{Positive integer seed.}
}
\description{Uses the state-augmented Poisson--Gamma model of Irie, Glynn and Aktekin
(2022, \doi{10.1214/21-AOAS1505}). Each series starts with a unit-shape, unit-rate
Gamma prior and stationary logit-discount particles. Validation log predictive density
selects the parameters per series. Test counts are assimilated after forecasting.}
\value{Test-row matrices of means, predictive log probabilities and pointwise 95 percent
count interval bounds, validation scores, selected parameters and settings.}
