\name{gmde_dynamic_fit}
\alias{gmde_dynamic_fit}
\title{Development sampler for the Poisson discount graphMoDE model}
\usage{gmde_dynamic_fit(input, method, config, progress = NULL)}
\arguments{
\item{input}{A list containing training count matrix \code{Y}, design \code{Fmat}, binary symmetric \code{adjacency}, and symmetric \code{edge_lengths}.}
\item{method}{One of \code{"Exp-W"}, \code{"SE-W"}, or \code{"C"}.}
\item{config}{An explicit list. Required scientific entries: \code{K}, \code{initial_mean}, \code{initial_covariance}, two positive Beta parameters in \code{discount_prior}, \code{adjustment}, \code{nu}, \code{tau}, and fixed log-range prior center \code{u0}. Required computational entries: positive integer \code{rho}, \code{block_size}, \code{iterations}, \code{warmup}, \code{initial_groups}, \code{ess_limit}; positive \code{discount_step}, \code{kappa_step}, \code{h_step}, \code{initial_h}; finite \code{initial_log_kappa}. No scientific defaults are selected.}
\item{progress}{Optional function called after each completed sweep with iteration, trace, elapsed seconds, and current state.}
}
\description{A separate dynamic Poisson API retaining independent full-rank softmax fields and guide-defined innovation covariances. Existing static interfaces are unchanged.}
\details{The sampler uses exact integer-shape Devroye Polya--Gamma draws, multivariate information-form FFBS with conditional temporal boundaries, and the Poisson/NB Metropolis correction. Discount proposals and sequential label proposals replay the deterministic guide. Empty experts receive joint prior draws. Fields use sequential column ESS; covariance parameters use log-coordinate MH with the required Jacobians. The transition is the identity. Raw graph weights and covariance scales are not normalized. Initialization uses only training counts. All K classes and field columns remain present. Numerical failures stop the run; no jitter, clipped intensity, approximate PG fallback, or silent graph-edge deletion is used. This is a development API, not a claim of convergence or formal validation for all parameter values.}
\value{A list with traces, discount traces, retained labels, posterior similarity matrix, fitted intensity mean, terminal state, elapsed seconds, configuration and development status. Raw draws should remain external to version control.}
