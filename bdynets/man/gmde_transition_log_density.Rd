\name{gmde_transition_log_density}
\alias{gmde_transition_log_density}
\title{Evaluate the graphMoDE state-transition term}
\usage{gmde_transition_log_density(state, transition, innovation_covariance)}
\arguments{
\item{state}{Finite (T+1)-by-p state path including time zero.}
\item{transition}{Constant finite p-by-p evolution matrix.}
\item{innovation_covariance}{Symmetric positive definite p-by-p-by-T array.}
}
\description{Evaluates minus one half of the sum of log determinants and
innovation quadratic forms. The initial-state density and the constant
-T*p*log(2*pi)/2 are excluded.}
\value{The scalar transition log-density term D_k.}
\details{Recompute the guide whenever its defining inputs change. Covariances
 are evaluated without jitter, truncation or numerical repair.}
