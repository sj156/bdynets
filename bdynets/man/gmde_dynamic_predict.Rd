\name{gmde_dynamic_predict}
\alias{gmde_dynamic_predict}
\title{Predict along simulated future discount-guide histories}
\usage{gmde_dynamic_predict(fit, future_design, histories = 4L, progress = NULL)}
\arguments{
\item{fit}{A dynamic fit with retained predictive states.}
\item{future_design}{Known future design matrix.}
\item{histories}{Independent future histories per retained draw.}
\item{progress}{Optional progress callback.}}
\description{Simulate future states and aggregate Poisson counts, update the guide, and average conditional one-step means. No held-out outcomes enter prediction. The caller records a separate predictive seed.}
\value{Predictive means, interleaved batch means and draw counts.}
