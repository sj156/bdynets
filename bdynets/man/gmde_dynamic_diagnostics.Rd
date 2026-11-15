\name{gmde_dynamic_diagnostics}
\alias{gmde_dynamic_diagnostics}
\title{Summarize completed development chains}
\usage{gmde_dynamic_diagnostics(fits)}
\arguments{\item{fits}{A list of at least two completed \code{gmde_dynamic_fit} results with identical scientific settings and lengths.}}
\description{Computes label-invariant rank-normalized R-hat and bulk/tail ESS using the posterior package, per-chain acceptance and occupancy summaries, and pairwise off-diagonal posterior similarity RMS differences.}
\details{Constant chains are retained and fail the diagnostic flag. Thresholds are fixed at R-hat below 1.01 and both ESS values at least 400. These are development checks, not certification of convergence or a formal simulation release.}
\value{A list of diagnostics, chain summaries, PSM discrepancies, thresholds, and development status.}
