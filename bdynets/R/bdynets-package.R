#' Bayesian Dynamic Network Analysis
#'
#' One package with separate analysis modules. The `gmde_*` functions provide
#' fixed-K Gaussian/static graph-informed mixtures. `gibbs_sampler()` retains
#' the historical experimental dynamic count-mixture method. The `gls_*`
#' interfaces implement the GLS dynamic count-network module. Shared scalar
#' numerical kernels are exposed by [gaussian_ffbs()] and
#' [poisson_discount_filter()] and an installed C++ header. Each method keeps
#' its own model assumptions and inference logic.
#'
#' Package code is maintained directly in R scripts. R Markdown notes do not
#' generate or overwrite the package. See `gmde_fit()` for a working example.
#' 
#' @docType package
#' @keywords internal
#' @import stats
#' @importFrom utils head tail
#' @importFrom Rcpp evalCpp
#' @useDynLib bdynets, .registration = TRUE
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
