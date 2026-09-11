#' Specify static Gaussian experts
#'
#' The coefficient prior is N(m0, C0), independent of the observation variance.
#' The variance prior has density proportional to x^(-shape-1) exp(-scale/x).
#' One common proper prior is used independently for each of the fixed K experts.
#' No near-static random walk is substituted for a static expert.
#' @param family Currently only "gaussian".
#' @param design Finite numeric T-by-p design, in panel time order.
#' @param mode Currently only "static".
#' @param prior List with m0 (length p), C0 (positive definite p-by-p covariance),
#'   and variance (list with positive shape and scale).
#' @param state Must be NULL; dynamic states are a later milestone.
#' @return A validated gmde_expert specification.
#' @export
gmde_expert <- function(family, design, mode = "static", prior, state = NULL) {
  if (!identical(family, "gaussian") || !identical(mode, "static") ||
      !is.null(state))
    stop("Milestone 1 supports only Gaussian/static experts with state = NULL.",
         call. = FALSE)
  .gmde_matrix(design, "design")
  p <- ncol(design)
  prior <- .gmde_options(prior, list(m0 = NULL, C0 = NULL, variance = NULL), "prior")
  if (!is.numeric(prior$m0) || length(prior$m0) != p ||
      any(!is.finite(prior$m0))) stop("prior$m0 must be finite with length p.")
  .gmde_matrix(prior$C0, "prior$C0")
  if (!identical(dim(prior$C0), c(p, p)) ||
      !all(prior$C0 == t(prior$C0)))
    stop("prior$C0 must be symmetric and p-by-p.")
  root <- t(.gmde_chol(prior$C0, "prior$C0"))
  prior$variance <- .gmde_options(prior$variance, list(shape = NULL, scale = NULL),
                             "variance prior")
  .gmde_positive(prior$variance$shape, "variance shape")
  .gmde_positive(prior$variance$scale, "variance scale")
  structure(list(family = family, mode = mode, design = design, prior = prior,
                 state = NULL, root = root, white_design = design %*% root),
            class = "gmde_expert")
}
