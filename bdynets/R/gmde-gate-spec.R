#' Configure adaptive graph guidance
#'
#' Binary fits always use one shared weight and one Beta prior. Multiclass fits
#' default to class-specific weights. Fixed endpoints apply to every class;
#' mixed endpoint/interior vectors are not supported in this reference version.
#' At a = 0 the gate is logistic-normal, not a Dirichlet mixture.
#' @param mode "adaptive" or "fixed".
#' @param a Initial adaptive weight(s), strictly between zero and one, or fixed
#'   weight(s). Supply a scalar or, for class-specific fits, K entries.
#' @param shared Logical; tie weights for K >= 3. Always tied for K = 2.
#' @param s_b Positive global intercept prior standard deviation.
#' @param beta_shape1,beta_shape2 Positive Beta prior shapes, counted once per
#'   free weight, including the single binary/shared weight.
#' @param rw_sd Positive logit random-walk standard deviation, fixed throughout
#'   the chain. There is no automatic tuning in version 0.1.0.
#' @return A gmde_gate specification, resolved against K when fitting.
#' @export
gmde_gate <- function(mode = "adaptive", a = 0.5, shared = FALSE, s_b = 1,
                      beta_shape1 = 1, beta_shape2 = 1, rw_sd = 1) {
  if (length(mode) != 1L || !mode %in% c("adaptive", "fixed"))
    stop("gate mode must be adaptive or fixed.")
  if (!is.numeric(a) || !length(a) || any(!is.finite(a)) ||
      any(a < 0 | a > 1)) stop("a must contain weights in [0, 1].")
  if (mode == "adaptive" && any(a <= 0 | a >= 1))
    stop("Adaptive initial weights must be strictly inside (0, 1).")
  if (mode == "fixed" && any(a == 0 | a == 1) &&
      !(all(a == 0) || all(a == 1)))
    stop("Fixed endpoints must be shared by all classes.")
  if (!is.logical(shared) || length(shared) != 1L || is.na(shared))
    stop("shared must be TRUE or FALSE.")
  .gmde_positive(s_b, "s_b"); .gmde_positive(beta_shape1, "beta_shape1")
  .gmde_positive(beta_shape2, "beta_shape2"); .gmde_positive(rw_sd, "rw_sd")
  structure(list(mode = mode, a = a, shared = shared, s_b = s_b,
                 beta_shape1 = beta_shape1, beta_shape2 = beta_shape2,
                 rw_sd = rw_sd), class = "gmde_gate")
}

.gmde_resolve_gate <- function(gate, K) {
  gate$shared <- K == 2L || gate$shared
  if (K == 1L) {
    gate$a <- numeric(0); gate$free <- 0L
    return(gate)
  }
  if (!length(gate$a) %in% c(1L, K)) stop("a must have length 1 or K.")
  if (gate$shared && length(unique(gate$a)) != 1L)
    stop("Binary/shared gates require tied a values.")
  gate$a <- rep(gate$a, length.out = K)
  gate$free <- if (gate$mode == "fixed") 0L else if (gate$shared) 1L else K
  gate
}

.gmde_contrasts <- function(K) {
  if (K == 1L) return(matrix(numeric(0), 1L, 0L))
  H <- stats::contr.helmert(K)
  unname(sweep(H, 2L, sqrt(colSums(H^2)), "/"))
}

.gmde_class_factors <- function(H, a, independent = 1 - a) {
  d <- ncol(H)
  factor <- function(w) {
    if (all(w == 0)) return(matrix(0, d, d))
    if (all(w == 1)) return(diag(d))
    t(.gmde_chol(crossprod(H, w * H), "Gate contrast covariance"))
  }
  list(graph = factor(a), independent = factor(independent))
}

.gmde_white_draw <- function(n, m, d) {
  list(c = stats::rnorm(d), Gamma = matrix(stats::rnorm(m * d), m, d),
       E = matrix(stats::rnorm(n * d), n, d))
}

.gmde_utilities <- function(x, graph, H, gate, a, independent = 1 - a,
                       graph_product = graph$Phi %*% x$Gamma) {
  factors <- .gmde_class_factors(H, a, independent)
  F <- (gate$s_b * outer(rep(1, nrow(graph$Phi)), x$c) +
          graph_product %*% t(factors$graph) +
          graph$tau * x$E %*% t(factors$independent)) %*% t(H)
  if (any(!is.finite(F))) stop("Gate utilities overflow; no values were clipped.")
  F
}
