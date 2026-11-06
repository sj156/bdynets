.ss_response <- function(x, name) {
  if (length(dim(x)) > 2L) stop(name, " must be a vector or matrix.", call. = FALSE)
  x <- as.matrix(x)
  if (!is.numeric(x) || is.complex(x) || nrow(x) < 1L || ncol(x) < 1L)
    stop(name, " must be a nonempty numeric vector or matrix.", call. = FALSE)
  storage.mode(x) <- "double"
  x
}

.ss_matrix <- function(x, nt, k, name) {
  if (!is.numeric(x) || is.complex(x)) stop(name, " must be real numeric.", call. = FALSE)
  if (!is.null(dim(x))) {
    if (!identical(dim(x), c(nt, k)))
      stop(name, " must have the same dimensions as the observations.", call. = FALSE)
    storage.mode(x) <- "double"
    return(x)
  }
  if (!length(x) %in% c(1L, nt))
    stop(name, " must be scalar, length T, or a T by K matrix.", call. = FALSE)
  matrix(rep_len(as.double(x), nt), nt, k)
}

.ss_scalar <- function(x, name, lower = -Inf, strict = FALSE) {
  if (!is.numeric(x) || is.complex(x) || length(x) != 1L || !is.finite(x) ||
      if (strict) x <= lower else x < lower)
    stop(name, " must be a finite scalar in the required range.", call. = FALSE)
  as.double(x)
}

.ss_time_vector <- function(x, nt, name, positive = FALSE) {
  if (!is.numeric(x) || is.complex(x) || !length(x) %in% c(1L, nt) ||
      any(!is.finite(x)) || (positive && any(x <= 0)))
    stop(name, " must be finite and scalar or length T",
         if (positive) ", with positive entries." else ".", call. = FALSE)
  rep_len(as.double(x), nt)
}

#' Draw a scalar Gaussian state path by FFBS
#'
#' Draws from the conditional Gaussian distribution defined by
#' `state[t] = transition[t] * state[t-1] + innovation[t]` and
#' `response[t,a] = offset[t,a] + loadings[t,a] * state[t] + error[t,a]`.
#' The innovations have variance `innovation_variance[t]`, observation errors
#' have precision `precision[t,a]`, and `state[0]` has the specified Gaussian
#' initial distribution. The innovation at the first time is applied before
#' observing the first response.
#'
#' @param response Numeric vector of length T or T by K observation matrix.
#' @param loadings,precision,offset Scalars, length T vectors (repeated over
#'   series), or T by K matrices. Precisions must be nonnegative. A zero
#'   precision ignores that observation; its response, loading, and offset may
#'   be missing. Positive precision requires finite values.
#' @param innovation_variance Positive scalar or length T vector.
#' @param transition Finite scalar or length T transition vector.
#' @param initial_mean Finite mean of the state at time zero.
#' @param initial_variance Positive variance of the state at time zero.
#' @param jitter Nonnegative scalar, default zero. A positive value enables
#'   the legacy diagonal-jitter convention in covariance and precision solves,
#'   with up to ten tenfold increases. It perturbs the Gaussian calculations;
#'   leave zero for the stated model when numerically well conditioned.
#' @return A list with `state` (length T) and `initial` (the time-zero draw).
#' @details Uses R's RNG and consumes T+1 standard-normal draws, in terminal,
#'   backward, then initial order. This is a single latent state observed by K
#'   series, not a K-dimensional latent process. Conditional on supplied
#'   Gaussian parameters, FFBS is exact up to floating-point arithmetic when
#'   `jitter = 0`.
#' @export
#' @examples
#' set.seed(1)
#' y <- sin(seq_len(30) / 5) + rnorm(30, sd = .2)
#' draw <- gaussian_ffbs(y, precision = 25, innovation_variance = .05,
#'                       transition = .95)
#' length(draw$state)
gaussian_ffbs <- function(response, loadings = 1, precision = 1,
                          innovation_variance = 1, transition = 1,
                          initial_mean = 0, initial_variance = 1, jitter = 0,
                          offset = 0) {
  y <- .ss_response(response, "response")
  nt <- nrow(y); k <- ncol(y)
  h <- .ss_matrix(loadings, nt, k, "loadings")
  om <- .ss_matrix(precision, nt, k, "precision")
  base <- .ss_matrix(offset, nt, k, "offset")
  q <- .ss_time_vector(innovation_variance, nt, "innovation_variance", TRUE)
  phi <- .ss_time_vector(transition, nt, "transition")
  m0 <- .ss_scalar(initial_mean, "initial_mean")
  C0 <- .ss_scalar(initial_variance, "initial_variance", 0, TRUE)
  eps <- .ss_scalar(jitter, "jitter", 0)
  .ffbs_scalar_cpp(y - base, h, om, q, phi, m0, C0, eps)
}

#' Poisson Laplace discount-reference filter
#'
#' Computes a scalar Gaussian reference recursion for independent Poisson
#' counts with log means `offset[t,a] + loadings[t,a] * state[t]`.
#' At each time, the prior variance is the previous variance divided by
#' `discount`; a bracketed scalar solver finds the posterior mode and inverse
#' curvature supplies the new variance. This is a Laplace approximation,
#' not exact Poisson filtering.
#'
#' @param counts Nonnegative integer vector of length T or T by K matrix.
#' @param loadings,offset Finite scalars, length T vectors (repeated over series),
#'   or T by K matrices.
#' @param discount Scalar strictly between zero and one.
#' @param initial_mean Finite initial state mean.
#' @param initial_variance Positive initial state variance.
#' @param tolerance Positive absolute root-solver tolerance.
#' @param max_iterations Positive integer root-solver iteration limit.
#' @return A list with `innovation_variance` (length T, computed before each
#'   observation), `filtered_mean`, `filtered_variance` (length T),
#'   `terminal_mean`, and `terminal_variance`.
#' @details Deterministic; consumes no RNG. No model-specific small-variance
#'   fallback or variance clipping is applied. If this reference is used in an
#'   MCMC acceptance correction, the calling model must assess the numerical
#'   sensitivity of its density ratios.
#' @export
#' @examples
#' f <- poisson_discount_filter(c(2, 3, 1, 4), loadings = .2,
#'                              offset = log(3), discount = .95)
#' f$terminal_variance
poisson_discount_filter <- function(counts, loadings = 1, offset = 0,
                                     discount = .95, initial_mean = 0,
                                     initial_variance = 1, tolerance = 1e-10,
                                     max_iterations = 1000L) {
  y <- .ss_response(counts, "counts")
  if (any(!is.finite(y)) || any(y < 0) || any(y != floor(y)))
    stop("counts must contain finite nonnegative integers.", call. = FALSE)
  nt <- nrow(y); k <- ncol(y)
  h <- .ss_matrix(loadings, nt, k, "loadings")
  base <- .ss_matrix(offset, nt, k, "offset")
  d <- .ss_scalar(discount, "discount", 0, TRUE)
  if (d >= 1) stop("discount must be strictly less than one.", call. = FALSE)
  m0 <- .ss_scalar(initial_mean, "initial_mean")
  C0 <- .ss_scalar(initial_variance, "initial_variance", 0, TRUE)
  tol <- .ss_scalar(tolerance, "tolerance", 0, TRUE)
  limit <- .ss_scalar(max_iterations, "max_iterations", 0, TRUE)
  if (limit != floor(limit) || limit > .Machine$integer.max)
    stop("max_iterations must be a positive integer.", call. = FALSE)
  .poisson_reference_cpp(y, h, base, d, m0, C0, tol, as.integer(limit))
}
