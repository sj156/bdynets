#' Replay the graphMoDE Poisson discount guide
#'
#' Deterministic Gamma log-moment matching and multivariate linear-Bayes
#' feedback. This is not a Laplace filter, exact Poisson filter or sampler.
#' The information proxy is computed from the undiscounted propagation.
#' All scientific inputs are explicit; no discount prior or adjustment default
#' is selected here. The current interface accepts one constant evolution
#' matrix and one fixed class size across the complete observation history.
#' @param sums Nonnegative integer aggregate counts, length T.
#' @param exposure Nonnegative integer class size N. Empty classes require zero sums.
#' @param design Finite T-by-p predictor design.
#' @param transition Finite p-by-p evolution matrix, constant over time.
#' @param initial_mean Finite length-p prior mean specified before observations.
#' @param initial_covariance Symmetric positive definite p-by-p prior covariance.
#' @param baseline Baseline discount in (0,1).
#' @param adjustment Positive fixed Poisson protection constant c.
#' @param tolerance Log-scale root tolerance, between 1e-14 and 1e-6.
#' @param max_iterations Maximum number of root-solver iterations.
#' @return A list of T-by-p means, p-by-p-by-T covariance arrays, effective
#'   discounts, information proxies, predictor moments and matched Gamma parameters.
#'   Arrays include all T transitions from the time-zero prior.
#' @export
gmde_discount_guide <- function(sums, exposure, design, transition,
    initial_mean, initial_covariance, baseline, adjustment,
    tolerance = 1e-12, max_iterations = 200L) {
    .gmde_scalar(exposure, "exposure", 0, .Machine$integer.max, TRUE)
    .gmde_scalar(baseline, "baseline", 0, 1)
    .gmde_positive(adjustment, "adjustment")
    .gmde_scalar(tolerance, "tolerance", 1e-14, 1e-6)
    .gmde_scalar(max_iterations, "max_iterations", 1, .Machine$integer.max, TRUE)
    .gmde_discount_guide_cpp(sums, exposure, design, transition, initial_mean,
        initial_covariance, baseline, adjustment, tolerance, as.integer(max_iterations))
}

#' Evaluate the state-transition term in the graphMoDE posterior
#'
#' Evaluates minus one half of the sum of log determinants and innovation
#' quadratic forms. Excludes the initial-state density and the constant
#' -T*p*log(2*pi)/2. Recompute the guide when its defining inputs change.
#' @param state Finite (T+1)-by-p matrix including the time-zero state.
#' @param transition Finite p-by-p constant evolution matrix.
#' @param innovation_covariance Symmetric positive definite p-by-p-by-T array.
#' @return The scalar transition log-density term D_k.
#' @export
gmde_transition_log_density <- function(state, transition, innovation_covariance) {
    .gmde_transition_log_density_cpp(state, transition, innovation_covariance)
}
