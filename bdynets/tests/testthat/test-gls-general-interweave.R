test_that("general loading-factor move uses the joint density and the time-zero Jacobian", {
  cfg <- gls_control("smoke", reference_backend = "R")
  Y <- matrix(c(2, 4, 3, 1, 3, 2), 3, 2)
  H <- array(c(-0.3, 0.7, 0.6, 0.6, -0.2, 0.1), c(3, 2, 1))
  offset <- matrix(log(3), 3, 2)
  beta <- matrix(c(0.8, -0.5, 0.4), ncol = 1)
  attr(beta, "initial") <- 1.2
  L <- matrix(c(0.1, -0.8), ncol = 1)
  prior_var <- c(0.5, 1.5)
  discount <- 0.90
  ref <- bdynets:::reference_path(Y, H, offset, discount, cfg)
  log_density <- function(beta, L, ref) {
    initial <- attr(beta, "initial")
    dnorm(initial, log = TRUE) +
      sum(dnorm(diff(c(initial, beta)), sd = sqrt(ref$W), log = TRUE)) +
      sum(dnorm(L, sd = cfg$dynamic_loading_scale * sqrt(prior_var), log = TRUE))
  }
  old <- log_density(beta, L, ref)
  for (seed in c(1L, 12L, 26L, 150L)) {
    set.seed(seed)
    delta <- rnorm(1L, sd = 0.4)
    scale <- exp(delta)
    log_u <- log(runif(1L))
    candidate_beta <- beta * scale
    attr(candidate_beta, "initial") <- 1.2 * scale
    candidate_ref <- bdynets:::reference_path(Y, H / scale, offset, discount, cfg)
    # Four states (including time zero) grow, and two loadings shrink.
    log_ratio <- log_density(candidate_beta, L / scale, candidate_ref) - old + 2 * delta
    accepted <- log_u < log_ratio
    set.seed(seed)
    ans <- bdynets:::interweave_general_loading_factor(Y, H, offset, beta, L, prior_var,
      discount, ref, 0.4, cfg)
    expect_identical(ans$accepted, as.integer(accepted))
    expect_equal(ans$beta, if (accepted) candidate_beta else beta)
    expect_equal(ans$L, if (accepted) L / scale else L)
    expect_equal(ans$reference, if (accepted) candidate_ref else ref)
    expect_equal(ans$jump, if (accepted) abs(delta) else 0)
    expect_equal(sweep(ans$H, 1L, ans$beta[, 1L], "*"), sweep(H, 1L, beta[, 1L], "*"))
  }
})
