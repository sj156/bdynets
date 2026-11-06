general_block_fixture <- function() {
  set.seed(904)
  n <- 14L; k <- 5L
  pairs <- expand.grid(target = seq_len(k), source = seq_len(k))
  beta <- matrix(rnorm(n), n, 1L); attr(beta, "initial") <- 0.2
  list(X = matrix(rnorm(n*k), n, k), Q = matrix(rnorm(n*2), n, 2L),
    Y = matrix(rpois(n*k, 2), n, k), ystar = matrix(rnorm(n*k), n, k),
    omega = matrix(runif(n*k, .3, 1), n, k), beta = beta,
    pairs = pairs, prior_var = rep(.04, nrow(pairs)), means = rep(2, k),
    current = list(alpha = rnorm(k), gamma = matrix(rnorm(k*2), k, 2L),
      B = rnorm(nrow(pairs), sd = .1), L = matrix(rnorm(nrow(pairs), sd = .02), nrow(pairs), 1L)))
}

test_that("general block controls match Simulation 1 and reject invalid steps", {
  g <- gls_control(); s <- gls_study_control()
  for (name in c("static_target_block_size", "static_pcn_initial_step",
    "static_pcn_target_acceptance", "static_pcn_adapt_rate", "static_pcn_min_step",
    "static_pcn_max_step", "initial_loading_sd", "startup_min_static_acceptance"))
    expect_equal(g[[name]], s[[name]], info = name)
  expect_error(gls_control(static_target_block_size = 0), "block_size")
  expect_error(gls_control(static_target_block_size = 1.5), "block_size")
  expect_error(gls_control(static_pcn_initial_step = 0), "pCN")
  expect_error(gls_control(static_pcn_initial_step = 1.1), "pCN")
  expect_error(gls_control(initialization = "unknown"), "initialization")
  expect_error(gls_control(startup_check_iteration = 0), "startup_check")
  expect_equal(gls_control(static_target_block_size = Inf)$static_target_block_size, Inf)
  old <- g; old$static_pcn_initial_step <- NULL
  expect_equal(GLS:::.validate_gls_control(old)$static_pcn_initial_step, .8)
})

test_that("general static proposal delegates exactly to the Simulation 1 pCN kernel", {
  f <- general_block_fixture(); cfg <- gls_control(); targets <- c(2L, 5L)
  shared <- cfg; shared$c_ell <- cfg$dynamic_loading_scale
  set.seed(712)
  expected <- GLS:::.gls_study_draw_static_targets(targets, f$ystar, f$omega, f$X, f$Q,
    rbind(attr(f$beta, "initial"), f$beta), f$pairs, f$current$alpha, f$current$gamma,
    f$current$B, f$current$L, f$prior_var, f$means, .6, shared)
  set.seed(712)
  actual <- GLS:::update_general_block(f$ystar, f$X, f$Q, f$beta, f$omega,
    f$pairs, f$prior_var, f$means, cfg, targets, f$current, .6)
  expect_identical(actual$alpha, expected$alpha)
  expect_identical(actual$gamma, expected$gamma)
  expect_identical(actual$B, expected$theta_bar)
  expect_identical(actual$L, expected$L)
  outside <- !f$pairs$target %in% targets
  expect_identical(actual$B[outside], f$current$B[outside])
  expect_identical(actual$L[outside, , drop = FALSE], f$current$L[outside, , drop = FALSE])
})

test_that("target blocks keep accepted references and roll back rejected coefficients", {
  f <- general_block_fixture()
  cfg <- gls_control(static_target_block_size = 2L)
  visits <- numeric()
  local_mocked_bindings(accept_reference = function(Y, H, offset, beta, discount, ref, cfg) {
    visits <<- c(visits, ref$marker)
    ref$marker <- ref$marker + 1
    list(accepted = TRUE, ref = ref)
  }, .package = "GLS")
  result <- GLS:::update_general_targets_with_reference(f$ystar, f$X, f$Q, f$Y,
    f$beta, f$omega, f$pairs, f$prior_var, f$means, f$current, .95,
    list(marker = 0), .8, cfg)
  expect_identical(visits, 0:2 * 1)
  expect_equal(result$accepted, 3L)
  expect_equal(result$proposed, 3L)
  expect_equal(result$reference$marker, 3)
  expect_true(all(result$alpha != f$current$alpha))
})

test_that("a rejected sweep leaves every coefficient and reference unchanged", {
  f <- general_block_fixture()
  cfg <- gls_control(static_target_block_size = 2L)
  local_mocked_bindings(accept_reference = function(Y, H, offset, beta, discount, ref, cfg)
    list(accepted = FALSE, ref = ref), .package = "GLS")
  result <- GLS:::update_general_targets_with_reference(f$ystar, f$X, f$Q, f$Y,
    f$beta, f$omega, f$pairs, f$prior_var, f$means, f$current, .95,
    list(marker = 4), .8, cfg)
  for (name in names(f$current)) expect_identical(result[[name]], f$current[[name]])
  expect_equal(result$accepted, 0L)
  expect_equal(result$proposed, 3L)
  expect_identical(result$reference, list(marker = 4))
})

test_that("general fits retain zero series and freeze pCN adaptation after burn-in", {
  set.seed(140)
  graph <- gls_grid(2); k <- nrow(graph$edges)
  Y <- matrix(rpois(24*k, 3), 24, k)
  Y[, k] <- 0; Y[, 2L] <- Y[, 1L]
  Q <- cbind(sin((1:24)/3), cos((1:24)/3))
  cfg <- gls_control("smoke", n_iter = 8L, burn_mcmc = 4L, n_chains = 2L,
    parallel_chains = FALSE, state_block_substeps = 1L, shrinkage_substeps = 1L,
    scale_move_steps = 1L, static_target_block_size = 3L, progress_every = 100L,
    dss_n_lambda = 3L, dss_max_draws = 2L, dss_validation_draws = 2L, predictive_draws = 2L)
  fit <- gls_fit_general(Y, graph, Q, train_rows = 1:16, control = cfg)
  expect_equal(fit$means[k], .5/16)
  expect_equal(fit$means[1], mean(Y[1:16, 1]))
  expect_true(all(is.finite(fit$alpha)))
  expect_equal(nrow(fit$coefficient_trace), 16L)
  expect_equal(fit$covariance_proposed, rep(8L * ceiling(k/3), 2L))
  for (trace in split(fit$coefficient_trace, fit$coefficient_trace$chain)) {
    expect_equal(trace$proposed, rep(ceiling(k/3), 8L))
    expect_true(any(diff(trace$pcn_step[1:5]) != 0))
    expect_equal(trace$pcn_step[5:8], rep(trace$next_pcn_step[4], 4L))
    expect_equal(trace$next_pcn_step[5:8], trace$pcn_step[5:8])
  }
  dss <- gls_dss(fit, validation_rows = 17:20, control = cfg)
  p <- predict(fit, rows = 21:24, conditioning_rows = 17:20, d = dss$d, control = cfg)
  expect_true(all(is.finite(p$lambda)))
  expect_equal(dim(p$lambda)[1:2], c(4L, k))
  expect_true(all(c("SplitRhat", "ESS") %in% names(gls_diagnostics(fit))))
  theta <- fit$B[1L, ] + fit$L[1L, , 1L] %o% fit$beta_path[1L, , 1L]
  expect_equal(fit$amplitude[1L, ], sqrt(rowMeans(theta^2)))
})

test_that("unit evolution supports the loading-only block kernel and empty Q", {
  graph <- gls_grid(2); k <- nrow(graph$edges)
  set.seed(502); Y <- matrix(rpois(16*k, 2), 16, k)
  cfg <- gls_control("smoke", n_iter = 4L, burn_mcmc = 0L, n_chains = 1L,
    factor_evolution = "unit", static_network = FALSE, state_block_substeps = 1L,
    initial_tau = .5,
    shrinkage_substeps = 1L, scale_move_steps = 1L, progress_every = 100L)
  fit <- gls_fit_general(Y, graph, matrix(numeric(), 16, 0), train_rows = 1:12, control = cfg)
  expect_equal(fit$B, matrix(0, 4, k^2))
  expect_equal(fit$covariance_acceptance, 1)
  expect_true(all(is.finite(fit$L)))
  p <- k^2; active <- min(cfg$expected_active_pairs, p-1L)
  old_initial <- max((active/(p-active))/(sqrt(12)*sqrt((12+3)/2)), cfg$tau_initial_floor)
  expect_equal(fit$chains[[1]]$tau_prior_scale,
    max(cfg$tau_prior_min_scale, cfg$tau_prior_multiplier*old_initial))
  expect_equal(fit$chains[[1]]$tau_initial, max(old_initial, cfg$initial_tau))
})

test_that("a frozen coefficient startup is stopped before it is reported as a fit", {
  graph <- gls_grid(2); k <- nrow(graph$edges)
  set.seed(111); Y <- matrix(rpois(12*k, 3), 12, k)
  cfg <- gls_control("smoke", n_iter = 4L, burn_mcmc = 0L, n_chains = 1L,
    state_block_substeps = 1L, startup_check_iteration = 1L)
  local_mocked_bindings(accept_reference = function(Y, H, offset, beta, discount, ref, cfg)
    list(accepted = FALSE, ref = ref), .package = "GLS")
  expect_error(gls_fit_general(Y, graph, matrix(0, 12, 0), control = cfg),
    "Startup protection: coefficient blocks did not move")
})

test_that("matrix products preserve repeated ordered-pair contributions", {
  pairs <- data.frame(source = c(1L, 1L, 2L), target = c(2L, 2L, 1L))
  X <- matrix(c(2, 3, 5, 7), 2L)
  B <- c(.1, .2, .4); L <- matrix(B, ncol = 1L)
  expected <- cbind(X[, 2] * .4, X[, 1] * .3)
  expect_equal(GLS:::static_network(X, pairs, B), expected)
  expect_equal(GLS:::build_network_array(X, pairs, L, 1L, 2L)[, , 1], expected)
  expect_equal(as.numeric(GLS:::factor_design_one_time(X[1, ], pairs, L, 2L)), expected[1, ])
})
