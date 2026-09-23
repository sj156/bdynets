test_that("control profiles are validated", {
  smoke <- bdcn_control("smoke")
  expect_s3_class(smoke, "bdcn_control")
  expect_equal(smoke$n_iter, 80L)
  expect_error(bdcn_control("smoke", not_a_setting = 1), "Unknown")
  expect_error(bdcn_control("smoke", 1), "must be named")
  expect_error(bdcn_control("smoke", factor_dim = 2), "factor_dim = 1")
  expect_error(bdcn_control("smoke", burn_mcmc = 80), "Require")
})

test_that("grid and nonlocal pair dictionary are coherent", {
  graph <- bdcn_grid(3)
  pairs <- bdcn_pairs(graph, bdcn_control("smoke"))
  expect_s3_class(graph, "bdcn_graph")
  expect_equal(dim(graph$A), c(nrow(graph$edges), nrow(graph$edges)))
  expect_true(all(abs(rowSums(graph$W)[rowSums(graph$W) > 0] - 1) < 1e-12))
  expect_true(all(!is.finite(pairs$hop) | pairs$hop >= 2))
  expect_true(all(pairs$affinity > 0))
  expect_equal(sort(unique(pairs$target)), seq_len(nrow(graph$edges)))
})

test_that("multi-hop simulation is reproducible", {
  control <- bdcn_control(
    "smoke", n_total = 30L, simulation_burn = 20L,
    calibration_length = 30L, calibration_iterations = 1L,
    n_long_pairs = 2L
  )
  first <- bdcn_simulate_multihop(0.05, 3L, control)
  second <- bdcn_simulate_multihop(0.05, 3L, control)
  expect_identical(first$simulation$Y, second$simulation$Y)
  expect_equal(nrow(first$simulation$Y), 30L)
  expect_equal(length(first$split$test), 5L)
})

test_that("tiny posterior fit supports diagnostics, DSS, and prediction", {
  skip_on_cran()
  set.seed(42)
  graph <- bdcn_grid(2)
  Y <- matrix(rpois(12L * nrow(graph$edges), 5), 12L, nrow(graph$edges))
  control <- bdcn_control(
    "smoke", n_total = 12L, n_iter = 6L, burn_mcmc = 2L,
    n_chains = 1L, thin = 1L, progress_every = 100L,
    shrinkage_substeps = 1L, state_block_substeps = 1L,
    scale_move_steps = 1L, predictive_draws = 2L,
    dss_validation_draws = 2L, dss_max_draws = 2L,
    dss_n_lambda = 3L
  )
  fit <- bdcn_fit(Y, graph, train_rows = 1:8, control = control)
  expect_s3_class(fit, "bdcn_fit")
  expect_equal(nrow(fit$alpha), 4L)
  diagnostics <- bdcn_diagnostics(fit)
  expect_true(all(c("SplitRhat", "ESS", "Converged") %in% names(diagnostics)))
  dss <- bdcn_dss(fit, 9:10)
  expect_s3_class(dss, "bdcn_dss")
  pred <- predict(fit, rows = 11:12, conditioning_rows = 9:10,
                  d = dss$d, control = control)
  expect_s3_class(pred, "bdcn_prediction")
  expect_equal(dim(pred$lambda)[1:2], c(2L, ncol(Y)))
})
