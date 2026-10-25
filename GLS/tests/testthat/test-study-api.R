test_that("directed-grid study design and DGP are reproducible", {
  control <- gls_study_control("smoke")
  design <- gls_study_design(control)
  first <- gls_simulate_grid(0.1, design)
  second <- gls_simulate_grid(0.1, design)

  expect_s3_class(control, "gls_study_control")
  expect_s3_class(design, "gls_study_design")
  expect_s3_class(first, "gls_study_simulation")
  expect_equal(nrow(design$pairs), nrow(design$graph$edges)^2)
  expect_identical(first$Y, second$Y)
  expect_identical(first$lambda, second$lambda)
})

test_that("Graph-Laplace tau-one option follows Appendix A.2", {
  control <- gls_study_control(
    "smoke", shrinkage_spec = "graph_laplace_tau1",
    graph_laplace_rho0 = 0.8
  )
  design <- gls_study_design(control)
  feature <- design$affinity_features[, 1L]

  expect_identical(colnames(design$affinity_features), "graph_laplace")
  expect_equal(mean(feature), 0, tolerance = 1e-12)
  expect_equal(mean(feature^2), 1, tolerance = 1e-12)
  expect_equal(attr(design$affinity_features, "rho0"), 0.8)
  expect_true(all(attr(design$affinity_features, "raw_score") >= -1e-12))
})

test_that("Graph-Laplace tau-one sampler keeps tau fixed", {
  skip_if_not_installed("BayesLogit")
  control <- gls_study_control(
    "smoke", signal_grid = 0.1,
    shrinkage_spec = "graph_laplace_tau1",
    startup_min_static_acceptance = 0,
    startup_min_static_accepts = 0L
  )
  design <- gls_study_design(control)
  simulation <- gls_simulate_grid(0.1, design)
  fit <- gls_fit_study(simulation, design, control)
  diagnostic <- gls_diagnostics(fit)

  expect_identical(unique(fit$tau), 1)
  expect_true(all(is.na(fit$acceptance$joint_scale)))
  expect_identical(unique(fit$acceptance$joint_scale_method), "fixed_tau_1")
  expect_identical(diagnostic$ParameterType[1L], "tau_fixed")
  expect_true(diagnostic$Converged[1L])
  expect_true(any(diagnostic$ParameterType == "theta_total"))
  expect_false(any(diagnostic$ParameterType %in%
    c("theta_bar", "loading", "beta_terminal")))
})

test_that("Graph-Laplace sampler supports a configured fixed tau", {
  skip_if_not_installed("BayesLogit")
  control <- gls_study_control(
    "smoke", signal_grid = 0.1,
    shrinkage_spec = "graph_laplace_fixed_tau", fixed_tau = 0.025,
    startup_min_static_acceptance = 0,
    startup_min_static_accepts = 0L
  )
  design <- gls_study_design(control)
  simulation <- gls_simulate_grid(0.1, design)
  fit <- gls_fit_study(simulation, design, control)
  diagnostic <- gls_diagnostics(fit)

  expect_equal(unique(fit$tau), 0.025)
  expect_identical(unique(fit$acceptance$joint_scale_method),
                   "fixed_tau_0.025")
  expect_identical(diagnostic$Parameter[1L], "tau=0.025 (fixed)")
})

test_that("tau 0.001 sensitivity preset preserves the full-run schedule", {
  control <- gls_study_control("full", sensitivity = "graph_laplace_tau0001")

  expect_identical(control$shrinkage_spec, "graph_laplace_fixed_tau")
  expect_equal(control$fixed_tau, 0.001)
  expect_identical(control$n_iter, 4000L)
  expect_identical(control$burn_mcmc, 2500L)
  expect_identical(control$n_chains, 3L)
  expect_equal(control$dss_predictive_tolerance, 0.01)
  expect_identical(control$dss_path_eval_stride, 1L)
})

test_that("smoke study runs GLS, DSS, prediction, and PNAR", {
  skip_if_not_installed("BayesLogit")
  control <- gls_study_control(
    "smoke", signal_grid = 0.1,
    startup_min_static_acceptance = 0,
    startup_min_static_accepts = 0L
  )
  design <- gls_study_design(control)
  scenario <- gls_run_grid_scenario(0.1, design, control)

  expect_s3_class(scenario, "gls_study_scenario")
  expect_s3_class(scenario$fit, "gls_study_fit")
  expect_s3_class(scenario$dss, "gls_dss")
  expect_setequal(
    scenario$performance$Model,
    c("GLS", "GLSS",
      "PNAR(1), physical W", "PNAR(2), physical W")
  )
  expect_equal(length(scenario$dss$d), nrow(design$pairs))
})
