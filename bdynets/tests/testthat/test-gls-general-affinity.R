test_that("general GLS learns the Graph-Laplace affinity on the candidate subset", {
  set.seed(418)
  graph <- gls_grid(2L)
  edges <- graph$edges
  k <- nrow(edges)
  Y <- matrix(1L + rpois(18L * k, 3), nrow = 18L)
  Q <- cbind(sin(seq_len(nrow(Y)) / 3), cos(seq_len(nrow(Y)) / 3))
  all_pairs <- gls_all_pairs(graph)
  rows <- rev(which(is.finite(all_pairs$hop) & all_pairs$hop <= 1))
  pairs <- all_pairs[rows, , drop = FALSE]
  expect_lt(nrow(pairs), nrow(all_pairs))

  rho <- 0.8
  cfg <- gls_control("smoke", n_iter = 12L, burn_mcmc = 2L, thin = 2L,
    n_chains = 2L, parallel_chains = FALSE, state_block_substeps = 1L,
    shrinkage_substeps = 1L, scale_move_steps = 1L, fixed_tau = 0.2,
    startup_min_static_acceptance = 0, startup_min_static_accepts = 0L,
    predictive_draws = 2L, graph_laplace_rho0 = rho)
  fit <- function(control, ...) gls_fit_general(Y, graph, Q, train_rows = 1:12,
    pairs = pairs, control = control, ...)

  default <- fit(cfg)
  fixed <- cfg
  fixed$affinity_model <- "fixed"
  explicit <- fit(fixed)
  expect_null(default$chi)
  expect_null(default$affinity_features)
  expect_identical(dim(default$B), c(10L, nrow(pairs)))
  expect_identical(dim(default$L), c(10L, nrow(pairs), 1L))
  for (field in c("B", "L", "beta_path", "tau", "network_rms"))
    expect_identical(default[[field]], explicit[[field]], info = field)

  learned <- cfg
  learned$affinity_model <- "graph_laplace"
  memory <- fit(learned)
  expect_identical(memory$pairs, pairs)
  expect_identical(dim(memory$affinity_features), c(nrow(pairs), 1L))
  expect_identical(colnames(memory$affinity_features), "graph_laplace")

  adjacency <- outer(edges$from, edges$from, "==") |
    outer(edges$from, edges$to, "==") | outer(edges$to, edges$from, "==") |
    outer(edges$to, edges$to, "==")
  diag(adjacency) <- FALSE
  correlation <- stats::cov2cor(solve(diag(rowSums(adjacency) + 1) - rho * adjacency))
  score <- correlation[cbind(pairs$target, pairs$source)]
  centered <- score - mean(score)
  feature <- centered / sqrt(mean(centered^2))
  expect_equal(as.vector(memory$affinity_features), feature, tolerance = 1e-12)
  expect_equal(attr(memory$affinity_features, "raw_score"), score, tolerance = 1e-12)

  expect_identical(memory$tau, rep(0.2, 10L))
  expect_identical(memory$chain_id, rep(1:2, each = 5L))
  expect_type(memory$chi, "double")
  expect_length(memory$chi, 10L)
  expect_true(all(is.finite(memory$chi) & memory$chi > 0))
  expect_gt(length(unique(memory$chi)), 1L)
  expect_identical(memory$chi, unlist(lapply(memory$chains, `[[`, "chi")))
  for (chain in memory$chains) {
    expect_true(is.finite(chain$chi_acceptance) &&
      chain$chi_acceptance >= 0 && chain$chi_acceptance <= 1)
    expect_true(is.finite(chain$chi_final_step) && chain$chi_final_step > 0)
  }
  diagnostics <- gls_diagnostics(memory)
  expect_identical(diagnostics$Parameter[diagnostics$ParameterType == "chi"], "chi")

  directory <- tempfile("gls-affinity-test-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  disk <- fit(learned, draw_dir = directory, chunk_size = 2L)
  expect_null(disk$chi)
  expect_identical(disk$affinity_features, memory$affinity_features)
  expect_equal(disk$chains[[1L]]$draw_chunks$sizes, c(2L, 2L, 1L))
  loaded <- gls_load_draws(disk)
  expect_identical(loaded$chi, memory$chi)
  expect_identical(lapply(loaded$chains, `[[`, "chi"), lapply(memory$chains, `[[`, "chi"))
  expect_identical(lapply(loaded$chains, `[[`, "chi_acceptance"),
    lapply(memory$chains, `[[`, "chi_acceptance"))
  expect_identical(lapply(loaded$chains, `[[`, "chi_final_step"),
    lapply(memory$chains, `[[`, "chi_final_step"))
  expect_identical(loaded$tau, memory$tau)
  expect_identical(gls_diagnostics(disk), diagnostics)
  expect_identical(predict(disk, rows = 13:14), predict(memory, rows = 13:14))

  loading_only <- learned
  loading_only$fixed_tau <- NULL
  loading_only$static_network <- FALSE
  fit_loading_only <- fit(loading_only)
  expect_true(all(fit_loading_only$B == 0))
  expect_true(all(is.finite(fit_loading_only$chi) & fit_loading_only$chi > 0))
  expect_true(all(is.finite(fit_loading_only$tau) & fit_loading_only$tau > 0))
})

test_that("general affinity controls reject unsupported model settings", {
  expect_error(gls_control(affinity_model = "unknown"), "affinity_model")
  expect_error(gls_control(graph_laplace_rho0 = -0.1), "graph_laplace_rho0")
  expect_error(gls_control(graph_laplace_rho0 = 1.1), "graph_laplace_rho0")
  expect_error(gls_control(chi_shape = 0), "chi_shape")
  expect_error(gls_control(chi_rate = 0), "chi_rate")
  expect_error(gls_control(chi_proposal_sd = 0), "chi_proposal_sd")

  graph <- gls_grid(2L)
  Y <- matrix(1, nrow = 12L, ncol = nrow(graph$edges))
  learned <- gls_control("smoke", affinity_model = "graph_laplace")
  expect_error(gls_fit(Y, graph, control = learned), "gls_fit_general")
  pairs <- gls_all_pairs(graph)
  invalid <- pairs
  invalid$source[1L] <- ncol(Y) + 1L
  expect_error(gls_fit_general(Y, graph, matrix(0, nrow(Y), 0L), pairs = invalid,
    control = learned), "target and source indices")
  self <- pairs[pairs$target == pairs$source, , drop = FALSE]
  expect_error(gls_fit_general(Y, graph, matrix(0, nrow(Y), 0L), pairs = self,
    control = learned), "[Cc]onstant")
})
