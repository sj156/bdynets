test_that("three general GLS chains respect worker caps and keep their seeded draws", {
  physical <- parallel::detectCores(logical = FALSE)
  skip_if(is.na(physical) || physical < 2L, "requires two detected physical cores")
  set.seed(807)
  graph <- gls_grid(2L)
  Y <- matrix(rpois(16L * nrow(graph$edges), 4), nrow = 16L)
  Q <- matrix(sin(seq_len(nrow(Y))), ncol = 1L)
  control <- gls_control("smoke", n_chains = 3L, n_iter = 6L, burn_mcmc = 2L,
    parallel_chains = TRUE, state_block_substeps = 1L, shrinkage_substeps = 1L,
    scale_move_steps = 1L, startup_min_static_acceptance = 0, startup_min_static_accepts = 0L)
  run <- function(cap) gls_fit_general(Y, graph, Q, control = control, max_workers = cap)
  workers <- function(fit) unique(vapply(fit$chains, "[[", integer(1), "worker_pid"))
  expect_error(run(0), "max_workers")
  parallel_fit <- run(2L)
  serial_fit <- run(1L)
  expect_length(parallel_fit$chains, 3L)
  expect_length(workers(parallel_fit), 2L)
  expect_identical(workers(serial_fit), Sys.getpid())
  for (field in c("B", "L", "beta_path", "tau", "coefficient_trace", "chain_id"))
    expect_identical(parallel_fit[[field]], serial_fit[[field]], info = field)
  control$parallel_chains <- FALSE
  expect_identical(workers(run(2L)), Sys.getpid())
})
