test_that("general GLS continues exactly after warmup and sampling interruptions", {
  root <- tempfile("gls-restart-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  graph <- gls_grid(2L)
  set.seed(1250)
  y <- matrix(rpois(18L * nrow(graph$edges), 4), 18L)
  q <- matrix(cos(seq_len(18)), ncol = 1L)
  cfg <- gls_control("smoke", n_iter = 14L, burn_mcmc = 5L, thin = 2L,
    n_chains = 1L, parallel_chains = FALSE, affinity_model = "graph_laplace",
    progress_every = 99999L, startup_min_static_accepts = 0L,
    startup_min_static_acceptance = 0)
  run <- function(dir = NULL, resume = FALSE, observations = y) {
    gls_fit_general(observations, graph, q, control = cfg, chunk_size = 2L,
      checkpoint_dir = dir, checkpoint_every = 4L, resume = resume)
  }
  expected <- run()
  commit <- .gls_commit_chain
  # Iteration four is in warmup; iteration eight commits a partial draw chunk.
  for (stop_at in c(4L, 8L)) {
    dir <- file.path(root, as.character(stop_at))
    expect_error(with_mocked_bindings(run(dir), .gls_commit_chain = function(record, dir) {
      commit(record, dir)
      if (record$iteration == stop_at) stop("simulated interruption")
    }), "simulated interruption")
    chain_dir <- file.path(dir, "chain-0001")
    expect_length(.gls_chain_snapshots(chain_dir), 1L)
    expect_identical(readRDS(.gls_chain_snapshots(chain_dir))$iteration, stop_at)
    expect_error(run(dir), "already exists")
    expect_error(run(dir, TRUE, y + 1), "differ from the checkpoint")
    actual <- gls_load_draws(run(dir, TRUE))
    for (field in setdiff(names(expected$chains[[1L]]), c("elapsed", "worker_pid")))
      expect_identical(actual$chains[[1L]][[field]], expected$chains[[1L]][[field]], info = field)
    completed <- run(dir, TRUE)
    expect_identical(gls_load_draws(completed)$B, expected$B)
    expect_length(.gls_chain_snapshots(chain_dir), 1L)
    damaged <- file.path(completed$draw_store$path, completed$chains[[1]]$draw_chunks$files[1L])
    cat("bad", file = damaged, append = TRUE)
    expect_error(run(dir, TRUE), "Missing or corrupt")
  }
})

test_that("general GLS rolls back uncommitted chunks and preserves completed chains", {
  root <- tempfile("gls-restart-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  graph <- gls_grid(2L)
  set.seed(223)
  y <- matrix(rpois(14L * nrow(graph$edges), 3), 14L)
  cfg <- gls_control("smoke", n_iter = 10L, burn_mcmc = 2L, thin = 1L,
    n_chains = 2L, parallel_chains = FALSE, fixed_tau = 0.02,
    startup_min_static_accepts = 0L, startup_min_static_acceptance = 0)
  run <- function(dir = NULL, resume = FALSE) gls_fit_general(y, graph, matrix(1, 14, 1),
    control = cfg, chunk_size = 1L, checkpoint_dir = dir, checkpoint_every = 4L, resume = resume)
  expected <- run()
  write_chunk <- .gls_write_draw_chunk
  expect_error(with_mocked_bindings(run(root), .gls_write_draw_chunk = function(buffer, draws, n, iteration) {
    buffer <- write_chunk(buffer, draws, n, iteration)
    if (buffer$chain_id == 2L && iteration == 6L) stop("after chunk publication")
    buffer
  }), "after chunk publication")
  complete <- .gls_chain_snapshots(file.path(root, "chain-0001"))
  original <- file.info(complete)$mtime
  actual <- gls_load_draws(run(root, TRUE))
  expect_identical(file.info(complete)$mtime, original)
  expect_identical(actual$B, expected$B)
  expect_identical(actual$beta_path, expected$beta_path)
  expect_identical(actual$coefficient_trace, expected$coefficient_trace)
})

test_that("a concurrent run is refused and OS locks release on process death", {
  skip_on_os("windows")  # mcparallel is unavailable on Windows.
  root <- tempfile("gls-hard-kill-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  graph <- gls_grid(2L)
  set.seed(700)
  y <- matrix(rpois(12L * nrow(graph$edges), 3), 12L)
  cfg <- gls_control("smoke", n_iter = 8L, burn_mcmc = 4L, thin = 1L,
    n_chains = 1L, parallel_chains = FALSE, startup_min_static_accepts = 0L,
    startup_min_static_acceptance = 0)
  run <- function(dir = NULL, resume = FALSE) gls_fit_general(y, graph, matrix(1, 12, 1),
    control = cfg, chunk_size = 2L, checkpoint_dir = dir, checkpoint_every = 2L, resume = resume)
  expected <- run()
  marker <- file.path(root, "waiting")
  commit <- .gls_commit_chain
  job <- parallel::mcparallel(with_mocked_bindings(run(root), .gls_commit_chain = function(record, dir) {
    commit(record, dir)
    if (record$iteration == 2L) {
      file.create(marker)
      Sys.sleep(60)
    }
  }), silent = TRUE)
  on.exit(if (!is.null(job)) tools::pskill(job$pid, tools::SIGKILL), add = TRUE)
  deadline <- Sys.time() + 30
  while (!file.exists(marker) && Sys.time() < deadline) Sys.sleep(0.02)
  expect_true(file.exists(marker))
  expect_error(run(root, TRUE), "Another process owns checkpoint lock")
  tools::pskill(job$pid, tools::SIGKILL)
  suppressWarnings(parallel::mccollect(job))
  job <- NULL
  actual <- gls_load_draws(run(root, TRUE))
  expect_identical(actual$B, expected$B)
  expect_identical(actual$beta_path, expected$beta_path)
  expect_identical(actual$coefficient_trace, expected$coefficient_trace)
})
