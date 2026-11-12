with_gls_chunk_test_directory <- function(code) {
  parent <- tempfile("gls-draw-test-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  code(parent)
}

gls_chunk_test_data <- function() {
  set.seed(520)
  graph <- gls_grid(2L)
  Y <- matrix(1 + rpois(20L * nrow(graph$edges), 4), 20L)
  list(Y = Y, graph = graph, train_rows = 1:14)
}

gls_chunk_test_control <- function(n_chains = 1L) {
  gls_control("smoke", n_iter = 13L, burn_mcmc = 2L, thin = 2L,
    n_chains = n_chains, parallel_chains = FALSE, state_block_substeps = 1L,
    shrinkage_substeps = 1L, scale_move_steps = 1L, progress_every = 99999L,
    startup_min_static_acceptance = 0, startup_min_static_accepts = 0L,
    dss_n_lambda = 3L, dss_validation_draws = 2L, dss_max_draws = 2L,
    predictive_draws = 2L)
}

expect_gls_chunk_roundtrip <- function(memory, disk, parent) {
  n_chains <- length(memory$chains)
  expect_null(memory$draw_store)
  expect_identical(gls_load_draws(memory), memory)
  expect_identical(class(disk), class(memory))
  expect_true(dir.exists(disk$draw_store$path))
  expect_identical(dirname(normalizePath(disk$draw_store$path)), normalizePath(parent))
  expect_length(disk$chains, n_chains)
  payload <- c("alpha", "B", "theta_bar", "L", "beta_path", "amplitude", "gamma", "ref_C_T")
  for (field in payload) expect_null(disk[[field]], info = field)
  chunk_files <- list.files(disk$draw_store$path, pattern = "^chunk-.*\\.rds$",
                            recursive = TRUE)
  expect_length(chunk_files, 3L * n_chains)
  for (chain_id in seq_len(n_chains)) {
    chain <- disk$chains[[chain_id]]
    for (field in payload) expect_null(chain[[field]], info = field)
    manifest <- chain$draw_chunks
    expect_length(manifest$files, 3L)
    expect_true(all(manifest$files %in% chunk_files))
    expect_equal(manifest$sizes, c(2L, 2L, 1L))
    expect_length(manifest$hashes, 3L)
    expect_equal(manifest$n_saved, 5L)
    expect_equal(manifest$last_iteration, 12L)
    chunks <- lapply(file.path(disk$draw_store$path, manifest$files), readRDS)
    expect_equal(vapply(chunks, function(x) as.integer(x$chain_id), integer(1L)),
                 rep(chain_id, 3L))
    expect_equal(vapply(chunks, function(x) as.integer(x$first_draw), integer(1L)),
                 c(1L, 3L, 5L))
    expect_equal(vapply(chunks, function(x) as.integer(x$last_draw), integer(1L)),
                 c(2L, 4L, 5L))
    expect_equal(vapply(chunks, function(x) as.integer(x$iteration), integer(1L)),
                 c(6L, 10L, 12L))
    for (i in seq_along(chunks)) {
      draws <- chunks[[i]]$draws
      expect_type(draws, "list")
      expect_true(all(c("alpha", "L", "beta_path", "discount") %in% names(draws)))
      sizes <- vapply(draws, function(x) {
        if (is.null(dim(x))) length(x) else dim(x)[1L]
      }, integer(1L))
      expect_true(all(sizes == c(2L, 2L, 1L)[i]))
    }
  }

  seed_before <- .Random.seed
  loaded <- gls_load_draws(disk)
  expect_identical(.Random.seed, seed_before)
  expect_identical(class(loaded), class(memory))
  expect_identical(loaded$chain_id, rep(seq_len(n_chains), each = 5L))
  for (field in setdiff(names(memory), c("elapsed", "chains", "draw_store"))) {
    expect_identical(loaded[[field]], memory[[field]], info = field)
  }
  for (i in seq_len(n_chains)) {
    for (field in setdiff(names(memory$chains[[i]]), c("elapsed", "draw_chunks"))) {
      expect_identical(loaded$chains[[i]][[field]], memory$chains[[i]][[field]],
                       info = paste("chain", i, field))
    }
  }
}

test_that("nonlocal GLS draw chunks preserve chain order and posterior workflows", {
  with_gls_chunk_test_directory(function(parent) {
    input <- gls_chunk_test_data()
    cfg <- gls_chunk_test_control(n_chains = 2L)
    cfg$progress_every <- 6L
    run <- function(...) gls_fit(input$Y, input$graph, train_rows = input$train_rows,
                                 control = cfg, ...)
    memory <- run()
    seed_memory <- .Random.seed
    chunks_at_six <- integer()
    disk <- with_mocked_bindings(run(draw_dir = parent, chunk_size = 2L),
      progress_with_eta = function(label, current, total, start_time) {
        if (current == 6L) {
          files <- list.files(parent, pattern = "^chunk-.*\\.rds$", recursive = TRUE)
          chunks_at_six <<- c(chunks_at_six, length(files))
        }
      }, .package = "bdynets")
    # Each chain has flushed its first two draws by iteration 6 of 13.
    expect_identical(chunks_at_six, c(1L, 4L))
    expect_identical(.Random.seed, seed_memory)
    expect_gls_chunk_roundtrip(memory, disk, parent)

    expect_identical(gls_diagnostics(disk), gls_diagnostics(memory))
    memory_summary <- summary(memory)
    disk_summary <- summary(disk)
    memory_summary$elapsed <- disk_summary$elapsed <- NULL
    expect_identical(disk_summary, memory_summary)
    memory_dss <- gls_dss(memory, validation_rows = 15:17)
    disk_dss <- gls_dss(disk, validation_rows = 15:17)
    expect_identical(disk_dss, memory_dss)
    expect_identical(predict(disk, rows = 18:20, conditioning_rows = 15:17, d = disk_dss$d),
                     predict(memory, rows = 18:20, conditioning_rows = 15:17, d = memory_dss$d))

    missing <- file.path(disk$draw_store$path, disk$chains[[1L]]$draw_chunks$files[1L])
    unlink(missing)
    expect_error(gls_load_draws(disk), "[Mm]issing|[Cc]hunk")
  })
})

test_that("general GLS chunks retain the empty covariate array and acceptance trace", {
  with_gls_chunk_test_directory(function(parent) {
    input <- gls_chunk_test_data()
    cfg <- gls_chunk_test_control()
    covariates <- matrix(numeric(), nrow(input$Y), 0L)
    run <- function(...) gls_fit_general(input$Y, input$graph, covariates,
      train_rows = input$train_rows, control = cfg, ...)
    memory <- run()
    seed_memory <- .Random.seed
    disk <- run(draw_dir = parent, chunk_size = 2L)
    expect_identical(.Random.seed, seed_memory)
    expect_gls_chunk_roundtrip(memory, disk, parent)
    expect_identical(gls_diagnostics(disk), gls_diagnostics(memory))
  })
})

test_that("study GLS chunks retain factor paths and reference states", {
  with_gls_chunk_test_directory(function(parent) {
    cfg <- gls_study_control("smoke", signal_grid = .1,
      n_iter = 13L, burn_mcmc = 2L, thin = 2L,
      startup_min_static_acceptance = 0, startup_min_static_accepts = 0L)
    design <- gls_study_design(cfg)
    simulation <- gls_simulate_grid(.1, design)
    memory <- gls_fit_study(simulation, design)
    seed_memory <- .Random.seed
    disk <- gls_fit_study(simulation, design, draw_dir = parent, chunk_size = 2L)
    expect_identical(.Random.seed, seed_memory)
    expect_gls_chunk_roundtrip(memory, disk, parent)
    expect_identical(gls_diagnostics(disk), gls_diagnostics(memory))
  })
})

test_that("study GLS retains its empty fit layout when thinning saves no draws", {
  with_gls_chunk_test_directory(function(parent) {
    cfg <- gls_study_control("smoke", signal_grid = .1,
      n_iter = 8L, burn_mcmc = 7L, thin = 2L,
      startup_min_static_acceptance = 0, startup_min_static_accepts = 0L)
    design <- gls_study_design(cfg)
    simulation <- gls_simulate_grid(.1, design)
    fit <- gls_fit_study(simulation, design, draw_dir = parent, chunk_size = 2L)
    expect_null(fit$draw_store)
    expect_identical(gls_load_draws(fit), fit)
    expect_equal(nrow(fit$alpha), 0L)
    expect_length(list.files(parent), 0L)
  })
})
