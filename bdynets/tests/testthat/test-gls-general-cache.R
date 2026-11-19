general_cache_fixture <- function() {
  set.seed(904)
  n <- 14L
  k <- 5L
  pairs <- expand.grid(target = seq_len(k), source = seq_len(k))
  beta <- matrix(rnorm(n), n, 1L)
  attr(beta, "initial") <- 0.2
  list(X = matrix(rnorm(n * k), n, k), Q = matrix(rnorm(n * 2L), n, 2L),
    Y = matrix(rpois(n * k, 2), n, k), ystar = matrix(rnorm(n * k), n, k),
    omega = matrix(runif(n * k, .3, 1), n, k), beta = beta,
    pairs = pairs, prior_var = rep(.04, nrow(pairs)), means = rep(2, k),
    current = list(alpha = rnorm(k), gamma = matrix(rnorm(k * 2L), k, 2L),
      B = rnorm(nrow(pairs), sd = .1), L = matrix(rnorm(nrow(pairs), sd = .02), nrow(pairs), 1L)))
}

test_that("general sweeps preserve accepted caches and discard rejected caches", {
  f <- general_cache_fixture()
  cfg <- gls_control(static_target_block_size = 2L)
  network <- bdynets:::build_network_array
  baseline <- bdynets:::baseline_general
  static <- bdynets:::static_network
  H <- network(f$X, f$pairs, f$current$L, 1L, ncol(f$X))
  offset <- baseline(f$current$alpha, f$current$gamma, f$Q) +
    static(f$X, f$pairs, f$current$B)
  for (decisions in list(rep(FALSE, 3L), rep(TRUE, 3L), c(TRUE, FALSE, TRUE))) {
    run <- function(cached) {
      count <- 0L
      constructions <- 0L
      result <- with_mocked_bindings({
        set.seed(903)
        ans <- bdynets:::update_general_targets_with_reference(f$ystar, f$X, f$Q, f$Y,
          f$beta, f$omega, f$pairs, f$prior_var, f$means, f$current, .95,
          list(marker = 0L), .8, cfg, H = if (cached) H else NULL,
          offset = if (cached) offset else NULL)
        list(fit = ans, seed = .Random.seed)
      }, .gls_study_make_target_blocks = function(k, block_size) list(1:2, 3:4, 5L),
        accept_reference = function(Y, H, offset, beta, discount, ref, cfg) {
          count <<- count + 1L
          if (decisions[count]) ref$marker <- ref$marker + 1L
          list(accepted = decisions[count], ref = ref)
        },
        build_network_array = function(...) {
          constructions <<- constructions + 1L
          network(...)
        },
        baseline_general = function(...) {
          constructions <<- constructions + 1L
          baseline(...)
        },
        static_network = function(...) {
          constructions <<- constructions + 1L
          static(...)
        }, .package = "bdynets")
      c(result, list(constructions = constructions))
    }
    no_cache <- run(FALSE)
    cached <- run(TRUE)
    expect_identical(cached$fit, no_cache$fit)
    expect_identical(cached$seed, no_cache$seed)
    expect_identical(cached$constructions, 0L)
    expect_identical(no_cache$constructions, 3L)
    expect_equal(cached$fit$H, network(f$X, f$pairs, cached$fit$L, 1L, ncol(f$X)),
      tolerance = 1e-12)
    expect_equal(cached$fit$offset, baseline(cached$fit$alpha, cached$fit$gamma, f$Q) +
      static(f$X, f$pairs, cached$fit$B), tolerance = 1e-12)
    expect_identical(cached$fit$accepted, sum(decisions))
    expect_identical(cached$fit$reference$marker, sum(decisions))
  }
})

test_that("discount updates reuse the matching covariance path with current factor weights", {
  f <- general_cache_fixture()
  cfg <- gls_control(reference_backend = "R")
  H <- bdynets:::build_network_array(f$X, f$pairs, f$current$L, 1L, ncol(f$X))
  offset <- bdynets:::baseline_general(f$current$alpha, f$current$gamma, f$Q) +
    bdynets:::static_network(f$X, f$pairs, f$current$B)
  path <- bdynets:::reference_path
  current_ref <- path(f$Y, H, offset, .95, cfg)
  run <- function(beta, cached, current_discount = .95) {
    visited <- numeric()
    ans <- with_mocked_bindings({
      set.seed(810)
      out <- bdynets:::update_discount(f$Y, H, offset, beta, cfg,
        current_discount = current_discount, current_ref = if (cached) current_ref else NULL)
      list(result = out, seed = .Random.seed)
    }, reference_path = function(Y, H, offset, discount, cfg, keep_mean = FALSE) {
      visited <<- c(visited, discount)
      path(Y, H, offset, discount, cfg, keep_mean)
    }, .package = "bdynets")
    c(ans, list(visited = visited))
  }
  for (beta in list(f$beta, f$beta * 2)) {
    attr(beta, "initial") <- 0.2
    no_cache <- run(beta, FALSE)
    cached <- run(beta, TRUE)
    expect_identical(cached$result, no_cache$result)
    expect_identical(cached$seed, no_cache$seed)
    expect_identical(cached$visited, cfg$discount_grid[cfg$discount_grid != .95])
    expect_identical(no_cache$visited, cfg$discount_grid)
  }
  unmatched <- run(f$beta, TRUE, current_discount = .97)
  expect_identical(unmatched$visited, cfg$discount_grid)

  cfg$factor_evolution <- "unit"
  cached <- path(f$Y, H, offset, NA_real_, cfg)
  visited <- 0L
  with_mocked_bindings({
    expect_identical(bdynets:::update_discount(f$Y, H, offset, f$beta, cfg),
      list(discount = NA_real_, ref = cached))
    expect_identical(visited, 1L)
    expect_identical(bdynets:::update_discount(f$Y, H, offset, f$beta, cfg,
      current_ref = cached), list(discount = NA_real_, ref = cached))
    expect_identical(visited, 1L)
  }, reference_path = function(...) {
    visited <<- visited + 1L
    path(...)
  }, .package = "bdynets")
})

test_that("general fit cache remains valid through substeps and loading rescaling", {
  graph <- gls_grid(2L)
  k <- nrow(graph$edges)
  set.seed(89)
  Y <- matrix(1L + rpois(20L * k, 3), nrow = 20L)
  sweep <- bdynets:::update_general_targets_with_reference
  update <- bdynets:::update_discount
  for (mode in c("discount", "unit")) {
    cfg <- gls_control("smoke", n_iter = 8L, burn_mcmc = 4L, n_chains = 1L,
      state_block_substeps = 2L, shrinkage_substeps = 1L, scale_move_steps = 1L,
      static_target_block_size = 3L, interweave_steps = 2L, reference_backend = "R",
      static_network = mode == "discount", factor_evolution = mode,
      startup_min_static_acceptance = 0, startup_min_static_accepts = 0L)
    Q <- if (mode == "discount") cbind(sin(seq_len(nrow(Y)) / 3)) else
      matrix(numeric(), nrow(Y), 0L)
    run <- function() gls_fit_general(Y, graph, Q, train_rows = 1:14, control = cfg)
    cached <- run()
    seed_cached <- .Random.seed
    uncached <- with_mocked_bindings(run(),
      update_general_targets_with_reference = function(ystar, X, Q, Y, beta, omega,
        pairs, prior_var, means, current, discount, ref, pcn_step, cfg, ids = NULL,
        H = NULL, offset = NULL) {
        sweep(ystar, X, Q, Y, beta, omega, pairs, prior_var, means, current,
          discount, ref, pcn_step, cfg, ids)
      },
      update_discount = function(Y, H, offset, beta, cfg, current_discount = NULL,
        current_ref = NULL) update(Y, H, offset, beta, cfg),
      .package = "bdynets")
    expect_identical(.Random.seed, seed_cached)
    for (field in c("alpha", "gamma", "B", "L", "beta_path", "discount", "tau",
        "network_rms", "coefficient_trace", "initial_beta", "reference_mean", "reference_variance"))
      expect_equal(cached[[field]], uncached[[field]], tolerance = 1e-10, info = field)
    for (i in seq_len(nrow(cached$alpha))) {
      L <- matrix(cached$L[i, , ], ncol = 1L)
      beta <- matrix(cached$beta_path[i, , ], ncol = 1L)
      H <- bdynets:::build_network_array(cached$X[1:14, , drop = FALSE], cached$pairs, L, 1L, k)
      static <- bdynets:::static_network(cached$X[1:14, , drop = FALSE], cached$pairs, cached$B[i, ])
      rms <- sqrt(mean((static + bdynets:::network_contribution(H, beta))^2))
      expect_equal(cached$network_rms[i], rms, tolerance = 1e-12)
    }
  }
})
