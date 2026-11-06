test_that("FFBS backend selection is separate from reference-filter selection", {
  expect_identical(gls_control("smoke")$backend, "cpp")
  expect_identical(gls_control("smoke")$reference_backend, "native")
  expect_identical(gls_control("smoke", backend = "R")$reference_backend, "native")
  expect_identical(gls_control("smoke", reference_backend = "R")$backend, "cpp")
  for (bad in list("C++", "r", "auto", c("R", "cpp"), NA_character_, 1, character())) {
    expect_error(gls_control("smoke", backend = bad), "backend")
  }
  old <- gls_control("smoke", backend = "R")
  old$backend <- NULL
  expect_identical(bdynets:::.validate_gls_control(old)$backend, "R")
})

test_that("rank-one native FFBS reproduces R draws, jitter and RNG order", {
  for (nt in c(1L, 17L)) for (k in c(1L, 4L)) {
    set.seed(701)
    response <- matrix(rnorm(nt * k), nt, k)
    omega <- matrix(.1 + rexp(nt * k), nt, k)
    for (loading_sd in c(0, 1e-10, .3)) {
      H <- array(rnorm(nt * k, sd = loading_sd), c(nt, k, 1L))
      for (W in list(NULL, rep(.03, nt), rep(1e-10, nt))) {
        set.seed(812)
        expected <- bdynets:::ffbs_factor_identified_r(response, H, omega, W)
        seed_r <- .Random.seed
        set.seed(812)
        actual <- bdynets:::.gls_ffbs_rank1_cpp(response, H, omega, W)
        expect_identical(.Random.seed, seed_r)
        expect_identical(dim(actual), c(nt, 1L))
        expect_length(attr(actual, "initial"), 1L)
        expect_lt(max(abs(actual - expected)), 1e-9)
        expect_lt(abs(attr(actual, "initial") - attr(expected, "initial")), 1e-9)
      }
    }
  }
})

test_that("rank-one adapters validate inputs and honor explicit dispatch", {
  y <- matrix(2, 3L, 2L)
  h <- array(.2, c(3L, 2L, 1L))
  omega <- matrix(1, 3L, 2L)
  native <- bdynets:::.gls_ffbs_rank1_cpp
  expect_error(native(y, as.vector(h), omega))
  expect_error(native(y, array(.2, c(3L, 2L, 2L)), omega))
  expect_error(native(y, h, omega[1:2, , drop = FALSE]))
  expect_error(native(y, h, omega, W = .1))
  expect_error(native(y, h, omega, W = rep(0, 3L)))
  bad <- omega; bad[1L] <- 0
  expect_error(native(y, h, bad))
  bad <- y; bad[1L] <- Inf
  expect_error(native(bad, h, omega))
  bad <- h; bad[1L] <- NA_real_
  expect_error(native(y, bad, omega))
  calls <- 0L
  with_mocked_bindings({
    bdynets:::ffbs_factor_identified(y, h, omega, backend = "cpp")
    expect_identical(calls, 1L)
    bdynets:::ffbs_factor_identified(y, h, omega, backend = "R")
    expect_identical(calls, 1L)
    expect_error(bdynets:::ffbs_factor_identified(y, h, omega, backend = "bad"), "backend")
  }, .gls_ffbs_rank1_cpp = function(response, H, omega, W = NULL) {
    calls <<- calls + 1L
    bdynets:::ffbs_factor_identified_r(response, H, omega, W)
  }, .package = "bdynets")
})

test_that("both GLS samplers reproduce paired R chains with native reference unchanged", {
  skip_if_not_installed("BayesLogit")
  set.seed(520)
  graph <- gls_grid(2L)
  Y <- matrix(1 + rpois(20L * nrow(graph$edges), 4), 20L)
  Q <- cbind(sin(seq_len(nrow(Y)) / 3), cos(seq_len(nrow(Y)) / 3))
  for (evolution in c("discount", "unit")) {
    cfg_r <- gls_control("smoke", backend = "R", n_iter = 8L,
      burn_mcmc = 4L, n_chains = 1L, parallel_chains = FALSE,
      state_block_substeps = 1L, shrinkage_substeps = 1L,
      scale_move_steps = 1L, progress_every = 99999L,
      startup_min_static_acceptance = 0, startup_min_static_accepts = 0L,
      factor_evolution = evolution, static_network = evolution == "discount")
    cfg_cpp <- cfg_r; cfg_cpp$backend <- "cpp"
    expect_identical(cfg_r$reference_backend, cfg_cpp$reference_backend)
    for (interface in c("multihop", "general")) {
      fit_with <- function(control) {
        if (interface == "multihop") {
          gls_fit(Y, graph, train_rows = 1:14, control = control)
        } else gls_fit_general(Y, graph, Q, train_rows = 1:14, control = control)
      }
      expected <- fit_with(cfg_r)
      seed_r <- .Random.seed
      native <- bdynets:::.gls_ffbs_rank1_cpp
      calls <- 0L
      actual <- with_mocked_bindings(fit_with(cfg_cpp),
        .gls_ffbs_rank1_cpp = function(response, H, omega, W = NULL) {
          calls <<- calls + 1L
          native(response, H, omega, W)
        }, .package = "bdynets")
      context <- paste(interface, evolution)
      expect_identical(.Random.seed, seed_r, info = context)
      expect_gt(calls, 0L, label = context)
      expect_identical(actual$backend, "cpp")
      expect_identical(expected$backend, "R")
      fields <- intersect(c("B", "L", "alpha", "gamma", "common_coef",
        "self_coef", "w_coef", "beta_path", "initial_beta", "tau",
        "reference_mean", "reference_variance", "theta_average", "amplitude"),
        names(expected))
      for (field in fields) {
        expect_lt(max(abs(actual[[field]] - expected[[field]])), 1e-7,
                  label = paste(context, field))
      }
      expect_identical(actual$discount, expected$discount, info = context)
      expect_identical(actual$covariance_acceptance, expected$covariance_acceptance,
                       info = context)
    }
  }
})

test_that("study FFBS retains its distinct multidimensional path and controls", {
  study <- gls_study_control("smoke", factor_dim = 2L)
  expect_identical(study$factor_dim, 2L)
  expect_false("backend" %in% names(study))
  expect_equal(study$reference_newton_tolerance, 1e-8)
  expect_equal(study$reference_newton_max_iterations, 30L)
  expect_equal(study$reference_eta_limit, 30)
  set.seed(391)
  nt <- 5L; k <- 3L; r <- 2L
  ystar <- matrix(rnorm(nt * k), nt, k)
  omega <- matrix(runif(nt * k, .2, 2), nt, k)
  offset <- matrix(rnorm(nt * k, sd = .2), nt, k)
  H <- array(rnorm(nt * k * r, sd = .3), c(nt, k, r))
  W <- array(rep(diag(c(.1, .3)), nt), c(r, r, nt))
  set.seed(399)
  expected <- bdynets:::.gls_study_ffbs_factor_discount(ystar, omega, offset, H, W)
  seed_r <- .Random.seed
  set.seed(399)
  actual <- with_mocked_bindings(
    bdynets:::.gls_study_ffbs_factor_discount(ystar, omega, offset, H, W),
    .gls_ffbs_rank1_cpp = function(...) stop("Unexpected scalar GLS FFBS call"),
    .ffbs_scalar_cpp = function(...) stop("Unexpected scalar generic FFBS call"),
    ffbs_factor_identified = function(...) stop("Unexpected main GLS FFBS call"),
    .package = "bdynets")
  expect_identical(actual, expected)
  expect_identical(.Random.seed, seed_r)
  expect_identical(dim(actual), c(nt + 1L, r))
  expect_null(attr(actual, "initial"))
  expect_true(all(is.finite(actual)))
})
