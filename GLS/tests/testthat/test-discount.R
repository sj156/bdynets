test_that("discount controls reject degenerate grids and normalize weights", {
  expect_error(gls_control(discount_grid = c(.9, 1)), "discount_grid")
  expect_error(gls_control(discount_grid = c(.9, .9)), "discount_grid")
  expect_error(gls_control(dynamic_loading_scale = 0), "dynamic_loading_scale")
  expect_equal(gls_control(discount_grid = c(.9, .95))$discount_prior, c(.5, .5))
})

test_that("reference evolution is predictable and has the zero-design limit", {
  cfg <- gls_control(discount_grid = .9)
  Y <- matrix(c(2, 4, 3, 1, 3, 2), 3, 2)
  H <- array(0, c(3, 2, 1)); offset <- matrix(log(3), 3, 2)
  ref <- GLS:::reference_path(Y, H, offset, .9, cfg)
  expect_equal(ref$W, (1/.9 - 1) * .9^-(0:2))
  expect_equal(ref$C, .9^-3)
  H[] <- .2
  first <- GLS:::reference_path(Y, H, offset, .9, cfg)
  Y[3, ] <- 100
  second <- GLS:::reference_path(Y, H, offset, .9, cfg)
  expect_equal(first$W, second$W)
  expect_false(isTRUE(all.equal(first$C, second$C)))
})

test_that("FFBS uses supplied innovation variance including time zero", {
  set.seed(511)
  draws <- replicate(4000, {
    b <- GLS:::ffbs_factor_identified(matrix(0, 1, 1), array(0, c(1, 1, 1)), matrix(1, 1, 1), W = .04)
    c(initial = attr(b, "initial"), final = b[1, 1])
  })
  expect_equal(var(draws[1, ]), 1, tolerance = .08)
  expect_equal(var(draws[2, ]), 1.04, tolerance = .08)
  expect_lt(abs(var(draws[2, ] - draws[1, ]) - .04), .004)
})

test_that("forecast intensity precedes assimilation and includes static effects", {
  args <- list(beta = 0, ref = list(m = 0, C = 1), discount = .95,
               evolution = "discount", H = matrix(.1, 2, 1), base = c(1, 2), rn = 100)
  set.seed(4); a <- do.call(GLS:::forecast_factor_step, c(args, list(y = c(1, 2))))
  set.seed(4); b <- do.call(GLS:::forecast_factor_step, c(args, list(y = c(10, 20))))
  expect_equal(a$eta, b$eta)
  expect_false(isTRUE(all.equal(a$ref, b$ref)))
  pairs <- data.frame(target = c(1, 2), source = c(2, 1))
  expect_equal(GLS:::static_network(matrix(c(2, 3), 1), pairs, c(.5, 2)), matrix(c(1.5, 4), 1))
  expect_equal(GLS:::static_network(matrix(c(2, 3), 1), pairs, c(.5, 2), c(0, 1)), matrix(c(0, 4), 1))
})

test_that("all-pairs fits save full effects and support legacy comparisons", {
  set.seed(19)
  graph <- gls_grid(2)
  Y <- matrix(rpois(12 * nrow(graph$edges), 3), 12)
  Q <- matrix(sin(seq_len(12)), ncol = 1)
  for (mode in c("discount", "unit")) {
    cfg <- gls_control("smoke", n_iter = 8, burn_mcmc = 4, n_chains = 1,
      state_block_substeps = 1, shrinkage_substeps = 1, scale_move_steps = 1,
      factor_evolution = mode, static_network = mode == "discount", predictive_draws = 2)
    fit <- gls_fit_general(Y, graph, Q, train_rows = 1:8, control = cfg)
    expect_true(all(is.finite(fit$B)))
    expect_true(all(fit$covariance_acceptance >= 0 & fit$covariance_acceptance <= 1))
    if (mode == "discount") expect_true(all(fit$discount %in% cfg$discount_grid))
    else expect_equal(fit$B, matrix(0, 4, nrow(fit$pairs)))
    beta <- as.numeric(fit$beta_path[1, , 1])
    theta <- fit$B[1, ] + outer(fit$L[1, , 1], beta)
    expect_equal(unname(fit$theta_average[1, ]), rowMeans(theta))
    pred <- predict(fit, rows = 9:10, control = cfg)
    expect_true(all(is.finite(pred$lambda)))
  }
})
