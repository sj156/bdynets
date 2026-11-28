test_that("lag scaling uses training data and preserves singleton dimensions", {
  y <- matrix(c(3, 7, 1, 1500), ncol = 1)
  x <- gls_lag_predictors(y, 1:3, matrix(1))
  raw <- log1p(c(1, 3, 7, 1))
  expect_equal(x$X, matrix((raw - mean(raw[1:3])) / sd(raw[1:3]), ncol = 1))
  dimnames(y) <- list(paste0("t", 1:4), "series")
  expect_identical(dimnames(gls_lag_predictors(y, 1:3)$X), dimnames(y))
  expect_equal(gls_lag_predictors(matrix(0, 3, 1), 1L)$scale, 1)
  expect_error(gls_lag_predictors(y, c(1L, 1L)), "indices")
})

test_that("forecast comparisons retain zeros, impossible outcomes and empty windows", {
  observed <- matrix(c(0, 2, 0, 1), 2)
  mean <- matrix(c(0, 0, 1, 1), 2)
  quiet <- matrix(FALSE, 2, 2)
  scores <- gls_forecast_metrics(observed, list(mean = mean),
    intensity = mean, windows = list(quiet = quiet))
  expect_identical(scores$value[scores$window == "all" & scores$metric == "deviance"], Inf)
  expect_equal(scores$value[scores$window == "all" & scores$metric == "intensity_rmse"], 0)
  expect_true(all(is.na(scores$value[scores$window == "quiet"])))
  expect_true(all(scores$cells[scores$window == "quiet"] == 0))
  expect_error(gls_forecast_metrics(observed, list(mean = mean), windows = list(all = quiet)),
    "unique name")
  expect_error(gls_forecast_metrics(observed, list(mean = mean, lower = observed)), "both")
  lambda <- array(1, c(2, 2, 3))
  mixture <- gls_poisson_mixture_forecast(lambda, observed, array(2, dim(lambda)))
  expect_equal(mixture$logprob, dpois(observed, 1, log = TRUE))
  expect_equal(mixture$upper, matrix(2, 2, 2))
})

test_that("structural summaries use the specified support and pair denominator", {
  truth <- cbind(c(1, 2), c(0, 0))
  groups <- list(both = c(TRUE, TRUE), neither = c(FALSE, FALSE))
  result <- gls_trajectory_metrics(truth, truth * 0, c(FALSE, FALSE), groups)
  value <- setNames(result$value[result$window == "both"], result$metric[result$window == "both"])
  expect_equal(unname(value["trajectory_rmse"]), sqrt(5 / 4))
  expect_equal(unname(value["recall"]), 0)
  expect_true(is.na(value["precision"]))
  expect_true(all(result$cells[result$window == "neither"] == 0))
})

test_that("a failed atomic write preserves the previous result", {
  dest <- tempfile()
  on.exit(unlink(dest))
  bdynets_atomic_write(dest, function(path) saveRDS("first", path))
  expect_error(bdynets_atomic_write(dest, function(path) {
    saveRDS("partial", path)
    stop("interrupted")
  }), "interrupted")
  expect_identical(readRDS(dest), "first")
  bdynets_atomic_write(dest, function(path) saveRDS("second", path))
  expect_identical(readRDS(dest), "second")
})
