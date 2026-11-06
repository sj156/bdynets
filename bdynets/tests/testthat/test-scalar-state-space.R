# Small, independent R reference for an arbitrary scalar linear Gaussian model.
# No GLS-specific defaults or numerical jitter are used in this calculation.
scalar_ffbs_reference <- function(y, h, w, q, phi, m0, C0) {
  nt <- nrow(y)
  m <- C <- a <- R <- numeric(nt)
  previous_m <- m0
  previous_C <- C0
  for (tt in seq_len(nt)) {
    a[tt] <- phi[tt] * previous_m
    R[tt] <- phi[tt]^2 * previous_C + q[tt]
    observed <- w[tt, ] > 0
    information <- sum(h[tt, observed]^2 * w[tt, observed])
    score <- sum(h[tt, observed] * w[tt, observed] * y[tt, observed])
    C[tt] <- 1 / (1 / R[tt] + information)
    m[tt] <- C[tt] * (a[tt] / R[tt] + score)
    previous_m <- m[tt]
    previous_C <- C[tt]
  }
  state <- numeric(nt)
  state[nt] <- rnorm(1L, m[nt], sqrt(C[nt]))
  if (nt > 1L) for (tt in (nt - 1L):1L) {
    gain <- C[tt] * phi[tt + 1L] / R[tt + 1L]
    state[tt] <- rnorm(1L, m[tt] + gain * (state[tt + 1L] - a[tt + 1L]),
                      sqrt(C[tt] - gain^2 * R[tt + 1L]))
  }
  gain0 <- C0 * phi[1L] / R[1L]
  initial <- rnorm(1L, m0 + gain0 * (state[1L] - a[1L]),
                   sqrt(C0 - gain0^2 * R[1L]))
  list(state = state, initial = initial)
}

test_that("Gaussian FFBS matches an independent smoother for arbitrary transitions", {
  set.seed(212)
  nt <- 20L
  k <- 3L
  y <- matrix(rnorm(nt * k), nt, k)
  h <- matrix(rnorm(nt * k, sd = .7), nt, k)
  w <- matrix(runif(nt * k, .1, 3), nt, k)
  w[cbind(c(2L, 11L), c(3L, 1L))] <- 0
  y[w == 0] <- NA_real_
  h[w == 0] <- NA_real_
  q <- seq(.02, .4, length.out = nt)
  phi <- seq(-.8, 1.1, length.out = nt)
  m0 <- -1.7
  C0 <- .43
  set.seed(451)
  expected <- scalar_ffbs_reference(y, h, w, q, phi, m0, C0)
  seed_r <- .Random.seed
  set.seed(451)
  actual <- gaussian_ffbs(y, loadings = h, precision = w,
    innovation_variance = q, transition = phi,
    initial_mean = m0, initial_variance = C0)
  expect_identical(.Random.seed, seed_r)
  expect_type(actual, "list")
  expect_named(actual, c("state", "initial"))
  expect_length(actual$state, nt)
  expect_length(actual$initial, 1L)
  expect_lt(max(abs(actual$state - expected$state)), 1e-10)
  expect_lt(abs(actual$initial - expected$initial), 1e-10)
})

test_that("Gaussian FFBS has the analytic one-step joint posterior", {
  m0 <- 1.2
  C0 <- .6
  phi <- .7
  q <- .2
  y <- -.3
  h <- 1.4
  precision <- 2.5
  prior_mean <- c(m0, phi * m0)
  prior_covariance <- matrix(c(C0, phi * C0, phi * C0, phi^2 * C0 + q), 2L)
  covariance_y <- prior_covariance[, 2L] * h
  variance_y <- h^2 * prior_covariance[2L, 2L] + 1 / precision
  posterior_mean <- prior_mean + covariance_y * (y - h * prior_mean[2L]) / variance_y
  posterior_covariance <- prior_covariance - tcrossprod(covariance_y) / variance_y
  set.seed(671)
  draws <- replicate(5000L, {
    sample <- gaussian_ffbs(y, loadings = h, precision = precision,
      innovation_variance = q, transition = phi,
      initial_mean = m0, initial_variance = C0)
    c(sample$initial, sample$state)
  })
  expect_lt(max(abs(rowMeans(draws) - posterior_mean)), .035)
  expect_lt(max(abs(stats::cov(t(draws)) - posterior_covariance)), .025)
})

test_that("zero observation precision gives the specified prior transition", {
  m0 <- -.8
  C0 <- .7
  phi <- -1.2
  q <- .3
  set.seed(225)
  draws <- replicate(5000L, {
    sample <- gaussian_ffbs(NA_real_, loadings = NA_real_, precision = 0,
      innovation_variance = q, transition = phi,
      initial_mean = m0, initial_variance = C0)
    c(sample$initial, sample$state)
  })
  prior_covariance <- matrix(c(C0, phi * C0, phi * C0, phi^2 * C0 + q), 2L)
  expect_lt(max(abs(rowMeans(draws) - c(m0, phi * m0))), .05)
  expect_lt(max(abs(stats::cov(t(draws)) - prior_covariance)), .065)
  innovation <- draws[2L, ] - phi * draws[1L, ]
  expect_lt(abs(mean(innovation)), .03)
  expect_lt(abs(var(innovation) - q), .025)
})

test_that("broadcasting is explicit and small variances have no implicit jitter", {
  y <- matrix(seq_len(12L) / 5, 4L, 3L)
  set.seed(41)
  scalar <- gaussian_ffbs(y, loadings = .4, precision = 2,
    innovation_variance = .2, transition = .8)
  set.seed(41)
  expanded <- gaussian_ffbs(y, loadings = matrix(.4, 4L, 3L),
    precision = matrix(2, 4L, 3L), innovation_variance = rep(.2, 4L),
    transition = rep(.8, 4L))
  expect_identical(scalar, expanded)
  h <- seq(.1, .4, length.out = 4L)
  w <- seq(1, 2, length.out = 4L)
  set.seed(55)
  time_vectors <- gaussian_ffbs(y, loadings = h, precision = w)
  set.seed(55)
  time_matrices <- gaussian_ffbs(y,
    loadings = matrix(rep(h, 3L), 4L, 3L),
    precision = matrix(rep(w, 3L), 4L, 3L))
  expect_identical(time_vectors, time_matrices)
  set.seed(52)
  vector_response <- gaussian_ffbs(y[, 1L])
  set.seed(52)
  matrix_response <- gaussian_ffbs(y[, 1L, drop = FALSE])
  expect_identical(vector_response, matrix_response)

  tiny_y <- matrix(0, 2L, 1L)
  tiny_h <- matrix(0, 2L, 1L)
  tiny_w <- matrix(1, 2L, 1L)
  set.seed(277)
  expected <- scalar_ffbs_reference(tiny_y, tiny_h, tiny_w,
    rep(1e-14, 2L), rep(.5, 2L), 0, 2e-14)
  set.seed(277)
  actual <- gaussian_ffbs(c(0, 0), loadings = 0, innovation_variance = 1e-14,
    transition = .5, initial_variance = 2e-14)
  expect_lt(max(abs(actual$state - expected$state)), 1e-15)
  expect_lt(abs(actual$initial - expected$initial), 1e-15)
})

test_that("Gaussian FFBS rejects invalid observation and state parameters", {
  y <- matrix(1, 3L, 4L)
  expect_error(gaussian_ffbs(matrix(numeric(), 0L, 2L)))
  expect_error(gaussian_ffbs(array(1, c(3L, 2L, 2L))))
  expect_error(gaussian_ffbs(y, loadings = c(1, 2)))
  expect_error(gaussian_ffbs(y, precision = matrix(1, 2L, 4L)))
  expect_error(gaussian_ffbs(y, precision = -1))
  expect_error(gaussian_ffbs(y, precision = NA_real_))
  expect_error(gaussian_ffbs(y, loadings = Inf))
  missing_y <- y; missing_y[1L] <- NA_real_
  expect_error(gaussian_ffbs(missing_y))
  for (bad in list(0, -1, NA_real_, Inf, c(.1, .2))) {
    expect_error(gaussian_ffbs(y, innovation_variance = bad))
    expect_error(gaussian_ffbs(y, initial_variance = bad))
  }
  for (bad in list(NA_real_, Inf, c(.1, .2))) {
    expect_error(gaussian_ffbs(y, transition = bad))
    expect_error(gaussian_ffbs(y, initial_mean = bad))
  }
  for (bad in list(-1, NA_real_, Inf, c(0, 0))) {
    expect_error(gaussian_ffbs(y, jitter = bad))
  }
})

test_that("the zero-loading Poisson filter respects custom initial conditions", {
  nt <- 8L
  discount <- .91
  m0 <- -1.4
  C0 <- .37
  set.seed(412)
  seed_before <- .Random.seed
  actual <- poisson_discount_filter(matrix(2, nt, 3L), loadings = 0,
    offset = 2, discount = discount, initial_mean = m0, initial_variance = C0)
  expect_identical(.Random.seed, seed_before)
  expect_named(actual, c("innovation_variance", "filtered_mean", "filtered_variance",
                         "terminal_mean", "terminal_variance"))
  expect_equal(actual$filtered_mean, rep(m0, nt), tolerance = 1e-9)
  expect_equal(actual$filtered_variance, C0 * discount^(-seq_len(nt)), tolerance = 1e-12)
  expect_equal(actual$innovation_variance,
    (1 / discount - 1) * C0 * discount^(-(seq_len(nt) - 1L)), tolerance = 1e-12)
  expect_equal(actual$terminal_mean, actual$filtered_mean[nt], tolerance = 0)
  expect_equal(actual$terminal_variance, actual$filtered_variance[nt], tolerance = 0)
})

test_that("the Poisson filter solves scalar modes and uses predictable innovations", {
  nt <- 12L
  set.seed(818)
  counts <- matrix(rpois(nt * 3L, 6), nt, 3L)
  h <- matrix(runif(nt * 3L, .1, .4), nt, 3L)
  offset <- matrix(rnorm(nt * 3L, 1, .2), nt, 3L)
  discount <- .94
  m0 <- .3
  C0 <- .7
  first <- poisson_discount_filter(counts, loadings = h, offset = offset,
    discount = discount, initial_mean = m0, initial_variance = C0)
  previous_m <- m0
  previous_C <- C0
  for (tt in seq_len(nt)) {
    R <- previous_C / discount
    b <- first$filtered_mean[tt]
    rate <- exp(offset[tt, ] + h[tt, ] * b)
    score <- (b - previous_m) / R + sum(h[tt, ] * (rate - counts[tt, ]))
    expect_lt(abs(score), 1e-6)
    expected_C <- 1 / (1 / R + sum(h[tt, ]^2 * rate))
    expect_equal(first$filtered_variance[tt], expected_C, tolerance = 1e-10)
    expect_equal(first$innovation_variance[tt], (1 / discount - 1) * previous_C,
                 tolerance = 1e-12)
    previous_m <- b
    previous_C <- first$filtered_variance[tt]
  }
  changed <- counts
  changed[8L, ] <- changed[8L, ] + 30
  second <- poisson_discount_filter(changed, loadings = h, offset = offset,
    discount = discount, initial_mean = m0, initial_variance = C0)
  expect_identical(first$innovation_variance[1:8], second$innovation_variance[1:8])
  expect_identical(first$filtered_mean[1:7], second$filtered_mean[1:7])
  expect_gt(abs(first$filtered_variance[8L] - second$filtered_variance[8L]), 1e-6)
  expect_gt(abs(first$innovation_variance[9L] - second$innovation_variance[9L]), 1e-8)
})

test_that("Poisson filter broadcasts designs and validates numerical controls", {
  counts <- matrix(c(0, 3, 5, 2, 4, 1), 3L, 2L)
  scalar <- poisson_discount_filter(counts, loadings = -.2, offset = .4)
  expanded <- poisson_discount_filter(counts,
    loadings = matrix(-.2, 3L, 2L), offset = matrix(.4, 3L, 2L))
  expect_identical(scalar, expanded)
  vector <- poisson_discount_filter(counts[, 1L])
  matrix <- poisson_discount_filter(counts[, 1L, drop = FALSE])
  expect_identical(vector, matrix)
  expect_error(poisson_discount_filter(matrix(numeric(), 0L, 2L)))
  expect_error(poisson_discount_filter(array(1, c(3L, 2L, 2L))))
  expect_error(poisson_discount_filter(counts, loadings = c(1, 2)))
  expect_error(poisson_discount_filter(counts, offset = Inf))
  expect_error(poisson_discount_filter(counts, loadings = NA_real_))
  for (bad in c(-1, 1.5, NA_real_, Inf)) {
    changed <- counts; changed[1L] <- bad
    expect_error(poisson_discount_filter(changed))
  }
  for (bad in list(0, 1, -1, NA_real_, Inf, c(.9, .95))) {
    expect_error(poisson_discount_filter(counts, discount = bad))
  }
  for (bad in list(0, -1, NA_real_, Inf, c(.1, .2))) {
    expect_error(poisson_discount_filter(counts, initial_variance = bad))
    expect_error(poisson_discount_filter(counts, tolerance = bad))
  }
  for (bad in list(NA_real_, Inf, c(.1, .2))) {
    expect_error(poisson_discount_filter(counts, initial_mean = bad))
  }
  for (bad in list(0, -1, 1.5, NA_real_, Inf, c(10L, 20L))) {
    expect_error(poisson_discount_filter(counts, max_iterations = bad))
  }
  expect_error(poisson_discount_filter(100, max_iterations = 1L))
})
