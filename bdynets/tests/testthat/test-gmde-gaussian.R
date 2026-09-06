test_that("static conditional matches an independent dense Gaussian reference", {
  expert <- gmde_test_small_expert(); B <- expert$design
  N <- 3; S <- c(1.2, -0.5, 2, 0.8); sigma2 <- 0.7
  # Explicit inverses occur only in this small independent test reference.
  Q0 <- solve(expert$prior$C0)
  V <- solve(Q0 + N / sigma2 * crossprod(B))
  mu <- as.vector(V %*% (Q0 %*% expert$prior$m0 + crossprod(B, S / sigma2)))
  conditional <- bdynets:::.gmde_gaussian_conditional(expert, N, S, sigma2)
  A <- conditional$root %*% backsolve(conditional$precision_chol, diag(2))
  expect_equal(conditional$mean, mu, tolerance = 1e-12)
  expect_equal(tcrossprod(A), V, tolerance = 1e-12)
  # Fixed seed and 6-MC-SE thresholds, with no repeated-until-pass runs.
  set.seed(90603); M <- 6000L
  draws <- replicate(M, bdynets:::.gmde_gaussian_draw(conditional))
  mean_z <- max(abs(rowMeans(draws) - mu) / sqrt(diag(V) / M))
  cov_se <- sqrt((outer(diag(V), diag(V)) + V^2) / (M - 1))
  covariance_z <- max(abs(cov(t(draws)) - V) / cov_se)
  expect_lt(mean_z, 6)
  expect_lt(covariance_z, 6)
})

test_that("extreme scalar precision remains positive without covariance subtraction", {
  expert <- gmde_expert("gaussian", matrix(1, 1, 1), prior = list(
    m0 = 0, C0 = matrix(1.1, 1, 1), variance = list(shape = 2, scale = 1)))
  conditional <- bdynets:::.gmde_gaussian_conditional(expert, 1, 0, 1e-18)
  actual <- (conditional$root[1, 1] / conditional$precision_chol[1, 1])^2
  expected <- 1 / (1 / 1.1 + 1e18)
  expect_equal(actual / expected, 1, tolerance = 1e-12)
  expect_gt(actual, 0)
})

test_that("empty components receive independent proper prior draws", {
  expert <- gmde_test_small_expert(); Y <- gmde_test_small_panel()
  Z <- rep(1L, 6); sufficient <- bdynets:::.gmde_sufficient(Y, Z, 2)
  expect_equal(sufficient$N, c(6L, 0L))
  expect_equal(sufficient$S[2, ], rep(0, 4))
  set.seed(90604); M <- 4000L
  draws <- replicate(M, {
    x <- bdynets:::.gmde_expert_update(Y, Z, sufficient, c(1, 1), expert, 2)
    c(x$beta[2, ], precision = 1 / x$sigma2[2])
  })
  expect_lt(max(abs(rowMeans(draws)[1:2] - expert$prior$m0) /
                  sqrt(diag(expert$prior$C0) / M)), 6)
  C <- expert$prior$C0
  cov_se <- sqrt((outer(diag(C), diag(C)) + C^2) / (M - 1))
  expect_lt(max(abs(cov(t(draws[1:2, ])) - C) / cov_se), 6)
  # IG(shape=3, scale=2) implies precision ~ Gamma(shape=3, rate=2).
  expect_lt(abs(mean(draws[3, ]) - 1.5) / sqrt(0.75 / M), 6)
})

test_that("allocation probabilities match complete-series likelihoods", {
  Y <- gmde_test_small_panel(); eta <- rbind(c(0.1, 0.3, 0.5, 0.7), c(-1, 0, 0.2, 0.3))
  sigma2 <- c(0.4, 1.7); F <- matrix(seq(-1, 1, length.out = 12), 6, 2)
  loglik <- bdynets:::.gmde_gaussian_loglik(Y, eta, sigma2)
  direct <- matrix(0, 6, 2)
  for (i in 1:6) for (k in 1:2)
    direct[i, k] <- sum(dnorm(Y[i, ], eta[k, ], sqrt(sigma2[k]), log = TRUE))
  expect_equal(loglik, direct)
  probability <- exp(bdynets:::.gmde_log_softmax(F + loglik))
  reference <- exp(F + direct) / rowSums(exp(F + direct))
  expect_equal(probability, reference)
  set.seed(90605); M <- 10000L
  samples <- replicate(M, bdynets:::.gmde_categorical(probability))
  expected <- probability[, 1]
  se <- sqrt(expected * (1 - expected) / M)
  expect_lt(max(abs(rowMeans(samples == 1L) - expected) / se), 6)
  expect_equal(exp(bdynets:::.gmde_log_softmax(matrix(c(1e200, 1e200), 1))),
               matrix(c(0.5, 0.5), 1))
})
