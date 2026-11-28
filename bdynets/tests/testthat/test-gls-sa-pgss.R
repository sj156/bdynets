test_that("zero discount innovation reproduces the exact Gamma-Poisson filter", {
  y <- cbind(c(2,0,3,0,1,4,2,1), c(7,3,2,6,2,0,3,5))
  d <- 0.8
  grid <- data.frame(discount = d, phi = .8, sigma = 0)
  set.seed(2)
  before <- .Random.seed
  p <- gls_sa_pgss(y, 1:4, 5:6, 7:8, grid, 20L, 30L, 200L)
  expect_identical(.Random.seed, before)
  one <- gls_sa_pgss(y[,1], 1:4, 5:6, 7:8, grid, 20L, 30L, 200L)
  expect_identical(one$mean[,1], p$mean[,1])
  for (a in seq_len(ncol(y))) {
    shape <- rate <- 1
    mu <- score <- numeric(nrow(y))
    for (t in seq_len(nrow(y))) {
      mu[t] <- shape / rate
      score[t] <- dnbinom(y[t,a], size = d*shape, mu = mu[t], log = TRUE)
      shape <- d*shape + y[t,a]
      rate <- d*rate + 1
    }
    expect_equal(p$mean[,a], mu[7:8])
    expect_equal(p$logprob[,a], score[7:8])
    expect_equal(p$validation_scores[1,a], sum(score[5:6]))
  }
})

test_that("sa-PGSS selects and predicts before assimilating test observations", {
  y <- matrix(1:24 %% 6, ncol = 2)
  change <- y
  change[10:12,] <- 20
  grid <- expand.grid(discount = c(.7,.95), phi = .8, sigma = .2)
  run <- function(x) gls_sa_pgss(x, 1:6, 7:9, 10:12, grid, 15L, 25L, 23L)
  p <- run(y)
  altered <- run(change)
  expect_identical(p, run(y))
  expect_identical(p$selected, altered$selected)
  expect_identical(p$validation_scores, altered$validation_scores)
  expect_identical(p$mean[1,], altered$mean[1,])
  expect_identical(p$lower[1,], altered$lower[1,])
  expect_true(all(p$lower <= p$upper))
})
