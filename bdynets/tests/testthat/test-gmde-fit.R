test_that("K=1,2,3 fits retain valid raw draws and matching sufficient statistics", {
  for (K in 1:3) {
    fit <- gmde_fit(gmde_test_small_panel(), gmde_test_path_graph(), K, gmde_test_small_expert(),
                    mcmc = list(iterations = 30L, warmup = 10L, thin = 2L))
    expect_s3_class(fit, "gmde_fit")
    expect_equal(dim(fit$draws$beta), c(10, K, 2))
    expect_equal(dim(fit$draws$Z), c(10, 6))
    expect_true(all(fit$draws$Z %in% seq_len(K)))
    expect_true(all(fit$draws$sigma2 > 0))
    expect_equal(fit$retained_iterations, seq(12, 30, by = 2))
    expect_equal(fit$final_state$sufficient,
                 bdynets:::.gmde_sufficient(gmde_test_small_panel(), fit$final_state$Z, K))
    expect_equal(rowSums(fit$diagnostics$cluster_sizes), rep(6, 30))
    if (K == 1L) {
      expect_equal(ncol(fit$draws$a), 0)
      expect_true(all(fit$diagnostics$ess_evaluations == 0))
      expect_equal(fit$final_state$utilities, matrix(0, 6, 1))
    } else {
      expect_true(all(fit$draws$a > 0 & fit$draws$a < 1))
      if (K == 2) expect_equal(fit$draws$a[, 1], fit$draws$a[, 2])
      expect_equal(ncol(fit$draws$v), if (K == 2) 1 else 3)
    }
  }
})

test_that("fixed endpoint gates, shared multiclass and singleton panels work", {
  for (a in c(0, 1)) {
    fit <- gmde_fit(gmde_test_small_panel(), gmde_test_path_graph(), 3, gmde_test_small_expert(),
                    gate = gmde_gate("fixed", a), mcmc = list(iterations = 8, warmup = 3))
    expect_true(all(fit$draws$a == a))
    expect_equal(ncol(fit$draws$v), 0)
  }
  fit <- gmde_fit(gmde_test_small_panel(), gmde_test_path_graph(), 3, gmde_test_small_expert(),
                  gate = list(shared = TRUE), mcmc = list(iterations = 8, warmup = 3))
  expect_equal(fit$draws$a[, 1], fit$draws$a[, 3])
  expect_equal(ncol(fit$draws$v), 1)
  Y <- matrix(0.2, 1, 1, dimnames = list("u1", NULL))
  expert <- gmde_expert("gaussian", matrix(1, 1, 1), prior = list(
    m0 = 0, C0 = matrix(1, 1, 1), variance = list(shape = 2, scale = 1)))
  fit <- gmde_fit(Y, gmde_test_path_graph(1), 3, expert,
                  mcmc = list(iterations = 4, warmup = 3))
  expect_equal(dim(fit$draws$beta), c(1, 3, 1))
  expect_true(all(rowSums(fit$diagnostics$cluster_sizes == 0) == 2))
  expect_equal(unname(gmde_partition(fit)$psm), matrix(1, 1, 1))
})

test_that("explicit seeds reproduce draws and restore caller RNG", {
  set.seed(90606); before <- .Random.seed
  fit <- gmde_fit(gmde_test_small_panel(), gmde_test_path_graph(), 2, gmde_test_small_expert(),
                  mcmc = list(iterations = 10, warmup = 5))
  expect_identical(.Random.seed, before)
  again <- gmde_fit(gmde_test_small_panel(), gmde_test_path_graph(), 2, gmde_test_small_expert(),
                    mcmc = list(iterations = 10, warmup = 5))
  expect_identical(fit$draws, again$draws)
  gmde_partition(fit)
  capture.output(print(fit))
  expect_identical(.Random.seed, before)
})

test_that("unsupported and ambiguous inputs fail clearly", {
  expert <- gmde_test_small_expert(); Y <- gmde_test_small_panel(); graph <- gmde_test_path_graph()
  expect_error(gmde_expert("poisson", expert$design, prior = expert$prior), "Milestone 1")
  expect_error(gmde_expert("gaussian", expert$design, "dynamic", expert$prior), "Milestone 1")
  expect_error(gmde_fit(unname(Y), graph, 2, expert), "row names")
  expect_error(gmde_fit(Y[6:1, ], graph, 2, expert), "row names")
  Y[1, 1] <- NA
  expect_error(gmde_fit(Y, graph, 2, expert), "finite")
  expect_error(gmde_fit(gmde_test_small_panel(), graph, 2, expert, gate = list(typo = 1)), "Unknown")
  expect_error(gmde_mcmc(iterations = 10, warmup = 10), "warmup")
  expect_error(gmde_gate(a = 0), "strictly")
})

test_that("representative partition minimizes full matrix loss with first ties", {
  Z <- rbind(c(1, 1, 2), c(2, 2, 1), c(1, 2, 2))
  fit <- structure(list(draws = list(Z = Z), unit_ids = letters[1:3],
                        retained_iterations = c(10L, 20L, 30L)), class = "gmde_fit")
  part <- gmde_partition(fit)
  expected <- matrix(c(1, 2/3, 0, 2/3, 1, 1/3, 0, 1/3, 1), 3)
  expect_equal(unname(part$psm), expected)
  expect_equal(part$draw_index, 1)
  expect_equal(part$iteration, 10)
  expect_equal(unname(part$partition), c(1L, 1L, 2L))
})
