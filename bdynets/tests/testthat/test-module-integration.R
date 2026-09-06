test_that("all module entry points share the bdynets namespace", {
  exports <- getNamespaceExports("bdynets")
  expect_true(all(c("gmde_fit", "gmde_graph", "gmde_expert", "gmde_gate",
                    "gmde_mcmc", "gmde_partition", "gibbs_sampler",
                    "adj_rand_index", "best_label_accuracy", "%>%") %in% exports))
  expect_false(any(grepl("^\\.gmde_", exports)))
  fit <- gmde_fit(gmde_test_small_panel(), gmde_test_path_graph(), 2,
                  gmde_test_small_expert(), mcmc = list(iterations = 8, warmup = 4))
  expect_identical(fit$metadata$package_name, "bdynets")
  expect_identical(fit$metadata$package_version, "0.1.0")
  expect_s3_class(fit, "gmde_fit")
})

test_that("historical clustering utilities remain callable", {
  x <- c(1, 1, 2, 2, 3, 3)
  y <- c(3, 3, 1, 1, 2, 2)
  expect_equal(adj_rand_index(x, y), 1)
  expect_equal(best_label_accuracy(x, y, 3), 1)
  Y <- matrix(1:12, 3, 4)
  sufficient <- bdynets:::cluster_stats(c(1, 1, 2), Y, 3)
  expect_equal(sufficient$N, c(2, 1, 0))
  expect_equal(sufficient$S[1, ], colSums(Y[1:2, , drop = FALSE]))
  expect_equal(sufficient$S[3, ], rep(0, 4))
})

test_that("historical sampler, summary and plot still work after integration", {
  # A short compatibility smoke test, not validation of its approximate target.
  Y <- matrix(rep(c(0, 1, 2, 1), 6), 6, 4)
  B <- cbind(1, seq(-1, 1, length.out = 4))
  fit <- gibbs_sampler(Y, B, K = 2, n_iter = 4, burn = 2, r_tune = 2,
                       print_freq = 100, store_theta = TRUE, seed = 90609)
  expect_s3_class(fit, "bdynets_mcmc")
  expect_equal(dim(fit$theta), c(4, 2, 4, 2))
  expect_equal(dim(fit$Z), c(4, 6))
  expect_true(all(is.finite(fit$lambda)))
  expect_equal(rowSums(fit$size), rep(6, 4))
  expect_output(summary(fit), "Bayesian Dynamic Network MCMC Summary")
  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  tryCatch(expect_identical(plot(fit, type = "trace"), fit),
           finally = grDevices::dev.off())
  expect_gt(file.info(plot_file)$size, 0)
  unlink(plot_file)
})
