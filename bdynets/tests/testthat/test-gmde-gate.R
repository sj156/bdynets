gate_covariance <- function(graph, K, a, s_b = 0.8) {
  H <- bdynets:::.gmde_contrasts(K)
  factors <- bdynets:::.gmde_class_factors(H, a)
  A <- cbind(s_b * kronecker(H, matrix(1, nrow(graph$Phi), 1)),
             kronecker(H %*% factors$graph, graph$Phi),
             graph$tau * kronecker(H %*% factors$independent, diag(nrow(graph$Phi))))
  tcrossprod(A)
}

test_that("white utilities match the projected independent-class prior", {
  graph <- gmde_test_path_graph(tau = 1.7); n <- 6L
  for (K in c(2L, 3L, 5L)) {
    H <- bdynets:::.gmde_contrasts(K)
    P <- diag(K) - matrix(1 / K, K, K)
    expect_equal(crossprod(H), diag(K - 1L))
    expect_equal(tcrossprod(H), P)
    a <- if (K == 2) rep(0.3, K) else seq(0.1, 0.8, length.out = K)
    raw <- matrix(0, n * K, n * K)
    for (k in seq_len(K)) {
      at <- (k - 1L) * n + seq_len(n)
      raw[at, at] <- 0.8^2 + a[k] * graph$covariance +
        (1 - a[k]) * graph$tau^2 * diag(n)
    }
    projection <- kronecker(P, diag(n))
    expect_equal(gate_covariance(graph, K, a), projection %*% raw %*% projection,
                 tolerance = 1e-10)
    x <- list(c = sin(seq_len(K - 1L)),
              Gamma = matrix(cos(seq_len(n * (K - 1L))), n),
              E = matrix(sin(seq_len(n * (K - 1L))), n))
    f <- bdynets:::.gmde_utilities(x, graph, H, gmde_gate(s_b = 0.8), a)
    expect_equal(unname(rowSums(f)), rep(0, n), tolerance = 1e-12)
    factors <- bdynets:::.gmde_class_factors(H, a)
    A <- cbind(0.8 * kronecker(H, matrix(1, n, 1)),
               kronecker(H %*% factors$graph, graph$Phi),
               graph$tau * kronecker(H %*% factors$independent, diag(n)))
    expect_equal(as.vector(f), as.vector(A %*% unlist(x)), tolerance = 1e-12)
  }
})

test_that("endpoints and identity geometry have the required covariance", {
  g <- gmde_test_path_graph(); K <- 3L
  P <- diag(K) - matrix(1 / K, K, K)
  expect_equal(gate_covariance(g, K, rep(0, K)),
               kronecker(P, 0.8^2 * matrix(1, 6, 6) + diag(6)))
  expect_equal(gate_covariance(g, K, rep(1, K)),
               unname(kronecker(P, 0.8^2 * matrix(1, 6, 6) + g$covariance)))
  identity <- gmde_graph(matrix(0, 6, 6), g$unit_ids)
  expect_equal(gate_covariance(identity, K, c(0.1, 0.5, 0.9)),
               gate_covariance(identity, K, c(0.8, 0.2, 0.4)))
  # Zero guidance eliminates any effect of changed graph geometry.
  expect_equal(gate_covariance(g, K, rep(0, K)),
               gate_covariance(identity, K, rep(0, K)))
})

test_that("binary/shared weights count one Jacobian-adjusted Beta prior", {
  gate <- gmde_gate(beta_shape1 = 2, beta_shape2 = 3)
  expect_equal(bdynets:::.gmde_resolve_gate(gate, 2)$free, 1)
  expect_equal(bdynets:::.gmde_resolve_gate(gate, 3)$free, 3)
  expect_equal(bdynets:::.gmde_resolve_gate(gmde_gate(shared = TRUE), 3)$free, 1)
  expect_error(bdynets:::.gmde_resolve_gate(gmde_gate(a = c(0.2, 0.8)), 2), "tied")
  for (v in c(-8, -1, 0, 2, 8)) {
    a <- plogis(v)
    reference <- dbeta(a, 2, 3, log = TRUE) + log(a) + log1p(-a) + lbeta(2, 3)
    expect_equal(bdynets:::.gmde_logit_prior(v, gate), reference, tolerance = 1e-10)
  }
  pair <- bdynets:::.gmde_weight_pair(40, 2, TRUE)
  expect_true(all(pair$independent > 0))
  expect_true(is.finite(bdynets:::.gmde_logit_prior(1000, gate)))
  expect_error(bdynets:::.gmde_weight_pair(1000, 2, TRUE), "floating-point")
})

test_that("ESS rotates the complete white state and cached utilities together", {
  set.seed(90602)
  g <- gmde_test_path_graph(); H <- bdynets:::.gmde_contrasts(3); gate <- gmde_gate()
  a <- c(0.2, 0.5, 0.8)
  x <- bdynets:::.gmde_white_draw(6, 6, 2)
  F <- bdynets:::.gmde_utilities(x, g, H, gate, a)
  result <- bdynets:::.gmde_ess_update(x, F, c(1, 1, 2, 2, 3, 3), g, H, gate,
                                   a, 1 - a, Inf)
  expect_equal(result$F, bdynets:::.gmde_utilities(result$x, g, H, gate, a),
               tolerance = 1e-12)
  expect_gte(result$evaluations, 1)
})

test_that("actual guidance transition matches direct Beta density and Jacobian", {
  g <- gmde_test_path_graph()
  for (K in c(2L, 3L)) for (shared in c(TRUE, FALSE)) {
    gate <- bdynets:::.gmde_resolve_gate(gmde_gate(shared = shared,
      beta_shape1 = 2, beta_shape2 = 3), K)
    H <- bdynets:::.gmde_contrasts(K)
    set.seed(90607)
    x <- bdynets:::.gmde_white_draw(6, 6, K - 1)
    v <- rep(-0.3, gate$free)
    pair <- bdynets:::.gmde_weight_pair(v, K, gate$shared)
    F <- bdynets:::.gmde_utilities(x, g, H, gate, pair$a)
    Z <- rep(seq_len(K), length.out = 6)
    reference_v <- v; reference_F <- F
    accepted <- logical(gate$free)
    set.seed(90608)
    for (h in seq_len(gate$free)) {
      proposal <- reference_v
      proposal[h] <- proposal[h] + rnorm(1, sd = gate$rw_sd)
      a <- plogis(proposal)
      Fc <- bdynets:::.gmde_utilities(x, g, H, gate,
        if (gate$shared) rep(a, K) else a)
      direct_density <- function(value) {
        weight <- plogis(value)
        dbeta(weight, 2, 3, log = TRUE) + log(weight) + log1p(-weight)
      }
      ratio <- sum(log(exp(Fc) / rowSums(exp(Fc)))[cbind(1:6, Z)]) -
        sum(log(exp(reference_F) / rowSums(exp(reference_F)))[cbind(1:6, Z)]) +
        direct_density(proposal[h]) - direct_density(reference_v[h])
      if (log(runif(1)) < ratio) {
        reference_v <- proposal; reference_F <- Fc; accepted[h] <- TRUE
      }
    }
    set.seed(90608)
    actual <- bdynets:::.gmde_guidance_update(x, F, v, Z, g, H, gate)
    expect_equal(actual$v, reference_v)
    expect_equal(actual$F, reference_F, tolerance = 1e-12)
    expect_identical(actual$accepted, accepted)
  }
})
