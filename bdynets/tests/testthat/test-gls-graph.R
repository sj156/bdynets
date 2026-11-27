test_that("directed edge construction, supplied weights, and temporal splits retain their meaning", {
  edges <- data.frame(from = c("A", "B", "B", "D"), to = c("B", "C", "A", "E"), name = letters[1:4])
  transitions <- gls_graph(edges)
  expect_equal(unname(transitions$A[, 1]), c(0, 1, 1, 0))
  expect_equal(unname(gls_graph(edges, exclude_uturns = TRUE)$A[, 1]), c(0, 1, 0, 0))
  expect_equal(unname(transitions$W[4, ]), rep(0, 4))
  fractional <- transitions$A / 4
  expect_identical(gls_graph(edges, fractional)$W, transitions$W)
  expect_error(gls_graph(edges, fractional[4:1, ]), "dimnames")
  expect_identical(gls_split_rows(11), list(train = 1:7, validation = 8:9, test = 10:11))
  expect_identical(gls_split_rows(15L, 8L, 10L), list(train = 1:8, validation = 9:10, test = 11:15))
  expect_error(gls_split_rows(10L, 8L, 8L), "boundaries")
})

test_that("directed hop distances and public pair dictionaries match dense powers", {
  reference <- function(A) {
    k <- nrow(A)
    out <- matrix(Inf, k, k)
    diag(out) <- 0
    reach <- diag(k)
    for (d in seq_len(k - 1L)) {
      reach <- reach %*% A
      out[reach > 0 & !is.finite(out)] <- d
    }
    out
  }
  grid <- gls_grid(3L)
  edges <- grid$edges
  previous <- matrix(0, nrow(edges), nrow(edges), dimnames = list(target = edges$name, source = edges$name))
  for (a in seq_len(nrow(edges))) for (b in seq_len(nrow(edges))) {
    previous[a, b] <- as.numeric(edges$to[b] == edges$from[a] && edges$from[b] != edges$to[a])
  }
  expect_identical(grid$A, previous)
  expect_identical(grid$W, previous / rowSums(previous))
  set.seed(1439)
  adj <- matrix(sample(c(0, 0, 0, .25, 2), 121, TRUE), 11)
  signed <- adj
  signed[1, 2] <- -1
  for (A in list(adj, signed, matrix(0, 1, 1), matrix(0, 7, 7), grid$A)) {
    expect_identical(hop_matrix(A), reference(A))
  }
  for (pairs in list(gls_all_pairs(grid), gls_pairs(grid))) {
    expect_identical(pairs$hop, reference(grid$A)[cbind(pairs$target, pairs$source)])
  }
})
