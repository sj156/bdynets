test_that("sparse pair products preserve duplicates, missing targets, and ordering", {
    set.seed(702)
    k <- 20L
    X <- matrix(rnorm(7L * k), 7L, k,
        dimnames = list(paste0("time", 1:7), paste0("source", seq_len(k))))
    pairs <- data.frame(source = c(8L, 2L, 2L, 19L, 8L, 2L),
        target = c(4L, 1L, 1L, 12L, 4L, 17L))
    values <- c(.5, 2, -1, 3, .25, -.2)
    dense <- X %*% pair_coefficient_matrix(values, pairs, k)
    seed <- .Random.seed
    expect_equal(.gls_pair_product(X, pairs, values), dense, tolerance = 1e-13)
    expect_identical(.Random.seed, seed)
    expect_identical(dimnames(.gls_pair_product(X, pairs, values)), dimnames(dense))
    order <- c(4, 3, 6, 2, 1, 5)
    expect_equal(.gls_pair_product(X, pairs[order, ], values[order]), dense, tolerance = 1e-13)
    expect_equal(.gls_pair_product(X[1L, , drop = FALSE], pairs, values),
        dense[1L, , drop = FALSE], tolerance = 1e-13)
    expect_identical(unname(.gls_pair_product(X, pairs[FALSE, ], numeric())),
        matrix(0, nrow(X), k))
})

test_that("static and multiple-factor products retain their output contracts", {
    set.seed(703)
    k <- 30L
    X <- matrix(rnorm(5L * k), 5L, k, dimnames = list(letters[1:5], NULL))
    pairs <- data.frame(source = c(2L, 8L, 2L, 15L), target = c(1L, 20L, 1L, 7L))
    B <- rnorm(nrow(pairs))
    d <- c(1, 0, .5, 2)
    expect_equal(static_network(X, pairs, B, d),
        unname(X %*% pair_coefficient_matrix(B * d, pairs, k)), tolerance = 1e-13)
    expect_identical(dimnames(static_network(X, pairs, B, d)), NULL)
    expect_identical(static_network(X, pairs, NULL), matrix(0, nrow(X), ncol(X)))
    L <- matrix(rnorm(nrow(pairs) * 2L), nrow(pairs), 2L)
    expected <- array(0, c(nrow(X), k, 2L))
    for (j in 1:2) expected[, , j] <- X %*% pair_coefficient_matrix(L[, j], pairs, k)
    actual <- build_network_array(X, pairs, L, 2L, k)
    expect_equal(actual, expected, tolerance = 1e-13)
    expect_identical(dimnames(actual), NULL)
    expect_equal(build_network_array(X, pairs, L[, 1L, drop = FALSE], 1L, k),
        expected[, , 1L, drop = FALSE], tolerance = 1e-13)
})

test_that("dense products and nonfinite inputs retain dense semantics", {
    set.seed(704)
    k <- 8L
    X <- matrix(rnorm(4L * k), 4L, k)
    pairs <- expand.grid(source = seq_len(k), target = seq_len(k))
    values <- rnorm(nrow(pairs))
    expect_identical(.gls_pair_product(X, pairs, values),
        X %*% pair_coefficient_matrix(values, pairs, k))
    sparse_pairs <- data.frame(source = c(1L, 3L), target = c(2L, 7L))
    X[2L, 8L] <- NA_real_
    expect_equal(.gls_pair_product(X, sparse_pairs, c(1, 2)),
        X %*% pair_coefficient_matrix(c(1, 2), sparse_pairs, k))
    X[2L, 8L] <- 0
    expect_identical(.gls_pair_product(X, sparse_pairs, 2),
        X %*% pair_coefficient_matrix(2, sparse_pairs, k))
    expect_equal(.gls_pair_product(X, sparse_pairs, c(Inf, 2)),
        X %*% pair_coefficient_matrix(c(Inf, 2), sparse_pairs, k))
    fractional <- data.frame(source = c(1.5, 3), target = c(2.25, 7))
    expect_identical(.gls_pair_product(X, fractional, c(1, 2)),
        X %*% pair_coefficient_matrix(c(1, 2), fractional, k))
    nonstandard <- data.frame(source = c(0, 3), target = c(2, 7))
    expect_identical(.gls_pair_product(X, nonstandard, c(1, 2)),
        X %*% pair_coefficient_matrix(c(1, 2), nonstandard, k))
    factored <- data.frame(source = factor(c(1, 3)), target = c(2, 7))
    expect_identical(suppressWarnings(.gls_pair_product(X, factored, c(1, 2))),
        suppressWarnings(X %*% pair_coefficient_matrix(c(1, 2), factored, k)))
    expect_error(.gls_pair_product_cpp(X, 0L, 1L, 1, k), "indices")
    expect_error(.gls_pair_product_cpp(X, 1L, 1L, numeric(), k), "match")
})
