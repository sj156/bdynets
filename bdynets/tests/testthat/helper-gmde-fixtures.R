gmde_test_path_graph <- function(n = 6L, tau = 1) {
  W <- matrix(0, n, n)
  if (n > 1L) for (i in seq_len(n - 1L)) W[i, i + 1L] <- W[i + 1L, i] <- 1
  gmde_graph(W, paste0("u", seq_len(n)), tau = tau)
}

gmde_test_small_expert <- function() {
  gmde_expert("gaussian", cbind(1, c(-1, -0.3, 0.3, 1)), prior = list(
    m0 = c(0.2, -0.1), C0 = matrix(c(2, 0.3, 0.3, 1), 2),
    variance = list(shape = 3, scale = 2)))
}

gmde_test_small_panel <- function(n = 6L) {
  Y <- matrix(sin(seq_len(n * 4L)), n, 4L)
  rownames(Y) <- paste0("u", seq_len(n))
  Y
}
