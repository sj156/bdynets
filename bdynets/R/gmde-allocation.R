.gmde_log_softmax <- function(x) {
  # Subtract first; F_selected - (max + log(sum)) loses precision for large F.
  shifted <- x - apply(x, 1L, max)
  out <- shifted - log(rowSums(exp(shifted)))
  if (anyNA(out) || any(rowSums(is.finite(out)) == 0))
    stop("Allocation probabilities are numerically undefined.")
  out
}

.gmde_allocation_loglik <- function(F, Z) {
  sum(.gmde_log_softmax(F)[cbind(seq_len(nrow(F)), Z)])
}

.gmde_categorical <- function(probability) {
  vapply(seq_len(nrow(probability)), function(i)
    sample.int(ncol(probability), 1L, prob = probability[i, ]), integer(1))
}

.gmde_gaussian_loglik <- function(Y, eta, sigma2) {
  out <- matrix(0, nrow(Y), nrow(eta))
  for (k in seq_len(nrow(eta))) {
    residual <- sweep(Y, 2L, eta[k, ], "-")
    out[, k] <- -ncol(Y) / 2 * log(2 * pi * sigma2[k]) -
      rowSums(residual^2) / (2 * sigma2[k])
  }
  if (any(!is.finite(out))) stop("Gaussian log likelihood overflow.")
  out
}

.gmde_sufficient <- function(Y, Z, K) {
  sums <- matrix(0, K, ncol(Y))
  for (k in seq_len(K)) sums[k, ] <- colSums(Y[Z == k, , drop = FALSE])
  list(N = tabulate(Z, nbins = K), S = sums)
}
