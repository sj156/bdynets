# Work in prior-white coordinates: beta = m0 + root %*% z.
# This is the exact static precision update, without forming C0^-1 or jitter.
.gmde_gaussian_conditional <- function(expert, N, S, sigma2) {
  B <- expert$design
  A <- expert$white_design
  p <- ncol(B)
  weighted_A <- sqrt(N / sigma2) * A
  precision <- diag(p) + crossprod(weighted_A)
  R <- .gmde_chol(precision, "Static posterior precision")
  h <- crossprod(A, (S - N * as.vector(B %*% expert$prior$m0)) / sigma2)
  white_mean <- backsolve(R, forwardsolve(t(R), h))
  mean <- as.vector(expert$prior$m0 + expert$root %*% white_mean)
  if (any(!is.finite(mean))) stop("Static posterior mean overflow.")
  list(mean = mean, precision_chol = R, root = expert$root)
}

.gmde_gaussian_draw <- function(conditional) {
  as.vector(conditional$mean + conditional$root %*%
              backsolve(conditional$precision_chol,
                        stats::rnorm(length(conditional$mean))))
}

.gmde_variance_draw <- function(shape, scale) {
  value <- 1 / stats::rgamma(1L, shape = shape, rate = scale)
  if (!is.finite(value) || value <= 0)
    stop("Inverse-gamma draw is not representable; no clipping was applied.")
  value
}

.gmde_expert_update <- function(Y, Z, sufficient, sigma2, expert, K) {
  beta <- matrix(0, K, ncol(expert$design))
  for (k in seq_len(K)) {
    N <- sufficient$N[k]
    if (N == 0L) {
      beta[k, ] <- expert$prior$m0 + expert$root %*% stats::rnorm(ncol(beta))
      sigma2[k] <- .gmde_variance_draw(expert$prior$variance$shape,
                                  expert$prior$variance$scale)
    } else {
      beta[k, ] <- .gmde_gaussian_draw(.gmde_gaussian_conditional(
        expert, N, sufficient$S[k, ], sigma2[k]))
      residual <- sweep(Y[Z == k, , drop = FALSE], 2L,
                        as.vector(expert$design %*% beta[k, ]), "-")
      sigma2[k] <- .gmde_variance_draw(expert$prior$variance$shape + N * ncol(Y) / 2,
                                  expert$prior$variance$scale + sum(residual^2) / 2)
    }
  }
  list(beta = beta, sigma2 = sigma2, eta = beta %*% t(expert$design))
}
