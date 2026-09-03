# Core numerical helpers for the manuscript-matched GMDE sampler.

# The version string is deliberately different from the frozen 2026-08-22
# benchmark.  Checkpoints and results from that release are not compatible
# with this implementation.
countdlm_gmde_sampler_version <-
    "exact-poisson-info-ffbs-joint-ess-devroye-2026-08-31-v2"

gmde_scalar_integer <- function(
    x,
    name,
    lower = 0L,
    upper = .Machine$integer.max
) {
    if (length(x) != 1L || !is.numeric(x) || !is.finite(x) ||
        x != round(x) || x < lower || x > upper) {
        stop(
            name, " must be one whole number in [", lower, ", ", upper,
            "].", call. = FALSE
        )
    }
    as.integer(x)
}

gmde_validate_labels <- function(labels, n, K, name = "labels") {
    if (!is.numeric(labels) || length(labels) != n ||
        any(!is.finite(labels)) || any(labels != round(labels)) ||
        any(labels < 1) || any(labels > K)) {
        stop(
            name, " must contain one whole-number label in 1, ..., K ",
            "per observation.", call. = FALSE
        )
    }
    as.integer(labels)
}

#' Strict Cholesky factorization of a symmetric positive-definite matrix
#'
#' This helper fails closed.  It does not add an unrecorded diagonal jitter,
#' because doing so inside the NB surrogate transition would change the
#' proposal kernel used by the Metropolis correction.
#'
#' @param x Square numeric matrix.
#' @param name Name used in error messages.
#' @return The upper-triangular Cholesky factor.
#' @export
gmde_chol_spd <- function(x, name = "matrix") {
    x <- as.matrix(x)
    if (length(dim(x)) != 2L || nrow(x) != ncol(x) ||
        any(!is.finite(x))) {
        stop(name, " must be a finite square matrix.", call. = FALSE)
    }
    asymmetry <- max(abs(x - t(x)))
    scale <- max(1, max(abs(x)))
    if (asymmetry > 1e-10 * scale) {
        stop(name, " is not numerically symmetric.", call. = FALSE)
    }
    tryCatch(
        chol((x + t(x)) / 2),
        error = function(e) {
            stop(name, " is not positive definite: ", conditionMessage(e),
                 call. = FALSE)
        }
    )
}

#' Solve a symmetric positive-definite linear system by Cholesky factors
#'
#' @param a Symmetric positive-definite coefficient matrix.
#' @param b Right-hand side vector or matrix.
#' @param name Name used in error messages.
#' @return Solution of `a %*% x = b`.
#' @export
gmde_solve_spd <- function(a, b, name = "matrix") {
    factor <- gmde_chol_spd(a, name = name)
    backsolve(factor, forwardsolve(t(factor), b))
}

gmde_right_solve_spd <- function(b, a, name = "matrix") {
    t(gmde_solve_spd(a, t(b), name = name))
}

#' Draw from a multivariate normal distribution using a strict covariance
#'
#' @param mean Mean vector.
#' @param covariance Positive-definite covariance matrix.
#' @param name Name used in error messages.
#' @return One numeric draw.
#' @export
gmde_rmvn <- function(mean, covariance, name = "covariance") {
    mean <- as.numeric(mean)
    if (!length(mean) || any(!is.finite(mean))) {
        stop("mean must be a nonempty finite vector.", call. = FALSE)
    }
    factor <- gmde_chol_spd(covariance, name = name)
    if (nrow(factor) != length(mean)) {
        stop("Mean and covariance dimensions do not agree.", call. = FALSE)
    }
    as.numeric(mean + t(factor) %*% stats::rnorm(length(mean)))
}

#' Draw a complete trajectory from a Gaussian DLM prior
#'
#' @param TT Number of time points.
#' @param m0,C0 Initial-state moments.
#' @param G,W State evolution matrix and covariance.
#' @return A `TT` by `p` state matrix.
#' @export
gmde_sample_prior_path <- function(TT, m0, C0, G, W) {
    TT <- gmde_scalar_integer(TT, "TT", lower = 1L)
    m0 <- as.numeric(m0)
    p <- length(m0)
    if (!identical(dim(as.matrix(C0)), c(p, p)) ||
        !identical(dim(as.matrix(G)), c(p, p)) ||
        !identical(dim(as.matrix(W)), c(p, p)) ||
        any(!is.finite(c(m0, C0, G, W)))) {
        stop("C0, G, and W must be p by p matrices.", call. = FALSE)
    }
    gmde_chol_spd(C0, "C0")
    gmde_chol_spd(W, "W")

    path <- matrix(NA_real_, nrow = TT, ncol = p)
    previous <- gmde_rmvn(m0, C0, "C0")
    for (tt in seq_len(TT)) {
        previous <- gmde_rmvn(
            as.numeric(G %*% previous),
            W,
            "W"
        )
        path[tt, ] <- previous
    }
    path
}

#' Aggregate allocation-specific count sufficient statistics
#'
#' @param Z Integer allocation vector.
#' @param Y Location-by-time count matrix.
#' @param K Number of experts.
#' @return A list containing expert sizes `N` and count sums `S`.
#' @export
gmde_cluster_stats <- function(Z, Y, K) {
    Y <- as.matrix(Y)
    K <- gmde_scalar_integer(K, "K", lower = 1L)
    Z <- gmde_validate_labels(Z, nrow(Y), K, "Z")
    if (nrow(Y) < 1L || ncol(Y) < 1L || any(!is.finite(Y)) ||
        any(Y < 0) || any(Y != round(Y)) ||
        length(Z) != nrow(Y)) {
        stop("Y must be a nonempty finite integer count matrix.",
             call. = FALSE)
    }
    N <- tabulate(Z, nbins = K)
    S <- matrix(0, nrow = K, ncol = ncol(Y))
    for (k in seq_len(K)) {
        if (N[k] > 0L) {
            S[k, ] <- colSums(Y[Z == k, , drop = FALSE])
        }
    }
    list(N = as.integer(N), S = S)
}

#' Structured negative-binomial approximation scale
#'
#' @param S Aggregated counts for one expert.
#' @param rho Positive common scale multiplier.
#' @return `rho * pmax(S, 1)`.
#' @export
gmde_make_nb_r <- function(S, rho) {
    S <- as.numeric(S)
    if (any(!is.finite(S)) || any(S < 0)) {
        stop("S must contain finite nonnegative counts.", call. = FALSE)
    }
    if (length(rho) != 1L || !is.finite(rho) || rho <= 0) {
        stop("rho must be one finite positive number.", call. = FALSE)
    }
    r <- rho * pmax(S, 1)
    if (any(!is.finite(r)) || any(r <= 0)) {
        stop("rho * pmax(S, 1) must remain finite and positive.",
             call. = FALSE)
    }
    r
}

#' Poisson-to-negative-binomial log-ratio contribution
#'
#' @param S Aggregated count vector.
#' @param mu Poisson mean vector.
#' @param r Negative-binomial size vector.
#' @return Vector of state-dependent log-ratio contributions.
#' @export
gmde_poisson_nb_log_ratio <- function(S, mu, r) {
    S <- as.numeric(S)
    mu <- as.numeric(mu)
    r <- as.numeric(r)
    if (!identical(length(S), length(mu)) ||
        !identical(length(S), length(r)) ||
        any(!is.finite(S)) || any(S < 0) ||
        any(is.na(mu)) || any(mu < 0) ||
        any(!is.finite(r)) || any(r <= 0)) {
        stop("S, mu, and r have incompatible or invalid values.",
             call. = FALSE)
    }
    out <- rep(-Inf, length(mu))
    finite <- is.finite(mu)
    mu_finite <- mu[finite]
    r_finite <- r[finite]
    S_finite <- S[finite]
    log_x <- log(mu_finite) - log(r_finite)
    large <- log_x > 30
    if (any(large)) {
        log_one_plus_x <- log_x[large] + log1p(exp(-log_x[large]))
        out[which(finite)[large]] <- -mu_finite[large] +
            (r_finite[large] + S_finite[large]) * log_one_plus_x
    }

    regular <- !large
    x <- mu_finite[regular] / r_finite[regular]
    log1pmx <- log1p(x) - x
    small <- x < 1e-4
    if (any(small)) {
        xs <- x[small]
        log1pmx[small] <-
            -xs^2 / 2 + xs^3 / 3 - xs^4 / 4 +
            xs^5 / 5 - xs^6 / 6
    }
    out[which(finite)[regular]] <-
        r_finite[regular] * log1pmx + S_finite[regular] * log1p(x)
    out
}

gmde_eta <- function(theta, Fmat) {
    theta <- as.array(theta)
    Fmat <- as.matrix(Fmat)
    if (ncol(Fmat) < 1L || length(dim(theta)) != 3L ||
        dim(theta)[2L] != nrow(Fmat) ||
        dim(theta)[3L] != ncol(Fmat)) {
        stop("theta must be K by T by p and match Fmat.", call. = FALSE)
    }
    K <- dim(theta)[1L]
    TT <- dim(theta)[2L]
    eta <- matrix(NA_real_, nrow = K, ncol = TT)
    for (k in seq_len(K)) {
        eta[k, ] <- rowSums(Fmat * matrix(
            theta[k, , ], nrow = TT, ncol = ncol(Fmat)
        ))
    }
    eta
}

#' Stable row-wise softmax
#'
#' @param x Numeric matrix of log weights.
#' @return Matrix of row-normalized probabilities.
#' @export
gmde_softmax_rows <- function(x) {
    x <- as.matrix(x)
    if (nrow(x) < 1L || ncol(x) < 1L || any(!is.finite(x))) {
        stop("Softmax input must be a nonempty finite matrix.", call. = FALSE)
    }
    row_max <- apply(x, 1L, max)
    shifted <- exp(x - row_max)
    shifted / rowSums(shifted)
}

#' Sample independent categorical rows
#'
#' @param probability Matrix with nonnegative rows summing to one.
#' @return Integer category labels.
#' @export
gmde_sample_categorical_rows <- function(probability) {
    probability <- as.matrix(probability)
    if (any(!is.finite(probability)) || any(probability < 0)) {
        stop("Categorical probabilities must be finite and nonnegative.",
             call. = FALSE)
    }
    totals <- rowSums(probability)
    if (any(totals <= 0)) {
        stop("Every categorical row must have positive mass.", call. = FALSE)
    }
    probability <- probability / totals
    vapply(
        seq_len(nrow(probability)),
        function(i) sample.int(ncol(probability), 1L, prob = probability[i, ]),
        integer(1)
    )
}

#' Orthonormal Helmert contrast matrix
#'
#' @param K Number of experts.
#' @return A `K` by `K - 1` orthonormal contrast matrix.
#' @export
gmde_helmert_contrast <- function(K) {
    K <- gmde_scalar_integer(K, "K", lower = 2L)
    H <- stats::contr.helmert(K)
    H <- sweep(H, 2L, sqrt(colSums(H^2)), "/")
    if (max(abs(crossprod(H) - diag(K - 1L))) > 1e-12 ||
        max(abs(colSums(H))) > 1e-12) {
        stop("Failed to construct an orthonormal Helmert basis.",
             call. = FALSE)
    }
    H
}

gmde_rdirichlet1 <- function(shape) {
    shape <- as.numeric(shape)
    if (any(!is.finite(shape)) || any(shape <= 0)) {
        stop("Dirichlet shapes must be finite and positive.", call. = FALSE)
    }
    draw <- stats::rgamma(length(shape), shape = shape, rate = 1)
    draw / sum(draw)
}

gmde_initialize_allocations <- function(Y, K) {
    Y <- as.matrix(Y)
    K <- gmde_scalar_integer(K, "K", lower = 1L, upper = nrow(Y))
    if (nrow(Y) < K) {
        stop("K cannot exceed the number of locations.", call. = FALSE)
    }
    if (K == nrow(Y)) return(seq_len(K))
    features <- cbind(
        rowMeans(Y),
        apply(Y, 1L, stats::sd),
        Y[, 1L],
        Y[, ncol(Y)]
    )
    features[!is.finite(features)] <- 0
    if (nrow(unique(features)) < K) {
        order_index <- order(rowMeans(Y), seq_len(nrow(Y)))
        labels <- integer(nrow(Y))
        labels[order_index] <- rep(seq_len(K), length.out = nrow(Y))
        return(labels)
    }
    as.integer(stats::kmeans(features, centers = K, nstart = 20L)$cluster)
}

gmde_adjusted_rand <- function(truth, estimate) {
    table_value <- table(as.integer(truth), as.integer(estimate))
    choose2 <- function(x) x * (x - 1) / 2
    a <- sum(choose2(rowSums(table_value)))
    b <- sum(choose2(colSums(table_value)))
    index <- sum(choose2(table_value))
    total <- choose2(sum(table_value))
    if (total == 0) return(1)
    expected <- a * b / total
    maximum <- (a + b) / 2
    if (maximum == expected) return(1)
    (index - expected) / (maximum - expected)
}

gmde_best_label_accuracy <- function(truth, estimate, K = NULL) {
    truth <- as.integer(factor(truth))
    estimate <- as.integer(factor(estimate))
    if (is.null(K)) K <- max(c(truth, estimate))
    K <- gmde_scalar_integer(K, "K", lower = 1L, upper = 15L)
    contingency <- matrix(0, nrow = K, ncol = K)
    observed <- table(truth, estimate)
    contingency[seq_len(nrow(observed)), seq_len(ncol(observed))] <- observed

    states <- 2^K
    score <- rep(-Inf, states)
    score[1L] <- 0
    for (row in seq_len(K)) {
        next_score <- rep(-Inf, states)
        for (mask in 0:(states - 1L)) {
            if (!is.finite(score[mask + 1L])) next
            for (column in seq_len(K)) {
                bit <- bitwShiftL(1L, column - 1L)
                if (bitwAnd(mask, bit) == 0L) {
                    new_mask <- bitwOr(mask, bit)
                    candidate <- score[mask + 1L] + contingency[row, column]
                    next_score[new_mask + 1L] <- max(
                        next_score[new_mask + 1L], candidate
                    )
                }
            }
        }
        score <- next_score
    }
    score[states] / length(truth)
}
