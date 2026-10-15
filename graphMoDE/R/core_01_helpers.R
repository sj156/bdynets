# Exact selected definitions from R/gmde-helpers.R
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
