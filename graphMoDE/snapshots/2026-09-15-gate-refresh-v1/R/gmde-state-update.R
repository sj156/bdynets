# Metropolis-corrected negative-binomial--Polya--Gamma state updates.

gmde_draw_pg <- function(
    b,
    z,
    backend = c("devroye-exact", "truncated"),
    trunc = 80L,
    draw_function = NULL
) {
    b <- as.numeric(b)
    z <- as.numeric(z)
    if (length(b) != length(z) || any(!is.finite(b)) || any(b <= 0) ||
        any(!is.finite(z))) {
        stop("Polya--Gamma parameters must be finite with b > 0.",
             call. = FALSE)
    }

    if (!is.null(draw_function)) {
        omega <- as.numeric(draw_function(b, z))
    } else {
        backend <- match.arg(backend)
        if (backend == "devroye-exact") {
            if (any(b != round(b)) ||
                any(b > .Machine$integer.max)) {
                stop(
                    "The exact Devroye PG backend requires positive integer ",
                    "shape b. With r[k,t] = rho * max(S[k,t], 1), use a ",
                    "positive integer rho for exact runs.",
                    call. = FALSE
                )
            }
            if (!requireNamespace("BayesLogit", quietly = TRUE)) {
                stop(
                    "The exact Devroye sampler requires package 'BayesLogit'. ",
                    "Use backend='truncated' only for an explicitly labelled ",
                    "non-inferential sensitivity or interface check.",
                    call. = FALSE
                )
            }
            omega <- as.numeric(BayesLogit::rpg.devroye(
                length(b), as.integer(round(b)), z
            ))
        } else {
            trunc <- gmde_scalar_integer(trunc, "trunc", lower = 20L)
            indices <- seq_len(trunc)
            omega <- numeric(length(b))
            for (i in seq_along(b)) {
                denominator <-
                    (indices - 0.5)^2 + z[i]^2 / (4 * pi^2)
                gamma_draws <- stats::rgamma(trunc, shape = b[i], rate = 1)
                omega[i] <- sum(gamma_draws / denominator) / (2 * pi^2)
            }
        }
    }

    if (length(omega) != length(b) || any(!is.finite(omega)) ||
        any(omega <= 0)) {
        stop(
            "The Polya--Gamma backend returned a non-positive or non-finite ",
            "draw; the exact kernel fails closed and does not clip omega.",
            call. = FALSE
        )
    }
    omega
}

#' Information-form forward filtering
#'
#' @param omega Positive Polya--Gamma precision vector.
#' @param zeta Natural-parameter vector.
#' @param Fmat Time-by-state design matrix.
#' @param m0,C0 Initial-state moments.
#' @param G,W State evolution matrix and covariance.
#' @return Stored predictive and filtered moments for backward sampling.
#' @export
gmde_information_filter <- function(omega, zeta, Fmat, m0, C0, G, W) {
    omega <- as.numeric(omega)
    zeta <- as.numeric(zeta)
    Fmat <- as.matrix(Fmat)
    m0 <- as.numeric(m0)
    C0 <- as.matrix(C0)
    G <- as.matrix(G)
    W <- as.matrix(W)
    TT <- nrow(Fmat)
    p <- ncol(Fmat)

    if (length(omega) != TT || length(zeta) != TT ||
        any(!is.finite(omega)) || any(omega <= 0) ||
        any(!is.finite(zeta))) {
        stop("omega and zeta must be finite length-T vectors with omega > 0.",
             call. = FALSE)
    }
    if (p < 1L || length(m0) != p || !identical(dim(C0), c(p, p)) ||
        !identical(dim(G), c(p, p)) || !identical(dim(W), c(p, p)) ||
        any(!is.finite(c(Fmat, m0, C0, G, W)))) {
        stop("The DLM inputs have incompatible dimensions or values.",
             call. = FALSE)
    }
    gmde_chol_spd(C0, "C0")
    gmde_chol_spd(W, "W")

    a_pred <- matrix(NA_real_, nrow = TT, ncol = p)
    m_filt <- matrix(NA_real_, nrow = TT, ncol = p)
    R_pred <- array(NA_real_, dim = c(p, p, TT))
    C_filt <- array(NA_real_, dim = c(p, p, TT))

    m_previous <- m0
    C_previous <- C0
    for (tt in seq_len(TT)) {
        Ft <- as.numeric(Fmat[tt, ])
        a <- as.numeric(G %*% m_previous)
        R <- G %*% C_previous %*% t(G) + W
        R <- (R + t(R)) / 2
        gmde_chol_spd(R, paste0("R[", tt, "]"))

        RF <- as.numeric(R %*% Ft)
        v <- sum(Ft * RF)
        s <- 1 + omega[tt] * v
        if (!is.finite(s) || s <= 0) {
            stop("The information-filter denominator is not positive and finite.",
                 call. = FALSE)
        }
        innovation_information <- zeta[tt] - omega[tt] * sum(Ft * a)
        m <- a + RF * innovation_information / s
        C <- R - (omega[tt] / s) * tcrossprod(RF)
        C <- (C + t(C)) / 2
        gmde_chol_spd(C, paste0("C[", tt, "]"))

        a_pred[tt, ] <- a
        R_pred[, , tt] <- R
        m_filt[tt, ] <- m
        C_filt[, , tt] <- C
        m_previous <- m
        C_previous <- C
    }

    list(a = a_pred, R = R_pred, m = m_filt, C = C_filt)
}

#' Information-form forward-filtering backward-sampling
#'
#' The forward pass stores Gaussian filtering moments.  The backward pass
#' samples one temporally coherent state trajectory and uses Cholesky solves
#' rather than explicit matrix inverses.
#'
#' @inheritParams gmde_information_filter
#' @return A list containing the sampled path and filtering moments.
#' @export
gmde_information_ffbs <- function(omega, zeta, Fmat, m0, C0, G, W) {
    moments <- gmde_information_filter(
        omega = omega,
        zeta = zeta,
        Fmat = Fmat,
        m0 = m0,
        C0 = C0,
        G = G,
        W = W
    )
    TT <- length(omega)
    p <- length(m0)
    G <- as.matrix(G)
    theta <- matrix(NA_real_, nrow = TT, ncol = p)
    theta[TT, ] <- gmde_rmvn(
        moments$m[TT, ], moments$C[, , TT], paste0("C[", TT, "]")
    )

    if (TT > 1L) {
        for (tt in seq.int(TT - 1L, 1L)) {
            B <- gmde_right_solve_spd(
                moments$C[, , tt] %*% t(G),
                moments$R[, , tt + 1L],
                paste0("R[", tt + 1L, "]")
            )
            h <- moments$m[tt, ] + as.numeric(
                B %*% (theta[tt + 1L, ] - moments$a[tt + 1L, ])
            )
            Hcov <- moments$C[, , tt] -
                B %*% moments$R[, , tt + 1L] %*% t(B)
            Hcov <- (Hcov + t(Hcov)) / 2
            theta[tt, ] <- gmde_rmvn(
                h, Hcov, paste0("backward covariance[", tt, "]")
            )
        }
    }
    list(theta = theta, moments = moments)
}

gmde_mu_from_eta <- function(N, eta) {
    log_mu <- log(N) + as.numeric(eta)
    mu <- rep(Inf, length(log_mu))
    finite <- log_mu <= log(.Machine$double.xmax)
    mu[finite] <- exp(log_mu[finite])
    mu
}

#' One exact-Poisson state-path update for a nonempty expert
#'
#' @param current Current `T` by `p` state path.
#' @param N Expert size, strictly positive.
#' @param S Aggregated expert counts.
#' @param rho Positive structured NB multiplier.
#' @param pg_backend Exact integer-shape `devroye-exact` or approximate
#'   `truncated` backend.
#' @param pg_trunc Truncation used only by the approximate backend.
#' @param pg_draw Optional injected PG draw function for focused tests.
#' @inheritParams gmde_information_filter
#' @return Updated path and path-level MH diagnostics.
#' @export
gmde_update_state_path <- function(
    current,
    N,
    S,
    rho,
    Fmat,
    m0,
    C0,
    G,
    W,
    pg_backend = c("devroye-exact", "truncated"),
    pg_trunc = 80L,
    pg_draw = NULL
) {
    current <- as.matrix(current)
    Fmat <- as.matrix(Fmat)
    S <- as.numeric(S)
    N <- gmde_scalar_integer(N, "N", lower = 1L)
    if (!identical(dim(current), dim(Fmat)) || length(S) != nrow(Fmat) ||
        any(!is.finite(current))) {
        stop("current, S, and Fmat have incompatible dimensions or values.",
             call. = FALSE)
    }
    pg_backend <- match.arg(pg_backend)
    if (identical(pg_backend, "devroye-exact") && any(S != round(S))) {
        stop(
            "Exact Devroye PG sampling requires integer aggregated counts S.",
            call. = FALSE
        )
    }
    started <- proc.time()[[3L]]

    r <- gmde_make_nb_r(S, rho)
    eta_current <- rowSums(Fmat * current)
    mu_current <- gmde_mu_from_eta(N, eta_current)
    if (any(!is.finite(mu_current))) {
        stop("The current state path gives a non-finite Poisson mean.",
             call. = FALSE)
    }

    psi <- eta_current + log(N) - log(r)
    b <- S + r
    kappa <- (S - r) / 2
    pg_started <- proc.time()[[3L]]
    omega <- gmde_draw_pg(
        b = b,
        z = psi,
        backend = pg_backend,
        trunc = pg_trunc,
        draw_function = pg_draw
    )
    pg_elapsed <- max(
        proc.time()[[3L]] - pg_started, .Machine$double.eps
    )
    zeta <- kappa + omega * (log(r) - log(N))

    proposal <- gmde_information_ffbs(
        omega = omega,
        zeta = zeta,
        Fmat = Fmat,
        m0 = m0,
        C0 = C0,
        G = G,
        W = W
    )$theta
    eta_proposal <- rowSums(Fmat * proposal)
    mu_proposal <- gmde_mu_from_eta(N, eta_proposal)

    current_ratio <- gmde_poisson_nb_log_ratio(S, mu_current, r)
    proposal_ratio <- gmde_poisson_nb_log_ratio(S, mu_proposal, r)
    log_ratio <- sum(proposal_ratio - current_ratio)
    if (is.nan(log_ratio)) {
        stop("The Poisson/NB Metropolis log ratio is undefined.",
             call. = FALSE)
    }
    log_acceptance <- min(0, log_ratio)
    accepted <- is.finite(log_acceptance) &&
        log(stats::runif(1)) <= log_acceptance
    updated <- if (accepted) proposal else current
    movement <- if (accepted) {
        sum(pmax(S, 1) * (eta_proposal - eta_current)^2)
    } else 0

    list(
        theta = updated,
        proposal = proposal,
        accepted = accepted,
        log_acceptance = log_acceptance,
        accepted_movement = movement,
        rho = rho,
        r = r,
        pg_shape = b,
        pg_shape_sum = sum(b),
        pg_shape_max = max(b),
        pg_elapsed_seconds = pg_elapsed,
        elapsed_seconds = max(proc.time()[[3L]] - started, .Machine$double.eps),
        pg_backend = pg_backend,
        exact_pg = identical(pg_backend, "devroye-exact") && is.null(pg_draw)
    )
}

#' Update every expert state path
#'
#' Empty experts are drawn from their DLM prior and never evaluate `log(N)`.
#'
#' @param Y Location-by-time count matrix.
#' @param Z Allocation vector.
#' @param theta Current `K` by `T` by `p` state array.
#' @param K Number of experts.
#' @inheritParams gmde_update_state_path
#' @return Updated state array, sufficient statistics, and diagnostics.
#' @export
gmde_update_all_state_paths <- function(
    Y,
    Z,
    theta,
    Fmat,
    m0,
    C0,
    G,
    W,
    rho,
    K = dim(theta)[1L],
    pg_backend = c("devroye-exact", "truncated"),
    pg_trunc = 80L,
    pg_draw = NULL
) {
    pg_backend <- match.arg(pg_backend)
    Y <- as.matrix(Y)
    Fmat <- as.matrix(Fmat)
    theta <- as.array(theta)
    K <- gmde_scalar_integer(K, "K", lower = 1L)
    TT <- ncol(Y)
    p <- ncol(Fmat)
    if (!identical(dim(theta), c(K, TT, p))) {
        stop("theta must have dimension K by T by p.", call. = FALSE)
    }
    stats <- gmde_cluster_stats(Z, Y, K)
    updated <- theta
    accepted <- rep(NA, K)
    log_acceptance <- rep(NA_real_, K)
    movement <- numeric(K)
    elapsed <- numeric(K)
    pg_elapsed <- numeric(K)
    pg_shape_sum <- rep(NA_real_, K)
    pg_shape_max <- rep(NA_real_, K)
    r_values <- vector("list", K)

    for (k in seq_len(K)) {
        if (stats$N[k] == 0L) {
            started <- proc.time()[[3L]]
            updated[k, , ] <- gmde_sample_prior_path(TT, m0, C0, G, W)
            elapsed[k] <- max(
                proc.time()[[3L]] - started, .Machine$double.eps
            )
            next
        }
        result <- gmde_update_state_path(
            current = matrix(theta[k, , ], nrow = TT, ncol = p),
            N = stats$N[k],
            S = stats$S[k, ],
            rho = rho,
            Fmat = Fmat,
            m0 = m0,
            C0 = C0,
            G = G,
            W = W,
            pg_backend = pg_backend,
            pg_trunc = pg_trunc,
            pg_draw = pg_draw
        )
        updated[k, , ] <- result$theta
        accepted[k] <- result$accepted
        log_acceptance[k] <- result$log_acceptance
        movement[k] <- result$accepted_movement
        elapsed[k] <- result$elapsed_seconds
        pg_elapsed[k] <- result$pg_elapsed_seconds
        pg_shape_sum[k] <- result$pg_shape_sum
        pg_shape_max[k] <- result$pg_shape_max
        r_values[[k]] <- result$r
    }

    list(
        theta = updated,
        stats = stats,
        accepted = accepted,
        log_acceptance = log_acceptance,
        accepted_movement = movement,
        elapsed_seconds = elapsed,
        pg_elapsed_seconds = pg_elapsed,
        pg_shape_sum = pg_shape_sum,
        pg_shape_max = pg_shape_max,
        rho = rho,
        r = r_values,
        pg_backend = pg_backend,
        target_exact = identical(pg_backend, "devroye-exact") && is.null(pg_draw)
    )
}
