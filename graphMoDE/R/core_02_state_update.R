# Exact selected definitions from R/gmde-state-update.R
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

gmde_mu_from_eta <- function(N, eta) {
    log_mu <- log(N) + as.numeric(eta)
    mu <- rep(Inf, length(log_mu))
    finite <- log_mu <= log(.Machine$double.xmax)
    mu[finite] <- exp(log_mu[finite])
    mu
}
