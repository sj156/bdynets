# Exact selected definitions from R/graphmode4-diagnostics.R
graphmode4_validate_config <- function(record) {
    if (!is.list(record) || !identical(record$protocol, graphmode4_protocol))
        stop("Expected an r4 configuration record.", call. = FALSE)
    signature <- record$signature
    record$signature <- NULL
    if (!identical(signature, graphmode_digest(record))) stop("r4 configuration changed.", call. = FALSE)
    graphmode_revalidate_config(record$core)
    graphmode4_permutation(record$ids, 121L)
    invisible(TRUE)
}

graphmode4_trace <- function(draws, record) {
    graphmode4_validate_config(record)
    config <- record$core
    TT <- nrow(config$Fmat); p <- ncol(config$Fmat); K <- config$K; n <- config$n
    if (TT < 84L || !is.list(draws) || !length(draws))
        stop("Complete retained draws and at least 84 times are required.", call. = FALSE)
    ids <- c(1L, 31L, 61L, 91L, 121L)
    times <- unique(c(1L, 84L, TT))
    units <- match(ids, record$ids)
    points <- expand.grid(id = ids, time = times)
    free <- if (!config$adaptive || config$guidance %in% c("none", "forced")) 0L else
        if (config$guidance == "shared") 1L else K
    point_names <- paste0(if (config$family == "poisson") "unit_log_mean_" else "unit_mean_",
                          points$id, "_t", points$time)
    columns <- c("log_likelihood", "Kocc", paste0("proportion_", seq_len(K)),
        if (free) paste0("guidance_", seq_len(free)), point_names)
    trace <- matrix(NA_real_, length(draws), length(columns), dimnames = list(NULL, columns))
    PSM <- matrix(0, n, n)
    for (i in seq_along(draws)) {
        d <- draws[[i]]
        Z <- gmde_validate_labels(d$Z, n, K)
        if (!is.numeric(d$theta) || !identical(dim(d$theta), c(K, TT, p)) || any(!is.finite(d$theta)))
            stop("Invalid retained state paths.", call. = FALSE)
        if (config$dynamics == "static") for (k in seq_len(K))
            if (any(sweep(matrix(d$theta[k, , ], TT, p), 2L, d$theta[k, 1L, ], "-") != 0))
                stop("Static retained paths must be exactly constant.", call. = FALSE)
        eta <- gmde_eta(d$theta, config$Fmat)
        ll <- graphmode_response_loglik(config$Y, eta, config$family, d$sigma2)
        # Full conditional response likelihood, including data constants; same
        # definition for all five methods (not a Potts marginal likelihood).
        loglik <- sum(ll[cbind(seq_len(n), Z)]) - if (config$family == "poisson")
            sum(lgamma(config$Y + 1)) else n * TT / 2 * log(2 * pi)
        sizes <- tabulate(Z, K)
        guidance <- numeric()
        if (free) {
            if (!is.numeric(d$v) || length(d$v) != free || any(!is.finite(d$v)))
                stop("Missing free guidance draws.", call. = FALSE)
            guidance <- sort(stats::plogis(d$v))
        }
        profile <- eta[cbind(Z[match(points$id, record$ids)], points$time)]
        trace[i, ] <- c(loglik, sum(sizes > 0), sort(sizes / n), guidance, profile)
        PSM <- PSM + outer(Z, Z, "==")
    }
    if (any(!is.finite(trace))) stop("Nonfinite required scalar.", call. = FALSE)
    list(scalars = trace, similarity = PSM / length(draws),
        discrete = c("Kocc", paste0("proportion_", seq_len(K))),
        units = units, points = points)
}

graphmode4_rhats <- function(x) {
    # Small direct implementation of the two published rank-split statistics;
    # ESS is delegated to the installed posterior backend, without fallback.
    split_rank <- function(y) {
        half <- floor(nrow(y) / 2)
        y <- cbind(y[seq_len(half), , drop = FALSE],
                   y[seq.int(nrow(y) - half + 1L, nrow(y)), , drop = FALSE])
        ranks <- rank(y, ties.method = "average")
        z <- matrix(stats::qnorm((ranks - 3/8) / (length(ranks) + 1/4)), nrow(y), ncol(y))
        within <- mean(apply(z, 2L, stats::var))
        between <- nrow(z) * stats::var(colMeans(z))
        if (!is.finite(within) || within <= 0) return(NA_real_)
        sqrt((between / within + nrow(z) - 1) / nrow(z))
    }
    c(rank_rhat = split_rank(x), folded_rhat = split_rank(abs(x - stats::median(x))))
}

graphmode4_scalar_diagnostic <- function(x, discrete = FALSE) {
    graphmode_matrix(x, "four-chain scalar draws")
    if (ncol(x) != 4L || nrow(x) < 4L) stop("Need four chains and at least four retained draws.", call. = FALSE)
    constant <- vapply(seq_len(4L), function(j) all(x[, j] == x[1L, j]), logical(1))
    result <- list(status = "failed", rank_rhat = NA_real_, folded_rhat = NA_real_,
                   bulk_ess = NA_real_, tail_ess = NA_real_)
    if (all(constant)) {
        result$status <- if (all(x == x[1L, 1L]) && discrete) "uninformative-constant-discrete" else
            if (!all(x == x[1L, 1L])) "failed-different-chain-constants" else "failed-undefined-constant"
        return(result)
    }
    if (!requireNamespace("posterior", quietly = TRUE))
        stop("Installed 'posterior' backend is required; no automatic installation or substitute ESS.", call. = FALSE)
    rhats <- graphmode4_rhats(x)
    result$rank_rhat <- rhats[["rank_rhat"]]
    result$folded_rhat <- rhats[["folded_rhat"]]
    result$bulk_ess <- posterior::ess_bulk(x)
    result$tail_ess <- posterior::ess_tail(x)
    values <- unlist(result[-1L])
    if (all(is.finite(values)) && result$rank_rhat <= 1.01 && result$folded_rhat <= 1.01 &&
        result$bulk_ess >= 400 && result$tail_ess >= 400) result$status <- "passed"
    result
}

graphmode4_psm_rms <- function(a, b) {
    validate <- function(x) {
        graphmode_matrix(x, "PSM")
        if (nrow(x) != ncol(x) || nrow(x) < 2L || any(x < 0 | x > 1) || any(diag(x) != 1) ||
            !isTRUE(all.equal(x, t(x), tolerance = 1e-12))) stop("Invalid PSM.", call. = FALSE)
    }
    validate(a); validate(b)
    if (!identical(dim(a), dim(b))) stop("PSM dimensions differ.", call. = FALSE)
    sqrt(mean((a[upper.tri(a)] - b[upper.tri(b)])^2))
}
