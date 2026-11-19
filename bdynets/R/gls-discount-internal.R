# Gaussian reference covariance and static-plus-dynamic network helpers.
# The public package currently supports rank one.
dynamic_scale <- function(cfg) if (isTRUE(cfg$static_network)) cfg$dynamic_loading_scale else 1

static_network <- function(X, pairs, B, d = NULL) {
    if (is.null(B)) return(matrix(0, nrow(X), ncol(X)))
    if (!is.null(d)) B <- B * d
    unname(.gls_pair_product(X, pairs, B))
}

.gls_reference_step_R <- function(y, h, offset, m, C, discount) {
    R <- C / discount
    # The scalar objective is strictly convex. Bracket its score to avoid
    # Newton stagnation when the likelihood is nearly flat at machine precision.
    score <- function(b) {
        z <- offset + h * b
        scale <- max(0, z)
        ((b - m) / R - sum(h * y)) * exp(-scale) + sum(h * exp(z - scale))
    }
    width <- max(1, sqrt(R))
    lower <- m - width; upper <- m + width
    for (i in seq_len(100L)) {
        if (score(lower) <= 0 && score(upper) >= 0) break
        width <- width * 2
        lower <- m - width; upper <- m + width
    }
    if (score(lower) > 0 || score(upper) < 0) stop("Could not bracket reference-filter mode.")
    b <- stats::uniroot(score, c(lower, upper), tol = 1e-10)$root
    z <- offset + h * b
    precision <- 1 / R + sum(h^2 * exp(z))
    if (!is.finite(precision) || precision <= 0) stop("Non-finite reference-filter precision.")
    list(m = b, C = 1 / precision)
}

reference_step <- function(y, h, offset, m, C, discount, backend = "native") {
    if (backend == "R") return(.gls_reference_step_R(y, h, offset, m, C, discount))
    ans <- .gls_reference_path_native(matrix(as.double(y), 1L),
        matrix(as.double(h), 1L), matrix(as.double(offset), 1L),
        as.double(discount), as.double(c(m, C)), FALSE)
    ans[c("m", "C")]
}

reference_path <- function(Y, H, offset, discount, cfg, keep_mean = FALSE) {
    if (cfg$factor_evolution != "unit" && !identical(cfg$reference_backend, "R"))
        return(.gls_reference_path_native(Y, H, offset, as.double(discount),
            c(0, 1), keep_mean))
    .gls_reference_path_R(Y, H, offset, discount, cfg, keep_mean)
}

.gls_reference_path_R <- function(Y, H, offset, discount, cfg, keep_mean = FALSE) {
    TT <- nrow(Y)
    if (cfg$factor_evolution == "unit") {
        out <- list(W = rep(1, TT), m = 0, C = 1)
        if (keep_mean) out$mean_path <- rep(0, TT)
        return(out)
    }
    m <- 0; C <- 1; W <- numeric(TT)
    if (keep_mean) mean_path <- numeric(TT)
    for (tt in seq_len(TT)) {
        W[tt] <- (1 / discount - 1) * C
        z <- .gls_reference_step_R(Y[tt, ], as.numeric(H[tt, , ]), offset[tt, ], m, C, discount)
        m <- z$m; C <- z$C
        if (keep_mean) mean_path[tt] <- m
    }
    out <- list(W = W, m = m, C = C)
    if (keep_mean) out$mean_path <- mean_path
    out
}

factor_log_density <- function(beta, ref) {
    b0 <- attr(beta, "initial")
    if (is.null(b0)) stop("Factor path must include its initial draw.")
    sum(dnorm(diff(c(b0, as.numeric(beta))), sd = sqrt(ref$W), log = TRUE))
}

update_discount <- function(Y, H, offset, beta, cfg, current_discount = NULL,
    current_ref = NULL) {
    if (cfg$factor_evolution == "unit") {
        if (is.null(current_ref)) current_ref <- reference_path(Y, H, offset, NA_real_, cfg)
        return(list(discount = NA_real_, ref = current_ref))
    }
    # Reuse only the path for these same inputs and the current discount.
    # The conditional weights still use the current factor draw.
    paths <- lapply(cfg$discount_grid, function(d) {
        if (!is.null(current_ref) && isTRUE(d == current_discount)) current_ref
        else reference_path(Y, H, offset, d, cfg)
    })
    logp <- vapply(seq_along(paths), function(j) if (is.null(paths[[j]])) -Inf else
        log(cfg$discount_prior[j]) + factor_log_density(beta, paths[[j]]), numeric(1))
    if (!any(is.finite(logp))) stop("No valid discount-grid covariance path.")
    j <- sample.int(length(paths), 1L, prob = exp(logp - max(logp)))
    list(discount = cfg$discount_grid[j], ref = paths[[j]])
}

accept_reference <- function(Y, H, offset, beta, discount, ref, cfg) {
    proposed <- tryCatch(reference_path(Y, H, offset, discount, cfg), error = function(e) NULL)
    if (is.null(proposed)) return(list(accepted = FALSE, ref = ref))
    logratio <- factor_log_density(beta, proposed) - factor_log_density(beta, ref)
    accept <- is.finite(logratio) && log(runif(1)) < min(0, logratio)
    list(accepted = accept, ref = if (accept) proposed else ref)
}

interweave_general_loading_factor <- function(Y, H, offset, beta, L, prior_var,
    discount, ref, proposal_sd, cfg) {
    delta <- rnorm(1L, 0, proposal_sd)
    scale <- exp(delta)
    initial <- attr(beta, "initial", exact = TRUE)
    candidate_beta <- beta * scale
    attr(candidate_beta, "initial") <- initial * scale
    candidate_H <- H / scale
    candidate_ref <- tryCatch(reference_path(Y, candidate_H, offset, discount, cfg),
        error = function(e) NULL)
    if (!is.null(candidate_ref)) {
        # Inverse scaling of the loadings preserves the observation predictor.
        # The initial factor and loadings still contribute to the joint density.
        log_state_ratio <- factor_log_density(candidate_beta, candidate_ref) -
            factor_log_density(beta, ref) - 0.5 * expm1(2 * delta) * sum(initial^2)
        variance <- dynamic_scale(cfg)^2 * prior_var
        log_loading_ratio <- -0.5 * expm1(-2 * delta) * sum(L^2 / variance)
        log_jacobian <- (length(beta) + length(initial) - length(L)) * delta
        log_ratio <- log_state_ratio + log_loading_ratio + log_jacobian
        if (is.finite(log_ratio) && log(runif(1L)) < log_ratio)
            return(list(beta = candidate_beta, L = L / scale, H = candidate_H,
                reference = candidate_ref, accepted = 1L, jump = abs(delta)))
    }
    list(beta = beta, L = L, H = H, reference = ref, accepted = 0L, jump = 0)
}

# One predict-before-update step, conditional on a retained parameter draw.
# The one-PG-sweep assimilation is an approximation, not an exact online filter.
forecast_factor_step <- function(beta, ref, discount, evolution, H, base, y, rn,
    reference_backend = "native") {
    W <- if (evolution == "unit") 1 else (1 / discount - 1) * ref$C
    bp <- beta + rnorm(length(beta), sd = sqrt(W))
    eta <- base + as.numeric(H %*% bp)
    if (any(!is.finite(eta))) stop("Non-finite forecast linear predictor.")
    om <- pmax(BayesLogit::rpg(length(y), h = y + rn, z = eta - log(rn)), 1e-10)
    ys <- log(rn) + (y - rn) / (2 * om)
    precision <- diag(1 / W, length(beta)) + crossprod(H, H * om)
    updated <- rmvn_precision(precision, beta / W + crossprod(H, om * (ys - base)))
    if (evolution != "unit") ref <- reference_step(y, as.numeric(H), base,
        ref$m, ref$C, discount, backend = reference_backend)
    list(eta = eta, beta = updated, ref = ref)
}

stored_reference <- function(fit, s) {
    list(m = if (is.null(fit$reference_mean)) 0 else fit$reference_mean[s],
         C = if (is.null(fit$reference_variance)) 1 else fit$reference_variance[s])
}

stored_discount <- function(fit, s) if (is.null(fit$discount)) NA_real_ else fit$discount[s]
stored_evolution <- function(fit) if (is.null(fit$factor_evolution)) "unit" else fit$factor_evolution


discount_occupancy <- function(chains, cfg) {
    if (cfg$factor_evolution == "unit") return(NULL)
    do.call(rbind, lapply(chains, function(z) data.frame(
        chain = z$chain_id, discount = cfg$discount_grid,
        probability = vapply(cfg$discount_grid, function(d) mean(z$discount == d), numeric(1)))))
}
