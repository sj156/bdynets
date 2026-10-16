# Gaussian reference covariance and static-plus-dynamic network helpers.
# The public package currently supports rank one.
dynamic_scale <- function(cfg) if (isTRUE(cfg$static_network)) cfg$dynamic_loading_scale else 1

static_network <- function(X, pairs, B, d = NULL) {
    if (is.null(B)) return(matrix(0, nrow(X), ncol(X)))
    if (!is.null(d)) B <- B * d
    out <- matrix(0, nrow(X), ncol(X))
    for (a in unique(pairs$target)) {
        ii <- which(pairs$target == a)
        out[, a] <- as.numeric(X[, pairs$source[ii], drop = FALSE] %*% B[ii])
    }
    out
}

reference_step <- function(y, h, offset, m, C, discount) {
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

reference_path <- function(Y, H, offset, discount, cfg) {
    TT <- nrow(Y)
    if (cfg$factor_evolution == "unit") return(list(W = rep(1, TT), m = 0, C = 1))
    m <- 0; C <- 1; W <- numeric(TT)
    for (tt in seq_len(TT)) {
        W[tt] <- (1 / discount - 1) * C
        z <- reference_step(Y[tt, ], as.numeric(H[tt, , ]), offset[tt, ], m, C, discount)
        m <- z$m; C <- z$C
    }
    list(W = W, m = m, C = C)
}

factor_log_density <- function(beta, ref) {
    b0 <- attr(beta, "initial")
    if (is.null(b0)) stop("Factor path must include its initial draw.")
    sum(dnorm(diff(c(b0, as.numeric(beta))), sd = sqrt(ref$W), log = TRUE))
}

update_discount <- function(Y, H, offset, beta, cfg) {
    if (cfg$factor_evolution == "unit") return(list(discount = NA_real_, ref = reference_path(Y, H, offset, NA_real_, cfg)))
    paths <- lapply(cfg$discount_grid, function(d) reference_path(Y, H, offset, d, cfg))
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

# One predict-before-update step, conditional on a retained parameter draw.
# The one-PG-sweep assimilation is an approximation, not an exact online filter.
forecast_factor_step <- function(beta, ref, discount, evolution, H, base, y, rn) {
    W <- if (evolution == "unit") 1 else (1 / discount - 1) * ref$C
    bp <- beta + rnorm(length(beta), sd = sqrt(W))
    eta <- base + as.numeric(H %*% bp)
    if (any(!is.finite(eta))) stop("Non-finite forecast linear predictor.")
    om <- pmax(BayesLogit::rpg(length(y), h = y + rn, z = eta - log(rn)), 1e-10)
    ys <- log(rn) + (y - rn) / (2 * om)
    precision <- diag(1 / W, length(beta)) + crossprod(H, H * om)
    updated <- rmvn_precision(precision, beta / W + crossprod(H, om * (ys - base)))
    if (evolution != "unit") ref <- reference_step(y, as.numeric(H), base, ref$m, ref$C, discount)
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
