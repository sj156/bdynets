# Internal BDCN engine.
# Mechanically migrated from TrafficFlowNTS_section6_grid_long_range.R.
# The package API and validation live in api.R; these functions are not exported.

clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

symmetrize <- function(A) (A + t(A))/2

chol_jitter <- function(A, initial = 1e-10, attempts = 10L) {
    A <- symmetrize(A)
    jitter <- initial
    for (i in seq_len(attempts)) {
        z <- try(chol(A + diag(jitter, nrow(A))), silent = TRUE)
        if (!inherits(z, "try-error")) 
            return(z)
        jitter <- jitter * 10
    }
    stop("Cholesky factorization failed.")
}

solve_spd <- function(A) chol2inv(chol_jitter(A))

rmvn_cov <- function(mu, Sigma) as.numeric(mu + t(chol_jitter(Sigma)) %*% rnorm(length(mu)))

rmvn_precision <- function(P, rhs) {
    R <- chol_jitter(P)
    as.numeric(backsolve(R, forwardsolve(t(R), rhs)) + backsolve(R, rnorm(length(rhs))))
}

rinvgamma <- function(n, shape, rate) 1/rgamma(n, shape = shape, rate = rate)

log_mean_exp <- function(x) {
    m <- max(x)
    m + log(mean(exp(x - m)))
}

softplus <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))

log_half_cauchy_logscale <- function(q, prior_scale = 1) {
    z <- q - log(prior_scale)
    z - softplus(2 * z)
}

joint_horseshoe_scale_move <- function(v, u, prior_scale, proposal_sd, n_steps = 2L) {
    q_tau <- 0.5 * log(v)
    q_local <- 0.5 * log(u)
    accepted <- 0L
    lo_tau <- 0.5 * log(1e-12)
    hi_tau <- 0.5 * log(1e+06)
    lo_local <- 0.5 * log(1e-12)
    hi_local <- 0.5 * log(1e+12)
    log_prior <- log_half_cauchy_logscale(q_tau, prior_scale) + sum(log_half_cauchy_logscale(q_local, 1))
    for (step in seq_len(n_steps)) {
        shift <- rnorm(1L, 0, proposal_sd)
        q_tau_new <- q_tau + shift
        q_local_new <- q_local - shift
        if (q_tau_new < lo_tau || q_tau_new > hi_tau || any(q_local_new < lo_local | q_local_new > hi_local)) 
            next
        log_prior_new <- log_half_cauchy_logscale(q_tau_new, prior_scale) + sum(log_half_cauchy_logscale(q_local_new, 1))
        if (log(runif(1L)) < log_prior_new - log_prior) {
            q_tau <- q_tau_new
            q_local <- q_local_new
            log_prior <- log_prior_new
            accepted <- accepted + 1L
        }
    }
    list(v = exp(2 * q_tau), u = exp(2 * q_local), accepted = accepted)
}

method_seed <- function(stage, label, cfg = CFG) {
    z <- utf8ToInt(paste(stage, label, sep = "::"))
    as.integer((cfg$seed + sum((seq_along(z) + 23) * z))%%2147483646 + 1)
}

format_seconds <- function(x) {
    x <- round(as.numeric(x))
    sprintf("%dm %02ds", x%/%60, x%%60)
}

progress_with_eta <- function(label, current, total, start_time) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    remaining <- if (current > 0L) 
        elapsed * max(total - current, 0L)/current
    else NA_real_
    pct <- if (total > 0L) 
        100 * current/total
    else 100
    message(sprintf("[%s] %d/%d (%.1f%%); elapsed %s; ETA remaining %s", label, current, total, pct, format_seconds(elapsed), 
        if (is.finite(remaining)) 
            format_seconds(remaining)
        else "estimating"))
}

balanced_indices <- function(chain, n, seed) {
    if (n >= length(chain)) 
        return(seq_along(chain))
    set.seed(seed)
    groups <- split(seq_along(chain), chain)
    per <- max(1L, floor(n/length(groups)))
    idx <- unlist(lapply(groups, function(z) sample(z, min(per, length(z)))))
    if (length(idx) < n) 
        idx <- c(idx, sample(setdiff(seq_along(chain), idx), n - length(idx)))
    sort(idx)
}

make_directed_grid <- function(side) {
    nodes <- expand.grid(row = seq_len(side), col = seq_len(side))
    nodes$id <- seq_len(nrow(nodes))
    links <- list()
    j <- 0L
    for (r in seq_len(side)) for (c in seq_len(side)) {
        u <- nodes$id[nodes$row == r & nodes$col == c]
        if (c < side) {
            v <- nodes$id[nodes$row == r & nodes$col == c + 1L]
            j <- j + 1L
            links[[j]] <- c(u, v)
        }
        if (r < side) {
            v <- nodes$id[nodes$row == r + 1L & nodes$col == c]
            j <- j + 1L
            links[[j]] <- c(u, v)
        }
    }
    uv <- do.call(rbind, links)
    edges <- rbind(data.frame(from = uv[, 1], to = uv[, 2]), data.frame(from = uv[, 2], to = uv[, 1]))
    edges$id <- seq_len(nrow(edges))
    edges$name <- paste0("e", edges$id)
    edges$x <- nodes$col[edges$from]
    edges$y <- nodes$row[edges$from]
    edges$xend <- nodes$col[edges$to]
    edges$yend <- nodes$row[edges$to]
    k <- nrow(edges)
    A <- matrix(0, k, k, dimnames = list(target = edges$name, source = edges$name))
    for (a in seq_len(k)) for (b in seq_len(k)) {
        if (edges$to[b] == edges$from[a] && edges$from[b] != edges$to[a]) 
            A[a, b] <- 1
    }
    W <- A
    rs <- rowSums(W)
    W[rs > 0, ] <- W[rs > 0, , drop = FALSE]/rs[rs > 0]
    list(nodes = nodes, edges = edges, A = A, W = W)
}

hop_matrix <- function(A) {
    k <- nrow(A)
    out <- matrix(Inf, k, k)
    diag(out) <- 0
    reach <- diag(k)
    for (d in seq_len(k - 1L)) {
        reach <- reach %*% A
        new <- reach > 0 & !is.finite(out)
        out[new] <- d
    }
    out
}

make_full_pairs <- function(graph, cfg = CFG) {
    k <- nrow(graph$edges)
    tab <- expand.grid(target = seq_len(k), source = seq_len(k))
    tab <- tab[order(tab$target, tab$source), ]
    rownames(tab) <- NULL
    H <- hop_matrix(graph$A)
    tab$hop <- H[cbind(tab$target, tab$source)]
    tab <- tab[!is.finite(tab$hop) | tab$hop >= 2L, , drop = FALSE]
    tab <- tab[order(tab$target, tab$source), ]
    rownames(tab) <- NULL
    if (length(unique(tab$target)) != k) 
        stop("Every target must have at least one nonlocal dynamic candidate.")
    tab$pair_id <- seq_len(nrow(tab))
    raw <- ifelse(is.finite(tab$hop), cfg$affinity_floor + (1 - cfg$affinity_floor) * exp(-cfg$affinity_decay * tab$hop), 
        cfg$affinity_floor)
    tab$affinity <- exp(log(raw) - mean(log(raw)))
    tab$pair <- paste0(graph$edges$name[tab$source], " -> ", graph$edges$name[tab$target])
    tab
}

choose_long_pairs <- function(graph, cfg = CFG) {
    H <- hop_matrix(graph$A)
    k <- nrow(H)
    candidates <- which(H == 2, arr.ind = TRUE)
    candidates <- data.frame(target = candidates[, 1], source = candidates[, 2])
    counts <- table(candidates$target)
    multiple <- as.integer(names(counts[counts >= 2]))
    if (length(multiple)) 
        candidates <- candidates[candidates$target %in% multiple, ]
    if (!nrow(candidates)) 
        stop("The grid has no admissible two-hop source-target pairs.")
    set.seed(method_seed("design", "long pairs", cfg))
    target_order <- sample(unique(candidates$target))
    chosen <- do.call(rbind, lapply(target_order, function(a) {
        z <- candidates[candidates$target == a, , drop = FALSE]
        z[sample(nrow(z), 1L), , drop = FALSE]
    }))
    chosen <- head(chosen, min(cfg$n_long_pairs, nrow(chosen)))
    chosen$heterogeneity <- rep(cfg$long_heterogeneity, length.out = nrow(chosen))
    chosen$long_id <- seq_len(nrow(chosen))
    rownames(chosen) <- NULL
    chosen
}

truth_amplitudes <- function(graph, long_pairs, signal, cfg = CFG) {
    k <- nrow(graph$edges)
    A <- matrix(0, k, k)
    if (nrow(long_pairs)) {
        A[cbind(long_pairs$target, long_pairs$source)] <- signal * long_pairs$heterogeneity
    }
    A
}

generate_identified_beta <- function(total, cfg = CFG) {
    if (cfg$factor_dim != 1L) 
        stop("This scalar-factor DGP requires factor_dim = 1.")
    set.seed(method_seed("DGP", "identified unit random walk", cfg))
    beta <- numeric(total)
    previous <- rnorm(1L)
    for (t in seq_len(total)) {
        beta[t] <- previous + rnorm(1L)
        previous <- beta[t]
    }
    beta
}

transform_counts <- function(y, center, scale) {
    sweep(sweep(log1p(y), 2L, center, "-"), 2L, scale, "/")
}

make_time_covariate <- function(n) {
    if (n <= 1L) 
        return(rep(0, n))
    seq(-0.5, 0.5, length.out = n)
}

make_season_covariate <- function(n, cfg = CFG, indices = seq_len(n)) {
    period <- cfg$n_total/cfg$seasonal_cycles
    sin(2 * pi * indices/period)
}

simulate_once <- function(alpha, L, means, center, scale, n_keep, burn, beta_path, time_covariate, season_covariate, W, seed, 
    cfg = CFG) {
    set.seed(seed)
    k <- length(alpha)
    total <- burn + n_keep
    if (length(beta_path) < n_keep) 
        stop("beta_path is shorter than n_keep.")
    if (length(time_covariate) != n_keep) 
        stop("time_covariate must have length n_keep.")
    if (length(season_covariate) != n_keep) 
        stop("season_covariate must have length n_keep.")
    Y <- matrix(NA_integer_, total + 2L, k)
    Y[1:2, ] <- matrix(rpois(2L * k, rep(means, each = 2L)), 2L, k)
    lambda <- matrix(NA_real_, total, k)
    beta <- c(rep(beta_path[1L], burn), beta_path[seq_len(n_keep)])
    trend <- c(rep(time_covariate[1L], burn), time_covariate)
    season_burn <- make_season_covariate(burn, cfg, indices = seq.int(1L - burn, 0L))
    season <- c(season_burn, season_covariate)
    for (t in seq_len(total)) {
        row <- t + 2L
        x <- (log1p(Y[row - 1L, ]) - center)/scale
        static_effect <- cfg$self_effect * x + cfg$local_network_effect * as.numeric(W %*% x)
        lam <- exp(alpha + cfg$time_trend_log_change * trend[t] + cfg$seasonal_log_amplitude * season[t] + static_effect + 
            beta[t] * as.numeric(L %*% x))
        if (any(!is.finite(lam)) || any(lam > cfg$maximum_rate)) 
            return(list(stable = FALSE, time = t, maximum = max(lam)))
        lambda[t, ] <- lam
        Y[row, ] <- rpois(k, lam)
    }
    keep <- burn + seq_len(n_keep)
    list(stable = TRUE, Y = Y[keep + 2L, ], lambda = lambda[keep, ], beta = beta[keep], time_covariate = trend[keep], season_covariate = season[keep], 
        pre_history = Y[(burn + 1L):(burn + 2L), ])
}

simulate_grid <- function(graph, long_pairs, signal, cfg = CFG) {
    k <- nrow(graph$edges)
    set.seed(method_seed("design", "target means", cfg))
    means <- runif(k, cfg$target_mean_min, cfg$target_mean_max)
    center <- log1p(means)
    scale <- rep(cfg$x_scale, k)
    maximum_length <- max(cfg$n_total, cfg$calibration_length)
    beta_path <- generate_identified_beta(maximum_length, cfg)
    retained_beta <- beta_path[seq_len(cfg$n_total)]
    beta_rms <- sqrt(mean(retained_beta^2))
    target_amplitude <- truth_amplitudes(graph, long_pairs, signal, cfg)
    beta_peak_ratio <- max(abs(retained_beta))/beta_rms
    static_feedback <- sweep(cfg$self_effect * diag(k) + cfg$local_network_effect * graph$W, 2L, scale, "/")
    dynamic_feedback <- sweep(target_amplitude, 2L, scale, "/")
    beta_ratio <- retained_beta/beta_rms
    spectral_radius_bound <- max(vapply(beta_ratio, function(bt) max(Mod(eigen(static_feedback + bt * dynamic_feedback, only.values = TRUE)$values)), 
        numeric(1)))
    if (!is.finite(spectral_radius_bound) || spectral_radius_bound >= cfg$dgp_spectral_radius_limit) 
        stop(sprintf(paste0("DGP feedback bound %.3f exceeds limit %.3f; ", "reduce self/local/long-range amplitude."), spectral_radius_bound, 
            cfg$dgp_spectral_radius_limit))
    L <- target_amplitude/beta_rms
    alpha <- log(means)
    history <- list()
    calibration_time <- make_time_covariate(cfg$calibration_length)
    calibration_season <- make_season_covariate(cfg$calibration_length, cfg)
    for (i in seq_len(cfg$calibration_iterations)) {
        z <- simulate_once(alpha, L, means, center, scale, cfg$calibration_length, cfg$simulation_burn, beta_path, calibration_time, 
            calibration_season, graph$W, method_seed("calibration", paste(signal, i), cfg), cfg)
        if (!z$stable) 
            stop(sprintf("Unstable calibration at time %d (maximum rate %.3g); reduce DGP feedback amplitudes.", z$time, 
                z$maximum))
        observed <- colMeans(z$Y)
        history[[i]] <- data.frame(signal = signal, iteration = i, edge = graph$edges$name, target_mean = means, observed_mean = observed, 
            alpha = alpha)
        alpha <- alpha + cfg$calibration_damping * log(means/observed)
    }
    final_time <- make_time_covariate(cfg$n_total)
    final_season <- make_season_covariate(cfg$n_total, cfg)
    z <- simulate_once(alpha, L, means, center, scale, cfg$n_total, cfg$simulation_burn, beta_path, final_time, final_season, 
        graph$W, method_seed("data", "nested grid", cfg), cfg)
    if (!z$stable) 
        stop(sprintf("Unstable final DGP at time %d (maximum rate %.3g); reduce DGP feedback amplitudes.", z$time, z$maximum))
    z$alpha <- alpha
    z$L <- L
    z$means <- means
    z$target_amplitude <- target_amplitude
    z$beta_rms <- beta_rms
    z$beta_peak_ratio <- beta_peak_ratio
    z$spectral_radius_bound <- spectral_radius_bound
    z$trend_coefficient <- cfg$time_trend_log_change
    z$season_coefficient <- cfg$seasonal_log_amplitude
    z$center <- center
    z$scale <- scale
    z$signal <- signal
    z$calibration <- do.call(rbind, history)
    z
}

make_lags <- function(Y, pre_history, center, scale, max_lag = 2L) {
    TT <- nrow(Y)
    hist <- rbind(tail(pre_history, max_lag), Y)
    out <- array(NA_real_, c(TT, ncol(Y), max_lag))
    for (ell in seq_len(max_lag)) {
        rows <- max_lag + seq_len(TT) - ell
        out[, , ell] <- transform_counts(hist[rows, ], center, scale)
    }
    out
}

standardize_lags_from_train <- function(raw_lags, train) {
    center <- colMeans(raw_lags[train, , 1L, drop = FALSE][, , 1L])
    scale <- apply(raw_lags[train, , 1L, drop = FALSE][, , 1L], 2L, sd)
    scale[!is.finite(scale) | scale < 1e-08] <- 1
    out <- raw_lags
    for (ell in seq_len(dim(raw_lags)[3L])) out[, , ell] <- sweep(sweep(raw_lags[, , ell], 2L, center, "-"), 2L, scale, "/")
    list(lags = out, center = center, scale = scale)
}

choose_r_nb_time <- function(Y, cfg = CFG) {
    TT <- nrow(Y)
    pilot <- Y
    if (TT >= 3L) 
        pilot[2:(TT - 1L), ] <- (Y[1:(TT - 2L), ] + Y[2:(TT - 1L), ] + Y[3:TT, ])/3
    q <- apply(pmax(pilot, 1), 1L, quantile, probs = cfg$r_nb_quantile, type = 8)
    r <- ceiling(q/cfg$r_nb_epsilon)
    step <- max(1L, as.integer(cfg$r_nb_round_to))
    clamp(ceiling(r/step) * step, cfg$r_nb_min, cfg$r_nb_max)
}

build_network_array <- function(X, pairs, L, r, k) {
    H <- array(0, c(nrow(X), k, r))
    ids <- split(seq_len(nrow(pairs)), pairs$target)
    for (a in seq_len(k)) {
        ii <- ids[[as.character(a)]]
        H[, a, ] <- X[, pairs$source[ii], drop = FALSE] %*% L[ii, , drop = FALSE]
    }
    H
}

network_contribution <- function(H, beta) {
    out <- matrix(0, dim(H)[1L], dim(H)[2L])
    for (j in seq_len(dim(H)[3L])) out <- out + H[, , j] * beta[, j]
    out
}

ffbs_factor_identified <- function(response, H, omega) {
    TT <- nrow(response)
    r <- dim(H)[3L]
    m <- a <- matrix(0, TT, r)
    C <- Rpred <- array(0, c(r, r, TT))
    mp <- rep(0, r)
    Cp <- diag(1, r)
    for (t in seq_len(TT)) {
        a[t, ] <- mp
        Rt <- symmetrize(Cp + diag(1, r))
        Ht <- H[t, , , drop = TRUE]
        if (r == 1L) 
            Ht <- matrix(Ht, ncol = 1L)
        Rinv <- solve_spd(Rt)
        wt <- omega[t, ]
        Ct <- solve_spd(Rinv + crossprod(Ht, Ht * wt))
        mt <- Ct %*% (Rinv %*% a[t, ] + crossprod(Ht, wt * response[t, ]))
        m[t, ] <- mt
        C[, , t] <- Ct
        Rpred[, , t] <- Rt
        mp <- mt
        Cp <- Ct
    }
    draw <- matrix(0, TT, r)
    draw[TT, ] <- rmvn_cov(m[TT, ], C[, , TT])
    if (TT > 1L) 
        for (t in (TT - 1L):1L) {
            Rn <- Rpred[, , t + 1L]
            B <- C[, , t] %*% solve_spd(Rn)
            mu <- m[t, ] + B %*% (draw[t + 1L, ] - a[t + 1L, ])
            V <- symmetrize(C[, , t] - B %*% Rn %*% t(B))
            draw[t, ] <- rmvn_cov(mu, V)
        }
    draw
}

physical_network_lag <- function(X, W) X %*% t(W)

baseline_matrix <- function(alpha, common_coef, self_coef, w_coef, X, WX, time_covariate, season_covariate) {
    outer(rep(1, nrow(X)), alpha) + common_coef["time_trend"] * outer(time_covariate, rep(1, ncol(X))) + common_coef["seasonal_sin"] * 
        outer(season_covariate, rep(1, ncol(X))) + sweep(X, 2L, self_coef, "*") + sweep(WX, 2L, w_coef, "*")
}

update_alpha_loadings <- function(ystar_minus_common, X, WX, beta, omega, pairs, prior_var, means, alpha_prior_sd, static_prior_sd) {
    k <- ncol(ystar_minus_common)
    r <- ncol(beta)
    L <- matrix(0, nrow(pairs), r)
    alpha <- self_coef <- w_coef <- numeric(k)
    ids <- split(seq_len(nrow(pairs)), pairs$target)
    for (a in seq_len(k)) {
        ii <- ids[[as.character(a)]]
        src <- pairs$source[ii]
        Z_network <- if (r == 1L) 
            X[, src, drop = FALSE] * as.numeric(beta[, 1L])
        else do.call(cbind, lapply(src, function(s) beta * X[, s]))
        Z <- cbind(Intercept = 1, SelfLag = X[, a], PhysicalW = WX[, a], Z_network)
        prior_precision <- c(1/alpha_prior_sd^2, rep(1/static_prior_sd^2, 2L), rep(1/prior_var[ii], each = r))
        prior_mean <- c(log(means[a]), 0, 0, rep(0, length(prior_precision) - 3L))
        precision <- crossprod(Z, Z * omega[, a]) + diag(prior_precision)
        rhs <- crossprod(Z, omega[, a] * ystar_minus_common[, a]) + prior_precision * prior_mean
        draw <- rmvn_precision(precision, rhs)
        alpha[a] <- draw[1L]
        self_coef[a] <- draw[2L]
        w_coef[a] <- draw[3L]
        L[ii, ] <- matrix(draw[-c(1L, 2L, 3L)], ncol = r, byrow = TRUE)
    }
    list(alpha = alpha, self_coef = self_coef, w_coef = w_coef, L = L)
}

update_common_time_effects <- function(ystar, X, WX, net, omega, alpha, self_coef, w_coef, time_covariate, season_covariate, 
    trend_prior_sd, seasonal_prior_sd) {
    TT <- nrow(X)
    k <- ncol(X)
    residual <- ystar - outer(rep(1, TT), alpha) - net - sweep(X, 2L, self_coef, "*") - sweep(WX, 2L, w_coef, "*")
    Z <- cbind(time_trend = as.numeric(outer(time_covariate, rep(1, k))), seasonal_sin = as.numeric(outer(season_covariate, 
        rep(1, k))))
    weight <- as.numeric(omega)
    prior_precision <- c(1/trend_prior_sd^2, 1/seasonal_prior_sd^2)
    precision <- crossprod(Z, Z * weight) + diag(prior_precision)
    rhs <- crossprod(Z, weight * as.numeric(residual))
    draw <- rmvn_precision(precision, rhs)
    names(draw) <- colnames(Z)
    draw
}

fit_chain <- function(Y, X, time_covariate, season_covariate, W, pairs, means, chain_id, signal, model_label = "BDCN full dynamic nonlocal domain", 
    cfg = CFG) {
    set.seed(method_seed("MCMC", paste(model_label, signal, chain_id), cfg))
    start <- Sys.time()
    TT <- nrow(Y)
    k <- ncol(Y)
    P <- nrow(pairs)
    r <- cfg$factor_dim
    factor_reference_scale <- sqrt((TT + 3)/2)
    WX <- physical_network_lag(X, W)
    alpha <- log(colMeans(Y) + 0.1)
    self_coef <- w_coef <- numeric(k)
    trend_initial <- season_initial <- numeric(k)
    beta <- matrix(factor_reference_scale, TT, r)
    L <- matrix(0, P, r)
    ids <- split(seq_len(P), pairs$target)
    for (a in seq_len(k)) {
        ii <- ids[[as.character(a)]]
        Z <- cbind(1, time_covariate, season_covariate, X[, a], WX[, a], X[, pairs$source[ii]])
        g0 <- suppressWarnings(glm.fit(Z, Y[, a], family = poisson()))
        co <- g0$coefficients
        co[!is.finite(co)] <- 0
        alpha[a] <- co[1L]
        trend_initial[a] <- co[2L]
        season_initial[a] <- co[3L]
        self_coef[a] <- co[4L]
        w_coef[a] <- co[5L]
        L[ii, 1L] <- co[-seq_len(5L)]/factor_reference_scale
    }
    common_coef <- c(time_trend = median(trend_initial), seasonal_sin = median(season_initial))
    r_nb_t <- choose_r_nb_time(Y, cfg)
    RNB <- matrix(r_nb_t, TT, k)
    expected_active <- cfg$n_long_pairs
    expected_active <- clamp(expected_active, 1, P - 1)
    tau_initial <- max((expected_active/(P - expected_active))/(sqrt(TT) * factor_reference_scale), cfg$tau_initial_floor)
    tau_prior_scale <- max(cfg$tau_prior_min_scale, cfg$tau_prior_multiplier * tau_initial)
    u <- rep(1, P)
    v <- tau_initial^2
    nu <- rinvgamma(P, 1, 1 + 1/u)
    xi <- rinvgamma(1, 1, 1/tau_prior_scale^2 + 1/v)
    S <- floor((cfg$n_iter - cfg$burn_mcmc)/cfg$thin)
    alpha_save <- self_save <- w_save <- matrix(NA_real_, S, k)
    common_save <- matrix(NA_real_, S, 2L, dimnames = list(NULL, names(common_coef)))
    final_beta <- matrix(NA_real_, S, r)
    beta_save <- array(NA_real_, c(S, TT, r))
    L_save <- array(NA_real_, c(S, P, r))
    amplitude <- theta_average <- matrix(NA_real_, S, P)
    tau_save <- network_rms <- numeric(S)
    si <- 0L
    scale_move_log_sd <- log(cfg$scale_move_initial_sd)
    scale_move_accepted <- scale_move_proposed <- 0L
    scale_move_post_accepted <- scale_move_post_proposed <- 0L
    for (iter in seq_len(cfg$n_iter)) {
        H <- build_network_array(X, pairs, L, r, k)
        net <- network_contribution(H, beta)
        baseline <- baseline_matrix(alpha, common_coef, self_coef, w_coef, X, WX, time_covariate, season_covariate)
        eta <- baseline + net
        zeta <- as.numeric(eta) - log(as.numeric(RNB))
        omega <- matrix(BayesLogit::rpg(length(zeta), h = as.numeric(Y) + as.numeric(RNB), z = zeta), TT, k)
        omega <- pmax(omega, 1e-10)
        ystar <- log(RNB) + (Y - RNB)/(2 * omega)
        prior_var <- pmax(v * u * pairs$affinity^2, 1e-12)
        for (state_step in seq_len(cfg$state_block_substeps)) {
            H <- build_network_array(X, pairs, L, r, k)
            baseline <- baseline_matrix(alpha, common_coef, self_coef, w_coef, X, WX, time_covariate, season_covariate)
            beta <- ffbs_factor_identified(ystar - baseline, H, omega)
            common_offset <- common_coef["time_trend"] * outer(time_covariate, rep(1, k)) + common_coef["seasonal_sin"] * 
                outer(season_covariate, rep(1, k))
            block <- update_alpha_loadings(ystar - common_offset, X, WX, beta, omega, pairs, prior_var, means, cfg$alpha_prior_sd, 
                cfg$static_prior_sd)
            alpha <- block$alpha
            self_coef <- block$self_coef
            w_coef <- block$w_coef
            L <- block$L
            H <- build_network_array(X, pairs, L, r, k)
            net <- network_contribution(H, beta)
            common_coef <- update_common_time_effects(ystar, X, WX, net, omega, alpha, self_coef, w_coef, time_covariate, 
                season_covariate, cfg$trend_prior_sd, cfg$seasonal_prior_sd)
        }
        L2 <- rowSums(L^2)
        for (shrinkage_step in seq_len(cfg$shrinkage_substeps)) {
            u <- clamp(rinvgamma(P, (r + 1)/2, 1/nu + L2/(2 * v * pairs$affinity^2)), 1e-12, 1e+12)
            nu <- clamp(rinvgamma(P, 1, 1 + 1/u), 1e-12, 1e+12)
            v <- clamp(rinvgamma(1, (r * P + 1)/2, 1/xi + 0.5 * sum(L2/(u * pairs$affinity^2))), 1e-12, 1e+06)
            xi <- clamp(rinvgamma(1, 1, 1/tau_prior_scale^2 + 1/v), 1e-12, 1e+12)
        }
        move <- joint_horseshoe_scale_move(v, u, tau_prior_scale, exp(scale_move_log_sd), cfg$scale_move_steps)
        v <- move$v
        u <- move$u
        scale_move_accepted <- scale_move_accepted + move$accepted
        scale_move_proposed <- scale_move_proposed + cfg$scale_move_steps
        if (iter > cfg$burn_mcmc) {
            scale_move_post_accepted <- scale_move_post_accepted + move$accepted
            scale_move_post_proposed <- scale_move_post_proposed + cfg$scale_move_steps
        }
        nu <- clamp(rinvgamma(P, 1, 1 + 1/u), 1e-12, 1e+12)
        xi <- clamp(rinvgamma(1, 1, 1/tau_prior_scale^2 + 1/v), 1e-12, 1e+12)
        if (iter <= cfg$burn_mcmc) {
            gain <- cfg$scale_move_adapt_rate/sqrt(iter)
            iteration_acceptance <- move$accepted/cfg$scale_move_steps
            scale_move_log_sd <- clamp(scale_move_log_sd + gain * (iteration_acceptance - cfg$scale_move_target_acceptance), 
                log(cfg$scale_move_min_sd), log(cfg$scale_move_max_sd))
        }
        if (iter > cfg$burn_mcmc && (iter - cfg$burn_mcmc)%%cfg$thin == 0L) {
            si <- si + 1L
            alpha_save[si, ] <- alpha
            self_save[si, ] <- self_coef
            w_save[si, ] <- w_coef
            common_save[si, ] <- common_coef
            final_beta[si, ] <- beta[TT, ]
            beta_save[si, , ] <- beta
            L_save[si, , ] <- L
            theta_path <- L %*% t(beta)
            amplitude[si, ] <- sqrt(rowMeans(theta_path^2))
            theta_average[si, ] <- rowMeans(theta_path)
            tau_save[si] <- sqrt(v)
            network_rms[si] <- sqrt(mean(network_contribution(build_network_array(X, pairs, L, r, k), beta)^2))
        }
        if (iter == 1L || iter%%cfg$progress_every == 0L || iter == cfg$n_iter) {
            progress_with_eta(sprintf("%s; signal %.2f; chain %d", model_label, signal, chain_id), iter, cfg$n_iter, start)
        }
    }
    list(chain_id = chain_id, alpha = alpha_save, trend_coef = common_save[, "time_trend", drop = FALSE], season_coef = common_save[, 
        "seasonal_sin", drop = FALSE], common_coef = common_save, self_coef = self_save, w_coef = w_save, final_beta = final_beta, 
        beta_path = beta_save, L = L_save, amplitude = amplitude, theta_average = theta_average, tau = tau_save, network_rms = network_rms, 
        r_nb_t = r_nb_t, tau_initial = tau_initial, tau_prior_scale = tau_prior_scale, scale_move_acceptance = scale_move_accepted/scale_move_proposed, 
        scale_move_post_acceptance = scale_move_post_accepted/scale_move_post_proposed, scale_move_final_sd = exp(scale_move_log_sd), 
        elapsed = as.numeric(difftime(Sys.time(), start, units = "secs")))
}

bind_array3 <- function(xs) {
    d <- dim(xs[[1L]])
    out <- array(NA_real_, c(sum(vapply(xs, function(z) dim(z)[1L], integer(1))), d[2L], d[3L]))
    pos <- 1L
    for (z in xs) {
        n <- dim(z)[1L]
        out[pos:(pos + n - 1L), , ] <- z
        pos <- pos + n
    }
    out
}

fit_bdcn <- function(Y, X, time_covariate, season_covariate, W, pairs, means, signal, model_label = "BDCN full dynamic nonlocal domain", 
    cfg = CFG) {
    start <- Sys.time()
    ids <- seq_len(cfg$n_chains)
    worker <- function(ch) fit_chain(Y, X, time_covariate, season_covariate, W, pairs, means, ch, signal, model_label, cfg)
    cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
    if (length(cores) != 1L || !is.finite(cores)) 
        cores <- 1L
    chains <- if (cfg$parallel_chains && cfg$n_chains > 1L && .Platform$OS.type != "windows") 
        parallel::mclapply(ids, worker, mc.cores = min(cfg$n_chains, cores))
    else lapply(ids, worker)
    list(label = model_label, pairs = pairs, chains = chains, chain_id = unlist(lapply(chains, function(z) rep(z$chain_id, 
        nrow(z$alpha)))), alpha = do.call(rbind, lapply(chains, `[[`, "alpha")), trend_coef = do.call(rbind, lapply(chains, 
        `[[`, "trend_coef")), season_coef = do.call(rbind, lapply(chains, `[[`, "season_coef")), common_coef = do.call(rbind, 
        lapply(chains, `[[`, "common_coef")), self_coef = do.call(rbind, lapply(chains, `[[`, "self_coef")), w_coef = do.call(rbind, 
        lapply(chains, `[[`, "w_coef")), W = W, final_beta = do.call(rbind, lapply(chains, `[[`, "final_beta")), beta_path = bind_array3(lapply(chains, 
        `[[`, "beta_path")), L = bind_array3(lapply(chains, `[[`, "L")), amplitude = do.call(rbind, lapply(chains, `[[`, 
        "amplitude")), theta_average = do.call(rbind, lapply(chains, `[[`, "theta_average")), tau = unlist(lapply(chains, 
        `[[`, "tau")), network_rms = unlist(lapply(chains, `[[`, "network_rms")), r_nb_t = chains[[1L]]$r_nb_t, tau_initial = vapply(chains, 
        `[[`, numeric(1), "tau_initial"), tau_prior_scale = vapply(chains, `[[`, numeric(1), "tau_prior_scale"), scale_move_acceptance = vapply(chains, 
        `[[`, numeric(1), "scale_move_acceptance"), scale_move_post_acceptance = vapply(chains, `[[`, numeric(1), "scale_move_post_acceptance"), 
        scale_move_final_sd = vapply(chains, `[[`, numeric(1), "scale_move_final_sd"), elapsed = as.numeric(difftime(Sys.time(), 
            start, units = "secs")))
}

split_rhat <- function(draw_matrix) {
    draw_matrix <- as.matrix(draw_matrix)
    n0 <- nrow(draw_matrix)
    half <- floor(n0/2L)
    if (ncol(draw_matrix) < 2L || half < 2L) 
        return(NA_real_)
    split_draws <- do.call(cbind, lapply(seq_len(ncol(draw_matrix)), function(j) cbind(draw_matrix[seq_len(half), j], draw_matrix[(n0 - 
        half + 1L):n0, j])))
    W <- mean(apply(split_draws, 2L, var))
    B <- half * var(colMeans(split_draws))
    if (!is.finite(W) || W <= .Machine$double.eps) 
        return(if (is.finite(B) && B <= .Machine$double.eps) 1 else Inf)
    sqrt((((half - 1)/half) * W + B/half)/W)
}

acf_ess <- function(draw_matrix) {
    draw_matrix <- as.matrix(draw_matrix)
    n <- nrow(draw_matrix)
    m <- ncol(draw_matrix)
    if (n < 3L || m < 1L) 
        return(NA_real_)
    if (sd(as.numeric(draw_matrix)) <= .Machine$double.eps) 
        return(n * m)
    lag_max <- min(n - 1L, 1000L)
    rho <- vapply(seq_len(m), function(j) {
        x <- draw_matrix[, j]
        if (sd(x) <= .Machine$double.eps) 
            return(c(1, rep(0, lag_max)))
        as.numeric(stats::acf(x, lag.max = lag_max, plot = FALSE, demean = TRUE)$acf)
    }, numeric(lag_max + 1L))
    rho_bar <- rowMeans(rho)[-1L]
    if (!length(rho_bar)) 
        return(n * m)
    npair <- floor(length(rho_bar)/2L)
    keep <- 0L
    if (npair > 0L) {
        pair_sum <- rho_bar[2L * seq_len(npair) - 1L] + rho_bar[2L * seq_len(npair)]
        bad <- which(!is.finite(pair_sum) | pair_sum <= 0)
        keep <- if (length(bad)) 
            2L * (bad[1L] - 1L)
        else 2L * npair
    }
    tau_int <- 1 + if (keep > 0L) 
        2 * sum(rho_bar[seq_len(keep)])
    else 0
    min(n * m, max(1, n * m/max(tau_int, 1)))
}

chain_matrix <- function(fit, type, index = 1L) {
    n <- min(vapply(fit$chains, function(z) {
        if (type %in% c("tau", "network_rms")) length(z[[type]]) else nrow(z[[type]])
    }, integer(1)))
    do.call(cbind, lapply(fit$chains, function(z) {
        if (type %in% c("tau", "network_rms")) 
            z[[type]][seq_len(n)]
        else z[[type]][seq_len(n), index]
    }))
}

diagnose_fit <- function(fit, signal, model_id, long_pairs, cfg = CFG) {
    specs <- list(list(type = "tau", index = 1L, parameter = "tau", target = NA_integer_, source = NA_integer_, is_long = FALSE), 
        list(type = "network_rms", index = 1L, parameter = "network_RMS", target = NA_integer_, source = NA_integer_, is_long = FALSE))
    for (j in seq_len(ncol(fit$alpha))) specs[[length(specs) + 1L]] <- list(type = "alpha", index = j, parameter = sprintf("alpha[e%d]", 
        j), target = j, source = NA_integer_, is_long = FALSE)
    for (j in seq_len(ncol(fit$trend_coef))) specs[[length(specs) + 1L]] <- list(type = "trend_coef", index = j, parameter = "time_trend[global]", 
        target = NA_integer_, source = NA_integer_, is_long = FALSE)
    for (j in seq_len(ncol(fit$season_coef))) specs[[length(specs) + 1L]] <- list(type = "season_coef", index = j, parameter = "seasonal_sin[global]", 
        target = NA_integer_, source = NA_integer_, is_long = FALSE)
    for (j in seq_len(ncol(fit$self_coef))) specs[[length(specs) + 1L]] <- list(type = "self_coef", index = j, parameter = sprintf("static_self[e%d]", 
        j), target = j, source = j, is_long = FALSE)
    for (j in seq_len(ncol(fit$w_coef))) specs[[length(specs) + 1L]] <- list(type = "w_coef", index = j, parameter = sprintf("static_W[e%d]", 
        j), target = j, source = NA_integer_, is_long = FALSE)
    for (j in seq_len(nrow(fit$pairs))) {
        a <- fit$pairs$target[j]
        b <- fit$pairs$source[j]
        specs[[length(specs) + 1L]] <- list(type = "amplitude", index = j, parameter = sprintf("A[e%d -> e%d]", b, a), target = a, 
            source = b, is_long = any(long_pairs$target == a & long_pairs$source == b))
        specs[[length(specs) + 1L]] <- list(type = "theta_average", index = j, parameter = sprintf("mean_theta[e%d -> e%d]", 
            b, a), target = a, source = b, is_long = any(long_pairs$target == a & long_pairs$source == b))
    }
    rows <- lapply(specs, function(s) {
        mat <- chain_matrix(fit, s$type, s$index)
        pooled <- as.numeric(mat)
        rhat <- split_rhat(mat)
        ess <- acf_ess(mat)
        data.frame(signal = signal, Model = fit$label, ModelID = model_id, ParameterType = s$type, ParameterIndex = s$index, 
            Parameter = s$parameter, Target = s$target, Source = s$source, InjectedLongPair = s$is_long, Mean = mean(pooled), 
            SD = sd(pooled), Q025 = unname(quantile(pooled, 0.025)), Median = unname(quantile(pooled, 0.5)), Q975 = unname(quantile(pooled, 
                0.975)), SplitRhat = rhat, ESS = ess, MCSE = sd(pooled)/sqrt(ess), PassRhat = is.finite(rhat) && rhat <= 
                cfg$mcmc_rhat_threshold, PassESS = is.finite(ess) && ess >= cfg$mcmc_ess_threshold)
    })
    out <- do.call(rbind, rows)
    out$Converged <- out$PassRhat & out$PassESS
    out
}

convergence_summary <- function(diagnostics, cfg = CFG) {
    groups <- split(diagnostics, interaction(diagnostics$signal, diagnostics$ModelID, drop = TRUE))
    do.call(rbind, lapply(groups, function(z) data.frame(signal = z$signal[1L], Model = z$Model[1L], ModelID = z$ModelID[1L], 
        Parameters = nrow(z), MaxSplitRhat = max(z$SplitRhat, na.rm = TRUE), MinESS = min(z$ESS, na.rm = TRUE), FailedRhat = sum(!z$PassRhat), 
        FailedESS = sum(!z$PassESS), AllConverged = all(z$Converged), RhatThreshold = cfg$mcmc_rhat_threshold, ESSThreshold = cfg$mcmc_ess_threshold)))
}

nnlasso_path <- function(G, cvec, lambdas) {
    p <- length(cvec)
    d <- numeric(p)
    out <- matrix(0, p, length(lambdas))
    dg <- pmax(diag(G), 1e-12)
    for (li in seq_along(lambdas)) {
        for (it in seq_len(2000L)) {
            old <- d
            for (j in seq_len(p)) d[j] <- max(0, (cvec[j] - (sum(G[j, ] * d) - G[j, j] * d[j]) - lambdas[li]/2)/dg[j])
            if (max(abs(d - old)) < 1e-08 * (1 + max(abs(old)))) 
                break
        }
        out[, li] <- d
    }
    out
}

validation_bdcn_deviance <- function(fit, X, WX, time_covariate, season_covariate, Y, rows, d, draw_ids, seed, cfg = CFG) {
    H <- length(rows)
    k <- ncol(X)
    r <- ncol(fit$final_beta)
    mu_sum <- matrix(0, H, k)
    set.seed(seed)
    for (s in draw_ids) {
        alpha <- fit$alpha[s, ]
        beta <- as.numeric(fit$final_beta[s, ])
        L <- matrix(fit$L[s, , , drop = TRUE], nrow = nrow(fit$pairs), ncol = r)
        for (h in seq_along(rows)) {
            tt <- rows[h]
            prior_mean <- beta
            beta_predictive <- prior_mean + rnorm(r)
            Ht <- factor_design_one_time(X[tt, ], fit$pairs, L, k, d)
            baseline <- alpha + fit$trend_coef[s, 1L] * time_covariate[tt] + fit$season_coef[s, 1L] * season_covariate[tt] + 
                fit$self_coef[s, ] * X[tt, ] + fit$w_coef[s, ] * WX[tt, ]
            eta <- baseline + as.numeric(Ht %*% beta_predictive)
            mu_sum[h, ] <- mu_sum[h, ] + exp(clamp(eta, -15, 12))
            r_now <- r_nb_from_previous(Y[max(1L, tt - 1L), ], cfg)
            omega <- pmax(BayesLogit::rpg(k, h = Y[tt, ] + r_now, z = eta - log(r_now)), 1e-10)
            ystar <- log(r_now) + (Y[tt, ] - r_now)/(2 * omega)
            precision <- diag(1, r) + crossprod(Ht, Ht * omega)
            rhs <- prior_mean + crossprod(Ht, omega * (ystar - baseline))
            beta <- rmvn_precision(precision, rhs)
        }
    }
    mu <- mu_sum/length(draw_ids)
    mean(poisson_deviance(Y[rows, , drop = FALSE], mu))
}

run_dss <- function(fit, X_train, time_covariate, season_covariate, X_all, time_all, season_all, Y, validation_rows, signal, 
    cfg = CFG) {
    if (!isTRUE(cfg$dss_common_kappa)) 
        stop("This implementation requires one common DSS kappa across targets.")
    draws <- balanced_indices(fit$chain_id, cfg$dss_max_draws, method_seed("DSS path", signal, cfg))
    k <- ncol(X_train)
    P <- nrow(fit$pairs)
    WX_train <- physical_network_lag(X_train, fit$W)
    objective <- vector("list", k)
    start <- Sys.time()
    for (a in seq_len(k)) {
        ii <- which(fit$pairs$target == a)
        src <- fit$pairs$source[ii]
        m <- length(ii)
        G <- matrix(0, m, m)
        cv <- numeric(m)
        yy <- 0
        for (s in draws) {
            beta <- matrix(fit$beta_path[s, , , drop = TRUE], nrow = nrow(X_train), ncol = ncol(fit$final_beta))
            Ls <- matrix(fit$L[s, ii, , drop = TRUE], nrow = m, ncol = ncol(fit$final_beta))
            U <- (beta %*% t(Ls)) * X_train[, src, drop = FALSE]
            y <- rowSums(U)
            w <- if (cfg$dss_weight == "intensity") 
                exp(clamp(fit$alpha[s, a] + fit$trend_coef[s, 1L] * time_covariate + fit$season_coef[s, 1L] * season_covariate + 
                  fit$self_coef[s, a] * X_train[, a] + fit$w_coef[s, a] * WX_train[, a] + y, -10, 10))
            else rep(1, nrow(X_train))
            Uw <- U * sqrt(w)
            yw <- y * sqrt(w)
            G <- G + crossprod(Uw)
            cv <- cv + as.numeric(crossprod(Uw, yw))
            yy <- yy + sum(yw^2)
        }
        n <- length(draws) * nrow(X_train)
        objective[[a]] <- list(ii = ii, G = G/n, cv = cv/n, yy = yy/n)
        if (a == 1L || a%%max(1L, floor(k/6L)) == 0L || a == k) 
            progress_with_eta(sprintf("build joint DSS path; signal %.2f", signal), a, k, start)
    }
    lmax <- max(vapply(objective, function(z) max(2 * z$cv), numeric(1)), 1e-10)
    lambdas <- exp(seq(log(lmax), log(lmax * cfg$dss_lambda_min_ratio), length.out = cfg$dss_n_lambda))
    D <- matrix(0, P, length(lambdas))
    for (a in seq_len(k)) {
        z <- objective[[a]]
        D[z$ii, ] <- nnlasso_path(z$G, z$cv, lambdas)
    }
    surrogate_numerator <- vapply(seq_along(lambdas), function(j) {
        sum(vapply(objective, function(z) {
            dj <- D[z$ii, j]
            max(0, z$yy - 2 * sum(z$cv * dj) + as.numeric(crossprod(dj, z$G %*% dj)))
        }, numeric(1)))
    }, numeric(1))
    surrogate_denominator <- sum(vapply(objective, `[[`, numeric(1), "yy"))
    surrogate_loss <- surrogate_numerator/(surrogate_denominator + 1e-12)
    candidate_type <- rep("DSS path", length(lambdas))
    if (isTRUE(cfg$dss_include_zero_model)) {
        D <- cbind(numeric(P), D)
        lambdas <- c(Inf, lambdas)
        surrogate_loss <- c(1, surrogate_loss)
        candidate_type <- c("zero dynamic network", candidate_type)
    }
    D <- cbind(D, rep(1, P))
    lambdas <- c(lambdas, 0)
    surrogate_loss <- c(surrogate_loss, 0)
    candidate_type <- c(candidate_type, "continuous full")
    support <- D > cfg$dss_zero_tolerance
    if (isTRUE(cfg$dss_skip_duplicate_supports)) {
        signature <- apply(support, 2L, paste0, collapse = "")
        keep <- !duplicated(signature, fromLast = TRUE)
        D <- D[, keep, drop = FALSE]
        support <- support[, keep, drop = FALSE]
        lambdas <- lambdas[keep]
        surrogate_loss <- surrogate_loss[keep]
        candidate_type <- candidate_type[keep]
    }
    n_selected <- colSums(support)
    ord <- order(n_selected, -lambdas, surrogate_loss)
    D <- D[, ord, drop = FALSE]
    support <- support[, ord, drop = FALSE]
    lambdas <- lambdas[ord]
    surrogate_loss <- surrogate_loss[ord]
    candidate_type <- candidate_type[ord]
    n_selected <- n_selected[ord]
    validation_draw_ids <- balanced_indices(fit$chain_id, cfg$dss_validation_draws, method_seed("DSS validation draw indices", 
        signal, cfg))
    validation_seed <- method_seed("DSS validation common random numbers", signal, cfg)
    WX_all <- physical_network_lag(X_all, fit$W)
    full_loss <- validation_bdcn_deviance(fit, X_all, WX_all, time_all, season_all, Y, validation_rows, rep(1, P), validation_draw_ids, 
        validation_seed, cfg)
    val_loss <- rep(NA_real_, ncol(D))
    evaluated <- rep(FALSE, ncol(D))
    full_id <- which(candidate_type == "continuous full")
    val_loss[full_id] <- full_loss
    evaluated[full_id] <- TRUE
    relative_degradation <- rep(NA_real_, ncol(D))
    relative_degradation[full_id] <- 0
    message(sprintf("[DSS validation; signal %.2f] continuous deviance %.6f", signal, full_loss))
    pick <- full_id[1L]
    sizes <- sort(unique(n_selected))
    evaluation_start <- Sys.time()
    evaluated_count <- 0L
    for (size in sizes) {
        jj <- which(n_selected == size & !evaluated)
        for (j in jj) {
            val_loss[j] <- validation_bdcn_deviance(fit, X_all, WX_all, time_all, season_all, Y, validation_rows, D[, j], 
                validation_draw_ids, validation_seed, cfg)
            evaluated[j] <- TRUE
            evaluated_count <- evaluated_count + 1L
            relative_degradation[j] <- (val_loss[j] - full_loss)/max(full_loss, 1e-12)
            progress_with_eta(sprintf("validate DSS candidates; signal %.2f", signal), evaluated_count, ncol(D) - 1L, evaluation_start)
        }
        passing <- which(n_selected == size & evaluated & relative_degradation <= cfg$dss_predictive_tolerance)
        if (length(passing)) {
            pick <- passing[which.min(val_loss[passing])]
            if (isTRUE(cfg$dss_early_stop)) 
                break
        }
    }
    if (!isTRUE(cfg$dss_early_stop)) {
        passing <- which(evaluated & relative_degradation <= cfg$dss_predictive_tolerance)
        if (length(passing)) {
            min_size <- min(n_selected[passing])
            passing <- passing[n_selected[passing] == min_size]
            pick <- passing[which.min(val_loss[passing])]
        }
    }
    relative_degradation[evaluated & is.na(relative_degradation)] <- (val_loss[evaluated & is.na(relative_degradation)] - 
        full_loss)/max(full_loss, 1e-12)
    d <- D[, pick]
    path <- data.frame(signal = signal, candidate = seq_len(ncol(D)), candidate_type = candidate_type, lambda = lambdas, 
        normalized_network_reconstruction_loss = surrogate_loss, n_selected = n_selected, evaluated = evaluated, validation_poisson_deviance = val_loss, 
        full_validation_poisson_deviance = full_loss, relative_validation_degradation = relative_degradation, predictive_tolerance = cfg$dss_predictive_tolerance, 
        passes_tolerance = evaluated & relative_degradation <= cfg$dss_predictive_tolerance, chosen = seq_len(ncol(D)) == 
            pick)
    selected <- d > cfg$dss_zero_tolerance
    message(sprintf("[DSS validation; signal %.2f] selected %d / %d pairs; deviance %.6f; relative degradation %.4f%%", signal, 
        sum(selected), P, val_loss[pick], 100 * relative_degradation[pick]))
    list(d = d, weighted_d = d, selected = selected, path = path, validation_loss = val_loss[pick], full_validation_loss = full_loss, 
        relative_validation_degradation = relative_degradation[pick], validation_draws = length(validation_draw_ids))
}

network_one_time <- function(x, pairs, L, beta, k, d = NULL) {
    out <- numeric(k)
    ids <- split(seq_len(nrow(pairs)), pairs$target)
    for (a in seq_len(k)) {
        ii <- ids[[as.character(a)]]
        theta <- as.numeric(L[ii, , drop = FALSE] %*% beta)
        if (!is.null(d)) 
            theta <- theta * d[ii]
        out[a] <- sum(theta * x[pairs$source[ii]])
    }
    out
}

factor_design_one_time <- function(x, pairs, L, k, d = NULL) {
    r <- ncol(L)
    H <- matrix(0, k, r)
    ids <- split(seq_len(nrow(pairs)), pairs$target)
    for (a in seq_len(k)) {
        ii <- ids[[as.character(a)]]
        Li <- L[ii, , drop = FALSE]
        if (!is.null(d)) 
            Li <- Li * d[ii]
        H[a, ] <- colSums(Li * x[pairs$source[ii]])
    }
    H
}

r_nb_from_previous <- function(y_previous, cfg = CFG) {
    q <- quantile(pmax(y_previous, 1), cfg$r_nb_quantile, type = 8)
    step <- max(1L, as.integer(cfg$r_nb_round_to))
    clamp(ceiling(ceiling(q/cfg$r_nb_epsilon)/step) * step, cfg$r_nb_min, cfg$r_nb_max)
}

predict_bdcn <- function(fit, X, time_covariate, season_covariate, Y, rows, conditioning_rows = integer(0), d = NULL, label, 
    signal, cfg = CFG) {
    ids <- balanced_indices(fit$chain_id, cfg$predictive_draws, method_seed("prediction", paste(label, signal), cfg))
    H <- length(rows)
    k <- ncol(X)
    WX <- physical_network_lag(X, fit$W)
    lam <- array(NA_real_, c(H, k, length(ids)))
    sequential_rows <- c(conditioning_rows, rows)
    start <- Sys.time()
    report_step <- max(1L, floor(length(ids)/10L))
    set.seed(method_seed("factor forecast", paste(signal, length(ids)), cfg))
    for (j in seq_along(ids)) {
        s <- ids[j]
        alpha <- fit$alpha[s, ]
        trend_coef <- fit$trend_coef[s, 1L]
        season_coef <- fit$season_coef[s, 1L]
        beta <- fit$final_beta[s, ]
        L <- fit$L[s, , , drop = TRUE]
        if (ncol(fit$final_beta) == 1L) 
            L <- matrix(L, ncol = 1L)
        for (tt in sequential_rows) {
            prior_mean <- beta
            beta_predictive <- prior_mean + rnorm(length(beta))
            Ht <- factor_design_one_time(X[tt, ], fit$pairs, L, k, d)
            baseline <- alpha + trend_coef * time_covariate[tt] + season_coef * season_covariate[tt] + fit$self_coef[s, ] * 
                X[tt, ] + fit$w_coef[s, ] * WX[tt, ]
            eta <- baseline + as.numeric(Ht %*% beta_predictive)
            h <- match(tt, rows)
            if (!is.na(h)) 
                lam[h, , j] <- exp(clamp(eta, -15, 12))
            r_now <- r_nb_from_previous(Y[max(1L, tt - 1L), ], cfg)
            zeta <- eta - log(r_now)
            omega <- pmax(BayesLogit::rpg(k, h = Y[tt, ] + r_now, z = zeta), 1e-10)
            ystar <- log(r_now) + (Y[tt, ] - r_now)/(2 * omega)
            precision <- diag(1, length(beta)) + crossprod(Ht, Ht * omega)
            rhs <- prior_mean + crossprod(Ht, omega * (ystar - baseline))
            beta <- rmvn_precision(precision, rhs)
        }
        if (j == 1L || j%%report_step == 0L || j == length(ids)) 
            progress_with_eta(sprintf("predict %s; signal %.2f", label, signal), j, length(ids), start)
    }
    set.seed(method_seed("predictive counts", paste(label, signal), cfg))
    list(label = label, lambda = lam, y = array(rpois(length(lam), lam), dim(lam)))
}

poisson_deviance <- function(y, mu) 2 * (ifelse(y == 0, 0, y * log(y/mu)) - (y - mu))

prediction_metrics <- function(pred, Y, truth, rows, signal, cfg = CFG) {
    obs <- Y[rows, ]
    tru <- truth[rows, ]
    mu <- apply(pred$lambda, c(1, 2), mean)
    yhat <- apply(pred$y, c(1, 2), mean)
    lo <- apply(pred$y, c(1, 2), quantile, 0.025)
    hi <- apply(pred$y, c(1, 2), quantile, 0.975)
    logs <- numeric(length(rows) * ncol(Y))
    z <- 0L
    for (t in seq_along(rows)) for (a in seq_len(ncol(Y))) {
        z <- z + 1L
        logs[z] <- log_mean_exp(dpois(obs[t, a], pred$lambda[t, a, ], log = TRUE))
    }
    data.frame(signal = signal, Model = pred$label, PoissonDeviance = mean(poisson_deviance(obs, mu)), RMSEIntensity = sqrt(mean((mu - 
        tru)^2)), MAECount = mean(abs(yhat - obs)), LogScore = mean(logs), Coverage95 = mean(obs >= lo & obs <= hi), Width95 = mean(hi - 
        lo))
}

