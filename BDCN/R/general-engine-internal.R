# Internal all-pairs/covariate BDCN engine.
# Mechanically migrated from section6_general_extension/run_general_bdcn_extension.R.
# Public validation and bike workflow live in general-api.R.

make_all_pairs <- function(graph, cfg = CFG) {
    k <- nrow(graph$edges)
    H <- hop_matrix(graph$A)
    tab <- expand.grid(target = seq_len(k), source = seq_len(k))
    tab <- tab[order(tab$target, tab$source), , drop = FALSE]
    tab$hop <- H[cbind(tab$target, tab$source)]
    raw <- ifelse(tab$target == tab$source, 1, ifelse(is.finite(tab$hop), cfg$affinity_floor + (1 - cfg$affinity_floor) * 
        exp(-cfg$affinity_decay * tab$hop), cfg$affinity_floor))
    tab$affinity <- exp(log(raw) - mean(log(raw)))
    tab$pair_id <- seq_len(nrow(tab))
    tab$pair <- paste0(graph$edges$name[tab$source], " -> ", graph$edges$name[tab$target])
    tab$pair_class <- ifelse(tab$target == tab$source, "self", ifelse(tab$hop == 1, "one-hop", ifelse(tab$hop == 2, "two-hop", 
        "far/unreachable")))
    rownames(tab) <- NULL
    stopifnot(nrow(tab) == k^2)
    tab
}

baseline_general <- function(alpha, gamma, Q) {
    out <- outer(rep(1, nrow(Q)), alpha)
    if (ncol(Q)) 
        out <- out + Q %*% t(gamma)
    out
}

update_general_block <- function(ystar, X, Q, beta, omega, pairs, prior_var, means, cfg = CFG) {
    k <- ncol(X)
    r <- ncol(beta)
    pQ <- ncol(Q)
    alpha <- numeric(k)
    gamma <- matrix(0, k, pQ)
    L <- matrix(0, nrow(pairs), r)
    ids <- split(seq_len(nrow(pairs)), pairs$target)
    for (a in seq_len(k)) {
        ii <- ids[[as.character(a)]]
        src <- pairs$source[ii]
        Znet <- if (r == 1L) 
            X[, src, drop = FALSE] * beta[, 1L]
        else do.call(cbind, lapply(src, function(s) beta * X[, s]))
        Z <- cbind(Intercept = 1, Q, Znet)
        prec <- c(1/cfg$alpha_prior_sd^2, rep(1/cfg$season_prior_sd^2, pQ), rep(1/prior_var[ii], each = r))
        pm <- c(log(means[a]), rep(0, length(prec) - 1L))
        P <- crossprod(Z, Z * omega[, a]) + diag(prec)
        rhs <- crossprod(Z, omega[, a] * ystar[, a]) + prec * pm
        draw <- rmvn_precision(P, rhs)
        alpha[a] <- draw[1L]
        if (pQ) 
            gamma[a, ] <- draw[1L + seq_len(pQ)]
        L[ii, ] <- matrix(draw[-seq_len(1L + pQ)], ncol = r, byrow = TRUE)
    }
    list(alpha = alpha, gamma = gamma, L = L)
}

fit_general_chain <- function(Y, X, Q, pairs, means, chain_id, signal, cfg = CFG) {
    set.seed(method_seed("general MCMC", paste(signal, chain_id), cfg))
    start <- Sys.time()
    TT <- nrow(Y)
    k <- ncol(Y)
    P <- nrow(pairs)
    r <- cfg$factor_dim
    pQ <- ncol(Q)
    reference <- sqrt((TT + 3)/2)
    alpha <- log(colMeans(Y) + 0.1)
    gamma <- matrix(0, k, pQ)
    beta <- matrix(reference, TT, r)
    L <- matrix(0, P, r)
    ids <- split(seq_len(P), pairs$target)
    for (a in seq_len(k)) {
        ii <- ids[[as.character(a)]]
        Z <- cbind(1, Q, X[, pairs$source[ii], drop = FALSE])
        g <- suppressWarnings(glm.fit(Z, Y[, a], family = poisson()))
        co <- g$coefficients
        co[!is.finite(co)] <- 0
        alpha[a] <- co[1L]
        if (pQ) 
            gamma[a, ] <- co[1L + seq_len(pQ)]
        L[ii, 1L] <- co[-seq_len(1L + pQ)]/reference
    }
    r_nb_t <- choose_r_nb_time(Y, cfg)
    RNB <- matrix(r_nb_t, TT, k)
    active0 <- clamp(cfg$expected_active_pairs, 1, P - 1)
    tau_initial <- max((active0/(P - active0))/(sqrt(TT) * reference), cfg$tau_initial_floor)
    tau_prior_scale <- max(cfg$tau_prior_min_scale, cfg$tau_prior_multiplier * tau_initial)
    u <- rep(1, P)
    v <- tau_initial^2
    nu <- rinvgamma(P, 1, 1 + 1/u)
    xi <- rinvgamma(1, 1, 1/tau_prior_scale^2 + 1/v)
    S <- floor((cfg$n_iter - cfg$burn_mcmc)/cfg$thin)
    alpha_save <- matrix(NA_real_, S, k)
    gamma_save <- array(NA_real_, c(S, k, pQ))
    beta_save <- array(NA_real_, c(S, TT, r))
    final_beta <- matrix(NA_real_, S, r)
    L_save <- array(NA_real_, c(S, P, r))
    amplitude <- theta_average <- matrix(NA_real_, S, P)
    tau_save <- network_rms <- numeric(S)
    si <- 0L
    move_sd <- log(cfg$scale_move_initial_sd)
    accepted <- proposed <- 0L
    for (iter in seq_len(cfg$n_iter)) {
        H <- build_network_array(X, pairs, L, r, k)
        base <- baseline_general(alpha, gamma, Q)
        eta <- base + network_contribution(H, beta)
        zeta <- as.numeric(eta) - log(as.numeric(RNB))
        omega <- matrix(BayesLogit::rpg(length(zeta), h = as.numeric(Y) + as.numeric(RNB), z = zeta), TT, k)
        omega <- pmax(omega, 1e-10)
        ystar <- log(RNB) + (Y - RNB)/(2 * omega)
        prior_var <- pmax(v * u * pairs$affinity^2, 1e-12)
        for (ss in seq_len(cfg$state_block_substeps)) {
            H <- build_network_array(X, pairs, L, r, k)
            beta <- ffbs_factor_identified(ystar - baseline_general(alpha, gamma, Q), H, omega)
            block <- update_general_block(ystar, X, Q, beta, omega, pairs, prior_var, means, cfg)
            alpha <- block$alpha
            gamma <- block$gamma
            L <- block$L
        }
        L2 <- rowSums(L^2)
        for (ss in seq_len(cfg$shrinkage_substeps)) {
            u <- clamp(rinvgamma(P, (r + 1)/2, 1/nu + L2/(2 * v * pairs$affinity^2)), 1e-12, 1e+12)
            nu <- clamp(rinvgamma(P, 1, 1 + 1/u), 1e-12, 1e+12)
            v <- clamp(rinvgamma(1, (r * P + 1)/2, 1/xi + 0.5 * sum(L2/(u * pairs$affinity^2))), 1e-12, 1e+06)
            xi <- clamp(rinvgamma(1, 1, 1/tau_prior_scale^2 + 1/v), 1e-12, 1e+12)
        }
        move <- joint_horseshoe_scale_move(v, u, tau_prior_scale, exp(move_sd), cfg$scale_move_steps)
        v <- move$v
        u <- move$u
        accepted <- accepted + move$accepted
        proposed <- proposed + cfg$scale_move_steps
        nu <- clamp(rinvgamma(P, 1, 1 + 1/u), 1e-12, 1e+12)
        xi <- clamp(rinvgamma(1, 1, 1/tau_prior_scale^2 + 1/v), 1e-12, 1e+12)
        if (iter <= cfg$burn_mcmc) {
            gain <- cfg$scale_move_adapt_rate/sqrt(iter)
            move_sd <- clamp(move_sd + gain * (move$accepted/cfg$scale_move_steps - cfg$scale_move_target_acceptance), log(cfg$scale_move_min_sd), 
                log(cfg$scale_move_max_sd))
        }
        if (iter > cfg$burn_mcmc && (iter - cfg$burn_mcmc)%%cfg$thin == 0L) {
            si <- si + 1L
            alpha_save[si, ] <- alpha
            if (pQ) 
                gamma_save[si, , ] <- gamma
            beta_save[si, , ] <- beta
            final_beta[si, ] <- beta[TT, ]
            L_save[si, , ] <- L
            th <- L %*% t(beta)
            amplitude[si, ] <- sqrt(rowMeans(th^2))
            theta_average[si, ] <- rowMeans(th)
            tau_save[si] <- sqrt(v)
            network_rms[si] <- sqrt(mean(network_contribution(build_network_array(X, pairs, L, r, k), beta)^2))
        }
        if (iter == 1L || iter%%cfg$progress_every == 0L || iter == cfg$n_iter) 
            progress_with_eta(sprintf("general BDCN; q=%.2f; chain %d", signal, chain_id), iter, cfg$n_iter, start)
    }
    list(chain_id = chain_id, alpha = alpha_save, gamma = gamma_save, beta_path = beta_save, final_beta = final_beta, L = L_save, 
        amplitude = amplitude, theta_average = theta_average, tau = tau_save, network_rms = network_rms, r_nb_t = r_nb_t, 
        tau_initial = tau_initial, tau_prior_scale = tau_prior_scale, scale_move_acceptance = accepted/proposed, elapsed = as.numeric(difftime(Sys.time(), 
            start, units = "secs")))
}

fit_general_bdcn <- function(Y, X, Q, pairs, means, signal, cfg = CFG) {
    start <- Sys.time()
    ids <- seq_len(cfg$n_chains)
    worker <- function(ch) fit_general_chain(Y, X, Q, pairs, means, ch, signal, cfg)
    cores <- max(1L, parallel::detectCores(logical = FALSE))
    chains <- if (cfg$parallel_chains && .Platform$OS.type != "windows") 
        parallel::mclapply(ids, worker, mc.cores = min(length(ids), cores))
    else lapply(ids, worker)
    list(label = "BDCN continuous, all ordered pairs", pairs = pairs, chains = chains, chain_id = unlist(lapply(chains, function(z) rep(z$chain_id, 
        nrow(z$alpha)))), alpha = do.call(rbind, lapply(chains, `[[`, "alpha")), gamma = bind_array3(lapply(chains, `[[`, 
        "gamma")), final_beta = do.call(rbind, lapply(chains, `[[`, "final_beta")), beta_path = bind_array3(lapply(chains, 
        `[[`, "beta_path")), L = bind_array3(lapply(chains, `[[`, "L")), amplitude = do.call(rbind, lapply(chains, `[[`, 
        "amplitude")), theta_average = do.call(rbind, lapply(chains, `[[`, "theta_average")), tau = unlist(lapply(chains, 
        `[[`, "tau")), network_rms = unlist(lapply(chains, `[[`, "network_rms")), r_nb_t = chains[[1L]]$r_nb_t, elapsed = as.numeric(difftime(Sys.time(), 
        start, units = "secs")))
}

predict_general_bdcn <- function(fit, X, Q, Y, rows, conditioning_rows = integer(0), d = NULL, label = fit$label, signal, 
    cfg = CFG) {
    ids <- balanced_indices(fit$chain_id, cfg$predictive_draws, method_seed("general prediction", paste(label, signal), cfg))
    Hn <- length(rows)
    k <- ncol(X)
    r <- ncol(fit$final_beta)
    lam <- array(NA_real_, c(Hn, k, length(ids)))
    sequential <- c(conditioning_rows, rows)
    set.seed(method_seed("general factor forecast", paste(label, signal), cfg))
    for (jj in seq_along(ids)) {
        s <- ids[jj]
        alpha <- fit$alpha[s, ]
        gamma <- fit$gamma[s, , , drop = TRUE]
        if (ncol(Q) == 1L) 
            gamma <- matrix(gamma, ncol = 1L)
        beta <- fit$final_beta[s, ]
        L <- fit$L[s, , , drop = TRUE]
        if (r == 1L) 
            L <- matrix(L, ncol = 1L)
        for (tt in sequential) {
            prior <- beta
            bp <- prior + rnorm(r)
            Ht <- factor_design_one_time(X[tt, ], fit$pairs, L, k, d)
            base <- alpha + if (ncol(Q)) 
                as.numeric(gamma %*% Q[tt, ])
            else 0
            eta <- base + as.numeric(Ht %*% bp)
            h <- match(tt, rows)
            if (!is.na(h)) 
                lam[h, , jj] <- exp(clamp(eta, -15, 12))
            rn <- r_nb_from_previous(Y[max(1L, tt - 1L), ], cfg)
            om <- pmax(BayesLogit::rpg(k, h = Y[tt, ] + rn, z = eta - log(rn)), 1e-10)
            ys <- log(rn) + (Y[tt, ] - rn)/(2 * om)
            precision <- diag(1, r) + crossprod(Ht, Ht * om)
            beta <- rmvn_precision(precision, prior + crossprod(Ht, om * (ys - base)))
        }
    }
    set.seed(method_seed("general predictive counts", paste(label, signal), cfg))
    list(label = label, lambda = lam, y = array(rpois(length(lam), lam), dim(lam)))
}

validation_general_deviance <- function(fit, X, Q, Y, rows, d, draw_ids, seed, cfg = CFG) {
    k <- ncol(X)
    r <- ncol(fit$final_beta)
    mu <- matrix(0, length(rows), k)
    set.seed(seed)
    for (s in draw_ids) {
        alpha <- fit$alpha[s, ]
        gamma <- fit$gamma[s, , , drop = TRUE]
        if (ncol(Q) == 1L) 
            gamma <- matrix(gamma, ncol = 1L)
        beta <- fit$final_beta[s, ]
        L <- fit$L[s, , , drop = TRUE]
        if (r == 1L) 
            L <- matrix(L, ncol = 1L)
        for (h in seq_along(rows)) {
            tt <- rows[h]
            prior <- beta
            bp <- prior + rnorm(r)
            Ht <- factor_design_one_time(X[tt, ], fit$pairs, L, k, d)
            base <- alpha + if (ncol(Q)) 
                as.numeric(gamma %*% Q[tt, ])
            else 0
            eta <- base + as.numeric(Ht %*% bp)
            mu[h, ] <- mu[h, ] + exp(clamp(eta, -15, 12))
            rn <- r_nb_from_previous(Y[max(1L, tt - 1L), ], cfg)
            om <- pmax(BayesLogit::rpg(k, h = Y[tt, ] + rn, z = eta - log(rn)), 1e-10)
            ys <- log(rn) + (Y[tt, ] - rn)/(2 * om)
            beta <- rmvn_precision(diag(1, r) + crossprod(Ht, Ht * om), prior + crossprod(Ht, om * (ys - base)))
        }
    }
    mean(poisson_deviance(Y[rows, , drop = FALSE], mu/length(draw_ids)))
}

run_general_dss <- function(fit, Xtrain, Qtrain, Xall, Qall, Y, validation, signal, cfg = CFG) {
    draws <- balanced_indices(fit$chain_id, cfg$dss_max_draws, method_seed("general DSS", signal, cfg))
    k <- ncol(Xtrain)
    P <- nrow(fit$pairs)
    objective <- vector("list", k)
    for (a in seq_len(k)) {
        ii <- which(fit$pairs$target == a)
        src <- fit$pairs$source[ii]
        m <- length(ii)
        G <- matrix(0, m, m)
        cv <- numeric(m)
        yy <- 0
        for (s in draws) {
            beta <- matrix(fit$beta_path[s, , , drop = TRUE], nrow(Xtrain), ncol(fit$final_beta))
            Ls <- matrix(fit$L[s, ii, , drop = TRUE], m, ncol(fit$final_beta))
            U <- (beta %*% t(Ls)) * Xtrain[, src, drop = FALSE]
            ynet <- rowSums(U)
            gamma <- fit$gamma[s, a, , drop = TRUE]
            base <- fit$alpha[s, a] + if (ncol(Qtrain)) 
                as.numeric(Qtrain %*% gamma)
            else 0
            w <- if (cfg$dss_weight == "intensity") 
                exp(clamp(base + ynet, -10, 10))
            else rep(1, nrow(Xtrain))
            Uw <- U * sqrt(w)
            yw <- ynet * sqrt(w)
            G <- G + crossprod(Uw)
            cv <- cv + as.numeric(crossprod(Uw, yw))
            yy <- yy + sum(yw^2)
        }
        n <- length(draws) * nrow(Xtrain)
        objective[[a]] <- list(ii = ii, G = G/n, cv = cv/n, yy = yy/n)
    }
    lmax <- max(vapply(objective, function(z) max(2 * z$cv), numeric(1)), 1e-10)
    lambdas <- exp(seq(log(lmax), log(lmax * cfg$dss_lambda_min_ratio), length.out = cfg$dss_n_lambda))
    D <- matrix(0, P, length(lambdas))
    for (a in seq_len(k)) {
        z <- objective[[a]]
        D[z$ii, ] <- nnlasso_path(z$G, z$cv, lambdas)
    }
    D <- cbind(numeric(P), D, rep(1, P))
    lambdas <- c(Inf, lambdas, 0)
    type <- c("zero network", rep("DSS path", cfg$dss_n_lambda), "continuous full")
    support <- D > cfg$dss_zero_tolerance
    sig <- apply(support, 2L, paste0, collapse = "")
    keep <- !duplicated(sig, fromLast = TRUE)
    D <- D[, keep, drop = FALSE]
    lambdas <- lambdas[keep]
    type <- type[keep]
    support <- support[, keep, drop = FALSE]
    sizes <- colSums(support)
    ord <- order(sizes, -lambdas)
    D <- D[, ord, drop = FALSE]
    lambdas <- lambdas[ord]
    type <- type[ord]
    sizes <- sizes[ord]
    draw_ids <- balanced_indices(fit$chain_id, cfg$dss_validation_draws, method_seed("general DSS validation draws", signal, 
        cfg))
    seed <- method_seed("general DSS validation CRN", signal, cfg)
    full <- validation_general_deviance(fit, Xall, Qall, Y, validation, rep(1, P), draw_ids, seed, cfg)
    loss <- rep(NA_real_, ncol(D))
    full_id <- which(type == "continuous full")
    loss[full_id] <- full
    pick <- full_id[1L]
    for (size in sort(unique(sizes))) {
        jj <- which(sizes == size & seq_along(sizes) != full_id)
        for (j in jj) loss[j] <- validation_general_deviance(fit, Xall, Qall, Y, validation, D[, j], draw_ids, seed, cfg)
        passing <- jj[(loss[jj] - full)/max(full, 1e-12) <= cfg$dss_predictive_tolerance]
        if (length(passing)) {
            pick <- passing[which.min(loss[passing])]
            break
        }
    }
    rel <- (loss - full)/max(full, 1e-12)
    path <- data.frame(signal = signal, candidate = seq_len(ncol(D)), candidate_type = type, lambda = lambdas, n_selected = sizes, 
        validation_poisson_deviance = loss, relative_validation_degradation = rel, predictive_tolerance = cfg$dss_predictive_tolerance, 
        chosen = seq_len(ncol(D)) == pick)
    list(d = D[, pick], selected = support[, pick], path = path, full_validation_loss = full, validation_loss = loss[pick], 
        relative_validation_degradation = rel[pick])
}

basic_convergence <- function(fit, signal, cfg = CFG) {
    quantities <- list()
    types <- character(0)
    add_vector <- function(name, type, value) {
        quantities[[name]] <<- value
        types[name] <<- type
    }
    add_matrix <- function(x, type, labels) for (j in seq_len(ncol(x))) add_vector(labels[j], type, x[, j])
    add_vector("tau", "tau", fit$tau)
    add_vector("network_RMS", "network_RMS", fit$network_rms)
    add_matrix(fit$alpha, "alpha", paste0("alpha[e", seq_len(ncol(fit$alpha)), "]"))
    if (dim(fit$gamma)[3L]) {
        for (a in seq_len(dim(fit$gamma)[2L])) for (j in seq_len(dim(fit$gamma)[3L])) add_vector(sprintf("gamma[e%d,h%d]", 
            a, j), "Fourier_gamma", fit$gamma[, a, j])
    }
    pair_names <- fit$pairs$pair
    add_matrix(fit$amplitude, "trajectory_amplitude", paste0("amplitude[", pair_names, "]"))
    add_matrix(fit$theta_average, "mean_theta", paste0("mean_theta[", pair_names, "]"))
    out <- lapply(names(quantities), function(nm) {
        z <- quantities[[nm]]
        mat <- do.call(cbind, lapply(split(z, fit$chain_id), identity))
        rhat <- split_rhat(mat)
        ess <- acf_ess(mat)
        data.frame(signal = signal, ParameterType = unname(types[nm]), Parameter = nm, SplitRhat = rhat, ESS = ess, PassRhat = is.finite(rhat) && 
            rhat <= cfg$mcmc_rhat_threshold, PassESS = is.finite(ess) && ess >= cfg$mcmc_ess_threshold)
    })
    do.call(rbind, out)
}

