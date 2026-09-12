# Explicit configuration for one complete-panel Bayesian fit. This constructor
# is deterministic; it neither chooses tuning parameters nor starts a chain.
graphmode_config <- function(Y, Fmat, m0, C0, method,
    family = "poisson", dynamics = "dynamic", K = 10L,
    G = NULL, W = NULL, Phi = NULL, graph_weight = NULL,
    guidance = "class-specific", tau = 1, s_b = 1,
    guidance_prior = c(1, 1), guidance_proposal_sd = NULL,
    rho = NULL, variance_prior = NULL, dirichlet_alpha = NULL,
    potts_beta = NULL, max_ess_steps = 1000L) {
    Y <- graphmode_matrix(Y, "Y")
    Fmat <- graphmode_matrix(Fmat, "Fmat")
    K <- gmde_scalar_integer(K, "K", 1L)
    n <- nrow(Y)
    if (ncol(Y) != nrow(Fmat)) stop("Y and Fmat times differ.", call. = FALSE)
    method <- match.arg(method, c("graphMoDE-W", "graphMoDE-C", "EucMoDE", "MoDE", "PottsMoDE"))
    family <- match.arg(family, c("poisson", "gaussian"))
    dynamics <- match.arg(dynamics, c("dynamic", "static"))
    graphmode_dlm(Fmat, m0, C0, G, W, dynamics)
    adaptive <- method %in% c("graphMoDE-W", "graphMoDE-C", "EucMoDE")
    guidance <- match.arg(guidance, c("class-specific", "shared", "none", "forced"))
    if (K == 1L) guidance <- "none"
    if (K == 2L && guidance == "class-specific") guidance <- "shared"
    if (adaptive) {
        graphmode_positive(tau, "tau")
        graphmode_positive(s_b, "s_b")
        if (length(guidance_prior) != 2L || any(!is.finite(guidance_prior)) ||
            any(guidance_prior <= 0)) stop("Invalid Beta prior.", call. = FALSE)
        if (guidance %in% c("shared", "class-specific"))
            graphmode_positive(guidance_proposal_sd, "guidance_proposal_sd")
        if (guidance != "none") {
            Phi <- graphmode_matrix(Phi, "Phi")
            if (nrow(Phi) != n || abs(mean(rowSums(Phi^2)) - tau^2) >
                1e-8 * tau^2) stop("Phi must match n and trace scale tau^2.", call. = FALSE)
        }
    }
    if (family == "poisson") {
        if (any(Y < 0 | Y != round(Y))) stop("Y must contain Poisson counts.", call. = FALSE)
        rho <- gmde_scalar_integer(rho, "rho (independently calibrated)", 1L)
        if (!is.null(variance_prior)) stop("Poisson experts have no variance prior.", call. = FALSE)
    } else {
        if (!is.null(rho)) stop("Gaussian experts do not use rho.", call. = FALSE)
        if (length(variance_prior) != 2L || any(!is.finite(variance_prior)) ||
            any(variance_prior <= 0)) stop("Specify both inverse-Gamma parameters.", call. = FALSE)
    }
    if (method == "MoDE") {
        if (is.null(dirichlet_alpha)) {
            if (K != 10L) stop("Known-dimension validation needs explicit Dirichlet concentrations.", call. = FALSE)
            dirichlet_alpha <- rep(0.1, K)
        }
        if (length(dirichlet_alpha) != K || any(!is.finite(dirichlet_alpha)) ||
            any(dirichlet_alpha <= 0)) stop("Invalid Dirichlet prior.", call. = FALSE)
    }
    if (method == "PottsMoDE") {
        graphmode_positive(potts_beta, "fixed potts_beta", zero = TRUE)
        graphmode_validate_weight(graph_weight)
        if (nrow(graph_weight) != n) stop("Potts graph has wrong n.", call. = FALSE)
    }
    max_ess_steps <- gmde_scalar_integer(max_ess_steps, "max_ess_steps", 1L)
    structure(list(Y = Y, Fmat = Fmat, m0 = as.numeric(m0), C0 = C0,
        method = method, family = family, dynamics = dynamics, K = K,
        G = G, W = W, Phi = Phi, graph_weight = graph_weight, guidance = guidance,
        tau = tau, s_b = s_b, guidance_prior = guidance_prior,
        guidance_proposal_sd = guidance_proposal_sd, rho = rho,
        variance_prior = variance_prior, dirichlet_alpha = dirichlet_alpha,
        potts_beta = potts_beta, max_ess_steps = max_ess_steps, n = n,
        adaptive = adaptive, sampler_version = graphmode_sampler_version,
        protocol = graphmode_protocol), class = "graphmode_config")
}

graphmode_revalidate_config <- function(config) {
    if (!identical(config$sampler_version, graphmode_sampler_version) ||
        !identical(config$protocol, graphmode_protocol)) stop("Incompatible sampler/protocol.", call. = FALSE)
    rebuilt <- do.call(graphmode_config, config[names(formals(graphmode_config))])
    if (!identical(config, rebuilt)) stop("Configuration was changed outside its constructor.", call. = FALSE)
    config
}

graphmode_initial_state <- function(config, Z, theta = NULL, sigma2 = NULL,
                                   x = NULL, v = NULL) {
    graphmode_revalidate_config(config)
    K <- config$K
    TT <- nrow(config$Fmat)
    p <- ncol(config$Fmat)
    Z <- gmde_validate_labels(Z, config$n, K)
    if (is.null(theta)) {
        theta <- array(0, c(K, TT, p))
        for (j in seq_len(p)) theta[, , j] <- config$m0[j]
    }
    if (!identical(dim(theta), c(K, TT, p)) || any(!is.finite(theta)))
        stop("Invalid initial state paths.", call. = FALSE)
    if (config$dynamics == "static") for (k in seq_len(K)) {
        path <- matrix(theta[k, , ], TT, p)
        if (any(sweep(path, 2L, path[1L, ], "-") != 0))
            stop("Static coefficients must be identical over time.", call. = FALSE)
    }
    if (config$family == "gaussian") {
        if (length(sigma2) != K || any(!is.finite(sigma2)) || any(sigma2 <= 0))
            stop("Specify K positive initial variances.", call. = FALSE)
    } else if (!is.null(sigma2)) stop("Poisson state has no sigma2.", call. = FALSE)
    if (config$adaptive) {
        dimension <- graphmode_gate_dimension(config)
        if (is.null(x)) x <- numeric(dimension)
        if (length(x) != dimension || any(!is.finite(x)))
            stop("Invalid whitened gate state.", call. = FALSE)
        free <- if (K == 1L || config$guidance %in% c("none", "forced")) 0L else
            if (config$guidance == "shared") 1L else K
        if (is.null(v)) v <- numeric(free)
        if (length(v) != free) stop("Invalid guidance state length.", call. = FALSE)
        graphmode_gate_utilities(x, v, config)
    } else {
        if (!is.null(x) || !is.null(v)) stop("This method has no Gaussian gate state.", call. = FALSE)
        x <- v <- numeric()
    }
    list(theta = theta, sigma2 = sigma2, Z = Z, x = x, v = v,
         pi = if (config$method == "MoDE") rep(1 / K, K) else NULL,
         iteration = 0L, sampler_version = graphmode_sampler_version)
}

graphmode_revalidate_state <- function(state, config) {
    if (!is.list(state) || !identical(state$sampler_version, graphmode_sampler_version))
        stop("Incompatible initial-state version.", call. = FALSE)
    rebuilt <- graphmode_initial_state(config, state$Z, state$theta, state$sigma2,
        if (config$adaptive) state$x else NULL, if (config$adaptive) state$v else NULL)
    if (!identical(state, rebuilt))
        stop("Use a new initial state; legacy or resumed checkpoints are not accepted.", call. = FALSE)
    state
}

graphmode_sweep <- function(state, config) {
    K <- config$K
    TT <- nrow(config$Fmat)
    p <- ncol(config$Fmat)
    before <- state$Z
    updates <- vector("list", K)
    for (k in seq_len(K)) {
        result <- graphmode_update_expert(matrix(state$theta[k, , ], TT, p),
            config$Y[before == k, , drop = FALSE], config, state$sigma2[k])
        state$theta[k, , ] <- result$theta
        if (config$family == "gaussian") state$sigma2[k] <- result$sigma2
        updates[[k]] <- result
    }
    eta <- gmde_eta(state$theta, config$Fmat)
    response <- graphmode_response_loglik(config$Y, eta, config$family, state$sigma2)
    ess_evaluations <- 0L
    guidance_accept <- logical()
    potts_gross <- NULL
    if (config$adaptive) {
        gate <- graphmode_gate_ess(state$x, state$v, before, config)
        state$x <- gate$x
        ess_evaluations <- gate$evaluations
        guided <- graphmode_guidance_update(state$x, state$v, before, config)
        state$v <- guided$v
        guidance_accept <- guided$accepted
        utilities <- guided$utilities
        state$Z <- graphmode_categorical(response + utilities)
    } else if (config$method == "MoDE") {
        mass <- graphmode_gamma(config$dirichlet_alpha + tabulate(before, K), 1)
        if (any(!is.finite(mass)) || any(mass <= 0) || !is.finite(sum(mass)))
            stop("Dirichlet draw underflow/overflow; no clipping applied.", call. = FALSE)
        state$pi <- mass / sum(mass)
        utilities <- matrix(rep(log(state$pi), each = config$n), config$n, K)
        state$Z <- graphmode_categorical(response + utilities)
    } else {
        allocated <- graphmode_potts_sweep(before, response, config$graph_weight, config$potts_beta)
        state$Z <- allocated$Z
        potts_gross <- allocated$gross
        utilities <- NULL # Potts local conditionals are not mixture weights.
    }
    events <- graphmode_allocation_events(before, state$Z, K)
    state$iteration <- state$iteration + 1L
    sizes <- tabulate(state$Z, K)
    list(state = state, eta = eta, utilities = utilities, sizes = sizes,
        sorted_sizes = sort(sizes, decreasing = TRUE), Kocc = sum(sizes > 0),
        fragmentation = sum(sizes[sizes <= 4L]) / config$n,
        events = events, potts_gross = potts_gross, expert_updates = updates,
        ess_evaluations = ess_evaluations, guidance_accept = guidance_accept)
}

# Label-invariant posterior summary. Representative selection never uses truth.
graphmode_summarize <- function(Z_draws, mean_draws, level = 0.95) {
    Z_draws <- graphmode_matrix(Z_draws, "Z_draws")
    M <- nrow(Z_draws)
    n <- ncol(Z_draws)
    dims <- dim(mean_draws)
    if (length(dims) != 3L || dims[1L] != M || any(dims < 1L) || any(!is.finite(mean_draws)))
        stop("mean_draws must be draw by K by time on the response-mean scale.", call. = FALSE)
    K <- dims[2L]
    TT <- dims[3L]
    graphmode_positive(level, "level")
    if (level >= 1) stop("level must be below one.", call. = FALSE)
    similarity <- matrix(0, n, n)
    size_draws <- matrix(0, M, K)
    unit_profiles <- array(0, c(M, n, TT))
    for (m in seq_len(M)) {
        Z <- gmde_validate_labels(Z_draws[m, ], n, K)
        similarity <- similarity + outer(Z, Z, "==") / M
        size_draws[m, ] <- tabulate(Z, K)
        unit_profiles[m, , ] <- matrix(mean_draws[m, , ], K, TT)[Z, , drop = FALSE]
    }
    loss <- vapply(seq_len(M), function(m)
        sum((outer(Z_draws[m, ], Z_draws[m, ], "==") - similarity)^2), numeric(1))
    representative <- which.min(loss)
    counts <- rowSums(size_draws > 0)
    list(similarity = similarity, representative_index = representative,
         Z = as.integer(Z_draws[representative, ]), Khat = counts[representative],
         Kocc = counts, Pr_Kocc_5 = mean(counts == 5),
         Kocc_probability = tabulate(counts, K) / M,
         sorted_sizes = t(matrix(vapply(seq_len(M), function(m)
             sort(size_draws[m, ], decreasing = TRUE), numeric(K)), K, M)),
         fragmentation = rowSums(size_draws * (size_draws <= 4)) / n,
         unit_mean = apply(unit_profiles, c(2, 3), mean),
         unit_lower = apply(unit_profiles, c(2, 3), stats::quantile, probs = (1 - level) / 2),
         unit_upper = apply(unit_profiles, c(2, 3), stats::quantile, probs = (1 + level) / 2))
}

graphmode_evaluate <- function(summary, truth, true_profile = NULL) {
    truth <- as.integer(factor(truth))
    if (length(truth) != length(summary$Z) || anyNA(truth)) stop("Invalid evaluation truth.", call. = FALSE)
    mask <- upper.tri(summary$similarity)
    true_Kocc <- length(unique(truth))
    out <- list(ARI = gmde_adjusted_rand(truth, summary$Z),
        coclustering_brier = if (any(mask)) mean((summary$similarity[mask] -
            outer(truth, truth, "==")[mask])^2) else NA_real_,
        true_Kocc = true_Kocc, Khat = summary$Khat,
        class_count_absolute_error = abs(summary$Khat - true_Kocc),
        overpartitioned = summary$Khat > true_Kocc,
        underpartitioned = summary$Khat < true_Kocc)
    if (!is.null(true_profile)) {
        if (!identical(dim(true_profile), dim(summary$unit_mean)) || any(!is.finite(true_profile)))
            stop("True profiles must be on the unit by time scale.", call. = FALSE)
        out$profile_rmse <- sqrt(mean((true_profile - summary$unit_mean)^2))
        out$profile_coverage <- mean(true_profile >= summary$unit_lower & true_profile <= summary$unit_upper)
    }
    out
}
