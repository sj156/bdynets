# Exact-target GMDE sampler for expert-specific state dimensions.

gmde_expand_expert_design <- function(Fk, TT) {
    Fk <- as.matrix(Fk)
    if (nrow(Fk) == 1L) {
        Fk <- matrix(
            rep(as.numeric(Fk), TT),
            nrow = TT,
            byrow = TRUE
        )
    }
    if (nrow(Fk) != TT || ncol(Fk) < 1L || any(!is.finite(Fk))) {
        stop(
            "Each expert design must have one or TT finite rows.",
            call. = FALSE
        )
    }
    Fk
}

gmde_heterogeneous_eta <- function(theta, Fmat_list) {
    K <- length(theta)
    TT <- nrow(Fmat_list[[1L]])
    eta <- matrix(NA_real_, nrow = K, ncol = TT)
    for (k in seq_len(K)) {
        theta_k <- as.matrix(theta[[k]])
        if (!identical(dim(theta_k), dim(Fmat_list[[k]])) ||
            any(!is.finite(theta_k))) {
            stop(
                "An expert path does not match its design matrix.",
                call. = FALSE
            )
        }
        eta[k, ] <- rowSums(Fmat_list[[k]] * theta_k)
    }
    eta
}

gmde_order_initial_allocations <- function(Z, Y, K) {
    Z <- gmde_validate_labels(Z, nrow(Y), K, "Z_init")
    cluster_level <- vapply(
        seq_len(K),
        function(k) {
            index <- which(Z == k)
            if (!length(index)) return(Inf)
            mean(Y[index, , drop = FALSE])
        },
        numeric(1)
    )
    order_index <- order(cluster_level, seq_len(K))
    old_to_new <- integer(K)
    old_to_new[order_index] <- seq_len(K)
    old_to_new[Z]
}

gmde_update_heterogeneous_paths <- function(
    Y,
    Z,
    theta,
    Fmat_list,
    m0_list,
    C0_list,
    G_list,
    W_list,
    rho,
    pg_backend,
    pg_trunc
) {
    K <- length(theta)
    TT <- ncol(Y)
    stats <- gmde_cluster_stats(Z, Y, K)
    updated <- theta
    accepted <- rep(NA, K)
    log_acceptance <- rep(NA_real_, K)
    movement <- numeric(K)
    elapsed <- numeric(K)
    pg_elapsed <- numeric(K)
    pg_shape_sum <- rep(NA_real_, K)
    pg_shape_max <- rep(NA_real_, K)

    for (k in seq_len(K)) {
        if (stats$N[k] == 0L) {
            started <- proc.time()[[3L]]
            updated[[k]] <- gmde_sample_prior_path(
                TT, m0_list[[k]], C0_list[[k]], G_list[[k]], W_list[[k]]
            )
            elapsed[k] <- max(
                proc.time()[[3L]] - started, .Machine$double.eps
            )
            next
        }
        result <- gmde_update_state_path(
            current = theta[[k]],
            N = stats$N[k],
            S = stats$S[k, ],
            rho = rho,
            Fmat = Fmat_list[[k]],
            m0 = m0_list[[k]],
            C0 = C0_list[[k]],
            G = G_list[[k]],
            W = W_list[[k]],
            pg_backend = pg_backend,
            pg_trunc = pg_trunc
        )
        updated[[k]] <- result$theta
        accepted[k] <- result$accepted
        log_acceptance[k] <- result$log_acceptance
        movement[k] <- result$accepted_movement
        elapsed[k] <- result$elapsed_seconds
        pg_elapsed[k] <- result$pg_elapsed_seconds
        pg_shape_sum[k] <- result$pg_shape_sum
        pg_shape_max[k] <- result$pg_shape_max
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
        pg_shape_max = pg_shape_max
    )
}

#' Run GMDE with expert-specific state dimensions
#'
#' Each expert may have its own design matrix, initial moments, evolution
#' matrix, and evolution covariance.  Because those specifications identify
#' the expert labels, the sampler never performs intensity-order relabeling.
#'
#' @param Y Location-by-time count matrix.
#' @param F_list List of one-row or time-by-state expert design matrices.
#' @param Phi Variance-calibrated graph basis.
#' @param K Number of experts.
#' @param n_iter Total MCMC iterations.
#' @param burn Initial discarded iterations.
#' @param m0_list,C0_list,G_list,W_list Expert-specific DLM inputs.
#' @param rho Fixed positive integer multiplier.  Alternatively supply the
#'   explicit calibration controls.
#' @param rho_grid,rho_warmup,rho_tie_break,rho_schedule Explicit rho
#'   calibration controls matching those of `run_gmde_mcmc()`.
#' @param r_tune Retired legacy argument; a non-NULL value fails closed.
#' @param Z_init Optional initial allocations.
#' @param beta_init,gamma_init Optional classifier initial values.
#' @param theta_init Optional list of initial state paths.
#' @param order_initial_labels Whether to order initial data-driven groups once.
#' @param Z_true Optional true labels used only for diagnostics.
#' @param substantive_min Minimum reported substantive expert size.
#' @param pg_backend Exact `devroye-exact` or approximate `truncated` backend.
#' @param pg_trunc Truncation used only by the approximate backend.
#' @param graph_meta Optional graph provenance metadata.
#' @param store_prediction_state Whether to retain post-burn terminal states.
#' @param max_ess_steps Maximum joint-ESS bracket evaluations.
#' @param print_freq Progress interval; zero suppresses output.
#' @param seed Optional random seed.
#' @return An object of class `gmde_heterogeneous_mcmc`.
#' @export
run_gmde_mcmc_heterogeneous <- function(
    Y,
    F_list,
    Phi,
    K,
    n_iter = 1500L,
    burn = floor(n_iter / 2),
    m0_list,
    C0_list,
    G_list,
    W_list,
    rho = NULL,
    rho_grid = NULL,
    rho_warmup = NULL,
    rho_tie_break = NULL,
    rho_schedule = NULL,
    r_tune = NULL,
    Z_init = NULL,
    beta_init = NULL,
    gamma_init = NULL,
    theta_init = NULL,
    order_initial_labels = FALSE,
    Z_true = NULL,
    substantive_min = 10L,
    pg_backend = c("devroye-exact", "truncated"),
    pg_trunc = 80L,
    graph_meta = NULL,
    store_prediction_state = TRUE,
    max_ess_steps = 1000L,
    print_freq = 100L,
    seed = NULL
) {
    if (!is.null(r_tune)) {
        stop(
            "r_tune belongs to the frozen approximate sampler; use rho.",
            call. = FALSE
        )
    }
    pg_backend <- match.arg(pg_backend)
    if (!is.null(seed)) set.seed(seed)
    Y <- as.matrix(Y)
    Phi <- as.matrix(Phi)
    n <- nrow(Y)
    TT <- ncol(Y)
    K <- gmde_scalar_integer(K, "K", lower = 2L, upper = n)
    n_iter <- gmde_scalar_integer(n_iter, "n_iter", lower = 2L)
    burn <- gmde_scalar_integer(burn, "burn", lower = 0L)
    substantive_min <- gmde_scalar_integer(
        substantive_min, "substantive_min", lower = 1L, upper = n
    )
    max_ess_steps <- gmde_scalar_integer(
        max_ess_steps, "max_ess_steps", lower = 1L
    )
    print_freq <- gmde_scalar_integer(
        print_freq, "print_freq", lower = 0L
    )
    if (burn >= n_iter || TT < 2L || any(!is.finite(Y)) ||
        any(Y < 0) || any(Y != round(Y))) {
        stop(
            "Y must be an integer count matrix and 0 <= burn < n_iter.",
            call. = FALSE
        )
    }
    if (nrow(Phi) != n || ncol(Phi) < 1L || any(!is.finite(Phi))) {
        stop("Phi must be a finite n by m matrix.", call. = FALSE)
    }
    if (!is.null(Z_true)) {
        if (!is.numeric(Z_true) || length(Z_true) != n ||
            any(!is.finite(Z_true)) || any(Z_true != round(Z_true)) ||
            length(unique(Z_true)) > K || K > 15L) {
            stop(
                "Z_true must contain n whole-number labels and at most K ",
                "classes; metrics currently support K <= 15.",
                call. = FALSE
            )
        }
        Z_true <- as.integer(Z_true)
    }
    expert_lists <- list(F_list, m0_list, C0_list, G_list, W_list)
    if (any(vapply(expert_lists, length, integer(1)) != K)) {
        stop("Every expert-specific list must have length K.", call. = FALSE)
    }

    Fmat_list <- lapply(F_list, gmde_expand_expert_design, TT = TT)
    p_k <- vapply(m0_list, length, integer(1))
    for (k in seq_len(K)) {
        p <- p_k[k]
        m0_list[[k]] <- as.numeric(m0_list[[k]])
        C0_list[[k]] <- as.matrix(C0_list[[k]])
        G_list[[k]] <- as.matrix(G_list[[k]])
        W_list[[k]] <- as.matrix(W_list[[k]])
        if (p < 1L || ncol(Fmat_list[[k]]) != p ||
            !identical(dim(C0_list[[k]]), c(p, p)) ||
            !identical(dim(G_list[[k]]), c(p, p)) ||
            !identical(dim(W_list[[k]]), c(p, p)) ||
            any(!is.finite(c(
                m0_list[[k]], C0_list[[k]], G_list[[k]], W_list[[k]]
            )))) {
            stop(
                "Expert ", k, " has incompatible DLM dimensions or values.",
                call. = FALSE
            )
        }
        gmde_chol_spd(C0_list[[k]], paste0("C0_list[[", k, "]]"))
        gmde_chol_spd(W_list[[k]], paste0("W_list[[", k, "]]"))
    }

    state_control <- gmde_validate_rho_control(
        rho, rho_grid, rho_warmup, rho_tie_break, rho_schedule, burn
    )
    gmde_validate_pg_control(pg_backend, state_control)
    H <- gmde_helmert_contrast(K)
    m <- ncol(Phi)

    Z <- if (is.null(Z_init)) {
        gmde_initialize_allocations(Y, K)
    } else gmde_validate_labels(Z_init, n, K, "Z_init")
    if (isTRUE(order_initial_labels) &&
        (!is.null(theta_init) || !is.null(beta_init) ||
         !is.null(gamma_init))) {
        stop(
            "Automatic initial-label ordering cannot be combined with ",
            "label-indexed theta/classifier initial values.",
            call. = FALSE
        )
    }
    if (isTRUE(order_initial_labels)) {
        Z <- gmde_order_initial_allocations(Z, Y, K)
    } else {
        Z <- gmde_validate_labels(Z, n, K, "Z_init")
    }

    if (is.null(theta_init)) {
        theta <- lapply(seq_len(K), function(k) {
            gmde_sample_prior_path(
                TT, m0_list[[k]], C0_list[[k]], G_list[[k]], W_list[[k]]
            )
        })
    } else {
        if (!is.list(theta_init) || length(theta_init) != K) {
            stop("theta_init must be NULL or a list of length K.",
                 call. = FALSE)
        }
        theta <- lapply(theta_init, as.matrix)
    }
    gmde_heterogeneous_eta(theta, Fmat_list)

    if (!is.null(beta_init) && !is.null(gamma_init)) {
        stop("Supply at most one of beta_init and gamma_init.", call. = FALSE)
    }
    if (!is.null(beta_init)) {
        beta_init <- as.matrix(beta_init)
        if (!identical(dim(beta_init), c(m, K)) ||
            any(!is.finite(beta_init))) {
            stop("beta_init must be a finite m by K matrix.",
                 call. = FALSE)
        }
        beta_init <- sweep(beta_init, 1L, rowMeans(beta_init), "-")
        gamma <- beta_init %*% H
    } else if (!is.null(gamma_init)) {
        gamma <- as.matrix(gamma_init)
        if (!identical(dim(gamma), c(m, K - 1L)) ||
            any(!is.finite(gamma))) {
            stop("gamma_init must be a finite m by K - 1 matrix.",
                 call. = FALSE)
        }
    } else {
        gamma <- matrix(stats::rnorm(m * (K - 1L)), m, K - 1L)
    }

    initialization <- list(
        model = "gmde-heterogeneous",
        Z = as.integer(Z),
        theta = theta,
        classifier = gamma
    )

    Z_store <- matrix(NA_integer_, n_iter, n)
    size_store <- matrix(NA_integer_, n_iter, K)
    mean_lambda_store <- matrix(NA_real_, n_iter, K)
    lambda_store <- array(NA_real_, c(n_iter, K, TT))
    loglik_store <- numeric(n_iter)
    observed_loglik_store <- numeric(n_iter)
    classifier_trace <- matrix(NA_real_, n_iter, K)
    mean_pi_store <- matrix(NA_real_, n_iter, K)
    occupied_store <- integer(n_iter)
    substantive_store <- integer(n_iter)
    ari_store <- rep(NA_real_, n_iter)
    acc_store <- rep(NA_real_, n_iter)
    state_accepted <- matrix(NA, n_iter, K)
    state_log_acceptance <- matrix(NA_real_, n_iter, K)
    state_movement <- matrix(0, n_iter, K)
    state_update_seconds <- matrix(0, n_iter, K)
    state_pg_seconds <- matrix(0, n_iter, K)
    state_pg_shape_sum <- matrix(NA_real_, n_iter, K)
    state_pg_shape_max <- matrix(NA_real_, n_iter, K)
    state_rho <- numeric(n_iter)
    ess_bracket_evaluations <- integer(n_iter)
    ess_likelihood_evaluations <- integer(n_iter)
    postburn_draws <- n_iter - burn
    theta_terminal <- if (isTRUE(store_prediction_state)) {
        lapply(p_k, function(p) matrix(NA_real_, postburn_draws, p))
    } else NULL
    beta_store <- if (isTRUE(store_prediction_state)) {
        array(NA_real_, c(postburn_draws, m, K))
    } else NULL
    gamma_store <- if (isTRUE(store_prediction_state)) {
        array(NA_real_, c(postburn_draws, m, K - 1L))
    } else NULL

    calibration_movement <- numeric(length(state_control$grid))
    calibration_seconds <- numeric(length(state_control$grid))
    calibration_proposals <- integer(length(state_control$grid))
    calibration_accepts <- integer(length(state_control$grid))
    selected_rho <- state_control$selected
    started <- proc.time()[[3L]]

    for (iter in seq_len(n_iter)) {
        rho_iter <- if (state_control$mode == "fixed" ||
            iter > state_control$warmup) {
            selected_rho
        } else state_control$schedule[iter]
        state_rho[iter] <- rho_iter
        state_result <- gmde_update_heterogeneous_paths(
            Y, Z, theta, Fmat_list, m0_list, C0_list, G_list, W_list,
            rho_iter, pg_backend, pg_trunc
        )
        theta <- state_result$theta
        state_accepted[iter, ] <- state_result$accepted
        state_log_acceptance[iter, ] <- state_result$log_acceptance
        state_movement[iter, ] <- state_result$accepted_movement
        state_update_seconds[iter, ] <- state_result$elapsed_seconds
        state_pg_seconds[iter, ] <- state_result$pg_elapsed_seconds
        state_pg_shape_sum[iter, ] <- state_result$pg_shape_sum
        state_pg_shape_max[iter, ] <- state_result$pg_shape_max

        if (state_control$mode == "warmup-grid" &&
            iter <= state_control$warmup) {
            grid_index <- match(rho_iter, state_control$grid)
            nonempty <- !is.na(state_result$accepted)
            calibration_movement[grid_index] <-
                calibration_movement[grid_index] +
                sum(state_result$accepted_movement[nonempty])
            calibration_seconds[grid_index] <-
                calibration_seconds[grid_index] +
                sum(state_result$elapsed_seconds[nonempty])
            calibration_proposals[grid_index] <-
                calibration_proposals[grid_index] + sum(nonempty)
            calibration_accepts[grid_index] <-
                calibration_accepts[grid_index] +
                sum(state_result$accepted[nonempty])
            if (iter == state_control$warmup) {
                score <- calibration_movement / calibration_seconds
                selected_rho <- gmde_select_calibrated_rho(
                    state_control$grid, score, state_control$tie_break
                )
                state_control$selected <- selected_rho
            }
        }

        eta <- gmde_heterogeneous_eta(theta, Fmat_list)
        classifier_result <- gmde_joint_ess(
            gamma, Phi, Z, H, max_bracket_steps = max_ess_steps
        )
        gamma <- classifier_result$gamma
        utilities <- classifier_result$utilities
        ess_bracket_evaluations[iter] <-
            classifier_result$bracket_evaluations
        ess_likelihood_evaluations[iter] <-
            classifier_result$likelihood_evaluations
        allocation <- gmde_allocation_probabilities(Y, eta, utilities)
        Z <- gmde_sample_categorical_rows(allocation$probability)

        lambda <- exp(eta)
        if (any(!is.finite(lambda))) {
            stop("A heterogeneous expert intensity is non-finite.",
                 call. = FALSE)
        }
        probability <- gmde_softmax_rows(utilities)
        Z_store[iter, ] <- Z
        size_store[iter, ] <- tabulate(Z, nbins = K)
        occupied_store[iter] <- sum(size_store[iter, ] > 0L)
        substantive_store[iter] <-
            sum(size_store[iter, ] >= substantive_min)
        mean_lambda_store[iter, ] <- rowMeans(lambda)
        lambda_store[iter, , ] <- lambda
        mean_pi_store[iter, ] <- colMeans(probability)
        loglik_store[iter] <- gmde_complete_loglik(Y, eta, utilities, Z)
        observed_loglik_store[iter] <-
            gmde_observed_loglik(Y, eta, utilities)
        beta <- gamma %*% t(H)
        classifier_trace[iter, ] <- sqrt(colSums(beta^2))
        if (!is.null(Z_true)) {
            ari_store[iter] <- gmde_adjusted_rand(Z_true, Z)
            acc_store[iter] <- gmde_best_label_accuracy(Z_true, Z, K)
        }
        if (isTRUE(store_prediction_state) && iter > burn) {
            draw_index <- iter - burn
            for (k in seq_len(K)) {
                theta_terminal[[k]][draw_index, ] <- theta[[k]][TT, ]
            }
            beta_store[draw_index, , ] <- beta
            gamma_store[draw_index, , ] <- gamma
        }
        if (print_freq > 0L && iter %% print_freq == 0L) {
            cat(
                "GMDE heterogeneous iteration", iter,
                "| observed loglik =", round(observed_loglik_store[iter], 1),
                "| rho =", format(rho_iter), "\n"
            )
        }
    }

    calibration <- data.frame(
        rho = state_control$grid,
        accepted_movement = calibration_movement,
        elapsed_seconds = calibration_seconds,
        proposals = calibration_proposals,
        accepts = calibration_accepts,
        efficiency = ifelse(
            calibration_seconds > 0,
            calibration_movement / calibration_seconds,
            NA_real_
        )
    )
    if (state_control$mode == "fixed") {
        calibration[] <- list(
            state_control$grid, NA_real_, NA_real_, NA_integer_,
            NA_integer_, NA_real_
        )
    }
    valid_acceptance <- !is.na(state_accepted)
    postburn_index <- seq.int(burn + 1L, n_iter)
    valid_postburn <- !is.na(
        state_accepted[postburn_index, , drop = FALSE]
    )
    out <- list(
        model = "GMDE heterogeneous",
        Z = Z_store,
        size = size_store,
        mean_lambda = mean_lambda_store,
        lambda = lambda_store,
        loglik = loglik_store,
        observed_loglik = observed_loglik_store,
        occupied_experts = occupied_store,
        substantive_experts = substantive_store,
        classifier_trace = classifier_trace,
        classifier_trace_name = "Centered graph class-coefficient norm",
        mean_assignment_probability = mean_pi_store,
        ari = ari_store,
        acc = acc_store,
        initialization = initialization,
        theta_terminal = theta_terminal,
        classifier_coefficients = beta_store,
        classifier_contrasts = gamma_store,
        state_accepted = state_accepted,
        state_log_acceptance = state_log_acceptance,
        state_movement = state_movement,
        state_update_seconds = state_update_seconds,
        state_pg_seconds = state_pg_seconds,
        state_pg_shape_sum = state_pg_shape_sum,
        state_pg_shape_max = state_pg_shape_max,
        state_rho = state_rho,
        state_acceptance_rate = mean(state_accepted[valid_acceptance]),
        postburn_state_acceptance_rate = mean(
            state_accepted[postburn_index, , drop = FALSE][valid_postburn]
        ),
        ess_bracket_evaluations = ess_bracket_evaluations,
        ess_likelihood_evaluations = ess_likelihood_evaluations,
        algorithm_exact = identical(pg_backend, "devroye-exact"),
        settings = list(
            n = n, TT = TT, K = K, m = m, p_k = p_k,
            K_true = if (is.null(Z_true)) NA_integer_ else
                length(unique(Z_true)),
            n_iter = n_iter, burn = burn,
            substantive_cluster_size = substantive_min,
            rho = selected_rho,
            r_rule = "rho * pmax(S_k,t, 1)",
            rho_control = state_control,
            rho_calibration = calibration,
            pg_backend = pg_backend,
            pg_trunc = if (pg_backend == "truncated") {
                as.integer(pg_trunc)
            } else NA_integer_,
            pg_implementation = if (pg_backend == "devroye-exact") {
                "BayesLogit::rpg.devroye (integer shape)"
            } else "finite gamma-series sensitivity backend",
            pg_package_version = if (
                pg_backend == "devroye-exact" &&
                requireNamespace("BayesLogit", quietly = TRUE)
            ) {
                as.character(utils::packageVersion("BayesLogit"))
            } else NA_character_,
            algorithm_exact = identical(pg_backend, "devroye-exact"),
            state_update = paste(
                "expert-specific full-path NB-PG information-FFBS",
                "with Poisson MH correction"
            ),
            classifier_parameterization =
                "whitened orthonormal Helmert contrasts",
            classifier_update = "joint elliptical slice sampling",
            relabeling = "none: heterogeneous expert specifications",
            order_initial_labels = isTRUE(order_initial_labels),
            graph_meta = graph_meta,
            prediction_state_stored = isTRUE(store_prediction_state),
            rolling_prediction_status =
                "not migrated: requires a validated Poisson predictive filter",
            m0_list = m0_list, C0_list = C0_list,
            G_list = G_list, W_list = W_list,
            Fmat_list = Fmat_list,
            sampler_version = countdlm_gmde_sampler_version,
            runtime = proc.time()[[3L]] - started,
            seed = seed
        )
    )
    class(out) <- c("gmde_heterogeneous_mcmc", "mixture_dlm_mcmc")
    out
}
