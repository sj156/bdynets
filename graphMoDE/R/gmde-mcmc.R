# End-to-end samplers matching the 2026-08-31 manuscript algorithm.

gmde_validate_rho_control <- function(
    rho,
    rho_grid,
    rho_warmup,
    rho_tie_break,
    rho_schedule,
    burn
) {
    if (!is.null(rho)) {
        if (length(rho) != 1L || !is.finite(rho) || rho <= 0) {
            stop("rho must be one finite positive number.", call. = FALSE)
        }
        if (!is.null(rho_grid) || !is.null(rho_warmup) ||
            !is.null(rho_tie_break) || !is.null(rho_schedule)) {
            stop(
                "Use either fixed rho or an explicit calibration specification, ",
                "not both.", call. = FALSE
            )
        }
        return(list(
            mode = "fixed",
            selected = as.numeric(rho),
            grid = as.numeric(rho),
            warmup = 0L,
            tie_break = NA_character_,
            schedule_rule = NA_character_,
            schedule = numeric(0)
        ))
    }

    if (is.null(rho_grid) || is.null(rho_warmup) ||
        is.null(rho_tie_break) || is.null(rho_schedule)) {
        stop(
            "The manuscript does not specify a formal rho grid. Supply either ",
            "fixed rho, or explicitly supply rho_grid, rho_warmup, and ",
            "rho_tie_break plus rho_schedule.", call. = FALSE
        )
    }
    rho_grid <- as.numeric(rho_grid)
    if (length(rho_grid) < 2L || any(!is.finite(rho_grid)) ||
        any(rho_grid <= 0) || anyDuplicated(rho_grid)) {
        stop("rho_grid must contain at least two distinct positive values.",
             call. = FALSE)
    }
    ratios <- rho_grid[-1L] / rho_grid[-length(rho_grid)]
    if (any(ratios <= 1) || max(abs(log(ratios) - mean(log(ratios)))) > 1e-8) {
        stop("rho_grid must be strictly increasing and geometric.",
             call. = FALSE)
    }
    rho_warmup <- gmde_scalar_integer(
        rho_warmup, "rho_warmup", lower = 1L
    )
    if (rho_warmup < length(rho_grid) || rho_warmup >= burn ||
        rho_warmup %% length(rho_grid) != 0L) {
        stop(
            "rho_warmup must be a positive multiple of length(rho_grid) and ",
            "strictly smaller than burn, leaving post-adaptation burn-in.",
            call. = FALSE
        )
    }
    rho_tie_break <- match.arg(
        rho_tie_break,
        c("smallest", "largest", "first")
    )
    rho_schedule <- match.arg(rho_schedule, c("cyclic", "blocked"))
    per_candidate <- rho_warmup %/% length(rho_grid)
    schedule <- if (identical(rho_schedule, "cyclic")) {
        rep(rho_grid, times = per_candidate)
    } else {
        rep(rho_grid, each = per_candidate)
    }
    list(
        mode = "warmup-grid",
        selected = NA_real_,
        grid = rho_grid,
        warmup = rho_warmup,
        tie_break = rho_tie_break,
        schedule_rule = rho_schedule,
        schedule = schedule
    )
}

gmde_validate_pg_control <- function(pg_backend, state_control) {
    if (!identical(pg_backend, "devroye-exact")) {
        return(invisible(state_control))
    }
    rho_values <- state_control$grid
    if (any(rho_values != round(rho_values)) ||
        any(rho_values > .Machine$integer.max)) {
        stop(
            "Exact Devroye PG sampling requires every fixed or candidate rho ",
            "to be a positive integer, so b[k,t] is an integer.",
            call. = FALSE
        )
    }
    if (!requireNamespace("BayesLogit", quietly = TRUE)) {
        stop(
            "pg_backend='devroye-exact' requires package 'BayesLogit'.",
            call. = FALSE
        )
    }
    invisible(state_control)
}

gmde_select_calibrated_rho <- function(grid, score, tie_break) {
    if (length(grid) != length(score) || any(!is.finite(score))) {
        stop("Every rho candidate must have a finite calibration score.",
             call. = FALSE)
    }
    best <- which(score == max(score))
    if (tie_break == "smallest") return(min(grid[best]))
    if (tie_break == "largest") return(max(grid[best]))
    grid[best[1L]]
}

gmde_validate_sampler_inputs <- function(
    Y, Fmat, K, n_iter, burn, m0, C0, G, W,
    substantive_min, print_freq
) {
    Y <- as.matrix(Y)
    Fmat <- as.matrix(Fmat)
    n <- nrow(Y)
    TT <- ncol(Y)
    p <- ncol(Fmat)
    K <- gmde_scalar_integer(K, "K", lower = 2L, upper = n)
    n_iter <- gmde_scalar_integer(n_iter, "n_iter", lower = 2L)
    burn <- gmde_scalar_integer(burn, "burn", lower = 0L)
    substantive_min <- gmde_scalar_integer(
        substantive_min, "substantive_min", lower = 1L, upper = n
    )
    print_freq <- gmde_scalar_integer(
        print_freq, "print_freq", lower = 0L
    )
    if (n < 1L || TT < 2L || p < 1L || nrow(Fmat) != TT ||
        any(!is.finite(c(Y, Fmat))) || any(Y < 0) ||
        any(Y != round(Y))) {
        stop("Y must be an n by T count matrix and Fmat a finite T by p matrix.",
             call. = FALSE)
    }
    if (burn >= n_iter) {
        stop("Require n_iter >= 2 and 0 <= burn < n_iter.", call. = FALSE)
    }
    if (is.null(m0)) m0 <- c(log(mean(Y) + 0.1), rep(0, p - 1L))
    if (is.null(C0)) C0 <- diag(c(2, rep(1, p - 1L)), p, p)
    if (is.null(G)) G <- diag(p)
    if (is.null(W)) W <- diag(rep(0.005, p), p, p)
    m0 <- as.numeric(m0)
    C0 <- as.matrix(C0)
    G <- as.matrix(G)
    W <- as.matrix(W)
    if (length(m0) != p || !identical(dim(C0), c(p, p)) ||
        !identical(dim(G), c(p, p)) || !identical(dim(W), c(p, p)) ||
        any(!is.finite(c(m0, C0, G, W)))) {
        stop("m0, C0, G, and W do not match the state dimension.",
             call. = FALSE)
    }
    gmde_chol_spd(C0, "C0")
    gmde_chol_spd(W, "W")
    list(
        Y = Y, Fmat = Fmat, K = K, n_iter = n_iter, burn = burn,
        n = n, TT = TT, p = p, m0 = m0, C0 = C0, G = G, W = W,
        substantive_min = substantive_min, print_freq = print_freq
    )
}

gmde_initialize_state_array <- function(Y, Fmat, K, m0, C0, G, W, Z) {
    TT <- ncol(Y)
    p <- ncol(Fmat)
    theta <- array(NA_real_, dim = c(K, TT, p))
    for (k in seq_len(K)) {
        index <- which(Z == k)
        level <- if (length(index)) mean(Y[index, , drop = FALSE]) else mean(Y)
        initial_mean <- m0
        initial_mean[1L] <- log(level + 0.1)
        theta[k, , ] <- gmde_sample_prior_path(
            TT, initial_mean, C0, G, W
        )
    }
    theta
}

gmde_validate_theta_initialization <- function(theta_init, K, TT, p) {
    if (!is.array(theta_init) || !is.numeric(theta_init) ||
        !identical(dim(theta_init), c(K, TT, p)) ||
        any(!is.finite(theta_init))) {
        stop(
            "theta_init must be a finite numeric K by T by p array.",
            call. = FALSE
        )
    }
    theta_init
}

gmde_relabel_nograph <- function(theta, pi_value, Z, Fmat) {
    eta <- gmde_eta(theta, Fmat)
    Z <- gmde_validate_labels(Z, length(Z), nrow(eta), "Z")
    order_index <- order(rowMeans(exp(eta)), seq_len(nrow(eta)))
    old_to_new <- integer(length(order_index))
    old_to_new[order_index] <- seq_along(order_index)
    list(
        theta = theta[order_index, , , drop = FALSE],
        pi = pi_value[order_index],
        Z = old_to_new[Z],
        order = order_index
    )
}

gmde_potts_utilities <- function(Z, graph_weight, beta, K) {
    graph_weight <- as.matrix(graph_weight)
    n <- nrow(graph_weight)
    Z <- gmde_validate_labels(Z, n, K, "Z")
    membership <- matrix(0, nrow = n, ncol = K)
    membership[cbind(seq_len(n), Z)] <- 1
    beta * graph_weight %*% membership
}

gmde_potts_unnormalized_complete_log_density <- function(
    Y, eta, Z, graph_weight, beta
) {
    Y <- as.matrix(Y)
    eta <- as.matrix(eta)
    n <- nrow(Y)
    K <- nrow(eta)
    Z <- gmde_validate_labels(Z, n, K, "Z")
    eta_by_location <- eta[Z, , drop = FALSE]
    count_part <- sum(
        Y * eta_by_location - exp(eta_by_location) - lgamma(Y + 1)
    )
    same_label <- outer(Z, Z, "==")
    graph_part <- beta * sum(graph_weight[upper.tri(graph_weight)] *
        same_label[upper.tri(same_label)])
    count_part + graph_part
}

gmde_potts_allocation_sweep <- function(
    Y, eta, Z, graph_weight, beta, random_scan = TRUE
) {
    Y <- as.matrix(Y)
    eta <- as.matrix(eta)
    n <- nrow(Y)
    K <- nrow(eta)
    Z <- gmde_validate_labels(Z, n, K, "Z")
    count_log_weight <- gmde_allocation_log_weights(
        Y, eta, matrix(0, nrow = n, ncol = K)
    )
    scan_order <- if (isTRUE(random_scan)) sample.int(n) else seq_len(n)
    for (i in scan_order) {
        neighbor_score <- vapply(
            seq_len(K),
            function(k) sum(graph_weight[i, Z == k]),
            numeric(1)
        )
        probability <- gmde_softmax_rows(matrix(
            count_log_weight[i, ] + beta * neighbor_score,
            nrow = 1L
        ))
        Z[i] <- gmde_sample_categorical_rows(probability)
    }
    list(
        Z = Z,
        utilities = gmde_potts_utilities(Z, graph_weight, beta, K),
        scan_order = scan_order
    )
}

gmde_relabel_potts <- function(theta, Z, Fmat) {
    eta <- gmde_eta(theta, Fmat)
    order_index <- order(rowMeans(exp(eta)), seq_len(nrow(eta)))
    Z <- gmde_validate_labels(Z, length(Z), nrow(eta), "Z")
    old_to_new <- integer(length(order_index))
    old_to_new[order_index] <- seq_along(order_index)
    list(
        theta = theta[order_index, , , drop = FALSE],
        Z = old_to_new[Z],
        order = order_index
    )
}

gmde_run_mixture_mcmc <- function(
    model,
    Y,
    Fmat,
    K,
    n_iter,
    burn,
    Phi = NULL,
    alpha_pi = NULL,
    potts_W = NULL,
    potts_beta = NULL,
    potts_random_scan = TRUE,
    m0 = NULL,
    C0 = NULL,
    G = NULL,
    W = NULL,
    rho = NULL,
    rho_grid = NULL,
    rho_warmup = NULL,
    rho_tie_break = NULL,
    rho_schedule = NULL,
    r_tune = NULL,
    Z_true = NULL,
    Z_init = NULL,
    theta_init = NULL,
    beta_init = NULL,
    gamma_init = NULL,
    substantive_min = 10L,
    pg_backend = c("devroye-exact", "truncated"),
    pg_trunc = 80L,
    graph_meta = NULL,
    store_prediction_state = TRUE,
    max_ess_steps = 1000L,
    print_freq = 100L,
    seed = NULL,
    rng_state_init = NULL,
    store_sampler_terminal_state = FALSE,
    store_allocation_conditionals = FALSE,
    resume_state = NULL
) {
    model <- match.arg(model, c("gmde", "nograph", "potts"))
    pg_backend <- match.arg(pg_backend)
    if (!is.null(r_tune)) {
        stop(
            "r_tune belongs to the frozen approximate sampler. The current ",
            "algorithm requires fixed rho or an explicit rho calibration.",
            call. = FALSE
        )
    }
    if (!is.null(resume_state) && (!is.null(seed) ||
        !is.null(rng_state_init) || !is.null(Z_init) ||
        !is.null(theta_init) || !is.null(beta_init) ||
        !is.null(gamma_init))) {
        stop(
            paste(
                "resume_state cannot be combined with seed, rng_state_init,",
                "Z_init, theta_init, beta_init, or gamma_init."
            ),
            call. = FALSE
        )
    }
    if (!is.null(seed) && !is.null(rng_state_init)) {
        stop(
            "Supply at most one of seed and rng_state_init.",
            call. = FALSE
        )
    }
    if (!is.logical(store_sampler_terminal_state) ||
        length(store_sampler_terminal_state) != 1L ||
        is.na(store_sampler_terminal_state)) {
        stop("store_sampler_terminal_state must be TRUE or FALSE.",
             call. = FALSE)
    }
    if (!is.logical(store_allocation_conditionals) ||
        length(store_allocation_conditionals) != 1L ||
        is.na(store_allocation_conditionals)) {
        stop("store_allocation_conditionals must be TRUE or FALSE.",
             call. = FALSE)
    }
    if (isTRUE(store_allocation_conditionals) && model != "gmde") {
        stop(
            paste(
                "Conditional allocation diagnostics currently support",
                "only the independent-row GMDE allocation update."
            ),
            call. = FALSE
        )
    }
    checked <- gmde_validate_sampler_inputs(
        Y, Fmat, K, n_iter, burn, m0, C0, G, W,
        substantive_min, print_freq
    )
    Y <- checked$Y
    Fmat <- checked$Fmat
    K <- checked$K
    n_iter <- checked$n_iter
    burn <- checked$burn
    n <- checked$n
    TT <- checked$TT
    p <- checked$p
    m0 <- checked$m0
    C0 <- checked$C0
    G <- checked$G
    W <- checked$W
    substantive_min <- checked$substantive_min
    print_freq <- checked$print_freq
    if (!is.null(Z_true)) {
        if (!is.numeric(Z_true) || length(Z_true) != n ||
            any(!is.finite(Z_true)) || any(Z_true != round(Z_true)) ||
            length(unique(Z_true)) > K || K > 15L) {
            stop(
                "Z_true must contain n whole-number labels and at most K ",
                "distinct classes; metrics currently support K <= 15.",
                call. = FALSE
            )
        }
        Z_true <- as.integer(Z_true)
    }
    state_control <- gmde_validate_rho_control(
        rho, rho_grid, rho_warmup, rho_tie_break, rho_schedule, burn
    )
    gmde_validate_pg_control(pg_backend, state_control)
    max_ess_steps <- gmde_scalar_integer(
        max_ess_steps, "max_ess_steps", lower = 1L
    )

    resumed_iterations <- 0L
    if (!is.null(resume_state)) {
        required_resume_fields <- c(
            "model", "completed_iterations", "Z", "theta", "classifier",
            "rng_state", "n", "TT", "p", "K", "m", "rho",
            "sampler_version"
        )
        if (!is.list(resume_state) ||
            !all(required_resume_fields %in% names(resume_state)) ||
            !identical(resume_state$model, model) ||
            !identical(resume_state$sampler_version,
                       countdlm_gmde_sampler_version) ||
            !identical(state_control$mode, "fixed") ||
            !identical(resume_state$rho, state_control$selected) ||
            !identical(resume_state$n, n) ||
            !identical(resume_state$TT, TT) ||
            !identical(resume_state$p, p) ||
            !identical(resume_state$K, K) ||
            !identical(resume_state$m, if (model == "gmde") {
                ncol(as.matrix(Phi))
            } else 0L) ||
            length(resume_state$completed_iterations) != 1L ||
            !is.numeric(resume_state$completed_iterations) ||
            !is.finite(resume_state$completed_iterations) ||
            resume_state$completed_iterations < 1L ||
            resume_state$completed_iterations !=
                round(resume_state$completed_iterations)) {
            stop(
                paste(
                    "resume_state is incompatible with this fixed-rho",
                    "sampler call."
                ),
                call. = FALSE
            )
        }
        if (!is.numeric(resume_state$rng_state) ||
            length(resume_state$rng_state) < 2L ||
            any(!is.finite(resume_state$rng_state)) ||
            any(resume_state$rng_state != round(resume_state$rng_state))) {
            stop("resume_state contains an invalid R RNG state.",
                 call. = FALSE)
        }
        Z <- gmde_validate_labels(resume_state$Z, n, K, "resume_state$Z")
        theta <- gmde_validate_theta_initialization(
            resume_state$theta, K, TT, p
        )
        resumed_iterations <- as.integer(resume_state$completed_iterations)
        assign(
            ".Random.seed", as.integer(resume_state$rng_state),
            envir = .GlobalEnv
        )
    } else {
        if (!is.null(rng_state_init)) {
            if (!is.numeric(rng_state_init) ||
                length(rng_state_init) < 2L ||
                any(!is.finite(rng_state_init)) ||
                any(rng_state_init != round(rng_state_init))) {
                stop(
                    paste(
                        "rng_state_init must be a finite whole-number R RNG",
                        "state."
                    ),
                    call. = FALSE
                )
            }
            assign(
                ".Random.seed", as.integer(rng_state_init),
                envir = .GlobalEnv
            )
        } else if (!is.null(seed)) {
            set.seed(seed)
        }
        Z <- if (is.null(Z_init)) {
            gmde_initialize_allocations(Y, K)
        } else gmde_validate_labels(Z_init, n, K, "Z_init")
        theta <- if (is.null(theta_init)) {
            gmde_initialize_state_array(Y, Fmat, K, m0, C0, G, W, Z)
        } else {
            gmde_validate_theta_initialization(theta_init, K, TT, p)
        }
    }

    H <- NULL
    gamma <- NULL
    pi_value <- NULL
    m <- 0L
    if (model == "gmde") {
        Phi <- as.matrix(Phi)
        if (nrow(Phi) != n || ncol(Phi) < 1L || any(!is.finite(Phi))) {
            stop("Phi must be a finite n by m matrix.", call. = FALSE)
        }
        m <- ncol(Phi)
        H <- gmde_helmert_contrast(K)
        if (!is.null(resume_state)) {
            gamma <- as.matrix(resume_state$classifier)
            if (!identical(dim(gamma), c(m, K - 1L)) ||
                any(!is.finite(gamma))) {
                stop(
                    "resume_state contains an invalid GMDE classifier.",
                    call. = FALSE
                )
            }
        } else {
            if (!is.null(beta_init) && !is.null(gamma_init)) {
                stop("Supply at most one of beta_init and gamma_init.",
                     call. = FALSE)
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
                gamma <- matrix(
                    stats::rnorm(m * (K - 1L)), m, K - 1L
                )
            }
            relabeled <- gmde_relabel_by_mean_intensity(
                theta, gamma, Z, Fmat, H
            )
            theta <- relabeled$theta
            gamma <- relabeled$gamma
            Z <- relabeled$Z
        }
    } else if (model == "nograph") {
        if (!is.null(resume_state)) {
            stop("resume_state currently supports fixed-rho GMDE only.",
                 call. = FALSE)
        }
        if (!is.null(beta_init) || !is.null(gamma_init)) {
            stop("Classifier initial values apply only to GMDE.",
                 call. = FALSE)
        }
        if (is.null(alpha_pi)) {
            stop("alpha_pi must be supplied explicitly for the graph-free model.",
                 call. = FALSE)
        }
        alpha_pi <- as.numeric(alpha_pi)
        if (length(alpha_pi) != K || any(!is.finite(alpha_pi)) ||
            any(alpha_pi <= 0)) {
            stop("alpha_pi must contain K finite positive values.",
                 call. = FALSE)
        }
        if (any(alpha_pi != alpha_pi[1L])) {
            stop(
                "Intensity-order relabeling requires a symmetric alpha_pi. ",
                "Asymmetric Dirichlet priors are not supported by this ",
                "canonical-label sampler.", call. = FALSE
            )
        }
        pi_value <- gmde_rdirichlet1(alpha_pi + tabulate(Z, nbins = K))
        relabeled <- gmde_relabel_nograph(theta, pi_value, Z, Fmat)
        theta <- relabeled$theta
        pi_value <- relabeled$pi
        Z <- relabeled$Z
    } else {
        if (!is.null(resume_state)) {
            stop("resume_state currently supports fixed-rho GMDE only.",
                 call. = FALSE)
        }
        if (!is.null(beta_init) || !is.null(gamma_init) ||
            !is.null(alpha_pi)) {
            stop(
                "Classifier and Dirichlet initial values do not apply to ",
                "Potts-MDE.", call. = FALSE
            )
        }
        potts_W <- as.matrix(potts_W)
        graph_scale <- max(1, abs(potts_W))
        graph_tolerance <- 1e-10 * graph_scale
        if (!identical(dim(potts_W), c(n, n)) ||
            any(!is.finite(potts_W)) ||
            any(potts_W < -graph_tolerance) ||
            max(abs(potts_W - t(potts_W))) > graph_tolerance ||
            any(abs(diag(potts_W)) > graph_tolerance)) {
            stop(
                "potts_W must be a finite symmetric nonnegative n by n ",
                "matrix with zero diagonal.", call. = FALSE
            )
        }
        potts_W[potts_W < 0] <- 0
        diag(potts_W) <- 0
        if (length(potts_beta) != 1L || !is.numeric(potts_beta) ||
            !is.finite(potts_beta) || potts_beta < 0) {
            stop("potts_beta must be one finite nonnegative number.",
                 call. = FALSE)
        }
        if (!is.logical(potts_random_scan) ||
            length(potts_random_scan) != 1L ||
            is.na(potts_random_scan)) {
            stop("potts_random_scan must be TRUE or FALSE.", call. = FALSE)
        }
        potts_random_scan <- isTRUE(potts_random_scan)
        relabeled <- gmde_relabel_potts(theta, Z, Fmat)
        theta <- relabeled$theta
        Z <- relabeled$Z
    }

    initialization <- list(
        model = model,
        Z = as.integer(Z),
        theta = theta,
        classifier = if (model == "gmde") {
            gamma
        } else if (model == "nograph") {
            pi_value
        } else NULL
    )

    Z_store <- matrix(NA_integer_, n_iter, n)
    size_store <- matrix(NA_integer_, n_iter, K)
    mean_lambda_store <- matrix(NA_real_, n_iter, K)
    lambda_store <- array(NA_real_, c(n_iter, K, TT))
    loglik_store <- numeric(n_iter)
    observed_loglik_store <- numeric(n_iter)
    classifier_trace <- matrix(NA_real_, n_iter, K)
    mean_pi_store <- matrix(NA_real_, n_iter, K)
    ari_store <- rep(NA_real_, n_iter)
    acc_store <- rep(NA_real_, n_iter)
    occupied_store <- integer(n_iter)
    substantive_store <- integer(n_iter)
    occupied_births <- integer(n_iter)
    occupied_deaths <- integer(n_iter)
    substantive_upcrossings <- integer(n_iter)
    substantive_downcrossings <- integer(n_iter)
    state_accepted <- matrix(NA, n_iter, K)
    state_log_acceptance <- matrix(NA_real_, n_iter, K)
    state_movement <- matrix(0, n_iter, K)
    state_update_seconds <- matrix(0, n_iter, K)
    state_pg_seconds <- matrix(0, n_iter, K)
    state_pg_shape_sum <- matrix(NA_real_, n_iter, K)
    state_pg_shape_max <- matrix(NA_real_, n_iter, K)
    state_rho <- numeric(n_iter)
    ess_bracket_evaluations <- rep(NA_integer_, n_iter)
    ess_likelihood_evaluations <- rep(NA_integer_, n_iter)
    postburn_draws <- n_iter - burn
    theta_terminal <- if (isTRUE(store_prediction_state)) {
        array(NA_real_, c(postburn_draws, K, p))
    } else NULL
    beta_store <- if (model == "gmde" && isTRUE(store_prediction_state)) {
        array(NA_real_, c(postburn_draws, m, K))
    } else NULL
    gamma_store <- if (model == "gmde" && isTRUE(store_prediction_state)) {
        array(NA_real_, c(postburn_draws, m, K - 1L))
    } else NULL
    pi_store <- if (model == "nograph") matrix(NA_real_, n_iter, K) else NULL
    allocation_conditionals <- if (isTRUE(
        store_allocation_conditionals
    )) {
        list(
            size_before = matrix(NA_integer_, n_iter, K),
            size_after = matrix(NA_integer_, n_iter, K),
            expected_size_after = matrix(NA_real_, n_iter, K),
            log_probability_empty_after = matrix(NA_real_, n_iter, K),
            probability_empty_after = matrix(NA_real_, n_iter, K),
            probability_substantive_after =
                matrix(NA_real_, n_iter, K),
            probability_nonsubstantive_after =
                matrix(NA_real_, n_iter, K),
            birth_probability = matrix(NA_real_, n_iter, K),
            death_probability = matrix(NA_real_, n_iter, K),
            upcross_probability = matrix(NA_real_, n_iter, K),
            downcross_probability = matrix(NA_real_, n_iter, K),
            expected_Kocc_after = numeric(n_iter),
            expected_Ksub_after = numeric(n_iter),
            expected_births = numeric(n_iter),
            expected_deaths = numeric(n_iter),
            expected_upcrossings = numeric(n_iter),
            expected_downcrossings = numeric(n_iter),
            expected_node_switches = numeric(n_iter),
            observed_node_switches = integer(n_iter),
            Kocc_after_variance = numeric(n_iter),
            birth_variance = numeric(n_iter),
            death_variance = numeric(n_iter),
            node_switch_variance = numeric(n_iter),
            maximum_row_sum_error = numeric(n_iter),
            maximum_dp_mass_error = numeric(n_iter),
            maximum_zero_probability_error = numeric(n_iter),
            rng_unchanged = rep(FALSE, n_iter)
        )
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

        state_result <- gmde_update_all_state_paths(
            Y = Y, Z = Z, theta = theta, Fmat = Fmat,
            m0 = m0, C0 = C0, G = G, W = W,
            rho = rho_iter, K = K,
            pg_backend = pg_backend, pg_trunc = pg_trunc
        )
        theta <- state_result$theta

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

        eta <- gmde_eta(theta, Fmat)
        if (model == "gmde") {
            classifier_result <- gmde_joint_ess(
                gamma = gamma, Phi = Phi, Z = Z, H = H,
                max_bracket_steps = max_ess_steps
            )
            gamma <- classifier_result$gamma
            utilities <- classifier_result$utilities
            ess_bracket_evaluations[iter] <-
                classifier_result$bracket_evaluations
            ess_likelihood_evaluations[iter] <-
                classifier_result$likelihood_evaluations
        } else if (model == "nograph") {
            utilities <- matrix(rep(log(pi_value), each = n), nrow = n)
        } else {
            utilities <- NULL
        }

        Z_before_allocation <- Z
        size_before_allocation <- tabulate(Z_before_allocation, nbins = K)
        if (model == "potts") {
            potts_allocation <- gmde_potts_allocation_sweep(
                Y = Y, eta = eta, Z = Z, graph_weight = potts_W,
                beta = potts_beta, random_scan = potts_random_scan
            )
            Z <- potts_allocation$Z
            relabeled <- gmde_relabel_potts(theta, Z, Fmat)
            theta <- relabeled$theta
            Z <- relabeled$Z
            utilities <- gmde_potts_utilities(Z, potts_W, potts_beta, K)
        } else {
            allocation <- gmde_allocation_probabilities(Y, eta, utilities)
            if (isTRUE(store_allocation_conditionals)) {
                if (!exists(
                    ".Random.seed", envir = .GlobalEnv, inherits = FALSE
                )) {
                    stop(
                        "The R RNG state is unavailable before diagnostics.",
                        call. = FALSE
                    )
                }
                rng_before_diagnostic <- get(
                    ".Random.seed", envir = .GlobalEnv, inherits = FALSE
                )
                conditional <- gmde_allocation_conditional_mechanics(
                    allocation$probability, Z_before_allocation,
                    substantive_min
                )
                rng_unchanged <- identical(
                    rng_before_diagnostic,
                    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
                )
                if (!rng_unchanged) {
                    stop(
                        paste(
                            "Conditional allocation diagnostics consumed",
                            "random numbers."
                        ),
                        call. = FALSE
                    )
                }
                component <- conditional$component
                allocation_conditionals$size_before[iter, ] <-
                    component$size_before
                allocation_conditionals$expected_size_after[iter, ] <-
                    component$expected_size_after
                allocation_conditionals$log_probability_empty_after[iter, ] <-
                    component$log_probability_empty_after
                allocation_conditionals$probability_empty_after[iter, ] <-
                    component$probability_empty_after
                allocation_conditionals$probability_substantive_after[iter, ] <-
                    component$probability_substantive_after
                allocation_conditionals$probability_nonsubstantive_after[iter, ] <-
                    component$probability_nonsubstantive_after
                allocation_conditionals$birth_probability[iter, ] <-
                    component$birth_probability
                allocation_conditionals$death_probability[iter, ] <-
                    component$death_probability
                allocation_conditionals$upcross_probability[iter, ] <-
                    component$upcross_probability
                allocation_conditionals$downcross_probability[iter, ] <-
                    component$downcross_probability
                for (field in names(conditional$total)) {
                    allocation_conditionals[[field]][[iter]] <-
                        conditional$total[[field]]
                }
                allocation_conditionals$Kocc_after_variance[[iter]] <-
                    conditional$variance[["Kocc_after"]]
                allocation_conditionals$birth_variance[[iter]] <-
                    conditional$variance[["births"]]
                allocation_conditionals$death_variance[[iter]] <-
                    conditional$variance[["deaths"]]
                allocation_conditionals$node_switch_variance[[iter]] <-
                    conditional$variance[["node_switches"]]
                allocation_conditionals$maximum_row_sum_error[[iter]] <-
                    conditional$row_sum_error
                allocation_conditionals$maximum_dp_mass_error[[iter]] <-
                    conditional$dp_mass_error
                allocation_conditionals$maximum_zero_probability_error[[iter]] <-
                    conditional$zero_probability_error
                allocation_conditionals$rng_unchanged[[iter]] <- TRUE
            }
            Z <- gmde_sample_categorical_rows(allocation$probability)
        }
        size_after_allocation <- tabulate(Z, nbins = K)
        if (isTRUE(store_allocation_conditionals)) {
            allocation_conditionals$size_after[iter, ] <-
                size_after_allocation
            allocation_conditionals$observed_node_switches[[iter]] <-
                sum(Z != Z_before_allocation)
        }
        occupied_births[iter] <- sum(
            size_before_allocation == 0L & size_after_allocation > 0L
        )
        occupied_deaths[iter] <- sum(
            size_before_allocation > 0L & size_after_allocation == 0L
        )
        substantive_upcrossings[iter] <- sum(
            size_before_allocation < substantive_min &
                size_after_allocation >= substantive_min
        )
        substantive_downcrossings[iter] <- sum(
            size_before_allocation >= substantive_min &
                size_after_allocation < substantive_min
        )
        if (model == "nograph") {
            pi_value <- gmde_rdirichlet1(
                alpha_pi + tabulate(Z, nbins = K)
            )
            relabeled <- gmde_relabel_nograph(theta, pi_value, Z, Fmat)
            theta <- relabeled$theta
            pi_value <- relabeled$pi
            Z <- relabeled$Z
            utilities <- matrix(rep(log(pi_value), each = n), nrow = n)
        } else if (model == "gmde") {
            relabeled <- gmde_relabel_by_mean_intensity(
                theta, gamma, Z, Fmat, H
            )
            theta <- relabeled$theta
            gamma <- relabeled$gamma
            Z <- relabeled$Z
            utilities <- Phi %*% gamma %*% t(H)
        }

        diagnostic_order <- relabeled$order
        state_accepted[iter, ] <- state_result$accepted[diagnostic_order]
        state_log_acceptance[iter, ] <-
            state_result$log_acceptance[diagnostic_order]
        state_movement[iter, ] <-
            state_result$accepted_movement[diagnostic_order]
        state_update_seconds[iter, ] <-
            state_result$elapsed_seconds[diagnostic_order]
        state_pg_seconds[iter, ] <-
            state_result$pg_elapsed_seconds[diagnostic_order]
        state_pg_shape_sum[iter, ] <-
            state_result$pg_shape_sum[diagnostic_order]
        state_pg_shape_max[iter, ] <-
            state_result$pg_shape_max[diagnostic_order]

        eta <- gmde_eta(theta, Fmat)
        lambda <- exp(eta)
        probability <- gmde_softmax_rows(utilities)
        Z_store[iter, ] <- Z
        size_store[iter, ] <- tabulate(Z, nbins = K)
        occupied_store[iter] <- sum(size_store[iter, ] > 0L)
        substantive_store[iter] <- sum(size_store[iter, ] >= substantive_min)
        mean_lambda_store[iter, ] <- rowMeans(lambda)
        lambda_store[iter, , ] <- lambda
        mean_pi_store[iter, ] <- colMeans(probability)
        if (model == "potts") {
            ## At fixed beta the global Potts normalizer is constant in Z,
            ## but it is not available for the observed-data marginal.  Keep
            ## the valid unnormalized complete-data diagnostic and do not
            ## mislabel a product of node conditionals as an observed
            ## likelihood.
            loglik_store[iter] <-
                gmde_potts_unnormalized_complete_log_density(
                    Y, eta, Z, potts_W, potts_beta
                )
            observed_loglik_store[iter] <- NA_real_
        } else {
            loglik_store[iter] <-
                gmde_complete_loglik(Y, eta, utilities, Z)
            observed_loglik_store[iter] <-
                gmde_observed_loglik(Y, eta, utilities)
        }
        if (model == "gmde") {
            beta <- gamma %*% t(H)
            classifier_trace[iter, ] <- sqrt(colSums(beta^2))
        } else if (model == "nograph") {
            pi_store[iter, ] <- pi_value
            classifier_trace[iter, ] <- pi_value
        } else {
            classifier_trace[iter, ] <- colMeans(utilities)
        }
        if (!is.null(Z_true)) {
            ari_store[iter] <- gmde_adjusted_rand(Z_true, Z)
            acc_store[iter] <- gmde_best_label_accuracy(Z_true, Z, K)
        }
        if (isTRUE(store_prediction_state) && iter > burn) {
            draw_index <- iter - burn
            theta_terminal[draw_index, , ] <-
                matrix(theta[, TT, ], nrow = K, ncol = p)
            if (model == "gmde") {
                beta_store[draw_index, , ] <- gamma %*% t(H)
                gamma_store[draw_index, , ] <- gamma
            }
        }
        if (print_freq > 0L && iter %% print_freq == 0L) {
            valid_accept <- !is.na(state_accepted[iter, ])
            cat(
                switch(
                    model,
                    gmde = "GMDE",
                    nograph = "MoDE",
                    potts = "Potts-MDE"
                ),
                "iteration", iter,
                if (model == "potts") {
                    "| unnormalized complete log density ="
                } else "| observed loglik =",
                round(if (model == "potts") {
                    loglik_store[iter]
                } else observed_loglik_store[iter], 1),
                "| state accepts =",
                paste(as.integer(state_accepted[iter, valid_accept]),
                      collapse = ","),
                "| rho =", format(rho_iter), "\n"
            )
        }
    }

    runtime <- proc.time()[[3L]] - started
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
    valid_all <- !is.na(state_accepted)
    postburn_index <- if (burn < n_iter) seq.int(burn + 1L, n_iter) else integer(0)
    valid_post <- !is.na(state_accepted[postburn_index, , drop = FALSE])

    sampler_terminal_state <- if (isTRUE(store_sampler_terminal_state)) {
        if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
            stop("The terminal R RNG state is unavailable.", call. = FALSE)
        }
        list(
            model = model,
            completed_iterations = resumed_iterations + n_iter,
            Z = as.integer(Z),
            theta = theta,
            classifier = if (model == "gmde") {
                gamma
            } else if (model == "nograph") {
                pi_value
            } else NULL,
            rng_state = get(
                ".Random.seed", envir = .GlobalEnv, inherits = FALSE
            ),
            rng_kind = RNGkind(),
            n = n,
            TT = TT,
            p = p,
            K = K,
            m = m,
            rho = selected_rho,
            sampler_version = countdlm_gmde_sampler_version
        )
    } else NULL

    out <- list(
        model = switch(
            model,
            gmde = "GMDE",
            nograph = "MoDE without graph",
            potts = "Potts-MDE"
        ),
        Z = Z_store,
        size = size_store,
        mean_lambda = mean_lambda_store,
        lambda = lambda_store,
        loglik = loglik_store,
        observed_loglik = observed_loglik_store,
        occupied_experts = occupied_store,
        substantive_experts = substantive_store,
        occupied_births = occupied_births,
        occupied_deaths = occupied_deaths,
        substantive_upcrossings = substantive_upcrossings,
        substantive_downcrossings = substantive_downcrossings,
        classifier_trace = classifier_trace,
        classifier_trace_name = switch(
            model,
            gmde = "Centered graph class-coefficient norm",
            nograph = "Common mixing probability",
            potts = "Mean conditional Potts utility"
        ),
        mean_assignment_probability = mean_pi_store,
        ari = ari_store,
        acc = acc_store,
        initialization = initialization,
        theta_terminal = theta_terminal,
        classifier_coefficients = beta_store,
        classifier_contrasts = gamma_store,
        prediction_mixing_probability = if (model == "nograph" &&
            isTRUE(store_prediction_state)) {
            pi_store[seq.int(burn + 1L, n_iter), , drop = FALSE]
        } else NULL,
        state_accepted = state_accepted,
        state_log_acceptance = state_log_acceptance,
        state_movement = state_movement,
        state_update_seconds = state_update_seconds,
        state_pg_seconds = state_pg_seconds,
        state_pg_shape_sum = state_pg_shape_sum,
        state_pg_shape_max = state_pg_shape_max,
        state_rho = state_rho,
        state_acceptance_rate = mean(state_accepted[valid_all]),
        postburn_state_acceptance_rate = mean(
            state_accepted[postburn_index, , drop = FALSE][valid_post]
        ),
        ess_bracket_evaluations = ess_bracket_evaluations,
        ess_likelihood_evaluations = ess_likelihood_evaluations,
        algorithm_exact = identical(pg_backend, "devroye-exact"),
        settings = list(
            n = n, TT = TT, p = p, K = K, m = m,
            K_true = if (is.null(Z_true)) NA_integer_ else
                length(unique(as.integer(Z_true))),
            substantive_cluster_size = substantive_min,
            n_iter = n_iter, burn = burn,
            resumed_from_iterations = resumed_iterations,
            rho = selected_rho,
            r_rule = "rho * pmax(S_k,t, 1)",
            rho_control = state_control,
            rho_calibration = calibration,
            pg_backend = pg_backend,
            pg_trunc = if (pg_backend == "truncated") as.integer(pg_trunc) else NA_integer_,
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
            state_update =
                "full-path NB-PG information-FFBS with Poisson MH correction",
            classifier_parameterization = switch(
                model,
                gmde = "whitened orthonormal Helmert contrasts",
                nograph = "common Dirichlet probability",
                potts = "weighted Potts allocation field"
            ),
            classifier_update = switch(
                model,
                gmde = "joint elliptical slice sampling",
                nograph = "Dirichlet full conditional",
                potts = "single-site random-scan Gibbs sweep"
            ),
            complete_loglik_definition = switch(
                model,
                gmde = "complete-data log likelihood",
                nograph = "complete-data log likelihood",
                potts = paste(
                    "complete-data count log likelihood plus the fixed-beta",
                    "Potts energy; global normalizing constant omitted"
                )
            ),
            observed_loglik_definition = switch(
                model,
                gmde = "exact observed-data mixture log likelihood",
                nograph = "exact observed-data mixture log likelihood",
                potts = paste(
                    "unavailable: the global Potts marginal is not replaced",
                    "by a node-conditional pseudolikelihood"
                )
            ),
            mean_assignment_probability_definition = switch(
                model,
                gmde = "mean graph-gating prior probability",
                nograph = "common mixing probability",
                potts = paste(
                    "mean node-conditional Potts prior probability at the",
                    "current allocation; not a joint marginal"
                )
            ),
            graph_meta = graph_meta,
            alpha_pi = if (model == "nograph") alpha_pi else NULL,
            potts_beta = if (model == "potts") potts_beta else NULL,
            potts_random_scan = if (model == "potts") {
                potts_random_scan
            } else NULL,
            prediction_state_stored = isTRUE(store_prediction_state),
            sampler_terminal_state_stored =
                isTRUE(store_sampler_terminal_state),
            allocation_conditionals_stored =
                isTRUE(store_allocation_conditionals),
            rolling_prediction_status =
                "not migrated: previous one-pass NB-PG filter is approximate",
            m0 = m0, C0 = C0, G = G, W = W,
            sampler_version = countdlm_gmde_sampler_version,
            runtime = runtime, seed = seed
        )
    )
    if (isTRUE(store_sampler_terminal_state)) {
        out$sampler_terminal_state <- sampler_terminal_state
    }
    if (isTRUE(store_allocation_conditionals)) {
        out$allocation_conditionals <- allocation_conditionals
    }
    class(out) <- switch(
        model,
        gmde = c("gmde_mcmc", "mixture_dlm_mcmc"),
        nograph = c("nograph_mode_mcmc", "mixture_dlm_mcmc"),
        potts = c("potts_mde_mcmc", "mixture_dlm_mcmc")
    )
    out
}

#' Run the current exact-target GMDE sampler
#'
#' @param Y,Fmat,Phi,K Model inputs.
#' @param n_iter Total MCMC iterations.
#' @param burn Number of initial iterations discarded as burn-in.
#' @param rho Fixed positive structured NB multiplier.  Alternatively leave
#'   `rho = NULL` and explicitly supply `rho_grid`, `rho_warmup`, and
#'   `rho_tie_break`.
#' @param rho_grid Explicit increasing geometric candidate grid.
#' @param rho_warmup Number of initial calibration transitions; it must be a
#'   multiple of `length(rho_grid)` and smaller than `burn`.
#' @param rho_tie_break One of `"smallest"`, `"largest"`, or `"first"`.
#' @param rho_schedule Explicit warm-up ordering: `"cyclic"` interleaves
#'   candidates, whereas `"blocked"` uses one contiguous block per candidate.
#' @param r_tune Retired legacy argument; any non-NULL value fails closed.
#' @param theta_init Optional finite `K` by `T` by `p` state-path array.
#'   The default `NULL` preserves the data-informed initializer.  This control
#'   is primarily useful for paired initialization diagnostics.
#' @param ... Additional controls passed to the shared sampler, including
#'   optional terminal-state retention, fixed-rho GMDE continuation, and the
#'   passive `store_allocation_conditionals` diagnostic switch.
#' @return An object of class `gmde_mcmc`.
#' @export
run_gmde_mcmc <- function(
    Y, Fmat, Phi, K,
    n_iter = 1500L,
    burn = floor(n_iter / 2),
    rho = NULL,
    rho_grid = NULL,
    rho_warmup = NULL,
    rho_tie_break = NULL,
    rho_schedule = NULL,
    r_tune = NULL,
    theta_init = NULL,
    ...
) {
    gmde_run_mixture_mcmc(
        model = "gmde", Y = Y, Fmat = Fmat, Phi = Phi, K = K,
        n_iter = n_iter, burn = burn,
        rho = rho, rho_grid = rho_grid, rho_warmup = rho_warmup,
        rho_tie_break = rho_tie_break, rho_schedule = rho_schedule,
        r_tune = r_tune, theta_init = theta_init, ...
    )
}

#' Run the graph-free MoDE with the same exact-target state kernel
#'
#' @param Y,Fmat,K Model inputs.
#' @param alpha_pi Explicit Dirichlet prior vector.
#' @inheritParams run_gmde_mcmc
#' @return An object of class `nograph_mode_mcmc`.
#' @export
run_nograph_mcmc <- function(
    Y, Fmat, K, alpha_pi,
    n_iter = 1500L,
    burn = floor(n_iter / 2),
    rho = NULL,
    rho_grid = NULL,
    rho_warmup = NULL,
    rho_tie_break = NULL,
    rho_schedule = NULL,
    r_tune = NULL,
    ...
) {
    gmde_run_mixture_mcmc(
        model = "nograph", Y = Y, Fmat = Fmat, K = K,
        alpha_pi = alpha_pi, n_iter = n_iter, burn = burn,
        rho = rho, rho_grid = rho_grid, rho_warmup = rho_warmup,
        rho_tie_break = rho_tie_break, rho_schedule = rho_schedule,
        r_tune = r_tune, ...
    )
}

#' Run Potts-MDE with the same exact-target state kernel
#'
#' The allocation update is a random-scan single-site Gibbs sweep under the
#' weighted Potts field.  The intractable global Potts normalizing constant is
#' not needed for this fixed-beta conditional update.
#'
#' @param Y,Fmat,K Model inputs.
#' @param potts_W Symmetric nonnegative graph-weight matrix.
#' @param potts_beta Fixed nonnegative Potts smoothing coefficient.
#' @param potts_random_scan Whether to randomize the node order each sweep.
#' @inheritParams run_gmde_mcmc
#' @return An object of class `potts_mde_mcmc`.
#' @export
run_potts_mcmc <- function(
    Y, Fmat, K, potts_W, potts_beta,
    n_iter = 1500L,
    burn = floor(n_iter / 2),
    rho = NULL,
    rho_grid = NULL,
    rho_warmup = NULL,
    rho_tie_break = NULL,
    rho_schedule = NULL,
    r_tune = NULL,
    potts_random_scan = TRUE,
    ...
) {
    gmde_run_mixture_mcmc(
        model = "potts", Y = Y, Fmat = Fmat, K = K,
        potts_W = potts_W, potts_beta = potts_beta,
        potts_random_scan = potts_random_scan,
        n_iter = n_iter, burn = burn,
        rho = rho, rho_grid = rho_grid, rho_warmup = rho_warmup,
        rho_tie_break = rho_tie_break, rho_schedule = rho_schedule,
        r_tune = r_tune, ...
    )
}
