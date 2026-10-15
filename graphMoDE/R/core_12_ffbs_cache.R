# Exact selected definitions from R/graphmode_dev.R
graphmode_dev_version <- "graphmode-cache-observation-20260914-v1"

graphmode_dev_bind <- function(fun, bindings) {
    stopifnot(is.function(fun), is.list(bindings), !is.null(names(bindings)),
              !anyDuplicated(names(bindings)), all(nzchar(names(bindings))))
    environment(fun) <- list2env(bindings, parent = environment(fun))
    fun
}

graphmode_dev_ffbs <- function(precision, natural, Fmat, m0, C0, G, W) {
    cached <- NULL
    backward <- function(mean, root, next_state, G, W_root) {
        if (is.null(cached)) {
            whitening <- graphmode_whitener(W_root)
            cached <<- list(G = G, W_root = W_root, whitening = whitening,
                design = whitening %*% G, precision = rep(1, nrow(G)))
        } else if (!identical(G, cached$G) || !identical(W_root, cached$W_root)) {
            stop("Evolution changed inside one cached FFBS call.", call. = FALSE)
        }
        graphmode_information_condition(mean, root, cached$design,
            cached$precision, as.numeric(cached$whitening %*% next_state))
    }
    driver <- graphmode_dev_bind(graphmode_ffbs,
        list(graphmode_backward_condition = backward))
    driver(precision, natural, Fmat, m0, C0, G, W)
}

graphmode_dev_allocation <- function(log_weights, before, after = NULL) {
    # Remove common offsets before forming log-sum-exp alternatives, so adding
    # log(K-1) to a huge shared log weight cannot erase relative category mass.
    log_weights <- graphmode_logsoftmax(graphmode_matrix(log_weights, "allocation log weights"))
    n <- nrow(log_weights); K <- ncol(log_weights)
    before <- gmde_validate_labels(before, n, K)
    if (!is.null(after)) after <- gmde_validate_labels(after, n, K)
    softplus <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))
    lse <- function(x) {
        if (!length(x) || all(x == -Inf)) return(-Inf)
        top <- max(x); top + log(sum(exp(x - top)))
    }
    log_stay <- numeric(n); log_switch <- rep(-Inf, n)
    if (K > 1L) for (i in seq_len(n)) {
        own <- log_weights[i, before[i]]
        alternative <- lse(log_weights[i, -before[i]])
        difference <- alternative - own
        if (!is.finite(difference)) stop("Allocation log-odds exceed numeric range.", call. = FALSE)
        log_stay[i] <- -softplus(difference)
        log_switch[i] <- -softplus(-difference)
    }
    probability <- exp(log_weights)
    list(expected_node_moves = sum(exp(log_switch)),
        log_expected_node_moves = lse(log_switch),
        log_probability_no_node_moves = sum(log_stay),
        log_switch_probability = log_switch,
        mean_max_probability = mean(apply(probability, 1L, max)),
        observed_node_moves = if (is.null(after)) NA_integer_ else sum(before != after),
        convergence_certified = FALSE,
        note = "One simultaneous categorical conditional; not a posterior mixing or convergence test.")
}

graphmode_dev_allocation_parts <- function(state, config, before) {
    if (config$method == "PottsMoDE")
        stop("Sequential Potts conditionals cannot be reconstructed as simultaneous weights.", call. = FALSE)
    eta <- gmde_eta(state$theta, config$Fmat)
    response <- graphmode_response_loglik(config$Y, eta, config$family, state$sigma2)
    if (config$adaptive) {
        gate <- graphmode_gate_utilities(state$x, state$v, config)
    } else {
        if (length(state$pi) != config$K || any(!is.finite(state$pi)) || any(state$pi <= 0) ||
            abs(sum(state$pi) - 1) > 1e-12) stop("Invalid retained mixture mass.", call. = FALSE)
        gate <- matrix(rep(log(state$pi), each = config$n), config$n, config$K)
    }
    list(combined = graphmode_dev_allocation(response + gate, before, state$Z),
        response_only = graphmode_dev_allocation(response, before),
        gate_only = graphmode_dev_allocation(gate, before))
}

graphmode_dev_expert <- function(current, Yk, config, sigma2 = NULL,
                               cache_ffbs = FALSE, observe = TRUE) {
    if (!is.logical(cache_ffbs) || length(cache_ffbs) != 1L || is.na(cache_ffbs) ||
        !is.logical(observe) || length(observe) != 1L || is.na(observe))
        stop("Specify logical cache/observation switches.", call. = FALSE)
    proposal <- NULL
    ffbs <- if (cache_ffbs) graphmode_dev_ffbs else graphmode_ffbs
    driver <- graphmode_dev_bind(graphmode_update_expert, list(
        graphmode_ffbs = function(...) {
            result <- ffbs(...)
            if (observe) proposal <<- result$theta
            result
        }))
    result <- driver(current, Yk, config, sigma2)
    started <- proc.time()[[3L]]
    observation <- NULL
    if (observe && nrow(Yk) && config$family == "poisson" && config$dynamics == "dynamic") {
        if (is.null(proposal)) stop("Missing actual dynamic proposal.", call. = FALSE)
        S <- colSums(Yk); N <- nrow(Yk)
        eta <- rowSums(current * config$Fmat)
        next_eta <- rowSums(proposal * config$Fmat)
        correction <- gmde_poisson_nb_log_ratio(S, gmde_mu_from_eta(N, next_eta), result$r) -
            gmde_poisson_nb_log_ratio(S, gmde_mu_from_eta(N, eta), result$r)
        total <- sum(correction)
        if (!is.finite(total) || !identical(min(0, total), result$log_acceptance))
            stop("Observed MH correction differs from actual expert decision.", call. = FALSE)
        observation <- list(schema = graphmode_dev_version, occupied_series = N,
            log_ratio = total, log_acceptance = result$log_acceptance,
            correction_by_time = correction,
            proposed_information_movement = sum(pmax(S, 1) * (next_eta - eta)^2),
            accepted_information_movement = result$movement,
            accepted = result$accepted,
            note = "Actual whole-path proposal; time terms are diagnostics, not block acceptances.")
        if (!is.finite(observation$proposed_information_movement))
            stop("Nonfinite diagnostic proposal movement.", call. = FALSE)
    }
    list(update = result, observation = observation,
        observation_seconds = proc.time()[[3L]] - started)
}
