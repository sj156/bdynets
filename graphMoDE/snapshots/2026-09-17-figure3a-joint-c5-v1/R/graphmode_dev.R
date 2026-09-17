# D-043: opt-in performance/observation layer over the frozen r4 source.
# The underscore filename is intentional: old source globs/registrations do
# not load this layer. No package export, default-kernel change or run authority.
graphmode_dev_version <- "graphmode-cache-observation-20260914-v1"

# Rebind only an explicitly named dependency in a private call environment.
# Never assign into the old source environment or installed package namespace.
graphmode_dev_bind <- function(fun, bindings) {
    stopifnot(is.function(fun), is.list(bindings), !is.null(names(bindings)),
              !anyDuplicated(names(bindings)), all(nzchar(names(bindings))))
    environment(fun) <- list2env(bindings, parent = environment(fun))
    fun
}

# The original forward/backward driver and information-condition/root audit
# are reused verbatim. Within ONE FFBS call W and G are fixed, so the backward
# observation rows W_root^-1 G need to be computed once, not T-1 times.
# No cross-call cache: a new W/G or a new expert cannot reuse stale factors.
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

# Stable alternatives-to-current mass. Summing the alternative probabilities
# avoids 1-p cancellation when an allocation is essentially deterministic.
# Log expected movement remains informative even when exp(log p) underflows.
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

# Allocation conditionals used in a non-Potts sweep, conditional on the just-
# updated experts and gate. Response-only/gate-only are descriptive ablations,
# not new fitted models or oracle calibration. No true labels are consulted.
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

# Invoke the existing expert kernel with an optional cached FFBS dependency;
# capture the ACTUAL proposal, including rejected ones, without a second draw.
# Observation timing is separate from the original complete expert-update time.
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

# An explicit development step, NOT a replacement for any registered runner.
# Reuses the old whole-path/gate/allocation driver and bounded warmup controller.
# Rich records stay outside old result schemas, so old rho reducers cannot
# accidentally treat them as accepted v3 run evidence.
graphmode_dev_sweep <- function(state, config, control,
                               cache_ffbs = FALSE, observe = TRUE) {
    if (config$family != "poisson" || config$dynamics != "dynamic" || !config$adaptive)
        stop("This development step currently supports dynamic Poisson adaptive gates only.", call. = FALSE)
    graphmode_revalidate_config(config)
    if (!is.logical(cache_ffbs) || length(cache_ffbs) != 1L || is.na(cache_ffbs) ||
        !is.logical(observe) || length(observe) != 1L || is.na(observe))
        stop("Specify logical cache/observation switches.", call. = FALSE)
    records <- list(); observation_seconds <- 0
    expert <- function(current, Yk, config, sigma2 = NULL) {
        x <- graphmode_dev_expert(current, Yk, config, sigma2, cache_ffbs, observe)
        records[[length(records) + 1L]] <<- list(expert = length(records) + 1L,
            observation = x$observation, seconds = x$observation_seconds)
        observation_seconds <<- observation_seconds + x$observation_seconds
        x$update
    }
    sweep <- graphmode_dev_bind(graphmode_sweep, list(graphmode_update_expert = expert))
    controlled <- graphmode_dev_bind(graphmode4_controlled_sweep, list(graphmode_sweep = sweep))
    result <- controlled(state, config, control)
    started <- proc.time()[[3L]]
    allocation <- if (observe) graphmode_dev_allocation_parts(result$out$state, config, state$Z) else NULL
    guidance <- if (observe) list(logit_jump_squared = sum((result$out$state$v - state$v)^2),
        weight_jump_squared = sum((plogis(result$out$state$v) - plogis(state$v))^2),
        scale = result$out$tuning, acceptance = result$out$guidance_accept) else NULL
    list(schema = graphmode_dev_version, transition = result, cache_ffbs = cache_ffbs,
        observation = if (observe) list(experts = records, allocation = allocation, guidance = guidance) else NULL,
        observation_seconds = observation_seconds + proc.time()[[3L]] - started,
        run_registered = FALSE, convergence_certified = FALSE, formal_authorized = FALSE)
}

# Only retained Poisson evidence; no pooling arbitrary expert labels across
# chains and no reporting empty-expert prior refreshes as rejected proposals.
graphmode_dev_mh_profile <- function(diagnostics, warmup) {
    old <- graphmode4_movement_report(diagnostics, warmup)
    retained <- diagnostics[seq.int(warmup + 1L, length(diagnostics))]
    profiles <- lapply(seq_len(nrow(old$experts)), function(k) {
        e <- Filter(function(x) !x$empty, lapply(retained, function(d) d$expert[[k]]))
        log_alpha <- vapply(e, `[[`, numeric(1), "log_acceptance")
        if (any(!is.finite(log_alpha)) || any(log_alpha > 0))
            stop("Invalid retained log MH probabilities.", call. = FALSE)
        data.frame(expert = k, proposals = length(e),
            mean_accept_probability = if (length(e)) mean(exp(log_alpha)) else NA_real_,
            median_log_acceptance = if (length(e)) median(log_alpha) else NA_real_,
            minimum_log_acceptance = if (length(e)) min(log_alpha) else NA_real_)
    })
    list(experts = cbind(old$experts, do.call(rbind, profiles)[, -1L, drop = FALSE]),
        convergence_certified = FALSE)
}

# Read-only investigation of already completed D-041 evidence. No old active-run
# guard is called with today's HEAD; original signed plan/result/exit/receipt
# identities are validated instead. No file is saved back to the experiment.
graphmode_dev_evidence_header <- function(screen, registration, batch, report) {
    unsigned <- registration; unsigned$signature <- NULL
    jobs <- names(screen$jobs)
    if (!is.list(registration) || !identical(registration$signature, graphmode_digest(unsigned)) ||
        !identical(batch$status, 0L) ||
        !identical(batch$registration_signature, registration$signature) ||
        !identical(report$registration_signature, registration$signature) ||
        !identical(report$screen_signature, screen$signature) ||
        !length(jobs) || anyDuplicated(jobs) ||
        !identical(report$dispatched, jobs) ||
        !identical(report$dispatch_status, setNames(rep(0L, length(jobs)), jobs)) ||
        !is.null(report$stop_reason))
        stop("Incomplete or mismatched batch/registration/screen evidence.", call. = FALSE)
    invisible(TRUE)
}

graphmode_dev_inspect <- function(directory) {
    directory <- normalizePath(directory, mustWork = TRUE)
    screen <- readRDS(file.path(directory, "run", "screen.rds"))
    unsigned <- screen; unsigned$signature <- NULL
    if (!identical(screen$schema, "graphmode-r4-rho-screen-20260912-v3") ||
        !identical(screen$signature, graphmode_digest(unsigned)))
        stop("Incompatible or changed screen.", call. = FALSE)
    batch <- readRDS(file.path(directory, "batch-execution.rds"))
    registration <- readRDS(file.path(directory, "registration.rds"))
    report <- readRDS(file.path(directory, "run", "report.rds"))
    graphmode_dev_evidence_header(screen, registration, batch, report)
    if (!identical(registration$directory, directory) ||
        !identical(registration$output_dir, file.path(directory, "run")))
        stop("Registered result tree moved.", call. = FALSE)
    rows <- profiles <- hashes <- list()
    for (id in names(screen$jobs)) {
        job <- screen$jobs[[id]]; plan <- job$plan
        graphmode4_controlled_validate(plan)
        expected <- normalizePath(file.path(directory, "run", id), mustWork = TRUE)
        if (!identical(plan$output_dir, expected) || plan$core_plan$thin != 1L)
            stop("Unexpected result location or thinned terminal evidence.", call. = FALSE)
        evidence <- graphmode4_controlled_evidence(plan)
        graphmode4_controlled_evidence_check(plan, evidence)
        dispatcher <- readRDS(file.path(directory, "run", paste0("DISPATCH-", id, ".rds")))
        if (!identical(dispatcher$status, 0L)) stop("Dispatcher failed.", call. = FALSE)
        cp <- evidence$result$checkpoint; config <- plan$core_plan$config
        last <- tail(cp$saved, 1L)[[1L]]
        if (length(cp$saved) < 2L ||
            cp$saved[[length(cp$saved) - 1L]]$iteration != cp$state$iteration - 1L ||
            !identical(last$iteration, cp$state$iteration) ||
            !all(vapply(c("Z", "theta", "v", "pi", "sigma2"),
                function(n) identical(last[[n]], cp$state[[n]]), logical(1))))
            stop("Missing previous-iteration labels for terminal allocation.", call. = FALSE)
        before <- cp$saved[[length(cp$saved) - 1L]]$Z
        a <- graphmode_dev_allocation_parts(cp$state, config, before)
        if (a$combined$observed_node_moves != tail(cp$diagnostics, 1L)[[1L]]$events[["node_moves"]])
            stop("Terminal allocation event mismatch.", call. = FALSE)
        profiles[[id]] <- graphmode_dev_mh_profile(cp$diagnostics, plan$core_plan$warmup)
        rows[[id]] <- data.frame(id = id, rho = job$rho, start = job$start,
            terminal_iteration = cp$state$iteration,
            expected_moves = a$combined$expected_node_moves,
            log_expected_moves = a$combined$log_expected_node_moves,
            log_probability_no_moves = a$combined$log_probability_no_node_moves,
            response_only_log_expected_moves = a$response_only$log_expected_node_moves,
            gate_only_expected_moves = a$gate_only$expected_node_moves,
            observed_moves = a$combined$observed_node_moves,
            guidance_sd = cp$control$sd, guidance_adapted = cp$control$updates > 0L)
        hashes[[id]] <- digest::digest(file = file.path(expected, "result.rds"), algo = "sha256", serialize = FALSE)
    }
    list(schema = graphmode_dev_version, terminal_conditionals = do.call(rbind, rows),
        mh_profiles = profiles, result_sha256 = hashes, original_screen_signature = screen$signature,
        note = "One final-sweep conditional per branch, not all retained sweeps or proof of posterior concentration. No draws or writes.",
        rho_selected = FALSE, convergence_certified = FALSE, formal_authorized = FALSE)
}
