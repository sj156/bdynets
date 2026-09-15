# D-050: optional fixed-count conditional gate refreshes. Frozen mathematical
# functions stay unchanged. This is NOT a registered runner or a convergence fix.
graphmode_gate_refresh_version <- "graphmode-gate-refresh-20260915-v1"

graphmode_gate_refresh_policy <- function(inner_steps, guidance_proposal_sd) {
    inner_steps <- gmde_scalar_integer(inner_steps, "fixed gate inner_steps", 1L, 64L)
    graphmode_positive(guidance_proposal_sd, "fixed guidance proposal scale")
    list(schema = graphmode_gate_refresh_version, inner_steps = inner_steps,
        guidance_proposal_sd = guidance_proposal_sd, adaptation = "none")
}

graphmode_gate_refresh_validate <- function(config, policy) {
    graphmode_revalidate_config(config)
    if (!is.list(policy) || !identical(policy, graphmode_gate_refresh_policy(
        policy$inner_steps, policy$guidance_proposal_sd)))
        stop("Invalid or changed fixed gate-refresh policy.", call. = FALSE)
    if (!isTRUE(config$adaptive) || config$family != "poisson" || config$dynamics != "dynamic" ||
        !config$guidance %in% c("class-specific", "shared") || config$K < 2L)
        stop("Gate refresh currently supports dynamic Poisson with free adaptive guidance only.", call. = FALSE)
    if (!identical(config$guidance_proposal_sd, policy$guidance_proposal_sd))
        stop("Config/policy scales differ; no silent override or adaptation.", call. = FALSE)
    invisible(TRUE)
}

# Fixed Z throughout: [ESS(x | v,Z), original coordinate MH(v | x,Z)]^m.
# No sorting of v, empty-class prior refresh, skipping, stopping on acceptance,
# extra allocation draw, or adaptive choice of m. Only final x/v are retained.
graphmode_gate_refresh <- function(x, v, Z, config, policy) {
    graphmode_gate_refresh_validate(config, policy)
    Z <- gmde_validate_labels(Z, config$n, config$K)
    graphmode_gate_utilities(x, v, config) # validate before consuming a draw
    original_Z <- Z
    free <- length(v)
    records <- vector("list", policy$inner_steps)
    for (j in seq_len(policy$inner_steps)) {
        before_x <- x; before_v <- v
        started <- proc.time()[[3L]]
        gate <- graphmode_gate_ess(x, v, Z, config)
        ess_seconds <- proc.time()[[3L]] - started
        if (!is.list(gate) || !is.numeric(gate$x) || length(gate$x) != length(x) ||
            any(!is.finite(gate$x)) || length(gate$evaluations) != 1L ||
            !is.numeric(gate$evaluations) || !is.finite(gate$evaluations) ||
            gate$evaluations < 1 || gate$evaluations > config$max_ess_steps ||
            gate$evaluations != floor(gate$evaluations))
            stop("Invalid gate ESS result; no partial-state return.", call. = FALSE)
        started <- proc.time()[[3L]]
        guided <- graphmode_guidance_update(gate$x, v, Z, config)
        guidance_seconds <- proc.time()[[3L]] - started
        if (!is.list(guided) || !is.numeric(guided$v) || length(guided$v) != free ||
            any(!is.finite(guided$v)) || !is.logical(guided$accepted) ||
            length(guided$accepted) != free || anyNA(guided$accepted) ||
            any(guided$v[!guided$accepted] != v[!guided$accepted]) ||
            !identical(guided$utilities, graphmode_gate_utilities(gate$x, guided$v, config)))
            stop("Invalid guidance update/state accounting; no partial-state return.", call. = FALSE)
        x <- gate$x; v <- guided$v
        records[[j]] <- list(inner_step = j, accepted = guided$accepted,
            proposals = rep(1L, free), ess_evaluations = gate$evaluations,
            x_jump_squared = sum((x - before_x)^2),
            logit_jump_squared = (v - before_v)^2,
            weight_jump_squared = (stats::plogis(v) - stats::plogis(before_v))^2,
            ess_seconds = ess_seconds, guidance_seconds = guidance_seconds)
        if (any(!is.finite(c(records[[j]]$x_jump_squared, records[[j]]$logit_jump_squared,
            records[[j]]$weight_jump_squared, ess_seconds, guidance_seconds))))
            stop("Nonfinite gate-refresh diagnostic.", call. = FALSE)
    }
    if (!identical(original_Z, Z)) stop("Allocations changed inside conditional refresh.", call. = FALSE)
    counts <- graphmode_gate_refresh_counts(records, policy, free)
    list(schema = graphmode_gate_refresh_version, x = x, v = v,
        utilities = guided$utilities, records = records, counts = counts,
        policy = policy, inner_states_are_retained_draws = FALSE)
}

graphmode_gate_refresh_counts <- function(records, policy, free) {
    free <- gmde_scalar_integer(free, "free guidance coordinates", 1L)
    if (!is.list(policy) || !identical(policy, graphmode_gate_refresh_policy(
        policy$inner_steps, policy$guidance_proposal_sd)) ||
        !is.list(records) || length(records) != policy$inner_steps)
        stop("Incomplete fixed-count gate records.", call. = FALSE)
    for (j in seq_along(records)) {
        r <- records[[j]]
        if (!is.list(r) || !identical(r$inner_step, j) || !is.logical(r$accepted) ||
            length(r$accepted) != free || anyNA(r$accepted) ||
            !identical(r$proposals, rep(1L, free)) ||
            !is.numeric(r$ess_evaluations) || length(r$ess_evaluations) != 1L ||
            !is.finite(r$ess_evaluations) || r$ess_evaluations < 1 ||
            r$ess_evaluations != floor(r$ess_evaluations))
            stop("Invalid per-substep proposal/acceptance records.", call. = FALSE)
    }
    accepted <- do.call(rbind, lapply(records, `[[`, "accepted"))
    proposals <- do.call(rbind, lapply(records, `[[`, "proposals"))
    list(inner_steps = policy$inner_steps, proposals_by_coordinate = colSums(proposals),
        accepted_by_coordinate = colSums(accepted),
        acceptance_by_coordinate = colSums(accepted) / colSums(proposals),
        total_proposals = sum(proposals), total_accepted = sum(accepted),
        acceptance = sum(accepted) / sum(proposals),
        ess_evaluations = sum(vapply(records, `[[`, numeric(1), "ess_evaluations")))
}

# A full outer sweep: one unchanged expert update per expert, m conditional
# x/v refreshes at fixed Z, then ONE original categorical allocation update.
# Private bindings leave graphmode_sweep and all original functions untouched.
# The envelope is deliberately incompatible with old controlled/warmup runners:
# their one-update acceptance accounting must not consume these multi-updates.
graphmode_gate_refresh_sweep <- function(state, config, policy, cache_ffbs) {
    graphmode_gate_refresh_validate(config, policy)
    if (!is.logical(cache_ffbs) || length(cache_ffbs) != 1L || is.na(cache_ffbs))
        stop("Specify the FFBS cache switch explicitly.", call. = FALSE)
    if (!is.list(state) || !identical(state$sampler_version, graphmode_sampler_version))
        stop("Invalid state schema.", call. = FALSE)
    iteration <- gmde_scalar_integer(state$iteration, "outer iteration", 0L)
    checked <- graphmode_initial_state(config, state$Z, state$theta, state$sigma2, state$x, state$v)
    checked$iteration <- iteration
    if (!identical(checked, state)) stop("Inconsistent outer state.", call. = FALSE)
    refresh <- NULL; gate_calls <- guidance_calls <- expert_calls <- 0L
    expert_observations <- vector("list", config$K)
    bindings <- list(
        graphmode_gate_ess = function(x, v, Z, config) {
            gate_calls <<- gate_calls + 1L
            if (gate_calls != 1L) stop("Unexpected outer gate call count.", call. = FALSE)
            refresh <<- graphmode_gate_refresh(x, v, Z, config, policy)
            list(x = refresh$x, utilities = refresh$utilities,
                evaluations = refresh$counts$ess_evaluations)
        },
        graphmode_guidance_update = function(x, v, Z, config) {
            guidance_calls <<- guidance_calls + 1L
            if (guidance_calls != 1L || is.null(refresh) || !identical(x, refresh$x) ||
                !identical(v, state$v) || !identical(Z, state$Z))
                stop("Outer/inner gate state handoff mismatch.", call. = FALSE)
            # Do not draw a second guidance update or expose a last-pass-only
            # acceptance rate. Correct counts live in the new envelope below.
            list(v = refresh$v, utilities = refresh$utilities, accepted = logical())
        },
        graphmode_update_expert = function(current, Yk, config, sigma2 = NULL) {
            expert_calls <<- expert_calls + 1L
            e <- graphmode_dev_expert(current, Yk, config, sigma2,
                cache_ffbs = cache_ffbs, observe = TRUE)
            expert_observations[[expert_calls]] <<- list(expert = expert_calls,
                observation = e$observation, observation_seconds = e$observation_seconds)
            e$update
        })
    driver <- graphmode_dev_bind(graphmode_sweep, bindings)
    out <- driver(state, config)
    if (gate_calls != 1L || guidance_calls != 1L || expert_calls != config$K ||
        !identical(out$state$x, refresh$x) || !identical(out$state$v, refresh$v) ||
        !identical(out$utilities, refresh$utilities) || out$state$iteration != iteration + 1L)
        stop("Incomplete outer-sweep gate integration.", call. = FALSE)
    out$guidance_accept <- NULL
    out$ess_evaluations <- NULL
    list(schema = graphmode_gate_refresh_version, transition = out,
        gate_refresh = refresh[c("schema", "records", "counts", "policy", "inner_states_are_retained_draws")],
        expert_observations = expert_observations,
        policy = policy, config_signature = graphmode_digest(config), cache_ffbs = cache_ffbs,
        expert_update_count = expert_calls, completed_outer_steps = 1L,
        run_registered = FALSE, convergence_certified = FALSE, formal_authorized = FALSE)
}
