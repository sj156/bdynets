# Exact selected definitions from R/graphmode_gate_cache_run.R
graphmode_gate_cache_run_step <- function(state, config, policy) {
    if (!identical(policy$ess_scheme, "contrast") || !is.logical(policy$factor_cache) ||
        length(policy$factor_cache) != 1L || is.na(policy$factor_cache))
        stop("Explicit contrast/cache policy required.", call. = FALSE)
    ess <- graphmode_gate_blocks_policy(config, policy$ess_scheme)
    sweep <- if (policy$factor_cache) graphmode_gate_factor_cache_sweep else graphmode_gate_blocks_sweep
    step <- graphmode_dev_bind(graphmode_blocks_run_step, list(
        graphmode_expert_blocks_sweep = function(state, config, gate_policy, expert_policy, cache_ffbs)
            sweep(state, config, gate_policy, expert_policy, ess, cache_ffbs)))
    step(state, config, policy)
}

graphmode_gate_cache_run_gate_check <- function(g, config, policy) {
    if (!identical(policy$ess_scheme, "contrast") || !is.logical(policy$factor_cache) ||
        length(policy$factor_cache) != 1L || is.na(policy$factor_cache))
        stop("Invalid cache evidence policy.", call. = FALSE)
    if (policy$factor_cache) {
        nb <- length(graphmode_gate_blocks_policy(config, "contrast")$blocks)
        expected <- if (nb == 1L) list(enabled = FALSE, requests = 0L, builds = 0L, hits = 0L) else
            list(enabled = TRUE, requests = 1L + 3L * nb, builds = 1L, hits = 3L * nb)
        if (!identical(g$schema, graphmode_gate_factor_cache_version) ||
            !identical(g$factor_cache, rep(list(expected), policy$gate$inner_steps)))
            stop("Invalid per-scan cache schema/counts.", call. = FALSE)
        g$schema <- graphmode_gate_blocks_version; g$factor_cache <- NULL
    } else if ("factor_cache" %in% names(g)) {
        stop("Reference arm cannot claim cached evidence.", call. = FALSE)
    }
    graphmode_gate_blocks_run_gate_check(g, config, policy)
    invisible(TRUE)
}
