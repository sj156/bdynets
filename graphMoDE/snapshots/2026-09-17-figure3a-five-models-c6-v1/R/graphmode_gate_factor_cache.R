# D-065: opt-in, per-ESS reuse of guidance factors, not a new transition rule.
# Underscore name keeps every frozen source glob and executor unchanged.
graphmode_gate_factor_cache_version <- "graphmode-gate-factor-cache-20260916-v1"

# One private context lives for one conditional block scan only. The original
# utility algebra (including matrix multiplication order) is reused verbatim.
graphmode_gate_factor_cache_context <- function() {
    key <- value <- NULL
    requests <- builds <- 0L
    factors <- function(v, K, guidance) {
        current <- list(v = v, K = K, guidance = guidance)
        if (is.null(value)) {
            value <<- graphmode_guidance_factors(v, K, guidance)
            key <<- current
            builds <<- builds + 1L
        } else if (!identical(current, key)) {
            stop("Guidance factor cache crossed its fixed-v scan; no stale reuse.", call. = FALSE)
        }
        requests <<- requests + 1L
        value
    }
    list(utilities = graphmode_dev_bind(graphmode_gate_utilities,
             list(graphmode_guidance_factors = factors)),
         counts = function() list(enabled = TRUE, requests = requests,
             builds = builds, hits = requests - builds))
}

graphmode_gate_factor_cache_ess <- function(x, v, Z, config, policy) {
    # Leave full-vector / one-block behavior completely in the original path.
    # The original entry validates the entire policy before consuming draws.
    if (is.list(policy) && length(policy$blocks) == 1L) {
        out <- graphmode_gate_blocks_ess(x, v, Z, config, policy)
        out$factor_cache <- list(enabled = FALSE, requests = 0L, builds = 0L, hits = 0L)
        return(out)
    }
    context <- graphmode_gate_factor_cache_context()
    driver <- graphmode_dev_bind(graphmode_gate_blocks_ess,
        list(graphmode_gate_utilities = context$utilities))
    out <- driver(x, v, Z, config, policy)
    counts <- context$counts()
    if (counts$builds != 1L || counts$requests != 1L + 3L * length(policy$blocks))
        stop("Incomplete per-scan guidance factor accounting.", call. = FALSE)
    out$factor_cache <- counts
    out
}

# D-060/D-050 still perform every ESS/MH/count/state check. A new context is
# constructed after each MH scan, even if the resulting v happens to be equal.
graphmode_gate_factor_cache_refresh <- function(x, v, Z, config, gate_policy, ess_policy) {
    records <- list()
    driver <- graphmode_dev_bind(graphmode_gate_blocks_refresh,
        list(graphmode_gate_blocks_ess = function(x, v, Z, config, policy) {
            out <- graphmode_gate_factor_cache_ess(x, v, Z, config, policy)
            records[[length(records) + 1L]] <<- out$factor_cache
            out
        }))
    out <- driver(x, v, Z, config, gate_policy, ess_policy)
    if (length(records) != gate_policy$inner_steps)
        stop("Incomplete per-refresh guidance factor accounting.", call. = FALSE)
    out$schema <- graphmode_gate_factor_cache_version
    out$factor_cache <- records
    out
}

# Optional complete-step candidate only. No old executor loads this module;
# distinct envelopes prevent the optimization from claiming frozen identity.
graphmode_gate_factor_cache_sweep <- function(state, config, gate_policy, expert_policy,
                                            ess_policy, cache_ffbs) {
    refresh <- NULL
    driver <- graphmode_dev_bind(graphmode_gate_blocks_sweep,
        list(graphmode_gate_blocks_refresh = function(x, v, Z, config, gate_policy, ess_policy) {
            if (!is.null(refresh)) stop("Unexpected repeated gate refresh.", call. = FALSE)
            refresh <<- graphmode_gate_factor_cache_refresh(x, v, Z, config, gate_policy, ess_policy)
            refresh
        }))
    out <- driver(state, config, gate_policy, expert_policy, ess_policy, cache_ffbs)
    if (is.null(refresh)) stop("Missing cached gate refresh.", call. = FALSE)
    out$schema <- graphmode_gate_factor_cache_version
    out$gate_refresh$factor_cache <- refresh$factor_cache
    out
}
