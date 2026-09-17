# D-060: optional conditional ESS blocks in independent whitened coordinates.
# No frozen driver loads this file. This is an unregistered development kernel.
graphmode_gate_blocks_version <- "graphmode-gate-blocks-20260916-v1"

graphmode_gate_blocks_policy <- function(config, scheme) {
    graphmode_revalidate_config(config)
    if (!isTRUE(config$adaptive) || config$K < 2L ||
        !config$guidance %in% c("class-specific", "shared"))
        stop("Gate blocks require free adaptive guidance.", call. = FALSE)
    if (!is.character(scheme) || length(scheme) != 1L || is.na(scheme) ||
        !scheme %in% c("full", "contrast"))
        stop("Choose full or contrast explicitly; no automatic selection.", call. = FALSE)
    d <- config$K - 1L; q <- ncol(config$Phi); n <- config$n
    dimension <- graphmode_gate_dimension(config)
    blocks <- if (scheme == "full") list(seq_len(dimension)) else
        lapply(seq_len(d), function(h) as.integer(c(h,
            d + (h - 1L) * q + seq_len(q),
            d + q * d + (h - 1L) * n + seq_len(n))))
    if (!identical(sort(unlist(blocks, use.names = FALSE)), seq_len(dimension)))
        stop("Gate blocks must cover every white coordinate exactly once.", call. = FALSE)
    list(schema = graphmode_gate_blocks_version, scheme = scheme,
        dimension = dimension, blocks = blocks, order = "fixed-forward",
        budget = "shared max_ess_steps across the complete block scan",
        adaptation = "none")
}

graphmode_gate_blocks_validate <- function(config, policy) {
    if (!is.list(policy) || !identical(policy,
        graphmode_gate_blocks_policy(config, policy$scheme)))
        stop("Changed or invalid gate-block policy.", call. = FALSE)
    invisible(TRUE)
}

graphmode_gate_blocks_ess <- function(x, v, Z, config, policy) {
    graphmode_gate_blocks_validate(config, policy)
    Z <- gmde_validate_labels(Z, config$n, config$K)
    utilities <- graphmode_gate_utilities(x, v, config)
    if (length(policy$blocks) == 1L) {
        # Preserve the original whole-vector kernel and its exact draw order.
        out <- graphmode_gate_ess(x, v, Z, config)
        out$blocks <- list(list(block = 1L, evaluations = out$evaluations,
            jump_squared = sum((out$x - x)^2)))
        return(out)
    }
    records <- vector("list", length(policy$blocks)); evaluations <- 0L
    for (h in seq_along(policy$blocks)) {
        if (evaluations >= config$max_ess_steps)
            stop("Gate block scan exceeded its shared bracket budget; no partial return.", call. = FALSE)
        indices <- policy$blocks[[h]]; before <- x[indices]
        threshold <- graphmode_gate_loglik(utilities, Z) + log(graphmode_uniform(1L))
        direction <- graphmode_normal(length(indices))
        # The complement is FIXED. Only the selected coordinates rotate.
        block_x <- numeric(length(x)); block_x[indices] <- before
        block_direction <- numeric(length(x)); block_direction[indices] <- direction
        Fblock <- graphmode_gate_utilities(block_x, v, config)
        Fdirection <- graphmode_gate_utilities(block_direction, v, config)
        Ffixed <- utilities - Fblock
        angle <- 2 * pi * graphmode_uniform(1L)
        lower <- angle - 2 * pi; upper <- angle
        start_count <- evaluations
        repeat {
            if (evaluations >= config$max_ess_steps)
                stop("Gate block scan exceeded its shared bracket budget; no partial return.", call. = FALSE)
            candidate <- Ffixed + Fblock * cos(angle) + Fdirection * sin(angle)
            evaluations <- evaluations + 1L
            if (graphmode_gate_loglik(candidate, Z) >= threshold) {
                x[indices] <- before * cos(angle) + direction * sin(angle)
                # Canonical evaluation avoids accumulated cached-map rounding
                # across the fixed scan. No utility/guidance sorting or clipping.
                utilities <- graphmode_gate_utilities(x, v, config)
                if (!isTRUE(all.equal(candidate, utilities, tolerance = 1e-12)))
                    stop("Gate block linear utility map mismatch.", call. = FALSE)
                records[[h]] <- list(block = h, evaluations = evaluations - start_count,
                    jump_squared = sum((x[indices] - before)^2))
                break
            }
            if (angle < 0) lower <- angle else upper <- angle
            angle <- lower + (upper - lower) * graphmode_uniform(1L)
        }
    }
    list(x = x, utilities = utilities, evaluations = evaluations, blocks = records)
}

# Reuse the original fixed-m ESS/MH composition and its count/state checks.
# One block scan is followed by ONE full coordinate-MH scan per inner pass.
graphmode_gate_blocks_refresh <- function(x, v, Z, config, gate_policy, ess_policy) {
    graphmode_gate_blocks_validate(config, ess_policy)
    scans <- list()
    driver <- graphmode_dev_bind(graphmode_gate_refresh, list(
        graphmode_gate_ess = function(x, v, Z, config) {
            out <- graphmode_gate_blocks_ess(x, v, Z, config, ess_policy)
            scans[[length(scans) + 1L]] <<- out$blocks
            out
        }))
    out <- driver(x, v, Z, config, gate_policy)
    if (length(scans) != gate_policy$inner_steps ||
        sum(vapply(scans, function(s) sum(vapply(s, `[[`, numeric(1), "evaluations")), numeric(1))) !=
        out$counts$ess_evaluations)
        stop("Incomplete gate-block scan accounting.", call. = FALSE)
    out$schema <- graphmode_gate_blocks_version
    out$ess_policy <- ess_policy; out$ess_scans <- scans
    out
}

# Optional complete scientific step, not an executor/registration/resume path.
# Original expert time blocks, guidance MH, and final allocation are reused.
graphmode_gate_blocks_sweep <- function(state, config, gate_policy, expert_policy,
                                      ess_policy, cache_ffbs) {
    graphmode_gate_blocks_validate(config, ess_policy)
    refresh <- NULL
    outer <- graphmode_dev_bind(graphmode_gate_refresh_sweep, list(
        graphmode_gate_refresh = function(x, v, Z, config, policy) {
            refresh <<- graphmode_gate_blocks_refresh(x, v, Z, config, policy, ess_policy)
            refresh
        }))
    driver <- graphmode_dev_bind(graphmode_expert_blocks_sweep,
        list(graphmode_gate_refresh_sweep = outer))
    out <- driver(state, config, gate_policy, expert_policy, cache_ffbs)
    if (is.null(refresh) || !identical(out$transition$state$x, refresh$x) ||
        !identical(out$transition$state$v, refresh$v))
        stop("Gate-block outer handoff mismatch.", call. = FALSE)
    out$schema <- graphmode_gate_blocks_version
    out$gate_refresh <- refresh[c("schema", "records", "counts", "policy",
        "inner_states_are_retained_draws", "ess_policy", "ess_scans")]
    out$ess_policy <- ess_policy
    out
}
