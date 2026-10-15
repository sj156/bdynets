# Exact selected definitions from R/graphmode_joint_run.R
graphmode_joint_run_json <- function(value, path, exclusive = FALSE) {
    if (exclusive && file.exists(path)) stop("Output already exists: ", path, call. = FALSE)
    temporary <- paste0(path, ".writing")
    jsonlite::write_json(value, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 16)
    if (!file.rename(temporary, path)) stop("Atomic JSON publication failed.", call. = FALSE)
}

graphmode_joint_run_step <- function(state, config, base_policy, joint_policy, enabled, every) {
    base <- graphmode_gate_cache_run_step(state, config, base_policy)
    scored <- NULL; joint <- NULL; final <- base$state
    if (enabled && base$state$iteration %% every == 0L) {
        step <- graphmode_dev_bind(graphmode_joint_partition_step, list(
            graphmode_joint_partition_score = function(...) {
                scored <<- graphmode_joint_partition_score(...); scored
            }))
        joint <- step(base$state, config, joint_policy); final <- joint$state
    }
    list(state = final, base_state = base$state, base_diagnostic = base$diagnostic,
        joint = joint, candidate = if (is.null(scored)) NULL else scored$candidate,
        events = graphmode_allocation_events(state$Z, final$Z, config$K))
}

graphmode_joint_run_check_step <- function(before, out, config, record, enabled) {
    base <- out$base_state; d <- out$base_diagnostic; final <- out$state
    if (base$iteration != before$iteration + 1L || final$iteration != base$iteration ||
        !identical(d$before_signature, graphmode_digest(before)) || !identical(d$after_signature, graphmode_digest(base)))
        stop("Base state handoff mismatch.", call. = FALSE)
    graphmode_gate_cache_run_gate_check(d$gate, config, record$base_policy)
    for (k in seq_len(config$K)) graphmode_blocks_run_expert_check(d$expert[[k]],
        matrix(before$theta[k, , ], ncol(config$Y), ncol(config$Fmat)),
        matrix(base$theta[k, , ], ncol(config$Y), ncol(config$Fmat)),
        config$Y[before$Z == k, , drop = FALSE], config, record$base_policy$block,
        d$block_offset, record$thresholds)
    scheduled <- enabled && base$iteration %% record$spec$joint_every == 0L
    if (scheduled != !is.null(out$joint)) stop("Joint schedule mismatch.", call. = FALSE)
    if (!scheduled && !identical(base, final)) stop("Unscheduled scientific change.", call. = FALSE)
    if (scheduled) {
        j <- out$joint
        if (!identical(final, j$state) || !identical(final$v, base$v) ||
            !identical(j$sizes, tabulate(final$Z, config$K)) ||
            !identical(j$eta, gmde_eta(final$theta, config$Fmat)) ||
            !identical(j$utilities, graphmode_gate_utilities(final$x, final$v, config)))
            stop("Joint state/derived-value handoff mismatch.", call. = FALSE)
        if (j$move == "self") {
            if (j$accepted || !is.null(out$candidate) || !identical(final, base)) stop("Invalid self transition.", call. = FALSE)
        } else {
            if (is.null(out$candidate) || !identical(j$accepted, j$log_uniform <= min(0, j$log_ratio)) ||
                !identical(final, if (j$accepted) out$candidate else base)) stop("Nonatomic joint decision.", call. = FALSE)
        }
    }
    if (!identical(out$events, graphmode_allocation_events(before$Z, final$Z, config$K)))
        stop("Final allocation events mismatch.", call. = FALSE)
    invisible(TRUE)
}
