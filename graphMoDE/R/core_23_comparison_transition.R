# Exact selected definitions from R/graphmode_compare_run.R
graphmode_compare_step <- function(state, config, policy) {
    if (config$adaptive) return(graphmode_joint_run_step(state, config, policy, NULL, FALSE, 10L))
    if (!config$method %in% c("MoDE", "PottsMoDE")) stop("Unknown nonadaptive method.", call. = FALSE)
    started <- proc.time()[[3L]]; offset <- NULL; calls <- 0L
    expert <- function(current, Yk, config, sigma2 = NULL) {
        if (!is.null(sigma2)) stop("This comparison uses Poisson experts.", call. = FALSE)
        if (is.null(offset)) offset <<- graphmode_expert_blocks_offset(nrow(config$Fmat), policy$block)
        calls <<- calls + 1L
        graphmode_expert_blocks_expert(current, Yk, config, policy$block, offset, TRUE)$update
    }
    # MoDE Dirichlet/Gibbs and Potts sequential Gibbs remain in the original driver.
    driver <- graphmode_dev_bind(graphmode_sweep, list(graphmode_update_expert = expert))
    out <- driver(state, config)
    if (calls != config$K) stop("Incomplete expert scan.", call. = FALSE)
    next_state <- out$state
    d <- list(iteration = next_state$iteration, policy = policy, block_offset = offset,
        before_signature = graphmode_digest(state), after_signature = graphmode_digest(next_state),
        expert = lapply(out$expert_updates, function(e) { e$theta <- NULL; e }),
        gate = NULL, potts_gross = out$potts_gross, events = out$events,
        transition_seconds = proc.time()[[3L]] - started)
    list(state = next_state, base_state = next_state, base_diagnostic = d,
        joint = NULL, candidate = NULL, events = out$events)
}

graphmode_compare_check <- function(before, out, config, record) {
    if (config$adaptive) {
        graphmode_joint_run_check_step(before, out, config, record, FALSE)
        return(invisible(TRUE))
    }
    s <- out$state; d <- out$base_diagnostic
    rebuilt <- graphmode_initial_state(config, s$Z, s$theta)
    rebuilt$iteration <- before$iteration + 1L
    if (config$method == "MoDE") {
        if (!is.numeric(s$pi) || length(s$pi) != config$K || any(!is.finite(s$pi) | s$pi <= 0) ||
            abs(sum(s$pi) - 1) > 1e-12) stop("Invalid MoDE mixing probabilities.", call. = FALSE)
        rebuilt$pi <- s$pi
    }
    if (!identical(s, rebuilt) || !identical(s, out$base_state) || !is.null(out$joint) ||
        !is.null(out$candidate) || !is.null(d$gate) || length(d$expert) != config$K ||
        !identical(d$before_signature, graphmode_digest(before)) ||
        !identical(d$after_signature, graphmode_digest(s)) ||
        !identical(out$events, graphmode_allocation_events(before$Z, s$Z, config$K)))
        stop("Nonadaptive state/scan handoff mismatch.", call. = FALSE)
    for (k in seq_len(config$K)) graphmode_blocks_run_expert_check(d$expert[[k]],
        matrix(before$theta[k, , ], nrow(config$Fmat), ncol(config$Fmat)),
        matrix(s$theta[k, , ], nrow(config$Fmat), ncol(config$Fmat)),
        config$Y[before$Z == k, , drop = FALSE], config, record$base_policy$block,
        d$block_offset, record$thresholds)
    if (config$method == "PottsMoDE") {
        # Gross births/deaths count intermediate single-site states, not net moves.
        z <- before$Z; sizes <- tabulate(z, config$K)
        gross <- c(births = 0, deaths = 0, upcrossings = 0, downcrossings = 0)
        for (i in seq_along(z)) if (z[i] != s$Z[i]) {
            old <- z[i]; new <- s$Z[i]
            gross <- gross + c(sizes[new] == 0L, sizes[old] == 1L,
                sizes[new] == 4L, sizes[old] == 5L)
            sizes[old] <- sizes[old] - 1L; sizes[new] <- sizes[new] + 1L; z[i] <- new
        }
        if (!identical(d$potts_gross, gross)) stop("Potts sequential event accounting changed.", call. = FALSE)
    }
    invisible(TRUE)
}
