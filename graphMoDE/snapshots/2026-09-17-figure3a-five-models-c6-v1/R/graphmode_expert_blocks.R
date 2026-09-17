# D-055: opt-in contiguous-time Poisson expert proposals. Not a run adapter.
# Reuse the audited PG/MH kernel and FFBS backward driver; condition each block
# on its current neighbouring states. Old source globs do not load this file.
graphmode_expert_blocks_version <- "graphmode-expert-blocks-20260915-v1"

graphmode_expert_blocks_policy <- function(block_length, offset_rule) {
    block_length <- gmde_scalar_integer(block_length, "fixed time-block length", 1L)
    offset_rule <- match.arg(offset_rule, c("fixed-zero", "uniform"))
    list(schema = graphmode_expert_blocks_version, block_length = block_length,
        offset_rule = offset_rule, adaptation = "none", order = "forward")
}

graphmode_expert_blocks_validate <- function(config, policy) {
    graphmode_revalidate_config(config)
    if (!is.list(policy) || !identical(policy, graphmode_expert_blocks_policy(
        policy$block_length, policy$offset_rule)))
        stop("Invalid or changed fixed expert-block policy.", call. = FALSE)
    if (config$family != "poisson" || config$dynamics != "dynamic")
        stop("Time blocks currently support dynamic Poisson experts only.", call. = FALSE)
    if (policy$block_length > nrow(config$Fmat))
        stop("Block length exceeds the observed time span.", call. = FALSE)
    invisible(TRUE)
}

# A partition of 1:T, never a circular state transition or an overlapping scan.
# An offset changes the first block's length; all subsequent interior lengths
# equal the fixed width. Width T has one block and consumes no offset draw.
graphmode_expert_blocks_layout <- function(TT, block_length, offset) {
    TT <- gmde_scalar_integer(TT, "time span", 1L)
    block_length <- gmde_scalar_integer(block_length, "block length", 1L, TT)
    offset <- gmde_scalar_integer(offset, "block offset", 0L, block_length - 1L)
    if (block_length == TT && offset != 0L)
        stop("A full-path policy requires offset zero.", call. = FALSE)
    first <- if (offset == 0L) block_length else offset
    ends <- if (first < TT) c(seq.int(first, TT - 1L, by = block_length), TT) else TT
    starts <- c(1L, head(ends, -1L) + 1L)
    data.frame(start = as.integer(starts), end = as.integer(ends))
}

graphmode_expert_blocks_offset <- function(TT, policy) {
    if (policy$offset_rule == "fixed-zero" || policy$block_length %in% c(1L, TT)) return(0L)
    u <- graphmode_uniform(1L)
    if (!is.numeric(u) || length(u) != 1L || !is.finite(u) || u < 0 || u >= 1)
        stop("Invalid state-independent block-offset draw.", call. = FALSE)
    as.integer(floor(u * policy$block_length))
}

# Filtering of a time interval. With no left boundary, theta_0 is integrated
# exactly as in the original filter (theta_1 ~ N(G m0, G C0 G' + W)). With
# a fixed left state, the first predictive covariance is W, NOT G C0 G' + W.
# No zero C0, matrix inversion of G, jitter or artificial observation is used.
graphmode_expert_block_filter <- function(precision, natural, Fmat, m0, C0, G, W,
                                        left = NULL, right = NULL) {
    graphmode_dlm(Fmat, m0, C0, G, W, "dynamic")
    TT <- nrow(Fmat); p <- ncol(Fmat)
    for (boundary in list(left, right)) if (!is.null(boundary) &&
        (!is.numeric(boundary) || length(boundary) != p || any(!is.finite(boundary))))
        stop("Invalid fixed time-block boundary.", call. = FALSE)
    if (length(precision) != TT || length(natural) != TT ||
        any(!is.finite(c(precision, natural))) || any(precision < 0) ||
        any(precision == 0 & natural != 0))
        stop("Invalid block observation information.", call. = FALSE)
    if (is.null(left)) {
        moments <- graphmode_information_filter(precision, natural, Fmat, m0, C0, G, W)
    } else {
        innovation_root <- t(gmde_chol_spd(W))
        m <- a <- matrix(0, TT, p)
        roots <- predictive <- array(0, c(p, p, TT))
        factor_residual <- root_residual <- root_rcond <- numeric(TT)
        for (tt in seq_len(TT)) {
            if (tt == 1L) {
                a[tt, ] <- G %*% left
                prediction <- innovation_root
            } else {
                a[tt, ] <- G %*% m[tt - 1L, ]
                previous <- matrix(roots[, , tt - 1L], p, p)
                prediction <- t(graphmode_qr_root(t(cbind(G %*% previous, innovation_root)))$R)
            }
            condition <- graphmode_information_condition(a[tt, ], prediction,
                Fmat[tt, , drop = FALSE], precision[tt], natural[tt])
            m[tt, ] <- condition$mean; roots[, , tt] <- condition$root
            predictive[, , tt] <- prediction
            factor_residual[tt] <- condition$factor_residual
            root_residual[tt] <- condition$root_residual
            root_rcond[tt] <- condition$root_reciprocal_condition
        }
        moments <- list(m = m, root = roots, a = a, predictive_root = predictive,
            factor_residual = factor_residual, root_residual = root_residual,
            root_reciprocal_condition = root_rcond, G = G, W_root = innovation_root)
    }
    if (!is.null(right)) {
        # theta[b+1] | theta[b] ~ N(G theta[b],W), a genuine boundary
        # likelihood. It enters only the terminal conditional, exactly once.
        end <- graphmode_backward_condition(moments$m[TT, ],
            matrix(moments$root[, , TT], p, p), right, G, moments$W_root)
        moments$m[TT, ] <- end$mean; moments$root[, , TT] <- end$root
        moments$factor_residual[TT] <- max(moments$factor_residual[TT], end$factor_residual)
        moments$root_residual[TT] <- max(moments$root_residual[TT], end$root_residual)
        moments$root_reciprocal_condition[TT] <- min(moments$root_reciprocal_condition[TT],
                                                    end$root_reciprocal_condition)
    }
    moments
}

graphmode_expert_block_ffbs <- function(precision, natural, Fmat, m0, C0, G, W,
                                      left, right, cache_ffbs) {
    if (!is.logical(cache_ffbs) || length(cache_ffbs) != 1L || is.na(cache_ffbs))
        stop("Specify the block FFBS cache switch.", call. = FALSE)
    filter <- function(precision, natural, Fmat, m0, C0, G, W)
        graphmode_expert_block_filter(precision, natural, Fmat, m0, C0, G, W, left, right)
    ffbs <- graphmode_dev_bind(graphmode_ffbs, list(graphmode_information_filter = filter))
    if (cache_ffbs) ffbs <- graphmode_dev_bind(graphmode_dev_ffbs, list(graphmode_ffbs = ffbs))
    ffbs(precision, natural, Fmat, m0, C0, G, W)
}

# Return a NEW expert envelope: each block has its own decision. There is no
# scalar whole-path acceptance probability and no stop-on-acceptance loop.
graphmode_expert_blocks_expert <- function(current, Yk, config, policy, offset, cache_ffbs) {
    started <- proc.time()[[3L]]
    graphmode_expert_blocks_validate(config, policy)
    TT <- nrow(config$Fmat); p <- ncol(config$Fmat)
    graphmode_matrix(current, "current expert path")
    if (!identical(dim(current), c(TT, p)) || !is.matrix(Yk) || !is.numeric(Yk) ||
        ncol(Yk) != TT || any(!is.finite(Yk)) || any(Yk < 0 | Yk != round(Yk)))
        stop("Invalid block expert path or count panel.", call. = FALSE)
    if (!is.logical(cache_ffbs) || length(cache_ffbs) != 1L || is.na(cache_ffbs))
        stop("Specify the block FFBS cache switch.", call. = FALSE)
    layout <- graphmode_expert_blocks_layout(TT, policy$block_length, offset)
    if (policy$offset_rule == "fixed-zero" && offset != 0L)
        stop("Offset differs from the fixed-zero policy.", call. = FALSE)
    records <- list(); observation_seconds <- 0
    updated <- current
    if (!nrow(Yk)) {
        # Empty experts retain the original FULL prior refresh, not one prior
        # restart per block. No PG or MH proposal is counted for this refresh.
        empty <- graphmode_dev_expert(current, Yk, config, NULL, cache_ffbs, TRUE)
        updated <- empty$update$theta
        factor_residual <- root_residual <- 0; root_rcond <- NA_real_
        accepted <- logical(); r <- numeric(); movement <- 0
    } else {
        r <- gmde_make_nb_r(colSums(Yk), config$rho)
        for (j in seq_len(nrow(layout))) {
            begin <- layout$start[j]; end <- layout$end[j]; idx <- seq.int(begin, end)
            left <- if (begin > 1L) updated[begin - 1L, ] else NULL
            right <- if (end < TT) updated[end + 1L, ] else NULL
            block_config <- config; block_config$Fmat <- config$Fmat[idx, , drop = FALSE]
            # The unmodified expert kernel now sees only this block's data;
            # its NB size and MH correction therefore use only these times.
            ffbs <- function(precision, natural, Fmat, m0, C0, G, W)
                graphmode_expert_block_ffbs(precision, natural, Fmat, m0, C0, G, W,
                                          left, right, cache_ffbs)
            driver <- graphmode_dev_bind(graphmode_dev_expert,
                list(graphmode_ffbs = ffbs, graphmode_dev_ffbs = ffbs))
            before <- updated[idx, , drop = FALSE]
            draw <- driver(before, Yk[, idx, drop = FALSE], block_config, NULL, cache_ffbs, TRUE)
            u <- draw$update; o <- draw$observation
            if (!identical(dim(u$theta), dim(before)) || any(!is.finite(u$theta)) ||
                !is.logical(u$accepted) || length(u$accepted) != 1L || is.na(u$accepted) ||
                !identical(u$r, r[idx]) || is.null(o) ||
                (!u$accepted && !identical(u$theta, before)))
                stop("Invalid conditional-block update; no partial expert returned.", call. = FALSE)
            updated[idx, ] <- u$theta
            records[[j]] <- list(block = j, start = begin, end = end,
                left = left, right = right, accepted = u$accepted,
                log_acceptance = u$log_acceptance, r = u$r,
                observation = o, kernel_seconds = u$seconds,
                observation_seconds = draw$observation_seconds,
                factor_residual = u$factor_residual, root_residual = u$root_residual,
                root_reciprocal_condition = u$root_reciprocal_condition)
            # Change the diagnostic interpretation, not the underlying terms.
            records[[j]]$observation$schema <- graphmode_expert_blocks_version
            records[[j]]$observation$note <- "Actual conditional time-block proposal; one local MH decision."
            observation_seconds <- observation_seconds + draw$observation_seconds
        }
        accepted <- vapply(records, `[[`, logical(1), "accepted")
        factor_residual <- max(vapply(records, `[[`, numeric(1), "factor_residual"))
        root_residual <- max(vapply(records, `[[`, numeric(1), "root_residual"))
        root_rcond <- min(vapply(records, `[[`, numeric(1), "root_reciprocal_condition"))
        movement <- sum(pmax(colSums(Yk), 1) *
            (rowSums(updated * config$Fmat) - rowSums(current * config$Fmat))^2)
    }
    if (any(!is.finite(updated)) || !is.finite(movement))
        stop("Nonfinite block path or movement.", call. = FALSE)
    counts <- list(proposals = length(accepted), accepted = sum(accepted),
        acceptance = if (length(accepted)) mean(accepted) else NA_real_,
        prior_refreshes = as.integer(!nrow(Yk)))
    update <- list(schema = graphmode_expert_blocks_version, theta = updated,
        sigma2 = NULL, empty = nrow(Yk) == 0L, r = r, movement = movement,
        seconds = proc.time()[[3L]] - started,
        timing_scope = "complete expert update including observations; do not add observation_seconds again",
        factor_residual = factor_residual,
        root_residual = root_residual, root_reciprocal_condition = root_rcond,
        counts = counts, blocks = records)
    list(update = update, observation = list(schema = graphmode_expert_blocks_version,
        layout = layout, offset = offset, counts = counts, blocks = records),
        observation_seconds = observation_seconds)
}

# Compose with D-050's original gate/one-allocation step, without editing it.
# One state-independent layout draw per OUTER sweep, shared by all experts.
# No old run/diagnostic adapter is allowed to infer a whole-path MH rate here.
graphmode_expert_blocks_sweep <- function(state, config, gate_policy, block_policy, cache_ffbs) {
    graphmode_expert_blocks_validate(config, block_policy)
    offset <- NULL
    expert <- function(current, Yk, config, sigma2 = NULL, cache_ffbs, observe) {
        if (!isTRUE(observe) || !is.null(sigma2)) stop("Unsupported block expert routing.", call. = FALSE)
        if (is.null(offset)) offset <<- graphmode_expert_blocks_offset(nrow(config$Fmat), block_policy)
        graphmode_expert_blocks_expert(current, Yk, config, block_policy, offset, cache_ffbs)
    }
    driver <- graphmode_dev_bind(graphmode_gate_refresh_sweep, list(graphmode_dev_expert = expert))
    out <- driver(state, config, gate_policy, cache_ffbs)
    out$schema <- graphmode_expert_blocks_version
    out$gate_policy <- out$policy; out$policy <- NULL
    out$block_policy <- block_policy; out$block_offset <- offset
    out$block_states_are_retained_draws <- FALSE
    out
}
