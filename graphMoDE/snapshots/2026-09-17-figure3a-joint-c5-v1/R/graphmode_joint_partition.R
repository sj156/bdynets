# Opt-in fixed-K joint partition/path/gate MH candidate. No runner or sourcing
# side effects. The underscore name deliberately excludes frozen source globs.
graphmode_joint_partition_version <- "graphmode-joint-partition-20260916-v1"

graphmode_joint_partition_policy <- function(newton_steps = 8L, backtracks = 20L,
    gate_steps = 64L, gate_tolerance = 1e-10) {
    list(schema = graphmode_joint_partition_version,
        newton_steps = gmde_scalar_integer(newton_steps, "Newton steps", 0L),
        backtracks = gmde_scalar_integer(backtracks, "backtracking steps", 1L),
        gate_steps = gmde_scalar_integer(gate_steps, "gate CG steps", 1L),
        gate_tolerance = graphmode_positive(gate_tolerance, "gate CG tolerance"),
        split_scale = 4, gate_scale = "log(K)", adaptation = "none")
}

graphmode_joint_partition_validate <- function(state, config, policy) {
    graphmode_revalidate_config(config)
    if (config$method != "graphMoDE-W" || config$family != "poisson" ||
        config$dynamics != "dynamic")
        stop("Joint partition candidate supports dynamic Poisson graphMoDE-W only.", call. = FALSE)
    fields <- names(formals(graphmode_joint_partition_policy))
    if (!is.list(policy) || !all(fields %in% names(policy)) ||
        !identical(policy, do.call(graphmode_joint_partition_policy, policy[fields])))
        stop("Invalid or changed joint partition policy.", call. = FALSE)
    if (!is.list(state)) stop("Invalid joint partition state.", call. = FALSE)
    rebuilt <- graphmode_initial_state(config, state$Z, state$theta, state$sigma2, state$x, state$v)
    rebuilt$iteration <- gmde_scalar_integer(state$iteration, "outer iteration", 0L)
    if (!identical(state, rebuilt))
        stop("Use a canonical state without extra caches or altered fields.", call. = FALSE)
    invisible(TRUE)
}

graphmode_joint_partition_uniform <- function(n) {
    u <- graphmode_uniform(n)
    if (!is.numeric(u) || length(u) != n || any(!is.finite(u)) || any(u < 0 | u >= 1))
        stop("Invalid joint partition uniform input.", call. = FALSE)
    u
}

graphmode_joint_partition_logsum <- function(x) {
    top <- max(x)
    if (top == -Inf) return(-Inf)
    top + log(sum(exp(x - top)))
}

# log(1-exp(x)), including the impossible remaining-completion case x=0.
graphmode_joint_partition_log1m <- function(x) {
    if (x > 0 || is.na(x)) stop("Invalid log probability.", call. = FALSE)
    if (x < -log(2)) log1p(-exp(x)) else log(-expm1(x))
}

graphmode_joint_partition_distribution <- function(Y, nodes) {
    Y <- graphmode_matrix(Y, "partition observations")
    if (any(Y < 0 | Y != round(Y)) || !is.numeric(nodes) || length(nodes) < 2L ||
        any(!is.finite(nodes)) || any(nodes != round(nodes) | nodes < 1 | nodes > nrow(Y)) ||
        anyDuplicated(nodes)) stop("Invalid split union.", call. = FALSE)
    nodes <- sort(as.integer(nodes))
    u <- log1p(Y[nodes, , drop = FALSE]) / sqrt(ncol(Y))
    # Direct differences avoid cancellation in squared-distance identities.
    best <- -1; pair <- c(1L, 2L)
    for (i in seq_len(length(nodes) - 1L)) for (j in seq.int(i + 1L, length(nodes))) {
        distance <- sum((u[i, ] - u[j, ])^2)
        if (distance > best) { best <- distance; pair <- c(i, j) }
    }
    score <- if (best == 0) rep(0, length(nodes)) else
        (rowSums(sweep(u, 2L, u[pair[1L], ], "-")^2) -
         rowSums(sweep(u, 2L, u[pair[2L], ], "-")^2)) / best
    p <- plogis(4 * score)
    if (any(!is.finite(p)) || any(p <= 0 | p >= 1))
        stop("Split probabilities lost full support.", call. = FALSE)
    logp <- log(p); log0 <- log1p(-p)
    excluded <- graphmode_joint_partition_logsum(c(sum(logp), sum(log0)))
    list(nodes = nodes, p = p, logp = logp, log0 = log0,
        log_normalizer = graphmode_joint_partition_log1m(excluded), anchors = nodes[pair])
}

graphmode_joint_partition_logq <- function(distribution, B) {
    nodes <- distribution$nodes
    if (!is.numeric(B) || !length(B) || length(B) >= length(nodes) ||
        anyNA(B) || anyDuplicated(B) || !all(B %in% nodes))
        stop("Split requires two nonempty complementary groups.", call. = FALSE)
    bit <- nodes %in% B
    lp <- sum(ifelse(bit, distribution$logp, distribution$log0))
    lc <- sum(ifelse(bit, distribution$log0, distribution$logp))
    graphmode_joint_partition_logsum(c(lp, lc)) - log(2) - distribution$log_normalizer
}

# Exact sequential conditional Bernoulli draw, then marginal mixture density.
# No random retries and no truncation of the distribution's support.
graphmode_joint_partition_draw_split <- function(distribution) {
    reverse <- graphmode_joint_partition_uniform(1L) < .5
    lp <- if (reverse) distribution$log0 else distribution$logp
    l0 <- if (reverse) distribution$logp else distribution$log0
    m <- length(lp); bit <- logical(m); seen0 <- seen1 <- FALSE
    tailp <- c(rev(cumsum(rev(lp))), 0)
    tail0 <- c(rev(cumsum(rev(l0))), 0)
    for (i in seq_len(m)) {
        remaining <- function(value) {
            has0 <- seen0 || !value; has1 <- seen1 || value
            if (has0 && has1) return(0)
            graphmode_joint_partition_log1m(if (has0) tail0[i + 1L] else tailp[i + 1L])
        }
        weights <- c(l0[i] + remaining(FALSE), lp[i] + remaining(TRUE))
        probability <- exp(weights[2L] - graphmode_joint_partition_logsum(weights))
        bit[i] <- graphmode_joint_partition_uniform(1L) < probability
        seen0 <- seen0 || !bit[i]; seen1 <- seen1 || bit[i]
    }
    B <- distribution$nodes[bit]
    list(B = B, logq = graphmode_joint_partition_logq(distribution, B))
}

graphmode_joint_partition_gaussian_log <- function(x, mean, root) {
    residual <- as.numeric(solve(root, x - mean, tol = 0))
    determinant <- determinant(root, logarithm = TRUE)
    value <- -length(x) * log(2 * pi) / 2 - as.numeric(determinant$modulus) - sum(residual^2) / 2
    if (determinant$sign == 0 || !is.finite(value))
        stop("Nonfinite normalized Gaussian density.", call. = FALSE)
    value
}

graphmode_joint_partition_prior <- function(config) {
    TT <- nrow(config$Fmat); p <- ncol(config$Fmat)
    mean <- matrix(0, TT, p); previous <- config$m0
    for (t in seq_len(TT)) { previous <- as.numeric(config$G %*% previous); mean[t, ] <- previous }
    list(mean = mean, first_root = t(gmde_chol_spd(config$G %*% config$C0 %*%
        t(config$G) + config$W)), W_root = t(gmde_chol_spd(config$W)), G = config$G)
}

graphmode_joint_partition_prior_log <- function(theta, prior) {
    value <- graphmode_joint_partition_gaussian_log(theta[1L, ], prior$mean[1L, ], prior$first_root)
    if (nrow(theta) > 1L) for (t in seq.int(2L, nrow(theta)))
        value <- value + graphmode_joint_partition_gaussian_log(theta[t, ],
            as.numeric(prior$G %*% theta[t - 1L, ]), prior$W_root)
    value
}

graphmode_joint_partition_response <- function(theta, totals, size, Fmat) {
    if (!size) return(0)
    eta <- rowSums(theta * Fmat)
    value <- sum(totals * eta - size * exp(eta))
    if (!is.finite(value)) stop("Nonfinite joint partition response density.", call. = FALSE)
    value
}

# Backward conditional means give the full Gaussian mean even when the
# expansion point is only an approximate mode. No dense Tp by Tp inverse.
graphmode_joint_partition_mean <- function(moments) {
    mean <- moments$m; TT <- nrow(mean); p <- ncol(mean)
    if (TT > 1L) for (t in seq.int(TT - 1L, 1L)) mean[t, ] <-
        graphmode_backward_condition(moments$m[t, ], matrix(moments$root[, , t], p, p),
            mean[t + 1L, ], moments$G, moments$W_root)$mean
    mean
}

graphmode_joint_partition_proposal <- function(config, nodes, policy, prior = NULL) {
    if (is.null(prior)) prior <- graphmode_joint_partition_prior(config)
    size <- length(nodes); totals <- colSums(config$Y[nodes, , drop = FALSE])
    common <- list(empty = size == 0L, prior = prior, size = size, totals = totals)
    if (!size) return(c(common, list(mean = prior$mean, iterations = 0L)))
    anchor <- prior$mean
    objective <- function(theta) graphmode_joint_partition_response(theta, totals, size, config$Fmat) +
        graphmode_joint_partition_prior_log(theta, prior)
    filter <- function(theta) {
        eta <- rowSums(theta * config$Fmat); precision <- size * exp(eta)
        natural <- totals - precision + precision * eta
        if (any(!is.finite(c(precision, natural))) || any(precision <= 0))
            stop("Nonfinite or underflowed path-proposal information.", call. = FALSE)
        graphmode_information_filter(precision, natural, config$Fmat, config$m0,
            config$C0, config$G, config$W)
    }
    for (iteration in seq_len(policy$newton_steps)) {
        step <- graphmode_joint_partition_mean(filter(anchor)) - anchor
        old <- objective(anchor); found <- FALSE
        for (j in seq_len(policy$backtracks)) {
            next_anchor <- anchor + 2^(-(j - 1L)) * step
            # Overflowing line-search trials have objective -Inf; this only
            # chooses a deterministic expansion point, never changes a target.
            eta <- rowSums(next_anchor * config$Fmat)
            next_value <- if (any(!is.finite(exp(eta)))) -Inf else objective(next_anchor)
            if (next_value >= old) { anchor <- next_anchor; found <- TRUE; break }
        }
        # Retaining the preceding anchor is deterministic; no proposal mixture.
        if (!found) break
    }
    moments <- filter(anchor)
    c(common, list(mean = graphmode_joint_partition_mean(moments), moments = moments,
        anchor = anchor, iterations = if (policy$newton_steps) iteration else 0L))
}

graphmode_joint_partition_proposal_log <- function(theta, proposal) {
    if (proposal$empty) return(graphmode_joint_partition_prior_log(theta, proposal$prior))
    moments <- proposal$moments; TT <- nrow(theta); p <- ncol(theta)
    value <- graphmode_joint_partition_gaussian_log(theta[TT, ], moments$m[TT, ],
        matrix(moments$root[, , TT], p, p))
    if (TT > 1L) for (t in seq.int(TT - 1L, 1L)) {
        condition <- graphmode_backward_condition(moments$m[t, ],
            matrix(moments$root[, , t], p, p), theta[t + 1L, ], moments$G, moments$W_root)
        value <- value + graphmode_joint_partition_gaussian_log(theta[t, ], condition$mean, condition$root)
    }
    value
}

graphmode_joint_partition_draw_path <- function(proposal) {
    TT <- nrow(proposal$mean); p <- ncol(proposal$mean); theta <- matrix(0, TT, p)
    draw <- function(mean, root) {
        z <- graphmode_normal(p)
        if (!is.numeric(z) || length(z) != p || any(!is.finite(z)))
            stop("Invalid joint partition normal input.", call. = FALSE)
        as.numeric(mean + root %*% z)
    }
    if (proposal$empty) {
        prior <- proposal$prior
        theta[1L, ] <- draw(prior$mean[1L, ], prior$first_root)
        if (TT > 1L) for (t in seq.int(2L, TT))
            theta[t, ] <- draw(prior$G %*% theta[t - 1L, ], prior$W_root)
    } else {
        moments <- proposal$moments
        theta[TT, ] <- draw(moments$m[TT, ], matrix(moments$root[, , TT], p, p))
        if (TT > 1L) for (t in seq.int(TT - 1L, 1L)) {
            condition <- graphmode_backward_condition(moments$m[t, ],
                matrix(moments$root[, , t], p, p), theta[t + 1L, ], moments$G, moments$W_root)
            theta[t, ] <- draw(condition$mean, condition$root)
        }
    }
    if (any(!is.finite(theta))) stop("Nonfinite proposed path.", call. = FALSE)
    theta
}

# The adjoint includes guidance mixing, intercepts and all graph/noise columns.
graphmode_joint_partition_gate_operator <- function(v, config) {
    H <- gmde_helmert_contrast(config$K)
    factors <- graphmode_guidance_factors(v, config$K, config$guidance)
    apply <- function(x) {
        parts <- graphmode_gate_parts(x, config)
        contrast <- matrix(rep(config$s_b * parts$c, each = config$n), config$n, config$K - 1L) +
            parts$structured %*% t(factors$g) + config$tau * parts$noise %*% t(factors$e)
        contrast %*% t(H)
    }
    adjoint <- function(U) {
        contrast <- U %*% H
        c(config$s_b * colSums(contrast),
            if (config$guidance != "none") as.numeric(crossprod(config$Phi, contrast %*% factors$g)),
            if (config$guidance != "forced") as.numeric(config$tau * contrast %*% factors$e))
    }
    list(apply = apply, adjoint = adjoint)
}

# Each center starts from zero and depends only on (Z,v,config,policy). A
# bounded approximate center is valid; do NOT solve only a difference RHS.
graphmode_joint_partition_gate_center <- function(Z, config, operator, policy) {
    target <- matrix(-1 / config$K, config$n, config$K)
    target[cbind(seq_len(config$n), Z)] <- 1 - 1 / config$K
    rhs <- operator$adjoint(log(config$K) * target)
    center <- numeric(length(rhs)); residual <- rhs; direction <- rhs
    initial <- sum(rhs^2); squared <- initial; used <- 0L
    if (!is.finite(initial)) stop("Nonfinite gate center RHS.", call. = FALSE)
    if (initial > 0) for (i in seq_len(policy$gate_steps)) {
        applied <- direction + operator$adjoint(operator$apply(direction))
        curvature <- sum(direction * applied)
        if (!is.finite(curvature) || curvature <= 0)
            stop("Gate center lost positive curvature.", call. = FALSE)
        alpha <- squared / curvature
        center <- center + alpha * direction; residual <- residual - alpha * applied
        next_squared <- sum(residual^2); used <- i
        if (!is.finite(next_squared) || any(!is.finite(center)))
            stop("Nonfinite gate center iterate.", call. = FALSE)
        if (sqrt(next_squared / initial) <= policy$gate_tolerance) break
        direction <- residual + (next_squared / squared) * direction; squared <- next_squared
    }
    actual <- rhs - center - operator$adjoint(operator$apply(center))
    relative <- sqrt(sum(actual^2)) / max(1, sqrt(initial))
    if (!is.finite(relative)) stop("Nonfinite gate center residual.", call. = FALSE)
    list(x = center, relative_residual = relative, iterations = used)
}

# Internal deterministic preparation shared by fixed-candidate evaluation and
# the one-move entry point. No initial-state constructor or optimizer uses RNG.
graphmode_joint_partition_prepare <- function(state, config, a, b, B, policy) {
    graphmode_joint_partition_validate(state, config, policy)
    a <- gmde_scalar_integer(a, "label a", 1L, config$K)
    b <- gmde_scalar_integer(b, "label b", 1L, config$K)
    if (a == b || !any(state$Z == a)) stop("Invalid ordered label pair.", call. = FALSE)
    old_a <- which(state$Z == a); old_b <- which(state$Z == b)
    nodes <- sort(c(old_a, old_b)); split <- !length(old_b)
    distribution <- graphmode_joint_partition_distribution(config$Y, nodes)
    if (split) {
        logq <- graphmode_joint_partition_logq(distribution, B)
        A <- setdiff(nodes, B); B <- sort(as.integer(B))
    } else {
        if (!is.null(B)) stop("Merge uses the current groups; do not supply B.", call. = FALSE)
        A <- old_a; B <- old_b
        logq <- graphmode_joint_partition_logq(distribution, B)
    }
    next_Z <- state$Z
    if (split) { next_Z[A] <- a; next_Z[B] <- b } else next_Z[nodes] <- a
    prior <- graphmode_joint_partition_prior(config)
    union <- graphmode_joint_partition_proposal(config, nodes, policy, prior)
    child_a <- graphmode_joint_partition_proposal(config, A, policy, prior)
    child_b <- graphmode_joint_partition_proposal(config, B, policy, prior)
    empty <- graphmode_joint_partition_proposal(config, integer(), policy, prior)
    old <- if (split) list(union, empty) else list(child_a, child_b)
    proposed <- if (split) list(child_a, child_b) else list(union, empty)
    operator <- graphmode_joint_partition_gate_operator(state$v, config)
    center_old <- graphmode_joint_partition_gate_center(state$Z, config, operator, policy)
    center_new <- graphmode_joint_partition_gate_center(next_Z, config, operator, policy)
    list(old = old, proposed = proposed, prior = prior, Z = next_Z,
        x = state$x + (center_new$x - center_old$x), logq = logq,
        split = split, a = a, b = b, M = length(unique(state$Z)),
        centers = list(old = center_old, proposed = center_new))
}

graphmode_joint_partition_score <- function(state, config, prepared, theta_a, theta_b) {
    TT <- nrow(config$Fmat); p <- ncol(config$Fmat)
    for (theta in list(theta_a, theta_b)) if (!is.matrix(theta) || !is.numeric(theta) ||
        !identical(dim(theta), c(TT, p)) || any(!is.finite(theta)))
        stop("Invalid fixed candidate path.", call. = FALSE)
    old <- list(matrix(state$theta[prepared$a, , ], TT, p), matrix(state$theta[prepared$b, , ], TT, p))
    proposed <- list(theta_a, theta_b)
    response <- prior <- forward <- reverse <- corrected_proposal <- numeric(2L)
    for (i in 1:2) {
        before <- prepared$old[[i]]; after <- prepared$proposed[[i]]
        response[i] <- graphmode_joint_partition_response(proposed[[i]], after$totals, after$size, config$Fmat) -
            graphmode_joint_partition_response(old[[i]], before$totals, before$size, config$Fmat)
        forward[i] <- graphmode_joint_partition_proposal_log(proposed[[i]], after)
        reverse[i] <- graphmode_joint_partition_proposal_log(old[[i]], before)
        # Empty p0/g0 cancels analytically, BEFORE summing large log densities.
        # Keep the full qf/qr separately for review of ordinary-sized inputs.
        prior[i] <- (if (after$empty) 0 else
            graphmode_joint_partition_prior_log(proposed[[i]], prepared$prior)) -
            (if (before$empty) 0 else graphmode_joint_partition_prior_log(old[[i]], prepared$prior))
        corrected_proposal[i] <- (if (before$empty) 0 else reverse[i]) -
            (if (after$empty) 0 else forward[i])
    }
    candidate <- state
    candidate$Z <- prepared$Z; candidate$x <- prepared$x
    candidate$theta[prepared$a, , ] <- theta_a; candidate$theta[prepared$b, , ] <- theta_b
    next_M <- prepared$M + if (prepared$split) 1L else -1L
    components <- c(response = sum(response), expert_prior = sum(prior),
        white_prior = -.5 * (sum(candidate$x^2) - sum(state$x^2)),
        allocation = graphmode_gate_loglik(graphmode_gate_utilities(candidate$x, state$v, config), candidate$Z) -
            graphmode_gate_loglik(graphmode_gate_utilities(state$x, state$v, config), state$Z),
        pair_selection = log(prepared$M) - log(next_M),
        partition_proposal = if (prepared$split) -prepared$logq else prepared$logq,
        path_proposal = sum(corrected_proposal))
    log_ratio <- sum(components)
    if (any(!is.finite(components)) || !is.finite(log_ratio))
        stop("Nonfinite joint MH ratio; no state applied.", call. = FALSE)
    list(candidate = candidate, log_ratio = log_ratio, log_acceptance = min(0, log_ratio),
        components = components, log_q_partition = prepared$logq,
        log_q_forward = -log(prepared$M * (config$K - 1L)) + sum(forward) +
            if (prepared$split) prepared$logq else 0,
        log_q_reverse = -log(next_M * (config$K - 1L)) + sum(reverse) +
            if (prepared$split) 0 else prepared$logq,
        gate_center_residuals = vapply(prepared$centers, function(z) z$relative_residual, numeric(1)))
}

# A deterministic inspection interface. Supplying candidates here never draws,
# applies a move, touches a checkpoint or updates an outer iteration counter.
graphmode_joint_partition_evaluate <- function(state, config, a, b, theta_a, theta_b,
    B = NULL, policy = graphmode_joint_partition_policy()) {
    prepared <- graphmode_joint_partition_prepare(state, config, a, b, B, policy)
    graphmode_joint_partition_score(state, config, prepared, theta_a, theta_b)
}

graphmode_joint_partition_result <- function(state, config, move, labels, accepted, score = NULL, u = NULL) {
    eta <- gmde_eta(state$theta, config$Fmat)
    list(schema = graphmode_joint_partition_version, state = state, move = move,
        labels = labels, accepted = accepted,
        log_ratio = if (is.null(score)) 0 else score$log_ratio,
        log_acceptance = if (is.null(score)) 0 else score$log_acceptance,
        log_uniform = if (is.null(u)) NULL else log(u),
        components = if (is.null(score)) numeric() else score$components,
        log_q_partition = if (is.null(score)) NULL else score$log_q_partition,
        log_q_forward = if (is.null(score)) NULL else score$log_q_forward,
        log_q_reverse = if (is.null(score)) NULL else score$log_q_reverse,
        gate_center_residuals = if (is.null(score)) numeric() else score$gate_center_residuals,
        eta = eta, response = graphmode_response_loglik(config$Y, eta, config$family),
        utilities = graphmode_gate_utilities(state$x, state$v, config), sizes = tabulate(state$Z, config$K))
}

# One optional MH move, NOT a complete sweep or experiment entry point.
# iteration remains unchanged. Existing callers never invoke this function.
graphmode_joint_partition_step <- function(state, config, policy = graphmode_joint_partition_policy()) {
    graphmode_joint_partition_validate(state, config, policy)
    if (config$K == 1L) return(graphmode_joint_partition_result(state, config, "self", integer(), FALSE))
    occupied <- sort(unique(state$Z))
    u <- graphmode_joint_partition_uniform(2L)
    a <- occupied[1L + floor(length(occupied) * u[1L])]
    others <- setdiff(seq_len(config$K), a); b <- others[1L + floor(length(others) * u[2L])]
    split <- !any(state$Z == b); nodes <- which(state$Z == a)
    if (split && length(nodes) < 2L)
        return(graphmode_joint_partition_result(state, config, "self", c(a, b), FALSE))
    B <- if (split) graphmode_joint_partition_draw_split(
        graphmode_joint_partition_distribution(config$Y, nodes))$B else NULL
    prepared <- graphmode_joint_partition_prepare(state, config, a, b, B, policy)
    theta_a <- graphmode_joint_partition_draw_path(prepared$proposed[[1L]])
    theta_b <- graphmode_joint_partition_draw_path(prepared$proposed[[2L]])
    score <- graphmode_joint_partition_score(state, config, prepared, theta_a, theta_b)
    u <- graphmode_joint_partition_uniform(1L)
    accepted <- log(u) <= score$log_acceptance
    graphmode_joint_partition_result(if (accepted) score$candidate else state, config,
        if (split) "split" else "merge", c(a, b), accepted, score, u)
}
