# Exact selected definitions from R/graphmode_c8_learning.R
graphmode_c8_kernel_context <- function(kind = c("graph", "euclidean"), input, ids = NULL) {
    kind <- match.arg(kind)
    if (kind == "graph") {
        graphmode_validate_weight(input)
        L <- diag(rowSums(input)) - input
        e <- eigen(L, symmetric = TRUE)
        tolerance <- 64 * .Machine$double.eps * max(1, norm(L, "I"))
        if (min(e$values) < -tolerance) stop("Invalid graph spectrum.")
        out <- list(kind = kind, values = pmax(e$values, 0), vectors = e$vectors,
            neighbors = upper.tri(input) & input > 0, nu = 1, tau = 1)
    } else {
        # No graph, graph weights, road distances or labels enter this branch.
        graphmode_matrix(input, "coordinates"); n <- nrow(input)
        if (n < 2L || anyDuplicated(as.data.frame(input))) stop("Distinct Euclidean sites required.")
        distance <- as.matrix(dist(input))
        if (is.null(ids)) ids <- seq_len(n)
        if (length(ids) != n || anyDuplicated(ids)) stop("Unique geometry IDs required.")
        support <- matrix(FALSE, n, n)
        for (i in seq_len(n)) {
            j <- setdiff(seq_len(n), i)
            support[i, j[order(distance[i, j], ids[j])[seq_len(min(4L, n - 1L))]]] <- TRUE
        }
        out <- list(kind = kind, coordinates = input, neighbors = upper.tri(distance) &
            (support | t(support)), nu = 1, tau = 1)
    }
    center <- function(z) {
        P <- graphmode_c8_kernel(out, z)
        C <- tcrossprod(P); sd <- sqrt(diag(C))
        median((C / outer(sd, sd))[out$neighbors]) - .5
    }
    # Bracketing determines a prior center only. It is not parameter truncation.
    grid <- seq(-7, 7, by = .5); previous <- NULL; bracket <- NULL
    for (x in grid) {
        value <- tryCatch(center(x), error = function(e) NA_real_)
        if (!is.finite(value)) next
        if (!is.null(previous) && previous[2] * value <= 0) {bracket <- c(previous[1], x); break}
        previous <- c(x, value)
    }
    if (is.null(bracket)) stop("Cannot calibrate scale prior from geometry.")
    out$log_center <- uniroot(center, bracket, tol = 1e-10)$root
    out
}

graphmode_c8_kernel <- function(context, log_scale) {
    if (length(log_scale) != 1L || !is.finite(log_scale)) stop("Invalid log scale.")
    scale <- exp(log_scale); graphmode_positive(scale, "kernel scale")
    if (context$kind == "euclidean")
        return(graphmode_euclidean_kernel(context$coordinates, scale, context$nu, context$tau)$Phi)
    log_r <- -context$nu * log(2 * context$nu / scale^2 + context$values)
    r <- exp(log_r - max(log_r)); r <- length(r) * context$tau^2 * r / sum(r)
    if (any(!is.finite(r) | r <= 0)) stop("Kernel spectrum underflowed.")
    sweep(context$vectors, 2L, sqrt(r), "*")
}

graphmode_c8_current_config <- function(fit, log_scale, log_q) {
    if (!identical(fit$version, graphmode_c8_version)) stop("Not a C8 fit.")
    q <- exp(log_q); graphmode_positive(q, "innovation variance")
    c <- fit$core; c$W <- diag(q, ncol(c$Fmat))
    if (!is.null(fit$kernel)) c$Phi <- graphmode_c8_kernel(fit$kernel, log_scale)
    graphmode_revalidate_config(c)
    c
}

graphmode_c8_scale_logtarget <- function(log_scale, core_state, fit, config) {
    candidate <- config; candidate$Phi <- graphmode_c8_kernel(fit$kernel, log_scale)
    F <- graphmode_gate_utilities(core_state$x, core_state$v, candidate)
    # White coordinates have a scale-independent N(0,I) prior. No extra det(K).
    graphmode_gate_loglik(F, core_state$Z) +
        dnorm(log_scale, fit$kernel$log_center, fit$prior$scale_log_sd, log = TRUE)
}

graphmode_c8_q_logtarget <- function(log_q, theta, config, prior) {
    q <- exp(log_q); graphmode_positive(q, "innovation variance")
    K <- dim(theta)[1]; TT <- dim(theta)[2]; p <- dim(theta)[3]
    stopifnot(identical(dim(theta), c(config$K, nrow(config$Fmat), ncol(config$Fmat))))
    # theta_0 is integrated out by the existing FFBS: theta_1 ~ N(G m0, G C0 G' + q I).
    R <- chol(config$G %*% config$C0 %*% t(config$G) + diag(q, p))
    initial <- matrix(theta[, 1, ], K, p)
    initial <- sweep(initial, 2L, as.vector(config$G %*% config$m0), "-")
    value <- -K * sum(log(diag(R))) - sum(backsolve(R, t(initial), transpose = TRUE)^2) / 2
    if (TT > 1L) for (k in seq_len(K)) {
        path <- matrix(theta[k, , ], TT, p)
        delta <- path[-1, , drop = FALSE] - path[-TT, , drop = FALSE] %*% t(config$G)
        value <- value - (TT - 1) * p * log_q / 2 - sum(delta^2) / (2*q)
    }
    # Gamma(shape,rate) prior on q, PLUS dq/dlog(q). Includes empty experts.
    value + prior$q_shape * log_q - prior$q_rate * q
}

graphmode_c8_scalar_mh <- function(current, target, proposal_sd,
                                   normal = graphmode_normal, uniform = graphmode_uniform) {
    graphmode_positive(proposal_sd, "proposal_sd")
    z <- normal(1L); u <- uniform(1L)
    if (length(current) != 1L || !is.finite(current) || length(z) != 1L ||
        !is.finite(z) || length(u) != 1L || !is.finite(u) || u <= 0 || u >= 1)
        stop("Invalid scalar MH inputs.")
    proposal <- current + proposal_sd * z
    old <- target(current); proposed <- target(proposal)
    if (any(!is.finite(c(old, proposed)))) stop("Nonfinite hyperparameter target; retain failure, no clipping.")
    ratio <- proposed - old
    accepted <- log(u) <= min(0, ratio)
    list(value = if (accepted) proposal else current, proposed = proposal,
        accepted = accepted, log_ratio = ratio)
}

graphmode_c8_validate_state <- function(state, fit, config = NULL) {
    if (!identical(state$version, graphmode_c8_version)) stop("C8 requires a new state; old checkpoints rejected.")
    if (is.null(config)) config <- graphmode_c8_current_config(fit, state$log_scale, state$log_q)
    if (length(state$log_q) != 1L || !is.finite(state$log_q) ||
        (is.null(fit$kernel) && !is.null(state$log_scale))) stop("Invalid scale state.")
    s <- state$core
    rebuilt <- graphmode_initial_state(config, s$Z, s$theta, s$sigma2,
        if (config$adaptive) s$x else NULL, if (config$adaptive) s$v else NULL)
    rebuilt$iteration <- gmde_scalar_integer(s$iteration, "iteration", 0L)
    if (config$method == "MoDE") {
        if (length(s$pi) != config$K || any(!is.finite(s$pi) | s$pi <= 0) ||
            abs(sum(s$pi) - 1) > 1e-10) stop("Invalid mixture weights.")
        rebuilt$pi <- s$pi
    }
    if (!identical(s, rebuilt)) stop("Invalid C8 core state.")
    invisible(TRUE)
}

graphmode_c8_initial <- function(fit, Z, theta = NULL, x = NULL, v = NULL) {
    list(version = graphmode_c8_version,
        core = graphmode_initial_state(fit$core, Z, theta, x = x, v = v),
        log_scale = if (is.null(fit$kernel)) NULL else fit$kernel$log_center,
        log_q = log(fit$prior$q_shape / fit$prior$q_rate))
}

graphmode_c8_common_start <- function(fit, source, chain) {
    chain <- gmde_scalar_integer(chain, "chain", 1L, 4L)
    if (!identical(source$iteration, 0L)) stop("Only a newly constructed shared initial state is allowed.")
    state <- graphmode_c8_initial(fit, source$Z, source$theta,
        if (fit$core$adaptive) source$x else NULL, if (fit$core$adaptive) source$v else NULL)
    s <- graphmode_c8_spec()
    if (!is.null(fit$kernel)) state$log_scale <- state$log_scale + s$bayes$start_scale_log_offsets[chain]
    state$log_q <- state$log_q + s$bayes$start_q_log_offsets[chain]
    graphmode_c8_validate_state(state, fit)
    state
}

graphmode_c8_step <- function(state, fit, base_step = graphmode_compare_step,
                              normal = graphmode_normal, uniform = graphmode_uniform) {
    if (!identical(state$version, graphmode_c8_version)) stop("C8 requires a new state; old checkpoints rejected.")
    config <- graphmode_c8_current_config(fit, state$log_scale, state$log_q)
    graphmode_c8_validate_state(state, fit, config)
    # Old expert/gate/allocation kernels operate conditional on the current scales.
    out <- base_step(state$core, config, fit$policy)
    if (out$state$iteration != state$core$iteration + 1L) stop("Incomplete base transition.")
    next_state <- state; next_state$core <- out$state; scale <- NULL
    graphmode_c8_validate_state(next_state, fit, config)
    if (!is.null(fit$kernel)) {
        scale <- graphmode_c8_scalar_mh(state$log_scale,
            function(z) graphmode_c8_scale_logtarget(z, next_state$core, fit, config),
            fit$scale_proposal_sd, normal, uniform)
        next_state$log_scale <- scale$value
    }
    q <- graphmode_c8_scalar_mh(state$log_q,
        function(z) graphmode_c8_q_logtarget(z, next_state$core$theta, config, fit$prior),
        fit$log_q_proposal_sd, normal, uniform)
    next_state$log_q <- q$value
    updated <- graphmode_c8_current_config(fit, next_state$log_scale, next_state$log_q)
    list(state = next_state, scale_update = scale, q_update = q,
        base = out, utilities = if (updated$adaptive)
            graphmode_gate_utilities(next_state$core$x, next_state$core$v, updated) else NULL)
}

graphmode_c8_predict <- function(draws, fit, Ffuture) {
    c <- fit$core; stopifnot(identical(c$G, diag(ncol(c$Fmat))), length(draws) > 0L)
    graphmode_matrix(Ffuture, "future design")
    if (ncol(Ffuture) != ncol(c$Fmat)) stop("Future design dimension differs.")
    result <- matrix(0, c$n, nrow(Ffuture)); TT <- nrow(c$Fmat); p <- ncol(Ffuture)
    # Exponential-tailed Gamma q prior ensures these finite-horizon means exist.
    if (fit$prior$q_rate <= max((TT + seq_len(nrow(Ffuture))) * rowSums(Ffuture^2) / 2))
        stop("Innovation prior does not guarantee a finite predictive mean at this horizon.")
    for (d in draws) {
      graphmode_c8_validate_state(d, fit)
      for (h in seq_len(nrow(Ffuture))) {
        core <- d$core; f <- Ffuture[h, ]; q <- exp(d$log_q)
        means <- exp(as.vector(matrix(core$theta[, TT, ], c$K, p) %*% f) + h*q*sum(f^2)/2)
        result[, h] <- result[, h] + means[core$Z] / length(draws)
      }
    }
    if (any(!is.finite(result) | result <= 0)) stop("Invalid predictive mean.")
    result
}
