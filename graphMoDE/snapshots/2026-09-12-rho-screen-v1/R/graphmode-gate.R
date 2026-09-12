# Identifiable, noncentered adaptive utilities from the current manuscript.
graphmode_log_weights <- function(v) {
    softplus <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))
    if (any(!is.finite(v))) stop("Guidance logits must be finite.", call. = FALSE)
    list(log_a = -softplus(-v), log_e = -softplus(v))
}

graphmode_guidance_factors <- function(v, K, guidance) {
    d <- K - 1L
    if (K == 1L) return(list(g = matrix(0, 0, 0), e = matrix(0, 0, 0), a = numeric()))
    if (guidance == "none") return(list(g = matrix(0, d, d), e = diag(d), a = rep(0, K)))
    if (guidance == "forced") return(list(g = diag(d), e = matrix(0, d, d), a = rep(1, K)))
    expected <- if (guidance == "shared") 1L else K
    if (length(v) != expected || (K == 2L && guidance != "shared")) {
        stop("Binary weights must be shared; invalid guidance dimension.", call. = FALSE)
    }
    logs <- graphmode_log_weights(v)
    a <- exp(logs$log_a)
    e <- exp(logs$log_e) # Never compute the small complement as 1 - a.
    if (any(a == 0 | e == 0)) stop("Guidance weights underflowed; no clipping applied.", call. = FALSE)
    if (guidance == "shared") return(list(g = sqrt(a) * diag(d),
        e = sqrt(e) * diag(d), a = rep(a, K)))
    H <- gmde_helmert_contrast(K)
    list(g = t(graphmode_qr_root(sqrt(a) * H)$R),
         e = t(graphmode_qr_root(sqrt(e) * H)$R), a = a)
}

graphmode_gate_dimension <- function(config) {
    if (config$K == 1L) return(0L)
    structured <- if (config$guidance == "none") 0L else ncol(config$Phi)
    noise <- if (config$guidance == "forced") 0L else config$n
    (config$K - 1L) * (1L + structured + noise)
}

graphmode_gate_parts <- function(x, config) {
    if (length(x) != graphmode_gate_dimension(config) || any(!is.finite(x))) {
        stop("Invalid whitened gate state.", call. = FALSE)
    }
    d <- config$K - 1L
    n <- config$n
    if (!d) return(list(c = numeric(), structured = matrix(0, n, 0), noise = matrix(0, n, 0)))
    offset <- d
    structured <- noise <- matrix(0, n, d)
    if (config$guidance != "none") {
        count <- ncol(config$Phi) * d
        gamma <- matrix(x[offset + seq_len(count)], ncol(config$Phi), d)
        structured <- config$Phi %*% gamma
        offset <- offset + count
    }
    if (config$guidance != "forced") noise <- matrix(x[offset + seq_len(n * d)], n, d)
    list(c = x[seq_len(d)], structured = structured, noise = noise)
}

graphmode_gate_utilities <- function(x, v, config, parts = NULL) {
    if (config$K == 1L) return(matrix(0, config$n, 1L))
    if (is.null(parts)) parts <- graphmode_gate_parts(x, config)
    factors <- graphmode_guidance_factors(v, config$K, config$guidance)
    contrasts <- matrix(rep(config$s_b * parts$c, each = config$n),
                        config$n, config$K - 1L) +
        parts$structured %*% t(factors$g) +
        config$tau * parts$noise %*% t(factors$e)
    utilities <- contrasts %*% t(gmde_helmert_contrast(config$K))
    if (any(!is.finite(utilities))) stop("Nonfinite adaptive utilities.", call. = FALSE)
    utilities
}

graphmode_gate_ess <- function(x, v, Z, config) {
    if (!length(x)) return(list(x = x, utilities = matrix(0, config$n, 1L), evaluations = 0L))
    F0 <- graphmode_gate_utilities(x, v, config)
    threshold <- graphmode_gate_loglik(F0, Z) + log(graphmode_uniform(1L))
    direction <- graphmode_normal(length(x))
    Fdirection <- graphmode_gate_utilities(direction, v, config)
    angle <- 2 * pi * graphmode_uniform(1L)
    lower <- angle - 2 * pi
    upper <- angle
    for (step in seq_len(config$max_ess_steps)) {
        candidate <- F0 * cos(angle) + Fdirection * sin(angle)
        if (graphmode_gate_loglik(candidate, Z) >= threshold) {
            return(list(x = x * cos(angle) + direction * sin(angle),
                        utilities = candidate, evaluations = step))
        }
        if (angle < 0) lower <- angle else upper <- angle
        angle <- lower + (upper - lower) * graphmode_uniform(1L)
    }
    stop("Adaptive ESS exceeded its registered bracket budget.", call. = FALSE)
}

graphmode_guidance_log_prior <- function(v, shape) {
    logs <- graphmode_log_weights(v)
    sum(shape[1L] * logs$log_a + shape[2L] * logs$log_e)
}

graphmode_guidance_update <- function(x, v, Z, config) {
    parts <- graphmode_gate_parts(x, config)
    utilities <- graphmode_gate_utilities(x, v, config, parts)
    accepted <- logical(length(v))
    for (h in seq_along(v)) {
        candidate <- v
        candidate[h] <- v[h] + config$guidance_proposal_sd * graphmode_normal(1L)
        next_utilities <- graphmode_gate_utilities(x, candidate, config, parts)
        ratio <- graphmode_gate_loglik(next_utilities, Z) -
            graphmode_gate_loglik(utilities, Z) +
            graphmode_guidance_log_prior(candidate, config$guidance_prior) -
            graphmode_guidance_log_prior(v, config$guidance_prior)
        if (!is.finite(ratio)) stop("Nonfinite guidance MH ratio.", call. = FALSE)
        if (log(graphmode_uniform(1L)) <= min(0, ratio)) {
            v <- candidate
            utilities <- next_utilities
            accepted[h] <- TRUE
        }
    }
    list(v = v, utilities = utilities, accepted = accepted)
}

graphmode_allocation_events <- function(before, after, K, threshold = 5L,
                                       after_to_before = seq_len(K)) {
    before <- gmde_validate_labels(before, length(before), K)
    after <- gmde_validate_labels(after, length(before), K)
    if (!is.numeric(after_to_before) || length(after_to_before) != K ||
        any(!is.finite(after_to_before)) || any(after_to_before != round(after_to_before)) ||
        !identical(sort(as.integer(after_to_before)), seq_len(K))) {
        stop("The label alignment must be a permutation.", call. = FALSE)
    }
    threshold <- gmde_scalar_integer(threshold, "threshold", 1L)
    aligned <- after_to_before[after]
    a <- tabulate(before, K)
    b <- tabulate(aligned, K)
    c(births = sum(a == 0 & b > 0), deaths = sum(a > 0 & b == 0),
      upcrossings = sum(a < threshold & b >= threshold),
      downcrossings = sum(a >= threshold & b < threshold),
      node_moves = sum(before != aligned),
      pair_changes = sum((outer(before, before, "==") != outer(aligned, aligned, "=="))[upper.tri(diag(length(before)))]))
}

graphmode_potts_logweights <- function(i, Z, response, weight, beta) {
    response[i, ] + beta * vapply(seq_len(ncol(response)), function(k) {
        sum(weight[i, Z == k])
    }, numeric(1))
}

graphmode_potts_sweep <- function(Z, response, weight, beta) {
    # Deterministic systematic scan is a valid composition of single-site Gibbs
    # kernels. Neighbor labels are updated immediately, never synchronously.
    gross <- c(births = 0, deaths = 0, upcrossings = 0, downcrossings = 0)
    K <- ncol(response)
    sizes <- tabulate(Z, K)
    for (i in seq_along(Z)) {
        old <- Z[i]
        new <- graphmode_categorical(matrix(
            graphmode_potts_logweights(i, Z, response, weight, beta), 1L))
        if (old != new) {
            gross <- gross + c(sizes[new] == 0L, sizes[old] == 1L,
                               sizes[new] == 4L, sizes[old] == 5L)
            sizes[old] <- sizes[old] - 1L
            sizes[new] <- sizes[new] + 1L
        }
        Z[i] <- new
    }
    list(Z = Z, gross = gross)
}
