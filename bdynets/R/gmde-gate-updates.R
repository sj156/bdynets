.gmde_ess_update <- function(x, F, Z, graph, H, gate, a, independent, max_steps) {
  direction <- .gmde_white_draw(nrow(graph$Phi), ncol(graph$Phi), ncol(H))
  F_direction <- .gmde_utilities(direction, graph, H, gate, a, independent)
  threshold <- .gmde_allocation_loglik(F, Z) + log(stats::runif(1L))
  angle <- stats::runif(1L, 0, 2 * pi)
  lo <- angle - 2 * pi
  hi <- angle
  steps <- 0L
  repeat {
    steps <- steps + 1L
    candidate <- F * cos(angle) + F_direction * sin(angle)
    if (.gmde_allocation_loglik(candidate, Z) >= threshold) {
      rotated <- Map(function(current, prior) current * cos(angle) +
                       prior * sin(angle), x, direction)
      return(list(x = rotated, F = candidate, evaluations = steps))
    }
    if (steps >= max_steps)
      stop("ESS diagnostic max_steps reached; no draw returned. Increase the explicit cap.")
    if (angle < 0) lo <- angle else hi <- angle
    if (lo == hi) stop("ESS bracket collapsed numerically; no draw returned.")
    angle <- stats::runif(1L, lo, hi)
  }
}

.gmde_logit_prior <- function(v, gate) {
  gate$beta_shape1 * stats::plogis(v, log.p = TRUE) +
    gate$beta_shape2 * stats::plogis(v, lower.tail = FALSE, log.p = TRUE)
}

.gmde_weight_pair <- function(v, K, shared) {
  # Evaluate both tails directly: 1 - plogis(v) can round to zero.
  a <- stats::plogis(v)
  independent <- stats::plogis(-v)
  if (any(a == 0 | independent == 0) || any(!is.finite(v)))
    stop("Adaptive logit exceeds floating-point support; no weights were clipped.")
  list(a = if (shared) rep(a, K) else a,
       independent = if (shared) rep(independent, K) else independent)
}

.gmde_guidance_update <- function(x, F, v, Z, graph, H, gate) {
  accepted <- logical(length(v))
  graph_product <- graph$Phi %*% x$Gamma
  for (h in seq_along(v)) {
    candidate_v <- v
    candidate_v[h] <- v[h] + stats::rnorm(1L, sd = gate$rw_sd)
    pair <- .gmde_weight_pair(candidate_v, nrow(H), gate$shared)
    candidate <- .gmde_utilities(x, graph, H, gate, pair$a, pair$independent,
                            graph_product)
    log_ratio <- .gmde_allocation_loglik(candidate, Z) - .gmde_allocation_loglik(F, Z) +
      .gmde_logit_prior(candidate_v[h], gate) - .gmde_logit_prior(v[h], gate)
    if (!is.finite(log_ratio)) stop("Nonfinite guidance MH ratio.")
    if (log(stats::runif(1L)) < log_ratio) {
      v <- candidate_v; F <- candidate; accepted[h] <- TRUE
    }
  }
  list(v = v, F = F, accepted = accepted)
}
