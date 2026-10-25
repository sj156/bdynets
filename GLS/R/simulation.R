.gls_study_make_affinity_features <- function(pairs, tolerance = 1e-9) {
  finite_hop <- pairs$hop[is.finite(pairs$hop)]
  unreachable_hop <- if (length(finite_hop)) max(finite_hop) + 1 else 1
  raw <- cbind(
    self = as.numeric(pairs$target == pairs$source),
    reachable = as.numeric(is.finite(pairs$hop)),
    proximity = -ifelse(is.finite(pairs$hop), pairs$hop, unreachable_hop)
  )
  centered <- scale(raw, center = TRUE, scale = TRUE)
  keep_finite <- apply(centered, 2L, function(x) all(is.finite(x)) && sd(x) > tolerance)
  centered <- centered[, keep_finite, drop = FALSE]
  if (!ncol(centered)) return(matrix(numeric(0), nrow(pairs), 0L))
  qr_obj <- qr(centered, tol = tolerance)
  keep <- sort(qr_obj$pivot[seq_len(qr_obj$rank)])
  out <- centered[, keep, drop = FALSE]
  attr(out, "raw_center") <- attr(centered, "scaled:center")[keep]
  attr(out, "raw_scale") <- attr(centered, "scaled:scale")[keep]
  out
}

.gls_study_edge_graph_adjacency <- function(graph) {
  graph <- .validate_gls_graph(graph)
  edges <- graph$edges
  k <- nrow(edges)
  A_edge <- matrix(0, k, k,
    dimnames = list(target = edges$name, source = edges$name))
  endpoints <- lapply(seq_len(k), function(j) c(edges$from[j], edges$to[j]))
  for (a in seq_len(k)) for (b in seq_len(a - 1L)) {
    if (length(intersect(endpoints[[a]], endpoints[[b]]))) {
      A_edge[a, b] <- A_edge[b, a] <- 1
    }
  }
  A_edge
}

.gls_study_make_graph_laplace_feature <- function(graph, pairs, rho0,
                                                   tolerance = 1e-9) {
  if (length(rho0) != 1L || !is.finite(rho0) || rho0 < 0 || rho0 > 1) {
    stop("graph_laplace_rho0 must be one finite number in [0, 1].",
         call. = FALSE)
  }
  A_edge <- .gls_study_edge_graph_adjacency(graph)
  degree <- rowSums(A_edge)
  precision <- diag(degree + 1, nrow(A_edge)) - rho0 * A_edge
  covariance <- chol2inv(chol(precision))
  marginal_sd <- sqrt(diag(covariance))
  correlation <- covariance / outer(marginal_sd, marginal_sd)
  raw <- correlation[cbind(pairs$target, pairs$source)]
  raw_center <- mean(raw)
  raw_scale <- sqrt(mean((raw - raw_center)^2))
  if (!is.finite(raw_scale) || raw_scale <= tolerance) {
    stop("The Graph-Laplace score is constant on the candidate domain.",
         call. = FALSE)
  }
  out <- matrix((raw - raw_center) / raw_scale, ncol = 1L,
                dimnames = list(NULL, "graph_laplace"))
  attr(out, "raw_score") <- raw
  attr(out, "raw_center") <- raw_center
  attr(out, "raw_scale") <- raw_scale
  attr(out, "rho0") <- rho0
  attr(out, "edge_adjacency") <- A_edge
  out
}

.gls_study_affinity_from_chi <- function(Fg, chi) {
  if (!ncol(Fg)) return(rep(1, nrow(Fg)))
  if (length(chi) != ncol(Fg) || any(!is.finite(chi)) || any(chi <= 0))
    stop("chi is incompatible with the affinity feature matrix.")
  log_g <- as.numeric(Fg %*% chi)
  log_g <- log_g - mean(log_g)
  g <- exp(log_g)
  if (abs(mean(log(g))) > 1e-10) stop("Affinity normalization failed.")
  g
}

.gls_study_reference_objective <- function(b, a, Rinv, offset, H, Y, eta_limit) {
  eta <- as.numeric(offset + H %*% b)
  if (any(eta > eta_limit)) return(Inf)
  mu <- exp(pmax(eta, -eta_limit)); d <- b - a
  0.5 * sum(d * (Rinv %*% d)) + sum(mu - Y * eta)
}

.gls_study_reference_update_one <- function(a, C_previous, discount, offset, H, Y, cfg) {
  # The experiments use r=1.  The scalar algebra is exactly the same Laplace
  # update as the generic branch, but avoids thousands of tiny Cholesky
  # factorizations inside every reference-filter recomputation.
  if (length(a) == 1L) {
    previous_variance <- as.numeric(C_previous)[1L]
    R_scalar <- previous_variance / discount
    Rinv_scalar <- 1 / R_scalar
    h <- as.numeric(H[, 1L]); b <- as.numeric(a); converged <- FALSE
    for (iter in seq_len(cfg$reference_newton_max_iterations)) {
      eta <- as.numeric(offset + h * b)
      if (any(eta > cfg$reference_eta_limit))
        stop("Reference-filter linear predictor exceeded its numerical limit.")
      mu <- exp(pmax(eta, -cfg$reference_eta_limit))
      grad <- Rinv_scalar * (b - a) + sum(h * (mu - Y))
      Hess <- Rinv_scalar + sum(h^2 * mu)
      step <- grad / Hess
      if (abs(step) < cfg$reference_newton_tolerance) {
        converged <- TRUE; break
      }
      old_obj <- 0.5 * Rinv_scalar * (b - a)^2 + sum(mu - Y * eta)
      step_scale <- 1
      repeat {
        candidate <- b - step_scale * step
        candidate_eta <- as.numeric(offset + h * candidate)
        new_obj <- if (any(candidate_eta > cfg$reference_eta_limit)) Inf else
          0.5 * Rinv_scalar * (candidate - a)^2 +
            sum(exp(pmax(candidate_eta, -cfg$reference_eta_limit)) -
                  Y * candidate_eta)
        if (is.finite(new_obj) && new_obj <= old_obj + 1e-10) break
        step_scale <- step_scale / 2
        if (step_scale < 2^-20)
          stop("Reference-filter Newton line search failed.")
      }
      b <- candidate
    }
    eta <- as.numeric(offset + h * b)
    mu <- exp(pmax(eta, -cfg$reference_eta_limit))
    if (!converged) {
      grad <- Rinv_scalar * (b - a) + sum(h * (mu - Y))
      if (abs(grad) > 1e-5)
        stop("Reference-filter Newton iteration did not converge.")
    }
    C_scalar <- 1 / (Rinv_scalar + sum(h^2 * mu))
    return(list(m = b, C = matrix(C_scalar, 1L, 1L),
      R = matrix(R_scalar, 1L, 1L),
      W = matrix((1 / discount - 1) * previous_variance, 1L, 1L)))
  }
  R <- C_previous / discount; Rinv <- solve_spd(R); b <- as.numeric(a)
  converged <- FALSE
  for (iter in seq_len(cfg$reference_newton_max_iterations)) {
    eta <- as.numeric(offset + H %*% b)
    if (any(eta > cfg$reference_eta_limit))
      stop("Reference-filter linear predictor exceeded its numerical limit.")
    mu <- exp(pmax(eta, -cfg$reference_eta_limit))
    grad <- as.numeric(Rinv %*% (b - a) + crossprod(H, mu - Y))
    Hess <- Rinv + crossprod(H, H * mu)
    step <- as.numeric(solve_spd(Hess) %*% grad)
    if (max(abs(step)) < cfg$reference_newton_tolerance) {
      converged <- TRUE; break
    }
    old_obj <- .gls_study_reference_objective(b, a, Rinv, offset, H, Y,
                                   cfg$reference_eta_limit)
    step_scale <- 1
    repeat {
      candidate <- b - step_scale * step
      new_obj <- .gls_study_reference_objective(candidate, a, Rinv, offset, H, Y,
                                     cfg$reference_eta_limit)
      if (is.finite(new_obj) && new_obj <= old_obj + 1e-10) break
      step_scale <- step_scale / 2
      if (step_scale < 2^-20) stop("Reference-filter Newton line search failed.")
    }
    b <- candidate
  }
  if (!converged) {
    eta <- as.numeric(offset + H %*% b)
    mu <- exp(pmax(eta, -cfg$reference_eta_limit))
    grad <- as.numeric(Rinv %*% (b - a) + crossprod(H, mu - Y))
    if (max(abs(grad)) > 1e-5)
      stop("Reference-filter Newton iteration did not converge.")
  }
  eta <- as.numeric(offset + H %*% b)
  mu <- exp(pmax(eta, -cfg$reference_eta_limit))
  C <- solve_spd(Rinv + crossprod(H, H * mu))
  list(m = b, C = C, R = R,
       W = symmetrize((1 / discount - 1) * C_previous))
}

.gls_study_pair_components <- function(X, pairs, theta_bar, L, k, r, d = NULL) {
  if (is.null(d)) d <- rep(1, nrow(pairs))
  ids <- split(seq_len(nrow(pairs)), pairs$target)
  static <- matrix(0, nrow(X), k); H <- array(0, c(nrow(X), k, r))
  for (a in seq_len(k)) {
    ii <- ids[[as.character(a)]]; src <- pairs$source[ii]
    static[, a] <- X[, src, drop = FALSE] %*% (d[ii] * theta_bar[ii])
    H[, a, ] <- X[, src, drop = FALSE] %*% (L[ii, , drop = FALSE] * d[ii])
  }
  list(static = static, H = H)
}

.gls_study_baseline_matrix <- function(alpha, gamma, Q) {
  out <- outer(rep(1, nrow(Q)), alpha)
  if (ncol(Q)) out <- out + Q %*% t(gamma)
  out
}

.gls_study_reference_filter <- function(Y, X, Q, pairs, alpha, gamma, theta_bar, L,
                             discount, cfg, d = NULL) {
  TT <- nrow(Y); k <- ncol(Y); r <- ncol(L)
  comp <- .gls_study_pair_components(X, pairs, theta_bar, L, k, r, d)
  offset <- .gls_study_baseline_matrix(alpha, gamma, Q) + comp$static
  m <- matrix(0, TT + 1L, r); C <- array(0, c(r, r, TT + 1L))
  W <- array(0, c(r, r, TT)); C[, , 1L] <- diag(1, r)
  for (t in seq_len(TT)) {
    Ht <- matrix(comp$H[t, , ], nrow = k, ncol = r)
    z <- .gls_study_reference_update_one(m[t, ], C[, , t], discount, offset[t, ], Ht,
                              Y[t, ], cfg)
    m[t + 1L, ] <- z$m; C[, , t + 1L] <- z$C; W[, , t] <- z$W
  }
  list(m = m, C = C, W = W, offset = offset, H = comp$H)
}

.gls_study_log_factor_evolution <- function(beta, W) {
  TT <- dim(W)[3L]; out <- 0
  if (nrow(beta) != TT + 1L) stop("beta must contain time zero through T.")
  for (t in seq_len(TT)) {
    z <- beta[t + 1L, ] - beta[t, ]; Wi <- solve_spd(W[, , t])
    out <- out - 0.5 * (length(z) * log(2 * pi) + .gls_study_logdet_spd(W[, , t]) +
                        sum(z * (Wi %*% z)))
  }
  out
}

.gls_study_discount_grid_probabilities <- function(beta, Y, X, Q, pairs, alpha, gamma,
                                        theta_bar, L, cfg, d = NULL,
                                        return_references = FALSE) {
  K <- length(cfg$discount_grid); logw <- numeric(K)
  refs <- if (return_references) vector("list", K) else NULL
  for (j in seq_len(K)) {
    ref <- .gls_study_reference_filter(Y, X, Q, pairs, alpha, gamma, theta_bar, L,
                            cfg$discount_grid[j], cfg, d)
    logw[j] <- log(cfg$discount_prior[j]) + .gls_study_log_factor_evolution(beta, ref$W)
    if (return_references) refs[[j]] <- ref
  }
  prob <- exp(logw - .gls_study_log_sum_exp(logw))
  list(probability = prob, log_weight = logw, references = refs)
}

.gls_study_choose_active_pairs <- function(pairs, cfg) {
  set.seed(.gls_study_seed("DGP", "active pairs", cfg))
  take <- function(label, n) {
    z <- pairs[pairs$pair_class == label, , drop = FALSE]
    if (!nrow(z) || n <= 0) return(z[FALSE, , drop = FALSE])
    z[sample(seq_len(nrow(z)), min(n, nrow(z))), , drop = FALSE]
  }
  self <- pairs[pairs$pair_class == "self", , drop = FALSE]
  out <- rbind(self, take("one-hop", cfg$active_one_hop),
               take("two-hop", cfg$active_two_hop),
               take("far/unreachable", cfg$active_far))
  mult <- c(self = cfg$self_multiplier, `one-hop` = cfg$one_hop_multiplier,
            `two-hop` = cfg$two_hop_multiplier,
            `far/unreachable` = cfg$far_multiplier)
  out$multiplier <- unname(mult[out$pair_class])
  out$sign <- rep(c(1, 1, -1, 1, -1), length.out = nrow(out))
  rownames(out) <- NULL; out
}

.gls_study_make_true_season <- function(k, cfg) {
  set.seed(.gls_study_seed("DGP", "season", cfg))
  p1 <- runif(k, -pi, pi); p2 <- runif(k, -pi, pi)
  a1 <- cfg$season_amplitude_1 * runif(k, 0.85, 1.15)
  a2 <- cfg$season_amplitude_2 * runif(k, 0.75, 1.25)
  cbind(a1 * cos(p1), a1 * sin(p1), a2 * cos(p2), a2 * sin(p2))
}

.gls_study_initialize_true_pair_effects <- function(pairs, active, signal, cfg) {
  P <- nrow(pairs); r <- cfg$factor_dim
  theta_bar <- numeric(P); L <- matrix(0, P, r)
  if (signal <= 0 || !nrow(active))
    return(list(theta_bar = theta_bar, L = L))
  id <- match(active$pair_id, pairs$pair_id)
  target <- signal * active$multiplier
  theta_bar[id] <- active$sign * target * sqrt(cfg$static_energy_fraction)
  L[id, 1L] <- active$sign * target * sqrt(1 - cfg$static_energy_fraction)
  list(theta_bar = theta_bar, L = L)
}

.gls_study_simulate_once <- function(graph, pairs, alpha, gamma, theta_bar, L,
                                  means, center, scale, cfg, seed) {
  set.seed(seed); TT <- cfg$n_total; k <- nrow(graph$edges); r <- cfg$factor_dim
  Q <- .gls_study_fourier_design(TT, cfg$season_period, cfg$true_fourier_order)
  Y <- matrix(0L, TT, k); lambda <- matrix(0, TT, k); X <- matrix(0, TT, k)
  beta <- matrix(0, TT + 1L, r); beta[1L, ] <- rnorm(r)
  pre_y <- rpois(k, means)
  mref <- rep(0, r); Cref <- diag(1, r)
  ref_m <- matrix(0, TT + 1L, r); ref_C <- array(0, c(r, r, TT + 1L))
  ref_C[, , 1L] <- Cref
  ids <- split(seq_len(nrow(pairs)), pairs$target)
  for (t in seq_len(TT)) {
    X[t, ] <- (log1p(pre_y) - center) / scale
    Wt <- (1 / cfg$true_discount - 1) * Cref
    beta[t + 1L, ] <- rmvn_cov(beta[t, ], Wt)
    static <- dynamic <- numeric(k); Ht <- matrix(0, k, r)
    for (a in seq_len(k)) {
      ii <- ids[[as.character(a)]]; src <- pairs$source[ii]
      static[a] <- sum(X[t, src] * theta_bar[ii])
      Ht[a, ] <- colSums(L[ii, , drop = FALSE] * X[t, src])
      dynamic[a] <- sum(Ht[a, ] * beta[t + 1L, ])
    }
    eta <- alpha + as.numeric(gamma %*% Q[t, ]) + static + dynamic
    lam <- exp(eta)
    if (any(!is.finite(lam)) || any(lam > cfg$maximum_rate))
      stop("Unstable GLS simulation DGP; reduce the signal or baseline.")
    lambda[t, ] <- lam; Y[t, ] <- rpois(k, lam)
    upd <- .gls_study_reference_update_one(mref, Cref, cfg$true_discount,
      alpha + as.numeric(gamma %*% Q[t, ]) + static, Ht, Y[t, ], cfg)
    mref <- upd$m; Cref <- upd$C; ref_m[t + 1L, ] <- mref
    ref_C[, , t + 1L] <- Cref; pre_y <- Y[t, ]
  }
  list(Y = Y, lambda = lambda, X_raw = X, Q_true = Q, beta = beta,
       pre_y = pre_y, ref_m = ref_m, ref_C = ref_C)
}

.gls_study_simulate_grid_impl <- function(graph, pairs, active, signal, cfg) {
  k <- nrow(graph$edges)
  set.seed(.gls_study_seed("DGP", "means", cfg)); means <- runif(k, cfg$target_mean_min,
                                                               cfg$target_mean_max)
  center <- log1p(means); scale <- rep(cfg$x_scale, k)
  gamma <- .gls_study_make_true_season(k, cfg); effects <- .gls_study_initialize_true_pair_effects(pairs,
    active, signal, cfg); alpha <- log(means)
  if (signal > 0 && nrow(active) && cfg$dgp_amplitude_calibration_steps > 0L) {
    active_id <- match(active$pair_id, pairs$pair_id)
    target_amplitude <- signal * active$multiplier
    for (step in seq_len(cfg$dgp_amplitude_calibration_steps)) {
      pilot <- .gls_study_simulate_once(graph, pairs, alpha, gamma,
        effects$theta_bar, effects$L, means, center, scale, cfg,
        .gls_study_seed("DGP amplitude calibration", paste(signal, step), cfg))
      pilot_theta <- sweep(effects$L %*% t(pilot$beta[-1L, , drop = FALSE]),
                           1L, effects$theta_bar, "+")
      actual <- sqrt(rowMeans(pilot_theta[active_id, , drop = FALSE]^2))
      ratio <- target_amplitude / pmax(actual, 1e-8)
      effects$theta_bar[active_id] <- effects$theta_bar[active_id] * ratio
      effects$L[active_id, ] <- effects$L[active_id, , drop = FALSE] * ratio
    }
  }
  history <- list(); last <- NULL
  for (cal in seq_len(cfg$calibration_iterations)) {
    last <- .gls_study_simulate_once(graph, pairs, alpha, gamma, effects$theta_bar,
      effects$L, means, center, scale, cfg,
      .gls_study_seed("DGP calibration", paste(signal, cal), cfg))
    observed <- colMeans(last$Y)
    history[[cal]] <- data.frame(signal = signal, iteration = cal,
      edge = graph$edges$name, target_mean = means, observed_mean = observed,
      alpha = alpha)
    alpha <- alpha + cfg$calibration_damping * log(means / pmax(observed, 0.1))
  }
  out <- .gls_study_simulate_once(graph, pairs, alpha, gamma, effects$theta_bar,
    effects$L, means, center, scale, cfg,
    .gls_study_seed("DGP final", as.character(signal), cfg))
  theta <- sweep(effects$L %*% t(out$beta[-1L, , drop = FALSE]), 1L,
                 effects$theta_bar, "+")
  out$true_amplitude <- sqrt(rowMeans(theta^2))
  out$theta_bar <- effects$theta_bar; out$L <- effects$L; out$theta <- theta
  out$alpha <- alpha; out$gamma <- gamma; out$means <- means
  out$center <- center; out$scale <- scale; out$signal <- signal
  out$calibration <- do.call(rbind, history); out$active <- active
  out
}
