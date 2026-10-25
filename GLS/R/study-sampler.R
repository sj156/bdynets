.gls_study_ffbs_factor_discount <- function(ystar, omega, offset, H, W) {
  TT <- nrow(ystar); k <- ncol(ystar); r <- dim(H)[3L]
  m <- matrix(0, TT + 1L, r); C <- array(0, c(r, r, TT + 1L))
  a <- matrix(0, TT, r); Rpred <- array(0, c(r, r, TT))
  C[, , 1L] <- diag(1, r)
  for (t in seq_len(TT)) {
    a[t, ] <- m[t, ]; Rt <- symmetrize(C[, , t] + W[, , t])
    Ht <- matrix(H[t, , ], nrow = k, ncol = r)
    Rinv <- solve_spd(Rt); wt <- omega[t, ]
    Ct <- solve_spd(Rinv + crossprod(Ht, Ht * wt))
    mt <- Ct %*% (Rinv %*% a[t, ] + crossprod(Ht, wt * (ystar[t, ] - offset[t, ])))
    m[t + 1L, ] <- mt; C[, , t + 1L] <- Ct; Rpred[, , t] <- Rt
  }
  draw <- matrix(0, TT + 1L, r)
  draw[TT + 1L, ] <- rmvn_cov(m[TT + 1L, ], C[, , TT + 1L])
  for (t in TT:1L) {
    B <- C[, , t] %*% solve_spd(Rpred[, , t])
    mu <- m[t, ] + B %*% (draw[t + 1L, ] - a[t, ])
    V <- symmetrize(C[, , t] - B %*% Rpred[, , t] %*% t(B))
    draw[t, ] <- rmvn_cov(mu, V)
  }
  draw
}

.gls_study_initial_static_fit <- function(Y, X, Q, pairs, cfg) {
  k <- ncol(Y); P <- nrow(pairs); r <- cfg$factor_dim; pQ <- ncol(Q)
  alpha <- log(colMeans(Y) + 0.1); gamma <- matrix(0, k, pQ)
  theta_bar <- numeric(P); L <- matrix(0, P, r)
  ids <- split(seq_len(P), pairs$target)
  for (a in seq_len(k)) {
    ii <- ids[[as.character(a)]]; src <- pairs$source[ii]
    Z <- cbind(1, Q, X[, src, drop = FALSE])
    fit <- suppressWarnings(glm.fit(Z, Y[, a], family = poisson()))
    co <- fit$coefficients; co[!is.finite(co)] <- 0
    alpha[a] <- co[1L]
    if (pQ) gamma[a, ] <- co[1L + seq_len(pQ)]
    theta_bar[ii] <- co[-seq_len(1L + pQ)]
  }
  list(alpha = alpha, gamma = gamma, theta_bar = theta_bar, L = L)
}

.gls_study_draw_static_targets <- function(targets, ystar, omega, X, Q, beta, pairs,
                                alpha, gamma, theta_bar, L, lambda2, means,
                                pcn_step, cfg) {
  r <- ncol(L); pQ <- ncol(Q); ids <- split(seq_len(nrow(pairs)), pairs$target)
  proposal <- list(alpha = alpha, gamma = gamma, theta_bar = theta_bar, L = L)
  beta_obs <- beta[-1L, , drop = FALSE]
  for (a in targets) {
    ii <- ids[[as.character(a)]]; src <- pairs$source[ii]; m <- length(ii)
    Zbar <- X[, src, drop = FALSE]
    Zdyn <- do.call(cbind, lapply(seq_len(m), function(j)
      X[, src[j]] * beta_obs))
    Z <- cbind(Intercept = 1, Q, Zbar, Zdyn)
    precision_prior <- c(1 / cfg$alpha_prior_sd^2,
      rep(1 / cfg$season_prior_sd^2, pQ), 1 / lambda2[ii],
      rep(1 / (cfg$c_ell^2 * lambda2[ii]), each = r))
    mean_prior <- c(log(means[a]), rep(0, length(precision_prior) - 1L))
    Pmat <- crossprod(Z, Z * omega[, a]) + diag(precision_prior)
    rhs <- crossprod(Z, omega[, a] * ystar[, a]) + precision_prior * mean_prior
    conditional_mean <- as.numeric(solve_spd(Pmat) %*% rhs)
    conditional_draw <- rmvn_precision(Pmat, rhs)
    current <- c(alpha[a], if (pQ) gamma[a, ] else numeric(0),
                 theta_bar[ii], as.vector(t(L[ii, , drop = FALSE])))
    rho <- sqrt(max(0, 1 - pcn_step^2))
    draw <- conditional_mean + rho * (current - conditional_mean) +
      pcn_step * (conditional_draw - conditional_mean)
    cursor <- 1L
    proposal$alpha[a] <- draw[cursor]; cursor <- cursor + 1L
    if (pQ) {
      proposal$gamma[a, ] <- draw[cursor + seq_len(pQ) - 1L]
      cursor <- cursor + pQ
    }
    proposal$theta_bar[ii] <- draw[cursor + seq_len(m) - 1L]
    cursor <- cursor + m
    proposal$L[ii, ] <- matrix(draw[cursor + seq_len(m * r) - 1L],
                               nrow = m, ncol = r, byrow = TRUE)
  }
  proposal
}

.gls_study_make_target_blocks <- function(k, block_size) {
  if (!is.finite(block_size) || block_size >= k) return(list(seq_len(k)))
  split(sample(seq_len(k)), ceiling(seq_len(k) / max(1L, as.integer(block_size))))
}

.gls_study_update_static_with_reference_correction <- function(ystar, omega, X, Q, Y,
    beta, pairs, alpha, gamma, theta_bar, L, lambda2, means, discount,
    reference, pcn_step, cfg) {
  k <- ncol(Y); accepted <- proposed <- 0L
  current <- list(alpha = alpha, gamma = gamma, theta_bar = theta_bar, L = L)
  current_ref <- reference; current_logB <- .gls_study_log_factor_evolution(beta, current_ref$W)
  for (targets in .gls_study_make_target_blocks(k, cfg$static_target_block_size)) {
    candidate <- .gls_study_draw_static_targets(targets, ystar, omega, X, Q, beta, pairs,
      current$alpha, current$gamma, current$theta_bar, current$L, lambda2,
      means, pcn_step, cfg)
    candidate_ref <- .gls_study_reference_filter(Y, X, Q, pairs, candidate$alpha,
      candidate$gamma, candidate$theta_bar, candidate$L, discount, cfg)
    candidate_logB <- .gls_study_log_factor_evolution(beta, candidate_ref$W)
    proposed <- proposed + 1L
    if (log(runif(1L)) < candidate_logB - current_logB) {
      current <- candidate; current_ref <- candidate_ref
      current_logB <- candidate_logB; accepted <- accepted + 1L
    }
  }
  c(current, list(reference = current_ref, accepted = accepted,
                  proposed = proposed, logB = current_logB))
}

.gls_study_joint_scale_rw_move <- function(u_tau, u_local, tau_prior_scale, proposal_sd,
                                n_steps) {
  q_tau <- 0.5 * log(u_tau); q_local <- 0.5 * log(u_local); accepted <- 0L
  total_jump <- 0
  logprior <- log_half_cauchy_logscale(q_tau, tau_prior_scale) +
    sum(log_half_cauchy_logscale(q_local, 1))
  for (s in seq_len(n_steps)) {
    shift <- rnorm(1L, 0, proposal_sd)
    q_tau_new <- q_tau + shift; q_local_new <- q_local - shift
    if (q_tau_new < -20 || q_tau_new > 10 || any(q_local_new < -20 | q_local_new > 20)) next
    newprior <- log_half_cauchy_logscale(q_tau_new, tau_prior_scale) +
      sum(log_half_cauchy_logscale(q_local_new, 1))
    if (log(runif(1L)) < newprior - logprior) {
      q_tau <- q_tau_new; q_local <- q_local_new; logprior <- newprior
      accepted <- accepted + 1L; total_jump <- total_jump + abs(shift)
    }
  }
  list(u_tau = exp(2 * q_tau), u_local = exp(2 * q_local),
       accepted = accepted, total_jump = total_jump)
}

.gls_study_joint_scale_slice_move <- function(u_tau, u_local, tau_prior_scale, width,
                                   n_steps, max_stepout, max_shrink) {
  q_tau <- 0.5 * log(u_tau); q_local <- 0.5 * log(u_local)
  log_product <- q_tau + q_local
  lower <- max(-20, max(log_product - 20))
  upper <- min(10, min(log_product + 20))
  if (!is.finite(lower) || !is.finite(upper) || lower >= upper)
    stop("The joint tau/local-scale slice interval is empty.")
  log_target <- function(q) {
    if (!is.finite(q) || q < lower || q > upper) return(-Inf)
    log_half_cauchy_logscale(q, tau_prior_scale) +
      sum(log_half_cauchy_logscale(log_product - q, 1))
  }
  total_jump <- 0; accepted <- 0L
  for (s in seq_len(n_steps)) {
    old <- q_tau; log_height <- log_target(old) + log(runif(1L))
    left <- max(lower, old - runif(1L) * width)
    right <- min(upper, left + width)
    if (right < old) right <- old
    if (left > old) left <- old
    budget_left <- floor(runif(1L, 0, max_stepout + 1L))
    budget_right <- max_stepout - budget_left
    while (budget_left > 0L && left > lower &&
           log_target(left) > log_height) {
      left <- max(lower, left - width); budget_left <- budget_left - 1L
    }
    while (budget_right > 0L && right < upper &&
           log_target(right) > log_height) {
      right <- min(upper, right + width); budget_right <- budget_right - 1L
    }
    moved <- FALSE
    for (shrink in seq_len(max_shrink)) {
      candidate <- runif(1L, left, right)
      if (log_target(candidate) >= log_height) {
        q_tau <- candidate; moved <- TRUE; break
      }
      if (candidate < old) left <- candidate else right <- candidate
    }
    if (!moved)
      stop("Joint tau/local-scale slice update exhausted its shrinkage budget.")
    accepted <- accepted + 1L; total_jump <- total_jump + abs(q_tau - old)
  }
  list(u_tau = exp(2 * q_tau),
       u_local = exp(2 * (log_product - q_tau)),
       accepted = accepted, total_jump = total_jump)
}

.gls_study_joint_scale_move <- function(u_tau, u_local, tau_prior_scale, proposal_sd,
                             n_steps, cfg) {
  if (identical(cfg$scale_move_method, "slice"))
    return(.gls_study_joint_scale_slice_move(u_tau, u_local, tau_prior_scale,
      proposal_sd, n_steps, cfg$scale_move_max_stepout,
      cfg$scale_move_max_shrink))
  .gls_study_joint_scale_rw_move(u_tau, u_local, tau_prior_scale, proposal_sd, n_steps)
}

.gls_study_loading_log_kernel <- function(L, lambda2, c_ell) {
  r <- ncol(L); variance <- pmax(c_ell^2 * lambda2, 1e-14)
  -0.5 * (r * sum(log(variance)) + sum(rowSums(L^2) / variance))
}

.gls_study_loading_factor_rescale <- function(beta, L, delta) {
  scale <- exp(delta)
  list(beta = beta * scale, L = L / scale)
}

.gls_study_loading_factor_interweave_move <- function(beta, L, lambda2, Y, X, Q,
    pairs, alpha, gamma, theta_bar, discount, reference, proposal_sd, cfg) {
  delta <- rnorm(1L, 0, proposal_sd)
  proposal <- .gls_study_loading_factor_rescale(beta, L, delta)
  candidate_ref <- .gls_study_reference_filter(Y, X, Q, pairs, alpha, gamma, theta_bar,
                                    proposal$L, discount, cfg)
  old_target <- .gls_study_log_factor_evolution(beta, reference$W) -
    0.5 * sum(beta[1L, ]^2) + .gls_study_loading_log_kernel(L, lambda2, cfg$c_ell)
  new_target <- .gls_study_log_factor_evolution(proposal$beta, candidate_ref$W) -
    0.5 * sum(proposal$beta[1L, ]^2) +
    .gls_study_loading_log_kernel(proposal$L, lambda2, cfg$c_ell)
  log_jacobian <- (length(beta) - length(L)) * delta
  accept <- is.finite(new_target) &&
    log(runif(1L)) < new_target - old_target + log_jacobian
  if (accept)
    list(beta = proposal$beta, L = proposal$L, reference = candidate_ref,
         accepted = 1L, jump = abs(delta)) else
    list(beta = beta, L = L, reference = reference,
         accepted = 0L, jump = 0)
}

.gls_study_log_chi_target_q <- function(q, Fg, S_pair, u_local, u_tau, cfg) {
  chi <- exp(q); g <- .gls_study_affinity_from_chi(Fg, chi); r <- cfg$factor_dim
  logprior <- sum((cfg$chi_shape - 1) * log(chi) - cfg$chi_rate * chi)
  loading_prior <- -(r + 1) * sum(log(g)) -
    0.5 / u_tau * sum(S_pair / (u_local * g^2))
  logprior + loading_prior + sum(q)
}

.gls_study_update_chi <- function(chi, Fg, S_pair, u_local, u_tau, proposal_sd, cfg) {
  if (!length(chi)) return(list(chi = chi, accepted = 0L, proposed = 0L))
  q <- log(chi); qp <- q + rnorm(length(q), 0, proposal_sd)
  loga <- .gls_study_log_chi_target_q(qp, Fg, S_pair, u_local, u_tau, cfg) -
    .gls_study_log_chi_target_q(q, Fg, S_pair, u_local, u_tau, cfg)
  accept <- log(runif(1L)) < loga
  list(chi = if (accept) exp(qp) else chi,
       accepted = as.integer(accept), proposed = 1L)
}

.gls_study_pair_scale_statistic <- function(theta_bar, L, c_ell) {
  theta_bar^2 + rowSums(L^2) / c_ell^2
}

.gls_study_local_scale_shape <- function(r) (r + 2) / 2
.gls_study_global_scale_shape <- function(r, P) ((r + 1) * P + 1) / 2

.gls_study_fit_chain <- function(Y, X, Q, pairs, Fg, means, signal, chain_id,
                              cfg) {
  if (!requireNamespace("BayesLogit", quietly = TRUE))
    stop("Package 'BayesLogit' is required.")
  set.seed(.gls_study_seed("MCMC", paste(signal, chain_id), cfg)); start <- Sys.time()
  TT <- nrow(Y); k <- ncol(Y); P <- nrow(pairs); r <- cfg$factor_dim
  pQ <- ncol(Q); M <- ncol(Fg)
  fixed_tau <- cfg$shrinkage_spec %in%
    c("graph_laplace_fixed_tau", "graph_laplace_tau1")
  init <- .gls_study_initial_static_fit(Y, X, Q, pairs, cfg)
  alpha <- init$alpha; gamma <- init$gamma; theta_bar <- init$theta_bar; L <- init$L
  chi <- if (M) rep(1 / cfg$chi_rate, M) else numeric(0)
  g <- .gls_study_affinity_from_chi(Fg, chi)
  # A strict L=0 start leaves beta unobserved and makes the covariance-
  # correction update practically absorbing. Start at the scale implied by
  # the prior while retaining chain-specific overdispersed signs.
  L <- matrix(rnorm(P * r, sd = cfg$initial_loading_sd), P, r)
  u_local <- rep(1, P); nu_local <- rinvgamma(P, 0.5, 1)
  fixed_tau_value <- if (identical(cfg$shrinkage_spec, "graph_laplace_tau1")) {
    1
  } else {
    cfg$fixed_tau
  }
  u_tau <- if (fixed_tau) fixed_tau_value^2 else 0.05^2
  nu_tau <- if (fixed_tau) NA_real_ else
    rinvgamma(1L, 0.5, 1 / cfg$tau_prior_scale^2)
  prior_discount_mean <- weighted.mean(cfg$discount_grid, cfg$discount_prior)
  discount_id <- which.min(abs(cfg$discount_grid - prior_discount_mean))
  discount <- cfg$discount_grid[discount_id]
  ref <- .gls_study_reference_filter(Y, X, Q, pairs, alpha, gamma, theta_bar, L,
                          discount, cfg)
  # The reference-filter mean is finite and informed by Y. Drawing an
  # unobserved prior trajectory here can explode before L has moved.
  beta <- ref$m
  r_nb_t <- .gls_study_choose_r_nb_time(Y, cfg); RNB <- matrix(r_nb_t, TT, k)
  Ssave <- floor((cfg$n_iter - cfg$burn_mcmc) / cfg$thin)
  alpha_save <- matrix(NA_real_, Ssave, k)
  gamma_save <- array(NA_real_, c(Ssave, k, pQ))
  bar_save <- matrix(NA_real_, Ssave, P)
  L_save <- array(NA_real_, c(Ssave, P, r))
  beta_save <- array(NA_real_, c(Ssave, TT + 1L, r))
  amp_save <- mean_save <- lambda_save <- matrix(NA_real_, Ssave, P)
  tau_save <- network_save <- numeric(Ssave)
  chi_save <- matrix(NA_real_, Ssave, M)
  discount_save <- numeric(Ssave); discount_prob_save <- matrix(NA_real_, Ssave,
                                                                length(cfg$discount_grid))
  ref_m_T_save <- matrix(NA_real_, Ssave, r)
  ref_C_T_save <- array(NA_real_, c(Ssave, r, r))
  static_accept <- static_propose <- chi_accept <- chi_propose <- 0L
  scale_accept <- scale_propose <- 0L; scale_total_jump <- 0
  interweave_accept <- interweave_propose <- 0L; interweave_total_jump <- 0
  save_id <- 0L
  scale_log_sd <- log(cfg$scale_move_initial_sd)
  interweave_log_sd <- log(cfg$interweave_initial_sd)
  chi_log_sd <- log(cfg$chi_proposal_sd)
  static_log_step <- log(cfg$static_pcn_initial_step)
  for (iter in seq_len(cfg$n_iter)) {
    grid <- .gls_study_discount_grid_probabilities(beta, Y, X, Q, pairs, alpha, gamma,
      theta_bar, L, cfg, return_references = TRUE)
    discount_id <- sample(seq_along(cfg$discount_grid), 1L, prob = grid$probability)
    discount <- cfg$discount_grid[discount_id]; ref <- grid$references[[discount_id]]

    comp <- .gls_study_pair_components(X, pairs, theta_bar, L, k, r)
    offset <- .gls_study_baseline_matrix(alpha, gamma, Q) + comp$static
    net_dynamic <- matrix(0, TT, k)
    for (j in seq_len(r)) net_dynamic <- net_dynamic + comp$H[, , j] * beta[-1L, j]
    eta <- offset + net_dynamic
    zeta <- as.numeric(eta) - log(as.numeric(RNB))
    omega <- matrix(BayesLogit::rpg(length(zeta), h = as.numeric(Y) +
      as.numeric(RNB), z = zeta), TT, k)
    omega <- pmax(omega, 1e-10)
    ystar <- log(RNB) + (Y - RNB) / (2 * omega)
    beta <- .gls_study_ffbs_factor_discount(ystar, omega, offset, comp$H, ref$W)

    lambda2 <- pmax(u_tau * u_local * g^2, 1e-14)
    block <- .gls_study_update_static_with_reference_correction(ystar, omega, X, Q, Y,
      beta, pairs, alpha, gamma, theta_bar, L, lambda2, means, discount,
      ref, exp(static_log_step), cfg)
    alpha <- block$alpha; gamma <- block$gamma; theta_bar <- block$theta_bar
    L <- block$L; ref <- block$reference
    static_accept <- static_accept + block$accepted
    static_propose <- static_propose + block$proposed

    S_pair <- .gls_study_pair_scale_statistic(theta_bar, L, cfg$c_ell)
    for (sub in seq_len(cfg$shrinkage_substeps)) {
      u_local <- clamp(rinvgamma(P, .gls_study_local_scale_shape(r),
        1 / nu_local + S_pair / (2 * u_tau * g^2)), 1e-12, 1e12)
      nu_local <- clamp(rinvgamma(P, 1, 1 + 1 / u_local), 1e-12, 1e12)
      if (!fixed_tau) {
        u_tau <- clamp(rinvgamma(1L, .gls_study_global_scale_shape(r, P),
          1 / nu_tau + 0.5 * sum(S_pair / (u_local * g^2))), 1e-12, 1e6)
        nu_tau <- clamp(rinvgamma(1L, 1,
          1 / cfg$tau_prior_scale^2 + 1 / u_tau), 1e-12, 1e12)
      }
    }
    move <- list(accepted = 0L, total_jump = 0)
    if (!fixed_tau) {
      move <- .gls_study_joint_scale_move(u_tau, u_local, cfg$tau_prior_scale,
        exp(scale_log_sd), cfg$scale_move_steps, cfg)
      u_tau <- move$u_tau; u_local <- move$u_local
      scale_accept <- scale_accept + move$accepted
      scale_propose <- scale_propose + cfg$scale_move_steps
      scale_total_jump <- scale_total_jump + move$total_jump
    }
    nu_local <- clamp(rinvgamma(P, 1, 1 + 1 / u_local), 1e-12, 1e12)
    if (!fixed_tau)
      nu_tau <- clamp(rinvgamma(1L, 1,
        1 / cfg$tau_prior_scale^2 + 1 / u_tau), 1e-12, 1e12)

    chi_move <- .gls_study_update_chi(chi, Fg, S_pair, u_local, u_tau,
                           exp(chi_log_sd), cfg)
    chi <- chi_move$chi; chi_accept <- chi_accept + chi_move$accepted
    chi_propose <- chi_propose + chi_move$proposed; g <- .gls_study_affinity_from_chi(Fg, chi)

    # Interweave the centered loading/state representation with its rescaled
    # counterpart.  The dynamic predictor is unchanged, while the MH ratio
    # accounts for the identified state law, loading prior, and Jacobian.
    lambda2 <- pmax(u_tau * u_local * g^2, 1e-14)
    iter_interweave_accept <- 0L
    if (cfg$interweave_steps > 0L) for (ss in seq_len(cfg$interweave_steps)) {
      iw <- .gls_study_loading_factor_interweave_move(beta, L, lambda2, Y, X, Q, pairs,
        alpha, gamma, theta_bar, discount, ref, exp(interweave_log_sd), cfg)
      beta <- iw$beta; L <- iw$L; ref <- iw$reference
      iter_interweave_accept <- iter_interweave_accept + iw$accepted
      interweave_accept <- interweave_accept + iw$accepted
      interweave_propose <- interweave_propose + 1L
      interweave_total_jump <- interweave_total_jump + iw$jump
    }

    if (iter <= cfg$burn_mcmc) {
      static_gain <- cfg$static_pcn_adapt_rate / sqrt(iter)
      static_rate_now <- block$accepted / max(block$proposed, 1L)
      static_log_step <- clamp(static_log_step + static_gain *
        (static_rate_now - cfg$static_pcn_target_acceptance),
        log(cfg$static_pcn_min_step), log(cfg$static_pcn_max_step))
      if (!fixed_tau && identical(cfg$scale_move_method, "random_walk")) {
        gain <- cfg$scale_move_adapt_rate / sqrt(iter)
        scale_log_sd <- clamp(scale_log_sd + gain *
          (move$accepted / cfg$scale_move_steps - cfg$scale_move_target_acceptance),
          log(cfg$scale_move_min_sd), log(cfg$scale_move_max_sd))
      }
      if (cfg$interweave_steps > 0L) {
        iw_gain <- cfg$interweave_adapt_rate / sqrt(iter)
        interweave_log_sd <- clamp(interweave_log_sd + iw_gain *
          (iter_interweave_accept / cfg$interweave_steps -
             cfg$interweave_target_acceptance),
          log(cfg$interweave_min_sd), log(cfg$interweave_max_sd))
      }
      if (chi_move$proposed) {
        cgain <- cfg$chi_adapt_rate / sqrt(iter)
        chi_log_sd <- clamp(chi_log_sd + cgain *
          (chi_move$accepted - cfg$chi_target_acceptance), log(0.002), log(0.5))
      }
    }

    if (iter == min(cfg$startup_check_iteration, cfg$n_iter)) {
      startup_rate <- static_accept / max(static_propose, 1L)
      if (static_accept < cfg$startup_min_static_accepts ||
          startup_rate < cfg$startup_min_static_acceptance)
        stop(sprintf(paste0("Startup protection: static block did not move ",
          "(chain=%d, signal=%.2f, accepted=%d/%d, rate=%.4g)."),
          chain_id, signal, static_accept, static_propose, startup_rate))
      if (any(!is.finite(beta)) || max(abs(beta)) > cfg$startup_beta_abs_limit)
        stop(sprintf(paste0("Startup protection: beta is unstable ",
          "(chain=%d, signal=%.2f, max_abs_beta=%.3g)."),
          chain_id, signal, max(abs(beta))))
    }

    if (iter > cfg$burn_mcmc && (iter - cfg$burn_mcmc) %% cfg$thin == 0L) {
      save_id <- save_id + 1L; alpha_save[save_id, ] <- alpha
      if (pQ) gamma_save[save_id, , ] <- gamma
      bar_save[save_id, ] <- theta_bar; L_save[save_id, , ] <- L
      beta_save[save_id, , ] <- beta
      theta <- sweep(L %*% t(beta[-1L, , drop = FALSE]), 1L, theta_bar, "+")
      amp_save[save_id, ] <- sqrt(rowMeans(theta^2)); mean_save[save_id, ] <- rowMeans(theta)
      lambda_save[save_id, ] <- sqrt(u_tau * u_local) * g
      tau_save[save_id] <- sqrt(u_tau); chi_save[save_id, ] <- chi
      discount_save[save_id] <- discount; discount_prob_save[save_id, ] <- grid$probability
      comp_now <- .gls_study_pair_components(X, pairs, theta_bar, L, k, r)
      net <- comp_now$static
      for (j in seq_len(r)) net <- net + comp_now$H[, , j] * beta[-1L, j]
      network_save[save_id] <- sqrt(mean(net^2))
      ref_m_T_save[save_id, ] <- ref$m[TT + 1L, ]
      ref_C_T_save[save_id, , ] <- ref$C[, , TT + 1L]
    }
    if (iter == 1L || iter %% cfg$progress_every == 0L || iter == cfg$n_iter)
      progress_with_eta(sprintf("GLS q=%.2f chain=%d", signal, chain_id),
                   iter, cfg$n_iter, start)
  }
  list(chain_id = chain_id, alpha = alpha_save, gamma = gamma_save,
    theta_bar = bar_save, L = L_save, beta_path = beta_save,
    final_beta = matrix(beta_save[, TT + 1L, , drop = TRUE],
                        nrow = Ssave, ncol = r),
    amplitude = amp_save, theta_average = mean_save, lambda_pair = lambda_save,
    tau = tau_save, chi = chi_save, discount = discount_save,
    discount_probability = discount_prob_save, network_rms = network_save,
    ref_m_T = ref_m_T_save, ref_C_T = ref_C_T_save, r_nb_t = r_nb_t,
    static_acceptance = static_accept / max(static_propose, 1L),
    static_pcn_final_step = exp(static_log_step),
    chi_acceptance = chi_accept / max(chi_propose, 1L),
    scale_acceptance = if (fixed_tau) NA_real_ else
      scale_accept / max(scale_propose, 1L),
    scale_mean_abs_log_jump = if (fixed_tau) NA_real_ else
      scale_total_jump / max(scale_accept, 1L),
    interweave_acceptance = interweave_accept / max(interweave_propose, 1L),
    interweave_mean_abs_log_jump = interweave_total_jump /
      max(interweave_accept, 1L),
    interweave_final_sd = exp(interweave_log_sd),
    initial_theta_bar = init$theta_bar,
    elapsed = as.numeric(difftime(Sys.time(), start, units = "secs")))
}

.gls_study_bind_array_first <- function(items) {
  d <- dim(items[[1L]]); out <- array(NA_real_, c(sum(vapply(items, function(x) dim(x)[1L], integer(1))), d[-1L]))
  cursor <- 0L
  for (x in items) {
    n <- dim(x)[1L]; idx <- cursor + seq_len(n)
    if (length(d) == 2L) out[idx, ] <- x else if (length(d) == 3L) out[idx, , ] <- x
    cursor <- cursor + n
  }
  out
}

.gls_study_fit_impl <- function(Y, X, Q, pairs, Fg, means, signal, cfg) {
  ids <- seq_len(cfg$n_chains); worker <- function(ch)
    .gls_study_fit_chain(Y, X, Q, pairs, Fg, means, signal, ch, cfg)
  chains <- if (cfg$parallel_chains && cfg$n_chains > 1L && .Platform$OS.type != "windows")
    parallel::mclapply(ids, worker, mc.cores = min(cfg$n_chains,
      max(1L, parallel::detectCores(logical = FALSE)))) else lapply(ids, worker)
  failed <- vapply(chains, inherits, logical(1), what = "try-error")
  if (any(failed))
    stop("MCMC chain failure(s): ", paste(ids[failed], collapse = ", "), "\n",
         paste(vapply(chains[failed], as.character, character(1)), collapse = "\n"))
  nsave <- vapply(chains, function(z) nrow(z$alpha), integer(1))
  list(label = "GLS", pairs = pairs, Fg = Fg,
    chain_id = rep(ids, nsave), chains = chains,
    alpha = do.call(rbind, lapply(chains, `[[`, "alpha")),
    gamma = .gls_study_bind_array_first(lapply(chains, `[[`, "gamma")),
    theta_bar = do.call(rbind, lapply(chains, `[[`, "theta_bar")),
    L = .gls_study_bind_array_first(lapply(chains, `[[`, "L")),
    beta_path = .gls_study_bind_array_first(lapply(chains, `[[`, "beta_path")),
    final_beta = do.call(rbind, lapply(chains, `[[`, "final_beta")),
    amplitude = do.call(rbind, lapply(chains, `[[`, "amplitude")),
    theta_average = do.call(rbind, lapply(chains, `[[`, "theta_average")),
    lambda_pair = do.call(rbind, lapply(chains, `[[`, "lambda_pair")),
    tau = unlist(lapply(chains, `[[`, "tau")),
    chi = do.call(rbind, lapply(chains, `[[`, "chi")),
    discount = unlist(lapply(chains, `[[`, "discount")),
    discount_probability = do.call(rbind, lapply(chains, `[[`, "discount_probability")),
    network_rms = unlist(lapply(chains, `[[`, "network_rms")),
    ref_m_T = do.call(rbind, lapply(chains, `[[`, "ref_m_T")),
    ref_C_T = .gls_study_bind_array_first(lapply(chains, `[[`, "ref_C_T")),
    r_nb_t = chains[[1L]]$r_nb_t,
    initial_theta_bar = chains[[1L]]$initial_theta_bar,
    acceptance = data.frame(chain = ids,
      static_block = vapply(chains, `[[`, numeric(1), "static_acceptance"),
      static_pcn_final_step = vapply(chains, `[[`, numeric(1),
                                     "static_pcn_final_step"),
      chi = vapply(chains, `[[`, numeric(1), "chi_acceptance"),
      joint_scale_method = rep(if (cfg$shrinkage_spec %in%
        c("graph_laplace_fixed_tau", "graph_laplace_tau1")) {
          tau_value <- if (identical(cfg$shrinkage_spec,
            "graph_laplace_tau1")) 1 else cfg$fixed_tau
          sprintf("fixed_tau_%g", tau_value)
        } else cfg$scale_move_method, length(ids)),
      joint_scale = vapply(chains, `[[`, numeric(1), "scale_acceptance"),
      joint_scale_mean_abs_log_jump = vapply(chains, `[[`, numeric(1),
                                             "scale_mean_abs_log_jump"),
      loading_factor_interweave = vapply(chains, `[[`, numeric(1),
                                         "interweave_acceptance"),
      interweave_mean_abs_log_jump = vapply(chains, `[[`, numeric(1),
                                            "interweave_mean_abs_log_jump"),
      interweave_final_sd = vapply(chains, `[[`, numeric(1),
                                   "interweave_final_sd")))
}
