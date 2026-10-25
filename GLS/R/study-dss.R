.gls_study_one_time_pair_components <- function(x, pairs, theta_bar, L, k, r, d) {
  if (is.null(d)) d <- rep(1, nrow(pairs)); ids <- split(seq_len(nrow(pairs)), pairs$target)
  static <- numeric(k); H <- matrix(0, k, r)
  for (a in seq_len(k)) {
    ii <- ids[[as.character(a)]]; src <- pairs$source[ii]
    static[a] <- sum(x[src] * d[ii] * theta_bar[ii])
    H[a, ] <- colSums(L[ii, , drop = FALSE] * (x[src] * d[ii]))
  }
  list(static = static, H = H)
}

.gls_study_rolling_prediction <- function(fit, X, Q, Y, rows,
    conditioning_rows = integer(0), d = NULL, draw_ids = NULL, seed = 1L,
    cfg, return_draws = TRUE) {
  if (is.null(draw_ids)) draw_ids <- balanced_indices(fit$chain_id,
    cfg$predictive_draws, seed)
  pairs <- fit$pairs; k <- ncol(Y); r <- ncol(fit$final_beta)
  sequence_rows <- c(conditioning_rows, rows); rn_all <- .gls_study_choose_r_nb_time(Y, cfg)
  lambda <- if (return_draws) array(NA_real_, c(length(rows), k, length(draw_ids))) else NULL
  mean_lambda <- matrix(0, length(rows), k)
  set.seed(seed)
  for (jj in seq_along(draw_ids)) {
    s <- draw_ids[jj]; alpha <- fit$alpha[s, ]
    gamma <- matrix(fit$gamma[s, , , drop = TRUE], nrow = k)
    theta_bar <- fit$theta_bar[s, ]
    L <- matrix(fit$L[s, , , drop = TRUE], nrow = nrow(pairs), ncol = r)
    beta_previous <- as.numeric(fit$final_beta[s, ])
    ref_m <- as.numeric(fit$ref_m_T[s, ])
    ref_C <- matrix(fit$ref_C_T[s, , ], r, r)
    discount <- fit$discount[s]
    for (tt in sequence_rows) {
      comp <- .gls_study_one_time_pair_components(X[tt, ], pairs, theta_bar, L, k, r, d)
      offset <- alpha + if (ncol(Q)) as.numeric(gamma %*% Q[tt, ]) else 0
      offset <- offset + comp$static
      W <- symmetrize((1 / discount - 1) * ref_C)
      beta_predictive <- rmvn_cov(beta_previous, W)
      eta_predictive <- offset + as.numeric(comp$H %*% beta_predictive)
      lam <- exp(clamp(eta_predictive, cfg$intensity_lower, cfg$intensity_upper))
      h <- match(tt, rows)
      if (!is.na(h)) {
        mean_lambda[h, ] <- mean_lambda[h, ] + lam
        if (return_draws) lambda[h, , jj] <- lam
      }
      rn <- rn_all[tt]
      omega <- pmax(BayesLogit::rpg(k, h = Y[tt, ] + rn,
        z = eta_predictive - log(rn)), 1e-10)
      ystar <- log(rn) + (Y[tt, ] - rn) / (2 * omega)
      Winv <- solve_spd(W)
      precision <- Winv + crossprod(comp$H, comp$H * omega)
      rhs <- Winv %*% beta_previous + crossprod(comp$H, omega * (ystar - offset))
      beta_previous <- rmvn_precision(precision, rhs)
      upd <- .gls_study_reference_update_one(ref_m, ref_C, discount, offset, comp$H,
                                  Y[tt, ], cfg)
      ref_m <- upd$m; ref_C <- upd$C
    }
  }
  mean_lambda <- mean_lambda / length(draw_ids)
  list(mean = mean_lambda, lambda = lambda, draw_ids = draw_ids,
       deviance = mean(poisson_deviance(Y[rows, , drop = FALSE], mean_lambda)))
}

.gls_study_nnlasso_path <- function(G, cvec, penalties, tolerance = 1e-9,
                         max_iterations = 5000L) {
  p <- length(cvec); d <- numeric(p); out <- matrix(0, p, length(penalties))
  diagonal <- pmax(diag(G), 1e-12)
  for (li in seq_along(penalties)) {
    penalty <- penalties[li]
    for (iter in seq_len(max_iterations)) {
      previous <- d
      for (j in seq_len(p)) {
        partial <- sum(G[j, ] * d) - G[j, j] * d[j]
        d[j] <- max(0, (cvec[j] - partial - penalty / 2) / diagonal[j])
      }
      if (max(abs(d - previous)) < tolerance) break
    }
    out[, li] <- d
  }
  out
}

.gls_study_build_dss_path <- function(fit, Xtrain, Qtrain, Ytrain, signal, cfg) {
  draws <- balanced_indices(fit$chain_id, cfg$dss_max_draws,
                            .gls_study_seed("DSS path", signal, cfg))
  pairs <- fit$pairs; k <- ncol(Xtrain); P <- nrow(pairs); r <- ncol(fit$final_beta)
  objective <- vector("list", k); weight_scale <- max(mean(Ytrain), 1)
  for (a in seq_len(k)) {
    ii <- which(pairs$target == a); src <- pairs$source[ii]; m <- length(ii)
    G <- matrix(0, m, m); cv <- numeric(m)
    for (s in draws) {
      beta <- matrix(fit$beta_path[s, -1L, , drop = TRUE], nrow(Xtrain), r)
      Ls <- matrix(fit$L[s, ii, , drop = TRUE], m, r)
      theta <- sweep(beta %*% t(Ls), 2L, fit$theta_bar[s, ii], "+")
      U <- theta * Xtrain[, src, drop = FALSE]; ynet <- rowSums(U)
      gamma <- as.numeric(fit$gamma[s, a, ])
      base <- fit$alpha[s, a] + if (ncol(Qtrain)) as.numeric(Qtrain %*% gamma) else 0
      w <- if (cfg$dss_weight == "intensity")
        exp(clamp(base + ynet, cfg$intensity_lower, cfg$intensity_upper)) /
          weight_scale else rep(1, nrow(Xtrain))
      Uw <- U * sqrt(w); yw <- ynet * sqrt(w)
      G <- G + crossprod(Uw); cv <- cv + as.numeric(crossprod(Uw, yw))
    }
    n <- length(draws) * nrow(Xtrain)
    objective[[a]] <- list(ii = ii, G = G / n, cv = cv / n)
  }
  lmax <- max(vapply(objective, function(z) max(2 * z$cv), numeric(1)), 1e-10)
  penalties <- exp(seq(log(lmax), log(lmax * cfg$dss_lambda_min_ratio),
                       length.out = cfg$dss_n_lambda))
  D <- matrix(0, P, length(penalties))
  for (a in seq_len(k)) {
    z <- objective[[a]]; D[z$ii, ] <- .gls_study_nnlasso_path(z$G, z$cv, penalties)
  }
  D <- cbind(zero = numeric(P), D, full = rep(1, P))
  penalties <- c(Inf, penalties, 0)
  type <- c("zero network", rep("DSS path", cfg$dss_n_lambda), "continuous full")
  support <- D > cfg$dss_zero_tolerance
  signature <- apply(support, 2L, paste0, collapse = "")
  keep <- !duplicated(signature, fromLast = TRUE)
  D <- D[, keep, drop = FALSE]; penalties <- penalties[keep]; type <- type[keep]
  support <- support[, keep, drop = FALSE]; size <- colSums(support)
  ord <- order(size, -penalties); D <- D[, ord, drop = FALSE]
  list(D = D, support = support[, ord, drop = FALSE], penalty = penalties[ord],
       type = type[ord], size = size[ord], draws = draws)
}

.gls_study_select_dss_by_validation <- function(path, fit, X, Q, Y, validation,
                                     conditioning_rows, signal, cfg) {
  full_id <- which(path$type == "continuous full")[1L]
  candidate_ids <- seq_along(path$size)
  stride <- max(1L, as.integer(cfg$dss_path_eval_stride))
  evaluate <- sort(unique(c(candidate_ids[seq.int(1L, length(candidate_ids), by = stride)],
                            head(candidate_ids, 2L), tail(candidate_ids, 2L), full_id)))
  draw_ids <- balanced_indices(fit$chain_id, cfg$dss_validation_draws,
                               .gls_study_seed("DSS validation draws", signal, cfg))
  common_seed <- .gls_study_seed("DSS validation CRN", signal, cfg)
  loss <- rep(NA_real_, length(candidate_ids))
  for (j in evaluate) {
    pred <- .gls_study_rolling_prediction(fit, X, Q, Y, validation,
      conditioning_rows, path$D[, j], draw_ids, common_seed, cfg,
      return_draws = FALSE)
    loss[j] <- pred$deviance
  }
  full_loss <- loss[full_id]
  if (!is.finite(full_loss)) stop("The continuous DSS reference was not evaluated.")
  admissible <- which(is.finite(loss) &
    (loss - full_loss) / max(full_loss, 1e-12) <= cfg$dss_predictive_tolerance)
  if (!length(admissible)) admissible <- full_id
  smallest <- min(path$size[admissible]); admissible <- admissible[path$size[admissible] == smallest]
  selected_id <- admissible[which.min(loss[admissible])]
  table <- data.frame(candidate = candidate_ids, type = path$type,
    penalty = path$penalty, selected_pairs = path$size,
    validation_deviance = loss,
    relative_to_full = (loss - full_loss) / max(full_loss, 1e-12),
    evaluated = candidate_ids %in% evaluate,
    chosen = candidate_ids == selected_id)
  list(d = path$D[, selected_id], selected = path$support[, selected_id],
       table = table, full_loss = full_loss, selected_loss = loss[selected_id],
       selected_id = selected_id)
}

.gls_study_run_dss <- function(fit, Xtrain, Qtrain, Ytrain, Xall, Qall, Yall,
                            validation, conditioning_rows, signal, cfg) {
  path <- .gls_study_build_dss_path(fit, Xtrain, Qtrain, Ytrain, signal, cfg)
  chosen <- .gls_study_select_dss_by_validation(path, fit, Xall, Qall, Yall, validation,
                                     conditioning_rows, signal, cfg)
  c(chosen, list(path = path))
}

.gls_study_marginal_pair_summary <- function(fit, cfg) {
  pip <- colMeans(fit$amplitude > cfg$delta_A)
  affinity_draws <- if (ncol(fit$chi))
    t(apply(fit$chi, 1L, function(z) .gls_study_affinity_from_chi(fit$Fg, z))) else
      matrix(1, nrow(fit$alpha), nrow(fit$pairs))
  data.frame(pair_id = fit$pairs$pair_id, posterior_amplitude = colMeans(fit$amplitude),
    posterior_mean_abs = colMeans(abs(fit$theta_average)),
    posterior_lambda = apply(fit$lambda_pair, 2L, median),
    posterior_affinity = apply(affinity_draws, 2L, median),
    exceedance_probability = pip, marginal_selected = pip > cfg$c_A)
}

.gls_study_shrinkage_dss_summary <- function(fit, Xtrain, dss = NULL, truth = NULL, cfg) {
  P <- nrow(fit$pairs); draws <- seq_len(nrow(fit$alpha)); contribution <- numeric(P)
  dynamic_amplitude <- numeric(P)
  for (s in draws) {
    beta <- matrix(fit$beta_path[s, -1L, , drop = TRUE], nrow(Xtrain),
                   ncol(fit$final_beta))
    Ls <- matrix(fit$L[s, , , drop = TRUE], nrow = P,
                 ncol = ncol(fit$final_beta))
    theta <- sweep(Ls %*% t(beta), 1L,
                   fit$theta_bar[s, ], "+")
    contribution <- contribution + rowMeans((theta * t(Xtrain[, fit$pairs$source]))^2)
    dynamic_amplitude <- dynamic_amplitude + rowMeans((Ls %*% t(beta))^2)
  }
  z <- .gls_study_marginal_pair_summary(fit, cfg)
  z$contribution_rms <- sqrt(contribution / length(draws))
  z$dynamic_amplitude <- sqrt(dynamic_amplitude / length(draws))
  z$posterior_static_abs <- colMeans(abs(fit$theta_bar))
  z$unregularized_amplitude <- abs(fit$initial_theta_bar)
  z$DSS_d <- if (is.null(dss)) NA_real_ else dss$d
  z$DSS_selected <- if (is.null(dss)) NA else dss$selected
  if (!is.null(truth)) {
    z$true_amplitude <- truth$true_amplitude
    z$is_active <- truth$true_amplitude > 0
  }
  merge(fit$pairs, z, by = "pair_id", sort = FALSE)
}

.gls_study_dss_amplitude_association <- function(selection, signal) {
  keep <- is.finite(selection$posterior_amplitude) & !is.na(selection$DSS_selected)
  z <- selection[keep, , drop = FALSE]; selected <- z$DSS_selected
  n1 <- sum(selected); n0 <- sum(!selected)
  auc <- if (n1 > 0 && n0 > 0) {
    ranks <- rank(z$posterior_amplitude)
    (sum(ranks[selected]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  } else NA_real_
  data.frame(signal = signal, selected = n1,
    median_amplitude_selected = if (n1) median(z$posterior_amplitude[selected]) else NA,
    median_amplitude_not_selected = if (n0) median(z$posterior_amplitude[!selected]) else NA,
    spearman_amplitude_vs_d = suppressWarnings(cor(z$posterior_amplitude,
      z$DSS_d, method = "spearman", use = "complete.obs")),
    amplitude_rank_AUC = auc)
}
