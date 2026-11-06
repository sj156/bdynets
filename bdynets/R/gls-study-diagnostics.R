.gls_study_split_rhat <- function(draw_matrix) {
  draw_matrix <- as.matrix(draw_matrix); n <- nrow(draw_matrix); m <- ncol(draw_matrix)
  if (m < 2L || n < 4L) return(NA_real_)
  half <- floor(n / 2L)
  z <- cbind(draw_matrix[seq_len(half), , drop = FALSE],
             draw_matrix[(n - half + 1L):n, , drop = FALSE])
  W <- mean(apply(z, 2L, var)); B <- half * var(colMeans(z))
  # A stochastic parameter that never moves is not converged. Returning one
  # here previously made a completely frozen block look ideal.
  if (!is.finite(W) || W <= .Machine$double.eps) return(Inf)
  sqrt(((half - 1) / half * W + B / half) / W)
}

.gls_study_acf_ess <- function(draw_matrix, max_lag = 500L) {
  draw_matrix <- as.matrix(draw_matrix); n <- nrow(draw_matrix); m <- ncol(draw_matrix)
  if (n < 4L) return(NA_real_)
  if (all(apply(draw_matrix, 2L, sd) <= .Machine$double.eps)) return(0)
  rho <- sapply(seq_len(m), function(j) {
    z <- draw_matrix[, j]
    if (sd(z) <= .Machine$double.eps) return(c(1, rep(0, min(max_lag, n - 1L))))
    as.numeric(acf(z, lag.max = min(max_lag, n - 1L), plot = FALSE)$acf)
  })
  if (is.vector(rho)) rho <- matrix(rho, ncol = 1L)
  mean_rho <- rowMeans(rho); keep <- 0L
  for (j in seq(2L, length(mean_rho) - 1L, by = 2L)) {
    if (mean_rho[j] + mean_rho[j + 1L] < 0) break
    keep <- j + 1L
  }
  tau_int <- 1 + if (keep >= 2L) 2 * sum(mean_rho[2L:keep]) else 0
  min(n * m, max(1, n * m / max(tau_int, 1)))
}

.gls_study_total_theta_at <- function(fit, time_id) {
  TT <- dim(fit$beta_path)[2L] - 1L
  if (length(time_id) != 1L || !is.finite(time_id) ||
      time_id < 1L || time_id > TT) {
    stop("time_id must select one fitted training time.", call. = FALSE)
  }
  out <- fit$theta_bar
  for (j in seq_len(dim(fit$L)[3L])) {
    out <- out + fit$L[, , j] * fit$beta_path[, time_id + 1L, j]
  }
  out
}

.gls_study_theta_monitor_times <- function(fit) {
  TT <- dim(fit$beta_path)[2L] - 1L
  unique(pmax(1L, pmin(TT, as.integer(round(TT * c(0.25, 0.50, 1.00))))))
}

.gls_study_theta_trace_data <- function(result, max_pairs = 4L) {
  fit <- result$fit
  posterior_amplitude <- colMeans(fit$amplitude)
  pair_class <- fit$pairs$pair_class
  truth <- result$simulation$true_amplitude
  classes <- c("self", "one-hop", "two-hop", "far/unreachable")
  selected <- integer()
  for (class_name in classes) {
    candidates <- which(pair_class == class_name)
    active <- candidates[truth[candidates] > 0]
    pool <- if (length(active)) active else candidates
    if (length(pool)) {
      score <- if (length(active)) truth[pool] else posterior_amplitude[pool]
      selected <- c(selected, pool[which.max(score)])
    }
  }
  if (length(selected) < max_pairs) {
    remaining <- setdiff(order(posterior_amplitude, decreasing = TRUE), selected)
    selected <- c(selected, head(remaining, max_pairs - length(selected)))
  }
  selected <- head(unique(selected), max_pairs)
  times <- .gls_study_theta_monitor_times(fit)
  draw_within_chain <- ave(seq_along(fit$chain_id), fit$chain_id,
                           FUN = seq_along)
  parts <- vector("list", length(selected) * length(times)); cursor <- 0L
  for (p in selected) for (t in times) {
    cursor <- cursor + 1L
    theta <- .gls_study_total_theta_at(fit, t)[, p]
    parts[[cursor]] <- data.frame(
      signal = fit$signal, pair_id = fit$pairs$pair_id[p],
      pair = fit$pairs$pair[p], pair_class = pair_class[p], time = t,
      parameter = sprintf("%s | t=%d", fit$pairs$pair[p], t),
      chain = fit$chain_id, draw = draw_within_chain, theta = theta
    )
  }
  out <- do.call(rbind, parts)
  out$parameter <- factor(out$parameter, levels = unique(out$parameter))
  out
}

.gls_study_convergence <- function(fit, signal, cfg) {
  groups <- split(seq_along(fit$chain_id), fit$chain_id)
  add <- function(values, type, name) {
    n <- min(vapply(groups, length, integer(1)))
    mat <- do.call(cbind, lapply(groups, function(ii) values[ii[seq_len(n)]]))
    data.frame(signal = signal, ParameterType = type, Parameter = name,
      SplitRhat = .gls_study_split_rhat(mat), ESS = .gls_study_acf_ess(mat))
  }
  fixed_tau <- cfg$shrinkage_spec %in%
    c("graph_laplace_fixed_tau", "graph_laplace_tau1")
  fixed_tau_value <- if (identical(cfg$shrinkage_spec, "graph_laplace_tau1")) {
    1
  } else {
    cfg$fixed_tau
  }
  tau_row <- if (fixed_tau) data.frame(
    signal = signal, ParameterType = "tau_fixed",
    Parameter = sprintf("tau=%g (fixed)", fixed_tau_value),
    SplitRhat = NA_real_, ESS = NA_real_
  ) else add(fit$tau, "tau", "tau")
  rows <- list(tau_row,
               add(fit$network_rms, "network_RMS", "network_RMS"))
  for (j in seq_len(ncol(fit$alpha)))
    rows[[length(rows) + 1L]] <- add(fit$alpha[, j], "alpha", paste0("alpha[", j, "]"))
  if (length(dim(fit$gamma)) == 3L)
    for (a in seq_len(dim(fit$gamma)[2L])) for (j in seq_len(dim(fit$gamma)[3L]))
      rows[[length(rows) + 1L]] <- add(fit$gamma[, a, j], "gamma",
        paste0("gamma[", a, ",", j, "]"))
  # L and beta coordinates are not separately identified under factor
  # rotations/sign changes. Monitor only their invariant total coefficient.
  for (t in .gls_study_theta_monitor_times(fit)) {
    theta_t <- .gls_study_total_theta_at(fit, t)
    for (p in seq_len(ncol(theta_t)))
      rows[[length(rows) + 1L]] <- add(
        theta_t[, p], "theta_total",
        paste0("theta[", fit$pairs$pair[p], ",t=", t, "]")
      )
  }
  for (j in seq_len(ncol(fit$chi)))
    rows[[length(rows) + 1L]] <- add(fit$chi[, j], "chi", paste0("chi[", j, "]"))
  for (j in seq_len(ncol(fit$amplitude)))
    rows[[length(rows) + 1L]] <- add(fit$amplitude[, j], "trajectory_amplitude",
                                      paste0("A[", fit$pairs$pair[j], "]"))
  out <- do.call(rbind, rows)
  out$PassRhat <- is.na(out$SplitRhat) | (is.finite(out$SplitRhat) &
    out$SplitRhat <= cfg$mcmc_rhat_threshold)
  out$PassESS <- is.na(out$ESS) | (is.finite(out$ESS) &
    out$ESS >= cfg$mcmc_ess_threshold)
  out
}

.gls_study_prediction_metrics <- function(label, prediction, Y, truth, rows, signal) {
  mu <- prediction$mean; obs <- Y[rows, , drop = FALSE]
  true <- truth[rows, , drop = FALSE]
  data.frame(signal = signal, Model = label,
    RMSEIntensity = sqrt(mean((mu - true)^2)),
    PoissonDeviance = mean(poisson_deviance(obs, mu)),
    MAECount = mean(abs(obs - mu)))
}

.gls_study_save_figures <- function(result, output_dir, cfg) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(invisible(NULL))
  theta <- NULL
  gg <- asNamespace("ggplot2"); dir.create(file.path(output_dir, "figures"),
                                            recursive = TRUE, showWarnings = FALSE)
  selection <- result$selection
  p0 <- gg$ggplot(selection, gg$aes(x = pmax(unregularized_amplitude, 1e-8),
    y = pmax(posterior_amplitude, 1e-8), color = DSS_selected,
    shape = is_active)) + gg$geom_abline(slope = 1, intercept = 0, linetype = 2) +
    gg$geom_point(alpha = 0.75, size = 2) + gg$scale_x_log10() + gg$scale_y_log10() +
    gg$labs(x = "Unregularized pair amplitude",
      y = "Posterior total-trajectory amplitude", color = "DSS", shape = "Truth",
      title = "Joint shrinkage of persistent and dynamic propagation") + gg$theme_bw()
  gg$ggsave(file.path(output_dir, "figures", "theta_shrinkage.pdf"),
            p0, width = 7.8, height = 5.2)

  p1 <- gg$ggplot(selection, gg$aes(x = posterior_amplitude,
    y = contribution_rms, color = DSS_selected, shape = is_active)) +
    gg$geom_point(alpha = 0.75, size = 2) + gg$scale_x_log10() + gg$scale_y_log10() +
    gg$labs(x = "Posterior total-trajectory amplitude",
      y = "Posterior contribution RMS", color = "DSS", shape = "Truth",
      title = "Total coefficient magnitude and joint DSS selection") + gg$theme_bw()
  gg$ggsave(file.path(output_dir, "figures", "theta_DSS_relation.pdf"),
            p1, width = 7.8, height = 5.2)

  dp <- as.data.frame(result$fit$discount_probability)
  names(dp) <- paste0("d", cfg$discount_grid)
  dp$draw <- seq_len(nrow(dp)); dp$chain <- result$fit$chain_id
  discount_columns <- setdiff(grep("^d", names(dp), value = TRUE), "draw")
  long <- reshape(dp, varying = discount_columns,
    v.names = "probability", timevar = "discount", times = cfg$discount_grid,
    direction = "long")
  p2 <- gg$ggplot(long, gg$aes(factor(discount), probability, fill = factor(discount))) +
    gg$stat_summary(fun = mean, geom = "col") +
    gg$labs(x = expression(delta[beta]), y = "Mean conditional grid probability",
      title = "Learned covariance discount", fill = NULL) + gg$theme_bw() +
    gg$theme(legend.position = "none")
  gg$ggsave(file.path(output_dir, "figures", "discount_posterior.pdf"),
            p2, width = 6.5, height = 4.5)

  tau <- data.frame(draw = ave(result$fit$tau, result$fit$chain_id, FUN = seq_along),
                    chain = factor(result$fit$chain_id), tau = result$fit$tau)
  p3 <- gg$ggplot(tau, gg$aes(draw, tau, color = chain)) + gg$geom_line(alpha = 0.8) +
    gg$scale_y_log10() + gg$labs(x = "Retained iteration", y = expression(tau),
      title = "Global-scale convergence") + gg$theme_bw()
  gg$ggsave(file.path(output_dir, "figures", "tau_trace.pdf"),
            p3, width = 7.8, height = 4.5)

  add_trace <- function(value, parameter) data.frame(
    draw = ave(seq_along(value), result$fit$chain_id, FUN = seq_along),
    chain = factor(result$fit$chain_id), parameter = parameter, value = value)
  trace_parts <- list(
    add_trace(result$fit$tau, "tau"),
    add_trace(result$fit$network_rms, "network RMS"),
    add_trace(rowMeans(result$fit$amplitude), "mean trajectory amplitude"))
  if (ncol(result$fit$chi)) for (j in seq_len(ncol(result$fit$chi)))
    trace_parts[[length(trace_parts) + 1L]] <-
      add_trace(result$fit$chi[, j], paste0("chi[", j, "]"))
  trace <- do.call(rbind, trace_parts)
  p4 <- gg$ggplot(trace, gg$aes(draw, value, color = chain)) +
    gg$geom_line(alpha = 0.72, linewidth = 0.28) +
    gg$facet_wrap(~parameter, ncol = 2, scales = "free_y") +
    gg$labs(x = "Retained iteration", y = NULL, color = "Chain",
      title = "Selected-parameter MCMC traces") + gg$theme_bw() +
    gg$theme(panel.grid.minor = gg$element_blank(), legend.position = "top")
  gg$ggsave(file.path(output_dir, "figures", "parameter_trace.pdf"),
            p4, width = 10.5, height = 8)
  gg$ggsave(file.path(output_dir, "figures", "parameter_trace.png"),
            p4, width = 10.5, height = 8, dpi = 220)

  theta_trace <- .gls_study_theta_trace_data(result)
  theta_table_dir <- file.path(output_dir, "tables")
  dir.create(theta_table_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(theta_trace,
    file.path(theta_table_dir, "theta_total_chain.csv"), row.names = FALSE)
  p_theta <- gg$ggplot(theta_trace,
    gg$aes(draw, theta, color = factor(chain))) +
    gg$geom_line(alpha = 0.72, linewidth = 0.28) +
    gg$facet_wrap(~parameter, ncol = 3, scales = "free_y") +
    gg$labs(x = "Retained MCMC iteration",
      y = expression(theta[ab * "," * t]), color = "Chain",
      title = "MCMC traces of identifiable total network coefficients") +
    gg$theme_bw() +
    gg$theme(panel.grid.minor = gg$element_blank(), legend.position = "top",
      strip.text = gg$element_text(size = 8))
  gg$ggsave(file.path(output_dir, "figures", "theta_total_trace.pdf"),
            p_theta, width = 11.5, height = 8.2)
  gg$ggsave(file.path(output_dir, "figures", "theta_total_trace.png"),
            p_theta, width = 11.5, height = 8.2, dpi = 220)

  recovery <- do.call(rbind, lapply(split(selection, selection$pair_class),
    function(z) data.frame(pair_class = unique(z$pair_class),
      Truth = sum(z$is_active), DSS = sum(z$DSS_selected))))
  recovery <- reshape(recovery, varying = c("Truth", "DSS"),
    v.names = "pairs", timevar = "type", times = c("Truth", "DSS"),
    direction = "long")
  recovery$type <- factor(recovery$type, levels = c("Truth", "DSS"))
  p5 <- gg$ggplot(recovery, gg$aes(pair_class, pairs, fill = type)) +
    gg$geom_col(position = "dodge", width = 0.72) +
    gg$scale_fill_manual(values = c(Truth = "#D55E00", DSS = "#0072B2")) +
    gg$labs(x = NULL, y = "Number of selected pairs", fill = NULL,
      title = "DSS support size by propagation class") + gg$theme_bw() +
    gg$theme(panel.grid.minor = gg$element_blank(), legend.position = "top",
      axis.text.x = gg$element_text(angle = 18, hjust = 1))
  gg$ggsave(file.path(output_dir, "figures", "DSS_support_by_class.pdf"),
            p5, width = 7.8, height = 4.8)
  gg$ggsave(file.path(output_dir, "figures", "DSS_support_by_class.png"),
            p5, width = 7.8, height = 4.8, dpi = 220)
  invisible(NULL)
}
