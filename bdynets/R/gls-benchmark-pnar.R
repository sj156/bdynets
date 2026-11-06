.gls_study_fit_pnar <- function(Y, X, Q, train, W, order = 1L) {
  k <- ncol(Y); fits <- vector("list", k)
  X2 <- rbind(X[1L, ], X[-nrow(X), , drop = FALSE])
  for (a in seq_len(k)) {
    Z <- cbind(1, Q, self1 = X[, a], W1 = as.numeric(X %*% W[a, ]))
    if (order == 2L) Z <- cbind(Z, self2 = X2[, a], W2 = as.numeric(X2 %*% W[a, ]))
    g <- suppressWarnings(glm.fit(Z[train, , drop = FALSE], Y[train, a], family = poisson()))
    co <- g$coefficients; co[!is.finite(co)] <- 0
    fits[[a]] <- list(coef = co, design = Z)
  }
  list(fits = fits, order = order)
}

.gls_study_predict_pnar <- function(fit, rows, cfg) {
  k <- length(fit$fits); mu <- matrix(0, length(rows), k)
  for (a in seq_len(k)) {
    z <- fit$fits[[a]]
    mu[, a] <- exp(clamp(z$design[rows, , drop = FALSE] %*% z$coef,
                         cfg$intensity_lower, cfg$intensity_upper))
  }
  list(mean = mu)
}

# Write a method-neutral snapshot so every benchmark can be run later, on a
# different computer, without reconstructing or changing the simulated data.
.gls_study_export_benchmark_data <- function(results, graph, cfg, output_dir) {
  data_dir <- file.path(output_dir, "benchmark_data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  edge_names <- graph$edges$name
  utils::write.csv(graph$edges[, c("id", "name", "from", "to")],
                   file.path(data_dir, "directed_edges.csv"),
                   row.names = FALSE)
  A <- as.data.frame(graph$A); W <- as.data.frame(graph$W)
  names(A) <- names(W) <- edge_names
  utils::write.csv(A, file.path(data_dir, "physical_A.csv"), row.names = FALSE)
  utils::write.csv(W, file.path(data_dir, "physical_W.csv"), row.names = FALSE)

  split <- results[[1L]]$split
  split_table <- data.frame(
    block = c("training", "validation", "test"),
    start = c(min(split$train), min(split$validation), min(split$test)),
    end = c(max(split$train), max(split$validation), max(split$test)))
  utils::write.csv(split_table, file.path(data_dir, "chronological_split.csv"),
                   row.names = FALSE)

  for (z in results) {
    tag <- sprintf("%.2f", z$simulation$signal)
    counts <- data.frame(Time = seq_len(nrow(z$simulation$Y)), z$simulation$Y,
                         check.names = FALSE)
    intensity <- data.frame(Time = seq_len(nrow(z$simulation$lambda)),
                            z$simulation$lambda,
                            check.names = FALSE)
    names(counts)[-1L] <- names(intensity)[-1L] <- edge_names
    fourier <- data.frame(
      Time = seq_len(nrow(z$covariates)), z$covariates,
      check.names = FALSE
    )
    utils::write.csv(
      counts, file.path(data_dir, paste0("counts_signal_", tag, ".csv")),
      row.names = FALSE
    )
    utils::write.csv(
      intensity,
      file.path(data_dir, paste0("intensity_signal_", tag, ".csv")),
      row.names = FALSE
    )
    utils::write.csv(
      fourier, file.path(data_dir, paste0("fourier_signal_", tag, ".csv")),
      row.names = FALSE
    )
  }
  invisible(data_dir)
}
