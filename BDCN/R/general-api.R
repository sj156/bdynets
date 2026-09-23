#' Build the all-pairs BDCN candidate dictionary
#'
#' Unlike [bdcn_pairs()], this dictionary includes self, one-hop, two-hop, and
#' unreachable ordered pairs. It is the candidate domain used by the packaged
#' Divvy bike example and by [bdcn_fit_general()].
#'
#' @param graph A BDCN graph containing `edges`, `A`, and `W`.
#' @param control A [bdcn_control()] object.
#' @return A data frame with one row per ordered source-target pair.
#' @export
bdcn_all_pairs <- function(graph, control = bdcn_control()) {
  graph <- .validate_bdcn_graph(graph)
  control <- .validate_bdcn_control(control)
  make_all_pairs(graph, control)
}

#' Fit all-pairs BDCN with an arbitrary known covariate matrix
#'
#' This is the general BDCN engine used by the Divvy bike example. Each target
#' has its own coefficients for the columns of `covariates`. All ordered lagged
#' source-target pairs enter the graph-informed shrinkage model; there are no
#' separate unpenalized self or physical-network lag terms.
#'
#' @param Y Time-by-series nonnegative integer count matrix.
#' @param graph A BDCN graph containing `edges`, `A`, and `W`.
#' @param covariates Known time-by-covariate design matrix for every row of `Y`.
#' @param train_rows Consecutive rows used for posterior fitting.
#' @param pre_history Optional count history preceding `Y`.
#' @param pairs Optional all-pairs dictionary from [bdcn_all_pairs()].
#' @param means Optional positive prior-centering means, one per series.
#' @param signal Numeric label used only to derive deterministic random seeds.
#' @param label Human-readable fit label.
#' @param control A [bdcn_control()] object.
#' @return A `bdcn_general_fit` object with posterior draws and frozen inputs.
#' @export
bdcn_fit_general <- function(
    Y, graph, covariates, train_rows = seq_len(nrow(Y)), pre_history = NULL,
    pairs = NULL, means = NULL, signal = 0,
    label = "BDCN continuous, all ordered pairs", control = bdcn_control()) {
  Y <- .validate_count_matrix(Y)
  graph <- .validate_bdcn_graph(graph)
  control <- .validate_bdcn_control(control)
  if (ncol(Y) != nrow(graph$edges)) {
    stop("The columns of Y must match graph$edges.", call. = FALSE)
  }
  covariates <- as.matrix(covariates)
  storage.mode(covariates) <- "double"
  if (nrow(covariates) != nrow(Y) || any(!is.finite(covariates))) {
    stop("covariates must be a finite matrix with nrow(Y) rows.", call. = FALSE)
  }
  train_rows <- sort(unique(as.integer(train_rows)))
  if (length(train_rows) < 3L || anyNA(train_rows) || min(train_rows) < 1L ||
      max(train_rows) > nrow(Y) || any(diff(train_rows) != 1L)) {
    stop("train_rows must select at least three consecutive valid rows of Y.",
         call. = FALSE)
  }
  if (is.null(pre_history)) {
    pre_history <- matrix(rep(Y[1L, ], times = 2L), nrow = 2L, byrow = TRUE)
  } else {
    pre_history <- as.matrix(pre_history)
    if (ncol(pre_history) != ncol(Y) || nrow(pre_history) < 1L ||
        any(!is.finite(pre_history)) || any(pre_history < 0)) {
      stop("pre_history must be a nonnegative matrix matching the columns of Y.",
           call. = FALSE)
    }
  }
  if (is.null(pairs)) pairs <- make_all_pairs(graph, control)
  if (!is.data.frame(pairs) ||
      !all(c("target", "source", "affinity") %in% names(pairs)) ||
      !nrow(pairs)) {
    stop("pairs must be a nonempty all-pairs BDCN dictionary.", call. = FALSE)
  }
  raw_lags <- make_lags(Y, pre_history, rep(0, ncol(Y)), rep(1, ncol(Y)), 1L)
  standardized <- standardize_lags_from_train(raw_lags, train_rows)
  X <- standardized$lags[, , 1L]
  if (is.null(means)) means <- colMeans(Y[train_rows, , drop = FALSE])
  if (length(means) != ncol(Y) || any(!is.finite(means)) || any(means <= 0)) {
    stop("means must contain one positive finite value per series.", call. = FALSE)
  }
  fit <- fit_general_bdcn(
    Y[train_rows, , drop = FALSE], X[train_rows, , drop = FALSE],
    covariates[train_rows, , drop = FALSE], pairs, means, signal, control
  )
  fit$label <- label
  fit$Y <- Y
  fit$X <- X
  fit$covariates <- covariates
  fit$train_rows <- train_rows
  fit$pre_history <- pre_history
  fit$scaler <- list(center = standardized$center, scale = standardized$scale)
  fit$graph <- graph
  fit$control <- control
  fit$signal <- signal
  class(fit) <- c("bdcn_general_fit", "list")
  fit
}

#' Sequential prediction from a general BDCN fit
#'
#' @param object A [bdcn_fit_general()] result.
#' @param rows Rows to predict and score.
#' @param conditioning_rows Earlier rows whose observations are assimilated
#'   before the requested prediction rows.
#' @param d Optional DSS multipliers.
#' @param label Prediction label.
#' @param control Optional replacement control object.
#' @param ... Reserved for the generic.
#' @return A `bdcn_prediction` containing intensity and count draws.
#' @export
predict.bdcn_general_fit <- function(
    object, rows, conditioning_rows = integer(), d = NULL,
    label = object$label, control = object$control, ...) {
  control <- .validate_bdcn_control(control)
  rows <- as.integer(rows)
  conditioning_rows <- as.integer(conditioning_rows)
  all_rows <- c(conditioning_rows, rows)
  if (!length(rows) || anyNA(all_rows) || any(all_rows < 1L) ||
      any(all_rows > nrow(object$Y)) || is.unsorted(all_rows, strictly = TRUE)) {
    stop("conditioning_rows followed by rows must be strictly increasing valid indices.",
         call. = FALSE)
  }
  if (!is.null(d) && (length(d) != nrow(object$pairs) || any(!is.finite(d)))) {
    stop("d must be NULL or one finite DSS multiplier per candidate pair.",
         call. = FALSE)
  }
  ans <- predict_general_bdcn(
    object, object$X, object$covariates, object$Y, rows,
    conditioning_rows, d, label, object$signal, control
  )
  ans$rows <- rows
  ans$conditioning_rows <- conditioning_rows
  class(ans) <- c("bdcn_prediction", "list")
  ans
}

#' Load the packaged Divvy bike example
#'
#' The bundled data are the frozen January--June 2026, 24-edge, three-hour
#' aggregation used by the existing real-data analysis. The first 12 bins are
#' context, January--April are training, May is validation, and June is test.
#'
#' @param data_dir Optional directory containing `counts.csv`, `A.csv`,
#'   `W.csv`, `edges.csv`, `calendar.csv`, and `split.csv`. By default, the
#'   copies bundled with BDCN are used.
#' @return A `bdcn_bike_data` list containing counts, timestamps, graph,
#'   calendar covariates, and split indices.
#' @export
bdcn_bike_data <- function(data_dir = NULL) {
  if (is.null(data_dir)) {
    data_dir <- system.file("extdata", "divvy", package = "BDCN")
  }
  if (!nzchar(data_dir) || !dir.exists(data_dir)) {
    stop("Cannot locate the Divvy example data directory.", call. = FALSE)
  }
  required <- c("counts.csv", "A.csv", "W.csv", "edges.csv",
                "calendar.csv", "split.csv")
  missing <- required[!file.exists(file.path(data_dir, required))]
  if (length(missing)) {
    stop("Missing bike data file(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  counts <- utils::read.csv(file.path(data_dir, "counts.csv"),
                            check.names = FALSE)
  timestamps <- as.POSIXct(counts[[1L]], tz = "UTC")
  Y <- as.matrix(counts[-1L])
  storage.mode(Y) <- "double"
  edges <- utils::read.csv(file.path(data_dir, "edges.csv"),
                           check.names = FALSE)
  A <- as.matrix(utils::read.csv(file.path(data_dir, "A.csv"), header = FALSE))
  W <- as.matrix(utils::read.csv(file.path(data_dir, "W.csv"), header = FALSE))
  graph <- .validate_bdcn_graph(list(
    A = A, W = W, edges = data.frame(name = edges$Edge),
    edge_metadata = edges
  ))
  class(graph) <- c("bdcn_graph", "list")
  Q <- as.matrix(utils::read.csv(file.path(data_dir, "calendar.csv"),
                                 check.names = FALSE))
  storage.mode(Q) <- "double"
  split_table <- utils::read.csv(file.path(data_dir, "split.csv"),
                                 stringsAsFactors = FALSE)
  split <- setNames(lapply(seq_len(nrow(split_table)), function(i) {
    seq.int(split_table$start[i], split_table$end[i])
  }), split_table$block)
  if (nrow(Y) != nrow(Q) || ncol(Y) != nrow(edges) || anyNA(timestamps)) {
    stop("The bundled bike data files have inconsistent dimensions.", call. = FALSE)
  }
  structure(list(
    Y = Y, timestamps = timestamps, graph = graph, covariates = Q,
    split = split, split_table = split_table, data_dir = normalizePath(data_dir)
  ), class = c("bdcn_bike_data", "list"))
}

#' Control settings for the Divvy bike example
#'
#' @param profile `"smoke"` for a workflow check, `"analysis"` for the original
#'   1,500-iteration analysis, or `"long"` for the 5,000-iteration sensitivity
#'   run.
#' @param ... Named replacements passed to [bdcn_control()].
#' @return A validated BDCN control object.
#' @export
bdcn_bike_control <- function(profile = c("smoke", "analysis", "long"), ...) {
  profile <- match.arg(profile)
  base_profile <- if (profile == "smoke") "smoke" else "default"
  settings <- switch(profile,
    smoke = list(
      seed = 20260922L, n_iter = 6L, burn_mcmc = 2L, n_chains = 1L,
      parallel_chains = FALSE, progress_every = 100L,
      predictive_draws = 2L, dss_validation_draws = 2L,
      dss_max_draws = 2L, dss_n_lambda = 3L,
      shrinkage_substeps = 1L, state_block_substeps = 1L,
      scale_move_steps = 1L, expected_active_pairs = 48L
    ),
    analysis = list(
      seed = 20260922L, n_iter = 1500L, burn_mcmc = 750L,
      n_chains = 3L, parallel_chains = TRUE, progress_every = 100L,
      predictive_draws = 300L, dss_validation_draws = 100L,
      dss_max_draws = 200L, dss_n_lambda = 40L,
      expected_active_pairs = 48L
    ),
    long = list(
      seed = 20260922L, n_iter = 5000L, burn_mcmc = 2500L,
      n_chains = 3L, parallel_chains = TRUE, progress_every = 100L,
      predictive_draws = 300L, dss_validation_draws = 100L,
      dss_max_draws = 200L, dss_n_lambda = 40L,
      expected_active_pairs = 48L
    )
  )
  replacements <- list(...)
  if (length(replacements) &&
      (is.null(names(replacements)) || any(!nzchar(names(replacements))))) {
    stop("All replacement bike settings must be named.", call. = FALSE)
  }
  settings[names(replacements)] <- replacements
  do.call(bdcn_control, c(list(profile = base_profile), settings))
}

.bike_prediction_frame <- function(prediction, data) {
  mu <- apply(prediction$lambda, c(1L, 2L), mean)
  lo <- apply(prediction$y, c(1L, 2L), stats::quantile, 0.025)
  hi <- apply(prediction$y, c(1L, 2L), stats::quantile, 0.975)
  data.frame(
    Model = prediction$label,
    Time = rep(prediction$rows, ncol(data$Y)),
    Timestamp = rep(data$timestamps[prediction$rows], ncol(data$Y)),
    Edge = rep(colnames(data$Y), each = length(prediction$rows)),
    Observed = as.vector(data$Y[prediction$rows, , drop = FALSE]),
    Prediction = as.vector(mu), Lower95 = as.vector(lo), Upper95 = as.vector(hi)
  )
}

#' Run the packaged Divvy bike example
#'
#' This function fits the general all-pairs BDCN, applies validation-selected
#' joint DSS, and produces rolling one-step June predictions. Set `output_dir`
#' to save reusable fit and result files. The smoke profile validates execution
#' only; use the analysis or long profile for substantive runs and inspect
#' convergence before interpretation.
#'
#' @param profile Bike profile passed to [bdcn_bike_control()].
#' @param data_dir Optional external prepared-data directory; defaults to the
#'   bundled example data.
#' @param output_dir Optional result directory. Nothing is written when `NULL`.
#' @param control Optional explicit bike control object.
#' @param reuse Reuse a matching package-generated fit checkpoint when present.
#' @return A `bdcn_bike_result` with data, fit, diagnostics, DSS, predictions,
#'   observed-data scores, selection table, and prediction table.
#' @export
#' @examples
#' bike <- bdcn_bike_data()
#' dim(bike$Y)
#' bike$split_table
#' if (interactive()) {
#'   result <- bdcn_run_bike_example("smoke")
#'   result$scores
#' }
bdcn_run_bike_example <- function(
    profile = c("smoke", "analysis", "long"), data_dir = NULL,
    output_dir = NULL, control = NULL, reuse = TRUE) {
  profile <- match.arg(profile)
  if (is.null(control)) control <- bdcn_bike_control(profile)
  control <- .validate_bdcn_control(control)
  data <- bdcn_bike_data(data_dir)
  pairs <- bdcn_all_pairs(data$graph, control)
  fit_file <- if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    file.path(output_dir, sprintf("bdcn_bike_fit_%d.rds", control$n_iter))
  } else NULL
  fit <- NULL
  if (isTRUE(reuse) && !is.null(fit_file) && file.exists(fit_file)) {
    candidate <- readRDS(fit_file)
    if (inherits(candidate, "bdcn_general_fit") &&
        identical(candidate$control, control) &&
        identical(candidate$Y, data$Y) &&
        identical(candidate$covariates, data$covariates) &&
        identical(candidate$graph$A, data$graph$A)) fit <- candidate
  }
  if (is.null(fit)) {
    fit <- bdcn_fit_general(
      data$Y, data$graph, data$covariates,
      train_rows = data$split$training, pairs = pairs,
      means = colMeans(data$Y[data$split$training, , drop = FALSE]),
      control = control
    )
    if (!is.null(fit_file)) saveRDS(fit, fit_file, compress = FALSE)
  }
  diagnostics <- bdcn_diagnostics(fit, control = control)
  dss <- bdcn_dss(fit, data$split$validation, control)
  continuous <- predict(
    fit, data$split$test, data$split$validation,
    label = "BDCN continuous", control = control
  )
  sparse <- predict(
    fit, data$split$test, data$split$validation, d = dss$d,
    label = "BDCN + DSS", control = control
  )
  predictions <- list(continuous = continuous, dss = sparse)
  scores <- do.call(rbind, lapply(predictions, function(pred) {
    z <- bdcn_score(pred, data$Y, data$Y, control = control)
    data.frame(
      Model = z$Model, PoissonDeviance = z$PoissonDeviance,
      ObservedRMSE = z$RMSEIntensity, MAECount = z$MAECount,
      LogScore = z$LogScore, Coverage95 = z$Coverage95, Width95 = z$Width95
    )
  }))
  selection <- pairs
  selection$posterior_amplitude <- colMeans(fit$amplitude)
  selection$probability_above_delta <- colMeans(
    fit$amplitude > control$delta_A
  )
  selection$DSS_d <- dss$d
  selection$DSS_selected <- dss$selected
  prediction_table <- do.call(rbind, lapply(predictions,
                                             .bike_prediction_frame,
                                             data = data))
  result <- structure(list(
    data = data, fit = fit, diagnostics = diagnostics, dss = dss,
    predictions = predictions, scores = scores, selection = selection,
    prediction_table = prediction_table, control = control
  ), class = c("bdcn_bike_result", "list"))
  if (!is.null(output_dir)) {
    utils::write.csv(diagnostics, file.path(output_dir, "mcmc_convergence.csv"),
                     row.names = FALSE)
    utils::write.csv(dss$path, file.path(output_dir, "dss_validation_path.csv"),
                     row.names = FALSE)
    utils::write.csv(selection, file.path(output_dir, "dependence_selection.csv"),
                     row.names = FALSE)
    utils::write.csv(prediction_table,
                     file.path(output_dir, "bike_predictions.csv"),
                     row.names = FALSE)
    utils::write.csv(scores, file.path(output_dir, "bike_scores.csv"),
                     row.names = FALSE)
    saveRDS(list(control = control, split = data$split,
                 scaler = fit$scaler),
            file.path(output_dir, "configuration.rds"))
    saveRDS(result, file.path(output_dir, "bike_results.rds"),
            compress = FALSE)
  }
  result
}

#' @export
print.bdcn_general_fit <- function(x, ...) {
  cat("General all-pairs Bayesian dynamic count network fit\n")
  cat("  series:", ncol(x$Y), "\n")
  cat("  covariates:", ncol(x$covariates), "\n")
  cat("  training times:", length(x$train_rows), "\n")
  cat("  candidate pairs:", nrow(x$pairs), "\n")
  cat("  posterior draws:", nrow(x$alpha), "across", length(x$chains), "chains\n")
  cat("  elapsed seconds:", format(x$elapsed, digits = 4L), "\n")
  invisible(x)
}

#' @export
summary.bdcn_general_fit <- function(object, ...) {
  diagnostics <- bdcn_diagnostics(object)
  structure(list(
    label = object$label, series = ncol(object$Y),
    covariates = ncol(object$covariates),
    training_times = length(object$train_rows),
    candidate_pairs = nrow(object$pairs), posterior_draws = nrow(object$alpha),
    elapsed = object$elapsed,
    posterior_inclusion_probability = colMeans(
      object$amplitude > object$control$delta_A
    ),
    all_converged = all(diagnostics$Converged), diagnostics = diagnostics
  ), class = c("summary.bdcn_general_fit", "list"))
}

#' @export
print.summary.bdcn_general_fit <- function(x, ...) {
  cat("General BDCN posterior summary\n")
  cat("  series:", x$series, "\n")
  cat("  covariates:", x$covariates, "\n")
  cat("  candidate pairs:", x$candidate_pairs, "\n")
  cat("  posterior draws:", x$posterior_draws, "\n")
  cat("  all monitored parameters converged:", x$all_converged, "\n")
  invisible(x)
}
