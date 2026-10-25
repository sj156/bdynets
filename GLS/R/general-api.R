#' Build the all-pairs GLS candidate dictionary
#'
#' Unlike [gls_pairs()], this dictionary includes self, one-hop, two-hop, and
#' unreachable ordered pairs. It is the candidate domain used by
#' [gls_fit_general()] and the directed-grid simulation study.
#'
#' @param graph A GLS graph containing `edges`, `A`, and `W`.
#' @param control A [gls_control()] object.
#' @return A data frame with one row per ordered source-target pair.
#' @export
gls_all_pairs <- function(graph, control = gls_control()) {
  graph <- .validate_gls_graph(graph)
  control <- .validate_gls_control(control)
  make_all_pairs(graph, control)
}

#' Fit all-pairs GLS with an arbitrary known covariate matrix
#'
#' Each target has its own coefficients for the columns of `covariates`. All
#' ordered lagged
#' source-target pairs enter the graph-informed shrinkage model; there are no
#' separate unpenalized self or physical-network lag terms.
#'
#' @param Y Time-by-series nonnegative integer count matrix.
#' @param graph A GLS graph containing `edges`, `A`, and `W`.
#' @param covariates Known time-by-covariate design matrix for every row of `Y`.
#' @param train_rows Consecutive rows used for posterior fitting.
#' @param pre_history Optional count history preceding `Y`.
#' @param pairs Optional all-pairs dictionary from [gls_all_pairs()].
#' @param means Optional positive prior-centering means, one per series.
#' @param signal Numeric label used only to derive deterministic random seeds.
#' @param label Human-readable fit label.
#' @param control A [gls_control()] object.
#' @return A `gls_general_fit` object with posterior draws and frozen inputs.
#' @export
gls_fit_general <- function(
    Y, graph, covariates, train_rows = seq_len(nrow(Y)), pre_history = NULL,
    pairs = NULL, means = NULL, signal = 0,
    label = "GLS, all ordered pairs", control = gls_control()) {
  Y <- .validate_count_matrix(Y)
  graph <- .validate_gls_graph(graph)
  control <- .validate_gls_control(control)
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
    stop("pairs must be a nonempty all-pairs GLS dictionary.", call. = FALSE)
  }
  raw_lags <- make_lags(Y, pre_history, rep(0, ncol(Y)), rep(1, ncol(Y)), 1L)
  standardized <- standardize_lags_from_train(raw_lags, train_rows)
  X <- standardized$lags[, , 1L]
  if (is.null(means)) means <- colMeans(Y[train_rows, , drop = FALSE])
  if (length(means) != ncol(Y) || any(!is.finite(means)) || any(means <= 0)) {
    stop("means must contain one positive finite value per series.", call. = FALSE)
  }
  fit <- fit_general_gls(
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
  class(fit) <- c("gls_general_fit", "list")
  fit
}

#' Sequential prediction from a general GLS fit
#'
#' @param object A [gls_fit_general()] result.
#' @param rows Rows to predict and score.
#' @param conditioning_rows Earlier rows whose observations are assimilated
#'   before the requested prediction rows.
#' @param d Optional DSS multipliers.
#' @param label Prediction label.
#' @param control Optional replacement control object.
#' @param ... Reserved for the generic.
#' @return A `gls_prediction` containing intensity and count draws.
#' @export
predict.gls_general_fit <- function(
    object, rows, conditioning_rows = integer(), d = NULL,
    label = object$label, control = object$control, ...) {
  control <- .validate_gls_control(control)
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
  ans <- predict_general_gls(
    object, object$X, object$covariates, object$Y, rows,
    conditioning_rows, d, label, object$signal, control
  )
  ans$rows <- rows
  ans$conditioning_rows <- conditioning_rows
  class(ans) <- c("gls_prediction", "list")
  ans
}

#' @export
print.gls_general_fit <- function(x, ...) {
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
summary.gls_general_fit <- function(object, ...) {
  diagnostics <- gls_diagnostics(object)
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
  ), class = c("summary.gls_general_fit", "list"))
}

#' @export
print.summary.gls_general_fit <- function(x, ...) {
  cat("General GLS posterior summary\n")
  cat("  series:", x$series, "\n")
  cat("  covariates:", x$covariates, "\n")
  cat("  candidate pairs:", x$candidate_pairs, "\n")
  cat("  posterior draws:", x$posterior_draws, "\n")
  cat("  all monitored parameters converged:", x$all_converged, "\n")
  invisible(x)
}
