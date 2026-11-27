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
#' @param means Optional positive prior-centering means, one per series. By
#'   default, training sample means with a `0.5 / length(train_rows)` floor
#'   for all-zero training series; observations are not changed.
#' @param signal Numeric label used only to derive deterministic random seeds.
#' @param label Human-readable fit label.
#' @param control A [gls_control()] object.
#' @param checkpoint_dir Optional persistent directory for this fit. When supplied,
#'   each chain commits its state during warmup and sampling. Draw chunks are
#'   saved under this directory unless a separate `draw_dir` is supplied.
#' @param checkpoint_every Positive number of MCMC iterations between commits.
#' @param resume Set to TRUE to continue this exact run, using the same inputs,
#'   controls, software environment, chunk size and checkpoint directory.
#'   The most recent committed iteration is restored separately for each chain.
#' @param max_workers Optional positive integer limiting concurrent chains.
#'   With parallel chains enabled, workers are capped by this value, the chain
#'   count and the detected physical core count. NULL uses the available cores.
#'   Setting the cap to one runs all chains sequentially.
#' @param draw_dir Optional parent directory for per-chain draw chunks. Draws
#'   stay in memory when both this argument and `checkpoint_dir` are NULL.
#' @inheritParams gls_fit
#' @return A `gls_general_fit` object with frozen inputs and posterior draws or
#'   a disk draw index. Use [gls_load_draws()] to access the full arrays.
#' @details Set `n_chains = 3L` and `parallel_chains = TRUE` in
#'   [gls_control()] to fit three chains concurrently when at least three
#'   physical cores are detected. Windows uses independent socket workers.
#'   Unix uses forked workers. Each chain records its worker process ID.
#'   With `affinity_model = "graph_laplace"`, the fit also retains the
#'   candidate-aligned `affinity_features` and posterior `chi` draws.
#'   Checkpointing uses an exclusive operating-system lock per run and per
#'   chain. Completed chains are reused. After interruption, chunks beyond
#'   the last commit are regenerated. Use a filesystem supporting exclusive
#'   locks and same-directory renames, such as a local SSD.
#' @export
gls_fit_general <- function(
    Y, graph, covariates, train_rows = seq_len(nrow(Y)), pre_history = NULL,
    pairs = NULL, means = NULL, signal = 0,
    label = "GLS, all ordered pairs", control = gls_control(),
    draw_dir = NULL, chunk_size = 100L, checkpoint_dir = NULL,
    checkpoint_every = 100L, resume = FALSE, max_workers = NULL) {
  Y <- .validate_count_matrix(Y)
  graph <- .validate_gls_graph(graph)
  control <- .validate_gls_control(control)
  if (!is.null(max_workers) && (!is.numeric(max_workers) || length(max_workers) != 1L ||
      !is.finite(max_workers) || max_workers < 1 || max_workers > .Machine$integer.max ||
      max_workers != floor(max_workers)))
    stop("max_workers must be NULL or a positive integer.", call. = FALSE)
  if (identical(control$scale_move_method, "slice"))
    stop("Use the archived pilot package to refit with the retired general GLS slice update.",
      call. = FALSE)
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
  affinity_features <- NULL
  if (control$affinity_model == "graph_laplace") {
    for (index in pairs[c("target", "source")]) {
      if (!is.numeric(index) || any(!is.finite(index) | index != floor(index) |
          index < 1L | index > ncol(Y)))
        stop("Graph-Laplace affinity needs valid target and source indices.", call. = FALSE)
    }
    affinity_features <- .gls_study_make_graph_laplace_feature(
      graph, pairs, control$graph_laplace_rho0)
    attr(affinity_features, "edge_adjacency") <- NULL
  }
  raw_lags <- make_lags(Y, pre_history, rep(0, ncol(Y)), rep(1, ncol(Y)), 1L)
  standardized <- standardize_lags_from_train(raw_lags, train_rows)
  X <- standardized$lags[, , 1L]
  if (is.null(means)) means <- pmax(colMeans(Y[train_rows, , drop = FALSE]),
                                   0.5 / length(train_rows))
  if (length(means) != ncol(Y) || any(!is.finite(means)) || any(means <= 0)) {
    stop("means must contain one positive finite value per series.", call. = FALSE)
  }
  if (!is.logical(resume) || length(resume) != 1L || is.na(resume))
    stop("resume must be TRUE or FALSE.", call. = FALSE)
  if (is.null(checkpoint_dir)) {
    if (resume) stop("resume requires checkpoint_dir.", call. = FALSE)
    draw_store <- .gls_prepare_draw_store(draw_dir, chunk_size)
  } else {
    checkpoint_dir <- .gls_checkpoint_directory(checkpoint_dir, checkpoint_every)
    lock <- .gls_checkpoint_lock(file.path(checkpoint_dir, "run.lock"))
    on.exit(filelock::unlock(lock), add = TRUE)
    draw_store <- .gls_checkpoint_store(checkpoint_dir, draw_dir, chunk_size,
      checkpoint_every, resume, list(Y = Y, graph = graph, covariates = covariates,
        train_rows = train_rows, pre_history = pre_history, pairs = pairs,
        means = means, signal = signal, control = control))
  }
  fit <- fit_general_gls(
    Y[train_rows, , drop = FALSE], X[train_rows, , drop = FALSE],
    covariates[train_rows, , drop = FALSE], pairs, means, signal, control,
    draw_store = draw_store, affinity_features = affinity_features, max_workers = max_workers
  )
  fit$affinity_features <- affinity_features
  fit$label <- label
  fit$Y <- Y
  fit$X <- X
  fit$covariates <- covariates
  fit$train_rows <- train_rows
  fit$means <- as.numeric(means)
  fit$pre_history <- pre_history
  fit$scaler <- list(center = standardized$center, scale = standardized$scale)
  fit$graph <- graph
  fit$control <- control
  fit$backend <- control$backend
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
  cat("  posterior draws:", .gls_retained_draw_count(x), "across", length(x$chains), "chains\n")
  cat("  elapsed seconds:", format(x$elapsed, digits = 4L), "\n")
  invisible(x)
}

.gls_general_diagnostic_view <- function(fit) {
  if (is.null(fit$draw_store)) return(fit)
  fields <- c("B", "discount", "tau", "network_rms", "alpha", "gamma", "amplitude", "theta_average")
  if (identical(fit$control$affinity_model, "graph_laplace")) fields <- c(fields, "chi")
  chains <- .gls_general_chain_ids(fit)
  view <- .gls_general_draw_fields(fit, seq_along(chains), fields)
  view$chain_id <- chains
  view$pairs <- fit$pairs
  view$control <- fit$control
  view
}

#' @export
summary.gls_general_fit <- function(object, ...) {
  view <- .gls_general_diagnostic_view(object)
  diagnostics <- basic_convergence(view, object$signal, object$control)
  diagnostics$Converged <- diagnostics$PassRhat & diagnostics$PassESS
  structure(list(
    label = object$label, series = ncol(object$Y),
    covariates = ncol(object$covariates),
    training_times = length(object$train_rows),
    candidate_pairs = nrow(object$pairs), posterior_draws = length(view$chain_id),
    elapsed = object$elapsed,
    posterior_inclusion_probability = colMeans(
      view$amplitude > object$control$delta_A
    ),
    all_converged = all(diagnostics$Converged[diagnostics$ParameterType != "tau_fixed"]),
    diagnostics = diagnostics
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
