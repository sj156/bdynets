#' Configure a BDCN analysis
#'
#' `bdcn_control()` collects simulation, sampler, shrinkage, DSS, and prediction
#' settings. The `smoke` and `quick` profiles are workflow checks; they are not
#' inferential recommendations.
#'
#' @param profile One of `"default"`, `"quick"`, or `"smoke"`.
#' @param ... Named settings replacing profile defaults.
#' @return A validated list of BDCN settings.
#' @export
#' @examples
#' ctrl <- bdcn_control("smoke", n_iter = 40, burn_mcmc = 20)
#' ctrl$n_iter
bdcn_control <- function(profile = c("default", "quick", "smoke"), ...) {
  profile <- match.arg(profile)
  cfg <- list(
    seed = 20260904L,
    grid_side = 3L,
    n_long_pairs = if (profile == "smoke") 2L else 8L,
    expected_active_pairs = 48L,
    long_heterogeneity = c(0.70, 0.85, 1.00, 1.15, 1.30),
    factor_dim = 1L,
    self_effect = 0.10,
    local_network_effect = 0.08,
    x_scale = 0.50,
    dgp_spectral_radius_limit = 0.90,
    target_mean_min = 12,
    target_mean_max = 20,
    time_trend_log_change = log(1.40),
    trend_prior_sd = 1,
    season_prior_sd = 1,
    seasonal_cycles = 6,
    seasonal_log_amplitude = log(1.80),
    seasonal_prior_sd = 1,
    simulation_burn = if (profile == "smoke") 100L else 500L,
    n_total = switch(profile, smoke = 160L, quick = 600L, default = 1800L),
    train_fraction = 2 / 3,
    validation_fraction = 1 / 6,
    calibration_iterations = if (profile == "smoke") 2L else 5L,
    calibration_length = switch(profile, smoke = 200L, quick = 600L,
                                default = 1200L),
    calibration_damping = 0.60,
    maximum_rate = 1e5,
    r_nb_epsilon = 0.05,
    r_nb_quantile = 0.99,
    r_nb_min = 25L,
    r_nb_max = 5000L,
    r_nb_round_to = 25L,
    n_iter = switch(profile, smoke = 80L, quick = 500L, default = 6000L),
    burn_mcmc = switch(profile, smoke = 40L, quick = 250L, default = 3000L),
    thin = 1L,
    n_chains = if (profile == "smoke") 2L else 3L,
    parallel_chains = profile != "smoke",
    mcmc_rhat_threshold = 1.01,
    mcmc_ess_threshold = 400,
    progress_every = switch(profile, smoke = 40L, quick = 175L, default = 300L),
    predictive_draws = switch(profile, smoke = 25L, quick = 150L, default = 800L),
    tau_prior_multiplier = 5,
    tau_prior_min_scale = 1e-6,
    tau_initial_floor = 1e-5,
    shrinkage_substeps = 5L,
    state_block_substeps = 2L,
    scale_move_steps = 4L,
    scale_move_initial_sd = 0.10,
    scale_move_target_acceptance = 0.35,
    scale_move_adapt_rate = 0.15,
    scale_move_min_sd = 5e-4,
    scale_move_max_sd = 0.30,
    alpha_prior_sd = 3,
    static_prior_sd = 1,
    affinity_decay = 0.85,
    affinity_floor = 0.04,
    delta_A = 0.01,
    pip_cutoff = 0.50,
    dss_weight = "intensity",
    dss_n_lambda = if (profile == "smoke") 12L else 100L,
    dss_lambda_min_ratio = 1e-4,
    dss_predictive_tolerance = 0.005,
    dss_validation_draws = switch(profile, smoke = 25L, quick = 75L,
                                  default = 150L),
    dss_common_kappa = TRUE,
    dss_include_zero_model = TRUE,
    dss_skip_duplicate_supports = TRUE,
    dss_early_stop = TRUE,
    dss_zero_tolerance = 1e-8,
    dss_max_draws = switch(profile, smoke = 25L, quick = 120L, default = 600L),
    interval_level = 0.95
  )
  replacements <- list(...)
  if (length(replacements) &&
      (is.null(names(replacements)) || any(!nzchar(names(replacements))))) {
    stop("All replacement control settings must be named.", call. = FALSE)
  }
  unknown <- setdiff(names(replacements), names(cfg))
  if (length(unknown)) {
    stop("Unknown control setting(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  cfg[names(replacements)] <- replacements
  .validate_bdcn_control(cfg)
}

.validate_bdcn_control <- function(cfg) {
  integer_fields <- c(
    "seed", "grid_side", "n_long_pairs", "expected_active_pairs", "factor_dim", "simulation_burn",
    "n_total", "calibration_iterations", "calibration_length", "r_nb_min",
    "r_nb_max", "r_nb_round_to", "n_iter", "burn_mcmc", "thin", "n_chains",
    "progress_every", "predictive_draws", "shrinkage_substeps",
    "state_block_substeps", "scale_move_steps", "dss_n_lambda",
    "dss_validation_draws", "dss_max_draws"
  )
  for (name in integer_fields) cfg[[name]] <- as.integer(cfg[[name]])
  if (is.na(cfg$seed)) stop("seed must be coercible to an integer.", call. = FALSE)
  if (cfg$grid_side < 2L) stop("grid_side must be at least 2.", call. = FALSE)
  if (cfg$factor_dim != 1L) {
    stop("This package release supports factor_dim = 1 only.", call. = FALSE)
  }
  if (cfg$n_total < 10L) stop("n_total must be at least 10.", call. = FALSE)
  if (cfg$n_iter < 4L || cfg$burn_mcmc < 0L || cfg$burn_mcmc >= cfg$n_iter) {
    stop("Require 0 <= burn_mcmc < n_iter and n_iter >= 4.", call. = FALSE)
  }
  if (cfg$thin < 1L || cfg$n_chains < 1L) {
    stop("thin and n_chains must be positive.", call. = FALSE)
  }
  if (floor((cfg$n_iter - cfg$burn_mcmc) / cfg$thin) < 2L) {
    stop("The control must retain at least two posterior draws per chain.",
         call. = FALSE)
  }
  positive <- c(
    "factor_dim", "n_long_pairs", "expected_active_pairs", "x_scale", "target_mean_min", "target_mean_max",
    "season_prior_sd",
    "seasonal_cycles", "calibration_iterations", "calibration_length",
    "r_nb_epsilon", "r_nb_min", "r_nb_max", "r_nb_round_to",
    "predictive_draws", "shrinkage_substeps", "state_block_substeps",
    "scale_move_steps", "dss_n_lambda", "dss_validation_draws",
    "dss_max_draws"
  )
  if (any(!vapply(cfg[positive], function(x) is.finite(x) && x > 0,
                  logical(1L)))) {
    stop("Positive control settings must be finite and greater than zero.",
         call. = FALSE)
  }
  if (cfg$target_mean_max <= cfg$target_mean_min) {
    stop("target_mean_max must exceed target_mean_min.", call. = FALSE)
  }
  if (cfg$r_nb_max < cfg$r_nb_min) {
    stop("r_nb_max must not be smaller than r_nb_min.", call. = FALSE)
  }
  if (cfg$train_fraction <= 0 || cfg$validation_fraction <= 0 ||
      cfg$train_fraction + cfg$validation_fraction >= 1) {
    stop("Training and validation fractions must be positive and sum to less than one.",
         call. = FALSE)
  }
  if (!cfg$dss_weight %in% c("intensity", "equal")) {
    stop("dss_weight must be 'intensity' or 'equal'.", call. = FALSE)
  }
  class(cfg) <- c("bdcn_control", "list")
  cfg
}

# Internal fallback for migrated engine functions. Public calls always pass an
# explicit validated control object.
CFG <- bdcn_control("default")

.validate_bdcn_graph <- function(graph) {
  if (!is.list(graph) || !all(c("edges", "A", "W") %in% names(graph))) {
    stop("graph must contain edges, A, and W.", call. = FALSE)
  }
  k <- nrow(graph$edges)
  if (!is.numeric(graph$A) || !identical(dim(graph$A), c(k, k)) ||
      !is.numeric(graph$W) || !identical(dim(graph$W), c(k, k))) {
    stop("graph$A and graph$W must be square matrices matching graph$edges.",
         call. = FALSE)
  }
  if (any(!is.finite(graph$A)) || any(!is.finite(graph$W))) {
    stop("Graph matrices must be finite.", call. = FALSE)
  }
  graph
}

#' Construct the directed road-segment grid used by BDCN examples
#'
#' @param side Number of nodes on each side of the square node grid.
#' @return A `bdcn_graph` with node and directed-edge tables, edge adjacency
#'   `A`, and row-normalized physical matrix `W`.
#' @export
bdcn_grid <- function(side = 3L) {
  side <- as.integer(side)
  if (length(side) != 1L || is.na(side) || side < 2L) {
    stop("side must be one integer of at least 2.", call. = FALSE)
  }
  graph <- make_directed_grid(side)
  class(graph) <- c("bdcn_graph", "list")
  graph
}

#' Build the BDCN nonlocal candidate dictionary
#'
#' Self and one-hop physical effects are handled by static terms. This function
#' returns the remaining ordered source-target pairs and their graph affinities.
#'
#' @param graph A graph returned by [bdcn_grid()] or an equivalent list.
#' @param control A [bdcn_control()] object.
#' @return A data frame with target, source, hop distance, affinity, and labels.
#' @export
bdcn_pairs <- function(graph, control = bdcn_control()) {
  graph <- .validate_bdcn_graph(graph)
  control <- .validate_bdcn_control(control)
  make_full_pairs(graph, control)
}

#' Simulate a sparse multi-hop propagation experiment
#'
#' @param signal RMS amplitude assigned to injected two-hop effects.
#' @param side Square-grid side length.
#' @param control A [bdcn_control()] object.
#' @return A list containing the graph, full candidate dictionary, injected
#'   long pairs, simulated counts and truth, and train/validation/test indices.
#' @export
bdcn_simulate_multihop <- function(signal = 0.20, side = 3L,
                                   control = bdcn_control()) {
  if (length(signal) != 1L || !is.finite(signal) || signal < 0) {
    stop("signal must be one finite nonnegative number.", call. = FALSE)
  }
  control <- .validate_bdcn_control(control)
  control$grid_side <- as.integer(side)
  graph <- bdcn_grid(side)
  pairs <- make_full_pairs(graph, control)
  long_pairs <- choose_long_pairs(graph, control)
  simulation <- simulate_grid(graph, long_pairs, signal, control)
  n_train <- floor(control$n_total * control$train_fraction)
  n_validation <- floor(control$n_total * control$validation_fraction)
  split <- list(
    train = seq_len(n_train),
    validation = n_train + seq_len(n_validation),
    test = seq.int(n_train + n_validation + 1L, control$n_total)
  )
  structure(
    list(graph = graph, pairs = pairs, long_pairs = long_pairs,
         simulation = simulation, split = split, signal = signal,
         control = control),
    class = c("bdcn_simulation", "list")
  )
}

.validate_count_matrix <- function(Y) {
  Y <- as.matrix(Y)
  storage.mode(Y) <- "double"
  if (nrow(Y) < 3L || ncol(Y) < 2L || any(!is.finite(Y)) || any(Y < 0) ||
      any(abs(Y - round(Y)) > sqrt(.Machine$double.eps))) {
    stop("Y must be a finite nonnegative integer-valued matrix with at least 3 rows and 2 columns.",
         call. = FALSE)
  }
  Y
}

#' Fit a graph-shrinkage BDCN model
#'
#' Counts are transformed with `log1p()` lag features centered and scaled from
#' the training rows only. Deterministic time and seasonal covariates are also
#' centered on the training period. The fitted object retains this preprocessing
#' so DSS and sequential prediction use the same frozen transformation.
#'
#' @param Y Time-by-series nonnegative integer count matrix.
#' @param graph A graph returned by [bdcn_grid()] or an equivalent list.
#' @param train_rows Rows of `Y` used to fit the posterior.
#' @param pre_history Optional matrix with at least one count row preceding `Y`.
#' @param pairs Optional candidate dictionary; defaults to [bdcn_pairs()].
#' @param time_covariate Optional known time covariate for all rows of `Y`.
#' @param season_covariate Optional known seasonal covariate for all rows of `Y`.
#' @param means Optional prior centering means, one per series.
#' @param W Optional physical network matrix used by the static network term.
#' @param lag_reference_scale Optional source-specific scale used by a known
#'   data-generating transformation. This is mainly for exact simulation
#'   reproduction; ordinary analyses should leave it `NULL`.
#' @param signal Numeric scenario label used only for deterministic seeds.
#' @param label Human-readable fit label.
#' @param control A [bdcn_control()] object.
#' @return A `bdcn_fit` object containing posterior draws and frozen inputs.
#' @export
bdcn_fit <- function(
    Y, graph, train_rows = seq_len(nrow(Y)), pre_history = NULL, pairs = NULL,
    time_covariate = NULL, season_covariate = NULL, means = NULL, W = NULL,
    lag_reference_scale = NULL, signal = 0,
    label = "BDCN full dynamic nonlocal domain", control = bdcn_control()) {
  Y <- .validate_count_matrix(Y)
  graph <- .validate_bdcn_graph(graph)
  control <- .validate_bdcn_control(control)
  if (ncol(Y) != nrow(graph$edges)) {
    stop("The columns of Y must match graph$edges.", call. = FALSE)
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
  if (is.null(pairs)) pairs <- make_full_pairs(graph, control)
  needed_pair_fields <- c("target", "source", "affinity")
  if (!is.data.frame(pairs) || !all(needed_pair_fields %in% names(pairs)) ||
      !nrow(pairs)) {
    stop("pairs must be a nonempty BDCN candidate data frame.", call. = FALSE)
  }
  n <- nrow(Y)
  if (is.null(time_covariate)) time_covariate <- make_time_covariate(n)
  if (is.null(season_covariate)) {
    season_covariate <- make_season_covariate(n, control)
  }
  if (length(time_covariate) != n || length(season_covariate) != n ||
      any(!is.finite(time_covariate)) || any(!is.finite(season_covariate))) {
    stop("Time and seasonal covariates must be finite vectors matching nrow(Y).",
         call. = FALSE)
  }
  time_covariate <- time_covariate - mean(time_covariate[train_rows])
  season_covariate <- season_covariate - mean(season_covariate[train_rows])
  raw_lags <- make_lags(Y, pre_history, rep(0, ncol(Y)), rep(1, ncol(Y)), 1L)
  standardized <- standardize_lags_from_train(raw_lags, train_rows)
  X <- standardized$lags[, , 1L]
  if (is.null(W)) W <- graph$W
  W <- as.matrix(W)
  if (!identical(dim(W), c(ncol(Y), ncol(Y))) || any(!is.finite(W))) {
    stop("W must be a finite square matrix matching ncol(Y).", call. = FALSE)
  }
  if (!is.null(lag_reference_scale)) {
    if (length(lag_reference_scale) != ncol(Y) ||
        any(!is.finite(lag_reference_scale)) || any(lag_reference_scale <= 0)) {
      stop("lag_reference_scale must contain one positive value per series.",
           call. = FALSE)
    }
    W <- sweep(W, 2L, standardized$scale / lag_reference_scale, "*")
  }
  if (is.null(means)) means <- colMeans(Y[train_rows, , drop = FALSE])
  if (length(means) != ncol(Y) || any(!is.finite(means)) || any(means <= 0)) {
    stop("means must contain one positive finite value per series.", call. = FALSE)
  }
  fit <- fit_bdcn(
    Y[train_rows, , drop = FALSE], X[train_rows, , drop = FALSE],
    time_covariate[train_rows], season_covariate[train_rows], W, pairs,
    means, signal, label, control
  )
  fit$Y <- Y
  fit$X <- X
  fit$time_covariate <- time_covariate
  fit$season_covariate <- season_covariate
  fit$train_rows <- train_rows
  fit$pre_history <- pre_history
  fit$scaler <- list(center = standardized$center, scale = standardized$scale)
  fit$graph <- graph
  fit$control <- control
  fit$signal <- signal
  class(fit) <- c("bdcn_fit", "list")
  fit
}

#' Diagnose a BDCN posterior fit
#'
#' @param object A fitted [bdcn_fit()] object.
#' @param truth_pairs Optional data frame with `target` and `source` columns for
#'   marking injected pairs in simulation diagnostics.
#' @param model_id Identifier stored in the returned table.
#' @param control Optional replacement [bdcn_control()] object.
#' @return A parameter-level split-Rhat, ESS, interval, and pass/fail table.
#' @export
bdcn_diagnostics <- function(object, truth_pairs = NULL,
                             model_id = "BDCN_continuous",
                             control = object$control) {
  if (inherits(object, "bdcn_general_fit")) {
    control <- .validate_bdcn_control(control)
    out <- basic_convergence(object, object$signal, control)
    out$Converged <- out$PassRhat & out$PassESS
    return(out)
  }
  if (!inherits(object, "bdcn_fit")) {
    stop("object must be a bdcn_fit or bdcn_general_fit.", call. = FALSE)
  }
  control <- .validate_bdcn_control(control)
  if (is.null(truth_pairs)) {
    truth_pairs <- data.frame(target = integer(), source = integer())
  }
  if (!all(c("target", "source") %in% names(truth_pairs))) {
    stop("truth_pairs must contain target and source columns.", call. = FALSE)
  }
  diagnose_fit(object, object$signal, model_id, truth_pairs, control)
}

#' Select a sparse BDCN network with joint DSS
#'
#' The posterior quadratic DSS path is built from the training period. A common
#' sparsity level is selected by sequential one-step Poisson deviance on the
#' validation rows, relative to the continuous posterior fit.
#'
#' @param object A fitted [bdcn_fit()] object.
#' @param validation_rows Rows held out from posterior fitting.
#' @param control Optional replacement [bdcn_control()] object.
#' @return A `bdcn_dss` object with the selected multipliers, support, and path.
#' @export
bdcn_dss <- function(object, validation_rows, control = object$control) {
  if (!inherits(object, c("bdcn_fit", "bdcn_general_fit"))) {
    stop("object must be a bdcn_fit or bdcn_general_fit.", call. = FALSE)
  }
  control <- .validate_bdcn_control(control)
  validation_rows <- sort(unique(as.integer(validation_rows)))
  if (!length(validation_rows) || anyNA(validation_rows) ||
      min(validation_rows) < 1L || max(validation_rows) > nrow(object$Y) ||
      any(validation_rows %in% object$train_rows)) {
    stop("validation_rows must be valid rows outside the training set.",
         call. = FALSE)
  }
  ans <- if (inherits(object, "bdcn_general_fit")) {
    run_general_dss(
      object, object$X[object$train_rows, , drop = FALSE],
      object$covariates[object$train_rows, , drop = FALSE], object$X,
      object$covariates, object$Y, validation_rows, object$signal, control
    )
  } else {
    run_dss(
      object, object$X[object$train_rows, , drop = FALSE],
      object$time_covariate[object$train_rows],
      object$season_covariate[object$train_rows], object$X,
      object$time_covariate, object$season_covariate, object$Y,
      validation_rows, object$signal, control
    )
  }
  ans$validation_rows <- validation_rows
  class(ans) <- c("bdcn_dss", "list")
  ans
}

#' Sequential one-step prediction from a BDCN fit
#'
#' @param object A fitted [bdcn_fit()] object.
#' @param rows Rows to score and predict.
#' @param conditioning_rows Optional earlier held-out rows whose observed counts
#'   are assimilated before the requested prediction rows.
#' @param d Optional DSS multipliers, usually `bdcn_dss(fit, validation)$d`.
#' @param label Prediction label.
#' @param control Optional replacement [bdcn_control()] object.
#' @param ... Reserved for compatibility with the generic.
#' @return A `bdcn_prediction` containing intensity and count draws.
#' @export
predict.bdcn_fit <- function(object, rows, conditioning_rows = integer(),
                             d = NULL, label = object$label,
                             control = object$control, ...) {
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
  ans <- predict_bdcn(
    object, object$X, object$time_covariate, object$season_covariate,
    object$Y, rows, conditioning_rows, d, label, object$signal, control
  )
  ans$rows <- rows
  ans$conditioning_rows <- conditioning_rows
  class(ans) <- c("bdcn_prediction", "list")
  ans
}

#' Score BDCN posterior predictions
#'
#' @param prediction A result from `predict()` on a `bdcn_fit`.
#' @param Y Observed count matrix.
#' @param truth Optional true intensity matrix. When unavailable, use `Y` to
#'   omit a distinct DGP-intensity target from interpretation.
#' @param signal Numeric scenario label.
#' @param control A [bdcn_control()] object.
#' @return One-row data frame of predictive metrics.
#' @export
bdcn_score <- function(prediction, Y, truth = Y, signal = 0,
                       control = bdcn_control()) {
  if (!inherits(prediction, "bdcn_prediction")) {
    stop("prediction must be a bdcn_prediction.", call. = FALSE)
  }
  Y <- as.matrix(Y)
  truth <- as.matrix(truth)
  if (!identical(dim(Y), dim(truth))) stop("Y and truth must have the same dimensions.", call. = FALSE)
  prediction_metrics(prediction, Y, truth, prediction$rows, signal,
                     .validate_bdcn_control(control))
}

#' Run the packaged sparse multi-hop example
#'
#' This convenience function performs simulation, posterior fitting, diagnostics,
#' joint DSS, and continuous/DSS prediction with the same frozen preprocessing.
#' Smoke and quick profiles are computational checks, not scientific evidence.
#'
#' @param profile Control profile passed to [bdcn_control()].
#' @param signal Injected two-hop RMS amplitude.
#' @param side Square-grid side length.
#' @param control Optional explicit control object. When supplied, `profile` is
#'   ignored.
#' @return A `bdcn_example` bundle containing simulation, fit, diagnostics, DSS,
#'   predictions, and score tables.
#' @export
#' @examples
#' if (interactive()) {
#'   result <- bdcn_run_multihop_example("smoke")
#'   result$scores
#' }
bdcn_run_multihop_example <- function(profile = c("smoke", "quick", "default"),
                                      signal = 0.20, side = 3L,
                                      control = NULL) {
  profile <- match.arg(profile)
  if (is.null(control)) control <- bdcn_control(profile)
  design <- bdcn_simulate_multihop(signal, side, control)
  sim <- design$simulation
  fit <- bdcn_fit(
    sim$Y, design$graph, train_rows = design$split$train,
    pre_history = sim$pre_history, pairs = design$pairs,
    time_covariate = sim$time_covariate,
    season_covariate = sim$season_covariate, means = sim$means,
    lag_reference_scale = sim$scale, signal = signal, control = control
  )
  diagnostics <- bdcn_diagnostics(fit, design$long_pairs, control = control)
  dss <- bdcn_dss(fit, design$split$validation, control)
  continuous <- predict(
    fit, design$split$test, design$split$validation,
    label = "BDCN continuous", control = control
  )
  sparse <- predict(
    fit, design$split$test, design$split$validation, d = dss$d,
    label = "BDCN joint DSS", control = control
  )
  scores <- rbind(
    bdcn_score(continuous, sim$Y, sim$lambda, signal, control),
    bdcn_score(sparse, sim$Y, sim$lambda, signal, control)
  )
  structure(
    list(design = design, fit = fit, diagnostics = diagnostics, dss = dss,
         predictions = list(continuous = continuous, dss = sparse),
         scores = scores),
    class = c("bdcn_example", "list")
  )
}

#' @export
print.bdcn_fit <- function(x, ...) {
  cat("Bayesian dynamic count network fit\n")
  cat("  series:", ncol(x$Y), "\n")
  cat("  training times:", length(x$train_rows), "\n")
  cat("  candidate pairs:", nrow(x$pairs), "\n")
  cat("  posterior draws:", nrow(x$alpha), "across", length(x$chains), "chains\n")
  cat("  elapsed seconds:", format(x$elapsed, digits = 4L), "\n")
  invisible(x)
}

#' @export
summary.bdcn_fit <- function(object, ...) {
  diagnostics <- bdcn_diagnostics(object)
  structure(list(
    label = object$label,
    series = ncol(object$Y),
    training_times = length(object$train_rows),
    candidate_pairs = nrow(object$pairs),
    posterior_draws = nrow(object$alpha),
    elapsed = object$elapsed,
    posterior_inclusion_probability = colMeans(
      object$amplitude > object$control$delta_A
    ),
    convergence = convergence_summary(diagnostics, object$control),
    diagnostics = diagnostics
  ), class = c("summary.bdcn_fit", "list"))
}

#' @export
print.summary.bdcn_fit <- function(x, ...) {
  cat("BDCN posterior summary\n")
  cat("  series:", x$series, "\n")
  cat("  training times:", x$training_times, "\n")
  cat("  candidate pairs:", x$candidate_pairs, "\n")
  cat("  posterior draws:", x$posterior_draws, "\n")
  cat("  all monitored parameters converged:",
      x$convergence$AllConverged[1L], "\n")
  invisible(x)
}

#' @export
print.bdcn_dss <- function(x, ...) {
  cat("BDCN joint DSS selection\n")
  cat("  selected pairs:", sum(x$selected), "/", length(x$selected), "\n")
  cat("  validation loss:", format(x$validation_loss, digits = 5L), "\n")
  cat("  relative degradation:",
      format(x$relative_validation_degradation, digits = 5L), "\n")
  invisible(x)
}
