#' Configure the GLS directed-grid simulation study
#'
#' The study uses a static edge-specific baseline, Fourier seasonality, all
#' ordered source-target pairs, learned affinity features, a covariance-discount
#' state evolution, and validation-selected joint DSS. The `smoke` and `quick`
#' profiles are workflow checks rather than inferential settings. Set
#' `shrinkage_spec = "graph_laplace_fixed_tau"` to use the Appendix A.2
#' Graph-Laplace score as the sole affinity feature and fix the common global
#' scale at `fixed_tau`. The legacy value `"graph_laplace_tau1"` remains
#' available for old saved configurations.
#'
#' @param profile One of `"full"`, `"quick"`, or `"smoke"`.
#' @param sensitivity Optional named sensitivity preset. Use
#'   [gls_study_sensitivity_presets()] to list the available presets.
#' @param ... Named replacements for study settings.
#' @return A validated `gls_study_control` object.
#' @export
gls_study_control <- function(profile = c("full", "quick", "smoke"),
                              sensitivity = "none", ...) {
  profile <- match.arg(profile)
  mode <- if (profile == "smoke") "smoke_check" else profile
  control <- .gls_study_control_impl(mode, sensitivity)
  replacements <- list(...)
  if (length(replacements) &&
      (is.null(names(replacements)) || any(!nzchar(names(replacements))))) {
    stop("All replacement study settings must be named.", call. = FALSE)
  }
  unknown <- setdiff(names(replacements), names(control))
  if (length(unknown)) {
    stop("Unknown study setting(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  control[names(replacements)] <- replacements
  .gls_study_validate_control(control)
}

#' List GLS study sensitivity presets
#'
#' @return A named list of setting replacements. The `none` element is empty.
#' @export
gls_study_sensitivity_presets <- function() {
  .gls_study_sensitivity_presets_impl()
}

#' Derive a deterministic GLS study seed
#'
#' This helper lets post-processing and benchmark scripts reproduce random
#' subsets and predictive draws without duplicating the package's seed rule.
#'
#' @param stage Stage label.
#' @param label Optional scenario or method label.
#' @param control A [gls_study_control()] object.
#' @return One positive integer seed.
#' @export
gls_study_seed <- function(stage, label = "",
                           control = gls_study_control()) {
  control <- .gls_study_validate_control(control)
  .gls_study_seed(stage, label, control)
}

#' Construct the GLS directed-grid study design
#'
#' This function deliberately reuses [gls_grid()] and [gls_all_pairs()] from
#' the package. The returned candidate domain contains every ordered pair,
#' including self, one-hop, two-hop, and unreachable pairs.
#'
#' @param control A [gls_study_control()] object.
#' @return A `gls_study_design` containing the graph, all-pairs dictionary,
#'   learned-affinity feature matrix, and fixed active-pair design.
#' @export
gls_study_design <- function(control = gls_study_control()) {
  control <- .gls_study_validate_control(control)
  graph <- gls_grid(control$grid_side)
  pairs <- gls_all_pairs(graph)
  Fg <- if (control$shrinkage_spec %in%
              c("graph_laplace_fixed_tau", "graph_laplace_tau1")) {
    .gls_study_make_graph_laplace_feature(
      graph, pairs, control$graph_laplace_rho0,
      control$affinity_feature_tolerance
    )
  } else {
    .gls_study_make_affinity_features(
      pairs, control$affinity_feature_tolerance
    )
  }
  active <- .gls_study_choose_active_pairs(pairs, control)
  structure(
    list(graph = graph, pairs = pairs, affinity_features = Fg,
         active_pairs = active, control = control),
    class = c("gls_study_design", "list")
  )
}

#' Simulate one GLS directed-grid scenario
#'
#' The data-generating process is implemented in `R/simulation.R`. It has a
#' static baseline for each directed edge, periodic Fourier effects, and sparse
#' static-plus-dynamic network propagation over the all-pairs domain.
#'
#' @param signal Target RMS amplitude for active propagation trajectories.
#' @param design Optional [gls_study_design()] object.
#' @param control A [gls_study_control()] object; ignored when `design` is
#'   supplied except for an identity check.
#' @return A `gls_study_simulation` object.
#' @export
gls_simulate_grid <- function(signal, design = NULL,
                              control = gls_study_control()) {
  if (length(signal) != 1L || !is.finite(signal) || signal < 0) {
    stop("signal must be one finite nonnegative number.", call. = FALSE)
  }
  if (is.null(design)) design <- gls_study_design(control)
  if (!inherits(design, "gls_study_design")) {
    stop("design must be a gls_study_design.", call. = FALSE)
  }
  control <- .gls_study_validate_control(design$control)
  out <- .gls_study_simulate_grid_impl(
    design$graph, design$pairs, design$active_pairs, signal, control
  )
  out$graph <- design$graph
  out$pairs <- design$pairs
  class(out) <- c("gls_study_simulation", "list")
  out
}

.gls_study_split <- function(control) {
  ntrain <- floor(control$n_total * control$train_fraction)
  nvalidation <- floor(control$n_total * control$validation_fraction)
  list(
    train = seq_len(ntrain),
    validation = ntrain + seq_len(nvalidation),
    test = (ntrain + nvalidation + 1L):control$n_total
  )
}

#' Fit the GLS study model to one simulated scenario
#'
#' @param simulation A [gls_simulate_grid()] result.
#' @param design The matching [gls_study_design()] object.
#' @param control The matching [gls_study_control()] object.
#' @return A `gls_study_fit` containing posterior draws and frozen inputs.
#' @export
gls_fit_study <- function(simulation, design,
                          control = design$control) {
  if (!inherits(simulation, "gls_study_simulation")) {
    stop("simulation must be a gls_study_simulation.", call. = FALSE)
  }
  if (!inherits(design, "gls_study_design")) {
    stop("design must be a gls_study_design.", call. = FALSE)
  }
  control <- .gls_study_validate_control(control)
  split <- .gls_study_split(control)
  standardized <- .gls_study_standardize_from_train(
    simulation$X_raw, split$train
  )
  X <- standardized$X
  Q <- .gls_study_fourier_design(
    control$n_total, control$season_period, control$fitted_fourier_order
  )
  fit <- .gls_study_fit_impl(
    simulation$Y[split$train, , drop = FALSE],
    X[split$train, , drop = FALSE], Q[split$train, , drop = FALSE],
    design$pairs, design$affinity_features, simulation$means,
    simulation$signal, control
  )
  fit$Y <- simulation$Y
  fit$X <- X
  fit$covariates <- Q
  fit$train_rows <- split$train
  fit$split <- split
  fit$scaler <- standardized[c("center", "scale")]
  fit$graph <- design$graph
  fit$control <- control
  fit$signal <- simulation$signal
  class(fit) <- c("gls_study_fit", "list")
  fit
}

#' Sequential prediction from a GLS study fit
#'
#' @param object A [gls_fit_study()] result.
#' @param rows Rows to predict.
#' @param conditioning_rows Earlier held-out observations to assimilate first.
#' @param d Optional DSS multipliers, one per candidate pair.
#' @param label Prediction label.
#' @param control Optional replacement study control.
#' @param ... Reserved for the generic.
#' @return A `gls_prediction` object.
#' @export
predict.gls_study_fit <- function(
    object, rows, conditioning_rows = integer(), d = NULL,
    label = object$label, control = object$control, ...) {
  control <- .gls_study_validate_control(control)
  rows <- as.integer(rows)
  conditioning_rows <- as.integer(conditioning_rows)
  sequence_rows <- c(conditioning_rows, rows)
  if (!length(rows) || anyNA(sequence_rows) || any(sequence_rows < 1L) ||
      any(sequence_rows > nrow(object$Y)) ||
      is.unsorted(sequence_rows, strictly = TRUE)) {
    stop("conditioning_rows followed by rows must be strictly increasing valid indices.",
         call. = FALSE)
  }
  if (!is.null(d) &&
      (length(d) != nrow(object$pairs) || any(!is.finite(d)))) {
    stop("d must be NULL or one finite multiplier per candidate pair.",
         call. = FALSE)
  }
  draw_ids <- balanced_indices(
    object$chain_id, control$predictive_draws,
    .gls_study_seed(paste0("prediction: ", label), object$signal, control)
  )
  ans <- .gls_study_rolling_prediction(
    object, object$X, object$covariates, object$Y, rows,
    conditioning_rows, d, draw_ids,
    .gls_study_seed(paste0("prediction draws: ", label), object$signal,
                    control),
    control
  )
  ans$label <- label
  ans$rows <- rows
  ans$conditioning_rows <- conditioning_rows
  class(ans) <- c("gls_prediction", "list")
  ans
}

#' Run one complete GLS directed-grid scenario
#'
#' @param signal Scenario signal amplitude.
#' @param design A [gls_study_design()] object.
#' @param control A [gls_study_control()] object.
#' @return A `gls_study_scenario` with simulation, fit, DSS, predictions,
#'   PNAR benchmarks, diagnostics, and summary tables.
#' @export
gls_run_grid_scenario <- function(signal, design,
                                  control = design$control) {
  control <- .gls_study_validate_control(control)
  message(sprintf("\nGLS directed-grid signal = %.2f", signal))
  simulation <- gls_simulate_grid(signal, design, control)
  fit <- gls_fit_study(simulation, design, control)
  split <- fit$split
  dss <- gls_dss(fit, split$validation, control)
  dss$table$signal <- signal
  full <- predict(
    fit, split$test, split$validation, rep(1, nrow(design$pairs)),
    label = "GLS", control = control
  )
  sparse <- predict(
    fit, split$test, split$validation, dss$d,
    label = "GLSS", control = control
  )
  pnar1_fit <- .gls_study_fit_pnar(
    fit$Y, fit$X, fit$covariates, split$train, design$graph$W, 1L
  )
  pnar2_fit <- .gls_study_fit_pnar(
    fit$Y, fit$X, fit$covariates, split$train, design$graph$W, 2L
  )
  pnar1 <- .gls_study_predict_pnar(pnar1_fit, split$test, control)
  pnar2 <- .gls_study_predict_pnar(pnar2_fit, split$test, control)
  performance <- rbind(
    .gls_study_prediction_metrics(
      "GLS", full, fit$Y, simulation$lambda, split$test, signal
    ),
    .gls_study_prediction_metrics(
      "GLSS", sparse, fit$Y, simulation$lambda, split$test,
      signal
    ),
    .gls_study_prediction_metrics(
      "PNAR(1), physical W", pnar1, fit$Y, simulation$lambda, split$test,
      signal
    ),
    .gls_study_prediction_metrics(
      "PNAR(2), physical W", pnar2, fit$Y, simulation$lambda, split$test,
      signal
    )
  )
  marginal <- .gls_study_marginal_pair_summary(fit, control)
  selection <- .gls_study_shrinkage_dss_summary(
    fit, fit$X[split$train, , drop = FALSE], dss, simulation, control
  )
  selection$signal <- signal
  selection <- merge(
    selection,
    marginal[, c("pair_id", "exceedance_probability", "marginal_selected")],
    by = "pair_id", all.x = TRUE, suffixes = c("", "_marginal"),
    sort = FALSE
  )
  structure(list(
    simulation = simulation, X = fit$X, covariates = fit$covariates,
    fit = fit, dss = dss, performance = performance,
    selection = selection,
    convergence = gls_diagnostics(fit, control = control),
    dss_amplitude_association =
      .gls_study_dss_amplitude_association(selection, signal),
    split = split,
    predictions = list(full = full, dss = sparse,
                       pnar1 = pnar1, pnar2 = pnar2)
  ), class = c("gls_study_scenario", "list"))
}

#' Run the GLS directed-grid simulation study
#'
#' @param control A [gls_study_control()] object.
#' @param output_dir Optional directory passed to [gls_write_grid_study()].
#' @return A `gls_grid_study` object.
#' @export
gls_run_grid_study <- function(control = gls_study_control(),
                               output_dir = NULL) {
  control <- .gls_study_validate_control(control)
  design <- gls_study_design(control)
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    dput(unclass(control), file.path(output_dir, "configuration.R"))
  }
  scenarios <- vector("list", length(control$signal_grid))
  for (i in seq_along(control$signal_grid)) {
    signal <- control$signal_grid[i]
    checkpoint <- if (!is.null(output_dir)) file.path(
      output_dir, sprintf("signal_%.2f", signal), "scenario_results.rds"
    ) else NULL
    scenario <- NULL
    if (!is.null(checkpoint) && file.exists(checkpoint)) {
      candidate <- try(readRDS(checkpoint), silent = TRUE)
      same_control <- !inherits(candidate, "try-error") &&
        inherits(candidate, "gls_study_scenario") &&
        isTRUE(all.equal(candidate$fit$control, control,
                         check.attributes = FALSE))
      same_signal <- same_control &&
        isTRUE(all.equal(candidate$simulation$signal, signal))
      if (same_signal) {
        message(sprintf("Using completed checkpoint for signal %.2f", signal))
        scenario <- candidate
      }
    }
    if (is.null(scenario)) {
      scenario <- gls_run_grid_scenario(signal, design, control)
      if (!is.null(checkpoint)) {
        signal_dir <- dirname(checkpoint)
        dir.create(signal_dir, recursive = TRUE, showWarnings = FALSE)
        temporary <- tempfile("scenario_results_", signal_dir, ".rds")
        saveRDS(scenario, temporary, compress = "xz")
        if (!file.rename(temporary, checkpoint)) {
          unlink(temporary)
          stop("Could not finalize scenario checkpoint: ", checkpoint,
               call. = FALSE)
        }
        message(sprintf("Saved checkpoint for signal %.2f: %s",
                        signal, checkpoint))
      }
    }
    scenarios[[i]] <- scenario
  }
  out <- structure(list(
    control = control, design = design, scenarios = scenarios
  ), class = c("gls_grid_study", "list"))
  if (!is.null(output_dir)) gls_write_grid_study(out, output_dir)
  out
}

.gls_study_bind <- function(scenarios, name) {
  do.call(rbind, lapply(scenarios, `[[`, name))
}

#' Write GLS directed-grid study results
#'
#' Existing files are replaced only inside `output_dir`. The saved scenario
#' objects and benchmark snapshot allow plotting and external benchmarks to be
#' run without repeating MCMC.
#'
#' @param study A [gls_run_grid_study()] result.
#' @param output_dir Destination directory.
#' @return The normalized output path, invisibly.
#' @export
gls_write_grid_study <- function(study, output_dir) {
  if (!inherits(study, "gls_grid_study")) {
    stop("study must be a gls_grid_study.", call. = FALSE)
  }
  if (length(output_dir) != 1L || !nzchar(output_dir)) {
    stop("output_dir must be one nonempty path.", call. = FALSE)
  }
  control <- study$control
  scenarios <- study$scenarios
  design <- study$design
  table_dir <- file.path(output_dir, "tables")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "figures"), recursive = TRUE,
             showWarnings = FALSE)
  utils::write.csv(
    .gls_study_bind(scenarios, "performance"),
    file.path(table_dir, "predictive_performance.csv"), row.names = FALSE
  )
  utils::write.csv(
    .gls_study_bind(scenarios, "selection"),
    file.path(table_dir, "pair_selection.csv"), row.names = FALSE
  )
  utils::write.csv(
    .gls_study_bind(scenarios, "convergence"),
    file.path(table_dir, "MCMC_convergence.csv"), row.names = FALSE
  )
  utils::write.csv(
    .gls_study_bind(scenarios, "dss_amplitude_association"),
    file.path(table_dir, "DSS_amplitude_association.csv"), row.names = FALSE
  )
  acceptance <- do.call(rbind, lapply(scenarios, function(x) {
    cbind(signal = x$simulation$signal, x$fit$acceptance)
  }))
  utils::write.csv(
    acceptance, file.path(table_dir, "sampler_acceptance.csv"),
    row.names = FALSE
  )
  discount <- do.call(rbind, lapply(scenarios, function(x) {
    data.frame(
      signal = x$simulation$signal, discount = control$discount_grid,
      posterior_frequency = vapply(
        control$discount_grid,
        function(d) mean(abs(x$fit$discount - d) < 1e-12), numeric(1)
      ),
      mean_conditional_probability = colMeans(x$fit$discount_probability)
    )
  }))
  utils::write.csv(
    discount, file.path(table_dir, "discount_posterior.csv"),
    row.names = FALSE
  )
  scale <- do.call(rbind, lapply(scenarios, function(x) {
    tau <- data.frame(
      signal = x$simulation$signal, parameter = "tau",
      mean = mean(x$fit$tau), median = stats::median(x$fit$tau),
      q025 = unname(stats::quantile(x$fit$tau, 0.025)),
      q975 = unname(stats::quantile(x$fit$tau, 0.975))
    )
    chi <- if (ncol(x$fit$chi)) do.call(rbind, lapply(
      seq_len(ncol(x$fit$chi)), function(j) data.frame(
        signal = x$simulation$signal,
        parameter = paste0("chi[", j, "]"),
        mean = mean(x$fit$chi[, j]), median = stats::median(x$fit$chi[, j]),
        q025 = unname(stats::quantile(x$fit$chi[, j], 0.025)),
        q975 = unname(stats::quantile(x$fit$chi[, j], 0.975))
      )
    )) else NULL
    rbind(tau, chi)
  }))
  utils::write.csv(
    scale, file.path(table_dir, "scale_parameter_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    do.call(rbind, lapply(scenarios, function(x) x$dss$table)),
    file.path(table_dir, "DSS_path.csv"), row.names = FALSE
  )
  utils::write.csv(
    data.frame(feature = colnames(design$affinity_features)),
    file.path(table_dir, "affinity_features.csv"), row.names = FALSE
  )
  utils::write.csv(
    design$active_pairs, file.path(table_dir, "true_active_pairs.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    do.call(rbind, lapply(scenarios, function(x) x$simulation$calibration)),
    file.path(table_dir, "DGP_baseline_calibration.csv"), row.names = FALSE
  )
  .gls_study_export_benchmark_data(
    scenarios, design$graph, control, output_dir
  )
  dput(unclass(control), file.path(output_dir, "configuration.R"))
  for (scenario in scenarios) {
    signal_dir <- file.path(
      output_dir, sprintf("signal_%.2f", scenario$simulation$signal)
    )
    dir.create(signal_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(scenario, file.path(signal_dir, "scenario_results.rds"),
            compress = "xz")
    .gls_study_save_figures(scenario, signal_dir, control)
  }
  saveRDS(
    study, file.path(output_dir, "GLS_simulation_summary.rds"),
    compress = "xz"
  )
  invisible(normalizePath(output_dir))
}
