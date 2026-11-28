#' Build one-bin log-count lags using training-only scaling
#' @param Y A time-by-series count matrix.
#' @param train_rows Indices used to estimate the lag means and scales.
#' @param pre_history Optional count matrix immediately preceding Y.
#' @return A list with the standardized lag matrix X, center and scale.
#' @export
gls_lag_predictors <- function(Y, train_rows, pre_history = NULL) {
  Y <- as.matrix(Y)
  if (!is.numeric(Y) || !nrow(Y) || !ncol(Y) || any(!is.finite(Y) | Y < 0))
    stop("Y must contain finite nonnegative counts.")
  if (!is.numeric(train_rows) || !length(train_rows) || anyDuplicated(train_rows) ||
      any(!is.finite(train_rows) | train_rows != floor(train_rows) |
          train_rows < 1 | train_rows > nrow(Y)))
    stop("train_rows must contain distinct valid row indices.")
  if (is.null(pre_history)) pre_history <- Y[1L, , drop = FALSE]
  pre_history <- as.matrix(pre_history)
  if (!is.numeric(pre_history) || ncol(pre_history) != ncol(Y) || !nrow(pre_history) ||
      any(!is.finite(pre_history) | pre_history < 0))
    stop("pre_history must contain nonnegative counts for the same series as Y.")
  previous <- rbind(tail(pre_history, 1L), head(Y, -1L))
  dimnames(previous) <- dimnames(Y)
  x <- log1p(previous)
  scaler <- .gls_lag_scaler(x, train_rows)
  list(X = sweep(sweep(x, 2L, scaler$center), 2L, scaler$scale, "/"),
       center = scaler$center, scale = scaler$scale)
}

.gls_lag_scaler <- function(x, rows) {
  training <- x[rows, , drop = FALSE]
  scale <- apply(training, 2L, stats::sd)
  scale[!is.finite(scale) | scale < 1e-8] <- 1
  list(center = colMeans(training), scale = scale)
}

#' Score forecasts of count panels on common time-series cells
#' @param observed A time-by-series matrix of observed counts.
#' @param forecast A list containing a matrix named mean, optionally logprob
#'   and both bounds lower and upper of a 95 percent predictive interval.
#' @param intensity Optional matrix of true generating means.
#' @param windows Named list of additional logical masks shaped like observed.
#' @return A data frame of window, metric, value and cell count.
#' @export
gls_forecast_metrics <- function(observed, forecast, intensity = NULL, windows = list()) {
  if (!is.matrix(observed) || !is.numeric(observed) || !length(observed) ||
      any(!is.finite(observed) | observed < 0))
    stop("observed must be a finite nonnegative count matrix.")
  check_matrix <- function(x, label) {
    if (!is.numeric(x) || !identical(dim(x), dim(observed)))
      stop(label, " must be a numeric matrix matching observed.")
  }
  mu <- forecast$mean
  check_matrix(mu, "forecast$mean")
  if (any(!is.finite(mu) | mu < 0)) stop("Forecast means must be finite and nonnegative.")
  errors <- list(mae = abs(mu - observed), deviance = poisson_deviance(observed, mu))
  if (!is.null(intensity)) {
    check_matrix(intensity, "intensity")
    if (any(!is.finite(intensity) | intensity < 0)) stop("Invalid true intensity.")
    errors$intensity_rmse <- (mu - intensity)^2
  }
  if (!is.null(forecast$logprob)) {
    check_matrix(forecast$logprob, "forecast$logprob")
    errors$log_score <- forecast$logprob
  }
  if (xor(is.null(forecast$lower), is.null(forecast$upper)))
    stop("Supply both 95 percent interval bounds.")
  if (!is.null(forecast$lower)) {
    check_matrix(forecast$lower, "forecast$lower")
    check_matrix(forecast$upper, "forecast$upper")
    if (any(forecast$lower > forecast$upper, na.rm = TRUE))
      stop("A lower interval bound exceeds its upper bound.")
    errors$coverage95 <- observed >= forecast$lower & observed <= forecast$upper
    errors$width95 <- forecast$upper - forecast$lower
  }
  if (!is.list(windows) || (length(windows) && (is.null(names(windows)) ||
      anyNA(names(windows)) || any(!nzchar(names(windows))) ||
      anyDuplicated(c("all", names(windows))))))
    stop("Each additional window needs a unique name other than 'all'.")
  windows <- c(list(all = matrix(TRUE, nrow(observed), ncol(observed))), windows)
  do.call(rbind, lapply(names(windows), function(name) {
    mask <- windows[[name]]
    if (!is.logical(mask) || !identical(dim(mask), dim(observed)) || anyNA(mask))
      stop("Window ", name, " must be a logical matrix matching observed.")
    cells <- sum(mask)
    score <- vapply(errors, function(error) if (cells) mean(error[mask]) else NA_real_, numeric(1))
    if (!is.null(intensity)) score["intensity_rmse"] <- sqrt(score["intensity_rmse"])
    data.frame(window = name, metric = names(score), value = unname(score), cells = cells)
  }))
}

#' Compare coefficient trajectories and selected support on a common dictionary
#' @param truth,estimate Finite matrices with times in rows and ordered pairs in columns.
#' @param selected Logical vector indicating which pairs were selected.
#' @param groups Named list of logical vectors selecting groups of pairs.
#' @return A long data frame with group (window), metric, value and cell count.
#' @export
gls_trajectory_metrics <- function(truth, estimate, selected,
                                   groups = list(all = rep(TRUE, ncol(truth)))) {
  if (!is.matrix(truth) || !is.numeric(truth) || !length(truth) ||
      !is.numeric(estimate) || !identical(dim(truth), dim(estimate)) ||
      any(!is.finite(truth)) || any(!is.finite(estimate)))
    stop("truth and estimate must be matching finite time-by-pair matrices.")
  pair_mask <- function(x) is.logical(x) && is.null(dim(x)) &&
    length(x) == ncol(truth) && !anyNA(x)
  if (!pair_mask(selected) || !is.list(groups) || !length(groups) ||
      is.null(names(groups)) || anyNA(names(groups)) || any(!nzchar(names(groups))) ||
      anyDuplicated(names(groups)) || !all(vapply(groups, pair_mask, logical(1))))
    stop("Provide a selection flag per pair and uniquely named logical pair groups.")
  rms <- sqrt(colMeans(truth^2))
  amplitude_error <- sqrt(colMeans(estimate^2)) - rms
  squared_error <- colMeans((estimate - truth)^2)
  active <- rms > 0
  average <- function(x) if (length(x)) mean(x) else NA_real_
  do.call(rbind, lapply(names(groups), function(name) {
    keep <- groups[[name]]
    values <- c(trajectory_rmse = sqrt(average(squared_error[keep])),
                active_amplitude_bias = average(amplitude_error[keep & active]),
                precision = average(active[keep & selected]),
                recall = average(selected[keep & active]),
                selection_rate = average(selected[keep]), selected = sum(selected[keep]))
    data.frame(window = name, metric = names(values), value = unname(values), cells = sum(keep))
  }))
}
