#' Compact summaries of posterior predictive counts
#'
#' Summarize a time-by-series-by-draw forecast one cell at a time. The log
#' score integrates the Poisson likelihood over intensity draws. Count
#' intervals use empirical quantiles of the posterior predictive counts.
#' @param prediction A GLS prediction with lambda, y, rows and label.
#' @param Y Full observed time-by-series count matrix.
#' @param level Central predictive interval probability.
#' @param quantile_type R quantile convention. Type 1 returns integer count limits;
#'   type 7 reproduces the earlier application exports.
#' @return A data frame with one row per forecast time and series.
#' @export
gls_forecast_summary <- function(prediction, Y, level = 0.95, quantile_type = 1L) {
  shape <- dim(prediction$lambda)
  rows <- prediction$rows
  if (length(shape) != 3L || any(shape < 1L) ||
      !identical(shape, dim(prediction$y)) || length(rows) != shape[1L] ||
      ncol(Y) != shape[2L] || anyNA(rows) || any(rows < 1 | rows > nrow(Y)) ||
      length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1)
    stop("Incompatible predictive arrays, row indices, or interval level.")
  observed <- as.vector(Y[rows, , drop = FALSE])
  cells <- length(observed)
  intensity <- prediction$lambda
  counts <- prediction$y
  dim(intensity) <- dim(counts) <- c(cells, shape[3L])
  probs <- (1 + c(-1, 1) * level) / 2
  interval <- matrix(0, cells, 2L)
  log_score <- numeric(cells)
  for (i in seq_len(cells)) {
    interval[i, ] <- stats::quantile(counts[i, ], probs, names = FALSE, type = quantile_type)
    lp <- stats::dpois(observed[i], intensity[i, ], log = TRUE)
    peak <- max(lp)
    log_score[i] <- if (is.finite(peak)) peak + log(mean(exp(lp - peak))) else peak
  }
  data.frame(model = prediction$label, row = rep(rows, shape[2L]),
             series = rep(seq_len(shape[2L]), each = shape[1L]), observed = observed,
             mean_intensity = rowMeans(intensity), count_lower = interval[, 1L],
             count_upper = interval[, 2L], level = level, log_score = log_score)
}

#' Compact matrix forecasts from draws of a Poisson mixture
#' @param lambda Finite nonnegative time-by-series-by-draw intensities.
#' @param observed The observed count matrix for these forecast times.
#' @param draws Optional predictive count draws in the same array layout.
#' @param seed Optional simulation seed when generating predictive counts.
#' @return A list of matrices named mean, lower, upper and logprob.
#' @export
gls_poisson_mixture_forecast <- function(lambda, observed, draws = NULL, seed = NULL) {
  shape <- dim(lambda)
  if (!is.numeric(lambda) || length(shape) != 3L || any(shape < 1L) ||
      !identical(dim(observed), shape[1:2]) || any(!is.finite(lambda) | lambda < 0))
    stop("lambda must have finite nonnegative draws for every observed time and series.")
  if (is.null(draws)) {
    if (!is.null(seed)) set.seed(seed)
    draws <- array(stats::rpois(length(lambda), lambda), shape)
  }
  pred <- list(lambda = lambda, y = draws, label = "Poisson mixture", rows = seq_len(shape[1L]))
  summary <- gls_forecast_summary(pred, observed)
  as_panel <- function(values) matrix(values, shape[1L], shape[2L])
  list(mean = as_panel(summary$mean_intensity), logprob = as_panel(summary$log_score),
       lower = as_panel(summary$count_lower), upper = as_panel(summary$count_upper))
}
