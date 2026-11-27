# Shared I/O and display conventions for the two manuscript applications.
preparation_options <- function(names) {
  args <- commandArgs(TRUE)
  keys <- sub("^--([^=]+)=.*$", "\\1", args)
  if (any(!grepl("^--[^=]+=.+$", args)) || anyDuplicated(keys) ||
      any(!keys %in% names)) stop("Use the documented --name=value options.")
  opts <- as.list(setNames(sub("^--[^=]+=", "", args), keys))
  if (is.null(opts$output) || !grepl("^(/|[A-Za-z]:[/\\\\])", path.expand(opts$output))) {
    stop("Specify an absolute --output=/path/to/prepared.rds.")
  }
  opts$output <- path.expand(opts$output)
  opts
}

daily_covariates <- function(n, bins_per_day) {
  phase <- 2 * pi * (seq_len(n) - 1L) / bins_per_day
  cbind(day_sin = sin(phase), day_cos = cos(phase))
}

save_prepared_case <- function(x, output, title, units, source_files) {
  required <- c("Y", "graph", "covariates", "split")
  if (!all(required %in% names(x))) stop("Prepared input requires Y, graph, covariates, and split.")
  Y <- as.matrix(x$Y)
  if (!nrow(Y) || !ncol(Y) || anyNA(Y) || any(!is.finite(Y) | Y < 0 | Y != floor(Y)))
    stop("Counts must be observed nonnegative integers on a regular panel.")
  if (ncol(Y) != nrow(x$graph$edges) || nrow(x$covariates) != nrow(Y) ||
      !all(c("train", "validation", "test") %in% names(x$split)) ||
      any(lengths(x$split[c("train", "validation", "test")]) < 1L))
    stop("Prepared count, graph, covariate, and split dimensions disagree.")
  idx <- unname(unlist(x$split[c("train", "validation", "test")], use.names = FALSE))
  if (anyNA(idx) || any(idx < 1 | idx > nrow(Y) | idx != floor(idx)) ||
      is.unsorted(idx, strictly = TRUE)) stop("Invalid chronological split.")
  groups <- rep("unscored", nrow(Y))
  for (key in c("train", "validation", "test")) groups[x$split[[key]]] <- key
  x$split_group <- groups
  if (is.null(x$series_labels)) x$series_labels <- colnames(Y)
  if (is.null(x$series_labels)) x$series_labels <- x$graph$edges$name
  if (is.null(x$series_labels)) x$series_labels <- paste("Series", seq_len(ncol(Y)))
  if (is.null(x$time_labels)) x$time_labels <- if (is.null(x$time)) as.character(seq_len(nrow(Y))) else as.character(x$time)
  if (length(x$series_labels) != ncol(Y) || length(x$time_labels) != nrow(Y)) stop("Invalid series or time labels.")
  x$source_files <- data.frame(file = normalizePath(source_files), md5 = unname(tools::md5sum(source_files)))
  x$summary <- list(series = ncol(Y), periods = nrow(Y), split_sizes = lengths(x$split),
                    total_count = sum(Y), zero_fraction = mean(Y == 0), units = units)

  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  png_file <- paste0(tools::file_path_sans_ext(output), "-counts.png")
  if (file.exists(output) || file.exists(png_file)) stop("Output exists. Supply a new output name.")
  draw <- tempfile(tmpdir = dirname(output), fileext = ".png")
  artifact <- tempfile(tmpdir = dirname(output), fileext = ".rds")
  on.exit(unlink(c(draw, artifact)), add = TRUE)
  grDevices::png(draw, width = 1100, height = 540, res = 110)
  tryCatch({
    graphics::par(mar = c(4.2, 4.5, 3, 1))
    colors <- grDevices::hcl.colors(4, "Dark 3")
    shown <- seq_len(min(4L, ncol(Y)))
    graphics::matplot(Y[, shown, drop = FALSE], type = "l", lty = 1,
        col = colors[shown], xlab = "Time bin", ylab = units, main = title)
    graphics::abline(v = c(min(x$split$validation), min(x$split$test)) - 0.5,
        col = "gray40", lty = 3)
    graphics::legend("topright", legend = x$series_labels[shown], col = colors[shown],
        lty = 1, cex = .75, bg = "white", bty = "o")
  }, finally = grDevices::dev.off())
  x$plots <- c(overview = basename(png_file))
  saveRDS(x, artifact)
  if (!file.rename(draw, png_file)) stop("Could not finalize the count plot.")
  if (!file.rename(artifact, output)) {
    unlink(png_file)
    stop("Could not finalize the prepared RDS.")
  }
  cat(x$case_id, "prepared", nrow(Y), "times by", ncol(Y), "series at", output, "\n")
}
