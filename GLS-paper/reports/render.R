#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
if (length(args) < 2L || length(args) > 3L)
  stop("Usage: Rscript GLS-paper/reports/render.R CASE RESULT_DIRECTORY [HTML_DIRECTORY]")
cases <- c("01-graph-error", "02-sparse-grid", "03-surges", "divvy", "pems")
case <- match.arg(args[1], cases)
script <- gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), fixed = TRUE)
here <- dirname(normalizePath(script, mustWork = TRUE))
results <- normalizePath(args[2], mustWork = TRUE)
output <- if (length(args) == 3L) args[3] else here
if (!dir.exists(output)) dir.create(output, recursive = TRUE)
if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("Install the rmarkdown package for rendering.")
rmarkdown::render(file.path(here, paste0(case, ".Rmd")), output_dir = output,
  params = list(results = results), envir = new.env(parent = globalenv()), quiet = TRUE)
