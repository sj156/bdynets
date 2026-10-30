#!/usr/bin/env Rscript
example <- NULL

build_examples <- function(root, example = NULL) {
  root <- normalizePath(root, mustWork = TRUE)
  source_package <- file.path(dirname(root), "bdynets")
  local_source <- file.exists(file.path(source_package, "DESCRIPTION"))
  if (local_source) {
    if ("bdynets" %in% loadedNamespaces()) stop("A fresh R session is required")
    dependencies <- read.dcf(file.path(source_package, "DESCRIPTION"))[1, "Imports"]
    dependencies <- trimws(strsplit(dependencies, ",", fixed = TRUE)[[1]])
    dependencies <- sub("[[:space:]]*\\(.*", "", dependencies)
    missing <- dependencies[!vapply(dependencies, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))
    local_library <- tempfile("bdynets-library-")
    dir.create(local_library)
    on.exit(unlink(local_library, recursive = TRUE), add = TRUE)
    status <- system2(file.path(R.home("bin"), "R"),
      c("CMD", "INSTALL", paste0("--library=", shQuote(local_library)), shQuote(source_package)))
    if (status != 0L) stop("Package installation failed")
    .libPaths(c(local_library, .libPaths()))
    library(bdynets, lib.loc = local_library)
  } else {
    library(bdynets)
  }
  folders <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  inputs <- file.path(folders, "data", "input.rds")
  keep <- file.exists(inputs); folders <- folders[keep]; inputs <- inputs[keep]
  if (!length(inputs)) stop("No input bundles found")
  if (!is.null(example)) {
    if (!is.character(example) || length(example) != 1L || is.na(example) ||
        !example %in% basename(folders)) stop("Unknown example")
    selected <- basename(folders) == example
    folders <- folders[selected]; inputs <- inputs[selected]
  }
  for (i in seq_along(inputs)) {
    x <- bdynets::read_traffic(inputs[i])
    bdynets::traffic_report(x, file.path(folders[i], "report.html"))
    cat("Built ", basename(folders[i]), "/report.html: ", nrow(x$roads),
        " roads, ", nrow(x$events), " events, ", length(x$times), " frames\n", sep = "")
  }
}
if (sys.nframe() == 0L) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(script) != 1L) stop("Rscript entrypoint is required")
  if (!file.exists(script)) script <- gsub("~+~", " ", script, fixed = TRUE)
  build_examples(dirname(normalizePath(script, mustWork = TRUE)), example)
}
