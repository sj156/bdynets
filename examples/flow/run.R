#!/usr/bin/env Rscript
example <- NULL
time_label <- "08:00"
export <- FALSE
export_dir <- "output"

build_examples <- function(root, example=NULL, time_label="08:00", export=FALSE, export_dir="output") {
  root <- normalizePath(root, mustWork=TRUE)
  required <- c("jsonlite", "knitr", "rmarkdown")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
  if(length(missing)) stop("Missing R packages: ", paste(missing, collapse=", "))
  if(!rmarkdown::pandoc_available()) stop("Pandoc is unavailable")
  if("bdynets" %in% loadedNamespaces()) stop("A fresh R session is required")
  local_library <- tempfile("bdynets-library-"); dir.create(local_library)
  on.exit(unlink(local_library, recursive=TRUE), add=TRUE)
  status <- system2(file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", paste0("--library=", shQuote(local_library)),
      shQuote(file.path(root, "..", "..", "bdynets"))))
  if(status != 0L) stop("Package installation failed")
  .libPaths(c(local_library, .libPaths()))
  library(bdynets, lib.loc=local_library)
  folders <- list.dirs(root, recursive=FALSE, full.names=TRUE)
  keep <- (file.exists(file.path(folders, "data", "source.rds")) |
           file.exists(file.path(folders, "data", "input.rds"))) &
          file.exists(file.path(folders, "run.R"))
  folders <- folders[keep]
  if(!is.null(example)) {
    if(length(example)!=1L || !is.character(example) || is.na(example) ||
       !example %in% basename(folders)) stop("Unknown example")
    folders <- folders[basename(folders)==example]
  }
  if(!length(folders)) stop("No executable cases found")
  for(folder in folders) {
    runner <- new.env(parent=globalenv())
    source(file.path(folder, "run.R"), local=runner)
    runner$render_example(folder, time_label=time_label, export=export, export_dir=export_dir)
  }
}
if(sys.nframe()==0L) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE))
  if(length(script)!=1L) stop("Rscript entrypoint is required")
  if(!file.exists(script)) script <- gsub("~+~", " ", script, fixed=TRUE)
  args <- commandArgs(TRUE)
  if(length(args)>1L) stop("At most one case name is accepted")
  build_examples(dirname(normalizePath(script, mustWork=TRUE)),
    if(length(args)) args[1] else example, time_label=time_label, export=export, export_dir=export_dir)
}
