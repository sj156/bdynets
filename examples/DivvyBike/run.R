#!/usr/bin/env Rscript
render_example <- function(base, output_dir=NULL) {
  base <- normalizePath(base, mustWork=TRUE)
  required <- c("curl", "jsonlite", "rmarkdown", "knitr", "data.table", "ggplot2", "magrittr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
  if(length(missing)) stop("Missing R packages: ", paste(missing, collapse=", "))
  if(!rmarkdown::pandoc_available()) stop("Pandoc is unavailable")
  if("bdynets" %in% loadedNamespaces()) stop("A fresh R session is required")
  package_path <- normalizePath(file.path(base,"..","..","bdynets"),mustWork=TRUE)
  local_library <- tempfile("bdynets-library-");dir.create(local_library)
  on.exit(unlink(local_library,recursive=TRUE),add=TRUE)
  status <- system2(file.path(R.home("bin"),"R"),
    c("CMD","INSTALL",paste0("--library=",shQuote(local_library)),shQuote(package_path)))
  if(status!=0L) stop("Bundled package installation failed")
  .libPaths(c(local_library,.libPaths()))
  scratch <- tempfile("traffic-render-"); dir.create(scratch)
  on.exit(unlink(scratch, recursive=TRUE), add=TRUE)
  map_file <- NULL
  if(!is.null(output_dir)) {
    output_dir <- path.expand(output_dir)
    if(file.exists(output_dir)) stop("Output directory already exists")
    dir.create(output_dir,recursive=TRUE)
    map_file <- file.path(normalizePath(output_dir,mustWork=TRUE),"divvy_od_r2000_month_0000_0300.png")
  }
  rmarkdown::render(file.path(base, "divvy-od-report.Rmd"),
    output_file="divvy-od-report.html", output_dir=base,
    intermediates_dir=scratch, knit_root_dir=base,
    params=list(map_file=map_file,figure_dir=scratch,work_dir=scratch),
    envir=new.env(parent=globalenv()), clean=TRUE, quiet=TRUE)
  cat("Built divvy-od-report.html\n")
}
if(sys.nframe()==0L) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE))
  if(length(script)!=1L) stop("Rscript entrypoint is required")
  if(!file.exists(script)) script <- gsub("~+~", " ", script, fixed=TRUE)
  args <- commandArgs(TRUE)
  if(length(args)>1L) stop("At most one new output directory is accepted")
  render_example(dirname(normalizePath(script, mustWork=TRUE)),
    output_dir=if(length(args))args[1] else NULL)
}
