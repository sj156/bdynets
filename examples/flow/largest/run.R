#!/usr/bin/env Rscript
render_example <- function(base, time_label="08:00", export=FALSE, export_dir="output") {
  base <- normalizePath(base, mustWork=TRUE)
  required <- c("bdynets", "jsonlite", "rmarkdown", "knitr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
  if(length(missing)) stop("Missing R packages: ", paste(missing, collapse=", "))
  if(!rmarkdown::pandoc_available()) stop("Pandoc is unavailable")
  scratch <- tempfile("traffic-render-"); dir.create(scratch)
  on.exit(unlink(scratch, recursive=TRUE), add=TRUE)
  rmarkdown::render(file.path(base, "largest-traffic-maps.Rmd"),
    output_file="largest-traffic-maps.html", output_dir=base,
    intermediates_dir=scratch, knit_root_dir=base,
    params=list(time_label=time_label, figure_dir=scratch, export=export, export_dir=export_dir),
    envir=new.env(parent=globalenv()), clean=TRUE, quiet=TRUE)
  cat("Built largest-traffic-maps.html\n")
}
if(sys.nframe()==0L) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE))
  if(length(script)!=1L) stop("Rscript entrypoint is required")
  if(!file.exists(script)) script <- gsub("~+~", " ", script, fixed=TRUE)
  args <- commandArgs(TRUE)
  if(length(args)>2L || (length(args)==2L && args[2]!="--export")) stop("Arguments: optional HH:MM and --export")
  render_example(dirname(normalizePath(script, mustWork=TRUE)),
    if(length(args)) args[1] else "08:00", export=length(args)==2L)
}
