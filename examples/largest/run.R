#!/usr/bin/env Rscript
# Modes: interactive HTML build (zero arguments), figure export (two arguments).
script <- sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
if(length(script)!=1L) stop('Rscript entrypoint required')
if (!file.exists(script)) script <- gsub('~+~', ' ', script, fixed=TRUE)
base <- dirname(normalizePath(script,mustWork=TRUE))
args <- commandArgs(TRUE)
if(!length(args) %in% c(0L,2L)) stop('Usage: Rscript run.R [snapshot.json.gz new-output-directory]')
# Argument paths are relative to the calling directory.
if(length(args)) {
  input <- normalizePath(args[1],mustWork=TRUE)
  output <- path.expand(args[2])
  if(!grepl('^/',output)) output <- file.path(getwd(),output)
  if(file.exists(output)) stop('Output directory already exists')
}
setwd(base)
for(pkg in c('jsonlite','rmarkdown','knitr'))
  if(!requireNamespace(pkg,quietly=TRUE)) stop('Missing R package: ',pkg)
if(length(args)) {
  source('pems-flow.R')
  pems_flow_figures(input,output_dir=output)
  cat('Figure export completed\n')
} else {
  if(!rmarkdown::pandoc_available()) stop('Pandoc is unavailable')
  # The temporary report is embedded as the map's documentation panel.
  build_directory <- tempfile('traffic-documentation-')
  dir.create(build_directory)
  report <- rmarkdown::render('largest-traffic-maps.Rmd',
    output_file='documentation.html', output_dir=build_directory,
    intermediates_dir=build_directory, knit_root_dir=base,
    envir=new.env(parent=globalenv()), quiet=TRUE)
  read_text <- function(path, compressed=FALSE) {
    connection <- if(compressed) gzfile(path, 'rt', encoding='UTF-8') else
      file(path, 'rt', encoding='UTF-8')
    on.exit(close(connection))
    paste(readLines(connection, warn=FALSE), collapse='\n')
  }
  # Escaped less-than signs prevent embedded values from closing a script tag.
  script_json <- function(path) {
    value <- read_text(path, compressed=TRUE)
    stopifnot(jsonlite::validate(value))
    gsub('<', paste0(intToUtf8(92),'u003c'), value, fixed=TRUE)
  }
  replace_marker <- function(text, marker, value) {
    parts <- strsplit(text, marker, fixed=TRUE)[[1]]
    if(length(parts)!=2L) stop('Template marker count differs from one: ',marker)
    paste0(parts[1], value, parts[2])
  }
  html <- read_text('map-template.html')
  html <- replace_marker(html, '@@TRAFFIC_JSON@@', script_json('data/pems-flow.json.gz'))
  html <- replace_marker(html, '@@BASEMAP_JSON@@', script_json('data/pems-osm.json.gz'))
  report_text <- read_text(report)
  # Documentation is static; map interaction uses the outer document only.
  report_text <- gsub('(?s)<script\\b[^>]*>.*?</script>', '', report_text, perl=TRUE)
  documentation <- jsonlite::base64_enc(charToRaw(enc2utf8(report_text)))
  html <- replace_marker(html, '@@DOCUMENTATION_BASE64@@', documentation)
  stopifnot(!grepl('@@[A-Z_]+@@', html))
  output_file <- 'largest-traffic-maps.html'
  connection <- file(output_file, 'wt', encoding='UTF-8')
  tryCatch(writeLines(html,connection,useBytes=TRUE),finally=close(connection))
  unlink(build_directory, recursive=TRUE)
  cat('Built largest-traffic-maps.html\n')
}
