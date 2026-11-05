prepare_example <- function(base=".", source_file="data/source.rds", output_file="data/input.rds") {
  base <- normalizePath(base, mustWork=TRUE)
  source_path <- file.path(base, source_file)
  s <- readRDS(source_path)
  stopifnot(identical(s$schema, "synthetic-records-v1"))
  roads <- s$roads; flow <- s$flow; events <- s$events
  roads$road_id <- as.character(roads$road_id)
  flow$road_id <- as.character(flow$road_id)
  s$settings$provenance$preparation_check <- list(supplied_flow_rows=nrow(flow),
    omitted_pairs=nrow(roads)*length(s$settings$times)-nrow(flow),
    explicit_missing=sum(is.na(flow$flow)))
  input <- c(list(schema="trafficmap-input-v1", roads=roads, flow=flow, events=events), s$settings)
  input$provenance$source_cache <- list(file=basename(source_file),
    bytes=unname(file.info(source_path)$size), md5=unname(tools::md5sum(source_path)), schema=s$schema)
  stopifnot(!anyDuplicated(roads$road_id), all(flow$road_id %in% roads$road_id),
            !anyDuplicated(flow[c("road_id", "time")]),
            all(is.na(flow$flow) | (is.finite(flow$flow) & flow$flow >= 0)))
  bdynets::traffic_data(roads=roads, flow=flow, events=events,
    times=input$times, time_labels=input$time_labels, interval_seconds=input$interval_seconds,
    unit=if(is.null(input$unit)) "unit unspecified" else input$unit,
    basemap=input$basemap)
  output <- file.path(base, output_file)
  dir.create(dirname(output), recursive=TRUE, showWarnings=FALSE)
  saveRDS(input, output, compress="xz", version=3)
  invisible(output)
}
if(sys.nframe()==0L) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE))
  if(length(script)!=1L) stop("Rscript entrypoint is required")
  if(!file.exists(script)) script <- gsub("~+~", " ", script, fixed=TRUE)
  prepare_example(dirname(normalizePath(script, mustWork=TRUE)))
  cat("Prepared data/input.rds\n")
}
