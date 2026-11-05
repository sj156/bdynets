prepare_example <- function(base=".", source_file="data/source.rds", output_file="data/input.rds") {
  base <- normalizePath(base, mustWork=TRUE)
  source_path <- file.path(base, source_file)
  s <- readRDS(source_path)
  stopifnot(identical(s$schema, "metropolis-volume-frames-v1"))
  roads <- s$road_attributes
  ids <- vapply(s$segments, function(z) as.character(z$id), "")
  stopifnot(identical(ids, as.character(roads$id)))
  roads$road_id <- ids
  roads$geometry <- lapply(s$segments, function(z)
    rbind(c(z$lon1, z$lat1), c(z$lon2, z$lat2)))
  volume <- vapply(s$frames, function(z) as.numeric(z$volume), numeric(length(ids)))
  labels <- vapply(s$frames, function(z) z$time, "")
  stopifnot(identical(labels, s$settings$time_labels), ncol(volume) == 96L,
            all(is.na(volume) | (is.finite(volume) & volume >= 0)))
  flow <- data.frame(road_id=rep(ids, ncol(volume)),
    time=rep(s$settings$times, each=length(ids)), flow=as.vector(volume))
  events <- NULL
  s$settings$provenance$preparation_check <- list(source_segments=length(ids),
    source_frames=length(s$frames), road_frame_values=length(volume),
    added_temporal_aggregation=FALSE, physical_unit_verified=FALSE)
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
