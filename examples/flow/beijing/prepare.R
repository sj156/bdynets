prepare_example <- function(base=".", source_file="data/source.rds", output_file="data/input.rds") {
  base <- normalizePath(base, mustWork=TRUE)
  source_path <- file.path(base, source_file)
  s <- readRDS(source_path)
  stopifnot(identical(s$schema, "beijing-native-minute-v1"), s$day_no == 10,
            identical(s$minute, 0:1439), ncol(s$minute_rates) == 1440L,
            all(is.finite(s$minute_rates)), all(s$minute_rates >= 0))
  roads <- s$roads
  roads$road_id <- as.character(roads$edge_index)
  stopifnot(nrow(s$minute_rates) == length(s$road_pairs), !anyDuplicated(s$road_pairs))
  source_row <- match(roads$edge_id, s$road_pairs)
  stopifnot(!anyNA(source_row))
  aligned <- s$minute_rates[source_row, , drop=FALSE]
  starts <- seq.int(1L, 1440L, by=5L)
  rates <- Reduce(`+`, lapply(0:4, function(k) aligned[, starts+k, drop=FALSE])) / 5
  s$settings$times <- s$settings$times[1] + (starts-1L)*60
  s$settings$interval_seconds <- rep(300, length(starts))
  flow <- data.frame(road_id=rep(roads$road_id, ncol(rates)),
    time=rep(s$settings$times, each=nrow(roads)), flow=as.vector(rates))
  events <- NULL
  s$settings$provenance$preparation_check <- list(native_records=length(s$minute_rates), retained_minute_records=length(aligned),
    excluded_source_pairs=length(s$road_pairs)-nrow(roads), samples_per_frame=5L, complete_frames=length(starts), missing_native_values=0L)
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
