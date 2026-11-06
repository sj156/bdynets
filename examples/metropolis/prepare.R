prepare_example <- function(base=".", source_file="data/source.rds", output_file="data/input.rds") {
  base <- normalizePath(base, mustWork=TRUE)
  source_path <- file.path(base, source_file)
  s <- bdynets::extract_traffic_data(source_path)$payload
  stopifnot(identical(s$schema, "metropolis-saved-metrics-v1"))
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
  measurements <- do.call(rbind, lapply(c("speed", "congestion"), function(key) {
    v <- vapply(s$frames, function(z) as.numeric(z[[key]]), numeric(length(ids)))
    data.frame(road_id=rep(ids,ncol(v)),time=rep(s$settings$times,each=length(ids)),
      metric=key,value=as.vector(v))
  }))
  metric_info <- list(
    flow=list(unit=s$settings$unit,definition="Saved volume array; the source builder averages three five-minute observations per frame."),
    speed=list(unit="source speed",label="Saved speed",definition="Saved speed array; source display labels speed as km/h, without a unit field in the JSON payload. No conversion is applied."),
    congestion=list(unit="dimensionless index",kind="index",label="Saved congestion index",
      title="City-traffic-M saved congestion index",
      definition=s$metadata$congestionDefinition,
      scale=list(kind="gradient",stops=c(0,0.52,1),palette=c("#00a651","#ffb000","#e02424")),
      display_source="Saved source map and traffic-metropolis.R reference: green/amber/red at 0, 0.52 and 1."))
  fields <- c(flow="meanVolume",speed="meanSpeed",congestion="congestionMean")
  definitions <- c(
    flow="Source meanVolume: saved arithmetic mean over all 720 segment volume values, rounded by the source builder to two decimals.",
    speed="Source meanSpeed: saved mean speed over segments with positive volume, rounded by the source builder to two decimals; zero when no segment is active.",
    congestion="Source congestionMean: saved arithmetic mean of the derived congestion index over all 720 segments, rounded by the source builder to three decimals.")
  trends <- do.call(rbind,lapply(names(fields),function(key) data.frame(
    time=s$settings$times,metric=key,value=vapply(s$frames,function(z)z[[fields[[key]]]],numeric(1)),
    definition=definitions[[key]],label=paste("Saved",fields[[key]]))))
  s$settings$provenance$preparation_check <- list(source_segments=length(ids),
    source_frames=length(s$frames), road_frame_values=length(volume),
    speed_values=sum(measurements$metric=="speed"),congestion_values=sum(measurements$metric=="congestion"),
    saved_trend_values=nrow(trends),added_temporal_aggregation=FALSE, physical_volume_unit_verified=FALSE)
  input <- c(list(schema="trafficmap-input-v1", roads=roads, flow=flow, events=NULL,
    measurements=measurements,metric_info=metric_info,trends=trends),s$settings)
  input$provenance$source_cache <- list(file=basename(source_file),
    bytes=unname(file.info(source_path)$size),md5=unname(tools::md5sum(source_path)),schema=s$schema)
  stopifnot(!anyDuplicated(roads$road_id),!anyDuplicated(flow[c("road_id","time")]),
    all(is.na(flow$flow)|(is.finite(flow$flow)&flow$flow>=0)))
  checked <- input;checked$schema <- NULL
  do.call(bdynets::traffic_data,checked)
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
