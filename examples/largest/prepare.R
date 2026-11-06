prepare_example <- function(base=".", source_file="data/source.rds", output_file="data/input.rds") {
  base <- normalizePath(base, mustWork=TRUE)
  source_path <- file.path(base, source_file)
  s <- bdynets::extract_traffic_data(source_path)$payload
  stopifnot(identical(s$schema, "pems-station-events-v1"))
  roads <- s$roads; roads$road_id <- as.character(roads$road_id)
  roads$road_class <- rep("motorway", nrow(roads))
  times <- s$settings$times
  stopifnot(length(times) == 24L, !anyDuplicated(s$d04$station_id),
            identical(as.numeric(s$d04$minute), seq(0, 1435, 5)))
  v <- t(s$la$position_values)
  stopifnot(nrow(v) == length(s$la$position_station), ncol(v) == 24L)
  v[v == s$la$missing_value] <- NA_real_
  sums <- rowsum(replace(v, is.na(v), 0), s$la$position_station, reorder=FALSE)
  counts <- rowsum(1L * !is.na(v), s$la$position_station, reorder=FALSE)
  la_hour <- sums / counts
  la_hour[counts == 0] <- NA_real_
  stopifnot(nrow(s$d04$counts) == length(s$d04$station_id),
            !anyDuplicated(s$d04$matched_station_id))
  matched <- match(s$d04$matched_station_id, s$d04$station_id)
  stopifnot(!anyNA(matched))
  native <- s$d04$counts[matched, , drop=FALSE]
  stopifnot(nrow(native) == length(s$d04$matched_station_id), ncol(native) == 288L,
            all(is.finite(native)), all(native >= 0))
  starts <- seq.int(1L, 288L, by=12L)
  d04_hour <- Reduce(`+`, lapply(0:11, function(k) native[, starts+k, drop=FALSE]))
  rownames(d04_hour) <- s$d04$matched_station_id
  endpoint_means <- function(station_hour, selected_roads) {
    a <- station_hour[match(selected_roads$from_station, rownames(station_hour)), , drop=FALSE]
    b <- station_hour[match(selected_roads$to_station, rownames(station_hour)), , drop=FALSE]
    available <- (!is.na(a)) + (!is.na(b))
    result <- (replace(a, is.na(a), 0) + replace(b, is.na(b), 0)) / available
    result[available == 0] <- NA_real_
    t(result)
  }
  values <- matrix(NA_real_, nrow=24, ncol=nrow(roads))
  la <- which(roads$region == "Los Angeles Area")
  bay <- which(roads$region == "San Francisco Bay Area")
  stopifnot(length(la)+length(bay) == nrow(roads))
  values[, la] <- endpoint_means(la_hour, roads[la, ])
  values[, bay] <- endpoint_means(d04_hour, roads[bay, ])
  flow <- data.frame(road_id=rep(roads$road_id, 24),
    time=rep(times, each=nrow(roads)), flow=as.vector(t(values)))
  raw <- s$event_summaries
  stopifnot(!anyDuplicated(raw$id), all(substr(raw$reported_time, 1, 10) == "2026-06-16"))
  region <- ifelse(raw$district == "04", "San Francisco Bay Area", "Los Angeles Area")
  stopifnot(all(raw$district %in% c("04", "07", "08", "12")))
  scope <- s$scopes[match(region, s$scopes$region), ]
  lon <- as.numeric(raw$lon); lat <- as.numeric(raw$lat)
  keep <- is.finite(lon) & is.finite(lat) & lon >= scope$west & lon <= scope$east &
          lat >= scope$south & lat <= scope$north
  raw <- raw[keep, , drop=FALSE]; region <- region[keep]
  second <- as.integer(substr(raw$reported_time, 12, 13))*3600 +
            as.integer(substr(raw$reported_time, 15, 16))*60 +
            as.integer(substr(raw$reported_time, 18, 19))
  type <- tolower(trimws(gsub("[[:space:]]+", " ", raw$source_type)))
  lookup <- tolower(trimws(s$event_categories$source_type))
  category <- s$event_categories$category[match(type, lookup)]
  category[is.na(category)] <- "Other alert"
  events <- data.frame(id=raw$id, time=times[1]+second, lon=as.numeric(raw$lon),
    lat=as.numeric(raw$lat), category=category, description=raw$description,
    time_label=raw$reported_time, region=region, stringsAsFactors=FALSE)
  s$settings$provenance$preparation_check <- list(la_position_hours=length(s$la$position_values),
    d04_source_records=length(s$d04$counts), d04_retained_records=length(native),
    excluded_source_stations=length(s$d04$station_id)-length(matched), d04_samples_per_station_hour=12L,
    source_event_summaries=nrow(s$event_summaries), included_event_starts=nrow(events),
    excluded_by_geographic_scope=sum(!keep), missing_link_hours=sum(is.na(values)))
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
