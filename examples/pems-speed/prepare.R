if (sys.nframe() == 0L) {
  pems_script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(pems_script) != 1L) stop("Rscript entrypoint is required.")
  if (!file.exists(pems_script)) pems_script <- gsub("~+~", " ", pems_script, fixed = TRUE)
} else {
  pems_script <- tail(Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile)), 1L)[[1L]]
}
pems_base <- dirname(normalizePath(pems_script, mustWork = TRUE))
source(file.path(pems_base, "scripts/read-source.R"), local = TRUE)

# Speed uses the V12 station average speed, in mph.
pems_la_speed <- function(positions, observations) {
  legacy <- positions$in_legacy
  stations <- unique(positions$station_id[legacy])
  mph <- pems_la_station_slots(observations, stations, "V12")
  observed <- is.finite(mph) & mph >= 1 & mph <= 100
  # The previous LA series stored single-precision values and rounded ties to even.
  single <- readBin(writeBin(mph[observed], raw(), size = 4L), "numeric", sum(observed), size = 4L)
  station_mph <- matrix(NA_integer_, nrow(mph), ncol(mph))
  station_mph[observed] <- as.integer(round(single))
  position_mph <- matrix(NA_integer_, nrow(mph), nrow(positions))
  position_mph[, legacy] <- station_mph[, match(positions$station_id[legacy], stations), drop = FALSE]
  list(position_mph = position_mph, position_station = positions$station_id, times = observations$clock)
}

pems_d04_speed <- function(observations, meta, station_map_file) {
  selection <- pems_d04_selection(meta, station_map_file)
  rows <- observations$records$detector04
  mph <- rows$V12
  mph[!is.finite(mph) | mph <= 0] <- NA_real_
  mph <- pems_station_slots(rows$V2, rows$V1, mph, selection$station_id, observations$labels)
  c(list(mph = mph, minute = observations$clock %% 86400 / 60), selection)
}

pems_speed_details <- function(native) {
  context <- pems_context(native)
  roads <- context$roads
  la <- native$la
  d04 <- native$d04
  hour_slots <- outer(context$times, (0:11) * 300L, `+`)
  if (!identical(as.numeric(la$times), pems_five_minute_clock(context$times))) {
    stop("The input hours must each cover twelve five-minute speed slots.")
  }
  slot_indices <- matrix(match(hour_slots, la$times), nrow = nrow(hour_slots))
  if (!is.matrix(la$position_mph) || !is.numeric(la$position_mph) ||
      nrow(la$position_mph) != length(la$times) ||
      ncol(la$position_mph) != length(la$position_station) || anyNA(la$position_station)) {
    stop("LA active-position speeds do not align with station IDs and five-minute slots.")
  }

  observed <- !is.na(la$position_mph)
  if (any(observed & (!is.finite(la$position_mph) | la$position_mph < 1 | la$position_mph > 100))) {
    stop("Invalid LA five-minute position speed.")
  }
  station_sums <- rowsum(t(replace(la$position_mph, !observed, 0L)), la$position_station, reorder = FALSE)
  station_counts <- rowsum(t(observed * 1L), la$position_station, reorder = FALSE)
  la_mph <- t(station_sums / station_counts)
  la_mph[t(station_counts == 0L)] <- NA_real_

  station_rows <- pems_d04_station_rows(d04, d04$mph, context$times)
  if (any(!is.na(d04$mph) & (!is.finite(d04$mph) | d04$mph <= 0))) {
    stop("Invalid D04 five-minute station speed.")
  }
  bay_mph <- t(d04$mph[station_rows, , drop = FALSE])
  from <- to <- matrix(NA_real_, length(la$times), nrow(roads))
  regions <- list("Los Angeles Area" = la_mph, "San Francisco Bay Area" = bay_mph)
  for (region in names(regions)) {
    use <- which(roads$region == region)
    stations <- regions[[region]]
    from[, use] <- stations[, match(roads$from_station[use], colnames(stations)), drop = FALSE]
    to[, use] <- stations[, match(roads$to_station[use], colnames(stations)), drop = FALSE]
  }
  road_mph <- pems_endpoint_mean(from, to)
  speed_by_hour <- lapply(seq_along(context$times), function(i) {
    observations <- road_mph[slot_indices[i, ], , drop = FALSE]
    valid_slots <- colSums(!is.na(observations))
    hourly_mph <- colMeans(observations, na.rm = TRUE)
    hourly_mph[valid_slots == 0L] <- NA_real_
    data.frame(road_id = roads$road_id, time = context$times[i], speed = hourly_mph,
      valid_five_minute_slots = valid_slots)
  })

  context$speed <- do.call(rbind, speed_by_hour)
  context$unit <- "mph"
  context$title <- "PeMS hourly speed | 2026-06-16"
  context$speed_label <- "Hourly mean speed"
  context$speed_definition <- "Mean of available five-minute endpoint-mean road speeds within each source hour."
  context$provenance <- c(context$provenance,
    list(d04_source_records = length(d04$mph), d04_available_records = sum(!is.na(d04$mph)),
      d04_retained_records = length(bay_mph),
      excluded_d04_stations = setdiff(d04$station_id, d04$matched_station_id),
      missing_link_hours = sum(is.na(context$speed$speed)),
      measurement = "Native five-minute V12 average speed in mph. Los Angeles retains the original position selection and rounding; Bay Area uses the source decimal values."))
  list(input = context, slots = slot_indices, from = from, to = to, road_mph = road_mph)
}

pems_speed_input <- function(native) pems_speed_details(native)$input

write_pems_speed <- function(native, input, output_dir, sources) {
  dictionary <- rbind(pems_roads_dictionary(),
    data.frame(table = "speed", field = "valid_five_minute_slots",
      meaning = "Five-minute slots with at least one observed endpoint speed in the hour."))
  write_traffic_input(input, output_dir, source_files = sources, native = native,
    dictionary = dictionary)
}

prepare_example <- function(base = ".", output_dir = file.path(base, "prepared"),
                            raw_root = file.path(pems_data_root(base), "raw", "caltrans"),
                            reference_root = file.path(pems_data_root(base), "raw", "reference"),
                            register_file = file.path(pems_data_root(base), "case-register.rds")) {
  files <- pems_files(raw_root, reference_root)
  pems_prepare_metric(output_dir, register_file, files, pems_speed_input, write_pems_speed)
}

if (sys.nframe() == 0L) pems_prepare_cli(pems_base, prepare_example)
rm(pems_script, pems_base)
