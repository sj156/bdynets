if (sys.nframe() == 0L) {
  pems_script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(pems_script) != 1L) stop("Rscript entrypoint is required.")
  if (!file.exists(pems_script)) pems_script <- gsub("~+~", " ", pems_script, fixed = TRUE)
} else {
  pems_script <- tail(Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile)), 1L)[[1L]]
}
pems_base <- dirname(normalizePath(pems_script, mustWork = TRUE))
source(file.path(pems_base, "scripts/read-source.R"), local = TRUE)

# Flow totals use the V10 whole count for each observed five-minute slot.
pems_five_minute_counts <- function(values, source = "PeMS observations") {
  counts <- suppressWarnings(as.numeric(values))
  if (any(!is.finite(counts) | counts < 0 | counts != trunc(counts))) {
    stop("Five-minute total flow must be a finite nonnegative whole count in ", source, ".")
  }
  counts
}

pems_counts <- function(records, station_ids, labels,
                        fields = c(time = "V1", station = "V2", count = "V10")) {
  if (!setequal(names(fields), c("time", "station", "count")) || anyNA(fields) ||
      anyDuplicated(fields) || any(!fields %in% names(records))) {
    stop("Map time, station and count to distinct source fields.")
  }
  pems_station_slots(records[[fields["station"]]], records[[fields["time"]]],
    pems_five_minute_counts(records[[fields["count"]]]), station_ids, labels)
}

pems_la_flow <- function(positions, observations) {
  stations <- unique(positions$station_id)
  counts <- pems_la_station_slots(observations, stations, "V10")
  counts <- counts[, match(positions$station_id, stations), drop = FALSE]
  hours <- nrow(counts) %/% 12L
  columns <- list(NULL, paste0("V", seq_len(nrow(positions))))
  flow <- matrix(NA_real_, hours, nrow(positions), dimnames = columns)
  coverage <- matrix(0L, hours, nrow(positions), dimnames = columns)
  for (i in seq_len(hours)) {
    slots <- counts[(i - 1L) * 12L + seq_len(12L), , drop = FALSE]
    coverage[i, ] <- as.integer(colSums(!is.na(slots)))
    available <- coverage[i, ] > 0L
    flow[i, available] <- colSums(slots[, available, drop = FALSE], na.rm = TRUE)
  }
  list(position_values = flow, position_station = positions$station_id,
    available_five_minute_slots = coverage)
}

pems_d04_counts <- function(observations, meta, station_map_file) {
  selection <- pems_d04_selection(meta, station_map_file)
  counts <- pems_counts(observations$records$detector04, selection$station_id, observations$labels)
  colnames(counts) <- paste0("V", seq_len(ncol(counts)))
  c(list(counts = counts, minute = observations$clock %% 86400 / 60), selection)
}

pems_flow_input <- function(native) {
  context <- pems_context(native)
  roads <- context$roads
  times <- context$times
  hours <- length(times)
  la <- native$la
  hourly <- la$position_values
  if (!is.matrix(hourly) || !is.numeric(hourly) || nrow(hourly) != hours ||
      ncol(hourly) != length(la$position_station) || anyNA(la$position_station)) {
    stop("LA active-position values do not align with station IDs and hours.")
  }
  coverage <- la$available_five_minute_slots
  if (!is.numeric(coverage) || !identical(dim(coverage), dim(hourly)) ||
      anyNA(coverage) || any(!coverage %in% 0:12) || any(is.na(hourly) != (coverage == 0L))) {
    stop("LA hourly position coverage must agree with the presence of the hourly count.")
  }
  position <- t(hourly)
  present <- !is.na(position)
  if (any(present & (!is.finite(position) | position < 0))) stop("Invalid LA hourly position value.")
  available <- rowsum(present * 1L, la$position_station, reorder = FALSE)
  la_hour <- rowsum(replace(position, !present, 0), la$position_station, reorder = FALSE) / available
  la_hour[available == 0L] <- NA_real_

  d04 <- native$d04
  station_row <- pems_d04_station_rows(d04, d04$counts, times)
  if (any(!is.finite(d04$counts) | d04$counts < 0 | d04$counts != trunc(d04$counts))) {
    stop("D04 source counts must be finite nonnegative whole counts.")
  }
  counts <- d04$counts[station_row, , drop = FALSE]
  first <- seq(1L, ncol(counts), by = 12L)
  bay_hour <- matrix(0, nrow(counts), hours, dimnames = list(d04$matched_station_id, NULL))
  for (offset in 0:11) bay_hour <- bay_hour + counts[, first + offset, drop = FALSE]
  count_before <- sum(counts)
  count_after <- sum(bay_hour)
  if (count_before != count_after) stop("D04 station-hour totals must conserve the retained counts.")

  values <- matrix(NA_real_, nrow(roads), hours)
  regions <- list("Los Angeles Area" = la_hour, "San Francisco Bay Area" = bay_hour)
  for (region in names(regions)) {
    use <- which(roads$region == region)
    stations <- regions[[region]]
    from <- stations[match(roads$from_station[use], rownames(stations)), , drop = FALSE]
    to <- stations[match(roads$to_station[use], rownames(stations)), , drop = FALSE]
    values[use, ] <- pems_endpoint_mean(from, to)
  }
  context$flow <- data.frame(road_id = rep(roads$road_id, hours),
    time = rep(times, each = nrow(roads)), flow = as.vector(values))
  context$title <- native$settings$title
  context$unit <- "vehicles/hour"
  context$provenance <- c(context$provenance,
    list(source_position_hours = length(position), d04_source_records = length(d04$counts),
      d04_retained_records = length(counts), d04_count_before = count_before,
      d04_count_after = count_after,
      excluded_d04_stations = setdiff(d04$station_id, d04$matched_station_id),
      missing_link_hours = sum(is.na(values)),
      measurement = "Hourly sums of native five-minute counts and available endpoint station means; LA keeps the original position coverage."))
  context
}

write_pems_flow <- function(native, input, output_dir, sources) {
  write_traffic_input(input, output_dir, source_files = sources, native = native,
    dictionary = pems_roads_dictionary())
}

prepare_example <- function(base = ".", output_dir = file.path(base, "prepared"),
                            raw_root = file.path(pems_data_root(base), "raw", "caltrans"),
                            reference_root = file.path(pems_data_root(base), "raw", "reference"),
                            register_file = file.path(pems_data_root(base), "case-register.rds")) {
  files <- pems_files(raw_root, reference_root)
  pems_prepare_metric(output_dir, register_file, files, pems_flow_input, write_pems_flow)
}

if (sys.nframe() == 0L) pems_prepare_cli(pems_base, prepare_example)
rm(pems_script, pems_base)
