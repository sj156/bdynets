# PeMS daily sample reading, road geometry and CHP context.
pems_case_times <- function(times) {
  midnight <- as.numeric(as.POSIXct("2026-06-16", tz = "UTC"))
  if (!is.numeric(times) || !identical(as.numeric(times), midnight + (0:23) * 3600)) {
    stop("The PeMS case requires all 24 consecutive source-hour starts on 2026-06-16.")
  }
}

pems_event_scope <- function(events, scopes) {
  regions <- c("Los Angeles Area", "San Francisco Bay Area")
  bounds <- c("west", "east", "south", "north")
  if (!is.data.frame(scopes) || !all(c("region", bounds) %in% names(scopes)) ||
      nrow(scopes) != length(regions) || !setequal(scopes$region, regions) ||
      !all(vapply(scopes[bounds], is.numeric, logical(1L))) ||
      any(!is.finite(as.matrix(scopes[bounds]))) ||
      any(scopes$west < -180 | scopes$east > 180 | scopes$west >= scopes$east |
          scopes$south < -90 | scopes$north > 90 | scopes$south >= scopes$north)) {
    stop("Each PeMS region needs one set of finite regional bounds in longitude and latitude.")
  }
  if (any(!events$district %in% c("04", "07", "08", "12"))) {
    stop("An incident district has no declared regional scope.")
  }
  region <- ifelse(events$district == "04", regions[2L], regions[1L])
  at <- match(region, scopes$region)
  longitude <- suppressWarnings(as.numeric(events$lon))
  latitude <- suppressWarnings(as.numeric(events$lat))
  valid <- is.finite(longitude) & is.finite(latitude) & abs(longitude) <= 180 & abs(latitude) <= 90
  inside <- valid & longitude >= scopes$west[at] & longitude <= scopes$east[at] &
    latitude >= scopes$south[at] & latitude <= scopes$north[at]
  list(inside = inside, outside = valid & !inside, invalid = !valid)
}

pems_scope_class <- function(events, scopes) {
  scope <- pems_event_scope(events, scopes)
  ifelse(scope$inside, "inside", ifelse(scope$invalid, "invalid", "outside"))
}

pems_chp_clock <- function(values) {
  text <- as.character(values)
  source_format <- "%m/%d/%Y %H:%M:%S"
  if (anyNA(text) || any(!grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}$", text))) {
    stop("CHP start times must use the complete source clock.")
  }
  clock <- as.POSIXct(strptime(text, source_format, tz = "UTC"))
  if (anyNA(clock) || any(format(clock, source_format, tz = "UTC") != text)) {
    stop("Invalid CHP source-clock timestamp.")
  }
  clock
}

pems_summary_records <- function(rows) {
  required <- c("V1", "V4", "V5", "V6", "V7", "V10", "V11", "V12")
  if (!all(required %in% names(rows))) stop("The CHP summary fields are incomplete.")
  clock <- pems_chp_clock(rows$V4)
  clean <- function(x) trimws(gsub("[[:space:]]+", " ", as.character(x)))
  data.frame(id = as.character(rows$V1), reported_time = format(clock, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    district = sprintf("%02d", as.integer(rows$V12)), lon = as.character(rows$V11),
    lat = as.character(rows$V10), source_type = clean(rows$V5), location = clean(rows$V6),
    area = clean(rows$V7), stringsAsFactors = FALSE)
}

pems_map_config <- function(file) {
  literal <- grep("^const config = ", readLines(file, warn = FALSE), value = TRUE)
  if (length(literal) != 1L) stop("Expected one source-map configuration in ", basename(file))
  jsonlite::fromJSON(sub(";[[:space:]]*$", "", sub("^const config = ", "", literal)))
}

pems_positions <- function(manifest, config, month_key) {
  months <- jsonlite::read_json(manifest, simplifyVector = FALSE)
  month <- Filter(function(x) identical(x$key, month_key), months)
  if (length(month) != 1L) stop("Expected one manifest entry for ", month_key)
  positions <- month[[1L]][["segmentPositions"]]
  stations <- config[["positionStations"]]
  valid_position <- function(index) {
    is.numeric(index) && length(index) == 1L && is.finite(index) && index == trunc(index) &&
      index >= 0 && index < length(stations)
  }
  if (!is.list(positions) || !is.null(names(positions)) || !length(positions) ||
      !all(vapply(positions, valid_position, logical(1L)))) {
    stop("Active positions and station mapping disagree.")
  }
  positions <- unlist(positions, use.names = FALSE)
  selected <- stations[positions + 1L]
  valid_station <- function(id) {
    (is.character(id) || is.numeric(id)) && length(id) == 1L && !is.na(id) && grepl("^[0-9]+$", id)
  }
  if (anyDuplicated(positions) || !all(vapply(selected, valid_station, logical(1L)))) {
    stop("Active positions and station mapping disagree.")
  }
  data.frame(segment_position = positions,
    station_id = vapply(selected, as.character, character(1L), USE.NAMES = FALSE))
}

pems_case_roads <- function(register, config, meta) {
  la <- config$links
  reverse <- la$id %in% register$la_reverse_id
  if (anyDuplicated(la$id) || any(!register$la_reverse_id %in% la$id)) {
    stop("LA display IDs differ from the direction register.")
  }
  la$geometry <- lapply(seq_len(nrow(la)), function(i) {
    geometry <- la$coords[[i]][, 2:1, drop = FALSE]
    if (reverse[i]) geometry[nrow(geometry):1, , drop = FALSE] else geometry
  })
  la$coords <- NULL
  for (key in c("station", "abs_pm", "station_index")) {
    from <- paste0("from_", key)
    to <- paste0("to_", key)
    saved <- la[[from]][reverse]
    la[[from]][reverse] <- la[[to]][reverse]
    la[[to]][reverse] <- saved
  }
  la$region <- "Los Angeles Area"
  la$road_id <- as.character(la$id)

  keys <- register$d04_links
  from <- meta[match(keys$from_station, meta$ID), ]
  to <- meta[match(keys$to_station, meta$ID), ]
  if (anyNA(from$ID) || anyNA(to$ID) ||
      any(from$Fwy != to$Fwy | from$Dir != to$Dir | from$District != "4" |
          to$District != "4" | from$Type != "ML" | to$Type != "ML")) {
    stop("D04 link endpoints do not agree with station metadata.")
  }
  bay <- data.frame(district = "04", freeway = from$Fwy, direction = from$Dir,
    from_station = from$ID, to_station = to$ID, from_abs_pm = as.numeric(from$Abs_PM),
    to_abs_pm = as.numeric(to$Abs_PM), id = seq_len(nrow(keys)) - 1L,
    from_station_index = NA_integer_, to_station_index = NA_integer_,
    region = "San Francisco Bay Area", road_id = keys$road_id)
  bay$geometry <- lapply(seq_len(nrow(keys)), function(i) {
    cbind(as.numeric(c(from$Longitude[i], to$Longitude[i])),
      as.numeric(c(from$Latitude[i], to$Latitude[i])))
  })
  rbind(la[names(bay)], bay)
}

pems_incidents <- function(summary, files, scopes) {
  events <- pems_summary_records(summary)
  scope <- pems_scope_class(events, scopes)
  event_order <- order(scope != "inside", events$district == "04", events$reported_time, events$id)
  events <- events[event_order, ]
  events$scope <- scope[event_order]
  rownames(events) <- NULL

  connection <- file(files["details"], "rt")
  on.exit(close(connection), add = TRUE)
  message_keys <- character()
  repeat {
    lines <- readLines(connection, n = 50000L, warn = FALSE)
    if (!length(lines)) break
    lines <- grep("^[0-9]+,[0-9]+,[0-9]{2}/[0-9]{2}/[0-9]{4} ", lines, value = TRUE, useBytes = TRUE)
    lines <- lines[sub(",.*", "", lines, useBytes = TRUE) %in% events$id]
    message_keys <- c(message_keys, sub("^([0-9]+,[0-9]+),.*", "\\1", lines, useBytes = TRUE))
  }
  if (anyDuplicated(message_keys)) stop("Incident detail message IDs are duplicated.")
  nmsg <- tabulate(match(sub(",.*", "", message_keys), events$id), nbins = nrow(events))

  detail <- events$area
  names_needed <- c("Incident ID", "Incident detail summary", "Raw message count")
  header <- names(read.csv(files["annotations"], nrows = 0, check.names = FALSE))
  if (!all(names_needed %in% header)) stop("English annotations lack required ID and message fields.")
  english <- read.csv(files["annotations"], check.names = FALSE, na.strings = NULL,
    colClasses = ifelse(header %in% names_needed, "character", "NULL"))
  use <- events$district != "04" & events$scope == "inside"
  annotations <- match(events$id[use], english[["Incident ID"]])
  message_counts <- suppressWarnings(as.integer(english[["Raw message count"]][annotations]))
  if (anyNA(annotations) || anyDuplicated(english[["Incident ID"]]) ||
      anyNA(message_counts) || any(message_counts != nmsg[use])) {
    stop("English annotations do not align with raw incident message counts.")
  }
  detail[use] <- english[["Incident detail summary"]][annotations]
  description <- function(type, place, text) paste(Filter(nzchar, c(type, place, text)), collapse = " | ")
  events$description <- mapply(description, events$source_type, events$location, detail, USE.NAMES = FALSE)
  list(summaries = events, details = list(records = sum(nmsg), events_with_detail = sum(nmsg > 0L)))
}

pems_data_root <- function(base) file.path(base, "data")

pems_files <- function(raw_root, reference_root) {
  map <- file.path(reference_root, "PeMS_2020_2026_maps", "map")
  chp <- file.path(raw_root, "chp", "2026", "2026_06")
  districts <- c("04", "07", "08", "12")
  detectors <- setNames(file.path(raw_root, "5min", paste0("D", districts),
    "2026", "2026_06", paste0("d", districts, "_text_station_5min_2026_06_16.txt")),
    paste0("detector", districts))
  shared <- c(detectors,
    metadata = file.path(raw_root, "metadata", "D04", "d04_text_meta_2026_04_08.txt"),
    incidents = file.path(chp, "all_text_chp_incidents_month_2026_06.txt"),
    details = file.path(chp, "all_text_chp_incident_det_month_2026_06.txt"),
    station_map = file.path(reference_root, "processing_runtime", "clean_data_update", "mapping_cache",
      "2026-06_d04", "station_maps", "snapshot_d04_2026-04-08.csv"),
    flow_geometry = file.path(map, "la_area_freeway_volume_2020_01_to_2026_06.html"),
    annotations = file.path(reference_root, "CHP_full_English_2026_10_21", "all_months",
      ".work", "2026-06", "events.csv"))
  c(shared, flow_manifest = file.path(map, "data", "flow", "manifest.json"))
}

pems_selection <- function(register_file, files) {
  register <- readRDS(register_file)
  if (!identical(register$schema, "pems-links-and-rules-v1")) {
    stop("The selected display links need a compatible endpoint register.")
  }
  pems_case_times(register$settings$times)
  month <- format(as.POSIXct(register$settings$times[1L], origin = "1970-01-01", tz = "UTC"),
    "%Y_%m", tz = "UTC")
  # The original volume map holds the shared LA link geometry.
  config <- pems_map_config(files["flow_geometry"])
  metadata <- read.delim(files["metadata"], colClasses = "character", quote = "",
    na.strings = NULL, check.names = FALSE, fileEncoding = "latin1")
  if (anyDuplicated(metadata$ID)) stop("Metadata station IDs are duplicated.")
  result <- list(register = register, config = config, metadata = metadata)
  result$flow <- pems_positions(files["flow_manifest"], config, month)
  result
}

pems_detector_rows <- function(file, district, labels) {
  columns <- rep("NULL", 52L)
  columns[c(1L, 2L, 3L, 6L)] <- c("character", "character", "integer", "character")
  columns[10L] <- "character"
  rows <- read.csv(file, header = FALSE, colClasses = columns, quote = "")
  if (any(!rows$V1 %in% labels)) {
    stop("Invalid daily five-minute source-clock timestamp in ", basename(file), ".")
  }
  if (anyNA(rows$V3) || any(rows$V3 != district)) {
    stop("The supplied detector text contains records from a different district: ", basename(file))
  }
  mainline <- rows[rows$V6 %in% "ML", , drop = FALSE]
  mainline$V10 <- pems_five_minute_counts(mainline$V10, basename(file))
  mainline
}

pems_five_minute_clock <- function(times) {
  rep(times, each = 12L) + rep(seq(0, 3300, 300), length(times))
}

pems_observations <- function(files, times) {
  pems_case_times(times)
  clock <- pems_five_minute_clock(times)
  labels <- format(as.POSIXct(clock, origin = "1970-01-01", tz = "UTC"), "%m/%d/%Y %H:%M:%S")
  districts <- c(4L, 7L, 8L, 12L)
  detector_names <- paste0("detector", sprintf("%02d", districts))
  records <- Map(pems_detector_rows, unname(files[detector_names]), districts,
    MoreArgs = list(labels = labels))
  names(records) <- detector_names
  columns <- rep("NULL", 20L)
  columns[c(1L, 4:7, 10:12)] <- "character"
  incidents <- read.csv(files["incidents"], header = FALSE, colClasses = columns, na.strings = NULL)
  starts <- as.numeric(pems_chp_clock(incidents$V4))
  if (any(starts < times[1L] | starts >= times[1L] + 86400)) {
    stop("CHP summary starts must all be on the supplied day.")
  }
  if (any(!incidents$V12 %in% c("4", "7", "8", "12"))) {
    stop("CHP summaries must belong to districts 04, 07, 08 or 12.")
  }
  list(clock = clock, labels = labels, records = records, incidents = incidents)
}

pems_la_station_slots <- function(observations, stations, field) {
  rows <- do.call(rbind, observations$records[c("detector07", "detector08", "detector12")])
  rows <- rows[rows$V2 %in% stations, , drop = FALSE]
  row <- match(rows$V1, observations$labels)
  column <- match(rows$V2, stations)
  slots <- length(observations$clock)
  cell <- row + (column - 1L) * slots
  if (anyNA(cell)) stop("Unknown LA detector or five-minute slot.")
  if (anyDuplicated(cell)) stop("Duplicate LA detector and five-minute slot.")
  values <- matrix(NA_real_, slots, length(stations))
  values[cell] <- rows[[field]]
  values
}

pems_d04_selection <- function(meta, station_map_file) {
  ids <- sort(meta$ID[meta$District == "4" & meta$Type == "ML"])
  station_map <- read.csv(station_map_file, colClasses = "character")
  mapped <- station_map[["station_id"]]
  if (!length(mapped) || anyDuplicated(mapped) || any(!mapped %in% ids)) {
    stop("D04 station mapping does not align with ML metadata.")
  }
  list(station_id = ids, matched_station_id = ids[ids %in% mapped])
}

pems_station_slots <- function(stations, clocks, values, station_ids, labels) {
  if (!length(station_ids) || anyNA(station_ids) || anyDuplicated(station_ids) ||
      !length(labels) || anyNA(labels) || anyDuplicated(labels)) {
    stop("Declare unique stations and source-clock slots.")
  }
  row <- match(as.character(stations), station_ids)
  column <- match(as.character(clocks), labels)
  if (anyNA(row) || anyNA(column)) stop("An unexpected source station or timestamp is present.")
  cell <- row + (column - 1L) * length(station_ids)
  if (anyDuplicated(cell)) stop("Duplicate station and timestamp.")
  if (length(cell) != length(station_ids) * length(labels)) {
    stop("Missing station and timestamp in D04 detector records.")
  }
  result <- matrix(NA_real_, length(station_ids), length(labels), dimnames = list(station_ids, labels))
  result[cell] <- values
  result
}

pems_d04_station_rows <- function(d04, values, times) {
  if (!is.matrix(values) || !is.numeric(values) ||
      anyNA(d04$station_id) || anyDuplicated(d04$station_id) ||
      anyNA(d04$matched_station_id) || anyDuplicated(d04$matched_station_id) ||
      !identical(rownames(values), d04$station_id) ||
      ncol(values) != 12L * length(times) ||
      !identical(as.numeric(d04$minute), pems_five_minute_clock(times) %% 86400 / 60)) {
    stop("D04 source minutes and output hours do not align.")
  }
  row <- match(d04$matched_station_id, d04$station_id)
  if (anyNA(row)) stop("A matched D04 station is absent from its source observations.")
  row
}

pems_native <- function(stages, observations, files) {
  register <- stages$register
  roads <- pems_case_roads(register, stages$config, stages$metadata)
  incidents <- pems_incidents(observations$incidents, files, register$scopes)
  la <- pems_la_flow(stages$flow, observations)
  d04 <- pems_d04_counts(observations, stages$metadata, files["station_map"])
  list(schema = "pems-flow-source-v1", roads = roads, la = la, d04 = d04,
    event_summaries = incidents$summaries, event_categories = register$event_categories,
    scopes = register$scopes, settings = register$settings, register_origin = register$origin,
    source_files = data.frame(file = basename(files), bytes = unname(file.info(files)$size),
      role = names(files), row.names = NULL), incident_details = incidents$details)
}

read_pems_sample <- function(register_file, files) {
  if (any(!file.exists(files))) {
    stop("Required PeMS files are missing: ", paste(names(files)[!file.exists(files)], collapse = ", "))
  }
  selection <- pems_selection(register_file, files)
  observations <- pems_observations(files, selection$register$settings$times)
  pems_native(selection, observations, files)
}

pems_context <- function(native) {
  if (!identical(native$schema, "pems-flow-source-v1")) {
    stop("Expected native PeMS flow observations and CHP summaries.")
  }
  roads <- native$roads
  roads$road_id <- as.character(roads$road_id)
  times <- native$settings$times
  pems_case_times(times)
  if (anyDuplicated(roads$road_id)) stop("Unique road IDs are required.")
  if (any(!roads$region %in% c("Los Angeles Area", "San Francisco Bay Area"))) {
    stop("A road has no declared station region.")
  }

  source <- native$event_summaries
  if (anyNA(source$id) || anyDuplicated(source$id) ||
      any(!grepl("^2026-06-16 ([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$", source$reported_time))) {
    stop("CHP IDs and reported start clocks must identify the selected source day.")
  }
  scope <- source$scope
  if (is.null(scope)) scope <- pems_scope_class(source, native$scopes)
  shown <- source[scope == "inside", , drop = FALSE]
  # A numeric encoding of the source clock, without assigning a source time zone.
  event_clock <- as.numeric(as.POSIXct(shown$reported_time, tz = "UTC"))
  key <- tolower(trimws(gsub("[[:space:]]+", " ", shown$source_type)))
  categories <- native$event_categories
  category <- categories$category[match(key, tolower(trimws(categories$source_type)))]
  category[is.na(category)] <- "Other alert"
  events <- data.frame(id = shown$id, time = event_clock, lon = as.numeric(shown$lon),
    lat = as.numeric(shown$lat), category = category, description = shown$description,
    region = ifelse(shown$district == "04", "San Francisco Bay Area", "Los Angeles Area"),
    stringsAsFactors = FALSE)
  list(crs = "EPSG:4326", roads = roads, events = events, times = times,
    time_label_format = "%Y-%m-%d %H:%M", interval_seconds = rep(3600, length(times)),
    provenance = list(source_dataset = "PeMS and CHP", day = "2026-06-16",
      clock = "Supplied source wall clock; numeric encoding does not establish a UTC offset.",
      register_origin = native$register_origin, source_files = native$source_files,
      source_incidents = nrow(source), included_incidents = nrow(events),
      outside_scope = sum(scope == "outside"), invalid_coordinates = sum(scope == "invalid")))
}

pems_endpoint_mean <- function(from, to) {
  available <- (!is.na(from)) + (!is.na(to))
  values <- (replace(from, is.na(from), 0) + replace(to, is.na(to), 0)) / available
  values[available == 0L] <- NA_real_
  values
}

pems_roads_dictionary <- function() {
  meanings <- c(district = "Two-digit PeMS district.", freeway = "Source freeway number.",
    direction = "Source travel direction code.", from_station = "Upstream detector ID.",
    to_station = "Downstream detector ID.", from_abs_pm = "Upstream absolute postmile.",
    to_abs_pm = "Downstream absolute postmile.", id = "Project display-link index, local to its region.",
    from_station_index = "Upstream archived station-list position, when supplied.",
    to_station_index = "Downstream archived station-list position, when supplied.",
    region = "The existing regional selection.")
  data.frame(table = "roads", field = names(meanings), meaning = unname(meanings))
}

pems_prepare_metric <- function(output_dir, register_file, files, build_input, write_input) {
  # Check and lock the destination before reading the source sample.
  delayedAssign("native", read_pems_sample(register_file, files))
  write_input(native, build_input(native), output_dir, c(register_file, unname(files)))
}

pems_prepare_cli <- function(base, prepare) {
  args <- commandArgs(TRUE)
  usage <- "Usage: Rscript prepare.R [NEW_DIRECTORY]"
  if (identical(args, "--help") || identical(args, "-h")) {
    cat(usage, "\n")
    return(invisible(NULL))
  }
  if (length(args) > 1L || any(startsWith(args, "--"))) stop(usage, call. = FALSE)
  source(file.path(base, "scripts", "prepare-input.R"), local = environment(prepare))
  result <- if (!length(args)) {
    prepare(base)
  } else {
    prepare(base, output_dir = args[1L])
  }
  cat("Drawing input", file.path(dirname(result), "flow-frames.rds"), "\n")
}
