#!/usr/bin/env Rscript
# Observed origin–destination counts between fixed 2 km cells for a one-day smoke case. A prepared
# projected-link study can instead be supplied via --input=/path/to/study.rds.
entry <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
here <- dirname(normalizePath(gsub("~+~", " ", entry, fixed = TRUE)))
source(file.path(dirname(here), "prepare-common.R"))
opts <- preparation_options(c("output", "input", "date"))
repo <- dirname(dirname(dirname(here)))
raw_file <- if (is.null(opts$input)) file.path(repo, "examples/DivvyBike/data/raw/202601-divvy-tripdata.csv") else opts$input
if (!file.exists(raw_file)) stop("Divvy source file is unavailable: ", raw_file)

if (grepl("\\.rds$", raw_file, ignore.case = TRUE)) {
  prepared <- readRDS(raw_file)
  if (!is.null(opts$date)) stop("--date applies to raw trip CSV input.")
  if (!is.list(prepared) || !all(c("Y", "graph", "covariates", "split") %in% names(prepared)))
    stop("Supply the prepared GLS input RDS for the projected archive, including the original train/validation/test split.")
  prepared$case_id <- "divvy"
  prepared$preparation <- list(kind = "supplied-prepared-input",
    description = "Original series, network, covariates, candidate pairs and chronological split supplied by the archive.",
    source = normalizePath(raw_file))
  save_prepared_case(prepared, opts$output, "Divvy flows from supplied prepared archive", "Count per bin", raw_file)
} else {
  day <- if (is.null(opts$date)) "2026-01-01" else opts$date
  tz <- "America/Chicago"
  start <- as.POSIXct(paste(day, "00:00:00"), tz = tz)
  until <- as.POSIXct(paste(as.Date(day) + 1L, "00:00:00"), tz = tz)
  times <- seq(start, until - 15 * 60, by = "15 min")
  bins <- length(times)
  split <- bdynets::gls_split_rows(bins)
  trips <- data.table::fread(raw_file, select = c("ride_id", "started_at", "start_lat", "start_lng", "end_lat", "end_lng"),
    colClasses = c(ride_id = "character", started_at = "character"), na.strings = c("", "NA"), data.table = FALSE)
  raw_count <- nrow(trips)
  trips <- trips[!is.na(trips$started_at) & substr(trips$started_at, 1L, 10L) == day, , drop = FALSE]
  in_day <- nrow(trips)
  trips <- unique(trips)
  if (anyNA(trips$ride_id) || anyDuplicated(trips$ride_id)) stop("Missing or conflicting ride IDs in the selected day.")
  valid_point <- function(lat, lon) is.finite(lat) & is.finite(lon) &
    abs(lat) <= 90 & abs(lon) <= 180 & !(lat == 0 & lon == 0)
  keep <- valid_point(trips$start_lat, trips$start_lng) & valid_point(trips$end_lat, trips$end_lng)
  missing_endpoints <- sum(!keep)
  trips <- trips[keep, , drop = FALSE]
  # A fixed local equirectangular approximation gives common cells for both
  # endpoints. The projection and cell origin use no estimated locations.
  cell_2km <- function(lat, lon) {
    radians <- pi / 180
    x <- 6371.0088 * cos(41.88 * radians) * (lon + 88) * radians
    y <- 6371.0088 * (lat - 41.50) * radians
    sprintf("%d,%d", floor(x / 2), floor(y / 2))
  }
  trips$from <- cell_2km(trips$start_lat, trips$start_lng)
  trips$to <- cell_2km(trips$end_lat, trips$end_lng)
  within_cell <- trips$from == trips$to
  trips <- trips[!within_cell, , drop = FALSE]
  times_observed <- as.POSIXct(substr(trips$started_at, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = tz)
  if (anyNA(times_observed)) stop("Unrecognized Divvy start timestamp.")
  trips$bin <- 1L + floor(as.numeric(difftime(times_observed, start, units = "secs")) / 900)
  trips$link <- paste(trips$from, trips$to, sep = " -> ")
  training <- trips[trips$bin %in% split$train, , drop = FALSE]
  if (!nrow(training)) stop("No valid OD trips in the training period.")
  edges <- stats::aggregate(rep(1L, nrow(training)), training[c("from", "to", "link")], sum)
  names(edges) <- c("from", "to", "name", "training_rides")
  edges <- edges[order(-edges$training_rides, edges$from, edges$to), , drop = FALSE]
  # Grow outward from the busiest training OD to include adjacent OD cell pairs.
  selected <- 1L
  while (length(selected) < min(6L, nrow(edges))) {
    nodes <- unique(c(edges$from[selected], edges$to[selected]))
    candidate <- which((edges$from %in% nodes | edges$to %in% nodes) & !seq_len(nrow(edges)) %in% selected)
    if (!length(candidate)) break
    selected <- c(selected, candidate[1L])
  }
  edges <- edges[selected, , drop = FALSE]
  if (nrow(edges) < 2L) stop("Insufficient connected training OD pairs for this case.")
  edges$id <- seq_len(nrow(edges))
  graph <- bdynets::gls_graph(edges)
  slots <- match(trips$link, edges$name)
  take <- !is.na(slots)
  Y <- matrix(tabulate(trips$bin[take] + (slots[take] - 1L) * bins, nbins = bins * nrow(edges)), bins,
              dimnames = list(NULL, edges$name))
  prepared <- list(case_id = "divvy", Y = Y, graph = graph, covariates = daily_covariates(bins, 96L),
    split = split, time = times, time_labels = format(times, "%Y-%m-%d %H:%M %Z"), series_labels = edges$name,
    provenance = list(source = "Divvy monthly public trip data", source_url = "https://divvy-tripdata.s3.amazonaws.com/index.html",
      date = day, timezone = tz, raw_rows = raw_count, selected_day_rows = in_day, missing_endpoints = missing_endpoints,
      excluded_within_cell = sum(within_cell), retained_rides_on_selected_links = sum(Y),
      unit = "Observed cross-cell OD departures per 15-minute bin",
      grid = "2 km square cells in an equirectangular approximation (Earth radius 6371.0088 km, standard parallel 41.88 N, origin 41.50 N, 88.00 W). Cell IDs are zero-based east,north indices.",
      endpoints = "Recorded start and end coordinates are used even when a station ID is absent; invalid coordinates and within-cell trips are excluded.",
      selection = "Six connected OD links starting with the busiest training OD, then highest training frequency among adjacent candidates. Ties use cell IDs.",
      scope = "One-day observed 2 km cell OD smoke analysis. The paper reports six months of projected three-hour link traversals."))
  save_prepared_case(prepared, opts$output, "Observed Divvy departures between 2 km cells", "Departures per 15 min", raw_file)
}
