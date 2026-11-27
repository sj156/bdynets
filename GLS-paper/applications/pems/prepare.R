#!/usr/bin/env Rscript
# One corridor of Caltrans District 4 five-minute mainline detector totals.
entry <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
here <- dirname(normalizePath(gsub("~+~", " ", entry, fixed = TRUE)))
source(file.path(dirname(here), "prepare-common.R"))
opts <- preparation_options(c("output", "input", "metadata"))
repo <- dirname(dirname(dirname(here)))
raw <- file.path(repo, "examples/pems-flow/data/raw/caltrans")
flow_file <- if (is.null(opts$input)) file.path(raw, "5min/D04/2026/2026_06/d04_text_station_5min_2026_06_16.txt") else opts$input
meta_file <- if (is.null(opts$metadata)) file.path(raw, "metadata/D04/d04_text_meta_2026_04_08.txt") else opts$metadata
if (!file.exists(flow_file) || !file.exists(meta_file)) stop("Supply District 4 daily five-minute station and metadata files with --input and --metadata.")

# Restrict I/O to the source fields needed for this panel. V10 is total flow.
flow <- data.table::fread(flow_file, header = FALSE, select = c(1L, 2L, 3L, 4L, 5L, 6L, 9L, 10L),
  col.names = c("time", "id", "district", "freeway", "direction", "type", "observed", "flow"),
  na.strings = c("", "NA"), data.table = FALSE)
metadata <- data.table::fread(meta_file, select = c("ID", "District", "Fwy", "Dir", "Type", "Abs_PM", "Name"),
                              data.table = FALSE)
# US-101 northbound postmiles increase in the direction of travel.
route <- 101L
direction <- "N"
meta <- metadata[metadata$District == 4L & metadata$Fwy == route & metadata$Dir == direction &
                   metadata$Type == "ML" & is.finite(metadata$Abs_PM), , drop = FALSE]
meta <- meta[order(meta$Abs_PM, meta$ID), , drop = FALSE]
flow <- flow[flow$district == 4L & flow$freeway == route & flow$direction == direction &
                 flow$type == "ML" & flow$id %in% meta$ID, , drop = FALSE]
if (nrow(meta) < 4L || !nrow(flow) || anyDuplicated(meta$ID) || anyDuplicated(flow[c("time", "id")]))
  stop("Missing or duplicated station records on the District 4 US-101 N mainline.")
# The archive supplies local clock labels. Integer slots avoid dependence on host TZ.
day <- unique(substr(flow$time, 1L, 10L))
if (length(day) != 1L || is.na(as.Date(day, "%m/%d/%Y"))) stop("Expected a single date with MM/DD/YYYY source-clock labels.")
tz <- "America/Los_Angeles"
t0 <- as.POSIXct(strptime(paste(day, "00:00:00"), "%m/%d/%Y %H:%M:%S", tz = tz))
times <- seq(t0, by = "5 min", length.out = 288L)
time_strings <- format(times, "%m/%d/%Y %H:%M:%S", tz = tz)
if (anyDuplicated(time_strings) || tail(substr(time_strings, 1L, 10L), 1L) != day)
  stop("This one-day case requires a regular 288-slot local day.")
flow$bin <- match(flow$time, time_strings)
if (anyNA(flow$bin)) stop("Station data include unexpected source-clock labels.")
split <- bdynets::gls_split_rows(length(times))
training <- flow[flow$bin %in% split$train, , drop = FALSE]
valid_flow <- function(x) is.finite(x) & x >= 0 & x == floor(x)
quality <- vapply(split(training, training$id), function(x) {
  if (nrow(x) == length(split$train) && all(valid_flow(x$flow)) && all(is.finite(x$observed)))
    return(mean(x$observed))
  NA_real_
}, numeric(1))
meta$training_percent_observed <- quality[as.character(meta$ID)]
# Pick a contiguous run of four distinct-postmile stations; rank runs solely on
# training completeness and detector observation percentage, then upstream mile.
windows <- seq_len(nrow(meta) - 3L)
window_quality <- vapply(windows, function(i) {
  ix <- i + 0:3
  if (anyDuplicated(meta$Abs_PM[ix]) || anyNA(meta$training_percent_observed[ix])) return(NA_real_)
  mean(meta$training_percent_observed[ix])
}, numeric(1))
if (all(is.na(window_quality))) stop("No four consecutive stations with complete training flow.")
idx <- which.max(window_quality) + 0:3
stations <- meta[idx, , drop = FALSE]
flow <- flow[flow$id %in% stations$ID, , drop = FALSE]
Y <- matrix(NA_real_, nrow = length(times), ncol = nrow(stations))
obs <- Y
location <- cbind(flow$bin, match(flow$id, stations$ID))
Y[location] <- flow$flow
obs[location] <- flow$observed
if (any(!valid_flow(Y))) {
  stop("The training-selected corridor has missing or invalid flow. GLS requires a complete panel. No zero filling or held-out station reselection was applied.")
}
edges <- data.frame(id = seq_len(nrow(stations)), from = seq_len(nrow(stations)),
                    to = seq_len(nrow(stations)) + 1L, name = as.character(stations$ID))
graph <- bdynets::gls_graph(edges)
colnames(Y) <- colnames(obs) <- edges$name
prepared <- list(case_id = "pems", Y = Y, graph = graph, covariates = daily_covariates(nrow(Y), 288L),
  split = split, time = times, time_labels = format(times, "%Y-%m-%d %H:%M %Z"), series_labels = edges$name,
  stations = stations, percent_observed = obs,
  provenance = list(source = "California Department of Transportation, PeMS District 4 station five-minute totals and metadata",
    source_url = "https://pems.dot.ca.gov/", date = day, timezone = tz, freeway = route, direction = direction,
    count_field = "V10 (total vehicles per five minutes); V9 is the percentage observed by working detectors.",
    observation_scope = "Provider-reported totals can include PeMS estimates when V9 is below 100 percent. V9 is retained by bin and station.",
    selection = "Four consecutive distinct-postmile mainline stations on D04 US-101 northbound maximizing mean training percent observed, each with all training counts present. Ties choose the furthest upstream.",
    graph = "Station series are ordered by ascending absolute postmile on US-101 northbound. Each downstream station has its immediate upstream station as physical predecessor.",
    scope = "One observed day on one Bay Area corridor."))
save_prepared_case(prepared, opts$output, "US-101 northbound five-minute station flow", "Vehicles per 5 min", c(flow_file, meta_file))
