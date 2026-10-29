# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

#' Read and validate raw Divvy node and hourly OD files
#'
#' @param data_dir Directory containing nodes_raw.csv and one raw OD CSV.
#' @param node_file Raw node filename.
#' @param flow_file Optional raw OD filename. When NULL, exactly one
#'   od_flow_*_raw.csv file must exist.
#' @param timezone IANA timezone for the source-local timestamps.
#' @return A list containing validated data.tables, source paths and timezone.
#' @import data.table
#' @export
read_divvy_raw <- function(
    data_dir,
    node_file = "nodes_raw.csv",
    flow_file = NULL,
    timezone = "America/Chicago") {
  .divvy_require("data.table")

  data_dir <- normalizePath(data_dir, mustWork = TRUE)
  node_path <- file.path(data_dir, node_file)
  if (!file.exists(node_path)) stop("Missing node file: ", node_path, call. = FALSE)

  if (is.null(flow_file)) {
    candidates <- list.files(
      data_dir,
      pattern = "^od_flow_.*_raw\\.csv$",
      full.names = TRUE
    )
    if (length(candidates) != 1L) {
      stop("Expected exactly one od_flow_*_raw.csv file in ", data_dir,
           "; found ", length(candidates), ".", call. = FALSE)
    }
    flow_path <- candidates
  } else {
    flow_path <- file.path(data_dir, flow_file)
  }
  if (!file.exists(flow_path)) stop("Missing flow file: ", flow_path, call. = FALSE)

  nodes <- data.table::fread(node_path, showProgress = FALSE)
  # Force timestamp to character. fread otherwise auto-detects it as UTC;
  # relabelling that POSIXct value as America/Chicago shifts the intended
  # local clock time by six hours.
  flows <- data.table::fread(
    flow_path,
    colClasses = c(timestamp = "character"),
    showProgress = FALSE
  )

  node_columns <- c(
    "node_idx", "node_id", "representative_station_id", "latitude",
    "longitude", "n_raw_stations", "display_name", "coordinate_available"
  )
  flow_columns <- c(
    "timestamp", "origin_node_idx", "destination_node_idx", "trip_count"
  )
  missing_nodes <- setdiff(node_columns, names(nodes))
  missing_flows <- setdiff(flow_columns, names(flows))
  if (length(missing_nodes)) {
    stop("Node file is missing columns: ", paste(missing_nodes, collapse = ", "),
         call. = FALSE)
  }
  if (length(missing_flows)) {
    stop("OD file is missing columns: ", paste(missing_flows, collapse = ", "),
         call. = FALSE)
  }

  nodes[, node_idx := as.integer(node_idx)]
  nodes[, latitude := as.numeric(latitude)]
  nodes[, longitude := as.numeric(longitude)]
  nodes[, n_raw_stations := as.integer(n_raw_stations)]
  nodes[, coordinate_available := .divvy_available(coordinate_available)]
  flows[, origin_node_idx := as.integer(origin_node_idx)]
  flows[, destination_node_idx := as.integer(destination_node_idx)]
  flows[, trip_count := as.numeric(trip_count)]
  parsed_time <- as.POSIXct(
    flows$timestamp,
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )
  if (anyNA(parsed_time)) stop("OD file contains invalid timestamps.", call. = FALSE)
  flows[, timestamp_parsed := parsed_time]

  if (anyNA(nodes$node_idx) || anyDuplicated(nodes$node_idx)) {
    stop("node_idx must be nonmissing and unique.", call. = FALSE)
  }
  if (any(!nodes$coordinate_available)) {
    stop("Raw aggregation requires coordinates for every node.", call. = FALSE)
  }
  if (any(!is.finite(nodes$latitude)) || any(!is.finite(nodes$longitude)) ||
      any(abs(nodes$latitude) > 90) || any(abs(nodes$longitude) > 180)) {
    stop("Node coordinates are invalid.", call. = FALSE)
  }
  if (anyNA(nodes$n_raw_stations) || any(nodes$n_raw_stations < 1L)) {
    stop("n_raw_stations must contain positive integers.", call. = FALSE)
  }
  if (anyNA(flows$origin_node_idx) || anyNA(flows$destination_node_idx)) {
    stop("OD endpoints must be nonmissing integers.", call. = FALSE)
  }
  valid_ids <- nodes$node_idx
  if (any(!flows$origin_node_idx %in% valid_ids) ||
      any(!flows$destination_node_idx %in% valid_ids)) {
    stop("OD table contains endpoints absent from nodes_raw.csv.", call. = FALSE)
  }
  if (!.divvy_integer_counts(flows$trip_count)) {
    stop("trip_count must contain nonnegative integer counts.", call. = FALSE)
  }
  duplicate_key <- duplicated(flows[, .(
    timestamp, origin_node_idx, destination_node_idx
  )])
  if (any(duplicate_key)) {
    stop("OD table contains duplicate timestamp/origin/destination keys.",
         call. = FALSE)
  }

  data.table::setorder(nodes, node_idx)
  data.table::setorder(flows, timestamp_parsed, origin_node_idx, destination_node_idx)
  list(
    nodes = nodes,
    flows = flows,
    node_path = node_path,
    flow_path = flow_path,
    timezone = timezone
  )
}

