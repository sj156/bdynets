# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

#' Prepare one Divvy OD time window for mapping
#'
#' @param nodes Aggregated node table.
#' @param flows Aggregated sparse hourly OD table.
#' @param start_time,end_time Half-open source-local time window [start,end).
#' @param timezone IANA timezone.
#' @param top_n Maximum number of nonloop routes to display.
#' @return Nodes, selected routes and a descriptive metadata list.
#' @export
prepare_divvy_od_window <- function(
    nodes,
    flows,
    start_time,
    end_time,
    timezone = "America/Chicago",
    top_n = 180L) {
  .divvy_require("data.table")
  nodes <- data.table::as.data.table(data.table::copy(nodes))
  flows <- data.table::as.data.table(data.table::copy(flows))
  required_nodes <- c("node_idx", "latitude", "longitude", "n_raw_stations")
  required_flows <- c("timestamp", "origin_node_idx", "destination_node_idx", "trip_count")
  if (length(setdiff(required_nodes, names(nodes))) ||
      length(setdiff(required_flows, names(flows)))) {
    stop("Invalid aggregated Divvy tables.", call. = FALSE)
  }
  if (!"timestamp_parsed" %in% names(flows)) {
    flows[, timestamp_parsed := as.POSIXct(timestamp, tz = timezone)]
  }
  start <- as.POSIXct(start_time, tz = timezone)
  end <- as.POSIXct(end_time, tz = timezone)
  if (is.na(start) || is.na(end) || end <= start) {
    stop("Supply a valid half-open time window with end_time after start_time.",
         call. = FALSE)
  }
  window <- flows[timestamp_parsed >= start & timestamp_parsed < end]
  if (!nrow(window)) stop("No OD observations in the selected time window.")
  total_trips <- sum(window$trip_count)
  loop_rows <- window[origin_node_idx == destination_node_idx]
  nonloop <- window[origin_node_idx != destination_node_idx]
  routes <- nonloop[, .(trip_count = sum(trip_count)),
                    by = .(origin_node_idx, destination_node_idx)]
  data.table::setorder(routes, -trip_count, origin_node_idx, destination_node_idx)
  active_routes <- nrow(routes)
  if (length(top_n) != 1L || !is.finite(top_n) || top_n < 1) {
    stop("top_n must be a positive finite number.")
  }
  routes <- utils::head(routes, as.integer(top_n))
  origin_at <- match(routes$origin_node_idx, nodes$node_idx)
  destination_at <- match(routes$destination_node_idx, nodes$node_idx)
  if (anyNA(origin_at) || anyNA(destination_at)) stop("Unknown route endpoint.")
  routes[, `:=`(
    origin_longitude = nodes$longitude[origin_at],
    origin_latitude = nodes$latitude[origin_at],
    destination_longitude = nodes$longitude[destination_at],
    destination_latitude = nodes$latitude[destination_at],
    curve_sign = ifelse(origin_node_idx < destination_node_idx, 1L, -1L)
  )]
  active_node_ids <- unique(c(routes$origin_node_idx, routes$destination_node_idx))
  active_nodes <- nodes[node_idx %in% active_node_ids]
  list(
    nodes = active_nodes,
    routes = routes,
    metadata = list(
      start = start,
      end = end,
      total_trips = as.numeric(total_trips),
      within_node_trips = as.numeric(sum(loop_rows$trip_count)),
      nonloop_trips = as.numeric(sum(nonloop$trip_count)),
      active_nonloop_routes = active_routes,
      displayed_routes = nrow(routes),
      displayed_route_trips = as.numeric(sum(routes$trip_count))
    )
  )
}

