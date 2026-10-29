# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

#' Aggregate raw Divvy stations and hourly OD counts at an arbitrary radius
#'
#' Representatives are selected deterministically in descending total endpoint
#' activity, breaking ties by raw node index. A candidate becomes a representative
#' when it is farther than radius_m from every existing representative. Every raw
#' node is then assigned to its nearest representative. OD counts are summed after
#' mapping both endpoints; the total trip count must remain unchanged.
#'
#' @param data_dir Directory containing raw Divvy CSV files.
#' @param radius_m Positive aggregation radius in metres.
#' @param timezone Source-local timezone.
#' @param output_dir Optional directory for generated CSV/JSON files.
#' @param overwrite Whether generated files may be replaced.
#' @return A divvy_aggregation list with nodes, flows, membership and metadata.
#' @export
aggregate_divvy_radius <- function(
    data_dir,
    radius_m,
    timezone = "America/Chicago",
    output_dir = NULL,
    overwrite = FALSE) {
  .divvy_require("data.table")
  if (length(radius_m) != 1L || !is.finite(radius_m) || radius_m <= 0) {
    stop("radius_m must be one positive number in metres.", call. = FALSE)
  }
  raw <- read_divvy_raw(data_dir, timezone = timezone)
  nodes <- data.table::copy(raw$nodes)
  flows <- data.table::copy(raw$flows)

  origin_activity <- flows[, .(activity = sum(trip_count)),
                           by = .(node_idx = origin_node_idx)]
  destination_activity <- flows[, .(activity = sum(trip_count)),
                                by = .(node_idx = destination_node_idx)]
  activity <- data.table::rbindlist(list(origin_activity, destination_activity))[
    , .(activity = sum(activity)), by = node_idx
  ]
  nodes[, activity := activity$activity[match(node_idx, activity$node_idx)]]
  nodes[is.na(activity), activity := 0]

  distances <- divvy_haversine_matrix(nodes$latitude, nodes$longitude)
  candidate_order <- order(-nodes$activity, nodes$node_idx)
  representatives <- integer(0)
  for (position in candidate_order) {
    if (!length(representatives) ||
        min(distances[position, representatives]) > radius_m) {
      representatives <- c(representatives, position)
    }
  }
  nearest <- max.col(-distances[, representatives, drop = FALSE],
                     ties.method = "first")
  assignment_distance <- distances[cbind(seq_len(nrow(nodes)), representatives[nearest])]
  if (max(assignment_distance) > radius_m + 1e-7) {
    stop("Radius-cover invariant failed.", call. = FALSE)
  }

  aggregate_ids <- nearest - 1L
  aggregate_nodes <- data.table::rbindlist(lapply(seq_along(representatives), function(k) {
    representative <- nodes[representatives[k]]
    members <- which(nearest == k)
    data.table::data.table(
      node_idx = as.integer(k - 1L),
      node_id = as.character(representative$node_id),
      representative_station_id = as.character(representative$representative_station_id),
      latitude = as.numeric(representative$latitude),
      longitude = as.numeric(representative$longitude),
      n_raw_stations = as.integer(sum(nodes$n_raw_stations[members])),
      display_name = as.character(representative$display_name),
      coordinate_available = TRUE,
      endpoint_activity = as.numeric(sum(nodes$activity[members]))
    )
  }))

  membership <- data.table::data.table(
    raw_node_idx = nodes$node_idx,
    raw_node_id = as.character(nodes$node_id),
    aggregate_node_idx = as.integer(aggregate_ids),
    aggregate_node_id = as.character(
      aggregate_nodes$node_id[match(aggregate_ids, aggregate_nodes$node_idx)]
    ),
    distance_to_representative_m = round(assignment_distance, 3)
  )
  mapped_origin <- aggregate_ids[match(flows$origin_node_idx, nodes$node_idx)]
  mapped_destination <- aggregate_ids[match(flows$destination_node_idx, nodes$node_idx)]
  if (anyNA(mapped_origin) || anyNA(mapped_destination)) {
    stop("A raw OD endpoint was not mapped.", call. = FALSE)
  }
  flows[, origin_node_idx := as.integer(mapped_origin)]
  flows[, destination_node_idx := as.integer(mapped_destination)]
  aggregated_flows <- flows[, .(
    trip_count = sum(trip_count)
  ), by = .(timestamp, timestamp_parsed, origin_node_idx, destination_node_idx)]
  data.table::setorder(
    aggregated_flows, timestamp_parsed, origin_node_idx, destination_node_idx
  )
  source_total <- sum(raw$flows$trip_count)
  output_total <- sum(aggregated_flows$trip_count)
  if (!identical(as.numeric(source_total), as.numeric(output_total))) {
    stop("Trip total changed during aggregation.", call. = FALSE)
  }

  metadata <- list(
    schema_version = 1L,
    method = "activity-prioritized greedy radius cover; nearest representative assignment",
    radius_m = as.numeric(radius_m),
    timezone = timezone,
    # Store portable filenames rather than a contributor's absolute path.
    source_nodes = basename(raw$node_path),
    source_flow = basename(raw$flow_path),
    source_node_md5 = unname(tools::md5sum(raw$node_path)),
    source_flow_md5 = unname(tools::md5sum(raw$flow_path)),
    raw_nodes = nrow(raw$nodes),
    aggregate_nodes = nrow(aggregate_nodes),
    raw_flow_rows = nrow(raw$flows),
    aggregate_flow_rows = nrow(aggregated_flows),
    total_trips = as.numeric(output_total),
    max_assignment_distance_m = max(assignment_distance),
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  out <- structure(
    list(
      nodes = aggregate_nodes,
      flows = aggregated_flows,
      membership = membership,
      metadata = metadata
    ),
    class = "divvy_aggregation"
  )
  if (!is.null(output_dir)) {
    out$files <- write_divvy_aggregation(out, output_dir, overwrite = overwrite)
  }
  out
}

