# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

#' Aggregate raw Divvy data and draw one requested OD map
#'
#' @param data_dir Raw Divvy data directory.
#' @param radius_m Aggregation radius in metres.
#' @param start_time,end_time Half-open local-time window [start,end).
#' @param aggregation_output_dir Optional generated-data directory.
#' @param ... Additional arguments passed to plot_divvy_od_map().
#' @return List with the aggregation object and map result.
#' @export
run_divvy_od_map <- function(
    data_dir,
    radius_m,
    start_time,
    end_time,
    aggregation_output_dir = NULL,
    ...) {
  aggregation <- aggregate_divvy_radius(
    data_dir = data_dir,
    radius_m = radius_m,
    output_dir = aggregation_output_dir,
    overwrite = !is.null(aggregation_output_dir)
  )
  map <- plot_divvy_od_map(
    nodes = aggregation$nodes,
    flows = aggregation$flows,
    start_time = start_time,
    end_time = end_time,
    radius_m = radius_m,
    ...
  )
  list(aggregation = aggregation, map = map)
}

