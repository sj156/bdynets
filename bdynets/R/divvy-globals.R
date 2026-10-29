# Register data.table and ggplot2 non-standard-evaluation symbols for R CMD check.
# All computational functions remain in separate one-function files.

utils::globalVariables(c(
  ".",
  "coordinate_available",
  "destination_latitude",
  "destination_longitude",
  "destination_node_idx",
  "latitude",
  "longitude",
  "n_raw_stations",
  "node_idx",
  "origin_latitude",
  "origin_longitude",
  "origin_node_idx",
  "street_class",
  "street_group",
  "timestamp",
  "timestamp_parsed",
  "trip_count"
))
