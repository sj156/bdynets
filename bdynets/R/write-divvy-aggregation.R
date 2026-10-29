# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

#' Write one generated Divvy aggregation bundle
#'
#' @param x Result from aggregate_divvy_radius().
#' @param output_dir Destination directory.
#' @param overwrite Whether generated files may be replaced.
#' @return Named character vector of written files.
#' @export
write_divvy_aggregation <- function(x, output_dir, overwrite = FALSE) {
  .divvy_require("data.table")
  .divvy_require("jsonlite")
  if (!inherits(x, "divvy_aggregation")) stop("Invalid aggregation object.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  tag <- paste0("r", as.integer(x$metadata$radius_m), "_from_raw")
  flow_base <- basename(x$metadata$source_flow)
  flow_name <- sub("_raw\\.csv$", paste0("_", tag, ".csv"), flow_base)
  if (identical(flow_name, flow_base)) flow_name <- paste0("od_flow_", tag, ".csv")
  paths <- c(
    nodes = file.path(output_dir, paste0("nodes_", tag, ".csv")),
    flows = file.path(output_dir, flow_name),
    membership = file.path(output_dir, paste0("membership_", tag, ".csv")),
    metadata = file.path(output_dir, paste0("aggregation_", tag, ".json"))
  )
  if (!overwrite && any(file.exists(paths))) {
    stop("Generated output already exists; set overwrite=TRUE or use a new directory.",
         call. = FALSE)
  }
  data.table::fwrite(x$nodes, paths[["nodes"]])
  flow_out <- data.table::copy(x$flows)
  flow_out[, timestamp_parsed := NULL]
  data.table::fwrite(flow_out, paths[["flows"]])
  data.table::fwrite(x$membership, paths[["membership"]])
  jsonlite::write_json(x$metadata, paths[["metadata"]], auto_unbox = TRUE,
                       pretty = TRUE, null = "null")
  paths
}

