.od_parse_time <- function(x, timezone = "UTC", time_format = NULL) {
  if (!is.character(timezone) || length(timezone) != 1L || is.na(timezone) ||
      !timezone %in% c("UTC", "GMT", OlsonNames())) {
    stop("timezone must be one recognized IANA timezone.", call. = FALSE)
  }
  if (inherits(x, "POSIXt")) {
    value <- as.numeric(as.POSIXct(x))
  } else {
    if (!is.character(x) && !is.factor(x)) {
      stop("Times must be POSIXt instants or explicit character timestamps.", call. = FALSE)
    }
    text <- trimws(as.character(x))
    if (anyNA(text) || any(!nzchar(text))) stop("Missing timestamps.", call. = FALSE)
    if (!is.null(time_format)) {
      if (!is.character(time_format) || length(time_format) != 1L || is.na(time_format) || !nzchar(time_format)) {
        stop("time_format must be one format string.", call. = FALSE)
      }
      value <- as.numeric(as.POSIXct(strptime(text, format = time_format, tz = timezone)))
    } else {
      pattern <- paste0("^([0-9]{4}-[0-9]{2}-[0-9]{2})",
        "(?:[T ]([0-9]{2}:[0-9]{2})(?::([0-9]{2}(?:\\.[0-9]+)?))?",
        "([Zz]|[+-][0-9]{2}:?[0-9]{2})?)?$")
      parts <- regmatches(text, regexec(pattern, text, perl = TRUE))
      if (any(lengths(parts) != 5L)) {
        stop("Unsupported or incomplete timestamp; supply a complete ISO timestamp or an explicit time_format.", call. = FALSE)
      }
      value <- rep(NA_real_, length(text))
      if (length(text)) {
        parts <- do.call(rbind, parts)
        clock <- ifelse(nzchar(parts[, 3]), parts[, 3], "00:00")
        seconds <- ifelse(nzchar(parts[, 4]), parts[, 4], "00")
        stamp <- paste(parts[, 2], paste(clock, seconds, sep = ":"))
        offset <- which(nzchar(parts[, 5]))
        local <- which(!nzchar(parts[, 5]))
        if (length(offset)) {
          zone <- ifelse(tolower(parts[offset, 5]) == "z", "+0000",
            gsub(":", "", parts[offset, 5], fixed = TRUE))
          value[offset] <- as.numeric(as.POSIXct(strptime(paste0(stamp[offset], zone),
            format = "%Y-%m-%d %H:%M:%OS%z", tz = "UTC")))
        }
        if (length(local)) {
          parsed <- as.POSIXct(strptime(stamp[local], format = "%Y-%m-%d %H:%M:%OS", tz = timezone))
          value[local] <- as.numeric(parsed)
          # Nonexistent wall-clock times must not silently move to another hour.
          shifted <- !is.na(parsed) & format(parsed, "%Y-%m-%d %H:%M", tz = timezone) != paste(parts[local, 2], clock[local])
          value[local[shifted]] <- NA_real_
        }
      }
    }
  }
  if (any(!is.finite(value))) stop("Invalid timestamps for the supplied format and timezone.", call. = FALSE)
  as.POSIXct(value, origin = "1970-01-01", tz = timezone)
}

#' Validate and aggregate spatial origin-destination data
#'
#' This is the only data-preparation function needed by the OD mapping workflow.
#' It accepts data frames or CSV/RDS paths, validates identifiers, coordinates,
#' times and flow counts, aggregates nearby nodes at an arbitrary radius, remaps
#' both OD endpoints, and verifies that the total flow is conserved. Column names
#' are supplied through named mappings, so unrelated columns are ignored and the
#' function is not tied to a particular data provider.
#'
#' Character times with Z or numeric offsets preserve absolute instants; unzoned
#' times use timezone. Exported UTC CSV timestamps use the same parser on re-read.
#'
#' @param nodes A node data frame/data.table, or a path to a CSV, CSV.GZ or RDS.
#' @param flows An OD data frame/data.table, or a path to a CSV, CSV.GZ or RDS.
#' @param radius_m Positive aggregation radius in metres.
#' @param node_columns Named character vector with `id`, `longitude`, `latitude`
#'   and optional `name` and `weight` mappings.
#' @param flow_columns Named character vector with `origin`, `destination` and
#'   optional `count` and `time` mappings. Use `NA` for `count` when every row is
#'   one trip, or for `time` when the table has no time dimension.
#' @param timezone IANA timezone used when character timestamps are parsed.
#' @param time_format Optional format passed to [as.POSIXct()].
#' @param output_dir Optional directory for standardized CSV and JSON outputs.
#' @param overwrite Whether existing generated files may be replaced.
#' @return An `od_aggregation` list containing standardized `nodes`, `flows`,
#'   `membership`, `metadata`, and optionally `files`.
#' @export
aggregate_od_data <- function(
    nodes,
    flows,
    radius_m,
    node_columns = c(
      id = "node_id", longitude = "longitude", latitude = "latitude",
      name = NA_character_, weight = NA_character_
    ),
    flow_columns = c(
      origin = "origin_id", destination = "destination_id",
      count = "flow", time = NA_character_
    ),
    timezone = "UTC",
    time_format = NULL,
    output_dir = NULL,
    overwrite = FALSE) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Install the 'data.table' package before using this function.", call. = FALSE)
  }
  if (length(radius_m) != 1L || !is.finite(radius_m) || radius_m <= 0) {
    stop("radius_m must be one positive number in metres.", call. = FALSE)
  }

  read_table <- function(x, label) {
    if (is.character(x) && length(x) == 1L) {
      if (!file.exists(x)) stop("Missing ", label, " file: ", x, call. = FALSE)
      extension <- tolower(tools::file_ext(sub("\\.gz$", "", x)))
      if (extension == "rds") {
        x <- readRDS(x)
      } else if (extension == "csv") {
        # Keep source timestamps as text so a timezone-free wall clock is
        # interpreted in the caller's requested timezone rather than UTC.
        x <- data.table::fread(x, showProgress = FALSE, colClasses = "character")
      } else {
        stop(label, " must be a data frame or CSV/CSV.GZ/RDS path.", call. = FALSE)
      }
    }
    if (!is.data.frame(x)) stop(label, " must be tabular data.", call. = FALSE)
    data.table::as.data.table(data.table::copy(x))
  }
  validate_mapping <- function(mapping, required, allowed, label) {
    if (is.null(names(mapping)) || anyDuplicated(names(mapping))) {
      stop(label, " must be a uniquely named character vector.", call. = FALSE)
    }
    unknown <- setdiff(names(mapping), allowed)
    missing <- setdiff(required, names(mapping))
    if (length(unknown)) stop("Unknown ", label, " names: ", paste(unknown, collapse = ", "), call. = FALSE)
    if (length(missing)) stop("Missing ", label, " names: ", paste(missing, collapse = ", "), call. = FALSE)
    mapping
  }
  mapped_column <- function(mapping, key, table, required = FALSE) {
    value <- unname(mapping[key])
    if (!length(value) || is.na(value) || !nzchar(value)) {
      if (required) stop("A column mapping is required for '", key, "'.", call. = FALSE)
      return(NULL)
    }
    if (!value %in% names(table)) {
      stop("Mapped column '", value, "' for '", key, "' does not exist.", call. = FALSE)
    }
    value
  }
  haversine <- function(lat1, lon1, lat2, lon2) {
    rad <- pi / 180
    lat1 <- lat1 * rad
    lon1 <- lon1 * rad
    lat2 <- lat2 * rad
    lon2 <- lon2 * rad
    a <- sin((lat2 - lat1) / 2)^2 +
      cos(lat1) * cos(lat2) * sin((lon2 - lon1) / 2)^2
    a[a < 0] <- 0
    a[a > 1] <- 1
    2 * 6371008.8 * asin(sqrt(a))
  }

  node_source <- if (is.character(nodes) && length(nodes) == 1L) basename(nodes) else "in-memory"
  flow_source <- if (is.character(flows) && length(flows) == 1L) basename(flows) else "in-memory"
  nodes <- read_table(nodes, "nodes")
  flows <- read_table(flows, "flows")
  node_columns <- validate_mapping(
    node_columns, c("id", "longitude", "latitude"),
    c("id", "longitude", "latitude", "name", "weight"), "node_columns"
  )
  flow_columns <- validate_mapping(
    flow_columns, c("origin", "destination"),
    c("origin", "destination", "count", "time"), "flow_columns"
  )

  id_col <- mapped_column(node_columns, "id", nodes, TRUE)
  lon_col <- mapped_column(node_columns, "longitude", nodes, TRUE)
  lat_col <- mapped_column(node_columns, "latitude", nodes, TRUE)
  name_col <- mapped_column(node_columns, "name", nodes)
  weight_col <- mapped_column(node_columns, "weight", nodes)
  origin_col <- mapped_column(flow_columns, "origin", flows, TRUE)
  destination_col <- mapped_column(flow_columns, "destination", flows, TRUE)
  count_col <- mapped_column(flow_columns, "count", flows)
  time_col <- mapped_column(flow_columns, "time", flows)

  node_id <- as.character(nodes[[id_col]])
  longitude <- as.numeric(nodes[[lon_col]])
  latitude <- as.numeric(nodes[[lat_col]])
  display_name <- if (is.null(name_col)) node_id else as.character(nodes[[name_col]])
  member_weight <- if (is.null(weight_col)) rep(1, nrow(nodes)) else as.numeric(nodes[[weight_col]])
  if (anyNA(node_id) || any(!nzchar(node_id)) || anyDuplicated(node_id)) {
    stop("Node identifiers must be nonmissing and unique.", call. = FALSE)
  }
  if (any(!is.finite(longitude)) || any(!is.finite(latitude)) ||
      any(abs(longitude) > 180) || any(abs(latitude) > 90)) {
    stop("Node coordinates are invalid.", call. = FALSE)
  }
  if (any(!is.finite(member_weight)) || any(member_weight <= 0)) {
    stop("Node weights must be positive finite numbers.", call. = FALSE)
  }

  origin_id <- as.character(flows[[origin_col]])
  destination_id <- as.character(flows[[destination_col]])
  flow_value <- if (is.null(count_col)) rep(1, nrow(flows)) else as.numeric(flows[[count_col]])
  if (anyNA(origin_id) || anyNA(destination_id) ||
      any(!origin_id %in% node_id) || any(!destination_id %in% node_id)) {
    stop("Every OD endpoint must match one node identifier.", call. = FALSE)
  }
  if (any(!is.finite(flow_value)) || any(flow_value < 0) ||
      any(abs(flow_value - round(flow_value)) > .Machine$double.eps^0.5)) {
    stop("Flow counts must be nonnegative integers.", call. = FALSE)
  }

  parsed_time <- NULL
  if (!is.null(time_col)) {
    raw_time <- flows[[time_col]]
    parsed_time <- .od_parse_time(raw_time, timezone, time_format)
    if (anyNA(parsed_time)) stop("The mapped time column contains invalid timestamps.", call. = FALSE)
  }

  endpoint_ids <- c(origin_id, destination_id)
  endpoint_flow <- c(flow_value, flow_value)
  activity_lookup <- tapply(endpoint_flow, endpoint_ids, sum)
  activity <- as.numeric(activity_lookup[node_id])
  activity[is.na(activity)] <- 0

  candidate_order <- order(-activity, node_id)
  representatives <- integer(0)
  for (position in candidate_order) {
    if (!length(representatives)) {
      representatives <- position
    } else {
      distance <- haversine(
        latitude[position], longitude[position],
        latitude[representatives], longitude[representatives]
      )
      if (min(distance) > radius_m) representatives <- c(representatives, position)
    }
  }

  nearest <- integer(length(node_id))
  assignment_distance <- numeric(length(node_id))
  for (i in seq_along(node_id)) {
    distance <- haversine(
      latitude[i], longitude[i],
      latitude[representatives], longitude[representatives]
    )
    nearest[i] <- which.min(distance)
    assignment_distance[i] <- distance[nearest[i]]
  }
  if (max(assignment_distance) > radius_m + 1e-7) {
    stop("Radius-cover invariant failed.", call. = FALSE)
  }

  representative_id <- node_id[representatives]
  aggregate_nodes <- data.table::rbindlist(lapply(seq_along(representatives), function(k) {
    members <- which(nearest == k)
    representative <- representatives[k]
    data.table::data.table(
      node_id = representative_id[k],
      longitude = longitude[representative],
      latitude = latitude[representative],
      display_name = display_name[representative],
      raw_node_count = length(members),
      member_weight = sum(member_weight[members]),
      endpoint_activity = sum(activity[members])
    )
  }))
  membership <- data.table::data.table(
    raw_node_id = node_id,
    aggregate_node_id = representative_id[nearest],
    distance_to_representative_m = round(assignment_distance, 3)
  )
  id_map <- stats::setNames(membership$aggregate_node_id, membership$raw_node_id)
  standardized_flows <- data.table::data.table(
    origin_id = unname(id_map[origin_id]),
    destination_id = unname(id_map[destination_id]),
    flow = flow_value
  )
  if (!is.null(parsed_time)) standardized_flows[, time := parsed_time]
  group_columns <- c(if (!is.null(parsed_time)) "time", "origin_id", "destination_id")
  aggregated_flows <- standardized_flows[
    , .(flow = sum(.SD[[1L]])), by = group_columns, .SDcols = "flow"
  ]
  data.table::setorderv(aggregated_flows, group_columns)

  source_total <- sum(flow_value)
  output_total <- sum(aggregated_flows$flow)
  if (!isTRUE(all.equal(source_total, output_total, tolerance = 0))) {
    stop("Total flow changed during aggregation.", call. = FALSE)
  }
  metadata <- list(
    schema_version = 1L,
    method = "activity-prioritized radius cover with nearest-representative assignment",
    radius_m = as.numeric(radius_m),
    timezone = timezone,
    source_nodes = node_source,
    source_flows = flow_source,
    raw_nodes = length(node_id),
    aggregate_nodes = nrow(aggregate_nodes),
    raw_flow_rows = nrow(flows),
    aggregate_flow_rows = nrow(aggregated_flows),
    total_flow = as.numeric(output_total),
    max_assignment_distance_m = max(assignment_distance),
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  result <- structure(
    list(nodes = aggregate_nodes, flows = aggregated_flows,
         membership = membership, metadata = metadata),
    class = "od_aggregation"
  )

  if (!is.null(output_dir)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Install the 'jsonlite' package to write aggregation files.", call. = FALSE)
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_dir <- normalizePath(output_dir, mustWork = TRUE)
    tag <- paste0("r", gsub("[^0-9]+", "_", format(radius_m, scientific = FALSE, trim = TRUE)))
    paths <- c(
      nodes = file.path(output_dir, paste0("nodes_", tag, ".csv")),
      flows = file.path(output_dir, paste0("flows_", tag, ".csv")),
      membership = file.path(output_dir, paste0("membership_", tag, ".csv")),
      metadata = file.path(output_dir, paste0("aggregation_", tag, ".json"))
    )
    if (!overwrite && any(file.exists(paths))) {
      stop("Output already exists; set overwrite=TRUE or choose another directory.", call. = FALSE)
    }
    data.table::fwrite(result$nodes, paths[["nodes"]])
    data.table::fwrite(result$flows, paths[["flows"]])
    data.table::fwrite(result$membership, paths[["membership"]])
    jsonlite::write_json(metadata, paths[["metadata"]], auto_unbox = TRUE,
                         pretty = TRUE, null = "null")
    result$files <- paths
  }
  result
}

utils::globalVariables(c(
  ".", "curve_sign", "destination_latitude", "destination_longitude",
  "flow", "origin_latitude", "origin_longitude", "time"
))
