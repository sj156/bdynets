# OD preparation and file export for this example.
for (helper in c(
  ".tm_assert",
  ".tm_read_table",
  ".tm_numeric",
  ".tm_crs",
  ".tm_mercator_limit",
  ".tm_od_plot_time",
  ".tm_map_mapped",
  ".od_parse_time",
  ".tm_provenance_for_json"
)) {
  assign(helper, getFromNamespace(helper, "bdynets"))
}
rm(helper)

.tm_mapping <- function(mapping,table,required,allowed,label,aliases=character(),optional_defaults=FALSE) {
  .tm_assert(is.character(mapping)&&!is.null(names(mapping))&&!anyNA(names(mapping))&&!anyDuplicated(names(mapping)),paste(label,'must be a uniquely named character vector'))
  alias <- match(names(mapping),names(aliases));names(mapping)[!is.na(alias)] <- unname(aliases[alias[!is.na(alias)]])
  .tm_assert(!anyDuplicated(names(mapping))&&!length(setdiff(names(mapping),allowed)),paste('Unknown or duplicate names in',label))
  .tm_assert(!length(setdiff(required,names(mapping))),paste('Missing required names in',label))
  for (key in names(mapping)) {
    value <- mapping[[key]]
    if (is.na(value)||!nzchar(value)) {
      .tm_assert(!key %in% required,paste('A column mapping is required for',key))
    } else if (!value %in% names(table)) {
      if (optional_defaults && !key %in% required) mapping[[key]] <- NA_character_
      else stop('Mapped column does not exist: ',value,call.=FALSE)
    }
  }
  mapping
}

.tm_od_input <- function(nodes,flows,node_columns,flow_columns,node_default=FALSE,flow_default=FALSE,size_key='size') {
  nodes <- .tm_read_table(nodes,'nodes');flows <- .tm_read_table(flows,'flows')
  nc <- .tm_mapping(node_columns,nodes,c('id','longitude','latitude'),c('id','longitude','latitude',size_key,if(size_key=='weight')'name'),'node_columns',
    c(node_id='id',lon='longitude',lat='latitude',member_weight=size_key),node_default)
  fc <- .tm_mapping(flow_columns,flows,c('origin','destination'),c('origin','destination','count','time'),'flow_columns',
    c(origin_id='origin',destination_id='destination',flow='count'),flow_default)
  for(key in intersect(c('longitude','latitude','size','weight'),names(nc))) if(!is.na(nc[[key]])&&nzchar(nc[[key]]))
    nodes[[nc[[key]]]] <- .tm_numeric(nodes[[nc[[key]]]],paste('Node',key))
  if ('count'%in%names(fc)&&!is.na(fc[['count']])&&nzchar(fc[['count']])) {
    value <- .tm_numeric(flows[[fc[['count']]]],'Flow counts')
    .tm_assert(all(value>=0 & abs(value-round(value))<=sqrt(.Machine$double.eps)),'Flow counts must be nonnegative integers; weighted OD is unsupported')
    flows[[fc[['count']]]] <- value
  }
  ids <- as.character(nodes[[nc[['id']]]])
  .tm_assert(!anyNA(ids)&&all(nzchar(ids))&&!anyDuplicated(ids),'Node identifiers must be unique and nonmissing')
  .tm_assert(all(abs(nodes[[nc[['longitude']]]])<=180)&&all(abs(nodes[[nc[['latitude']]]])<=.tm_mercator_limit),'Node coordinates are invalid longitude/latitude degrees')
  for(key in c('origin','destination')) .tm_assert(!anyNA(flows[[fc[[key]]]])&&all(as.character(flows[[fc[[key]]]])%in%ids),'Every OD endpoint must match one node identifier')
  list(nodes=nodes,flows=flows,node_columns=nc,flow_columns=fc)
}

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
  max_distance <- max(assignment_distance)
  if (max_distance > radius_m + 1e-7) {
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
    max_assignment_distance_m = max_distance,
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

# A node-table RDS and a combined OD RDS share the same entry point.
.tm_od_read_nodes <- function(nodes) {
  if (is.character(nodes) && length(nodes) == 1L && !is.na(nodes) &&
      file.exists(nodes) && !dir.exists(nodes) &&
      tolower(tools::file_ext(sub("\\.gz$", "", nodes, ignore.case = TRUE))) == "rds") {
    return(readRDS(nodes))
  }
  nodes
}

.tm_od_bundle <- function(x) {
  is.list(x) && !is.data.frame(x) && !is.null(x[["nodes"]]) &&
    !is.null(x[["flows"]])
}

.tm_od_bundle_tables <- function(bundle) {
  unit <- bundle[["unit"]]
  if (!is.null(unit)) {
    .tm_assert(identical(unit, "trips"), "Canonical OD values are integer trip counts; weighted OD and other units are unsupported")
  }
  if (!is.null(bundle$crs)) .tm_crs(bundle$crs)
  list(nodes = bundle$nodes, flows = bundle$flows)
}


#' Select one time window from OD flows
#'
#' The first independent step of an OD map: adapt the node and flow tables,
#' then keep the records inside one explicit window. Nothing about the canvas,
#' basemap or styling is decided here.
#'
#' @param nodes Node table, CSV/RDS path, a standard OD bundle, or a
#'   `traffic_od_data` or `od_aggregation` object.
#' @param flows Flow table or path; omit for OD objects.
#' @param node_columns,flow_columns Column mappings for the two tables.
#' @param start_time,end_time Half-open timestamp window; supply both or neither.
#'   Numeric values are Unix seconds.
#' @param hour_window Two clock hours such as `c(8, 9)`, pooled across dates.
#' @param timezone IANA timezone used to parse timestamps.
#' @param allow_empty Keep a time window containing no records.
#' @return A `bdynets_od_selection`: standardised nodes, kept records and window information.
#' @export
traffic_od_select <- function(nodes,flows=NULL,
    node_columns=c(id="node_id",longitude="longitude",latitude="latitude",size="member_weight"),
    flow_columns=c(origin="origin_id",destination="destination_id",count="flow",time="time"),
    start_time=NULL,end_time=NULL,hour_window=NULL,timezone="UTC",
    allow_empty=FALSE) {
  input <- .od_selection_input(nodes, flows, node_columns, flow_columns, timezone,
    missing(node_columns), missing(flow_columns), missing(timezone))
  .od_select_window(input, start_time, end_time, hour_window, allow_empty)
}

.od_selection_input <- function(nodes, flows = NULL,
    node_columns = c(id = "node_id", longitude = "longitude", latitude = "latitude", size = "member_weight"),
    flow_columns = c(origin = "origin_id", destination = "destination_id", count = "flow", time = "time"),
    timezone = "UTC", node_default = missing(node_columns), flow_default = missing(flow_columns),
    timezone_default = missing(timezone)) {
  nodes <- .tm_od_read_nodes(nodes)
  provenance <- NULL
  if (.tm_od_bundle(nodes)) {
    .tm_assert(is.null(flows),"When nodes is an OD object, omit flows")
    if (node_default && !is.null(nodes$node_columns)) {
      node_columns <- nodes$node_columns
      node_default <- FALSE
    }
    if (flow_default && !is.null(nodes$flow_columns)) {
      flow_columns <- nodes$flow_columns
      flow_default <- FALSE
    }
    if (timezone_default && is.character(nodes$timezone) &&
        length(nodes$timezone) == 1L) timezone <- nodes$timezone
    provenance <- nodes$provenance
    tables <- .tm_od_bundle_tables(nodes)
    flows <- tables$flows
    nodes <- tables$nodes
  }
  input <- .tm_od_input(nodes, flows, node_columns, flow_columns, node_default, flow_default)
  nodes <- input$nodes
  flows <- input$flows
  nc <- input$node_columns
  fc <- input$flow_columns
  size_col <- .tm_map_mapped(nc,"size",nodes)
  count_col <- .tm_map_mapped(fc,"count",flows)
  time_col <- .tm_map_mapped(fc,"time",flows)
  node_data <- data.frame(node_id = as.character(nodes[[nc[["id"]]]]),
    longitude = as.numeric(nodes[[nc[["longitude"]]]]),
    latitude = as.numeric(nodes[[nc[["latitude"]]]]),
    size = if (is.null(size_col)) rep(1, nrow(nodes)) else as.numeric(nodes[[size_col]]),
    stringsAsFactors = FALSE)
  .tm_assert(all(node_data$size >= 0), "Mapped node sizes must be nonnegative and finite")
  origin <- as.character(flows[[fc[["origin"]]]])
  destination <- as.character(flows[[fc[["destination"]]]])
  count <- if (is.null(count_col)) rep(1, nrow(flows)) else as.numeric(flows[[count_col]])
  list(nodes = node_data, origin = origin, destination = destination, count = count,
    time = if (!is.null(time_col)) flows[[time_col]], timezone = timezone,
    provenance = provenance)
}

.od_selection_times <- function(input) {
  .tm_assert(!is.null(input$time), "A mapped time column is required for time filtering")
  time <- .tm_od_plot_time(input$time, input$timezone)
  .tm_assert(!anyNA(time), "The mapped time column contains invalid timestamps")
  time
}

.od_select_window <- function(input, start_time, end_time, hour_window, allow_empty,
    time = NULL) {
  timezone <- input$timezone
  .tm_assert(is.logical(allow_empty) && length(allow_empty) == 1L && !is.na(allow_empty),
    "allow_empty must be TRUE or FALSE")
  .tm_assert(!xor(is.null(start_time),is.null(end_time)),
    "Supply both start_time and end_time, or neither")
  .tm_assert(is.null(hour_window) || (is.null(start_time) && is.null(end_time)),
    "hour_window cannot be combined with start_time or end_time")
  keep <- rep(TRUE, length(input$count))
  start <- end <- NULL
  if (is.null(time) && (!is.null(start_time) || !is.null(hour_window))) {
    time <- .od_selection_times(input)
  }
  if (!is.null(start_time)) {
    start <- .tm_od_plot_time(start_time,timezone)
    end <- .tm_od_plot_time(end_time,timezone)
    .tm_assert(length(start) == 1L && length(end) == 1L && end > start,
      "The mapped timestamps or requested time window are invalid")
    keep <- time >= start & time < end
  } else if (!is.null(hour_window)) {
    .tm_assert(is.numeric(hour_window) && length(hour_window) == 2L &&
      all(is.finite(hour_window)) && all(hour_window == floor(hour_window)) &&
      all(hour_window >= 0) && all(hour_window <= 24) &&
      hour_window[1] != hour_window[2],
      "hour_window must contain two different integer clock hours from 0 to 24")
    hour_start <- hour_window[1] %% 24
    hour_end <- hour_window[2] %% 24
    clock_hour <- as.integer(format(time,"%H",tz=timezone))
    keep <- if (hour_start < hour_end) {
      clock_hour >= hour_start & clock_hour < hour_end
    } else {
      clock_hour >= hour_start | clock_hour < hour_end
    }
  }
  selected <- data.frame(origin_id = input$origin[keep],destination_id = input$destination[keep],
    flow = input$count[keep],stringsAsFactors = FALSE)
  if (!nrow(selected) && !allow_empty) stop(structure(list(
    message="No flows remain in the requested time window",call=NULL,
    nodes=input$nodes,start_time=start,end_time=end,timezone=timezone),
    class=c("traffic_od_empty_window","error","condition")))
  structure(list(nodes = input$nodes,selected = selected,
    start_time = start,end_time = end,hour_window = hour_window,
    timezone = timezone,unit = "trips",provenance = input$provenance,
    records = nrow(selected),flow = sum(selected$flow)),
    class = "bdynets_od_selection")
}

#' Summarise selected OD records into directed pairs
#'
#' The second independent step: aggregate the kept records into one row per
#' directed pair, separate within-node trips, and attach endpoint coordinates.
#'
#' @param selection A `bdynets_od_selection` from [traffic_od_select()].
#' @param top_n Maximum nonloop pairs kept; `Inf` keeps all.
#' @param include_loops Whether within-node flows may be the only result.
#' @param active_nodes_only Whether to keep only endpoints of drawn pairs.
#' @param allow_empty Keep a frame even when there are no nonloop routes.
#' @return A `bdynets_od_pairs`: routes, plotted nodes, totals and counters.
#' @export
traffic_od_pairs <- function(selection,top_n=Inf,include_loops=FALSE,active_nodes_only=TRUE,
    allow_empty=FALSE) {
  .tm_assert(inherits(selection,"bdynets_od_selection"),
    "traffic_od_pairs() needs the object returned by traffic_od_select()")
  .tm_assert(is.logical(allow_empty) && length(allow_empty) == 1L && !is.na(allow_empty),
    "allow_empty must be TRUE or FALSE")
  node_data <- selection$nodes; selected <- selection$selected
  positive <- selected[selected$flow > 0,,drop=FALSE]
  totals <- if (nrow(positive)) stats::aggregate(flow ~ origin_id + destination_id,positive,sum) else positive
  loop_total <- sum(totals$flow[totals$origin_id == totals$destination_id])
  routes <- totals[totals$origin_id != totals$destination_id,,drop=FALSE]
  routes <- routes[order(-routes$flow,routes$origin_id,routes$destination_id),,drop=FALSE]
  active_pairs <- nrow(routes)
  .tm_assert(length(top_n) == 1L && !is.na(top_n) && top_n > 0,
    "top_n must be positive or Inf")
  if (is.finite(top_n)) routes <- utils::head(routes,as.integer(top_n))
  if (!nrow(routes) && !isTRUE(include_loops) && !allow_empty) stop(structure(list(
    message="No nonloop OD flows remain for plotting",call=NULL,
    nodes=node_data,start_time=selection$start_time,
    end_time=selection$end_time,timezone=selection$timezone,selected_records=nrow(selected),
    selected_flow=selection$flow,within_node_flow=loop_total,
    nonloop_flow=sum(totals$flow)-loop_total),
    class=c("traffic_od_no_routes","error","condition")))
  origin_at <- match(routes$origin_id,node_data$node_id)
  destination_at <- match(routes$destination_id,node_data$node_id)
  routes$origin_longitude <- node_data$longitude[origin_at]
  routes$origin_latitude <- node_data$latitude[origin_at]
  routes$destination_longitude <- node_data$longitude[destination_at]
  routes$destination_latitude <- node_data$latitude[destination_at]
  plot_nodes <- node_data
  if (isTRUE(active_nodes_only) && nrow(routes)) {
    active_ids <- unique(c(routes$origin_id,routes$destination_id))
    plot_nodes <- node_data[node_data$node_id %in% active_ids,,drop=FALSE]
  }
  .tm_assert(nrow(plot_nodes) > 0L,"No nodes remain for plotting")
  structure(list(routes = routes,nodes = plot_nodes,all_nodes = node_data,
    totals = totals,active_pairs = active_pairs,within_node_flow = loop_total,
    selected_flow = selection$flow,selected_records = selection$records,
    status = if (!nrow(selected)) "no_records" else if (!nrow(routes)) "no_routes" else "ok",
    start_time = selection$start_time,
    end_time = selection$end_time,hour_window = selection$hour_window,
    timezone = selection$timezone,unit = selection$unit,provenance = selection$provenance,
    include_loops = isTRUE(include_loops)),
    class = "bdynets_od_pairs")
}


.tm_aggregate_od_reference <- aggregate_od_data

#' @export
#' @noRd
aggregate_od_data <- function(
    nodes,
    flows = NULL,
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
  args <- as.list(environment())
  if(inherits(nodes,'traffic_od_data')) {
    .tm_assert(is.null(flows),'When nodes is a traffic_od_data object, omit flows')
    args$flows <- nodes$flows;args$nodes <- nodes$nodes
    if(missing(node_columns)) {
      # Display size is separate from aggregation weight. Keep unit weights
      # unless the caller explicitly maps a weight column.
      args$node_columns <- nodes$node_columns[names(nodes$node_columns)!='size']
    }
    if(missing(flow_columns))args$flow_columns <- nodes$flow_columns
    if(missing(timezone))args$timezone <- nodes$timezone
  }
  sources <- list(nodes=if(is.character(nodes)&&length(nodes)==1L)basename(nodes) else 'in-memory',
    flows=if(is.character(flows)&&length(flows)==1L)basename(flows) else 'in-memory')
  input <- .tm_od_input(args$nodes,args$flows,args$node_columns,args$flow_columns,missing(node_columns),missing(flow_columns),'weight')
  args$nodes <- input$nodes;args$flows <- input$flows
  args$node_columns <- input$node_columns;args$flow_columns <- input$flow_columns
  result <- do.call(.tm_aggregate_od_reference,args)
  result$metadata$source_nodes <- sources$nodes;result$metadata$source_flows <- sources$flows
  if(!is.null(result$files))jsonlite::write_json(result$metadata,result$files[['metadata']],auto_unbox=TRUE,pretty=TRUE,null='null')
  result
}

#' Canonical trip-count OD input
#' @export
#' @noRd
traffic_od_data <- function(nodes,flows=NULL,
    node_columns=c(id='node_id',longitude='longitude',latitude='latitude',size='member_weight'),
    flow_columns=c(origin='origin_id',destination='destination_id',count='flow',time='time'),
    unit='trips',crs='EPSG:4326',timezone='UTC') {
  .tm_crs(crs)
  .tm_assert(identical(unit,'trips'),'Canonical OD values are integer trip counts; weighted OD and other units are unsupported')
  if(inherits(nodes,'od_aggregation')) {
    .tm_assert(is.null(flows),'When nodes is an od_aggregation object, omit flows')
    flows <- nodes$flows;nodes <- nodes$nodes
  }
  input <- .tm_od_input(nodes,flows,node_columns,flow_columns,missing(node_columns),missing(flow_columns))
  size_col <- unname(input$node_columns['size'])
  if(length(size_col)&&!is.na(size_col)&&nzchar(size_col))
    .tm_assert(all(input$nodes[[size_col]]>=0),'Mapped node sizes must be nonnegative and finite')
  time_col <- unname(input$flow_columns['time'])
  if(length(time_col)&&!is.na(time_col)&&nzchar(time_col)) {
    parsed <- .od_parse_time(input$flows[[time_col]],timezone)
    .tm_assert(!anyNA(parsed),'The mapped time column contains invalid timestamps')
    input$flows[[time_col]] <- parsed
  }
  input$source_columns <- list(nodes=input$node_columns,flows=input$flow_columns)
  nc <- input$node_columns;fc <- input$flow_columns
  count_col <- unname(fc['count'])
  input$nodes <- data.frame(node_id=as.character(input$nodes[[nc[['id']]]]),
    longitude=input$nodes[[nc[['longitude']]]],latitude=input$nodes[[nc[['latitude']]]],
    member_weight=if(length(size_col)&&!is.na(size_col)&&nzchar(size_col))input$nodes[[size_col]] else rep(1,nrow(input$nodes)),stringsAsFactors=FALSE)
  canonical_flows <- data.frame(origin_id=as.character(input$flows[[fc[['origin']]]]),
    destination_id=as.character(input$flows[[fc[['destination']]]]),
    flow=if(length(count_col)&&!is.na(count_col)&&nzchar(count_col))input$flows[[count_col]] else rep(1,nrow(input$flows)),stringsAsFactors=FALSE)
  input$node_columns <- c(id='node_id',longitude='longitude',latitude='latitude',size='member_weight')
  input$flow_columns <- c(origin='origin_id',destination='destination_id',count='flow')
  if(length(time_col)&&!is.na(time_col)&&nzchar(time_col)) {
    canonical_flows$time <- input$flows[[time_col]]
    input$flow_columns <- c(input$flow_columns,time='time')
  }
  input$flows <- canonical_flows
  input$unit <- unit;input$crs <- crs;input$timezone <- timezone
  structure(input,class='traffic_od_data')
}

.prep_path <- function(path) {
  if(!is.character(path)||length(path)!=1L||is.na(path)||!nzchar(trimws(path)))
    stop('Supply one nonblank local path.',call.=FALSE)
  path<-path.expand(path)
  if(file.exists(path)||dir.exists(path))return(normalizePath(path,winslash='/',mustWork=TRUE))
  parent<-dirname(path)
  if(basename(path)=='.')return(.prep_path(parent))
  if(basename(path)=='..')return(dirname(.prep_path(parent)))
  if(identical(parent,path))stop('Cannot resolve output path.',call.=FALSE)
  file.path(.prep_path(parent),basename(path))
}

.prep_exists <- function(path) {
  link<-Sys.readlink(path)
  file.exists(path)||dir.exists(path)||(!is.na(link)&&nzchar(link))
}

.prep_destination <- function(path,source_files) {
  if(!is.character(source_files)||anyNA(source_files)||any(!nzchar(source_files)))
    stop('source_files must contain local source paths.',call.=FALSE)
  if(any(!file.exists(source_files)&!dir.exists(source_files)))
    stop('A declared source path does not exist.',call.=FALSE)
  destination<-.prep_path(path)
  for(source in source_files) {
    original<-.prep_path(source)
    compare<-if(.Platform$OS.type=='windows')tolower else identity
    a<-compare(destination);b<-compare(original)
    if(a==b||(dir.exists(original)&&startsWith(a,paste0(b,'/'))))
      stop('The output is a protected source location. Choose a new directory.',call.=FALSE)
  }
  if(.prep_exists(path))stop('Output directory already exists. Choose a new directory.',call.=FALSE)
  destination
}

.prep_sources <- function(paths) {
  paths<-unique(paths[file.exists(paths)&!dir.exists(paths)])
  data.frame(file=basename(paths),bytes=unname(file.info(paths)$size),
    sha256=vapply(paths,digest::digest,character(1),file=TRUE,algo='sha256',serialize=FALSE),
    stringsAsFactors=FALSE,row.names=NULL)
}

.prep_validate_od <- function(input) {
  if (!is.list(input) || is.null(names(input)) || anyDuplicated(names(input)))
    stop('Supply a uniquely named list of prepared tables.', call. = FALSE)
  if (any(c('roads', 'flow', 'speed', 'od') %in% names(input)))
    stop('Flow, Speed and OD inputs are separate formats.', call. = FALSE)
  required<-c('nodes','flows','unit','time_kind')
  allowed<-c(required,'timezone','interval_seconds','crs','node_columns','flow_columns',
    'provenance','title','description','preparation','notes')
  if(!all(required%in%names(input))||any(!names(input)%in%allowed))
    stop('OD inputs require nodes, flows, unit and time_kind; road fields do not apply.',call.=FALSE)
  if(!is.data.frame(input[['nodes']])||!all(c('node_id','longitude','latitude')%in%names(input[['nodes']]))||
     !is.data.frame(input[['flows']])||!all(c('origin_id','destination_id','flow')%in%names(input[['flows']])))
    stop('OD tables require nodes(node_id, longitude, latitude) and flows(origin_id, destination_id, flow).',call.=FALSE)
  if(!is.character(input[['nodes']]$node_id)||!is.character(input[['flows']]$origin_id)||
     !is.character(input[['flows']]$destination_id))stop('OD IDs must be character in both OD tables.',call.=FALSE)
  kind<-input[['time_kind']]
  if(!is.character(kind)||length(kind)!=1L||is.na(kind)||!kind%in%c('instant','interval_start','untimed','unspecified'))
    stop('Declare the OD time_kind.',call.=FALSE)
  time_column<-if(is.null(input[['flow_columns']]))'time' else unname(input[['flow_columns']]['time'])
  timed<-length(time_column)==1L&&!is.na(time_column)&&time_column%in%names(input[['flows']])
  if((kind=='untimed')==timed)stop('OD time_kind must agree with the supplied time column.',call.=FALSE)
  if(timed&&!inherits(input[['flows']][[time_column]],'POSIXt')&&is.null(input[['timezone']]))
    stop('Supply a timezone for text OD timestamps or pass parsed POSIXct instants.',call.=FALSE)
  interval<-input[['interval_seconds']]
  if(!is.null(interval)&&(kind!='interval_start'||!is.numeric(interval)||length(interval)!=1L||
    !is.finite(interval)||interval<=0))stop('OD interval_seconds requires a positive interval_start width.',call.=FALSE)
  args<-input[intersect(names(input),names(formals(traffic_od_data)))]
  if(is.null(args$timezone))args$timezone<-'UTC'
  do.call(traffic_od_data,args)
}

.prep_tables <- function(input) {
  input[c('nodes', 'flows')]
}

.prep_dictionary <- function(tables,input,custom) {
  definitions <- c(node_id='Stable node identifier used by origin and destination.',
    origin_id='Departure node ID joining nodes.node_id.',destination_id='Arrival node ID joining nodes.node_id.',
    time='Observation time. POSIXct values are Unix seconds in CSV; numeric values retain the supplied source clock.',
    flow='Primary value in the declared unit. An observed zero and a missing value have separate meanings.',
    ride_id='Stable identifier for one individual trip.',
    longitude='WGS84 longitude, degrees east.',latitude='WGS84 latitude, degrees north.')
  required <- list(nodes=c('node_id','longitude','latitude'),flows=c('origin_id','destination_id','flow'))
  rows<-lapply(names(tables),function(name) {
    tab<-tables[[name]]
    fields<-names(tab)
    meaning<-unname(definitions[fields])
    meaning[is.na(meaning)]<-paste('Retained source attribute',fields[is.na(meaning)])
    type<-vapply(tab,function(x)if(inherits(x,'POSIXct'))'timestamp_seconds' else if(is.factor(x))'character' else if(is.list(x))'json' else typeof(x),'')
    units<-rep('',length(fields))
    units[fields=='flow']<-input$unit
    units[fields %in% c('longitude','latitude','lon','lat')]<-'degrees'
    units[fields=='time']<-'seconds'
    data.frame(table=name,field=fields,type=unname(type),meaning=meaning,unit=units,
      required=fields%in%required[[name]],stringsAsFactors=FALSE)
  })
  dictionary<-do.call(rbind,rows)
  if(!is.null(custom)) {
    if(!is.data.frame(custom)||!all(c('table','field','meaning')%in%names(custom)))
      stop('Dictionary additions need table, field and meaning.',call.=FALSE)
    key<-paste(dictionary$table,dictionary$field,sep='/')
    custom_key<-paste(custom$table,custom$field,sep='/')
    if(anyDuplicated(custom_key)||any(!custom_key%in%key))stop('Dictionary additions must identify existing fields once.',call.=FALSE)
    at<-match(custom_key,key)
    for(field in intersect(names(custom),names(dictionary)))dictionary[at,field]<-custom[[field]]
  }
  rownames(dictionary)<-NULL
  dictionary
}

.prep_csv <- function(table) {
  lapply(table,function(x) {
    if(inherits(x,'POSIXct'))x<-as.numeric(x)
    if(is.factor(x))x<-as.character(x)
    if(is.list(x))return(vapply(x,function(v)as.character(jsonlite::toJSON(.tm_provenance_for_json(v),auto_unbox=TRUE,null='null',na='null',digits=NA)),''))
    if(is.double(x)) {
      missing<-is.na(x);x<-sprintf('%.17g',x);x[missing]<-NA_character_
    }
    x
  })
}

write_traffic_input <- function(input,output_dir,source_files=character(),dictionary=NULL) {
  target<-.prep_destination(output_dir,source_files)
  parent<-dirname(target);dir.create(parent,recursive=TRUE,showWarnings=FALSE)
  lock<-paste0(target,'.preparation-lock')
  if(!dir.create(lock,showWarnings=FALSE))stop('Another preparation is using this output directory.',call.=FALSE)
  on.exit(unlink(lock,recursive=TRUE,expand=FALSE),add=TRUE)
  .prep_destination(output_dir,source_files)
  stage<-tempfile('prepared-data-',tmpdir=parent)
  if(!dir.create(stage))stop('Cannot create preparation staging directory.',call.=FALSE)
  on.exit(unlink(stage,recursive=TRUE,expand=FALSE),add=TRUE)
  info_before<-file.info(source_files)[,c('size','mtime'),drop=FALSE]
  identities<-.prep_sources(source_files)
  object<-input
  .prep_validate_od(object)
  object$provenance$preparation_sources <- identities
  tables <- .prep_tables(object)
  dictionary <- .prep_dictionary(tables, object, dictionary)
  na_token<-'\\N'
  strings<-unlist(lapply(tables,function(t)unlist(lapply(t,function(x)if(is.character(x))x else character()),use.names=FALSE)),use.names=FALSE)
  while(na_token%in%strings)na_token<-paste0(na_token,'_')
  saveRDS(object,file.path(stage,'input.rds'),compress='xz',version=3)
  for(name in names(tables)) {
    tab<-as.data.frame(.prep_csv(tables[[name]]),stringsAsFactors=FALSE,optional=TRUE)
    utils::write.csv(tab,file.path(stage,paste0(name,'.csv')),row.names=FALSE,na=na_token,fileEncoding='UTF-8')
  }
  utils::write.csv(dictionary,file.path(stage,'data-dictionary.csv'),row.names=FALSE,na='')
  utils::write.csv(identities,file.path(stage,'source-files.csv'),row.names=FALSE)
  metadata<-object[setdiff(names(object),names(tables))]
  metadata$input_type <- 'od'
  metadata$csv<-list(missing=na_token,timestamp='POSIXct times are Unix seconds; numeric times retain the declared source clock.',
                     identifiers='Read identifier fields as character.',data='input.rds preserves original types, geometry, timezones and nested provenance.')
  writeLines(jsonlite::toJSON(.tm_provenance_for_json(metadata),auto_unbox=TRUE,null='null',na='null',digits=NA,pretty=TRUE),file.path(stage,'metadata.json'),useBytes=TRUE)
  files<-sort(list.files(stage))
  utils::write.csv(data.frame(file=files,md5=unname(tools::md5sum(file.path(stage,files)))),file.path(stage,'checksums.csv'),row.names=FALSE)
  if(!identical(info_before,file.info(source_files)[,c('size','mtime'),drop=FALSE]))stop('A source changed during preparation.',call.=FALSE)
  .prep_destination(output_dir,source_files)
  if(!file.rename(stage,target))stop('Cannot publish the completed preparation.',call.=FALSE)
  invisible(file.path(target,'input.rds'))
}
