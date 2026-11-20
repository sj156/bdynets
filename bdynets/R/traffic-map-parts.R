# Building blocks for the OD map, so data adaptation, time selection, OD pair
# summarisation and layer drawing stay independent, while the canvas, extent,
# basemap, text, annotations and file output are shared by every map.

#' @noRd
.tm_map_mapping <- function(mapping,required,allowed,label) {
  .tm_assert(is.character(mapping) && !is.null(names(mapping)) &&
    !anyDuplicated(names(mapping)),
    paste0(label, " must be a uniquely named character vector"))
  .tm_assert(!length(setdiff(names(mapping), allowed)),
    paste0("Unknown names in ", label))
  .tm_assert(!length(setdiff(required, names(mapping))),
    paste0("Missing required names in ", label))
  mapping
}

#' @noRd
.tm_map_mapped <- function(mapping,key,table,required=FALSE) {
  value <- unname(mapping[key])
  if (!length(value) || is.na(value) || !nzchar(value)) {
    .tm_assert(!required, paste0("A column mapping is required for '", key, "'"))
    return(NULL)
  }
  .tm_assert(value %in% names(table), paste0("Mapped column does not exist: ", value))
  value
}

#' @noRd
.od_label_window <- function(time_label,timezone) {
  .tm_assert(is.character(time_label) && length(time_label) == 1L &&
    !is.na(time_label) && grepl("^([01]?[0-9]|2[0-3]):00$", time_label),
    "time_label must name a whole clock hour (HH:00); use start_time and end_time for other windows")
  hour <- as.integer(sub(":00$", "", time_label))
  next_hour <- (hour + 1L) %% 24L
  list(hour_window = c(hour, next_hour),
    label = sprintf("%02d:00-%02d:00 (%s)", hour, next_hour, timezone))
}

#' Select one time window from OD flows
#'
#' The first independent step of an OD map: adapt the node and flow tables,
#' then keep the records inside one explicit window. Nothing about the canvas,
#' basemap or styling is decided here.
#'
#' @param nodes Node table, CSV/RDS path, `traffic_od_data` or `od_aggregation`.
#' @param flows Flow table or path; omit for OD objects.
#' @param node_columns,flow_columns Column mappings for the two tables.
#' @param start_time,end_time Half-open timestamp window; supply both or neither.
#' @param hour_window Two clock hours such as `c(8, 9)`, pooled across dates.
#' @param time_label Whole clock-hour shorthand such as `"08:00"`, pooled across dates.
#' @param timezone IANA timezone used to parse timestamps.
#' @return A `bdynets_od_selection`: standardised nodes, kept records, label.
#' @export
traffic_od_select <- function(nodes,flows=NULL,
    node_columns=c(id="node_id",longitude="longitude",latitude="latitude",size="member_weight"),
    flow_columns=c(origin="origin_id",destination="destination_id",count="flow",time="time"),
    start_time=NULL,end_time=NULL,hour_window=NULL,time_label=NULL,timezone="UTC") {
  if (inherits(nodes,"traffic_od_data")) {
    .tm_assert(is.null(flows),"When nodes is a traffic_od_data object, omit flows")
    if (missing(node_columns)) node_columns <- nodes$node_columns
    if (missing(flow_columns)) flow_columns <- nodes$flow_columns
    if (missing(timezone)) timezone <- nodes$timezone
    flows <- nodes$flows; nodes <- nodes$nodes
  } else if (inherits(nodes,"od_aggregation")) {
    .tm_assert(is.null(flows),"When nodes is an od_aggregation object, omit flows")
    flows <- nodes$flows; nodes <- nodes$nodes
  } else if (is.list(nodes) && !is.data.frame(nodes) &&
      all(c("nodes","flows") %in% names(nodes))) {
    .tm_assert(is.null(flows),"When nodes is a named OD list, omit flows")
    bundle <- nodes
    if (missing(timezone) && is.character(bundle$timezone) &&
        length(bundle$timezone) == 1L) timezone <- bundle$timezone
    flows <- bundle$flows; nodes <- bundle$nodes
  }
  nodes <- .tm_read_table(nodes,"nodes"); flows <- .tm_read_table(flows,"flows")
  node_columns <- .tm_map_mapping(node_columns,c("id","longitude","latitude"),
    c("id","longitude","latitude","size"),"node_columns")
  flow_columns <- .tm_map_mapping(flow_columns,c("origin","destination"),
    c("origin","destination","count","time"),"flow_columns")
  id_col <- .tm_map_mapped(node_columns,"id",nodes,TRUE)
  lon_col <- .tm_map_mapped(node_columns,"longitude",nodes,TRUE)
  lat_col <- .tm_map_mapped(node_columns,"latitude",nodes,TRUE)
  size_col <- .tm_map_mapped(node_columns,"size",nodes)
  node_data <- data.frame(node_id = as.character(nodes[[id_col]]),
    longitude = as.numeric(.tm_numeric(nodes[[lon_col]], "Node longitude")),
    latitude = as.numeric(.tm_numeric(nodes[[lat_col]], "Node latitude")),
    size = if (is.null(size_col)) 1 else as.numeric(.tm_numeric(nodes[[size_col]], "Node size")),
    stringsAsFactors = FALSE)
  .tm_assert(!anyNA(node_data$node_id) && all(nzchar(node_data$node_id)) &&
    !anyDuplicated(node_data$node_id),
    "Node identifiers must be unique and nonmissing")
  .tm_assert(all(is.finite(node_data$longitude)) &&
    all(is.finite(node_data$latitude)) &&
    all(abs(node_data$longitude) <= 180) &&
    all(abs(node_data$latitude) <= 85.0511), "Node coordinates are invalid")
  .tm_assert(all(is.finite(node_data$size)) && all(node_data$size >= 0),
    "Mapped node sizes must be nonnegative and finite")

  origin_col <- .tm_map_mapped(flow_columns,"origin",flows,TRUE)
  destination_col <- .tm_map_mapped(flow_columns,"destination",flows,TRUE)
  count_col <- .tm_map_mapped(flow_columns,"count",flows)
  time_col <- .tm_map_mapped(flow_columns,"time",flows)
  origin <- as.character(flows[[origin_col]])
  destination <- as.character(flows[[destination_col]])
  count <- if (is.null(count_col)) rep(1,nrow(flows)) else as.numeric(.tm_numeric(flows[[count_col]], "OD flow"))
  .tm_assert(!anyNA(origin) && !anyNA(destination) &&
    all(origin %in% node_data$node_id) && all(destination %in% node_data$node_id),
    "Every OD endpoint must match one node identifier")
  .tm_assert(all(is.finite(count)) && all(count >= 0),
    "Flow values must be nonnegative and finite")
  .tm_assert(!xor(is.null(start_time),is.null(end_time)),
    "Supply both start_time and end_time, or neither")
  .tm_assert(is.null(hour_window) || (is.null(start_time) && is.null(end_time)),
    "hour_window cannot be combined with start_time or end_time")
  .tm_assert(is.null(time_label) || (is.null(start_time) && is.null(end_time) && is.null(hour_window)),
    "time_label cannot be combined with start_time, end_time or hour_window")
  label_only <- NULL
  if (!is.null(time_label)) {
    spec <- .od_label_window(time_label,timezone)
    hour_window <- spec$hour_window; label_only <- spec$label
  }
  keep <- rep(TRUE,nrow(flows)); window_label <- NULL
  if (!is.null(start_time) || !is.null(hour_window)) {
    .tm_assert(!is.null(time_col),"A mapped time column is required for time filtering")
    time <- .od_parse_time(flows[[time_col]],timezone)
    .tm_assert(!anyNA(time),"The mapped time column contains invalid timestamps")
  }
  if (!is.null(start_time)) {
    start <- .od_parse_time(start_time,timezone)
    end <- .od_parse_time(end_time,timezone)
    .tm_assert(!is.na(start) && !is.na(end) && end > start,
      "The mapped timestamps or requested time window are invalid")
    keep <- time >= start & time < end
    window_label <- paste(format(start,"%Y-%m-%d %H:%M"),"to",
      format(end,"%Y-%m-%d %H:%M"),paste0("(",timezone,")"))
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
    window_label <- if (is.null(label_only)) {
      sprintf("all dates pooled over %02d:00-%02d:00 (%s)",hour_start,hour_end,timezone)
    } else label_only
  }
  selected <- data.frame(origin_id = origin[keep],destination_id = destination[keep],
    flow = count[keep],stringsAsFactors = FALSE)
  if (!nrow(selected)) stop(structure(list(
    message="No flows remain in the requested time window",call=NULL,
    nodes=node_data,time_window=window_label),
    class=c("traffic_od_empty_window","error","condition")))
  structure(list(nodes = node_data,selected = selected,
    time_window = window_label,timezone = timezone,
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
#' @return A `bdynets_od_pairs`: routes, plotted nodes, totals and counters.
#' @export
traffic_od_pairs <- function(selection,top_n=Inf,include_loops=FALSE,active_nodes_only=TRUE) {
  .tm_assert(inherits(selection,"bdynets_od_selection"),
    "traffic_od_pairs() needs the object returned by traffic_od_select()")
  node_data <- selection$nodes; selected <- selection$selected
  totals <- stats::aggregate(flow ~ origin_id + destination_id,selected,sum)
  totals <- totals[totals$flow > 0,,drop=FALSE]
  loop_total <- sum(totals$flow[totals$origin_id == totals$destination_id])
  routes <- totals[totals$origin_id != totals$destination_id,,drop=FALSE]
  routes <- routes[order(-routes$flow,routes$origin_id,routes$destination_id),,drop=FALSE]
  active_pairs <- nrow(routes)
  .tm_assert(length(top_n) == 1L && !is.na(top_n) && top_n > 0,
    "top_n must be positive or Inf")
  if (is.finite(top_n)) routes <- utils::head(routes,as.integer(top_n))
  if (!nrow(routes) && !isTRUE(include_loops)) stop(structure(list(
    message="No nonloop OD flows remain for plotting",call=NULL,
    nodes=node_data,time_window=selection$time_window,selected_records=nrow(selected),
    selected_flow=sum(selected$flow),within_node_flow=loop_total,
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
    selected_flow = sum(selected$flow),
    time_window = selection$time_window),
    class = "bdynets_od_pairs")
}

#' Derive the map extent from plotted nodes
#'
#' Shared by every map: either the caller's `bounds` or the node range expanded
#' by `padding`.
#'
#' @param nodes Standardised node table with longitude and latitude.
#' @param bounds Optional `c(west,east,south,north)`.
#' @param padding Fractional padding around the node range.
#' @return Named numeric vector `c(west,east,south,north)`.
#' @export
traffic_map_extent <- function(nodes,bounds=NULL,padding=0.045) {
  nodes <- .tm_read_table(nodes,"nodes")
  .tm_assert(all(c("longitude","latitude") %in% names(nodes)),
    "Extent nodes need longitude and latitude columns")
  .tm_assert(is.numeric(padding) && length(padding) == 1L &&
    is.finite(padding) && padding >= 0,"padding must be one nonnegative number")
  if (!is.null(bounds)) {
    .tm_assert(is.numeric(bounds) && length(bounds) == 4L && all(is.finite(bounds)) &&
      bounds[1] < bounds[2] && bounds[3] < bounds[4],
      "bounds must be c(west,east,south,north)")
    return(stats::setNames(as.numeric(bounds),c("west","east","south","north")))
  }
  longitude_range <- range(.tm_numeric(nodes$longitude,"Node longitude"))
  latitude_range <- range(.tm_numeric(nodes$latitude,"Node latitude"))
  longitude_span <- max(diff(longitude_range),1e-6)
  latitude_span <- max(diff(latitude_range),1e-6)
  stats::setNames(c(longitude_range[1] - padding * longitude_span,
    longitude_range[2] + padding * longitude_span,
    latitude_range[1] - padding * latitude_span,
    latitude_range[2] + padding * latitude_span),c("west","east","south","north"))
}

#' Obtain the basemap for one extent
#'
#' Shared by every map. `basemap` says *how* context is obtained; the extent
#' says *where*. Accepted forms: `NULL` (no context), `"auto"` (read the cache
#' for this extent and fetch only when a device is going to draw), a list with
#' `cache_dir`, `offline` and `refresh` fields, an already loaded
#' FeatureCollection, a grouped line table, or an RDS/CSV path.
#'
#' @param basemap Basemap specification as described above.
#' @param extent `c(west,east,south,north)`.
#' @param basemap_columns Mappings for a grouped line table.
#' @param cache_dir Cache directory used when context has to be obtained.
#' @param offline Whether cached context must not be refreshed from Overpass.
#' @param refresh Whether to ignore cached content.
#' @param draws Whether a device will actually draw; `"auto"` returns `NULL`
#'   when nothing is drawn, so dry runs never touch network or disk.
#' @return A trafficmap FeatureCollection or `NULL`.
#' @export
traffic_map_base <- function(basemap="auto",extent,
    basemap_columns=c(longitude="longitude",latitude="latitude",group="street_group",class="street_class"),
    cache_dir=file.path("data","basemap-cache"),offline=FALSE,refresh=FALSE,draws=TRUE) {
  .tm_assert(is.numeric(extent) && length(extent) == 4L && all(is.finite(extent)),
    "extent must be c(west,east,south,north)")
  extent <- as.numeric(extent)
  if (is.null(basemap)) return(NULL)
  if (is.list(basemap) && !is.data.frame(basemap) && is.null(names(basemap)) == FALSE &&
      any(c("cache_dir","offline","refresh","mode") %in% names(basemap)) &&
      is.null(basemap$type)) {
    cache_dir <- if (is.null(basemap$cache_dir)) cache_dir else basemap$cache_dir
    offline <- if (is.null(basemap$offline)) offline else basemap$offline
    refresh <- if (is.null(basemap$refresh)) refresh else basemap$refresh
    basemap <- if (is.null(basemap$mode)) "auto" else basemap$mode
  }
  if (identical(basemap,"auto")) {
    if (!isTRUE(draws) || !isTRUE(getOption("bdynets.basemap.auto",TRUE))) return(NULL)
    return(traffic_basemap(bounds = extent,cache_dir = cache_dir,
      offline = offline,refresh = refresh))
  }
  if (is.list(basemap) && identical(basemap$type,"FeatureCollection"))
    return(.tm_basemap(basemap))
  if (identical(basemap,"cache")) {
    .tm_assert(dir.exists(cache_dir),"cache_dir must be an existing directory")
    return(traffic_basemap(bounds = extent,cache_dir = cache_dir,
      offline = TRUE,refresh = refresh))
  }
  x <- .tm_read_table(basemap,"basemap")
  mapping <- .tm_map_mapping(basemap_columns,c("longitude","latitude","group"),
    c("longitude","latitude","group","class"),"basemap_columns")
  lon_col <- .tm_map_mapped(mapping,"longitude",x,TRUE)
  lat_col <- .tm_map_mapped(mapping,"latitude",x,TRUE)
  group_col <- .tm_map_mapped(mapping,"group",x,TRUE)
  class_col <- .tm_map_mapped(mapping,"class",x)
  line_data <- data.frame(longitude = as.numeric(.tm_numeric(x[[lon_col]], "Basemap longitude")),
    latitude = as.numeric(.tm_numeric(x[[lat_col]], "Basemap latitude")),group = as.character(x[[group_col]]),
    class = if (is.null(class_col)) "" else as.character(x[[class_col]]),
    stringsAsFactors = FALSE)
  .tm_assert(!anyNA(line_data$group) && all(nzchar(line_data$group)),
    "Basemap line groups must be nonmissing")
  .tm_assert(all(is.finite(line_data$longitude)) && all(is.finite(line_data$latitude)) &&
    all(abs(line_data$longitude) <= 180) && all(abs(line_data$latitude) <= 85.0511),
    "Basemap coordinates are invalid")
  indices <- split(seq_len(nrow(line_data)),line_data$group)
  intersects <- vapply(indices,function(i) {
    q <- line_data[i,,drop=FALSE]
    min(q$longitude) <= extent[2] && max(q$longitude) >= extent[1] &&
      min(q$latitude) <= extent[4] && max(q$latitude) >= extent[3]
  },logical(1))
  indices <- indices[intersects]
  features <- lapply(indices,function(i) {
    coordinates <- as.matrix(line_data[i,c("longitude","latitude"),drop=FALSE])
    if (nrow(coordinates) < 2L) return(NULL)
    list(type = "Feature",
      geometry = list(type = "LineString",coordinates = coordinates),
      properties = list(class = line_data$class[i[1L]]))
  })
  features <- Filter(Negate(is.null),features)
  .tm_basemap(list(type = "FeatureCollection",features = features,
    attribution = "Basemap source unspecified"))
}

#' Open a map canvas for one extent
#'
#' Shared by every map: projects the extent, sets the margins and draws the
#' basemap underneath everything else.
#'
#' @param extent Result of [traffic_map_extent()].
#' @param basemap Result of [traffic_map_base()].
#' @param margins Plot margins in lines.
#' @return A `bdynets_map_canvas`: projected limits and plotting region.
#' @export
traffic_map_canvas <- function(extent,basemap=NULL,margins=c(3.6,1.2,5.1,1.2)) {
  extent <- as.numeric(extent)
  projected_extent <- .tm_project(matrix(c(extent[1],extent[3],extent[2],extent[4]),
    ncol = 2,byrow = TRUE))
  limits <- c(projected_extent[1,1],projected_extent[2,1],
    projected_extent[1,2],projected_extent[2,2])
  usr <- .tm_map_canvas(limits,margins)
  .tm_static_context(basemap,usr)
  structure(list(extent = extent,limits = limits,usr = usr,par = graphics::par(no.readonly = TRUE)),
    class = "bdynets_map_canvas")
}

#' Draw the OD layer and its legend
#'
#' @param pairs Result of [traffic_od_pairs()].
#' @param flow_style Named style list; see [traffic_od_map()].
#' @param legend Whether to draw the legend strip.
#' @return Invisibly, the layer style actually used.
#' @export
traffic_od_layer <- function(pairs,flow_style=list(),legend=TRUE) {
  .tm_assert(inherits(pairs,"bdynets_od_pairs"),
    "traffic_od_layer() needs the object returned by traffic_od_pairs()")
  flow_right <- if (is.null(flow_style$right)) TRUE else flow_style$right
  layer_style <- .tm_od_layer_style(pairs$routes,pairs$nodes,
    if (is.null(flow_style$breaks)) c(1,3,5,7) else flow_style$breaks,
    if (is.null(flow_style$labels)) c("1","2-3","4-5","6-7",">= 8") else flow_style$labels,
    if (is.null(flow_style$palette)) c("#69afe0","#a1c181","#e9c46a","#f4a261","#e76f51") else flow_style$palette,
    flow_right,
    if (is.null(flow_style$single_flow_dashed)) TRUE else flow_style$single_flow_dashed,
    if (is.null(flow_style$curvature)) 0.10 else flow_style$curvature,
    if (is.null(flow_style$arrow_length)) 0.065 else flow_style$arrow_length,
    if (is.null(flow_style$node_fill)) "white" else flow_style$node_fill,
    if (is.null(flow_style$node_colour)) "#17324d" else flow_style$node_colour,
    if (is.null(flow_style$node_size)) 0.82 else flow_style$node_size,
    if (is.null(flow_style$scale_node_size)) FALSE else flow_style$scale_node_size)
  .tm_draw_od_layer(pairs$routes,pairs$nodes,layer_style,"topleft",legend=isTRUE(legend))
  invisible(layer_style)
}

#' Add the shared title and caption
#'
#' @param title,subtitle,caption Map text.
#' @param window_label Optional window text appended when no subtitle is given.
#' @return Invisibly, the title actually drawn.
#' @export
traffic_map_caption <- function(title,subtitle=NULL,caption=NULL,window_label=NULL) {
  map_title <- if (is.null(subtitle) || !nzchar(subtitle)) title else paste(title,subtitle,sep = "\n")
  if (is.null(subtitle) && !is.null(window_label)) map_title <- paste(title,window_label,sep = "\n")
  graphics::title(main = map_title,cex.main = 0.9)
  if (!is.null(caption) && nzchar(caption))
    graphics::mtext(caption,side = 1,line = 2.2,cex = 0.62,col = "#56636a")
  invisible(map_title)
}

#' @export
traffic_map_north_arrow <- function() {
  .tm_map_north_arrow()
  invisible(TRUE)
}

#' @export
traffic_map_scale_bar <- function() {
  .tm_map_scale_bar()
  invisible(TRUE)
}

#' Write a map to PNG or PDF
#'
#' Shared by every map; `draw` controls whether the current device also draws.
#'
#' @param draw_function Function that draws the finished map.
#' @param output_file Optional PNG/PDF path.
#' @param width,height,dpi Output size.
#' @param draw Whether to draw on the current device.
#' @return Invisibly, the written path or `NULL`.
#' @export
traffic_map_output <- function(draw_function,output_file=NULL,width=11,height=9,dpi=220,draw=TRUE) {
  .tm_assert(is.function(draw_function),"draw_function must be a function")
  .tm_map_device(draw_function,output_file,width,height,dpi,draw)
}
