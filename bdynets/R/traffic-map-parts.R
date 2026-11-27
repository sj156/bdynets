# Shared rendering helpers for prepared OD frames and street context.

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

.tm_od_plot_time <- function(value, timezone) {
  if (is.numeric(value)) value <- as.POSIXct(value, origin = "1970-01-01", tz = "UTC")
  .od_parse_time(value, timezone)
}

#' Derive the map extent from plotted nodes
#'
#' Uses the caller's `bounds` or the node range expanded by `padding`.
#' Supply the output dimensions to plan background coverage for a static OD
#' map using its series extent.
#'
#' @param nodes Standardised node table with longitude and latitude.
#' @param bounds Optional `c(west,east,south,north)`.
#' @param padding Fractional padding around the node range.
#' @param width,height Optional OD map output dimensions in inches. Supply
#'   both to include the canvas expansion at that aspect ratio.
#' @return Named numeric vector `c(west,east,south,north)`.
#' @export
traffic_map_extent <- function(nodes,bounds=NULL,padding=0.045,width=NULL,height=NULL) {
  nodes <- .tm_read_table(nodes,"nodes")
  .tm_assert(all(c("longitude","latitude") %in% names(nodes)),
    "Extent nodes need longitude and latitude columns")
  .tm_assert(is.numeric(padding) && length(padding) == 1L &&
    is.finite(padding) && padding >= 0,"padding must be one nonnegative number")
  if (!is.null(bounds)) {
    .tm_validate_extent(bounds)
    extent <- stats::setNames(as.numeric(bounds),c("west","east","south","north"))
  } else {
    extent <- .tm_extent_coordinates(cbind(.tm_numeric(nodes$longitude, "Node longitude"),
      .tm_numeric(nodes$latitude, "Node latitude")), padding)
  }
  if (is.null(width) && is.null(height)) return(extent)
  .tm_background_viewport(.tm_project_extent(extent), width, height, .tm_od_map_margins)
}

#' Derive the background extents for prepared Flow or Speed frames
#'
#' Use the result to prepare background files before drawing. The extents
#' include the visible canvas around the roads and events at the specified
#' output size. Region columns can be character vectors or factors.
#'
#' @param x Prepared `trafficmap_data` drawing object or its RDS path.
#' @param region Optional exact region to cover. Use `NULL` to cover all records.
#' @param width,height Output dimensions in inches, as passed to the drawing function.
#' @param padding,full_extent Road map layout options. Match the drawing call.
#' @param split_regions With `region = NULL`, prepare one extent per region
#'   plus one for any unassigned roads and events. Set to `FALSE` to prepare
#'   one continuous background for a map that displays all records together.
#' @return An unnamed list of numeric extents, each in west, east, south, north order.
#' @export
traffic_road_extents <- function(x, region = NULL, width = 11, height = 9,
    padding = 0.08, full_extent = TRUE, split_regions = TRUE) {
  .tm_check_padding(padding)
  .tm_check_full_extent(full_extent)
  .tm_assert(is.logical(split_regions) && length(split_regions) == 1L && !is.na(split_regions),
    "split_regions must be TRUE or FALSE")
  x <- read_traffic(x)
  coverage <- function(data, area=NULL) {
    limits <- .tm_road_plot_limits(data, area, full_extent, padding)
    .tm_background_viewport(limits, width, height, .tm_road_map_margins)
  }
  if (!is.null(region) || !split_regions) return(list(coverage(x, region)))
  regions <- .tm_regions(x)
  if (!length(regions)) return(list(coverage(x)))
  boxes <- lapply(regions, function(area) coverage(x, area))
  unassigned <- function(records) {
    labels <- .tm_region_labels(records[["region"]])
    if (is.null(labels)) return(seq_len(NROW(records)))
    which(is.na(labels) | !nzchar(labels))
  }
  roads <- unassigned(x$roads)
  events <- unassigned(x$events)
  if (length(roads) || length(events)) {
    x$roads <- x$roads[roads, , drop=FALSE]
    if (!is.null(x$events)) x$events <- x$events[events, , drop=FALSE]
    boxes <- c(boxes, list(coverage(x)))
  }
  boxes
}

.tm_mercator_limit <- 85.0511287798066
.tm_road_map_margins <- c(3, 1, 4, 1)
.tm_od_map_margins <- c(4.5, 1.2, 5.1, 1.2)

.tm_validate_extent <- function(bounds) {
  .tm_assert(is.numeric(bounds) && length(bounds) == 4L && all(is.finite(bounds)) &&
    diff(bounds[1:2]) > 0 && diff(bounds[1:2]) <= 360 && bounds[3] < bounds[4] &&
    all(abs(bounds[3:4]) <= .tm_mercator_limit),
    "Bounds must be c(west,east,south,north), with up to 360 degrees of longitude and latitudes within Web Mercator limits")
}

.tm_project_extent <- function(extent) {
  corners <- .tm_project(matrix(extent[c(1L, 3L, 2L, 4L)], ncol = 2L, byrow = TRUE))
  c(corners[, 1L], corners[, 2L])
}

# The same aspect expansion is used by the graphics device and background preparation.
.tm_fit_map_viewport <- function(limits, plot_size) {
  span <- c(diff(limits[1:2]), diff(limits[3:4]))
  target <- plot_size * max(span / plot_size)
  extra <- pmax(target - span, 0) / 2
  limits + c(-extra[1L], extra[1L], -extra[2L], extra[2L])
}

.tm_background_viewport <- function(limits, width, height, margins) {
  .tm_assert(is.numeric(width) && length(width) == 1L && is.finite(width) &&
    is.numeric(height) && length(height) == 1L && is.finite(height),
    "Supply finite map width and height in inches")
  # On the standard static devices, one line of margin measures 0.2 inches.
  plot_size <- c(width, height) - 0.2 * c(sum(margins[c(2L, 4L)]), sum(margins[c(1L, 3L)]))
  .tm_assert(all(plot_size > 0), "Map width and height must leave room for the margins")
  .tm_context_coverage(.tm_projected_extent(.tm_fit_map_viewport(limits, plot_size)))
}

.tm_context_coverage <- function(extent) {
  if (diff(extent[1:2]) > 360) extent[1:2] <- mean(extent[1:2]) + c(-180, 180)
  extent[3:4] <- c(max(-.tm_mercator_limit, extent[3L]), min(.tm_mercator_limit, extent[4L]))
  extent
}

.tm_extent_coordinates <- function(coordinates, padding = 0.045, minimum_span = 1e-6) {
  .tm_assert(nrow(coordinates) > 0L && all(is.finite(coordinates)) &&
    all(abs(coordinates[, 1L]) <= 180) && all(abs(coordinates[, 2L]) <= .tm_mercator_limit),
    "Supply finite longitude/latitude coordinates within Web Mercator limits")
  lon <- coordinates[, 1L]
  longitude_range <- range(.tm_longitude_near(lon, .tm_longitude_center(lon)))
  latitude_range <- range(coordinates[, 2L])
  margin <- function(x) {
    span <- max(diff(x), minimum_span)
    (span - diff(x)) / 2 + span * padding
  }
  longitude_margin <- min(margin(longitude_range), (360 - diff(longitude_range)) / 2)
  latitude_margin <- margin(latitude_range)
  stats::setNames(c(longitude_range + c(-1, 1) * longitude_margin,
    max(-.tm_mercator_limit, latitude_range[1L] - latitude_margin),
    min(.tm_mercator_limit, latitude_range[2L] + latitude_margin)),
    c("west", "east", "south", "north"))
}

# Drawing options can locate an existing cache. Acquisition is a separate step.
.tm_require_local_basemap <- function(offline, refresh) {
  .tm_assert(!identical(offline, FALSE) && !isTRUE(refresh),
    "Prepare or refresh street context first with traffic_basemap(), then supply the prepared basemap.")
}

#' Obtain the basemap for one extent
#'
#' Shared by every map. Supply `NULL` for no context, `"auto"` to look
#' for local cached context, or give a prepared FeatureCollection, grouped
#' line table, or table path. Use [traffic_basemap()] before drawing to
#' acquire context for a new extent.
#'
#' @param basemap Basemap specification as described above.
#' @param extent `c(west,east,south,north)`.
#' @param basemap_columns Mappings for a grouped line table.
#' @param cache_dir Cache directory used when context has to be obtained.
#' @param offline,refresh Drawing uses local files. Call [traffic_basemap()] to acquire or refresh context.
#' @param draws Whether a device will actually draw; `"auto"` skips cache
#'   lookup when no device will draw.
#' @return A trafficmap FeatureCollection or `NULL`.
#' @export
traffic_map_base <- function(basemap="auto",extent,
    basemap_columns=c(longitude="longitude",latitude="latitude",group="street_group",class="street_class"),
    cache_dir=file.path("data","basemap-cache"),offline=TRUE,refresh=FALSE,draws=TRUE) {
  .tm_require_local_basemap(offline, refresh)
  .tm_validate_extent(extent)
  extent <- as.numeric(extent)
  if (is.null(basemap)) return(NULL)
  if (is.list(basemap) && !is.data.frame(basemap) && is.null(names(basemap)) == FALSE &&
      any(c("cache_dir","offline","refresh","mode") %in% names(basemap)) &&
      is.null(basemap$type)) {
    .tm_require_local_basemap(basemap$offline, basemap$refresh)
    cache_dir <- if (is.null(basemap$cache_dir)) cache_dir else basemap$cache_dir
    basemap <- if (is.null(basemap$mode)) "auto" else basemap$mode
  }
  if (identical(basemap,"auto")) {
    if (!isTRUE(draws) || !isTRUE(getOption("bdynets.basemap.auto",TRUE))) return(NULL)
    return(tryCatch(traffic_basemap(bounds = extent,cache_dir = cache_dir,offline = TRUE),
      bdynets_basemap_cache_miss = function(e) NULL))
  }
  if (.tm_geojson_path(basemap) || (is.list(basemap) && identical(basemap$type,"FeatureCollection")))
    return(.tm_basemap(basemap))
  if (identical(basemap,"cache")) {
    .tm_assert(dir.exists(cache_dir),"cache_dir must be an existing directory")
    return(traffic_basemap(bounds = extent,cache_dir = cache_dir,
      offline = TRUE))
  }
  x <- .tm_read_table(basemap,"basemap")
  mapping <- .tm_map_mapping(basemap_columns,c("longitude","latitude","group"),
    c("longitude","latitude","group","class"),"basemap_columns")
  lon_col <- .tm_map_mapped(mapping,"longitude",x,TRUE)
  lat_col <- .tm_map_mapped(mapping,"latitude",x,TRUE)
  group_col <- .tm_map_mapped(mapping,"group",x,TRUE)
  if (identical(unname(mapping["class"]), "street_class") &&
      !"street_class" %in% names(x)) mapping["class"] <- NA_character_
  class_col <- .tm_map_mapped(mapping,"class",x)
  line_data <- data.frame(longitude = as.numeric(.tm_numeric(x[[lon_col]], "Basemap longitude")),
    latitude = as.numeric(.tm_numeric(x[[lat_col]], "Basemap latitude")),group = as.character(x[[group_col]]),
    class = if (is.null(class_col)) rep("", nrow(x)) else as.character(x[[class_col]]),
    stringsAsFactors = FALSE)
  .tm_assert(!anyNA(line_data$group) && all(nzchar(line_data$group)),
    "Basemap line groups must be nonmissing")
  .tm_assert(all(is.finite(line_data$longitude)) && all(is.finite(line_data$latitude)) &&
    all(abs(line_data$longitude) <= 180) && all(abs(line_data$latitude) <= .tm_mercator_limit),
    "Basemap coordinates are invalid")
  indices <- split(seq_len(nrow(line_data)),line_data$group)
  intersects <- vapply(indices,function(i) {
    q <- line_data[i,,drop=FALSE]
    longitude <- .tm_longitude_near(q$longitude, mean(extent[1:2]))
    min(longitude) <= extent[2] && max(longitude) >= extent[1] &&
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
  .tm_validate_extent(extent)
  extent <- as.numeric(extent)
  limits <- .tm_project_extent(extent)
  usr <- .tm_map_canvas(limits,margins)
  .tm_static_context(basemap,usr)
  structure(list(extent = extent,limits = limits,usr = usr,par = graphics::par(no.readonly = TRUE)),
    class = "bdynets_map_canvas")
}

#' Draw the OD layer and its legend
#'
#' @param pairs Prepared OD frame (bdynets_od_pairs).
#' @param flow_style Named style list; see [traffic_od_map()].
#' @param legend Whether to draw the legend strip.
#' @return Invisibly, the layer style actually used.
#' @export
traffic_od_layer <- function(pairs,flow_style=list(),legend=TRUE) {
  .tm_assert(inherits(pairs,"bdynets_od_pairs"),
    "traffic_od_layer() needs a prepared OD frame")
  routes <- .tm_od_drawable_routes(pairs$routes)
  flow_right <- if (is.null(flow_style$right)) TRUE else flow_style$right
  scale <- .tm_od_count_scale(routes$flow, flow_style$breaks, flow_style$labels,
    flow_style$palette, flow_right)
  layer_style <- .tm_od_layer_style(routes,pairs$nodes,
    scale$breaks, scale$labels, scale$palette, scale$right,
    if (is.null(flow_style$single_flow_dashed)) TRUE else flow_style$single_flow_dashed,
    if (is.null(flow_style$curvature)) 0.10 else flow_style$curvature,
    if (is.null(flow_style$arrow_length)) 0.065 else flow_style$arrow_length,
    if (is.null(flow_style$node_fill)) "white" else flow_style$node_fill,
    if (is.null(flow_style$node_colour)) "#17324d" else flow_style$node_colour,
    if (is.null(flow_style$node_size)) 0.82 else flow_style$node_size,
    if (is.null(flow_style$scale_node_size)) FALSE else flow_style$scale_node_size)
  .tm_draw_od_layer(routes,pairs$nodes,layer_style,"topleft",legend=isTRUE(legend))
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
