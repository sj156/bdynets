# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

#' Plot a static, north-up Divvy OD map over Chicago street centre lines
#'
#' @param nodes,flows Aggregated Divvy node and hourly OD tables.
#' @param start_time,end_time Half-open source-local time window [start,end).
#' @param radius_m Aggregation radius, used in labels only.
#' @param timezone Source-local timezone.
#' @param top_n Maximum number of nonloop OD arrows.
#' @param street_file Bundled Chicago street-centre-line RDS file.
#' @param output_file Optional PNG or PDF destination.
#' @param width,height Output dimensions in inches.
#' @return A list containing the ggplot, displayed data and metadata.
#' @export
plot_divvy_od_map <- function(
    nodes,
    flows,
    start_time,
    end_time,
    radius_m,
    timezone = "America/Chicago",
    top_n = 180L,
    street_file = file.path("data", "chicago_streets_divvy.rds"),
    output_file = NULL,
    width = 11,
    height = 9) {
  .divvy_require("ggplot2")
  prepared <- prepare_divvy_od_window(
    nodes, flows, start_time, end_time, timezone = timezone, top_n = top_n
  )
  all_nodes <- data.table::as.data.table(nodes)
  west <- min(all_nodes$longitude); east <- max(all_nodes$longitude)
  south <- min(all_nodes$latitude); north <- max(all_nodes$latitude)
  x_padding <- max((east - west) * .045, .01)
  y_padding <- max((north - south) * .045, .01)
  bounds <- c(
    west = west - x_padding,
    east = east + x_padding,
    south = south - y_padding,
    north = north + y_padding
  )
  streets <- read_divvy_street_background(street_file, bounds = bounds)
  major_streets <- streets[street_class %in% c("1", "2", "3")]
  local_streets <- streets[!street_class %in% c("1", "2", "3")]

  p <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = local_streets,
      ggplot2::aes(x = longitude, y = latitude, group = street_group),
      linewidth = .16,
      colour = "#d9dee5",
      alpha = .78,
      lineend = "round"
    ) +
    ggplot2::geom_path(
      data = major_streets,
      ggplot2::aes(x = longitude, y = latitude, group = street_group),
      linewidth = .32,
      colour = "#c2cad4",
      alpha = .9,
      lineend = "round"
    )
  positive <- prepared$routes[prepared$routes$curve_sign > 0]
  negative <- prepared$routes[prepared$routes$curve_sign < 0]
  route_layer <- function(data, curvature) {
    ggplot2::geom_curve(
      data = data,
      ggplot2::aes(
        x = origin_longitude,
        y = origin_latitude,
        xend = destination_longitude,
        yend = destination_latitude,
        colour = trip_count,
        linewidth = trip_count
      ),
      curvature = curvature,
      alpha = .78,
      lineend = "round",
      arrow = grid::arrow(length = grid::unit(.075, "inches"), type = "closed")
    )
  }
  if (nrow(positive)) p <- p + route_layer(positive, .10)
  if (nrow(negative)) p <- p + route_layer(negative, -.10)
  p <- p +
    ggplot2::geom_point(
      data = prepared$nodes,
      ggplot2::aes(x = longitude, y = latitude, size = sqrt(n_raw_stations)),
      shape = 21,
      fill = "white",
      colour = "#17324d",
      stroke = .45,
      alpha = .9
    ) +
    ggplot2::scale_size_continuous(range = c(1.2, 4.2), guide = "none") +
    ggplot2::scale_colour_gradient(
      low = "#f7b267", high = "#a50026", trans = "sqrt", name = "Trips"
    ) +
    ggplot2::scale_linewidth_continuous(
      range = c(.45, 3.1), trans = "sqrt", name = "Trips", guide = "none"
    )

  x_span <- bounds["east"] - bounds["west"]
  y_span <- bounds["north"] - bounds["south"]
  north_x <- bounds["east"] - .055 * x_span
  p <- p +
    ggplot2::annotate(
      "segment",
      x = north_x,
      xend = north_x,
      y = bounds["north"] - .13 * y_span,
      yend = bounds["north"] - .035 * y_span,
      linewidth = .7,
      colour = "#172033",
      arrow = grid::arrow(length = grid::unit(.14, "inches"), type = "closed")
    ) +
    ggplot2::annotate(
      "text",
      x = north_x,
      y = bounds["north"] - .018 * y_span,
      label = "N",
      fontface = "bold",
      size = 4,
      colour = "#172033"
    )

  meta <- prepared$metadata
  title <- sprintf("Divvy directed OD flows | %s m aggregation", as.integer(radius_m))
  number <- function(x) format(as.integer(x), big.mark = ",", scientific = FALSE)
  subtitle <- sprintf(
    "%s to %s (%s) | %s trips | %s active nonloop routes | showing top %s\n%s within-node trips are retained in the data but excluded from arrows",
    format(meta$start, "%Y-%m-%d %H:%M"),
    format(meta$end, "%Y-%m-%d %H:%M"),
    timezone,
    number(meta$total_trips),
    number(meta$active_nonloop_routes),
    number(meta$displayed_routes),
    number(meta$within_node_trips)
  )
  p <- p +
    ggplot2::coord_quickmap(
      xlim = bounds[c("west", "east")],
      ylim = bounds[c("south", "north")],
      expand = FALSE,
      clip = "on"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = paste(
        "Background: City of Chicago street center lines (Chicago Data Portal).",
        "Curved arrows connect aggregate OD endpoints; they are not observed bicycle paths."
      )
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 16, colour = "#172033"),
      plot.subtitle = ggplot2::element_text(size = 9.5, colour = "#334155",
                                            margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(size = 8, colour = "#52606d", hjust = 0),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = ggplot2::element_text(face = "bold")
    )

  if (!is.null(output_file)) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(output_file, p, width = width, height = height,
                    dpi = 220, bg = "white")
  }
  list(
    plot = p,
    nodes = prepared$nodes,
    routes = prepared$routes,
    metadata = meta,
    bounds = bounds,
    street_file = normalizePath(street_file, mustWork = TRUE),
    output_file = output_file
  )
}

