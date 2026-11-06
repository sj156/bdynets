.gls_neighbor_edge_geometry <- function(graph, lane_offset = 0.055,
                                        endpoint_trim = 0.11,
                                        label_offset = 0.10) {
  edges <- graph$edges
  dx <- edges$xend - edges$x
  dy <- edges$yend - edges$y
  edge_length <- sqrt(dx^2 + dy^2)
  ux <- dx / edge_length
  uy <- dy / edge_length
  nx <- -uy
  ny <- ux
  data.frame(
    edge_id = edges$id,
    edge_name = edges$name,
    x = edges$x + endpoint_trim * ux + lane_offset * nx,
    y = edges$y + endpoint_trim * uy + lane_offset * ny,
    xend = edges$xend - endpoint_trim * ux + lane_offset * nx,
    yend = edges$yend - endpoint_trim * uy + lane_offset * ny,
    label_x = (edges$x + edges$xend) / 2 +
      (lane_offset + label_offset) * nx,
    label_y = (edges$y + edges$yend) / 2 +
      (lane_offset + label_offset) * ny
  )
}

#' Compare the true and GLSS-selected neighborhood of one target edge
#'
#' For an ordered network pair `(a, b)`, `a` is the target edge and `b` is the
#' source edge whose lag may affect it. This function fixes one target edge and
#' displays its true source neighborhood beside the neighborhood retained by
#' joint DSS (GLSS). In the GLSS panel, retained true sources, retained false
#' sources, and missed true sources are shown separately.
#'
#' @param selection Pair-level selection table. It must contain `target`,
#'   `source`, `signal`, `is_active`, and `DSS_selected`; the table written as
#'   `tables/pair_selection.csv` by [gls_write_grid_study()] can be used
#'   directly.
#' @param graph A directed graph returned by [gls_grid()].
#' @param target_edge Target edge number or edge name such as `"e8"`.
#' @param signals Signal values to display. The default uses every positive
#'   signal available in `selection`.
#' @return A list containing the `ggplot` object, a pair-level comparison table,
#'   a signal-level recovery summary, and the resolved target edge.
#' @export
gls_plot_neighbor_recovery <- function(selection, graph, target_edge = 8L,
                                       signals = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this figure.", call. = FALSE)
  }
  required <- c("target", "source", "signal", "is_active", "DSS_selected")
  if (!is.data.frame(selection) || !all(required %in% names(selection))) {
    stop("selection must contain: ", paste(required, collapse = ", "),
         ".", call. = FALSE)
  }
  if (!is.list(graph) || !is.data.frame(graph$nodes) ||
      !is.data.frame(graph$edges)) {
    stop("graph must be a directed GLS graph.", call. = FALSE)
  }
  edges <- graph$edges
  if (is.character(target_edge)) {
    if (length(target_edge) != 1L) {
      stop("target_edge must identify one edge.", call. = FALSE)
    }
    target_id <- match(target_edge, edges$name)
  } else {
    target_id <- suppressWarnings(as.integer(target_edge))
  }
  if (length(target_id) != 1L || is.na(target_id) ||
      !target_id %in% edges$id) {
    stop("target_edge does not identify an edge in graph.", call. = FALSE)
  }

  z <- selection[selection$target == target_id, , drop = FALSE]
  z$signal <- as.numeric(z$signal)
  z$is_active <- as.logical(z$is_active)
  z$DSS_selected <- as.logical(z$DSS_selected)
  if (!nrow(z) || anyNA(z$signal) || anyNA(z$is_active) ||
      anyNA(z$DSS_selected)) {
    stop("The target-edge selection rows are missing or invalid.",
         call. = FALSE)
  }
  available_signals <- sort(unique(z$signal))
  if (is.null(signals)) {
    signals <- available_signals[available_signals > 0]
    if (!length(signals)) signals <- available_signals
  }
  signals <- sort(unique(as.numeric(signals)))
  if (!length(signals) || any(!is.finite(signals))) {
    stop("signals must contain at least one finite value.", call. = FALSE)
  }
  signal_match <- vapply(z$signal, function(value)
    any(abs(value - signals) < 1e-10), logical(1))
  z <- z[signal_match, , drop = FALSE]
  missing_signals <- signals[!vapply(signals, function(value)
    any(abs(value - z$signal) < 1e-10), logical(1))]
  if (length(missing_signals)) {
    stop("Requested signal(s) are absent: ",
         paste(missing_signals, collapse = ", "), ".", call. = FALSE)
  }
  if (any(!z$source %in% edges$id)) {
    stop("selection contains source edge IDs absent from graph.",
         call. = FALSE)
  }

  z$target_edge <- edges$name[match(z$target, edges$id)]
  z$source_edge <- edges$name[match(z$source, edges$id)]
  z$Recovery <- ifelse(
    z$is_active & z$DSS_selected, "True positive",
    ifelse(!z$is_active & z$DSS_selected, "False positive",
      ifelse(z$is_active & !z$DSS_selected, "False negative",
             "True negative"))
  )
  z <- z[order(z$signal, z$source), , drop = FALSE]

  recovery_summary <- do.call(rbind, lapply(signals, function(value) {
    w <- z[abs(z$signal - value) < 1e-10, , drop = FALSE]
    tp <- sum(w$is_active & w$DSS_selected)
    fp <- sum(!w$is_active & w$DSS_selected)
    fn <- sum(w$is_active & !w$DSS_selected)
    precision <- if (tp + fp) tp / (tp + fp) else NA_real_
    recall <- if (tp + fn) tp / (tp + fn) else NA_real_
    f1 <- if (is.finite(precision) && is.finite(recall) &&
              precision + recall > 0) {
      2 * precision * recall / (precision + recall)
    } else if (tp == 0 && tp + fn > 0) 0 else NA_real_
    data.frame(
      signal = value, target_edge = edges$name[target_id],
      true_neighbors = sum(w$is_active),
      GLSS_neighbors = sum(w$DSS_selected), TP = tp, FP = fp, FN = fn,
      precision = precision, recall = recall, F1 = f1
    )
  }))

  panels <- c("True neighborhood", "GLSS neighborhood")
  signal_levels <- sprintf("q = %.2f", signals)
  facet_key <- expand.grid(
    Signal = factor(signal_levels, levels = signal_levels),
    Panel = factor(panels, levels = panels),
    stringsAsFactors = FALSE
  )
  geometry <- .gls_neighbor_edge_geometry(graph)
  background <- merge(facet_key, geometry, by = NULL)
  target_geometry <- geometry[geometry$edge_id == target_id, , drop = FALSE]
  target_geometry <- merge(facet_key, target_geometry, by = NULL)

  truth <- z[z$is_active, c("signal", "source"), drop = FALSE]
  truth$Panel <- "True neighborhood"
  truth$Status <- "True neighbor"
  recovered <- z[z$Recovery != "True negative",
    c("signal", "source", "Recovery"), drop = FALSE]
  recovered$Panel <- "GLSS neighborhood"
  recovered$Status <- recovered$Recovery
  recovered$Recovery <- NULL
  highlighted <- rbind(truth, recovered)
  highlighted$Signal <- factor(sprintf("q = %.2f", highlighted$signal),
                               levels = signal_levels)
  highlighted$Panel <- factor(highlighted$Panel, levels = panels)
  source_geometry <- geometry[match(highlighted$source, geometry$edge_id), ]
  highlighted <- cbind(highlighted, source_geometry)
  highlighted$Status <- factor(highlighted$Status, levels = c(
    "True neighbor", "True positive", "False positive", "False negative"
  ))

  nodes <- graph$nodes
  node_data <- merge(facet_key, data.frame(x = nodes$col, y = nodes$row),
                     by = NULL)
  gg <- asNamespace("ggplot2")
  plot <- gg$ggplot(background) +
    gg$geom_segment(gg$aes(x, y, xend = xend, yend = yend),
      colour = "grey82", linewidth = 0.75,
      arrow = grid::arrow(length = grid::unit(0.075, "cm"), type = "closed"),
      lineend = "round") +
    gg$geom_segment(data = target_geometry,
      gg$aes(x, y, xend = xend, yend = yend),
      colour = "#111111", linewidth = 3.1,
      arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed"),
      lineend = "round") +
    gg$geom_segment(data = highlighted,
      gg$aes(x, y, xend = xend, yend = yend,
             colour = Status, linetype = Status),
      linewidth = 1.65,
      arrow = grid::arrow(length = grid::unit(0.085, "cm"), type = "closed"),
      lineend = "round") +
    gg$geom_point(data = node_data, gg$aes(x, y), shape = 21,
      fill = "white", colour = "grey25", stroke = 0.8, size = 3.0) +
    gg$geom_text(gg$aes(label_x, label_y, label = edge_name),
      colour = "grey25", size = 2.8) +
    gg$facet_grid(Signal ~ Panel, drop = FALSE) +
    gg$scale_colour_manual(values = c(
      "True neighbor" = "#2C7FB8", "True positive" = "#009E73",
      "False positive" = "#E69F00", "False negative" = "#CC79A7"
    )) +
    gg$scale_linetype_manual(values = c(
      "True neighbor" = "solid", "True positive" = "solid",
      "False positive" = "solid", "False negative" = "22"
    )) +
    gg$coord_equal(clip = "off") +
    gg$labs(
      title = paste0("Source-neighborhood recovery for target ",
                     edges$name[target_id]),
      subtitle = paste0(
        "Pair direction is source b -> target a. Black border marks target ",
        edges$name[target_id],
        "; when present, dashed pink sources are true neighbors missed by GLSS."
      ),
      colour = NULL, linetype = NULL, x = NULL, y = NULL
    ) +
    gg$theme_void(base_size = 10.5) +
    gg$theme(
      strip.text = gg$element_text(face = "bold", size = 10),
      strip.background = gg$element_rect(fill = "grey94", colour = "grey55"),
      legend.position = "bottom",
      legend.key.width = grid::unit(1.2, "cm"),
      legend.background = gg$element_rect(fill = "white", colour = NA),
      legend.key = gg$element_rect(fill = "white", colour = NA),
      plot.title = gg$element_text(face = "bold", hjust = 0.5),
      plot.subtitle = gg$element_text(hjust = 0.5, size = 8.5),
      panel.background = gg$element_rect(fill = "white", colour = NA),
      plot.background = gg$element_rect(fill = "white", colour = NA),
      plot.margin = gg$margin(8, 16, 8, 16)
    )

  structure(list(
    plot = plot,
    pairs = z,
    summary = recovery_summary,
    target_edge = target_id,
    target_edge_name = edges$name[target_id],
    signals = signals
  ), class = c("gls_neighbor_recovery", "list"))
}
