#' Build a common network visualization payload
#' @param x A bdynets_panel with node coordinates.
#' @return Versioned list of geometry, time, values, observation mask and measurement
#'   metadata. Values are time by unit; NA remains missing. Limits span all frames.
#'   This payload does not infer model adjacency or transform coordinates.
#' @export
network_view_data <- function(x) {
  if(!inherits(x,'bdynets_panel')) stop('Invalid panel')
  net <- x$network
  if(!all(c('x','y') %in% names(net$nodes))) stop('Visualization requires node x/y coordinates')
  finite <- x$values[is.finite(x$values)]
  limits <- if(length(finite)) range(finite) else c(0,1)
  list(schema_version=1L,nodes=net$nodes,edges=net$edges,directed=net$directed,crs=net$crs,
       unit_type=x$unit_type,unit_ids=x$unit_ids,time=x$time,values=x$values,
       observed=!is.na(x$values),limits=limits,measure=x$measure,unit=x$unit,
       aggregation=x$aggregation,provenance=x$provenance)
}

#' Plot a frame through the shared visualization interface
#' @param x A bdynets_panel.
#' @param time_index One-based frame index.
#' @param main Optional title.
#' @param ... Additional arguments passed to the base plot canvas.
#' @return Invisibly the visualization payload. Missing observations are gray;
#'   all frames use the same color limits. Coordinates are plotted as supplied,
#'   without a map projection or basemap. Edge geometry is endpoint-to-endpoint.
#' @export
plot.bdynets_panel <- function(x,time_index=1L,main=NULL,...) {
  p <- network_view_data(x)
  if(length(time_index)!=1L || !is.finite(time_index) || time_index!=floor(time_index) ||
      time_index<1 || time_index>nrow(p$values)) stop('Invalid time_index')
  palette <- grDevices::colorRampPalette(c('#2166ac','#f7f7f7','#b2182b'))(64)
  v <- p$values[time_index,]; span <- diff(p$limits)
  idx <- if(span==0) rep(32L,length(v)) else 1L+floor(63*(v-p$limits[1])/span)
  colors <- rep('#999999',length(v)); colors[!is.na(v)] <- palette[idx[!is.na(v)]]
  nodes <- p$nodes; edges <- p$edges
  if(is.null(main)) main <- paste(p$measure,format(p$time[time_index]))
  yr <- range(nodes$y); padding <- max(diff(yr), 1e-6)
  graphics::plot(nodes$x,nodes$y,type='n',xlab='x',ylab='y',main=main,asp=1,
                 ylim=c(yr[1],yr[2]+.45*padding),...)
  draw_edges <- function(e,col,lwd) {
    a <- match(e$from,nodes$node_id); b <- match(e$to,nodes$node_id)
    if(p$directed) graphics::arrows(nodes$x[a],nodes$y[a],nodes$x[b],nodes$y[b],col=col,lwd=lwd,length=.08)
    else graphics::segments(nodes$x[a],nodes$y[a],nodes$x[b],nodes$y[b],col=col,lwd=lwd)
  }
  if(nrow(edges)) draw_edges(edges,'grey85',1)
  if(p$unit_type=='edge') draw_edges(edges[match(p$unit_ids,edges$edge_id),,drop=FALSE],colors,3)
  else {
    at <- match(p$unit_ids,nodes$node_id)
    graphics::points(nodes$x[at],nodes$y[at],pch=19,col=colors,cex=1.8)
  }
  graphics::legend('topleft',legend=c(paste('Min',format(p$limits[1]),p$unit),
    paste('Max',format(p$limits[2]),p$unit),'Missing'),col=c(palette[1],palette[64],'#999999'),pch=19,bty='n',cex=.7)
  invisible(p)
}
