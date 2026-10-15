.data_ids <- function(x, label) {
  x <- as.character(x)
  if (!length(x) || anyNA(x) || any(!nzchar(x)) || anyDuplicated(x))
    stop(label, " must be nonempty, unique IDs.", call. = FALSE)
  x
}

#' Construct a physical network
#' @param nodes Data frame with unique node_id; optional x and y coordinates.
#' @param edges Data frame with unique edge_id, from and to node IDs. May be empty.
#' @param directed Whether physical edges are directed.
#' @param crs Coordinate reference description, or NULL for nongeographic coordinates.
#' @param provenance List describing the source and transformations.
#' @return A bdynets_network object. Rows retain their supplied order.
#' @export
network_data <- function(nodes, edges = data.frame(edge_id=character(), from=character(), to=character()),
                         directed = TRUE, crs = NULL, provenance = list()) {
  if (!is.data.frame(nodes) || !'node_id' %in% names(nodes)) stop('nodes needs node_id')
  nodes$node_id <- .data_ids(nodes$node_id, 'node_id')
  if (!is.data.frame(edges) || !all(c('edge_id','from','to') %in% names(edges))) stop('Invalid edges')
  edges$edge_id <- if (nrow(edges)) .data_ids(edges$edge_id, 'edge_id') else character()
  edges$from <- as.character(edges$from); edges$to <- as.character(edges$to)
  if (anyNA(edges$from) || anyNA(edges$to) ||
      !all(c(edges$from,edges$to) %in% nodes$node_id)) stop('Unknown edge endpoints')
  if (length(directed)!=1L || is.na(directed) || !is.logical(directed)) stop('Invalid directed')
  if (!is.list(provenance)) stop('provenance must be a list')
  if (xor('x' %in% names(nodes), 'y' %in% names(nodes))) stop('Supply both x and y')
  if ('x' %in% names(nodes) && (!is.numeric(nodes$x) || !is.numeric(nodes$y) ||
      any(!is.finite(nodes$x)) || any(!is.finite(nodes$y)))) stop('Coordinates must be finite')
  structure(list(nodes=nodes,edges=edges,directed=directed,crs=crs,provenance=provenance),class='bdynets_network')
}

#' Construct a time-by-unit observation panel
#' @param values Numeric time-by-unit matrix. NA means unobserved; zero is observed.
#' @param time Unique increasing numeric or POSIXct times, one per row.
#' @param unit_ids Unique IDs in column order. Must match column names if supplied.
#' @param network A bdynets_network object.
#' @param unit_type Whether observations belong to nodes or edges.
#' @param exposure Positive scalar or matrix with exactly the dimensions of values.
#' @param measure,unit,aggregation Explicit measurement name, units and aggregation rule.
#' @param provenance Source metadata. POSIXct timezone is retained in time.
#' @return A bdynets_panel object; missing values are never zero-filled.
#' @export
network_panel <- function(values, time, unit_ids, network, unit_type=c('node','edge'),
                          exposure=1, measure='response', unit='unspecified',
                          aggregation='unspecified', provenance=list()) {
  unit_type <- match.arg(unit_type)
  if (!inherits(network,'bdynets_network')) stop('Invalid network')
  ids <- .data_ids(unit_ids,'unit_ids')
  if (!is.matrix(values) || !is.numeric(values) || !nrow(values) || ncol(values)!=length(ids) ||
      any(is.infinite(values)) || any(is.nan(values))) stop('Invalid values; use NA for missing observations')
  if (!is.null(colnames(values)) && !identical(colnames(values),ids)) stop('Column names must match unit_ids in order')
  if (!(is.numeric(time) || inherits(time,'POSIXct')) || length(time)!=nrow(values) ||
      any(!is.finite(as.numeric(time))) || any(diff(as.numeric(time))<=0)) stop('time must be finite and strictly increasing')
  domain <- if(unit_type=='node') network$nodes$node_id else network$edges$edge_id
  if (!all(ids %in% domain)) stop('Unknown observation units')
  if (!is.numeric(exposure) || any(!is.finite(exposure)) || any(exposure<=0) ||
      !(length(exposure)==1L || (is.matrix(exposure) && identical(dim(exposure),dim(values))))) stop('Invalid exposure')
  if (is.matrix(exposure) && !is.null(colnames(exposure)) && !identical(colnames(exposure),ids)) stop('Exposure IDs disagree')
  if (!is.list(provenance)) stop('provenance must be a list')
  for (s in list(measure,unit,aggregation)) if (!is.character(s) || length(s)!=1L || is.na(s) || !nzchar(s)) stop('Invalid measurement metadata')
  colnames(values) <- ids
  if(length(exposure)==1L) exposure <- matrix(exposure,nrow(values),ncol(values))
  dimnames(exposure) <- dimnames(values)
  structure(list(values=values,time=time,unit_ids=ids,network=network,unit_type=unit_type,
                 exposure=exposure,measure=measure,unit=unit,aggregation=aggregation,
                 provenance=provenance),class='bdynets_panel')
}

#' Convert a panel to a complete long observation table
#' @param x A bdynets_panel.
#' @return Data frame including missing rows and explicit observed flags.
#' @export
panel_long <- function(x) {
  if(!inherits(x,'bdynets_panel')) stop('Invalid panel')
  data.frame(unit_id=rep(x$unit_ids,each=length(x$time)),time=rep(x$time,length(x$unit_ids)),
             value=as.vector(x$values),observed=as.vector(!is.na(x$values)),exposure=as.vector(x$exposure))
}

.panel_complete <- function(x, counts=FALSE) {
  if(!inherits(x,'bdynets_panel')) stop('Invalid panel')
  if(anyNA(x$values)) stop('This model adapter requires complete observations')
  if(counts && any(x$values<0 | x$values!=floor(x$values))) stop('Model requires nonnegative integer counts')
  if(any(x$exposure!=1)) stop('This model API does not support exposure; values have not been rescaled')
}

#' Prepare existing graphMoDE inputs without fitting
#' @param x A complete panel with unit exposure.
#' @param graph A gmde_graph whose IDs exactly match the panel order.
#' @return List with Y (series by time) and graph for gmde_fit().
#' @export
as_gmde_inputs <- function(x,graph) {
  .panel_complete(x)
  if(!inherits(graph,'gmde_graph') || !identical(graph$unit_ids,x$unit_ids)) stop('Graph IDs must match panel order')
  list(Y=t(x$values),graph=graph)
}

#' Prepare existing BDCN inputs without fitting
#' @param x Complete edge-count panel with unit exposure.
#' @param graph Existing BDCN graph with edges, A and W.
#' @param graph_unit_ids Explicit IDs in the BDCN graph edge order.
#' @return List with Y (time by series) and graph. Pass other model arguments separately.
#' @export
as_bdcn_inputs <- function(x,graph,graph_unit_ids) {
  .panel_complete(x,TRUE)
  ids <- .data_ids(graph_unit_ids,'graph_unit_ids')
  if(x$unit_type!='edge' || !identical(ids,x$unit_ids)) stop('BDCN edge IDs must match panel order')
  n <- length(ids)
  if(!is.list(graph) || !is.data.frame(graph$edges) || nrow(graph$edges)!=n ||
      !is.matrix(graph$A) || !is.matrix(graph$W) ||
      !identical(dim(graph$A),c(n,n)) || !identical(dim(graph$W),c(n,n))) stop('Invalid BDCN graph shape')
  list(Y=x$values,graph=graph)
}

#' Prepare one time slice for the existing static LPR model
#' @param x Edge panel. Missing observations at the selected time are omitted.
#' @param time_index One-based row index; required to avoid pooling dynamic observations.
#' @param family Poisson or Bernoulli, as in lpr_data().
#' @return An lpr_data object with node_ids, edge_ids and time metadata added.
#' @export
as_lpr_data <- function(x,time_index,family=c('poisson','bernoulli')) {
  if(!inherits(x,'bdynets_panel') || x$unit_type!='edge') stop('LPR requires an edge panel')
  if(length(time_index)!=1L || !is.finite(time_index) || time_index!=floor(time_index) ||
      time_index<1 || time_index>nrow(x$values)) stop('Invalid time_index')
  edges <- x$network$edges[match(x$unit_ids,x$network$edges$edge_id),,drop=FALSE]
  keep <- !is.na(x$values[time_index,])
  endpoints <- cbind(match(edges$from,x$network$nodes$node_id),match(edges$to,x$network$nodes$node_id))
  out <- lpr_data(x$values[time_index,keep],endpoints[keep,,drop=FALSE],nrow(x$network$nodes),
                  x$exposure[time_index,keep],match.arg(family))
  out$node_ids <- x$network$nodes$node_id; out$edge_ids <- x$unit_ids[keep]; out$time <- x$time[time_index]
  out
}
