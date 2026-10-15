#' Import processed Beijing edge observations
#' @param nodes Processed table with node_id, x_84, y_84.
#' @param links Processed table with edge_index, from_node_id, to_node_id.
#' @param observations Long table with edge_index, time_bin and value_column.
#' @param time_bins Complete ordered bin IDs, including bins without observations.
#' @param time Numeric or POSIXct time corresponding to time_bins.
#' @param value_column Name of measurement column.
#' @param measure,unit,aggregation Measurement metadata; specify entry versus occupancy explicitly.
#' @param absent Either missing (default) or zero for a known sparse-zero export.
#' @return A bdynets_panel. No raw trajectory processing is performed.
#' @export
beijing_panel <- function(nodes,links,observations,time_bins,time,value_column='x_ijt',
                          measure='traffic',unit='vehicles',aggregation='unspecified',
                          absent=c('missing','zero')) {
  absent <- match.arg(absent)
  if(!all(c('node_id','x_84','y_84') %in% names(nodes)) ||
     !all(c('edge_index','from_node_id','to_node_id') %in% names(links)) ||
     !all(c('edge_index','time_bin',value_column) %in% names(observations))) stop('Missing Beijing source columns')
  bins <- .data_ids(time_bins,'time_bins')
  net <- network_data(data.frame(node_id=nodes$node_id,x=nodes$x_84,y=nodes$y_84),
    data.frame(edge_id=links$edge_index,from=links$from_node_id,to=links$to_node_id),
    crs='EPSG:4326',provenance=list(source='Beijing processed network'))
  i <- match(as.character(observations$time_bin),bins)
  j <- match(as.character(observations$edge_index),net$edges$edge_id)
  if(anyNA(i) || anyNA(j) || anyDuplicated(paste(i,j,sep=':'))) stop('Unknown or duplicate observation keys')
  if(!is.numeric(observations[[value_column]])) stop('Measurement column must be numeric')
  values <- matrix(if(absent=='zero') 0 else NA_real_,length(bins),nrow(links))
  values[cbind(i,j)] <- observations[[value_column]]
  network_panel(values,time,net$edges$edge_id,net,'edge',measure=measure,unit=unit,
                aggregation=aggregation,provenance=list(source='Beijing',absent=absent,time_bins=time_bins))
}

#' Import a LargeST sensor matrix using source column identities
#' @param metadata Table with ID, Lat and Lng columns; may be a subset of sensors.
#' @param values Numeric time-by-sensor matrix from the source data.
#' @param sensor_ids IDs in source matrix column order (HDF5 axis0), not positional ID2 guesses.
#' @param time Numeric or POSIXct observation times.
#' @param aggregation Measurement aggregation description.
#' @return A node-observation bdynets_panel in metadata row order. No display links are inferred.
#' @export
largest_panel <- function(metadata,values,sensor_ids,time,aggregation='native five-minute observations') {
  if(!all(c('ID','Lat','Lng') %in% names(metadata))) stop('Missing LargeST metadata columns')
  ids <- .data_ids(sensor_ids,'sensor_ids')
  selected <- .data_ids(metadata$ID,'metadata ID')
  if(!is.matrix(values) || ncol(values)!=length(ids)) stop('Invalid source matrix')
  if(!is.null(colnames(values)) && !identical(colnames(values),ids)) stop('Source column names disagree with sensor_ids')
  at <- match(selected,ids)
  if(anyNA(at)) stop('Metadata sensor missing from source matrix')
  net <- network_data(data.frame(node_id=selected,x=metadata$Lng,y=metadata$Lat),
                      directed=FALSE,crs='EPSG:4326',provenance=list(source='LargeST sensors'))
  values <- values[,at,drop=FALSE]; colnames(values) <- selected
  network_panel(values,time,selected,net,'node',measure='traffic flow',unit='vehicles',
                aggregation=aggregation,provenance=list(source='LargeST',source_sensor_ids=ids))
}

#' Small synthetic examples matching the Beijing and LargeST source schemas
#' @param source Dataset schema to demonstrate. These are not real traffic data.
#' @return A bdynets_panel with zeros and missing observations.
#' @export
network_example <- function(source=c('beijing','largest')) {
  source <- match.arg(source)
  time <- as.POSIXct('2026-01-01 00:00:00',tz='UTC') + c(0,300,600)
  if(source=='beijing') {
    out <- beijing_panel(data.frame(node_id=c('a','b','c'),x_84=c(116.3,116.31,116.32),y_84=c(39.9,39.91,39.9)),
      data.frame(edge_index=c('e1','e2'),from_node_id=c('a','b'),to_node_id=c('b','c')),
      data.frame(edge_index=rep(c('e1','e2'),each=3),time_bin=rep(0:2,2),x_ijt=c(0,4,7,2,NA,3)),
      0:2,time,measure='entry count',aggregation='entries per five-minute bin')
  } else {
    out <- largest_panel(data.frame(ID=c('s2','s1'),Lat=c(34.01,34.02),Lng=c(-118.2,-118.21)),
      matrix(c(2,NA,4,0,8,6),3,2),c('s1','s2'),time)
  }
  out$provenance$synthetic <- TRUE
  out
}
