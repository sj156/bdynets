# Read only a JSON literal on a single assignment line, never execute JavaScript.
.traffic_literal <- function(lines, name) {
  pattern <- paste0('^\\s*const ',name,'\\s*=\\s*')
  line <- grep(pattern,lines,value=TRUE)
  literal <- sub(pattern,'',line)
  literal <- sub(';\\s*$','',literal)
  literal <- literal[vapply(literal,jsonlite::validate,logical(1))]
  if(length(literal)!=1L) stop('Missing or ambiguous HTML literal: ',name)
  jsonlite::fromJSON(literal,simplifyVector=FALSE)
}
.traffic_vector <- function(x) vapply(x,function(v) if(is.null(v)) NA_real_ else as.numeric(v),0)
.traffic_xy <- function(x,latlon=FALSE) {
  a <- do.call(rbind,lapply(x,unlist))
  if(latlon) a <- a[,2:1,drop=FALSE]
  if(!is.matrix(a) || ncol(a)!=2 || any(!is.finite(a)) || any(abs(a[,2])>=90)) stop('Invalid geographic geometry')
  a
}

#' Read embedded traffic data from an existing visualization snapshot
#' @param path Local HTML visualization file. Only JSON literals are parsed;
#'   no scripts are executed and no network requests are made.
#' @param source Beijing saturation or LargeST flow visualization schema.
#' @return A traffic_snapshot retaining source geometry, rounded displayed values,
#'   styling metadata and source-file MD5. This is a visualization artifact, not
#'   raw inference data. LargeST display links are not model adjacency.
#' @export
read_traffic_snapshot <- function(path,source=c('beijing','largest')) {
  source <- match.arg(source)
  if(!requireNamespace('jsonlite',quietly=TRUE)) stop('Install jsonlite to read visualization snapshots')
  lines <- readLines(path,warn=FALSE,encoding='UTF-8')
  get <- function(n) .traffic_literal(lines,n)
  .traffic_decode(get,source,list(file=basename(path),md5=unname(tools::md5sum(path)),representation='saved HTML display precision'))
}

.traffic_decode <- function(get,source,provenance) {
  if(source=='beijing') {
    out <- list(network=get('network'),flow=get('flowByBin'),duration=get('durationByBin'),
      time_meta=get('timeMeta'),bins=unlist(get('timeBins')),bounds=get('bounds'),
      stops=get('congestionStops'),bin_minutes=get('timeBinMinutes'),metric=get('metricKey'))
    if(out$metric!='saturation' || !length(out$network$features)) stop('Expected Beijing saturation snapshot')
    ids <- vapply(out$network$features,function(f) as.character(f$properties$edge_index),'')
    .data_ids(ids,'snapshot edge IDs')
    out$ids <- ids
    out$geometry <- lapply(out$network$features,function(f) .traffic_xy(f$geometry$coordinates))
  } else {
    out <- list(data=get('data'),colors=unlist(get('colors')),initial_index=get('initialTimeIndex')+1L)
    if(!length(out$data$regions)) stop('No LargeST regions')
    for(region in out$data$regions) {
      n <- lengths(lapply(region$sensors,`[[`,'values'))
      if(!length(n) || any(n!=288L) || any(lengths(lapply(region$links,`[[`,'values'))!=288L)) stop('Expected 288 LargeST frames')
      .data_ids(vapply(region$sensors,`[[`,'','id'),'sensor IDs')
    }
  }
  structure(c(list(source=source,provenance=provenance),out),class='traffic_snapshot')
}

.traffic_sparse <- function(entries,ids) {
  out <- numeric(length(ids))
  if(length(entries)) {
    keys <- vapply(entries,function(z) as.character(z[[1]]),'')
    at <- match(keys,ids)
    if(anyNA(at) || anyDuplicated(at)) stop('Unknown or repeated snapshot edge')
    out[at] <- vapply(entries,function(z) as.numeric(z[[2]]),0)
  }
  out
}

#' Extract one traffic frame for reproducible static plotting
#' @param x A traffic_snapshot.
#' @param time_index One-based frame index (1 through 288 for bundled snapshots).
#' @param region LargeST region name, defaulting to its first region.
#' @param mode Beijing current load, change from previous frame, or duration.
#' @return A traffic_frame with geographic polylines, values, exact source color
#'   classes, line widths and provenance. Duration values are minutes. Beijing
#'   absent entries follow the source viewer's zero convention; LargeST nulls
#'   remain NA. First-frame changes are relative to zero, as in the source viewer.
#' @export
traffic_frame <- function(x,time_index=1L,region=NULL,mode=c('current','delta','duration')) {
  if(!inherits(x,'traffic_snapshot')) stop('Invalid snapshot')
  mode <- match.arg(mode)
  n <- if(x$source=='beijing') length(x$bins) else 288L
  if(length(time_index)!=1 || !is.finite(time_index) || time_index!=floor(time_index) || time_index<1 || time_index>n) stop('Invalid time_index')
  if(x$source=='beijing') {
    key <- as.character(x$bins[time_index]); current <- .traffic_sparse(x$flow[[key]],x$ids)
    previous <- if(time_index>1) .traffic_sparse(x$flow[[as.character(x$bins[time_index-1])]],x$ids) else current*0
    value <- switch(mode,current=current,delta=current-previous,duration=.traffic_sparse(x$duration[[key]],x$ids))
    colors <- vapply(x$stops,`[[`,'','color'); breaks <- vapply(x$stops,`[[`,0,'min')
    color <- colors[pmax(1,findInterval(value,breaks))]
    width <- 1+5.3*pmin(1,pmax(0,value)/.8)
    active <- value!=0
    legend <- paste0(round(100*breaks),'%'); legend[length(legend)] <- '80%+'
    if(mode=='delta') {
      active <- abs(value)>=.0001; color <- ifelse(value>0,'#b2182b','#2166ac')
      width <- .95+4.8*pmin(1,abs(value)/.5)
      colors <- c('#2166ac','#b2182b'); legend <- c('Decrease','Increase')
    }
    if(mode=='duration') {
      t <- pmin(1,value/12); color <- ifelse(t>.7,'#b2182b',ifelse(t>.35,'#f46d43','#fdd349'))
      width <- 1+5.2*t; value <- value*x$bin_minutes
      colors <- c('#fdd349','#f46d43','#b2182b'); legend <- c('5-20 min','25-40 min','45+ min')
    }
    start <- x$time_meta[[key]]$start_min
    label <- sprintf('%02d:%02d-%02d:%02d',start%/%60,start%%60,(start+x$bin_minutes)%/%60,(start+x$bin_minutes)%%60)
    out <- list(ids=x$ids,geometry=x$geometry,value=value,color=color,width=width,active=active,
      legend=legend,legend_colors=colors,title=paste('Beijing',mode,label),
      unit=switch(mode,current='Relative load (x_ijt / estimated capacity)',delta='Change in relative load',duration='Consecutive time at load >=80%'),
      bounds=.traffic_xy(x$bounds,TRUE),sensors=NULL,summary=x$time_meta[[key]],zoom=NULL)
  } else {
    if(mode!='current') stop('LargeST snapshot supports current flow only')
    if(is.null(region)) region <- names(x$data$regions)[1]
    if(length(region)!=1 || !region %in% names(x$data$regions)) stop('Unknown region')
    r <- x$data$regions[[region]]; links <- r$links
    value <- vapply(links,function(z) .traffic_vector(z$values)[time_index],0)
    breaks <- unlist(r$breaks)
    cls <- vapply(value,function(v) if(is.na(v)) NA_integer_ else 1L+sum(v>breaks),1L)
    color <- rep('#94a3b8',length(value)); color[!is.na(cls)] <- x$colors[cls[!is.na(cls)]]
    geometry <- lapply(links,function(z) .traffic_xy(z$coords,TRUE))
    # Browser pixel offset at the source region zoom; preserve opposite directions.
    out <- list(ids=vapply(links,function(z) paste(z$from,z$to,sep='->'),''),geometry=geometry,
      direction=vapply(links,`[[`,'','direction'),value=value,color=color,width=rep(2.8,length(value)),active=rep(TRUE,length(value)),
      legend=c(paste0('<= ',breaks[1]),paste(utils::head(breaks,-1),utils::tail(breaks,-1),sep='-'),paste0('> ',utils::tail(breaks,1)),'Missing'),
      legend_colors=c(x$colors,'#94a3b8'),title=paste(region,x$data$date,sprintf('%02d:%02d',((time_index-1)*5)%/%60,((time_index-1)*5)%%60)),
      unit='Flow (source five-minute display values)',bounds=NULL,sensors=r$sensors,breaks=breaks,summary=r$summary,zoom=r$zoom)
  }
  structure(c(out,list(source=x$source,mode=mode,time_index=time_index,provenance=x$provenance)),class='traffic_frame')
}

#' Read standalone extracted traffic visualization data
#' @param path JSON or gzipped JSON file with schema_version, source, provenance
#'   and payload fields. Reads no HTML, executes no scripts, and makes no requests.
#' @return A traffic_snapshot usable by traffic_frame() and traffic_figures().
#'   Source HTML identity is retained alongside the standalone file identity.
#' @export
read_traffic_data <- function(path) {
  if(!requireNamespace('jsonlite',quietly=TRUE)) stop('Install jsonlite')
  con <- if(grepl('\\.gz$',path)) gzfile(path,'rt') else file(path,'rt')
  on.exit(close(con))
  artifact <- jsonlite::fromJSON(paste(readLines(con,warn=FALSE,encoding='UTF-8'),collapse='\n'),simplifyVector=FALSE)
  if(!identical(artifact$schema_version,1L) || !artifact$source %in% c('beijing','largest') || !is.list(artifact$payload)) stop('Unsupported traffic data schema')
  get <- function(name) {
    if(!name %in% names(artifact$payload)) stop('Missing payload field: ',name)
    artifact$payload[[name]]
  }
  provenance <- c(list(file=basename(path),md5=unname(tools::md5sum(path))),artifact$provenance)
  .traffic_decode(get,artifact$source,provenance)
}

#' Extract embedded visualization data once into a standalone data file
#' @param path Existing source HTML visualization.
#' @param source Beijing or LargeST schema.
#' @param file New .json.gz output file; existing files are refused.
#' @return Invisibly the output path. JSON data only are saved, with source MD5;
#'   scripts are never executed. Normal tutorials use read_traffic_data().
#' @export
extract_traffic_data <- function(path,source=c('beijing','largest'),file) {
  source <- match.arg(source)
  if(!requireNamespace('jsonlite',quietly=TRUE)) stop('Install jsonlite')
  if(file.exists(file) || !grepl('\\.json\\.gz$',file)) stop('Supply a new .json.gz output file')
  lines <- readLines(path,warn=FALSE,encoding='UTF-8')
  keys <- if(source=='beijing') c('network','flowByBin','durationByBin','timeMeta','timeBins','bounds','congestionStops','timeBinMinutes','metricKey') else c('data','colors','initialTimeIndex')
  payload <- stats::setNames(lapply(keys,function(k).traffic_literal(lines,k)),keys)
  .traffic_decode(function(k)payload[[k]],source,list())
  artifact <- list(schema_version=1L,source=source,provenance=list(source_html=basename(path),
    source_md5=unname(tools::md5sum(path)),representation='saved HTML display precision; extracted without recalculation'),payload=payload)
  encoded <- jsonlite::toJSON(artifact,auto_unbox=TRUE,null='null',digits=NA)
  restored <- jsonlite::fromJSON(encoded,simplifyVector=FALSE)
  if(!isTRUE(all.equal(restored$payload,payload,check.attributes=TRUE))) stop('Extraction round-trip mismatch')
  dir.create(dirname(file),recursive=TRUE,showWarnings=FALSE)
  con <- gzfile(file,'wt'); on.exit(close(con))
  writeLines(encoded,con,useBytes=TRUE)
  invisible(file)
}
