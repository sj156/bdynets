# Input reading and normalization
.tm_assert <- function(ok,message) if(!isTRUE(ok))stop(message,call.=FALSE)
.tm_palette <- c('#16a43a','#7ad151','#f7ea2a','#ffc928','#f5a623','#f36b2b','#f01818')
.tm_time <- function(time) {
  .tm_assert((is.numeric(time)||inherits(time,'POSIXct'))&&!anyNA(time)&&all(is.finite(as.numeric(time))),
    'Time must contain finite numeric seconds or POSIXct timestamps')
  list(value=as.numeric(time),clock=if(inherits(time,'POSIXct'))'absolute' else 'source_clock')
}

# Convert the stored clock to display labels without changing any frame key.
.tm_display_timezone <- function(timezone) {
  if (is.null(timezone)) return("UTC")
  .tm_assert(is.character(timezone) && length(timezone) == 1L &&
    !is.na(timezone) && timezone %in% c("UTC", "GMT", OlsonNames()),
    "timezone must be an IANA timezone or NULL")
  timezone
}

.tm_elapsed_labels <- function(times) {
  seconds <- abs(times)
  whole <- floor(seconds)
  hours <- floor(whole / 3600)
  minutes <- floor(whole %% 3600 / 60)
  labels <- paste0(ifelse(times < 0, "-", ""), sprintf("%02.0f:%02.0f", hours, minutes))
  if (!any(seconds %% 60 != 0)) return(labels)
  paste0(labels, sprintf(":%02.0f", whole %% 60), .tm_clock_fractions(seconds))
}

.tm_format_zoned <- function(instants, timezone, label_format) {
  times <- as.numeric(instants)
  whole <- as.POSIXlt(as.POSIXct(floor(times), origin = "1970-01-01", tz = timezone), tz = timezone)
  fraction <- .tm_clock_fractions(abs(times), pmax(abs(times), whole$sec))
  negative <- which(times < 0 & nzchar(fraction))
  fraction[negative] <- vapply(fraction[negative], .tm_fraction_complement, character(1L))
  text <- paste0(format(whole, format = label_format, tz = timezone), fraction)
  if (timezone %in% c("UTC", "GMT")) text else paste(text, format(whole, "%z", tz = timezone))
}

.tm_frame_labels <- function(times, clock, timezone = NULL, label_format = NULL) {
  timezone <- .tm_display_timezone(timezone)
  if (!is.null(label_format)) {
    .tm_assert(is.character(label_format) && length(label_format) == 1L &&
      !is.na(label_format) && nzchar(label_format), "time_label_format must be one nonblank string")
  }
  if (identical(clock, "absolute") || !is.null(label_format)) {
    encoded <- as.POSIXct(times, origin = "1970-01-01", tz = timezone)
    if (is.null(label_format)) return(.tm_format_zoned(encoded, timezone, "%Y-%m-%d %H:%M:%S"))
    return(format(encoded, format = label_format, tz = timezone))
  }
  .tm_elapsed_labels(times)
}

.tm_flow_display_time <- function(x, time_label_format = NULL, timezone = NULL) {
  if (!is.null(time_label_format)) x$time_label_format <- time_label_format
  if (!is.null(timezone)) x$timezone <- .tm_display_timezone(timezone)
  x$time_labels <- .tm_frame_labels(x$times, x$clock, x$timezone, x$time_label_format)
  x
}


.tm_geojson_path <- function(path) {
  is.character(path) && length(path) == 1L && !is.na(path) &&
    grepl("[.](geojson|json)([.]gz)?$", path, ignore.case = TRUE)
}

.tm_basemap <- function(base) {
 if(is.null(base))return(NULL)
 if (.tm_geojson_path(base)) {
   path <- path.expand(base)
   .tm_assert(file.exists(path) && !dir.exists(path), 'Supply a local GeoJSON file')
   base <- jsonlite::read_json(path, simplifyVector = FALSE)
 }
 .tm_assert(is.list(base)&&identical(base$type,'FeatureCollection')&&is.list(base$features),
  'basemap must be a GeoJSON FeatureCollection')
 valid <- function(g,n) is.matrix(g)&&is.numeric(g)&&ncol(g)==2L&&nrow(g)>=n&&
  all(is.finite(g))&&all(abs(g[,1])<=180)&&all(abs(g[,2])<=.tm_mercator_limit)
 pair <- function(p) {
   if (is.list(p) && all(vapply(p, function(value)
       is.numeric(value) && length(value) == 1L, logical(1)))) {
     unlist(p, use.names = FALSE)
   } else p
 }
 vertices <- function(points) {
   if (is.list(points) && length(points)) {
     points <- lapply(points, pair)
     .tm_assert(all(vapply(points, function(p)
       is.numeric(p) && length(p) == 2L, logical(1))), 'Invalid context coordinates or polygon ring')
     do.call(rbind, points)
   } else points
 }
 base$features <- lapply(base$features, function(f) {
  .tm_assert(is.list(f)&&identical(f$type,'Feature')&&is.list(f$geometry),'Invalid context feature')
  g<-f$geometry
  .tm_assert(is.character(g$type)&&length(g$type)==1L&&g$type%in%c('LineString','Polygon','Point'),
   'Context geometry must be LineString, Polygon or Point')
  g$coordinates <- switch(g$type, LineString = vertices(g$coordinates),
    Polygon = if (is.list(g$coordinates)) lapply(g$coordinates, vertices),
    Point = pair(g$coordinates))
  ok<-switch(g$type,LineString=valid(g$coordinates,2L),
   Polygon=is.list(g$coordinates)&&length(g$coordinates)>0L&&all(vapply(g$coordinates,
    function(r)valid(r,4L)&&all(r[1,]==r[nrow(r),]),logical(1))),
   Point=is.numeric(g$coordinates)&&length(g$coordinates)==2L&&valid(matrix(g$coordinates,1),1L))
  .tm_assert(ok,'Invalid context coordinates or polygon ring')
  .tm_assert(is.null(f$properties)||is.list(f$properties),'Context properties must be a list')
  label<-f$properties$label
  .tm_assert(is.null(label)||(is.character(label)&&length(label)==1L&&!is.na(label)),
   'Context label must be one string')
  f$geometry <- g
  f
 })
 if(is.null(base$attribution))base$attribution<-'Basemap source unspecified'
 .tm_assert(is.character(base$attribution)&&length(base$attribution)==1L&&!is.na(base$attribution)&&
  nzchar(base$attribution),'Context attribution must be one nonempty string')
 # GeoJSON features form an array, including when a street table names its groups.
 base$features<-unname(base$features)
 base
}

#' @export
#' @noRd
flow_scale <- function(values) {
  .tm_observation_values(values, 'Flow')
  v <- values[!is.na(values)]
  if(!length(v))return(list(kind='missing',maximum=NA_real_,breaks=numeric(),palette=.tm_palette))
  maximum <- max(v)
  if(maximum==0)return(list(kind='zero',maximum=0,breaks=numeric(),palette=.tm_palette))
  list(kind='log1p',maximum=maximum,
    breaks=expm1(seq(0,log1p(maximum),length.out=8L)[2:7]),palette=.tm_palette)
}
.tm_road_kind <- function(x) {
  if (identical(x$input_type, "speed")) "speed" else "flow"
}

.tm_road_label <- function(x) {
  if (!is.null(x$measurement$label)) return(x$measurement$label)
  if (.tm_road_kind(x) == "speed") "Speed" else "Flow"
}

.tm_road_title <- function(x) {
  if (!is.null(x[["title"]])) return(x[["title"]])
  paste("Traffic", .tm_road_kind(x), "map")
}

.tm_road_display <- function(x) {
  if (identical(x$input_type, "speed")) {
    .tm_observation_values(x$values, "Speed")
    .tm_assert(x$unit %in% c("mph", "km/h"), "Speed unit must be mph or km/h.")
    breaks <- c(35, 40, 45, 50, 55, 60)
    if (x$unit == "km/h") breaks <- breaks * 1.609344
    limits <- format(breaks, trim = TRUE, scientific = FALSE)
    labels <- c(paste("<", limits[1L]),
      paste(utils::head(limits, -1L), "to <", limits[-1L]),
      paste(">=", utils::tail(limits, 1L)))
    x$scale <- list(kind = "classes", breaks = breaks, labels = labels,
      right = FALSE, palette = rev(.tm_palette),
      maximum = if (all(is.na(x$values))) NA_real_ else max(x$values, na.rm = TRUE))
  } else if (is.null(x$measurement)) {
    x$scale <- flow_scale(x$values)
  }
  x$title <- .tm_road_title(x)
  if (is.null(x$description)) x$description <- ""
  .tm_flow_display_time(x)
}
.tm_observation_values <- function(values, label) {
  .tm_assert(is.numeric(values) && !any(is.infinite(values)) && !any(is.nan(values)) &&
    all(values[!is.na(values)] >= 0), paste(label, 'must be nonnegative and finite or NA'))
}
.tm_colors <- function(values,scale) {
  out <- rep('#94a3b8',length(values));valid <- which(!is.na(values))
  if(length(valid)) {
   if(scale$kind=='change')out[valid]<-scale$palette[ifelse(values[valid]<0,1L,ifelse(values[valid]>0,3L,2L))]
   else if(scale$kind=='classes')out[valid]<-scale$palette[findInterval(values[valid],scale$breaks,left.open=scale$right)+1L]
   else if(scale$kind=='gradient') {
    stops<-scale$breaks;v<-pmin(utils::tail(stops,1),pmax(stops[1],values[valid]));i<-pmin(length(stops)-1L,findInterval(v,stops));u<-(v-stops[i])/(stops[i+1L]-stops[i]);rgb<-grDevices::col2rgb(scale$palette)
    result<-rgb[,i,drop=FALSE]+sweep(rgb[,i+1L,drop=FALSE]-rgb[,i,drop=FALSE],2,u,'*')
    out[valid]<-grDevices::rgb(floor(result[1,]+.5),floor(result[2,]+.5),floor(result[3,]+.5),maxColorValue=255)
   } else out[valid]<-scale$palette[findInterval(log1p(values[valid]),log1p(scale$breaks))+1L]
  }
  out
}


.tm_read_table <- function(x, label) {
  if (is.character(x) && length(x)==1L) {
    .tm_assert(file.exists(x),paste('Missing',label,'file:',x))
    ext <- tolower(tools::file_ext(sub('\\.gz$','',x)))
    x <- switch(ext,rds=readRDS(x),csv=as.data.frame(data.table::fread(x,colClasses='character',showProgress=FALSE)),
      stop(label,' must be a table or CSV/CSV.GZ/RDS path',call.=FALSE))
  }
  .tm_assert(is.data.frame(x),paste(label,'must be tabular data'))
  as.data.frame(x,stringsAsFactors=FALSE)
}
.tm_numeric <- function(x,label,allow_na=FALSE) {
  if (is.factor(x)) x <- as.character(x)
  .tm_assert(is.numeric(x)||is.character(x),paste(label,'must be numeric'))
  out <- if(is.numeric(x)) x else suppressWarnings(as.numeric(x))
  .tm_assert(all(is.finite(out)|(allow_na & is.na(x))),paste(label,'must contain finite numeric values'))
  out
}
.tm_crs <- function(crs) .tm_assert(is.character(crs)&&length(crs)==1L&&!is.na(crs)&&crs %in% c('EPSG:4326','WGS84','OGC:CRS84'),
  'Unsupported CRS; supply WGS84 longitude/latitude degrees (EPSG:4326); no transformation is performed')
.tm_source_scale <- function(config,values) {
 if(is.null(config))return(NULL)
 .tm_assert(is.list(config)&&is.character(config$kind)&&length(config$kind)==1L,'A supplied scale needs one kind')
 if(config$kind=='log1p')return(flow_scale(values))
 .tm_assert(config$kind%in%c('gradient','classes'),'Supplied scale kind must be gradient, classes or log1p')
 palette<-config$palette
 .tm_assert(is.character(palette)&&length(palette)>0L&&!anyNA(palette),'A supplied scale requires colors')
 .tm_assert(!inherits(try(grDevices::col2rgb(palette),silent=TRUE),'try-error'),'Invalid supplied colors')
 palette<-apply(grDevices::col2rgb(palette),2,function(v)tolower(grDevices::rgb(v[1],v[2],v[3],maxColorValue=255)))
 if(config$kind=='gradient') {
  b<-config$stops
  .tm_assert(is.numeric(b)&&length(b)>=2L&&length(b)==length(palette)&&all(is.finite(b))&&all(diff(b)>0),'Gradient colors require matching, strictly increasing source stops')
  v<-values[!is.na(values)];.tm_assert(all(v>=b[1]&v<=utils::tail(b,1)),'Metric values exceed the supplied display range')
  labels<-vapply(b,function(v)format(v,trim=TRUE,scientific=FALSE),'')
  list(kind='gradient',breaks=b,palette=palette,labels=labels,maximum=utils::tail(b,1))
 } else {
  b<-config$breaks;labels<-config$labels
  .tm_assert(is.numeric(b)&&all(is.finite(b))&&all(diff(b)>0)&&length(palette)==length(b)+1L,'Class colors require one more color than supplied breaks')
  .tm_assert(is.character(labels)&&length(labels)==length(palette)&&!anyNA(labels)&&all(nzchar(labels)),'Class labels must be supplied explicitly')
  .tm_assert(is.logical(config$right)&&length(config$right)==1L&&!is.na(config$right),'Class boundary inclusion must be supplied as right=TRUE or FALSE')
  list(kind='classes',breaks=b,palette=palette,labels=labels,right=config$right,maximum=if(any(!is.na(values)))max(values,na.rm=TRUE) else NA_real_)
 }
}

.tm_metric <- function(x,metric=NULL) {
 if(is.null(metric))return(x)
 .tm_assert(is.character(metric)&&length(metric)==1L&&!is.na(metric),'metric must be one supplied name')
 if (identical(x$input_type, 'speed')) {
  .tm_assert(metric == 'speed' && (is.null(x$measurement) || identical(x$measurement$mode, 'provided')),
    'Select an available metric from the original Speed input')
  return(x)
 }
 if(metric=='flow') {
  .tm_assert(is.null(x$measurement),'Select metrics from the original object');return(x)
 }
 .tm_assert(is.null(x$measurement)&&metric%in%names(x$metrics),'Select an available metric from the original object')
 info<-x$metric_info[[metric]];x$values<-x$metrics[[metric]];x$source_values<-x$values;x$selected_metric<-metric;x$unit<-info$unit
 x$scale<-.tm_source_scale(info$scale,x$values)
 label<-if(is.null(info$label))metric else info$label
 x$title<-if(is.null(info$title))paste(x$title,label,sep=' | ') else info$title
 x$measurement<-list(mode='provided',origin='provided',label=label,source_metric=metric,source_unit=info$unit,unit=info$unit,definition=info$definition,description=info$definition,time_alignment=info$time_alignment)
 x
}

.tm_region_labels <- function(labels) {
  if (is.factor(labels)) as.character(labels) else labels
}

.tm_regions <- function(x) {
  labels <- c(.tm_region_labels(x$roads[["region"]]),
    .tm_region_labels(x$events[["region"]]))
  available <- unique(labels[!is.na(labels) & nzchar(labels)])
  order <- .tm_region_labels(x[["regions"]])
  c(intersect(order, available), setdiff(available, order))
}

.tm_select <- function(x,region=NULL) {
  .tm_assert(inherits(x,'trafficmap_data'),'Expected trafficmap_data')
  if(!is.null(region)).tm_assert(is.character(region)&&length(region)==1L&&region%in%.tm_regions(x),'Unknown region')
  list(roads=if(is.null(region))seq_len(nrow(x$roads)) else which(.tm_region_labels(x$roads[["region"]])==region),
    events=if(is.null(region))seq_len(NROW(x$events)) else which(.tm_region_labels(x$events[["region"]])==region))
}
# Select the supplied metric for analysis or rendering.
.tm_view <- function(x,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL) {
 .tm_assert(inherits(x,'trafficmap_data'),'Supply prepared Flow or Speed frames')
 .tm_assert(identical(mode,'flow') && is.null(reference) && is.null(threshold) &&
   is.null(comparison) && is.null(duration_rule),
   'Prepare changes and durations before rendering.')
 .tm_metric(x,metric)
}

.tm_road_clock_data <- function(x) {
  x$time_labels <- NULL
  if ("time_label" %in% names(x$events)) x$events$time_label <- NULL
  x
}

#' Read ready road frames without converting source tables
#' @export
#' @noRd
read_traffic <- function(file) {
  x <- if (is.character(file) && length(file) == 1L) readRDS(file) else file
  .tm_assert(inherits(x,'trafficmap_data'),'Supply prepared Flow or Speed frames (trafficmap_data or its RDS).')
  .tm_road_clock_data(x)
}

#' @method plot traffic_trend
#' @export
#' @noRd
plot.traffic_trend <- function(x,metric=NULL,index=NULL,...) {
 if(is.null(metric))metric<-if(isTRUE(attr(x,'has_roads')))'mean' else 'events'
 metric<-match.arg(metric,c('mean','events'))
 .tm_assert(nrow(x)>0L,'Trend needs at least one frame')
 if(!is.null(index)).tm_assert(is.numeric(index)&&length(index)==1L&&!is.na(index)&&is.finite(index)&&index==floor(index)&&index>=1&&index<=nrow(x),'Invalid selected trend frame')
 if(metric=='mean').tm_assert(isTRUE(attr(x,'has_roads')),'Mean trend requires road measurements')
 saved<-graphics::par(no.readonly=TRUE);on.exit(graphics::par(saved),add=TRUE)
 graphics::par(mar=c(4,4,3,1))
 y<-x[[metric]];limits<-range(c(0,y[is.finite(y)]));if(diff(limits)==0)limits<-limits+c(0,1)
 label<-if(metric=='events')'Reported starts' else paste(if(is.null(attr(x,'series_label')))paste('Mean',attr(x,'value_label')) else attr(x,'series_label'),'[',attr(x,'unit'),']')
 args<-utils::modifyList(list(x=x$time,y=y,type=if(metric=='events')'h' else if(nrow(x)==1L)'p' else 'l',lwd=if(metric=='events')5 else 2,
  col=if(metric=='events')'#9b4840' else '#365c70',xaxt='n',ylim=limits,xlab='Source time',ylab=label),list(...))
 do.call(graphics::plot,args)
 at<-unique(round(seq(1,nrow(x),length.out=min(7,nrow(x)))))
 clock_labels <- .tm_frame_labels(x$time, attr(x,'clock'), attr(x,'timezone'), attr(x,'time_label_format'))
 labels <- clock_labels[at]
 if (all(grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2} ', clock_labels)) &&
     length(unique(substr(clock_labels, 1L, 10L))) == 1L) {
   labels <- substring(labels, 12L)
 }
 graphics::axis(1,at=x$time[at],labels=labels,cex.axis=.7,las=if(any(nchar(labels)>12))2 else 1)
 if(!is.null(index))graphics::abline(v=x$time[index],lty=2,col='#7b858b')
 invisible(x)
}

#' @export
#' @noRd
traffic_trend <- function(x,region=NULL,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,method='mean') {
 x<-.tm_view(x,mode,reference,threshold,comparison,metric,duration_rule)
  selected <- .tm_select(x,region);v <- x$values[,selected$roads,drop=FALSE]
  mean <- vapply(seq_along(x$times),function(i) {
    finite <- v[i,][is.finite(v[i,])];if(length(finite))base::mean(finite) else NA_real_
  },numeric(1))
  method<-match.arg(method,c('mean','provided'));definition<-'Arithmetic mean of available supplied road values; no length or lane weighting.';series_label<-NULL
  if(method=='provided') {
   .tm_assert(is.null(x$measurement)||x$measurement$origin=='provided','Provided trends cannot describe newly computed views')
   scope<-if(is.null(region))'' else region;provided<-x$provided_trends
   .tm_assert(!is.null(provided),'No provided trends are available')
   provided<-provided[provided$metric==x$selected_metric&provided$region==scope,,drop=FALSE]
   .tm_assert(nrow(provided)>0L&&length(unique(provided$definition))==1L&&length(unique(provided$label))==1L,'No unambiguous provided trend exists for the requested metric and scope')
   mean<-provided$value[match(x$times,provided$time)];definition<-provided$definition[1];series_label<-provided$label[1]
  }
  count <- tabulate(as.integer(x$events$frame[selected$events]),nbins=length(x$times))
  out<-data.frame(time=x$times,available=rowSums(!is.na(v)),mean=mean,events=count)
  attr(out,'clock')<-x$clock;attr(out,'timezone')<-x$timezone;attr(out,'time_label_format')<-x$time_label_format
  class(out)<-c('traffic_trend','data.frame');attr(out,'unit')<-x$unit;attr(out,'has_roads')<-length(selected$roads)>0L
  attr(out,'value_label')<-if(is.null(x$measurement)&&.tm_road_kind(x)=='flow')'road value' else tolower(.tm_road_label(x))
  attr(out,'method')<-method;attr(out,'definition')<-definition;attr(out,'series_label')<-series_label
  out
}
#' @export
#' @noRd
traffic_summary <- function(x,index=NULL,region=NULL,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL) {
 x<-.tm_view(x,mode,reference,threshold,comparison,metric,duration_rule)
  selected <- .tm_select(x,region)
  index <- .tm_map_time_index(x,index)
  v <- x$values[index,selected$roads];finite <- v[is.finite(v)]
  data.frame(frame=as.integer(index),time=x$times[index],roads=length(selected$roads),
    available=length(finite),missing=sum(is.na(v)),zero=sum(finite==0),
    minimum=if(length(finite))min(finite) else NA_real_,median=if(length(finite))stats::median(finite) else NA_real_,
    mean=if(length(finite))mean(finite) else NA_real_,p95=if(length(finite))unname(stats::quantile(finite,.95)) else NA_real_,
    maximum=if(length(finite))max(finite) else NA_real_,
    events=sum(x$events$frame[selected$events]==index),stringsAsFactors=FALSE)
}


#' Whole-period traffic coverage
#' @export
#' @noRd
traffic_coverage <- function(x,region=NULL) {
  .tm_assert(inherits(x,'trafficmap_data'),'x must be trafficmap_data')
  selected <- .tm_select(x,region)
  values <- x$values[,selected$roads,drop=FALSE]
  data.frame(roads=length(selected$roads),frames=length(x$times),road_time_cells=length(values),
    observed=sum(!is.na(values)),missing=sum(is.na(values)),zero=sum(values==0,na.rm=TRUE),event_starts=length(selected$events))
}
