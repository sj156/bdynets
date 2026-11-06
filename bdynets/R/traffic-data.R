# Input reading and normalization
.tm_assert <- function(ok,message) if(!isTRUE(ok))stop(message,call.=FALSE)
.tm_palette <- c('#16a43a','#7ad151','#f7ea2a','#ffc928','#f5a623','#f36b2b','#f01818')
.tm_time <- function(time) {
  .tm_assert((is.numeric(time)||inherits(time,'POSIXct'))&&!anyNA(time)&&all(is.finite(as.numeric(time))),
    'Time must contain finite numeric seconds or POSIXct timestamps')
  list(value=as.numeric(time),clock=if(inherits(time,'POSIXct'))'absolute' else 'source_clock')
}

.tm_basemap <- function(base) {
 if(is.null(base))return(NULL)
 .tm_assert(is.list(base)&&identical(base$type,'FeatureCollection')&&is.list(base$features),
  'basemap must be a GeoJSON FeatureCollection')
 valid <- function(g,n) is.matrix(g)&&is.numeric(g)&&ncol(g)==2L&&nrow(g)>=n&&
  all(is.finite(g))&&all(abs(g[,1])<=180)&&all(abs(g[,2])<=85.0511)
 for(f in base$features) {
  .tm_assert(is.list(f)&&identical(f$type,'Feature')&&is.list(f$geometry),'Invalid context feature')
  g<-f$geometry
  .tm_assert(is.character(g$type)&&length(g$type)==1L&&g$type%in%c('LineString','Polygon','Point'),
   'Context geometry must be LineString, Polygon or Point')
  ok<-switch(g$type,LineString=valid(g$coordinates,2L),
   Polygon=is.list(g$coordinates)&&length(g$coordinates)>0L&&all(vapply(g$coordinates,
    function(r)valid(r,4L)&&all(r[1,]==r[nrow(r),]),logical(1))),
   Point=is.numeric(g$coordinates)&&length(g$coordinates)==2L&&valid(matrix(g$coordinates,1),1L))
  .tm_assert(ok,'Invalid context coordinates or polygon ring')
  .tm_assert(is.null(f$properties)||is.list(f$properties),'Context properties must be a list')
  label<-f$properties$label
  .tm_assert(is.null(label)||(is.character(label)&&length(label)==1L&&!is.na(label)),
   'Context label must be one string')
 }
 if(is.null(base$attribution))base$attribution<-'Basemap source unspecified'
 .tm_assert(is.character(base$attribution)&&length(base$attribution)==1L&&!is.na(base$attribution)&&
  nzchar(base$attribution),'Context attribution must be one nonempty string')
 base
}

#' @export
#' @noRd
flow_scale <- function(values) {
  .tm_assert(is.numeric(values)&&!any(is.infinite(values))&&!any(is.nan(values))&&
    all(values[!is.na(values)]>=0),'Flow must be nonnegative and finite or NA')
  v <- values[!is.na(values)]
  if(!length(v))return(list(kind='missing',maximum=NA_real_,breaks=numeric(),palette=.tm_palette))
  maximum <- max(v)
  if(maximum==0)return(list(kind='zero',maximum=0,breaks=numeric(),palette=.tm_palette))
  list(kind='log1p',maximum=maximum,
    breaks=expm1(seq(0,log1p(maximum),length.out=8L)[2:7]),palette=.tm_palette)
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


.tm_json_assignment <- function(text,assignment) {
 .tm_assert(is.character(assignment)&&length(assignment)==1L&&!is.na(assignment)&&grepl('^[A-Za-z_$][A-Za-z0-9_$]*(\\.[A-Za-z_$][A-Za-z0-9_$]*)*$',assignment),'JS/HTML extraction requires one explicit assignment name')
 # Mask strings and comments before locating assignments; inspect literals in
 # the original text so JSON strings and their values remain untouched.
 original<-strsplit(text,'',fixed=TRUE)[[1]];masked<-original;i<-1L;n<-length(original)
 quote<-NULL;comment<-NULL;escaped<-FALSE
 while(i<=n) {
  ch<-original[i];next_ch<-if(i<n)original[i+1L] else ''
  if(!is.null(comment)) {
   if(comment=='line'&&ch%in%c('\r','\n'))comment<-NULL else {
    if(!ch%in%c('\r','\n'))masked[i]<-' '
    if(comment=='block'&&ch=='*'&&next_ch=='/') {masked[i+1L]<-' ';i<-i+1L;comment<-NULL}
   }
  } else if(!is.null(quote)) {
   if(!ch%in%c('\r','\n'))masked[i]<-' '
   if(escaped)escaped<-FALSE else if(ch=='\\')escaped<-TRUE else if(ch==quote)quote<-NULL
  } else if(ch%in%c('"',"'",'`')) {quote<-ch;masked[i]<-' '}
  else if(ch=='/'&&next_ch%in%c('/','*')) {comment<-if(next_ch=='/')'line' else 'block';masked[i:(i+1L)]<-' ';i<-i+1L}
  i<-i+1L
 }
 code<-paste(masked,collapse='')
 pattern<-paste0('(?:^|[\\r\\n])[ \\t\\r\\n]*(?:(?:const|let|var)[ \\t]+)?\\Q',assignment,'\\E[ \\t]*=')
 hits<-gregexpr(pattern,code,perl=TRUE)[[1]];lengths<-attr(hits,'match.length');valid<-list()
 if(hits[1]>0L)for(k in seq_along(hits)) {
  remainder<-substring(text,hits[k]+lengths[k],nchar(text));remainder<-sub('^[ \\t\\r\\n]+','',remainder,perl=TRUE)
  chars<-strsplit(remainder,'',fixed=TRUE)[[1]]
  if(!length(chars))next
  quoted<-FALSE;escaped<-FALSE;depth<-0L;end<-NA_integer_
  if(chars[1]%in%c('{','[','"')) {
   root_string<-chars[1]=='"'
   for(i in seq_along(chars)) {
    ch<-chars[i]
    if(quoted) {
     if(escaped)escaped<-FALSE else if(ch=='\\')escaped<-TRUE else if(ch=='"') {
      quoted<-FALSE;if(root_string){end<-i;break}
     }
    } else if(ch=='"')quoted<-TRUE else if(ch%in%c('{','['))depth<-depth+1L else if(ch%in%c('}',']')) {
     depth<-depth-1L;if(depth==0L){end<-i;break}
    }
   }
  } else {
   stop_at<-regexpr('[;\\r\\n<]',remainder,perl=TRUE)[1];end<-if(stop_at>0L)stop_at-1L else nchar(remainder)
  }
  if(is.na(end)||end<1L)next
  literal<-substr(remainder,1L,end);tail<-substring(remainder,end+1L,nchar(remainder))
  if(!grepl('^[ \\t]*(;|\\r?\\n|</script[ >]|$)',tail,perl=TRUE,ignore.case=TRUE)||!jsonlite::validate(literal))next
  valid<-c(valid,list(jsonlite::fromJSON(literal,simplifyVector=FALSE)))
 }
 .tm_assert(length(valid)==1L,'Missing, invalid or ambiguous JSON literal for the requested assignment')
 valid[[1L]]
}

#' @export
#' @noRd
extract_traffic_data <- function(path,file=NULL,assignment=NULL,format='auto') {
 .tm_assert(is.character(path)&&length(path)==1L&&!is.na(path)&&file.exists(path)&&!dir.exists(path),'Source path must be one existing local file')
 format<-match.arg(format,c('auto','rds','json','csv','js','html'))
 if(format=='auto') {
  extension<-tolower(tools::file_ext(sub('\\.gz$','',path,ignore.case=TRUE)))
  if(extension=='htm')extension<-'html'
  .tm_assert(extension%in%c('rds','json','csv','js','html'),'Unsupported source extension; specify a supported format explicitly')
  format<-extension
 }
 .tm_assert(format%in%c('js','html')||is.null(assignment),'assignment applies only to JS/HTML source files')
 if(format=='rds')payload<-readRDS(path) else {
  con<-if(grepl('\\.gz$',path,ignore.case=TRUE))gzfile(path,'rt',encoding='UTF-8') else base::file(path,'rt',encoding='UTF-8')
  on.exit(close(con),add=TRUE)
  if(format=='csv')payload<-utils::read.csv(con,stringsAsFactors=FALSE,check.names=FALSE) else {
   text<-paste(readLines(con,warn=FALSE),collapse='\n');text<-sub('^\ufeff','',text)
   if(format=='html') {
    scripts<-regmatches(text,gregexpr('(?is)<!--.*?-->|<script\\b[^>]*>.*?</script\\s*>',text,perl=TRUE))[[1]]
    scripts<-scripts[grepl('(?i)^<script\\b',scripts,perl=TRUE)]
    .tm_assert(length(scripts)>0L,'No inline script content in HTML source')
    text<-paste(sub('(?is)</script\\s*>$','',sub('(?is)^<script\\b[^>]*>','',scripts,perl=TRUE),perl=TRUE),collapse='\n')
   }
   payload<-if(format%in%c('js','html')) .tm_json_assignment(text,assignment) else {
    .tm_assert(jsonlite::validate(text),'Invalid JSON source');jsonlite::fromJSON(text,simplifyVector=FALSE)
   }
  }
 }
 identity<-list(file=basename(path),md5=unname(tools::md5sum(path)),bytes=unname(file.info(path)$size),format=format,assignment=assignment)
 if(is.list(payload)&&identical(payload$schema,'traffic-source-v1')) {
  .tm_assert(all(c('payload','provenance')%in%names(payload)),'Invalid extracted source container')
  result<-payload;result$container<-identity
 } else result<-list(schema='traffic-source-v1',payload=payload,provenance=identity)
 if(!is.null(file)) {
  .tm_assert(is.character(file)&&length(file)==1L&&!is.na(file)&&nzchar(file)&&!file.exists(file),'Extraction output must be a new file')
  .tm_assert(grepl('\\.(rds|json|json\\.gz)$',file,ignore.case=TRUE),'Extraction output must be RDS, JSON or JSON.GZ')
  dir.create(dirname(file),recursive=TRUE,showWarnings=FALSE);temp<-tempfile('traffic-source-',tmpdir=dirname(file))
  on.exit(unlink(temp),add=TRUE)
  if(grepl('\\.rds$',file,ignore.case=TRUE))saveRDS(result,temp,compress='xz') else {
   encoded<-jsonlite::toJSON(result,auto_unbox=TRUE,digits=NA,null='null',na='null',dataframe='rows',matrix='rowmajor')
   output<-if(grepl('\\.gz$',file,ignore.case=TRUE))gzfile(temp,'wt') else base::file(temp,'wt')
   tryCatch(writeLines(encoded,output,useBytes=TRUE),finally=close(output))
  }
  .tm_assert(file.rename(temp,file),'Unable to finalize extracted source file')
 }
 result
}


# Shared input boundary. Algorithms consume normalized tables only.
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
.tm_crs <- function(crs) .tm_assert(is.character(crs)&&length(crs)==1L&&!is.na(crs)&&crs %in% c('EPSG:4326','WGS84','OGC:CRS84'),
  'Unsupported CRS; supply WGS84 longitude/latitude degrees (EPSG:4326); no transformation is performed')
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
  .tm_assert(all(abs(nodes[[nc[['longitude']]]])<=180)&&all(abs(nodes[[nc[['latitude']]]])<=85.0511),'Node coordinates are invalid longitude/latitude degrees')
  for(key in c('origin','destination')) .tm_assert(!anyNA(flows[[fc[[key]]]])&&all(as.character(flows[[fc[[key]]]])%in%ids),'Every OD endpoint must match one node identifier')
  list(nodes=nodes,flows=flows,node_columns=nc,flow_columns=fc)
}
.tm_rename_table <- function(x,mapping,label) {
  if(is.null(x))return(NULL)
  x <- .tm_read_table(x,label)
  if(!is.null(mapping)) {
    allowed <- switch(label,roads=c('road_id','geometry','region','opposite_id'),flow=c('road_id','time','flow'),events=c('time','lon','lat','id','category','description','region','time_label'))
    mapping <- .tm_mapping(mapping,x,character(),allowed,paste0(label,'_columns'))
    source <- x
    for(key in names(mapping)) if(!is.na(mapping[[key]])&&nzchar(mapping[[key]])) x[[key]] <- source[[mapping[[key]]]]
    # Mapped source attributes must not survive as duplicate semantic fields.
    x <- x[,setdiff(names(x),setdiff(unname(mapping),names(mapping))),drop=FALSE]
  }
  numeric <- switch(label,flow='flow',events=c('lon','lat'),character())
  for(key in intersect(numeric,names(x)))x[[key]] <- .tm_numeric(x[[key]],paste(label,key),allow_na=label=='flow')
  if('time'%in%names(x)) {
    if(is.factor(x$time))x$time <- as.character(x$time)
    if(is.character(x$time)) {
      seconds <- suppressWarnings(as.numeric(x$time))
      if(all(is.finite(seconds)))x$time <- seconds
    }
  }
  x
}

.tm_traffic_data_reference <- function(roads=NULL,flow=NULL,events=NULL,times=NULL,time_labels=NULL,
                         interval_seconds=NULL,unit='unit unspecified',title='Traffic map',
                         basemap=NULL,description='',preparation=character(),notes=character(),
                         provenance=list(),measurements=NULL,metric_info=list(),trends=NULL) {
  .tm_assert(is.character(unit)&&length(unit)==1L&&!is.na(unit)&&nzchar(unit),'unit must be one label')
  .tm_assert(is.character(title)&&length(title)==1L&&!is.na(title),'title must be one string')
  if(is.null(roads)) {
    roads <- data.frame(road_id=character(),region=character());roads$geometry <- list()
  }
  .tm_assert(is.data.frame(roads)&&all(c('road_id','geometry')%in%names(roads)),
    'roads needs road_id and geometry columns')
  roads$road_id <- as.character(roads$road_id)
  .tm_assert(!anyNA(roads$road_id)&&all(nzchar(roads$road_id))&&!anyDuplicated(roads$road_id),'Road IDs must be unique and nonempty')
  .tm_assert(is.list(roads$geometry),'geometry must be a list-column')
  for(g in roads$geometry).tm_assert(is.matrix(g)&&is.numeric(g)&&ncol(g)==2L&&nrow(g)>=2L&&
    all(is.finite(g))&&all(abs(g[,1])<=180)&&all(abs(g[,2])<=85.0511),
    'Road geometry must be longitude/latitude matrices with at least two vertices')
  if(!'region'%in%names(roads))roads$region <- rep('',nrow(roads))
  roads$region <- as.character(roads$region)
  .tm_assert(!anyNA(roads$region),'Road region labels must be strings; an empty label is unassigned')
  for(name in setdiff(names(roads),'geometry')).tm_assert(is.atomic(roads[[name]]),'Road attributes must be scalar columns')
  intervals_explicit <- !is.null(interval_seconds)
  clocks <- character();observed_times <- numeric()
  if(is.null(flow))flow <- data.frame(road_id=character(),time=numeric(),flow=numeric())
  .tm_assert(is.data.frame(flow)&&all(c('road_id','time','flow')%in%names(flow)),'flow needs road_id, time and flow columns')
  if(nrow(flow)) {
    t <- .tm_time(flow$time);clocks <- c(clocks,t$clock);flow$time <- t$value
    observed_times <- c(observed_times,flow$time)
  }
  flow$road_id <- as.character(flow$road_id)
  .tm_assert(all(flow$road_id%in%roads$road_id),'Flow references unknown road IDs')
  flow_scale(flow$flow)
  if(is.null(events))events <- data.frame(time=numeric(),lon=numeric(),lat=numeric())
  .tm_assert(is.data.frame(events)&&all(c('time','lon','lat')%in%names(events)),
    'events needs time, lon and lat columns')
  if(nrow(events)) {
    if(!'time_label'%in%names(events))events$time_label <- if(inherits(events$time,'POSIXct'))format(events$time,tz='UTC',usetz=FALSE) else format(events$time,trim=TRUE)
    t <- .tm_time(events$time);clocks <- c(clocks,t$clock);events$time <- t$value
    observed_times <- c(observed_times,events$time)
  }
  .tm_assert(is.numeric(events$lon)&&is.numeric(events$lat)&&all(is.finite(events$lon))&&
    all(is.finite(events$lat))&&all(abs(events$lon)<=180)&&all(abs(events$lat)<=85.0511),
    'Event coordinates must be finite longitude/latitude degrees')
  if(!'id'%in%names(events))events$id <- as.character(seq_len(nrow(events)))
  events$id <- as.character(events$id)
  .tm_assert(!anyNA(events$id)&&all(nzchar(events$id))&&!anyDuplicated(events$id),'Event IDs must be unique and nonempty')
  for(name in c('category','description','region')) {
    if(!name%in%names(events))events[[name]] <- rep(if(name=='category')'Uncategorized' else '',nrow(events))
    events[[name]] <- as.character(events[[name]])
    events[[name]][is.na(events[[name]])] <- if(name=='category')'Uncategorized' else ''
    if(name=='category')events[[name]][!nzchar(events[[name]])] <- 'Uncategorized'
  }
  .tm_assert(nrow(roads)>0L||nrow(events)>0L,'At least one road or event is required')
  .tm_assert(!is.null(times)||length(observed_times)>0L,'A frame clock or observations are required')
  if(is.null(times)) {
    if(nrow(flow))times <- sort(unique(flow$time)) else {
      if(is.null(interval_seconds))interval_seconds <- 3600
      step <- interval_seconds[1]
      .tm_assert(is.numeric(step)&&length(step)==1L&&is.finite(step)&&step>0,'Invalid interval')
      times <- seq(floor(min(observed_times)/step)*step,floor(max(observed_times)/step)*step,by=step)
    }
  } else {
    t <- .tm_time(times);clocks <- c(clocks,t$clock);times <- t$value
  }
  .tm_assert(length(unique(clocks))<=1L,'Flow, events and frame clocks must have the same type')
  clock <- if(length(clocks))clocks[1] else 'source_clock'
  .tm_assert(length(times)>0L&&all(is.finite(times))&&all(diff(times)>0),'Frame times must be strictly increasing')
  if(is.null(interval_seconds))interval_seconds <- if(length(times)>1L)c(diff(times),utils::tail(diff(times),1)) else 3600
  if(length(interval_seconds)==1L)interval_seconds <- rep(interval_seconds,length(times))
  .tm_assert(is.numeric(interval_seconds)&&length(interval_seconds)==length(times)&&
    all(is.finite(interval_seconds))&&all(interval_seconds>0)&&
    (length(times)==1L||all(utils::head(times+interval_seconds,-1)<=utils::tail(times,-1))),
    'Frame intervals must be positive, aligned and nonoverlapping')
  if(is.null(time_labels))time_labels <- if(clock=='absolute')
    format(as.POSIXct(times,origin='1970-01-01',tz='UTC'),tz='UTC',usetz=FALSE) else format(times,trim=TRUE)
  .tm_assert(is.character(time_labels)&&length(time_labels)==length(times)&&!anyNA(time_labels),'Invalid time labels')
  rows <- match(flow$time,times);columns <- match(flow$road_id,roads$road_id)
  .tm_assert(!anyNA(rows),'Flow timestamps must match frame starts')
  keys <- (rows-1)*nrow(roads)+columns
  .tm_assert(!anyDuplicated(keys),'Duplicate road-time observations')
  values <- matrix(NA_real_,length(times),nrow(roads))
  if(length(rows))values[cbind(rows,columns)] <- flow$flow
  events$frame <- findInterval(events$time,times)
  if(nrow(events)).tm_assert(all(events$frame>0L)&&
    all(events$time<times[events$frame]+interval_seconds[events$frame]),
    'Events lie outside the supplied half-open frame intervals')
  basemap <- .tm_basemap(basemap)
  .tm_assert(is.list(provenance)&&is.character(description)&&length(description)==1L&&!is.na(description)&&is.character(preparation)&&!anyNA(preparation)&&is.character(notes)&&!anyNA(notes),'Invalid descriptive metadata')
  out <- structure(list(roads=roads,values=values,events=events,times=times,time_labels=time_labels,
    interval_seconds=interval_seconds,intervals_explicit=intervals_explicit,clock=clock,unit=unit,title=title,basemap=basemap,
    description=description,preparation=preparation,notes=notes,provenance=provenance,
    regions=unique(c(roads$region[nzchar(roads$region)],events$region[nzchar(events$region)])),scale=flow_scale(values)),class='trafficmap_data')
  out$opposing <- .tm_opposing(roads$geometry,roads$opposite_id,roads$road_id)
  out$initial <- 1L
  .tm_add_metrics(out,measurements,metric_info,trends)
}

#' @export
#' @noRd
read_traffic <- function(file) {
  input <- readRDS(file)
  .tm_assert(is.list(input)&&identical(input$schema,'trafficmap-input-v1'),'Unsupported traffic input schema')
  input$schema <- NULL
  .tm_assert(all(names(input)%in%names(formals(traffic_data))),'Unknown traffic input fields')
  do.call(traffic_data,input)
}

#' @export
#' @import data.table
#' @noRd
traffic_data <- function(roads=NULL,flow=NULL,events=NULL,times=NULL,time_labels=NULL,
                         interval_seconds=NULL,unit='unit unspecified',title='Traffic map',
                         basemap=NULL,description='',preparation=character(),notes=character(),
                         provenance=list(),measurements=NULL,metric_info=list(),trends=NULL,
                         road_columns=NULL,flow_columns=NULL,event_columns=NULL,crs='EPSG:4326') {
  args <- as.list(environment())
  .tm_crs(crs)
  args$roads <- .tm_rename_table(roads,road_columns,'roads')
  args$flow <- .tm_rename_table(flow,flow_columns,'flow')
  args$events <- .tm_rename_table(events,event_columns,'events')
  args[c('road_columns','flow_columns','event_columns','crs')] <- NULL
  do.call(.tm_traffic_data_reference,args)
}

# Summaries and measurement views
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

.tm_add_metrics <- function(x,measurements,metric_info,trends) {
 .tm_assert(is.list(metric_info),'metric_info must be a named list')
 if(length(metric_info)).tm_assert(!is.null(names(metric_info))&&!anyDuplicated(names(metric_info))&&all(nzchar(names(metric_info))),'Metric definitions need unique names')
 for(info in metric_info) {
  .tm_assert(is.list(info)&&all(vapply(info[c('unit','definition')],function(v)is.character(v)&&length(v)==1L&&!is.na(v)&&nzchar(v),logical(1))),'Every metric requires a supplied unit and definition')
  for(k in intersect(c('label','title','kind'),names(info))).tm_assert(is.character(info[[k]])&&length(info[[k]])==1L&&!is.na(info[[k]])&&nzchar(info[[k]]),paste('Metric metadata needs one nonempty',k))
  if(identical(info$kind,'duration')).tm_assert(is.character(info$time_alignment)&&length(info$time_alignment)==1L&&info$time_alignment%in%c('interval_start','interval_end'),'Provided duration needs its source time alignment')
 }
 if(!is.null(metric_info$flow)).tm_assert(identical(metric_info$flow$unit,x$unit),'Primary metric metadata must retain the supplied flow unit')
 x$metrics<-list();x$metric_info<-metric_info;x$selected_metric<-'flow'
 if(!is.null(measurements)) {
  .tm_assert(is.data.frame(measurements)&&all(c('road_id','time','metric','value')%in%names(measurements)),'measurements needs road_id, time, metric and value')
  if(nrow(measurements)) {
   t<-.tm_time(measurements$time);.tm_assert(t$clock==x$clock,'Metric clocks must match frame clocks');measurements$time<-t$value
   measurements$road_id<-as.character(measurements$road_id);measurements$metric<-as.character(measurements$metric)
   .tm_assert(!anyNA(measurements$metric)&&all(nzchar(measurements$metric))&&all(measurements$metric!='flow')&&all(measurements$metric%in%names(metric_info)),'Every additional metric needs an explicit definition; flow is reserved')
   rows<-match(measurements$time,x$times);columns<-match(measurements$road_id,x$roads$road_id)
   .tm_assert(!anyNA(rows)&&!anyNA(columns)&&!anyDuplicated(measurements[c('road_id','time','metric')]),'Metric records need known road/time keys without duplicates')
   flow_scale(measurements$value)
   for(key in unique(measurements$metric)) {
    take<-which(measurements$metric==key);v<-matrix(NA_real_,length(x$times),nrow(x$roads));v[cbind(rows[take],columns[take])]<-measurements$value[take]
    .tm_source_scale(metric_info[[key]]$scale,v);x$metrics[[key]]<-v
   }
  }
 }
 if(!is.null(trends)) {
  .tm_assert(is.data.frame(trends)&&all(c('time','metric','value','definition','label')%in%names(trends)),'Provided trends need time, metric, value, definition and label')
  if(!'region'%in%names(trends))trends$region<-rep('',nrow(trends))
  if(nrow(trends)) {
   t<-.tm_time(trends$time);.tm_assert(t$clock==x$clock,'Provided trend clocks must match frame clocks');trends$time<-t$value
   for(k in c('metric','region','definition','label'))trends[[k]]<-as.character(trends[[k]])
   .tm_assert(all(trends$time%in%x$times)&&all(trends$metric%in%c('flow',names(x$metrics)))&&!anyNA(trends[c('metric','region','definition','label')])&&all(nzchar(trends$definition))&&all(nzchar(trends$label))&&!anyDuplicated(trends[c('time','metric','region')]),'Provided trends require valid unique scope/time keys and explicit definitions')
   flow_scale(trends$value)
  }
 }
 x$provided_trends<-trends;x
}

.tm_metric <- function(x,metric=NULL) {
 if(is.null(metric))return(x)
 .tm_assert(is.character(metric)&&length(metric)==1L&&!is.na(metric),'metric must be one supplied name')
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

.tm_select <- function(x,region=NULL) {
  .tm_assert(inherits(x,'trafficmap_data'),'Expected trafficmap_data')
  if(!is.null(region)).tm_assert(is.character(region)&&length(region)==1L&&region%in%x$regions,'Unknown region')
  list(roads=if(is.null(region))seq_len(nrow(x$roads)) else which(x$roads$region==region),
    events=if(is.null(region))seq_len(nrow(x$events)) else which(x$events$region==region))
}
.tm_view <- function(x,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL) {
 .tm_assert(inherits(x,'trafficmap_data'),'Expected trafficmap_data')
 mode<-match.arg(mode,c('flow','change','duration'))
 if(mode=='duration'&&is.null(metric)) {
  available<-names(x$metrics)[vapply(names(x$metrics),function(k)identical(x$metric_info[[k]]$kind,'duration'),logical(1))]
  if(length(available)) {
   .tm_assert(length(available)==1L,'Select one supplied duration metric explicitly; computation over primary flow requires metric="flow"')
   metric<-available
  } else .tm_assert(!is.null(duration_rule),'Duration requires provided records or a complete explicit computation rule')
 }
 x<-.tm_metric(x,metric)
 if(mode=='flow') {
  .tm_assert(is.null(reference)&&is.null(threshold)&&is.null(comparison)&&is.null(duration_rule),'Analysis arguments require an analysis mode')
  return(x)
 }
 .tm_assert(is.null(x$measurement)||identical(x$measurement$origin,'provided'),'Analysis modes require original source measurements')
 .tm_assert(nrow(x$roads)>0L,'Analysis modes require road measurements')
 source<-x$values;n<-nrow(source);source_unit<-x$unit;source_metric<-x$selected_metric
 if(mode=='duration'&&identical(x$metric_info[[source_metric]]$kind,'duration')) {
  .tm_assert(is.null(reference)&&is.null(threshold)&&is.null(comparison)&&is.null(duration_rule),'Provided duration records cannot be overridden by computation parameters')
  x$measurement$mode<-'duration';return(x)
 }
 values<-matrix(NA_real_,n,ncol(source));contiguous<-c(FALSE,abs(utils::head(x$times+x$interval_seconds,-1)-utils::tail(x$times,-1))<=1e-6)
 if(mode=='change') {
  .tm_assert(is.null(threshold)&&is.null(comparison)&&is.null(duration_rule),'Threshold rules apply only to duration mode')
  if(is.null(reference)) {
   if(n>1L)values[2:n,]<-source[2:n,,drop=FALSE]-source[1:(n-1L),,drop=FALSE]
   values[!contiguous,]<-NA_real_;refs<-c(NA_integer_,if(n>1L)seq_len(n-1L) else integer())
   detail<-'Current minus previous contiguous frame. No implicit zero baseline; missing observations and time gaps remain missing.'
  } else {
   .tm_assert(is.numeric(reference)&&length(reference)==1L&&!is.na(reference)&&is.finite(reference)&&reference==floor(reference)&&reference>=1&&reference<=n,'Invalid reference frame')
   refs<-rep(as.integer(reference),n);values<-sweep(source,2,source[reference,],'-')
   detail<-paste('Current minus the explicitly selected reference frame',reference,'(',x$time_labels[reference],'). Missing observations remain missing.')
  }
  scale<-list(kind='change',breaks=0,palette=c('#2166ac','#f7f7f7','#b2182b'),labels=c('Decrease (< 0)','No change (0)','Increase (> 0)'),maximum=if(all(is.na(values)))NA_real_ else max(abs(values),na.rm=TRUE))
  measurement<-list(mode=mode,origin='computed',label='Change',source_metric=source_metric,source_unit=source_unit,unit=source_unit,reference=reference,reference_frames=refs,description=detail)
 } else {
  .tm_assert(is.null(reference),'reference applies only to change mode')
  .tm_assert(isTRUE(x$intervals_explicit),'Duration computation requires explicit interval_seconds')
  .tm_assert(is.numeric(threshold)&&length(threshold)==1L&&!is.na(threshold)&&is.finite(threshold),'Duration computation requires an explicit finite threshold')
  .tm_assert(is.character(comparison)&&length(comparison)==1L&&!is.na(comparison)&&comparison%in%c('>=','>','<=','<'),'Duration computation requires an explicit comparison')
  .tm_assert(is.list(duration_rule),'Duration computation requires an explicit rule')
  for(k in c('definition','interval_meaning','unit','missing_policy')) .tm_assert(is.character(duration_rule[[k]])&&length(duration_rule[[k]])==1L&&!is.na(duration_rule[[k]])&&nzchar(duration_rule[[k]]),paste('Duration rule requires',k))
  .tm_assert(duration_rule$unit%in%c('minutes','seconds')&&duration_rule$missing_policy=='break','Duration rule must explicitly choose minutes/seconds and missing_policy="break"')
  qualifies<-switch(comparison,`>=`=source>=threshold,`>`=source>threshold,`<=`=source<=threshold,`<`=source<threshold)
  run<-numeric(ncol(source));divisor<-if(duration_rule$unit=='minutes')60 else 1
  for(i in seq_len(n)) {
   if(!contiguous[i])run[]<-0
   known<-!is.na(qualifies[i,]);active<-known&qualifies[i,];run[!active]<-0;run[active]<-run[active]+x$interval_seconds[i];values[i,known]<-run[known]/divisor
  }
  scale<-.tm_source_scale(duration_rule$scale,values)
  measurement<-list(mode=mode,origin='computed',label='Threshold-run duration',source_metric=source_metric,source_unit=source_unit,unit=duration_rule$unit,threshold=threshold,comparison=comparison,rule=duration_rule,time_alignment='interval_end',
   description=paste(duration_rule$definition,duration_rule$interval_meaning,'Condition:',comparison,threshold,source_unit,'. Accumulated through interval end in',duration_rule$unit,'. Missing records and gaps interrupt confirmed runs; onset before the observed window is unknown.'))
 }
 x$source_values<-source;x$values<-values;x$scale<-scale;x$unit<-measurement$unit;x$measurement<-measurement;x$selected_metric<-source_metric
 x$title<-paste(x$title,measurement$label,sep=' | ');x$provenance$measurement<-measurement;x
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
 labels<-x$label[at];prefix<-sub(' [^ ]*$','',x$label)
 if(length(unique(prefix))==1L&&any(grepl(' ',x$label,fixed=TRUE)))labels<-sub('^.* ','',labels)
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
  count <- tabulate(x$events$frame[selected$events],nbins=length(x$times))
  out<-data.frame(time=x$times,label=x$time_labels,available=rowSums(!is.na(v)),mean=mean,events=count)
  class(out)<-c('traffic_trend','data.frame');attr(out,'unit')<-x$unit;attr(out,'has_roads')<-length(selected$roads)>0L
  attr(out,'value_label')<-if(is.null(x$measurement))'road value' else tolower(x$measurement$label)
  attr(out,'method')<-method;attr(out,'definition')<-definition;attr(out,'series_label')<-series_label
  out
}
#' @export
#' @noRd
traffic_summary <- function(x,index=x$initial,region=NULL,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL) {
 x<-.tm_view(x,mode,reference,threshold,comparison,metric,duration_rule)
  selected <- .tm_select(x,region)
  .tm_assert(is.numeric(index)&&length(index)==1L&&is.finite(index)&&index==floor(index)&&index>=1&&index<=length(x$times),'Invalid frame index')
  v <- x$values[index,selected$roads];finite <- v[is.finite(v)]
  data.frame(frame=as.integer(index),time=x$time_labels[index],roads=length(selected$roads),
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

# OD aggregation input boundary
# The aggregation source file is retained byte-for-byte. This alias captures its
# original implementation before installing the public normalization boundary.
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
