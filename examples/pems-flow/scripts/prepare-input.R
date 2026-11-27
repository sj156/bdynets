# Flow preparation and file export for this example.
for (helper in c(
  ".tm_assert",
  ".tm_time",
  ".tm_display_timezone",
  ".tm_basemap",
  "flow_scale",
  ".tm_observation_values",
  ".tm_read_table",
  ".tm_numeric",
  ".tm_crs",
  ".tm_mercator_limit",
  ".tm_source_scale",
  ".tm_opposing",
  ".tm_provenance_for_json"
)) {
  assign(helper, getFromNamespace(helper, "bdynets"))
}
rm(helper)

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

.tm_rename_table <- function(x,mapping,label) {
  if(is.null(x))return(NULL)
  x <- .tm_read_table(x,label)
  if(!is.null(mapping)) {
    allowed <- switch(label,roads=c('road_id','geometry','region','opposite_id'),flow=c('road_id','time','flow'),events=c('time','lon','lat','id','category','description','region'))
    mapping <- .tm_mapping(mapping,x,character(),allowed,paste0(label,'_columns'))
    source <- x
    for(key in names(mapping)) if(!is.na(mapping[[key]])&&nzchar(mapping[[key]])) x[[key]] <- source[[mapping[[key]]]]
    # Mapped source attributes must not survive as duplicate semantic fields.
    x <- x[,setdiff(names(x),setdiff(unname(mapping),names(mapping))),drop=FALSE]
  }
  numeric <- switch(label,flow='flow',events=c('lon','lat'),character())
  for(key in intersect(numeric,names(x)))x[[key]] <- .tm_numeric(x[[key]],paste(label,key),allow_na=label == 'flow')
  if('time'%in%names(x)) {
    if(is.factor(x$time))x$time <- as.character(x$time)
    if(is.character(x$time)) {
      seconds <- suppressWarnings(as.numeric(x$time))
      if(all(is.finite(seconds)))x$time <- seconds
    }
  }
  x
}

.tm_traffic_data_reference <- function(roads=NULL,flow=NULL,events=NULL,times=NULL,
                         interval_seconds=NULL,unit='unit unspecified',title='Traffic map',
                         basemap=NULL,description='',preparation=character(),notes=character(),
                         provenance=list(),measurements=NULL,metric_info=list(),trends=NULL,
                         timezone=NULL,time_label_format=NULL) {
  primary_metric <- 'flow'
  primary_label <- 'Flow'
  observations <- flow
  if (is.null(timezone)) {
    source_clocks <- list(times, if (is.data.frame(observations)) observations$time,
      if (is.data.frame(events)) events$time)
    zones <- unlist(lapply(source_clocks, function(x) attr(x, "tzone", exact = TRUE)[1L]))
    zones <- zones[!is.na(zones) & nzchar(zones)]
    if (length(zones)) timezone <- zones[1L]
  }
  if (!is.null(timezone)) timezone <- .tm_display_timezone(timezone)
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
    all(is.finite(g))&&all(abs(g[,1])<=180)&&all(abs(g[,2])<=.tm_mercator_limit),
    'Road geometry must be longitude/latitude matrices with at least two vertices')
  if(!'region'%in%names(roads))roads$region <- rep('',nrow(roads))
  roads$region <- as.character(roads$region)
  .tm_assert(!anyNA(roads$region),'Road region labels must be strings; an empty label is unassigned')
  for(name in setdiff(names(roads),'geometry')).tm_assert(is.atomic(roads[[name]]),'Road attributes must be scalar columns')
  intervals_explicit <- !is.null(interval_seconds)
  clocks <- character();observed_times <- numeric()
  if (is.null(observations)) observations <- data.frame(road_id=character(),time=numeric(),flow=numeric())
  .tm_assert(is.data.frame(observations) && all(c('road_id','time',primary_metric) %in% names(observations)),
    paste(primary_metric, 'needs road_id, time and', primary_metric, 'columns'))
  if(nrow(observations)) {
    t <- .tm_time(observations$time);clocks <- c(clocks,t$clock);observations$time <- t$value
    observed_times <- c(observed_times,observations$time)
  }
  observations$road_id <- as.character(observations$road_id)
  .tm_assert(all(observations$road_id %in% roads$road_id), paste(primary_label, 'references unknown road IDs'))
  .tm_observation_values(observations[[primary_metric]], primary_label)
  if(is.null(events))events <- data.frame(time=numeric(),lon=numeric(),lat=numeric())
  .tm_assert(is.data.frame(events)&&all(c('time','lon','lat')%in%names(events)),
    'events needs time, lon and lat columns')
  if(nrow(events)) {
    t <- .tm_time(events$time);clocks <- c(clocks,t$clock);events$time <- t$value
    observed_times <- c(observed_times,events$time)
  }
  .tm_assert(is.numeric(events$lon)&&is.numeric(events$lat)&&all(is.finite(events$lon))&&
    all(is.finite(events$lat))&&all(abs(events$lon)<=180)&&all(abs(events$lat)<=.tm_mercator_limit),
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
    if(nrow(observations))times <- sort(unique(observations$time)) else {
      if(is.null(interval_seconds))interval_seconds <- 3600
      step <- interval_seconds[1]
      .tm_assert(is.numeric(step)&&length(step)==1L&&is.finite(step)&&step>0,'Invalid interval')
      times <- seq(floor(min(observed_times)/step)*step,floor(max(observed_times)/step)*step,by=step)
    }
  } else {
    t <- .tm_time(times);clocks <- c(clocks,t$clock);times <- t$value
  }
  .tm_assert(length(unique(clocks))<=1L,paste0(primary_label, ', events and frame clocks must have the same type'))
  clock <- if(length(clocks))clocks[1] else 'source_clock'
  .tm_assert(length(times)>0L&&all(is.finite(times))&&all(diff(times)>0),'Frame times must be strictly increasing')
  if(is.null(interval_seconds))interval_seconds <- if(length(times)>1L)c(diff(times),utils::tail(diff(times),1)) else 3600
  if(length(interval_seconds)==1L)interval_seconds <- rep(interval_seconds,length(times))
  .tm_assert(is.numeric(interval_seconds)&&length(interval_seconds)==length(times)&&
    all(is.finite(interval_seconds))&&all(interval_seconds>0)&&
    (length(times)==1L||all(utils::head(times+interval_seconds,-1)<=utils::tail(times,-1))),
    'Frame intervals must be positive, aligned and nonoverlapping')
  rows <- match(observations$time,times);columns <- match(observations$road_id,roads$road_id)
  .tm_assert(!anyNA(rows),paste(primary_label, 'timestamps must match frame starts'))
  keys <- (rows-1)*nrow(roads)+columns
  .tm_assert(!anyDuplicated(keys),'Duplicate road-time observations')
  values <- matrix(NA_real_,length(times),nrow(roads))
  if(length(rows))values[cbind(rows,columns)] <- observations[[primary_metric]]
  events$frame <- findInterval(events$time,times)
  if(nrow(events)).tm_assert(all(events$frame>0L)&&
    all(events$time<times[events$frame]+interval_seconds[events$frame]),
    'Events lie outside the supplied half-open frame intervals')
  basemap <- .tm_basemap(basemap)
  .tm_assert(is.list(provenance)&&is.character(description)&&length(description)==1L&&!is.na(description)&&is.character(preparation)&&!anyNA(preparation)&&is.character(notes)&&!anyNA(notes),'Invalid descriptive metadata')
  out <- structure(c(list(roads=roads,values=values,events=events,times=times),
    list(interval_seconds=interval_seconds,intervals_explicit=intervals_explicit,clock=clock,unit=unit,title=title,basemap=basemap,
      description=description,preparation=preparation,notes=notes,provenance=provenance,
      regions=unique(c(roads$region[nzchar(roads$region)],events$region[nzchar(events$region)])),
      scale=NULL)),class='trafficmap_data')
  out$opposing <- .tm_opposing(roads$geometry,roads$opposite_id,roads$road_id)
  out$initial <- 1L
  out$timezone <- timezone
  out$time_label_format <- time_label_format
  .tm_add_metrics(out,measurements,metric_info,trends,primary_metric)
}

traffic_data <- function(roads=NULL,flow=NULL,events=NULL,times=NULL,
                         interval_seconds=NULL,unit='unit unspecified',title='Traffic map',
                         basemap=NULL,description='',preparation=character(),notes=character(),
                         provenance=list(),measurements=NULL,metric_info=list(),trends=NULL,
                         road_columns=NULL,flow_columns=NULL,event_columns=NULL,crs='EPSG:4326',
                         timezone=NULL,time_label_format=NULL) {
  args <- as.list(environment())
  .tm_crs(crs)
  args$roads <- .tm_rename_table(roads,road_columns,'roads')
  args$flow <- .tm_rename_table(flow,flow_columns,'flow')
  args$events <- .tm_rename_table(events,event_columns,'events')
  args[c('road_columns','flow_columns','event_columns','crs')] <- NULL
  do.call(.tm_traffic_data_reference,args)
}

.tm_add_metrics <- function(x,measurements,metric_info,trends,primary_metric='flow') {
 .tm_assert(is.list(metric_info),'metric_info must be a named list')
 if(length(metric_info)).tm_assert(!is.null(names(metric_info))&&!anyDuplicated(names(metric_info))&&all(nzchar(names(metric_info))),'Metric definitions need unique names')
 for(info in metric_info) {
  .tm_assert(is.list(info)&&all(vapply(info[c('unit','definition')],function(v)is.character(v)&&length(v)==1L&&!is.na(v)&&nzchar(v),logical(1))),'Every metric requires a supplied unit and definition')
  for(k in intersect(c('label','title','kind'),names(info))).tm_assert(is.character(info[[k]])&&length(info[[k]])==1L&&!is.na(info[[k]])&&nzchar(info[[k]]),paste('Metric metadata needs one nonempty',k))
  if(identical(info$kind,'duration')).tm_assert(is.character(info$time_alignment)&&length(info$time_alignment)==1L&&info$time_alignment%in%c('interval_start','interval_end'),'Provided duration needs its source time alignment')
 }
 if(!is.null(metric_info[[primary_metric]])).tm_assert(identical(metric_info[[primary_metric]]$unit,x$unit),
  paste('Primary metric metadata must retain the supplied', primary_metric, 'unit'))
 x$metrics<-list();x$metric_info<-metric_info;x$selected_metric<-primary_metric
 if(!is.null(measurements)) {
  .tm_assert(is.data.frame(measurements)&&all(c('road_id','time','metric','value')%in%names(measurements)),'measurements needs road_id, time, metric and value')
  if(nrow(measurements)) {
   t<-.tm_time(measurements$time);.tm_assert(t$clock==x$clock,'Metric clocks must match frame clocks');measurements$time<-t$value
   measurements$road_id<-as.character(measurements$road_id);measurements$metric<-as.character(measurements$metric)
   .tm_assert(!anyNA(measurements$metric)&&all(nzchar(measurements$metric))&&all(measurements$metric!=primary_metric)&&all(measurements$metric%in%names(metric_info)),
    paste('Every additional metric needs an explicit definition;', primary_metric, 'is reserved'))
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
   .tm_assert(all(trends$time%in%x$times)&&all(trends$metric%in%c(primary_metric,names(x$metrics)))&&!anyNA(trends[c('metric','region','definition','label')])&&all(nzchar(trends$definition))&&all(nzchar(trends$label))&&!anyDuplicated(trends[c('time','metric','region')]),'Provided trends require valid unique scope/time keys and explicit definitions')
   unknown <- setdiff(trends$region, c('', x$regions))
   .tm_assert(!length(unknown), paste('Unknown provided trend region:', paste(unknown, collapse = ', ')))
   flow_scale(trends$value)
  }
 }
 x$provided_trends<-trends;x
}

.prep_path <- function(path) {
  if(!is.character(path)||length(path)!=1L||is.na(path)||!nzchar(trimws(path)))
    stop('Supply one nonblank local path.',call.=FALSE)
  path<-path.expand(path)
  if(file.exists(path)||dir.exists(path))return(normalizePath(path,winslash='/',mustWork=TRUE))
  parent<-dirname(path)
  if(basename(path)=='.')return(.prep_path(parent))
  if(basename(path)=='..')return(dirname(.prep_path(parent)))
  if(identical(parent,path))stop('Cannot resolve output path.',call.=FALSE)
  file.path(.prep_path(parent),basename(path))
}

.prep_exists <- function(path) {
  link<-Sys.readlink(path)
  file.exists(path)||dir.exists(path)||(!is.na(link)&&nzchar(link))
}

.prep_destination <- function(path,source_files) {
  if(!is.character(source_files)||anyNA(source_files)||any(!nzchar(source_files)))
    stop('source_files must contain local source paths.',call.=FALSE)
  if(any(!file.exists(source_files)&!dir.exists(source_files)))
    stop('A declared source path does not exist.',call.=FALSE)
  destination<-.prep_path(path)
  for(source in source_files) {
    original<-.prep_path(source)
    compare<-if(.Platform$OS.type=='windows')tolower else identity
    a<-compare(destination);b<-compare(original)
    if(a==b||(dir.exists(original)&&startsWith(a,paste0(b,'/'))))
      stop('The output is a protected source location. Choose a new directory.',call.=FALSE)
  }
  if(.prep_exists(path))stop('Output directory already exists. Choose a new directory.',call.=FALSE)
  destination
}

.prep_sources <- function(paths) {
  paths<-unique(paths[file.exists(paths)&!dir.exists(paths)])
  data.frame(file=basename(paths),bytes=unname(file.info(paths)$size),
    sha256=vapply(paths,digest::digest,character(1),file=TRUE,algo='sha256',serialize=FALSE),
    stringsAsFactors=FALSE,row.names=NULL)
}

.prep_road_frames <- function(input) {
  if (!is.list(input) || is.null(names(input)) || anyDuplicated(names(input)))
    stop('Supply a uniquely named list of prepared tables.', call. = FALSE)
  if (any(c('nodes', 'flows', 'od', 'speed') %in% names(input)))
    stop('Flow, Speed and OD inputs are separate formats.', call. = FALSE)
  schema <- input[['schema']]
  if (!is.null(schema) && !identical(schema, 'bdynets-flow-input-v1'))
    stop('Unsupported Flow input schema.', call. = FALSE)
  roads <- input[['roads']]
  observations <- input[['flow']]
  if (!is.data.frame(roads) || !all(c('road_id', 'geometry') %in% names(roads)) ||
      !is.data.frame(observations) || !all(c('road_id', 'time', 'flow') %in% names(observations)))
    stop('Flow tables require roads(road_id, geometry) and flow(road_id, time, flow).', call. = FALSE)
  if (!is.character(roads$road_id) || !is.character(observations$road_id))
    stop('Road IDs must be character in both Flow tables.', call. = FALSE)
  if (is.null(input[['unit']])) stop('Supply the Flow unit.', call. = FALSE)
  input[['schema']] <- NULL
  if (any(!names(input) %in% names(formals(traffic_data))))
    stop('Unknown Flow input fields.', call. = FALSE)
  frames <- do.call(traffic_data, input)
  frames$input_type <- 'flow'
  frames
}

.prep_tables <- function(input) {
  keys<-c('roads','flow','events','measurements','trends')
  tables<-input[keys[!vapply(input[keys],is.null,logical(1))]]
  if(!is.null(tables$roads)) {
    roads<-tables$roads
    vertices<-lapply(seq_len(nrow(roads)),function(i) {
      g<-roads$geometry[[i]]
      data.frame(road_id=roads$road_id[i],vertex=seq_len(nrow(g)),longitude=g[,1],latitude=g[,2])
    })
    tables$`road-vertices`<-do.call(rbind,vertices)
    tables$roads$geometry<-NULL
  }
  tables
}

.prep_dictionary <- function(tables,input,custom) {
  definitions <- c(road_id='Stable road identifier joining roads, flow and measurements.',
    time='Observation time. POSIXct values are Unix seconds in CSV; numeric values retain the supplied source clock.',
    flow='Primary value in the declared unit. An observed zero and a missing value have separate meanings.',
    longitude='WGS84 longitude, degrees east.',latitude='WGS84 latitude, degrees north.',
    lon='WGS84 longitude, degrees east.',lat='WGS84 latitude, degrees north.',
    vertex='One-based vertex order within the road, preserving its direction.',
    metric='Measurement key joining metric_info.',value='Numeric measurement in the unit declared for its metric.',
    id='Source record identifier.',category='Source-supplied or explicitly mapped classification.',
    description='Retained source description.',region='Declared geographic selection.')
  required <- list(roads='road_id',flow=c('road_id','time','flow'),events=c('time','lon','lat'),
    measurements=c('road_id','time','metric','value'),`road-vertices`=c('road_id','vertex','longitude','latitude'))
  rows<-lapply(names(tables),function(name) {
    tab<-tables[[name]]
    fields<-names(tab)
    meaning<-unname(definitions[fields])
    meaning[is.na(meaning)]<-paste('Retained source attribute',fields[is.na(meaning)])
    type<-vapply(tab,function(x)if(inherits(x,'POSIXct'))'timestamp_seconds' else if(is.factor(x))'character' else if(is.list(x))'json' else typeof(x),'')
    units<-rep('',length(fields))
    units[fields=='flow']<-input$unit
    units[fields %in% c('longitude','latitude','lon','lat')]<-'degrees'
    units[fields=='time']<-'seconds'
    data.frame(table=name,field=fields,type=unname(type),meaning=meaning,unit=units,
      required=fields%in%required[[name]],stringsAsFactors=FALSE)
  })
  dictionary<-do.call(rbind,rows)
  if(!is.null(custom)) {
    if(!is.data.frame(custom)||!all(c('table','field','meaning')%in%names(custom)))
      stop('Dictionary additions need table, field and meaning.',call.=FALSE)
    key<-paste(dictionary$table,dictionary$field,sep='/')
    custom_key<-paste(custom$table,custom$field,sep='/')
    if(anyDuplicated(custom_key)||any(!custom_key%in%key))stop('Dictionary additions must identify existing fields once.',call.=FALSE)
    at<-match(custom_key,key)
    for(field in intersect(names(custom),names(dictionary)))dictionary[at,field]<-custom[[field]]
  }
  rownames(dictionary)<-NULL
  dictionary
}

.prep_csv <- function(table) {
  lapply(table,function(x) {
    if(inherits(x,'POSIXct'))x<-as.numeric(x)
    if(is.factor(x))x<-as.character(x)
    if(is.list(x))return(vapply(x,function(v)as.character(jsonlite::toJSON(.tm_provenance_for_json(v),auto_unbox=TRUE,null='null',na='null',digits=NA)),''))
    if(is.double(x)) {
      missing<-is.na(x);x<-sprintf('%.17g',x);x[missing]<-NA_character_
    }
    x
  })
}

write_traffic_input <- function(input,output_dir,source_files=character(),native=NULL,dictionary=NULL) {
  target<-.prep_destination(output_dir,source_files)
  parent<-dirname(target);dir.create(parent,recursive=TRUE,showWarnings=FALSE)
  lock<-paste0(target,'.preparation-lock')
  if(!dir.create(lock,showWarnings=FALSE))stop('Another preparation is using this output directory.',call.=FALSE)
  on.exit(unlink(lock,recursive=TRUE,expand=FALSE),add=TRUE)
  .prep_destination(output_dir,source_files)
  stage<-tempfile('prepared-data-',tmpdir=parent)
  if(!dir.create(stage))stop('Cannot create preparation staging directory.',call.=FALSE)
  on.exit(unlink(stage,recursive=TRUE,expand=FALSE),add=TRUE)
  info_before<-file.info(source_files)[,c('size','mtime'),drop=FALSE]
  identities<-.prep_sources(source_files)
  object<-input
  object$provenance$preparation_sources <- identities
  road_frames <- .prep_road_frames(object)
  object$schema <- 'bdynets-flow-input-v1'
  tables <- .prep_tables(object)
  dictionary <- .prep_dictionary(tables, object, dictionary)
  na_token<-'\\N'
  strings<-unlist(lapply(tables,function(t)unlist(lapply(t,function(x)if(is.character(x))x else character()),use.names=FALSE)),use.names=FALSE)
  while(na_token%in%strings)na_token<-paste0(na_token,'_')
  saveRDS(object,file.path(stage,'input.rds'),compress='xz',version=3)
  saveRDS(road_frames,file.path(stage,'flow-frames.rds'),compress='xz',version=3)
  if(!is.null(native))saveRDS(native,file.path(stage,'source.rds'),compress='xz',version=3)
  for(name in names(tables)) {
    tab<-as.data.frame(.prep_csv(tables[[name]]),stringsAsFactors=FALSE,optional=TRUE)
    utils::write.csv(tab,file.path(stage,paste0(name,'.csv')),row.names=FALSE,na=na_token,fileEncoding='UTF-8')
  }
  utils::write.csv(dictionary,file.path(stage,'data-dictionary.csv'),row.names=FALSE,na='')
  utils::write.csv(identities,file.path(stage,'source-files.csv'),row.names=FALSE)
  metadata<-object[setdiff(names(object),names(tables))]
  metadata$input_type <- 'flow'
  metadata$csv<-list(missing=na_token,timestamp='POSIXct times are Unix seconds; numeric times retain the declared source clock.',
                     identifiers='Read identifier fields as character.',data='input.rds preserves original types, geometry, timezones and nested provenance.')
  writeLines(jsonlite::toJSON(.tm_provenance_for_json(metadata),auto_unbox=TRUE,null='null',na='null',digits=NA,pretty=TRUE),file.path(stage,'metadata.json'),useBytes=TRUE)
  files<-sort(list.files(stage))
  utils::write.csv(data.frame(file=files,md5=unname(tools::md5sum(file.path(stage,files)))),file.path(stage,'checksums.csv'),row.names=FALSE)
  if(!identical(info_before,file.info(source_files)[,c('size','mtime'),drop=FALSE]))stop('A source changed during preparation.',call.=FALSE)
  .prep_destination(output_dir,source_files)
  if(!file.rename(stage,target))stop('Cannot publish the completed preparation.',call.=FALSE)
  invisible(file.path(target,'input.rds'))
}
