# Prepared datasets are published only into new directories. Source locations are
# checked before the input argument is evaluated.
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
.prep_input_type <- function(input) {
  if(!is.list(input)||is.null(names(input))||anyDuplicated(names(input)))
    stop('Supply named road inputs or named OD inputs.',call.=FALSE)
  if(identical(input[['schema']],'bdynets-traffic-input-v1')) {
    args<-input[intersect(names(input),names(formals(traffic_input)))]
    do.call(traffic_input,args)
    return('legacy')
  }
  has_road<-any(c('roads','flow','events','measurements','metric_info','trends')%in%names(input))
  has_od<-any(c('nodes','flows','od')%in%names(input))
  if(has_road&&has_od)stop('Road and OD inputs are separate formats; do not combine their tables.',call.=FALSE)
  if(has_road) {
    if(!all(c('roads','flow','unit')%in%names(input))||is.null(input[['roads']])||is.null(input[['flow']]))
      stop('Road inputs require roads, flow and a declared unit.',call.=FALSE)
    if(!is.data.frame(input[['roads']])||!all(c('road_id','geometry')%in%names(input[['roads']]))||
       !is.data.frame(input[['flow']])||!all(c('road_id','time','flow')%in%names(input[['flow']])))
      stop('Road tables require roads(road_id, geometry) and flow(road_id, time, flow).',call.=FALSE)
    if(!is.character(input[['roads']]$road_id)||!is.character(input[['flow']]$road_id))
      stop('Road IDs must be character in both road tables.',call.=FALSE)
    if(any(!names(input)%in%names(formals(traffic_data))))stop('Unknown road input fields.',call.=FALSE)
    do.call(traffic_data,input)
    return('road')
  }
  if(has_od) {
    required<-c('nodes','flows','unit','time_kind')
    allowed<-c(required,'timezone','interval_seconds','crs','node_columns','flow_columns',
      'provenance','title','description','preparation','notes')
    if(!all(required%in%names(input))||any(!names(input)%in%allowed))
      stop('OD inputs require nodes, flows, unit and time_kind; road fields do not apply.',call.=FALSE)
    if(!is.data.frame(input[['nodes']])||!all(c('node_id','longitude','latitude')%in%names(input[['nodes']]))||
       !is.data.frame(input[['flows']])||!all(c('origin_id','destination_id','flow')%in%names(input[['flows']])))
      stop('OD tables require nodes(node_id, longitude, latitude) and flows(origin_id, destination_id, flow).',call.=FALSE)
    if(!is.character(input[['nodes']]$node_id)||!is.character(input[['flows']]$origin_id)||
       !is.character(input[['flows']]$destination_id))stop('OD IDs must be character in both OD tables.',call.=FALSE)
    kind<-input[['time_kind']]
    if(!is.character(kind)||length(kind)!=1L||is.na(kind)||!kind%in%c('instant','interval_start','untimed','unspecified'))
      stop('Declare the OD time_kind.',call.=FALSE)
    time_column<-if(is.null(input[['flow_columns']]))'time' else unname(input[['flow_columns']]['time'])
    timed<-length(time_column)==1L&&!is.na(time_column)&&time_column%in%names(input[['flows']])
    if((kind=='untimed')==timed)stop('OD time_kind must agree with the supplied time column.',call.=FALSE)
    if(timed&&!inherits(input[['flows']][[time_column]],'POSIXt')&&is.null(input[['timezone']]))
      stop('Supply a timezone for text OD timestamps or pass parsed POSIXct instants.',call.=FALSE)
    interval<-input[['interval_seconds']]
    if(!is.null(interval)&&(kind!='interval_start'||!is.numeric(interval)||length(interval)!=1L||
      !is.finite(interval)||interval<=0))stop('OD interval_seconds requires a positive interval_start width.',call.=FALSE)
    args<-input[intersect(names(input),names(formals(traffic_od_data)))]
    if(is.null(args$timezone))args$timezone<-'UTC'
    do.call(traffic_od_data,args)
    return('od')
  }
  stop('Supply road tables (roads and flow) or OD tables (nodes and flows).',call.=FALSE)
}
.prep_tables <- function(input) {
  keys<-c('roads','flow','events','nodes','flows','od','measurements','trends')
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
  definitions<-c(road_id='Stable road identifier joining roads, flow and measurements.',
    node_id='Stable node identifier used by origin and destination.',
    origin_id='Departure node ID joining nodes.node_id.',destination_id='Arrival node ID joining nodes.node_id.',
    time='Observation time. POSIXct values are Unix seconds in CSV; numeric values retain the supplied source clock.',
    flow='Primary value in the declared unit. An observed zero and a missing value have separate meanings.',
    ride_id='Stable identifier for one individual trip.',
    longitude='WGS84 longitude, degrees east.',latitude='WGS84 latitude, degrees north.',
    lon='WGS84 longitude, degrees east.',lat='WGS84 latitude, degrees north.',
    vertex='One-based vertex order within the road, preserving its direction.',
    metric='Measurement key joining metric_info.',value='Numeric measurement in the unit declared for its metric.',
    id='Source record identifier.',category='Source-supplied or explicitly mapped classification.',
    description='Retained source description.',region='Declared geographic selection.',time_label='Supplied human-readable clock label.')
  required<-list(roads='road_id',flow=c('road_id','time','flow'),nodes=c('node_id','longitude','latitude'),
    od=c('origin_id','destination_id','flow'),flows=c('origin_id','destination_id','flow'),events=c('time','lon','lat'),measurements=c('road_id','time','metric','value'),
    `road-vertices`=c('road_id','vertex','longitude','latitude'))
  rows<-lapply(names(tables),function(name) {
    tab<-tables[[name]]
    fields<-names(tab)
    meaning<-unname(definitions[fields])
    meaning[is.na(meaning)]<-paste('Retained source attribute',fields[is.na(meaning)])
    type<-vapply(tab,function(x)if(inherits(x,'POSIXct'))'timestamp_seconds' else if(is.factor(x))'character' else if(is.list(x))'json' else typeof(x),'')
    units<-rep('',length(fields))
    units[fields=='flow']<-if(name=='od')input$od_unit else input$unit
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
.prep_validation <- function(input,custom) {
  tables<-.prep_tables(input)
  rows<-lapply(names(tables),function(name)data.frame(check=paste(name,'rows'),observed=nrow(tables[[name]]),
    expected=NA_real_,passed=TRUE,detail='Rows retained in the prepared table.'))
  if(!is.null(input[['flow']]))rows[[length(rows)+1L]]<-data.frame(check=c('flow observed','flow missing','flow observed zero'),
    observed=c(sum(!is.na(input[['flow']]$flow)),sum(is.na(input[['flow']]$flow)),sum(input[['flow']]$flow==0,na.rm=TRUE)),
    expected=NA_real_,passed=TRUE,detail='Recorded zero contributes to observed coverage.')
  result<-do.call(rbind,rows)
  if(!is.null(custom)) {
    if(!is.data.frame(custom)||!all(names(result)%in%names(custom)))
      stop('Validation additions need check, observed, expected, passed and detail.',call.=FALSE)
    result<-rbind(result,custom[names(result)])
  }
  if(anyNA(result$passed)||any(!result$passed))stop('Prepared-data validation did not pass.',call.=FALSE)
  rownames(result)<-NULL
  result
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

#' Publish validated prepared data to a new directory
#' @param input Road inputs (roads and flow) or OD inputs (nodes and flows).
#'   Evaluation starts after path protection. Earlier standard objects remain readable.
#' @param output_dir A new output directory, distinct from all sources.
#' @param source_files Source paths to protect. Existing directory paths protect descendants.
#' @param native Optional native source object to preserve in source.rds.
#' @param dictionary Optional field descriptions with table, field and meaning columns.
#' @param validation Optional checks with check, observed, expected, passed and detail.
#' @return The path to the published input.rds.
#' @export
write_traffic_input <- function(input,output_dir,source_files=character(),native=NULL,dictionary=NULL,validation=NULL) {
  target<-.prep_destination(output_dir,source_files)
  parent<-dirname(target);dir.create(parent,recursive=TRUE,showWarnings=FALSE)
  lock<-paste0(target,'.preparation-lock')
  if(!dir.create(lock,showWarnings=FALSE))stop('Another preparation is using this output directory.',call.=FALSE)
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  .prep_destination(output_dir,source_files)
  stage<-tempfile('prepared-data-',tmpdir=parent)
  if(!dir.create(stage))stop('Cannot create preparation staging directory.',call.=FALSE)
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  info_before<-file.info(source_files)[,c('size','mtime'),drop=FALSE]
  identities<-.prep_sources(source_files)
  object<-input
  input_type<-.prep_input_type(object)
  # Declare a published road bundle as a standard input, after its road or OD
  # shape has been classified. read_traffic() and traffic_flow_map() can then
  # take the written file directly instead of reconstructing the arguments.
  # OD bundles keep their own shape and receive no road schema.
  if(identical(input_type,'road'))
    object$schema<-if(is.null(object$schema))'bdynets-traffic-input-v1' else object$schema
  object$provenance$preparation_sources<-identities
  tables<-.prep_tables(object)
  dictionary<-.prep_dictionary(tables,object,dictionary)
  validation<-.prep_validation(object,validation)
  na_token<-'\\N'
  strings<-unlist(lapply(tables,function(t)unlist(lapply(t,function(x)if(is.character(x))x else character()),use.names=FALSE)),use.names=FALSE)
  while(na_token%in%strings)na_token<-paste0(na_token,'_')
  saveRDS(object,file.path(stage,'input.rds'),compress='xz',version=3)
  if(!identical(readRDS(file.path(stage,'input.rds')),object))stop('RDS readback did not match.',call.=FALSE)
  if(!is.null(native))saveRDS(native,file.path(stage,'source.rds'),compress='xz',version=3)
  for(name in names(tables)) {
    tab<-as.data.frame(.prep_csv(tables[[name]]),stringsAsFactors=FALSE,optional=TRUE)
    utils::write.csv(tab,file.path(stage,paste0(name,'.csv')),row.names=FALSE,na=na_token,fileEncoding='UTF-8')
  }
  utils::write.csv(dictionary,file.path(stage,'data-dictionary.csv'),row.names=FALSE,na='')
  utils::write.csv(validation,file.path(stage,'validation.csv'),row.names=FALSE,na='')
  utils::write.csv(identities,file.path(stage,'source-files.csv'),row.names=FALSE)
  metadata<-object[setdiff(names(object),names(tables))]
  metadata$roads<-NULL
  metadata$input_type<-input_type
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
