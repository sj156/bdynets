# The LA archive is hourly; D04 has twelve observed five-minute counts per hour.
pems_counts <- function(records,station_ids,labels,fields=c(time='V1',station='V2',count='V10')) {
  if(!setequal(names(fields),c('time','station','count'))||anyNA(fields)||anyDuplicated(fields)||
     any(!fields%in%names(records)))stop('Map time, station and count to distinct source fields.')
  if(!length(station_ids)||anyNA(station_ids)||anyDuplicated(station_ids)||
     !length(labels)||anyNA(labels)||anyDuplicated(labels))stop('Declare unique stations and source-clock slots.')
  row<-match(as.character(records[[fields['station']]]),station_ids)
  column<-match(as.character(records[[fields['time']]]),labels)
  value<-suppressWarnings(as.numeric(records[[fields['count']]]))
  if(anyNA(row)||anyNA(column))stop('An unexpected source station or timestamp is present.')
  if(any(!is.finite(value)|value<0|value!=trunc(value)))stop('Five-minute total flow must be a nonnegative observed count.')
  cell<-row+(column-1L)*length(station_ids)
  if(anyDuplicated(cell))stop('Duplicate station and timestamp.')
  if(length(cell)!=length(station_ids)*length(labels))stop('Missing station and timestamp; missing counts are not zero.')
  counts<-matrix(NA_integer_,length(station_ids),length(labels),dimnames=list(station_ids,labels))
  counts[cell]<-value
  counts
}

pems_scope <- function(events,scopes) {
  if(any(!events$district%in%c('04','07','08','12')))stop('An incident district has no declared regional scope.')
  region<-ifelse(events$district=='04','San Francisco Bay Area','Los Angeles Area')
  at<-match(region,scopes$region)
  if(anyNA(at))stop('An incident region is missing from the supplied bounds.')
  longitude<-as.numeric(events$lon);latitude<-as.numeric(events$lat)
  is.finite(longitude)&is.finite(latitude)&longitude>=scopes$west[at]&longitude<=scopes$east[at]&
    latitude>=scopes$south[at]&latitude<=scopes$north[at]
}

pems_summary_records <- function(rows) {
  required<-c('V1','V4','V5','V6','V7','V10','V11','V12')
  if(!all(required%in%names(rows)))stop('The CHP summary fields are incomplete.')
  text<-as.character(rows$V4)
  if(anyNA(text)||any(!grepl('^[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}$',text)))
    stop('CHP start times must use the complete source clock.')
  clock<-as.POSIXct(strptime(text,'%m/%d/%Y %H:%M:%S',tz='UTC'))
  if(anyNA(clock)||any(format(clock,'%m/%d/%Y %H:%M:%S',tz='UTC')!=text))stop('Invalid CHP source-clock timestamp.')
  clean<-function(x)trimws(gsub('[[:space:]]+',' ',as.character(x)))
  result<-data.frame(id=as.character(rows$V1),reported_time=format(clock,'%Y-%m-%d %H:%M:%S',tz='UTC'),
    district=sprintf('%02d',as.integer(rows$V12)),lon=as.character(rows$V11),lat=as.character(rows$V10),
    source_type=clean(rows$V5),location=clean(rows$V6),area=clean(rows$V7),stringsAsFactors=FALSE)
  result$description<-mapply(function(type,location,area)paste(Filter(nzchar,c(type,location,area)),collapse=' | '),
    result$source_type,result$location,result$area,USE.NAMES=FALSE)
  result
}

pems_input <- function(native) {
  if(!identical(native$schema,'pems-station-events-v1'))stop('Expected native PeMS station observations and CHP summaries.')
  roads<-native$roads;roads$road_id<-as.character(roads$road_id);roads$road_class<-'motorway'
  times<-native$settings$times;labels<-native$settings$time_labels;hours<-length(times)
  if(!hours||anyNA(times)||any(!is.finite(times))||any(diff(times)!=3600)||any(times%%3600!=0)||
     length(labels)!=hours||anyDuplicated(roads$road_id))stop('Road IDs and complete source-hour starts are required.')
  la<-native$la;position<-t(la$position_values)
  if(nrow(position)!=length(la$position_station)||ncol(position)!=hours||anyNA(la$position_station))
    stop('LA active-position values do not align with station IDs and hours.')
  position[position==la$missing_value]<-NA_real_
  if(any(!is.na(position)&(!is.finite(position)|position<0)))stop('Invalid LA hourly position value.')
  available<-rowsum((!is.na(position))*1L,la$position_station,reorder=FALSE)
  la_hour<-rowsum(replace(position,is.na(position),0),la$position_station,reorder=FALSE)/available
  la_hour[available==0]<-NA_real_
  d04<-native$d04
  if(anyDuplicated(d04$station_id)||anyDuplicated(d04$matched_station_id)||
     nrow(d04$counts)!=length(d04$station_id)||ncol(d04$counts)!=12L*hours||
     length(d04$minute)!=12L*hours||anyNA(d04$minute)||any(diff(d04$minute)!=5)||
     !identical(as.numeric(d04$minute[seq(1,length(d04$minute),12)]),as.numeric((times-times[1]+times[1]%%86400)/60)))
    stop('D04 source minutes and output hours do not align.')
  if(any(!is.finite(d04$counts)|d04$counts<0|d04$counts!=trunc(d04$counts)))stop('D04 source counts must be finite and nonnegative.')
  station_row<-match(d04$matched_station_id,d04$station_id)
  if(anyNA(station_row))stop('A matched D04 station is absent from its source observations.')
  counts<-d04$counts[station_row,,drop=FALSE]
  first<-seq(1,ncol(counts),12)
  bay_hour<-matrix(0,nrow(counts),hours,dimnames=list(d04$matched_station_id,NULL))
  for(offset in 0:11)bay_hour<-bay_hour+counts[,first+offset,drop=FALSE]
  values<-matrix(NA_real_,nrow(roads),hours)
  for(region in c('Los Angeles Area','San Francisco Bay Area')) {
    use<-which(roads$region==region);stations<-if(region=='Los Angeles Area')la_hour else bay_hour
    a<-stations[match(roads$from_station[use],rownames(stations)),,drop=FALSE]
    b<-stations[match(roads$to_station[use],rownames(stations)),,drop=FALSE]
    n<-(!is.na(a))+(!is.na(b));mean<-(replace(a,is.na(a),0)+replace(b,is.na(b),0))/n
    mean[n==0]<-NA_real_;values[use,]<-mean
  }
  if(any(!roads$region%in%c('Los Angeles Area','San Francisco Bay Area')))stop('A road has no declared station region.')
  flow<-data.frame(road_id=rep(roads$road_id,hours),time=rep(times,each=nrow(roads)),flow=as.vector(values))
  source<-native$event_summaries
  if(anyNA(source$id)||anyDuplicated(source$id)||any(!grepl('^2026-06-16 ([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$',source$reported_time)))
    stop('CHP IDs and reported start clocks must identify the selected source day.')
  keep<-pems_scope(source,native$scopes);e<-source[keep,,drop=FALSE]
  # UTC provides a numeric encoding of the source clock, without assigning a source timezone.
  event_clock<-as.numeric(as.POSIXct(e$reported_time,tz='UTC'))
  key<-tolower(trimws(gsub('[[:space:]]+',' ',e$source_type)))
  category<-native$event_categories$category[match(key,tolower(trimws(native$event_categories$source_type)))]
  category[is.na(category)]<-'Other alert'
  events<-data.frame(id=e$id,time=event_clock,lon=as.numeric(e$lon),lat=as.numeric(e$lat),category=category,
    description=e$description,time_label=e$reported_time,
    region=ifelse(e$district=='04','San Francisco Bay Area','Los Angeles Area'),stringsAsFactors=FALSE)
  bdynets::traffic_input(roads=roads,flow=flow,events=events,times=times,time_labels=labels,
    interval_seconds=rep(3600,hours),unit='vehicles/hour',title='PeMS traffic flow and reported events | 2026-06-16',
    provenance=list(source_dataset='PeMS and CHP',day='2026-06-16',
      clock='Supplied source wall clock; numeric encoding does not establish a UTC offset.',
      source_position_hours=length(position),d04_source_records=length(d04$counts),d04_retained_records=length(counts),
      d04_count_before=sum(counts),d04_count_after=sum(bay_hour),
      excluded_d04_stations=setdiff(d04$station_id,d04$matched_station_id),source_incidents=nrow(source),
      included_incidents=nrow(events),outside_scope=sum(!keep),missing_link_hours=sum(is.na(values)),
      measurement='Original LA station-position means and D04 sums of twelve native five-minute counts; available endpoint station means for display links.'))
}

prepare_example <- function(base='.',output_dir=file.path(base,'prepared'),source_file=file.path(base,'data','source.rds'),
                            raw_root=NULL,reference_root=NULL,register_file=file.path(base,'data','case-register.rds')) {
  if(xor(is.null(raw_root),is.null(reference_root)))stop('Supply both the PeMS archive and project reference roots.')
  files<-if(is.null(raw_root))source_file else c(register_file,raw_root,reference_root,pems_files(raw_root,reference_root))
  native<-NULL;input<-NULL
  build<-function() {
    native<<-if(is.null(raw_root))readRDS(source_file) else read_pems_archives(raw_root,reference_root,register_file)
    input<<-pems_input(native);input
  }
  meanings<-c(district='Two-digit PeMS district.',freeway='Source freeway number.',direction='Source travel direction code.',
    from_station='Upstream detector ID.',to_station='Downstream detector ID.',from_abs_pm='Upstream absolute postmile.',
    to_abs_pm='Downstream absolute postmile.',id='Project display-link index, local to its region.',
    from_station_index='Upstream archived station-list position, when supplied.',to_station_index='Downstream archived station-list position, when supplied.',
    region='The existing regional selection.',road_class='Motorway classification for the supplied freeway display links.')
  dictionary<-data.frame(table='roads',field=names(meanings),meaning=unname(meanings))
  checks<-function() {
    result<-data.frame(check=c('D04 native station-hour slots','D04 count conservation','Reported starts in supplied extents'),
    observed=c(length(native$d04$counts),input$provenance$d04_count_after,nrow(input$events)),
    expected=c(length(native$d04$station_id)*12L*length(native$settings$times),input$provenance$d04_count_before,sum(pems_scope(native$event_summaries,native$scopes))),
    passed=TRUE,detail=c('Twelve unique native five-minute counts per station-hour.','Station-hour totals sum the same retained observations.','Only the declared geographic selection is applied.'))
    result$passed<-result$observed==result$expected;result
  }
  bdynets::write_traffic_input(build(),output_dir,source_files=files,native=native,dictionary=dictionary,validation=checks())
}

pems_files <- function(raw_root,reference_root) {
  map<-file.path(reference_root,'PeMS_2020_2026_maps','map')
  chp<-file.path(raw_root,'chp','2026','2026_06')
  files<-c(detector=file.path(raw_root,'5min','D04','2026','2026_06','d04_text_station_5min_2026_06_16.txt.gz'),
    metadata=file.path(raw_root,'metadata','D04','d04_text_meta_2026_04_08.txt.gz'),
    station_map=file.path(reference_root,'processing_runtime','clean_data_update','mapping_cache','2026-06_d04','station_maps','snapshot_d04_2026-04-08.csv'),
    hourly=file.path(map,'data','flow','flow_2026_06.binz'),manifest=file.path(map,'data','flow','manifest.json'),
    geometry=file.path(map,'la_area_freeway_volume_2020_01_to_2026_06.html'),
    incidents=file.path(chp,'all_text_chp_incidents_month_2026_06.txt.gz'),details=file.path(chp,'all_text_chp_incident_det_month_2026_06.txt.gz'))
  annotation<-file.path(reference_root,'CHP_full_English_2026_10_21','all_months','.work','2026-06','events.csv')
  if(file.exists(annotation))files<-c(files,annotations=annotation)
  files
}
read_pems_archives <- function(raw_root=NULL,reference_root=NULL,register_file,files=NULL) {
  if(is.null(files))files<-pems_files(raw_root,reference_root)
  required<-c('detector','metadata','station_map','hourly','manifest','geometry','incidents','details')
  if(!is.character(files)||!all(required%in%names(files))||anyDuplicated(names(files)))stop('Declare each original archive role once.')
  if(any(!file.exists(files)))stop('Required PeMS archive is missing: ',paste(names(files)[!file.exists(files)],collapse=', '))
  register<-readRDS(register_file)
  if(!identical(register$schema,'pems-links-and-rules-v1'))stop('The selected display links need a compatible endpoint register.')
  literal<-grep('^const config = ',readLines(files['geometry'],warn=FALSE),value=TRUE)
  if(length(literal)!=1L)stop('Expected one source-map configuration.')
  config<-jsonlite::fromJSON(sub(';[[:space:]]*$','',sub('^const config = ','',literal)))
  la<-config$links
  reverse<-la$id%in%register$la_reverse_id
  if(anyDuplicated(la$id)||any(!register$la_reverse_id%in%la$id))stop('LA display IDs differ from the direction register.')
  la$geometry<-lapply(seq_len(nrow(la)),function(i) {
    g<-la$coords[[i]][,2:1,drop=FALSE]
    if(reverse[i])g[nrow(g):1,,drop=FALSE] else g
  })
  la$coords<-NULL
  for(key in c('station','abs_pm','station_index')) {
    a<-paste0('from_',key);b<-paste0('to_',key);saved<-la[[a]][reverse]
    la[[a]][reverse]<-la[[b]][reverse];la[[b]][reverse]<-saved
  }
  la$region<-'Los Angeles Area';la$road_id<-as.character(la$id)
  meta<-read.delim(files['metadata'],colClasses='character',quote='',na.strings=NULL,check.names=FALSE,fileEncoding='latin1')
  if(anyDuplicated(meta$ID))stop('Metadata station IDs are duplicated.')
  keys<-register$d04_links
  a<-meta[match(keys$from_station,meta$ID),];b<-meta[match(keys$to_station,meta$ID),]
  if(anyNA(a$ID)||anyNA(b$ID)||any(a$Fwy!=b$Fwy|a$Dir!=b$Dir|a$District!='4'|b$District!='4'|a$Type!='ML'|b$Type!='ML'))stop('D04 link endpoints do not agree with station metadata.')
  bay<-data.frame(district='04',freeway=a$Fwy,direction=a$Dir,from_station=a$ID,to_station=b$ID,
    from_abs_pm=as.numeric(a$Abs_PM),to_abs_pm=as.numeric(b$Abs_PM),id=seq_len(nrow(keys))-1L,
    from_station_index=NA_integer_,to_station_index=NA_integer_,region='San Francisco Bay Area',road_id=keys$road_id)
  bay$geometry<-lapply(seq_len(nrow(keys)),function(i)cbind(as.numeric(c(a$Longitude[i],b$Longitude[i])),as.numeric(c(a$Latitude[i],b$Latitude[i]))))
  roads<-rbind(la[names(bay)],bay)
  manifests<-jsonlite::read_json(files['manifest'],simplifyVector=FALSE)
  june<-Filter(function(x)identical(x$key,'2026_06'),manifests)
  if(length(june)!=1L)stop('Expected one June hourly archive entry.')
  month<-june[[1]];positions<-unlist(month$segmentPositions,use.names=FALSE);n<-length(positions)
  station<-unlist(config$positionStations,use.names=FALSE)
  if(month$timeSteps!=720L||month$values!=n*720L||anyDuplicated(positions)||any(positions<0|positions>=length(station)))stop('Hourly binary dimensions and active-position mapping disagree.')
  con<-gzfile(files['hourly'],'rb')
  bytes<-tryCatch(readBin(con,'raw',n*720L*2L+1L),finally=close(con))
  if(length(bytes)!=n*720L*2L)stop('Hourly archive byte count is incorrect.')
  times<-register$settings$times
  date<-unique(format(as.POSIXct(times,origin='1970-01-01',tz='UTC'),'%Y-%m-%d',tz='UTC'))
  if(length(date)!=1L||date!='2026-06-16')stop('The source register must select hours on 2026-06-16.')
  month_start<-as.numeric(as.POSIXct('2026-06-01 00:00:00',tz='UTC'))
  frame<-(times-month_start)/3600
  if(any(!is.finite(frame)|frame!=trunc(frame)|frame<0|frame>=month$timeSteps))stop('Selected hours fall outside the monthly archive.')
  rawday<-bytes[as.vector(outer(seq_len(n*2L),as.integer(frame)*n*2L,'+'))]
  values<-readBin(rawday,integer(),length(times)*n,size=2L,signed=FALSE,endian='little')
  la_native<-list(position_values=matrix(values,length(times),byrow=TRUE,dimnames=list(NULL,paste0('V',seq_len(n)))),
                  position_station=station[positions+1L],missing_value=65535L)
  ids<-sort(meta$ID[meta$District=='4'&meta$Type=='ML'])
  station_map<-read.csv(files['station_map'],colClasses='character')
  if(anyDuplicated(station_map$station_id)||any(!station_map$station_id%in%ids))stop('D04 station mapping does not align with ML metadata.')
  types<-rep('NULL',52L);types[c(1,2,3,6,10)]<-c('character','character','integer','character','integer')
  records<-read.csv(files['detector'],header=FALSE,colClasses=types,quote='')
  if(any(records$V3!=4L))stop('The D04 archive contains a different district.')
  labels<-format(as.POSIXct(rep(times,each=12)+rep(seq(0,3300,300),length(times)),origin='1970-01-01',tz='UTC'),'%m/%d/%Y %H:%M:%S',tz='UTC')
  counts<-pems_counts(records[records$V6=='ML',],ids,labels)
  colnames(counts)<-paste0('V',seq_len(ncol(counts)))
  d04<-list(counts=counts,station_id=ids,minute=rep(times%%86400/60,each=12)+rep(seq(0,55,5),length(times)),matched_station_id=ids[ids%in%station_map$station_id])
  types<-rep('NULL',20);types[c(1,4:7,10:12)]<-'character'
  summary<-read.csv(files['incidents'],header=FALSE,colClasses=types,na.strings=NULL)
  summary<-summary[startsWith(summary$V4,'06/16/2026 ')&summary$V12%in%c('4','7','8','12'),]
  events<-pems_summary_records(summary)
  events<-events[order(!pems_scope(events,register$scopes),events$district=='04',events$reported_time,events$id),];rownames(events)<-NULL
  con<-gzfile(files['details'],'rt');on.exit(close(con),add=TRUE)
  message_keys<-character()
  repeat {
    lines<-readLines(con,n=50000L,warn=FALSE)
    if(!length(lines))break
    lines<-grep('^[0-9]+,[0-9]+,[0-9]{2}/[0-9]{2}/[0-9]{4} ',lines,value=TRUE,useBytes=TRUE)
    lines<-lines[sub(',.*','',lines,useBytes=TRUE)%in%events$id]
    message_keys<-c(message_keys,sub('^([0-9]+,[0-9]+),.*','\\1',lines,useBytes=TRUE))
  }
  if(anyDuplicated(message_keys))stop('Incident detail message IDs are duplicated.')
  nmsg<-tabulate(match(sub(',.*','',message_keys),events$id),nbins=nrow(events))
  detail<-events$area
  if('annotations'%in%names(files)) {
    names_needed<-c('Incident ID','Incident detail summary','Raw message count')
    header<-names(read.csv(files['annotations'],nrows=0,check.names=FALSE))
    if(!all(names_needed%in%header))stop('English annotations lack required ID and message fields.')
    english<-read.csv(files['annotations'],check.names=FALSE,na.strings=NULL,colClasses=ifelse(header%in%names_needed,'character','NULL'))
    use<-events$district!='04'&pems_scope(events,register$scopes);at<-match(events$id[use],english[['Incident ID']])
    if(anyNA(at)||anyDuplicated(english[['Incident ID']])||any(as.integer(english[['Raw message count']][at])!=nmsg[use]))stop('English annotations do not align with raw incident message counts.')
    detail[use]<-english[['Incident detail summary']][at]
  }
  events$description<-mapply(function(type,place,text)paste(Filter(nzchar,c(type,place,text)),collapse=' | '),events$source_type,events$location,detail,USE.NAMES=FALSE)
  list(schema='pems-station-events-v1',roads=roads,la=la_native,d04=d04,event_summaries=events,
    event_categories=register$event_categories,scopes=register$scopes,settings=register$settings,
    source_files=data.frame(file=basename(files),bytes=unname(file.info(files)$size),row.names=NULL),
    incident_details=list(records=sum(nmsg),events_with_detail=sum(nmsg>0L)))
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
  if(length(script)!=1L)stop('Rscript entrypoint is required.')
  if(!file.exists(script))script<-gsub('~+~',' ',script,fixed=TRUE)
  base<-dirname(normalizePath(script,mustWork=TRUE))
  source(file.path(base,'..','..','bdynets','inst','preparation-session.R'))
  preparation_session(base)
  args<-commandArgs(TRUE)
  if(!length(args))result<-prepare_example(base) else if(length(args)==1L)result<-prepare_example(base,output_dir=args[1]) else if(length(args)==4L&&args[1]=='--raw')result<-prepare_example(base,output_dir=args[4],raw_root=args[2],reference_root=args[3]) else stop('Usage: Rscript prepare.R [NEW_DIRECTORY | --raw ARCHIVE_ROOT REFERENCE_ROOT NEW_DIRECTORY]')
  cat('Prepared',result,'\n')
}
