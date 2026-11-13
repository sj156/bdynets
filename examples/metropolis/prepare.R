# The source map prepares three native five-minute observations per quarter hour.
metropolis_bins <- function(values,labels,complete_day=FALSE) {
  if(!is.matrix(values)||!nrow(values)||ncol(values)!=length(labels)||!length(labels)||
     anyNA(labels)||any(!grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}$',labels)))
    stop('Native observations need a road-by-time matrix and complete source-minute labels.')
  parsed<-as.POSIXct(strptime(labels,'%Y-%m-%d %H:%M',tz='UTC'))
  if(anyNA(parsed)||any(format(parsed,'%Y-%m-%d %H:%M',tz='UTC')!=labels))stop('Invalid native source clock.')
  clock<-as.numeric(parsed)
  if(any(diff(clock)!=300)||clock[1]%%900||length(clock)%%3L||
     any(!is.na(values)&(!is.finite(values)|values<0)))stop('The original quarter-hour conversion requires complete aligned groups of three five-minute slots.')
  if(complete_day&&(length(clock)!=288L||clock[1]%%86400))stop('The source day must contain 288 consecutive slots beginning at midnight.')
  begins_day<-clock[1]%%86400==0
  ends_day<-tail(clock,1)%%86400==86100
  full_day<-begins_day&&ends_day&&length(clock)==288L
  dimnames(values)<-list(rownames(values),labels)
  starts<-seq(1,ncol(values),3L);nr<-nrow(values);nf<-length(starts)
  observed<-matrix(vapply(starts,function(j)as.integer(rowSums(!is.na(values[,j+0:2,drop=FALSE]))),integer(nr)),nr,nf)
  average<-matrix(vapply(starts,function(j)rowMeans(values[,j+0:2,drop=FALSE],na.rm=TRUE),numeric(nr)),nr,nf)
  average[is.nan(average)]<-NA_real_
  method<-matrix('none',nr,nf)
  for(i in seq_len(nr)) {
    known<-which(observed[i,]>0L);missing<-which(observed[i,]==0L)
    if(!length(missing))next
    if(!length(known)) {
      method[i,missing]<-if(full_day)'all_missing_zero' else 'needs_day_context'
      if(full_day)average[i,missing]<-0
      next
    }
    inside<-missing[missing>min(known)&missing<max(known)]
    if(length(inside)) {
      average[i,inside]<-approx(known,average[i,known],xout=inside)$y
      method[i,inside]<-'linear_interpolation'
    }
    for(side in c('leading','trailing')) {
      outside<-if(side=='leading')missing[missing<min(known)] else missing[missing>max(known)]
      boundary_known<-if(side=='leading')begins_day else ends_day
      method[i,outside]<-if(boundary_known)paste0(side,'_extension') else 'needs_day_context'
      if(boundary_known&&length(outside))average[i,outside]<-average[i,if(side=='leading')min(known) else max(known)]
    }
  }
  dimnames(average)<-dimnames(observed)<-dimnames(method)<-list(rownames(values),labels[starts])
  status<-matrix(as.raw(ifelse(is.na(values),0,ifelse(values==0,2,1))),nr,ncol(values),dimnames=dimnames(values))
  list(values=average,times=clock[starts],labels=labels[starts],native_status=status,
    frame_observed_n=observed,frame_imputed=observed==0L&method!='needs_day_context',frame_fill_method=method)
}

read_metropolis_lesson <- function(directory) {
  read<-function(name)read.csv(file.path(directory,paste0(name,'.csv')),check.names=FALSE)
  static<-read('static-features');volume<-read('volume');speed<-read('speed');calibration<-read('calibration')
  ids<-as.character(static$static_row0)
  if(!identical(volume$timestamp,speed$timestamp)||!identical(as.character(calibration$road_id),ids)||
     !identical(names(volume)[-1],paste0('node_',ids))||!identical(names(speed),names(volume)))
    stop('The original static, volume, speed and calibration fields do not align.')
  list(schema='metropolis-native-v1',ids=ids,static=static,labels=volume$timestamp,
    volume=t(as.matrix(volume[-1])),speed=t(as.matrix(speed[-1])),
    free_flow=calibration$full_history_nonzero_speed_p90,calibration_p95=unique(calibration$selected_day_frame_volume_p95),complete_day=FALSE)
}

metropolis_input <- function(native) {
  if(!identical(native$schema,'metropolis-native-v1'))stop('Expected original five-minute volume/speed and source calibration.')
  ids<-as.character(native$ids);static<-native$static
  if(!length(ids)||anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids)||nrow(static)!=length(ids)||
     nrow(native$volume)!=length(ids)||nrow(native$speed)!=length(ids)||
     length(native$free_flow)!=length(ids)||any(!is.finite(native$free_flow)|native$free_flow<0))
    stop('Road IDs, original attributes, dynamic rows and speed calibration do not align.')
  rownames(native$volume)<-rownames(native$speed)<-ids
  volume<-metropolis_bins(native$volume,native$labels,isTRUE(native$complete_day))
  speed<-metropolis_bins(native$speed,native$labels,isTRUE(native$complete_day))
  p95<-if(is.null(native$calibration_p95))max(1,as.numeric(quantile(volume$values,.95,na.rm=TRUE))) else native$calibration_p95
  if(length(p95)!=1L||!is.finite(p95)||p95<1)stop('The source volume calibration is invalid.')
  slowdown<-1-speed$values/native$free_flow
  slowdown[!is.finite(slowdown)&!is.na(speed$values)]<-0
  congestion<-pmax(pmin(slowdown,1),0)*pmax(pmin(log1p(volume$values)/log1p(p95),1),0)
  roads<-data.frame(id=as.integer(ids),length=round(static$length,1))
  if(all(c('region_id','category','edge_type','speed_limit')%in%names(static))) {
    code<-as.integer(static$speed_limit);code[is.na(code)]<-0L
    limits<-c(NA,5,20,30,40,50,60,70,80,90,100,110)
    if(any(!code%in%0:11))stop('Unknown source speed-limit code.')
    roads$source_region<-as.integer(static$region_id);roads$category<-as.integer(static$category)
    roads$edgeType<-as.integer(static$edge_type);roads$speedLimitCode<-code
    roads$speedLimit<-ifelse(code==0,'unknown',paste(limits[code+1L],'km/h'))
    roads$freeFlowSpeed<-round(native$free_flow,2);roads$maxVolume<-round(native$max_volume,1)
  }
  roads$road_id<-ids
  roads$geometry<-lapply(seq_along(ids),function(i)rbind(
    c(round(static$x_coordinate_start[i],6),round(static$y_coordinate_start[i],6)),
    c(round(static$x_coordinate_end[i],6),round(static$y_coordinate_end[i],6))))
  times<-volume$times;labels<-volume$labels
  table_for<-function(values)data.frame(road_id=rep(ids,length(times)),time=rep(times,each=length(ids)),flow=as.vector(values))
  flow<-table_for(round(volume$values,2))
  measurements<-do.call(rbind,lapply(c('speed','congestion'),function(key) {
    table<-table_for(if(key=='speed')round(speed$values,2) else round(congestion,3))
    names(table)[3]<-'value';table$metric<-key;table[c('road_id','time','metric','value')]
  }))
  definition<-c(flow='Mean unrounded frame volume across the selected source roads.',
    speed='Mean unrounded frame speed over roads with positive frame volume; zero if none are active.',
    congestion='Mean unrounded derived index across the selected source roads.')
  if(!is.null(native$source_summaries)) {
    values<-native$source_summaries
    if(!isTRUE(native$complete_day)||!setequal(names(values),names(definition))||
       any(vapply(values,length,0L)!=length(times)))stop('Source summaries must match the selected full-day frame domain.')
  } else {
    values<-list(flow=round(colMeans(volume$values),2),speed=vapply(seq_along(times),function(j) {
      active<-which(volume$values[,j]>0);if(length(active))round(mean(speed$values[active,j]),2) else 0
    },0),congestion=round(colMeans(congestion),3))
  }
  trends<-do.call(rbind,lapply(names(definition),function(key)data.frame(time=times,metric=key,value=as.numeric(values[[key]]),definition=definition[[key]],label=key)))
  rownames(trends)<-NULL
  coverage<-function(frame)frame[c('native_status','frame_observed_n','frame_imputed','frame_fill_method')]
  frame_summaries<-data.frame(time=times,active=as.integer(colSums(volume$values>0)),
    congestionP95=round(apply(congestion,2,quantile,probs=.95,na.rm=TRUE),3),row.names=NULL)
  sample_summaries<-list(peakFrame=labels[which.max(colSums(congestion,na.rm=TRUE))],
    volumeP95=round(p95,2),congestionP95=round(as.numeric(quantile(congestion,.95,na.rm=TRUE)),3))
  bdynets::traffic_input(roads=roads,flow=flow,measurements=measurements,trends=trends,
    times=times,time_labels=labels,interval_seconds=rep(900,length(times)),unit='source volume',title='City-traffic-M traffic volume',
    provenance=list(source_dataset='mightyneghbor/city-traffic-benchmarks v4',clock='Source wall clock; timezone is unspecified.',
      selection='The supplied 720-road register and 2024-10-18 source date.',source_files=native$source_files,
      source_summary_definition=native$summary_definition,frame_summaries=frame_summaries,sample_summaries=sample_summaries,
      full_history_speed_p90=native$free_flow,volume_p95=p95,
      native_coverage=list(status_codes=c(missing=0L,observed_nonzero=1L,observed_zero=2L),volume=coverage(volume),speed=coverage(speed))),
    metric_info=list(flow=list(unit='source volume',definition='Original mean of available native five-minute volume values in each quarter hour.'),
      speed=list(unit='source speed',label='Speed',definition='Original quarter-hour mean speed and recorded source fill; physical units are not inferred from the Parquets.'),
      congestion=list(unit='dimensionless index',kind='index',label='Congestion index',
        definition='The source map slowdown relative to full-history nonzero speed P90, weighted by clipped log1p volume relative to frame volume P95.',
        scale=list(kind='gradient',stops=c(0,.52,1),palette=c('#00a651','#ffb000','#e02424')))))
}

read_metropolis_raw <- function(base,raw_dir,selection_file=file.path(base,'data','selected-roads.csv'),python='python3',minutes=NULL,calibration_file=NULL,day='2024-10-18') {
  path<-tempfile('metropolis-native-',fileext='.json');on.exit(unlink(path),add=TRUE)
  if(!is.null(minutes)&&(!is.numeric(minutes)||!length(minutes)||anyNA(minutes)))stop('Declare source minutes explicitly.')
  arguments<-c(file.path(base,'read_parquet.py'),raw_dir,selection_file,path,day,
    if(is.null(minutes))'-' else paste(minutes,collapse=','),if(is.null(calibration_file))'-' else calibration_file)
  required<-c(file.path(raw_dir,paste0('city-traffic-M-raw-',c('static-features','volume','speed'),'.parquet')),selection_file,calibration_file)
  if(any(!file.exists(required)))stop('Missing original Parquet input: ',paste(basename(required[!file.exists(required)]),collapse=', '))
  diagnostic<-tempfile('parquet-reader-',fileext='.log');on.exit(unlink(diagnostic),add=TRUE)
  status<-system2(python,shQuote(arguments),stderr=diagnostic)
  if(status!=0L) {
    lines<-readLines(diagnostic,warn=FALSE)
    detail<-grep('^(ValueError|ModuleNotFoundError|ImportError):',lines,value=TRUE)
    stop('The source Parquet reader failed. ',if(length(detail))tail(detail,1) else 'Check original fields and Python, NumPy, pandas and PyArrow.')
  }
  native<-jsonlite::read_json(path,simplifyVector=TRUE)
  native$static<-as.data.frame(native$static);native$complete_day<-is.null(minutes)
  native
}

prepare_example <- function(base='.',output_dir=file.path(base,'prepared'),source_file=file.path(base,'data','source.rds'),raw_dir=NULL,python='python3') {
  native<-NULL;input<-NULL
  sources<-if(is.null(raw_dir))source_file else c(raw_dir,file.path(raw_dir,paste0('city-traffic-M-raw-',c('static-features','volume','speed'),'.parquet')),file.path(base,'data','selected-roads.csv'))
  build<-function() {
    native<<-if(is.null(raw_dir))readRDS(source_file) else read_metropolis_raw(base,raw_dir,python=python)
    input<<-metropolis_input(native);input
  }
  meanings<-c(id='Zero-based static Parquet row ID.',length='Source road length in metres, rounded to one decimal.',
    source_region='Source region_id category.',category='Source category code.',edgeType='Source edge_type code.',
    speedLimitCode='Source speed_limit category.',speedLimit='Source speed-limit category decoded using the source map lookup.',
    freeFlowSpeed='Full-history nonzero speed P90, rounded for display; unrounded calibration is retained in provenance.',maxVolume='Full-history maximum source volume, rounded to one decimal.')
  dictionary<-rbind(data.frame(table='roads',field=names(meanings),meaning=unname(meanings)),
    data.frame(table='trends',field=c('definition','label'),meaning=c('Definition of this derived source summary.','Display label for the summary metric.')))
  checks<-function()data.frame(check=c('Native volume missing','Native speed missing','Speed frames filled'),
    observed=c(sum(is.na(native$volume)),sum(is.na(native$speed)),sum(input$provenance$native_coverage$speed$frame_imputed)),
    expected=NA_real_,passed=TRUE,detail=c('Counted before aggregation; observed zero is retained.','Counted before source interpolation and filling.','Every fill has its native sample count and method.'))
  bdynets::write_traffic_input(build(),output_dir,source_files=sources,native=native,dictionary=dictionary,validation=checks())
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
  if(length(script)!=1L)stop('Rscript entrypoint is required.')
  if(!file.exists(script))script<-gsub('~+~',' ',script,fixed=TRUE)
  base<-dirname(normalizePath(script,mustWork=TRUE))
  source(file.path(base,'..','..','bdynets','inst','preparation-session.R'))
  preparation_session(base)
  args<-commandArgs(TRUE)
  if(!length(args))result<-prepare_example(base) else if(length(args)==1L)result<-prepare_example(base,output_dir=args[1]) else if(length(args)==3L&&args[1]=='--raw')result<-prepare_example(base,output_dir=args[3],raw_dir=args[2]) else stop('Usage: Rscript prepare.R [NEW_DIRECTORY | --raw PARQUET_DIRECTORY NEW_DIRECTORY]')
  cat('Prepared',result,'\n')
}
