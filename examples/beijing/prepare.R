# MOE observations are hourly all-lane entry rates sampled at each minute.
beijing_fields <- c(pair='link_id_from_to',day='day_no',minute='timestamp_in_min',
                    rate='link_volume_in_veh_per_hour_for_all_lanes')

beijing_network <- function(network) {
  if(!identical(network$type,'FeatureCollection')||any(network$features$geometry$type!='LineString'))
    stop('The geometry source must contain directed LineString features.')
  roads<-network$features$properties
  roads$road_id<-as.character(roads$edge_index)
  roads$geometry<-network$features$geometry$coordinates
  if(!nrow(roads)||anyNA(roads$road_id)||anyNA(roads$edge_id)||anyDuplicated(roads$road_id)||
     anyDuplicated(roads$edge_id)||!identical(roads$edge_id,paste(roads$from_node_id,roads$to_node_id,sep='->')))
    stop('Road IDs and directed endpoint pairs must be unique and agree.')
  roads
}

beijing_records <- function(records,roads,expected_minutes,fields=beijing_fields,day=10L) {
  if(!setequal(names(fields),names(beijing_fields))||anyNA(fields)||anyDuplicated(fields)||
     any(!fields%in%names(records)))stop('Map each required MOE role to one distinct source field.')
  records<-as.data.frame(records)[unname(fields[names(beijing_fields)])]
  names(records)<-names(beijing_fields)
  records<-records[!is.na(records$day)&records$day==day,,drop=FALSE]
  if(!nrow(records)||anyNA(records)||!is.numeric(records$minute)||!is.numeric(records$rate)||
     any(!is.finite(records$minute))||any(records$minute!=trunc(records$minute))||
     any(!is.finite(records$rate)|records$rate<0)||any(!nzchar(as.character(records$pair))))
    stop('MOE rows need a directed pair, a whole source minute and a nonnegative finite hourly rate.')
  if(!length(expected_minutes)||anyNA(expected_minutes)||any(diff(expected_minutes)!=1))
    stop('Declare the consecutive source-minute domain.')
  if(anyDuplicated(records[c('pair','minute')]))stop('Duplicate directed-pair and minute key.')
  pairs<-unique(as.character(records$pair))
  column<-match(records$minute,expected_minutes)
  if(anyNA(column)||nrow(records)!=length(pairs)*length(expected_minutes))
    stop('Missing or unexpected directed-pair and minute record.')
  values<-matrix(NA_real_,length(pairs),length(expected_minutes))
  values[cbind(match(records$pair,pairs),column)]<-records$rate
  if(anyNA(values)||any(!roads$edge_id%in%pairs))stop('Source minutes or directed geometry matches are incomplete.')
  list(schema='beijing-native-minute-v1',road_pairs=pairs,minute=expected_minutes,
       minute_rates=values,roads=roads,day_no=day,source_field=unname(fields['rate']),source_columns=fields)
}

beijing_input <- function(native,interval_minutes=5L) {
  if(!identical(native$schema,'beijing-native-minute-v1'))stop('Expected native MOE minute observations.')
  minute<-native$minute;rates<-native$minute_rates;roads<-native$roads
  if(!length(minute)||anyNA(minute)||any(diff(minute)!=1)||ncol(rates)!=length(minute)||
     nrow(rates)!=length(native$road_pairs)||anyDuplicated(native$road_pairs)||
     any(!is.finite(rates)|rates<0))stop('Native MOE IDs, values and minute grid do not agree.')
  if(length(interval_minutes)!=1L||!is.finite(interval_minutes)||interval_minutes<1||
     interval_minutes!=trunc(interval_minutes)||length(minute)%%interval_minutes||minute[1]%%interval_minutes)
    stop('The interval must contain complete aligned source minutes.')
  road_row<-match(roads$edge_id,native$road_pairs)
  if(anyNA(road_row))stop('A directed geometry has no source MOE pair.')
  first<-seq.int(1L,length(minute),by=interval_minutes)
  values<-matrix(0,nrow(roads),length(first))
  for(offset in seq_len(interval_minutes)-1L)values<-values+rates[road_row,first+offset,drop=FALSE]
  values<-values/interval_minutes
  starts<-minute[first]*60
  roads$road_id<-as.character(roads$edge_index)
  flow<-data.frame(road_id=rep(roads$road_id,length(starts)),time=rep(starts,each=nrow(roads)),flow=as.vector(values))
  bdynets::traffic_input(roads=roads,flow=flow,times=starts,
    time_labels=sprintf('%02d:%02d',starts%/%3600,(starts%/%60)%%60),
    interval_seconds=rep(interval_minutes*60,length(starts)),unit='vehicles/hour',title='Beijing simulated entry flow',
    provenance=list(source_fields=if(is.null(native$source_columns))beijing_fields else native$source_columns,day_no=native$day_no,source=native$source,
      source_pairs=length(native$road_pairs),display_roads=nrow(roads),
      excluded_pairs=length(native$road_pairs)-nrow(roads),native_minutes=length(minute),
      native_records=length(rates),retained_minute_records=nrow(roads)*length(minute),
      operation='Arithmetic mean of the minute-sampled hourly entry rates; original example interval is five minutes.'))
}

read_beijing_raw <- function(moe_file,geometry_file,fields=beijing_fields,day=10L,expected_minutes=0:1439) {
  lines<-readLines(geometry_file,warn=FALSE,encoding='UTF-8')
  hit<-grep('^[[:space:]]*const network[[:space:]]*=',lines,value=TRUE)
  if(length(hit)!=1L)stop('Geometry file must contain one network FeatureCollection.')
  literal<-sub(';[[:space:]]*$','',sub('^[[:space:]]*const network[[:space:]]*=[[:space:]]*','',hit))
  roads<-beijing_network(jsonlite::fromJSON(literal))
  header<-names(read.csv(text=readLines(moe_file,n=1L),check.names=FALSE))
  at<-match(unname(fields),header)
  if(anyNA(at))stop('Required source columns are missing.')
  raw<-data.table::fread(moe_file,skip=1L,header=FALSE,select=at,col.names=unname(fields),showProgress=FALSE)
  raw<-raw[raw[[fields['day']]]==day & raw[[fields['minute']]]%in%expected_minutes,]
  native<-beijing_records(raw,roads,expected_minutes,fields,day)
  native$source<-c(basename(moe_file),basename(geometry_file))
  native
}
prepare_example <- function(base='.',output_dir=file.path(base,'prepared'),interval_minutes=5L,
                            source_file=file.path(base,'data','source.rds'),moe_file=NULL,geometry_file=NULL) {
  if(xor(is.null(moe_file),is.null(geometry_file)))stop('Supply both the MOE CSV and geometry HTML.')
  sources<-if(is.null(moe_file))source_file else c(moe_file,geometry_file)
  native<-NULL
  build<-function() {
    native<<-if(is.null(moe_file))readRDS(source_file) else read_beijing_raw(moe_file,geometry_file)
    native$source<-basename(sources)
    beijing_input(native,interval_minutes)
  }
  meanings<-c(edge_index='Source GeoJSON display-road index.',edge_id='Directed source pair, from_node_id -> to_node_id.',
    from_node_id='Source upstream node ID.',to_node_id='Source downstream node ID.',length_km='Source link length in kilometres.',
    lanes='Source all-lane count.',speed_limit='Source speed-limit attribute, unchanged.',link_type_name='Source link-type description.')
  dictionary<-data.frame(table='roads',field=names(meanings),meaning=unname(meanings))
  checks<-function() {
    result<-data.frame(check=c('Native minute grid','Retained road-minute matches'),
    observed=c(length(native$minute_rates),nrow(native$roads)*length(native$minute)),
    expected=c(length(native$road_pairs)*length(native$minute),nrow(native$roads)*length(native$minute)),
    passed=TRUE,detail=c('One observed rate per declared pair-minute; duplicate and gap checks precede conversion.',
                        'Every display road joins to a source directed pair.'))
    result$passed<-result$observed==result$expected;result
  }
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
  if(!length(args))result<-prepare_example(base) else if(length(args)==1L)result<-prepare_example(base,output_dir=args[1]) else if(length(args)==4L&&args[1]=='--raw')result<-prepare_example(base,output_dir=args[4],moe_file=args[2],geometry_file=args[3]) else stop('Usage: Rscript prepare.R [NEW_DIRECTORY | --raw MOE_CSV GEOMETRY_HTML NEW_DIRECTORY]')
  cat('Prepared',result,'\n')
}
