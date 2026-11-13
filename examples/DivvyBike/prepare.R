# The example preserves individual January 1 departures and the station-resolution OD method.
divvy_fields <- c(ride='ride_id',departure='started_at',origin='start_station_id',destination='end_station_id',
  origin_name='start_station_name',destination_name='end_station_name',
  origin_lat='start_lat',origin_lon='start_lng',destination_lat='end_lat',destination_lon='end_lng')

read_divvy_source <- function(source_file,day='2026-01-01',fields=divvy_fields) {
  if(length(day)!=1L||is.na(day)||!grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$',day)||is.na(as.Date(day)))stop('Declare one source departure date.')
  raw<-as.data.frame(data.table::fread(source_file,colClasses='character',na.strings='',strip.white=FALSE,showProgress=FALSE))
  if(!all(unname(fields)%in%names(raw)))stop('The original trip columns do not match the declared roles.')
  clock<-raw[[fields['departure']]]
  if(anyNA(clock)||any(!grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]{1,6})?$',clock)))stop('Original departures need complete source dates and clocks.')
  day_rows<-raw[substr(clock,1,10)==day,,drop=FALSE];rownames(day_rows)<-NULL
  if(!nrow(day_rows))stop('The original file contains no departures on the selected date.')
  day_rows
}

# Source-name cleaning affects only trailing ASCII spaces in the two mapped name fields.
clean_divvy_station_names <- function(records,fields=divvy_fields) {
  name_fields<-unname(fields[c('origin_name','destination_name')])
  if(!is.data.frame(records)||length(name_fields)!=2L||anyNA(name_fields)||anyDuplicated(name_fields)||
     any(!name_fields%in%names(records)))stop('Map the two station-name columns before cleaning.')
  for(field in name_fields) {
    if(!is.character(records[[field]]))stop('Read original station names as character fields.')
    records[[field]]<-sub(' +$','',records[[field]])
  }
  records
}

extract_divvy_day <- function(source_file,output_file,day='2026-01-01',fields=divvy_fields) {
  if(!is.character(output_file)||length(output_file)!=1L||is.na(output_file)||!nzchar(trimws(output_file)))stop('Declare one new daily CSV path.')
  if(!file.exists(source_file)||dir.exists(source_file))stop('The original trip CSV is missing.')
  occupied<-function(path)file.exists(path)||dir.exists(path)||(!is.na(Sys.readlink(path))&&nzchar(Sys.readlink(path)))
  if(occupied(output_file))stop('The daily CSV destination already exists or is a source alias.')
  parent<-normalizePath(dirname(output_file),mustWork=TRUE)
  target<-file.path(parent,basename(output_file))
  if(identical(target,normalizePath(source_file,mustWork=TRUE)))stop('The daily CSV cannot replace its source.')
  lock<-paste0(target,'.extraction-lock')
  if(!dir.create(lock,showWarnings=FALSE))stop('Another extraction is using this daily CSV path.')
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  if(occupied(target))stop('The daily CSV destination already exists.')
  identity<-unname(tools::md5sum(source_file))
  records<-clean_divvy_station_names(read_divvy_source(source_file,day,fields),fields)
  stage<-tempfile('day-extract-',tmpdir=parent,fileext='.csv');on.exit(unlink(stage),add=TRUE)
  write.csv(records,stage,row.names=FALSE,na='',fileEncoding='UTF-8')
  restored<-as.data.frame(data.table::fread(stage,colClasses='character',na.strings='',strip.white=FALSE,showProgress=FALSE))
  if(!identical(records,restored))stop('The daily original-column CSV readback differs.')
  if(!identical(identity,unname(tools::md5sum(source_file))))stop('The original trip file changed during extraction.')
  if(occupied(target))stop('The daily CSV destination already exists.')
  if(!file.rename(stage,target))stop('Cannot publish the daily original-column CSV.')
  invisible(target)
}

ride_input <- function(records,day='2026-01-01',timezone='America/Chicago',bin_seconds=NULL,fields=divvy_fields) {
  if(length(day)!=1L||is.na(day)||!grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$',day)||is.na(as.Date(day)))stop('A departure date in YYYY-MM-DD format is required.')
  if(length(timezone)!=1L||!timezone%in%OlsonNames())stop('An explicit IANA source timezone is required.')
  if(!setequal(names(fields),names(divvy_fields))||anyNA(fields)||anyDuplicated(fields)||
     any(!fields%in%names(records)))stop('Map each required trip role to one distinct source field.')
  records<-as.data.frame(records)
  clocks<-as.character(records[[fields['departure']]])
  if(anyNA(clocks)||any(!grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]{1,6})?$',clocks)))
    stop('Departure timestamps need complete local dates and clocks, with optional fractional seconds.')
  records<-records[substr(clocks,1,10)==day,,drop=FALSE]
  source_rows<-nrow(records)
  if(!source_rows)stop('The source contains no departure on the selected date.')
  records<-unique(records)
  duplicates<-source_rows-nrow(records)
  records<-records[unname(fields[names(divvy_fields)])];names(records)<-names(divvy_fields)
  records[]<-lapply(records,as.character)
  if(anyNA(records$ride)||any(!nzchar(trimws(records$ride)))||anyDuplicated(records$ride))
    stop('Ride IDs must be nonblank and unique after exact duplicate removal.')
  departure<-as.POSIXct(strptime(records$departure,'%Y-%m-%d %H:%M:%OS',tz=timezone))
  if(anyNA(departure)||any(format(departure,'%Y-%m-%d %H:%M:%S',tz=timezone)!=substr(records$departure,1,19)))
    stop('A departure timestamp is invalid in the declared source timezone.')
  labels<-format(departure,'%Y-%m-%d %H:%M:%S',tz=timezone)
  for(delta in c(-7200,-3600,-1800,1800,3600,7200))
    if(any(format(departure+delta,'%Y-%m-%d %H:%M:%S',tz=timezone)==labels))stop('A source local clock is ambiguous at a timezone transition.')
  records$origin<-trimws(records$origin);records$destination<-trimws(records$destination)
  coordinate<-function(text) {
    value<-suppressWarnings(as.numeric(text))
    if(any(!is.na(text)&nzchar(text)&is.na(value)))stop('An endpoint coordinate contains nonnumeric text.')
    value
  }
  endpoint<-function(side)data.frame(node_id=records[[side]],name=trimws(records[[paste0(side,'_name')]]),
    latitude=coordinate(records[[paste0(side,'_lat')]]),longitude=coordinate(records[[paste0(side,'_lon')]]))
  endpoints<-rbind(endpoint('origin'),endpoint('destination'))
  endpoints<-endpoints[!is.na(endpoints$node_id)&nzchar(endpoints$node_id),,drop=FALSE]
  located<-endpoints[is.finite(endpoints$latitude)&abs(endpoints$latitude)<=85.0511&
    is.finite(endpoints$longitude)&abs(endpoints$longitude)<=180,,drop=FALSE]
  ids<-sort(unique(located$node_id))
  station_name<-function(id) {
    text<-endpoints$name[endpoints$node_id==id]
    text<-text[!is.na(text)&nzchar(text)]
    if(!length(text))return(NA_character_)
    frequency<-table(text);names(frequency)[order(-frequency,names(frequency))][1]
  }
  nodes<-data.frame(node_id=ids,
    latitude=vapply(ids,function(id)median(located$latitude[located$node_id==id]),0),
    longitude=vapply(ids,function(id)median(located$longitude[located$node_id==id]),0),
    display_name=vapply(ids,station_name,''),row.names=NULL)
  origin_ok<-!is.na(records$origin)&nzchar(records$origin)
  destination_ok<-!is.na(records$destination)&nzchar(records$destination)
  mapped<-records$origin%in%nodes$node_id&records$destination%in%nodes$node_id
  keep<-origin_ok&destination_ok&mapped
  if(!any(keep))stop('No departure has two identified and located stations.')
  active<-unique(c(records$origin[keep],records$destination[keep]))
  nodes<-nodes[nodes$node_id%in%active,,drop=FALSE];rownames(nodes)<-NULL
  nodes$node_idx<-seq_len(nrow(nodes))-1L
  nodes$representative_station_id<-nodes$node_id
  nodes$n_raw_stations<-1L;nodes$member_weight<-1L;nodes$coordinate_available<-TRUE
  od<-data.frame(ride_id=records$ride[keep],origin_id=records$origin[keep],destination_id=records$destination[keep],time=departure[keep],flow=1L)
  # Optional temporal totals retain the original helper's whole-clock-hour divisors.
  # The supplied example leaves this argument NULL and retains each departure.
  if(!is.null(bin_seconds)) {
    if(length(bin_seconds)!=1L||!is.finite(bin_seconds)||bin_seconds<1||bin_seconds!=trunc(bin_seconds)||3600%%bin_seconds)
      stop('The source departure interval must divide one clock hour in whole seconds.')
    clock<-as.POSIXlt(od$time,tz=timezone);seconds<-clock$min*60+clock$sec
    hour_start<-round(as.numeric(od$time)-seconds)
    od$time<-as.POSIXct(hour_start+floor(seconds/bin_seconds)*bin_seconds,origin='1970-01-01',tz=timezone)
    od<-aggregate(list(flow=od$flow),od[c('time','origin_id','destination_id')],sum)
  }
  od$origin_node_idx<-nodes$node_idx[match(od$origin_id,nodes$node_id)]
  od$destination_node_idx<-nodes$node_idx[match(od$destination_id,nodes$node_id)]
  od<-od[order(od$time,od$origin_id,od$destination_id),,drop=FALSE];rownames(od)<-NULL
  if(sum(od$flow)!=sum(keep)||source_rows!=duplicates+sum(!keep)+sum(keep))stop('Source departures and retained OD totals do not reconcile.')
  bdynets::traffic_input(nodes=nodes,od=od,timezone=timezone,
    od_time_kind=if(is.null(bin_seconds))'instant' else 'interval_start',od_interval_seconds=bin_seconds,
    title='Divvy station departures',provenance=list(source_columns=fields,departure_date=day,timezone=timezone,
      source_departures=source_rows,duplicate_rows=duplicates,missing_origin_only=sum(!origin_ok&destination_ok),
      missing_destination_only=sum(origin_ok&!destination_ok),missing_both=sum(!origin_ok&!destination_ok),
      unmatched_coordinates=sum(origin_ok&destination_ok&!mapped),retained_trips=sum(keep),
      geometry='Per-station median of valid paired source endpoint coordinates on the selected departure day.'))
}

prepare_example <- function(base='.',output_dir=file.path(base,'prepared'),source_file=file.path(base,'data','trips-2026-01-01.csv'),
                            day='2026-01-01',timezone='America/Chicago',bin_seconds=NULL,fields=divvy_fields) {
  input<-NULL
  build<-function() {
    raw<-read_divvy_source(source_file,day,fields)
    input<<-ride_input(raw,day,timezone,bin_seconds,fields);input
  }
  meanings<-c(display_name='Most frequent nonblank station name; alphabetical tie break.',node_idx='Zero-based row in this station register.',
    representative_station_id='The original station ID; one source station per node.',n_raw_stations='One source station per node.',
    member_weight='One station per node, for the existing OD interface.',coordinate_available='Valid paired station coordinates are available.')
  dictionary<-rbind(data.frame(table='nodes',field=names(meanings),meaning=unname(meanings)),
    data.frame(table='od',field=c('origin_node_idx','destination_node_idx'),
      meaning=c('Generated zero-based departure index joining nodes.node_idx.','Generated zero-based arrival index joining nodes.node_idx.')))
  checks<-function() {
    result<-data.frame(check=c('Retained departure total','Retained endpoint references','Excluded source departures'),
    observed=c(sum(input$od$flow),sum(input$od$origin_id%in%input$nodes$node_id & input$od$destination_id%in%input$nodes$node_id),
      input$provenance$source_departures-input$provenance$duplicate_rows-input$provenance$retained_trips),
    expected=c(input$provenance$retained_trips,nrow(input$od),with(input$provenance,missing_origin_only+missing_destination_only+missing_both+unmatched_coordinates)),
    passed=TRUE,detail=c('Each retained original departure contributes one trip.','Both endpoints join the supplied node register.','Exclusion reasons partition the nonretained source rows.'))
    result$passed<-result$observed==result$expected;result
  }
  bdynets::write_traffic_input(build(),output_dir,source_files=source_file,dictionary=dictionary,validation=checks())
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
  if(length(script)!=1L)stop('Rscript entrypoint is required.')
  if(!file.exists(script))script<-gsub('~+~',' ',script,fixed=TRUE)
  base<-dirname(normalizePath(script,mustWork=TRUE))
  source(file.path(base,'..','..','bdynets','inst','preparation-session.R'))
  preparation_session(base)
  args<-commandArgs(TRUE)
  if(!length(args))result<-prepare_example(base) else if(length(args)==1L)result<-prepare_example(base,output_dir=args[1]) else if(length(args)==3L&&args[1]=='--raw')result<-prepare_example(base,output_dir=args[3],source_file=args[2]) else stop('Usage: Rscript prepare.R [NEW_DIRECTORY | --raw TRIP_CSV NEW_DIRECTORY]')
  cat('Prepared',result,'\n')
}
