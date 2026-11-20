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

ambiguous_divvy_clock <- function(time,timezone) {
  labels <- format(time,'%Y-%m-%d %H:%M:%S',tz=timezone)
  for(delta in c(-7200,-3600,-1800,1800,3600,7200))
    if(any(format(time+delta,'%Y-%m-%d %H:%M:%S',tz=timezone)==labels))return(TRUE)
  FALSE
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
  if(ambiguous_divvy_clock(departure,timezone))stop('A source local clock is ambiguous at a timezone transition.')
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
  input<-list(nodes=nodes,flows=od,unit='trips',timezone=timezone,
    time_kind=if(is.null(bin_seconds))'instant' else 'interval_start',
    provenance=list(source_columns=fields,departure_date=day,timezone=timezone,
      source_departures=source_rows,duplicate_rows=duplicates,missing_origin_only=sum(!origin_ok&destination_ok),
      missing_destination_only=sum(origin_ok&!destination_ok),missing_both=sum(!origin_ok&!destination_ok),
      unmatched_coordinates=sum(origin_ok&destination_ok&!mapped),retained_trips=sum(keep),
      geometry='Per-station median of valid paired source endpoint coordinates on the selected departure day.'))
  if(!is.null(bin_seconds))input$interval_seconds<-bin_seconds
  input
}

# Spatial grouping retains the full day, with grouped endpoints and departure times.
# The prepared day supplies the later map-window conversion.
spatial_od_input <- function(input,radius_m=1500) {
  if(!is.list(input)||!all(c('nodes','flows','unit','timezone','time_kind','provenance')%in%names(input)))
    stop('input must be a prepared OD list with nodes, flows and metadata.')
  if(length(radius_m)!=1L||!is.finite(radius_m)||radius_m<=0)
    stop('radius_m must be one positive number in metres.')
  aggregation<-bdynets::aggregate_od_data(
    nodes=input$nodes,flows=input$flows,radius_m=radius_m,
    node_columns=c(id='node_id',longitude='longitude',latitude='latitude',name='display_name',weight=NA_character_),
    flow_columns=c(origin='origin_id',destination='destination_id',count='flow',time='time'),
    timezone=input$timezone)
  membership<-as.data.frame(aggregation$membership)
  result<-list(nodes=as.data.frame(aggregation$nodes),flows=as.data.frame(aggregation$flows),
    unit=input$unit,timezone=input$timezone,time_kind=input$time_kind,
    provenance=list(station_preparation=input$provenance,display_radius_m=as.numeric(radius_m),
      spatial_aggregation=c(aggregation$metadata,list(membership=membership))))
  if(!is.null(input$interval_seconds))result$interval_seconds<-input$interval_seconds
  result
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
    data.frame(table='flows',field=c('origin_node_idx','destination_node_idx'),
      meaning=c('Generated zero-based departure index joining nodes.node_idx.','Generated zero-based arrival index joining nodes.node_idx.')))
  checks<-function() {
    result<-data.frame(check=c('Retained departure total','Retained endpoint references','Excluded source departures'),
    observed=c(sum(input$flows$flow),sum(input$flows$origin_id%in%input$nodes$node_id & input$flows$destination_id%in%input$nodes$node_id),
      input$provenance$source_departures-input$provenance$duplicate_rows-input$provenance$retained_trips),
    expected=c(input$provenance$retained_trips,nrow(input$flows),with(input$provenance,missing_origin_only+missing_destination_only+missing_both+unmatched_coordinates)),
    passed=TRUE,detail=c('Each retained original departure contributes one trip.','Both endpoints join the supplied node register.','Exclusion reasons partition the nonretained source rows.'))
    result$passed<-result$observed==result$expected;result
  }
  bdynets::write_traffic_input(build(),output_dir,source_files=source_file,dictionary=dictionary,validation=checks())
}

prepare_spatial_example <- function(base='.',output_dir=file.path(base,'prepared-spatial-1500m'),
                                    station_file=file.path(base,'data','input.rds'),radius_m=1500) {
  if(!file.exists(station_file)||dir.exists(station_file))stop('The prepared station OD input is missing.')
  station_input<-readRDS(station_file)
  spatial_input<-spatial_od_input(station_input,radius_m)
  membership<-spatial_input$provenance$spatial_aggregation$membership
  dictionary<-data.frame(table=c(rep('nodes',4),rep('flows',3)),
    field=c('raw_node_count','member_weight','endpoint_activity','display_name','origin_id','destination_id','flow'),
    meaning=c('Number of source stations assigned to this representative.',
      'Sum of unit source-station weights in this group.',
      'Departure plus arrival activity across member stations.',
      'Name of the representative source station.',
      'Prepared representative origin ID.','Prepared representative destination ID.',
      'Trip count for this time and prepared directed pair.'),stringsAsFactors=FALSE)
  checks<-data.frame(
    check=c('Retained trip total','Complete station membership','Representative radius bound'),
    observed=c(sum(spatial_input$flows$flow),nrow(membership),
      spatial_input$provenance$spatial_aggregation$max_assignment_distance_m),
    expected=c(sum(station_input$flows$flow),nrow(station_input$nodes),as.numeric(radius_m)),
    passed=c(sum(spatial_input$flows$flow)==sum(station_input$flows$flow),
      nrow(membership)==nrow(station_input$nodes)&&!anyDuplicated(membership$raw_node_id),
      spatial_input$provenance$spatial_aggregation$max_assignment_distance_m<=radius_m+1e-7),
    detail=c('Spatial preparation conserves every retained trip.',
      'Every station is assigned to exactly one representative.',
      'Every station is within the declared radius of its representative.'),stringsAsFactors=FALSE)
  bdynets::write_traffic_input(spatial_input,output_dir,source_files=station_file,
    dictionary=dictionary,validation=checks)
}

# Divvy windows use local clock bounds on one declared departure day.
divvy_window_times <- function(day, start_clock, end_clock, timezone) {
  if(length(day)!=1L || is.na(day) || !grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$',day) ||
     is.na(as.Date(day,format='%Y-%m-%d')))stop('Supply one departure day in YYYY-MM-DD format.')
  if(length(timezone)!=1L || !timezone%in%OlsonNames())stop('Supply a valid IANA timezone.')
  clock_pattern <- '^([01][0-9]|2[0-3]):[0-5][0-9]$'
  if(length(start_clock)!=1L || length(end_clock)!=1L || anyNA(c(start_clock,end_clock)) ||
     !grepl(clock_pattern,start_clock) || !(grepl(clock_pattern,end_clock) || end_clock=='24:00'))
    stop('Use HH:MM for the window clocks. 24:00 is allowed as the end.')
  from <- paste(day,start_clock)
  to <- if(end_clock=='24:00')paste(as.Date(day)+1L,'00:00') else paste(day,end_clock)
  times <- as.POSIXct(c(from,to),format='%Y-%m-%d %H:%M',tz=timezone)
  if(anyNA(times) || !identical(format(times,'%Y-%m-%d %H:%M',tz=timezone),c(from,to)) ||
     times[1]>=times[2])stop('The display window must have valid local times and end after it starts.')
  if(ambiguous_divvy_clock(times,timezone))stop('An OD window clock is ambiguous at a timezone transition.')
  list(start_time=times[1],end_time=times[2])
}

# Build the final input for the map from departures that have already been grouped.
divvy_window_input <- function(spatial_input, start_time, end_time) {
  timezone <- spatial_input$timezone
  if(!inherits(start_time,'POSIXct') || !inherits(end_time,'POSIXct') ||
     length(start_time)!=1L || length(end_time)!=1L ||
     any(!is.finite(c(as.numeric(start_time),as.numeric(end_time)))) || start_time>=end_time)
    stop('Supply two ordered POSIXct bounds for the OD window.')
  attr(start_time,'tzone') <- attr(end_time,'tzone') <- timezone
  selection <- bdynets::traffic_od_select(spatial_input,start_time=start_time,
    end_time=end_time,timezone=timezone)
  pairs <- bdynets::traffic_od_pairs(selection,include_loops=TRUE)
  pairs$time_window <- NULL
  pairs$start_time <- start_time
  pairs$end_time <- end_time
  pairs$timezone <- timezone
  pairs$unit <- spatial_input$unit
  pairs$display_radius_m <- spatial_input$provenance$display_radius_m
  pairs$provenance <- spatial_input$provenance
  pairs
}

prepare_window_pairs <- function(spatial_file,start_time,end_time,timezone,output_file) {
  if(!file.exists(spatial_file)||dir.exists(spatial_file))stop('The prepared spatial OD input is missing.')
  if(file.exists(output_file)||isTRUE(nzchar(Sys.readlink(output_file),keepNA=TRUE)))
    stop('Choose a new OD window output file.')
  spatial_input <- readRDS(spatial_file)
  if(!identical(timezone,spatial_input$timezone))stop('The OD window must use the prepared data timezone.')
  pairs <- divvy_window_input(spatial_input,start_time,end_time)
  saveRDS(pairs,output_file,compress='xz')
  output_file
}

prepare_all_example <- function(base='.',output_dir=file.path(base,'prepared'),
                                source_file=file.path(base,'data','trips-2026-01-01.csv'),
                                day='2026-01-01',timezone='America/Chicago',radius_m=1500,fields=divvy_fields,
                                start_clock='00:00',end_clock='03:00') {
  window <- divvy_window_times(day,start_clock,end_clock,timezone)
  if(file.exists(output_dir)||isTRUE(nzchar(Sys.readlink(output_dir),keepNA=TRUE)))
    stop('Choose a new Divvy output directory.')
  parent <- dirname(output_dir)
  dir.create(parent,recursive=TRUE,showWarnings=FALSE)
  if(!dir.exists(parent))stop('Cannot create the Divvy output parent directory.')
  lock <- paste0(output_dir,'.preparation-lock')
  if(!dir.create(lock,showWarnings=FALSE))stop('Another preparation is using the Divvy output path.')
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  if(file.exists(output_dir)||isTRUE(nzchar(Sys.readlink(output_dir),keepNA=TRUE)))
    stop('Choose a new Divvy output directory.')
  stage <- tempfile('divvy-preparation-',tmpdir=parent)
  if(!dir.create(stage))stop('Cannot stage the Divvy outputs.')
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  spatial_dir <- paste0('spatial-',format(radius_m,trim=TRUE),'m')
  window_name <- paste0('window-',gsub(':','',start_clock),'-',gsub(':','',end_clock),'.rds')
  station_path <- prepare_example(base,file.path(stage,'station'),source_file,day,timezone,NULL,fields)
  spatial_path <- prepare_spatial_example(base,file.path(stage,spatial_dir),station_path,radius_m)
  prepare_window_pairs(spatial_path,window$start_time,window$end_time,timezone,
    file.path(stage,spatial_dir,window_name))
  if(file.exists(output_dir)||isTRUE(nzchar(Sys.readlink(output_dir),keepNA=TRUE))||
     !file.rename(stage,output_dir))stop('Cannot publish the new Divvy output directory.')
  c(station=file.path(output_dir,'station','input.rds'),
    spatial=file.path(output_dir,spatial_dir,'input.rds'),
    window=file.path(output_dir,spatial_dir,window_name))
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
  if(length(script)!=1L)stop('Rscript entrypoint is required.')
  if(!file.exists(script))script<-gsub('~+~',' ',script,fixed=TRUE)
  base<-dirname(normalizePath(script,mustWork=TRUE))
  source(file.path(base,'..','..','bdynets','inst','preparation-session.R'))
  usage <- paste('Usage: Rscript prepare.R [--day YYYY-MM-DD] [--start HH:MM] [--end HH:MM]',
    '[NEW_DIRECTORY | --raw TRIP_CSV NEW_DIRECTORY]')
  args <- commandArgs(TRUE)
  flags <- c('--day'='day','--start'='start_clock','--end'='end_clock')
  indices <- which(args%in%names(flags))
  if(anyDuplicated(args[indices])||any(indices==length(args))||
     any(startsWith(args[indices+1L],'--')))stop(usage)
  config <- as.list(setNames(args[indices+1L],unname(flags[args[indices]])))
  args <- args[!seq_along(args)%in%c(indices,indices+1L)]
  if(length(args)==1L && !startsWith(args[1],'--'))config$output_dir<-args[1]
  else if(length(args)==3L&&args[1]=='--raw') {
    config$source_file<-args[2]
    config$output_dir<-args[3]
  } else if(length(args))stop(usage)
  preparation_session(base)
  result <- do.call(prepare_all_example,c(list(base=base),config))
  cat('Prepared',paste(names(result),result,sep='=',collapse='\n'),"\n")
}
