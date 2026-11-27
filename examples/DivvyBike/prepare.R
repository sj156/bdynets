# The example preserves individual January 1 departures and the station-resolution OD method.
divvy_fields <- c(ride='ride_id',departure='started_at',origin='start_station_id',destination='end_station_id',
  origin_name='start_station_name',destination_name='end_station_name',
  origin_lat='start_lat',origin_lon='start_lng',destination_lat='end_lat',destination_lon='end_lng')

read_divvy_csv <- function(source_file,fields=divvy_fields) {
  raw<-as.data.frame(data.table::fread(file=source_file,colClasses='character',na.strings='',strip.white=FALSE,showProgress=FALSE))
  if(!all(unname(fields)%in%names(raw)))stop('The original trip columns do not match the declared roles.')
  raw
}

divvy_date <- function(day) {
  if(length(day)!=1L||is.na(day)||!grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$',day))
    stop('A departure date in YYYY-MM-DD format is required.')
  date<-as.Date(as.character(day),format='%Y-%m-%d')
  if(is.na(date))stop('A valid departure date is required.')
  date
}

divvy_departure_clocks <- function(records,fields) {
  clocks<-as.character(records[[fields['departure']]])
  pattern<-'^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]{1,6})?$'
  if(anyNA(clocks)||any(!grepl(pattern,clocks)))
    stop('Departure timestamps need complete local dates and clocks, with optional fractional seconds.')
  clocks
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

ambiguous_divvy_clock <- function(time,timezone) {
  labels <- format(time,'%Y-%m-%d %H:%M:%S',tz=timezone)
  for(delta in c(-7200,-3600,-1800,1800,3600,7200))
    if(any(format(time+delta,'%Y-%m-%d %H:%M:%S',tz=timezone)==labels))return(TRUE)
  FALSE
}

prepare_divvy_day <- function(records,day='2026-01-01',timezone='America/Chicago',bin_seconds=NULL,fields=divvy_fields) {
  divvy_date(day)
  if(length(timezone)!=1L||!timezone%in%OlsonNames())stop('An explicit IANA source timezone is required.')
  if(!setequal(names(fields),names(divvy_fields))||anyNA(fields)||anyDuplicated(fields)||
     any(!fields%in%names(records)))stop('Map each required trip role to one distinct source field.')
  records<-as.data.frame(records)
  clocks<-divvy_departure_clocks(records,fields)
  if(any(substr(clocks,1,10)!=day))stop('Trip records must all depart on the example day.')
  source_rows<-nrow(records)
  if(!source_rows)stop('The source contains no departure on the example day.')
  name_fields<-unname(fields[c('origin_name','destination_name')])
  records[name_fields]<-lapply(records[name_fields],as.character)
  cleaned<-clean_divvy_station_names(records,fields)
  records<-unique(cleaned)
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
  endpoint<-function(side)data.frame(node_id=records[[side]],name=records[[paste0(side,'_name')]],
    latitude=coordinate(records[[paste0(side,'_lat')]]),longitude=coordinate(records[[paste0(side,'_lon')]]))
  endpoints<-rbind(endpoint('origin'),endpoint('destination'))
  endpoints<-endpoints[!is.na(endpoints$node_id)&nzchar(endpoints$node_id),,drop=FALSE]
  latitude_limit <- getFromNamespace(".tm_mercator_limit", "bdynets")
  located<-endpoints[is.finite(endpoints$latitude)&abs(endpoints$latitude)<=latitude_limit&
    is.finite(endpoints$longitude)&abs(endpoints$longitude)<=180,,drop=FALSE]
  ids<-sort(unique(located$node_id))
  station_name<-function(id) {
    text<-endpoints$name[endpoints$node_id==id]
    text<-text[!is.na(text)&nzchar(trimws(text))]
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
  retained<-sum(keep)
  exclusions<-list(missing_origin_only=sum(!origin_ok&destination_ok),
    missing_destination_only=sum(origin_ok&!destination_ok),missing_both=sum(!origin_ok&!destination_ok),
    unmatched_coordinates=sum(origin_ok&destination_ok&!mapped))
  if(sum(od$flow)!=retained||source_rows!=duplicates+retained+sum(unlist(exclusions)))
    stop('Source departures and retained OD totals do not reconcile.')
  input<-list(nodes=nodes,flows=od,unit='trips',timezone=timezone,
    time_kind=if(is.null(bin_seconds))'instant' else 'interval_start',
    provenance=c(list(source_columns=fields,departure_date=day,timezone=timezone,
      source_departures=source_rows,duplicate_rows=duplicates),exclusions,
      list(retained_trips=retained,
        geometry='Per-station median of valid paired source endpoint coordinates on the selected departure day.')))
  if(!is.null(bin_seconds))input$interval_seconds<-bin_seconds
  list(cleaned=cleaned,input=input)
}

ride_input <- function(records,day='2026-01-01',timezone='America/Chicago',bin_seconds=NULL,fields=divvy_fields) {
  prepare_divvy_day(records,day,timezone,bin_seconds,fields)$input
}

# Spatial grouping retains the full day, with grouped endpoints and departure times.
# The prepared day supplies the later map-window conversion.
spatial_od_input <- function(input,radius_m=1500) {
  if(!is.list(input)||!all(c('nodes','flows','unit','timezone','time_kind','provenance')%in%names(input)))
    stop('input must be a prepared OD list with nodes, flows and metadata.')
  if(length(radius_m)!=1L||!is.finite(radius_m)||radius_m<=0)
    stop('radius_m must be one positive number in metres.')
  aggregation<-aggregate_od_data(
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

prepare_example <- function(base='.',output_dir=file.path(base,'prepared'),source_file=file.path(base,'data','raw','202601-divvy-tripdata.csv'),
                            day='2026-01-01',timezone='America/Chicago',bin_seconds=NULL,fields=divvy_fields) {
  raw<-read_divvy_csv(source_file,fields)
  write_divvy_station_input(ride_input(raw,day,timezone,bin_seconds,fields),output_dir,source_file)
}

write_divvy_station_input <- function(input,output_dir,source_file) {
  meanings<-c(display_name='Most frequent nonblank station name; alphabetical tie break.',node_idx='Zero-based row in this station register.',
    representative_station_id='The original station ID; one source station per node.',n_raw_stations='One source station per node.',
    member_weight='One station per node, for the existing OD interface.',coordinate_available='Valid paired station coordinates are available.')
  dictionary<-rbind(data.frame(table='nodes',field=names(meanings),meaning=unname(meanings)),
    data.frame(table='flows',field=c('origin_node_idx','destination_node_idx'),
      meaning=c('Generated zero-based departure index joining nodes.node_idx.','Generated zero-based arrival index joining nodes.node_idx.')))
  write_traffic_input(input,output_dir,source_files=source_file,dictionary=dictionary)
}

prepare_spatial_example <- function(base='.',output_dir=file.path(base,'prepared-spatial-1500m'),
                                    station_file,radius_m=1500) {
  if(!file.exists(station_file)||dir.exists(station_file))stop('The prepared station OD input is missing.')
  station_input<-readRDS(station_file)
  spatial_input<-spatial_od_input(station_input,radius_m)
  dictionary<-data.frame(table=c(rep('nodes',4),rep('flows',3)),
    field=c('raw_node_count','member_weight','endpoint_activity','display_name','origin_id','destination_id','flow'),
    meaning=c('Number of source stations assigned to this representative.',
      'Sum of unit source-station weights in this group.',
      'Departure plus arrival activity across member stations.',
      'Name of the representative source station.',
      'Prepared representative origin ID.','Prepared representative destination ID.',
      'Trip count for this time and prepared directed pair.'),stringsAsFactors=FALSE)
  write_traffic_input(spatial_input,output_dir,source_files=station_file,dictionary=dictionary)
}

# Divvy windows use local clock bounds on one declared departure day.
divvy_window_times <- function(day, start_clock, end_clock, timezone) {
  date <- divvy_date(day)
  if(length(timezone)!=1L || !timezone%in%OlsonNames())stop('Supply a valid IANA timezone.')
  clock_pattern <- '^([01][0-9]|2[0-3]):[0-5][0-9]$'
  if(length(start_clock)!=1L || length(end_clock)!=1L || anyNA(c(start_clock,end_clock)) ||
     !grepl(clock_pattern,start_clock) || !(grepl(clock_pattern,end_clock) || end_clock=='24:00'))
    stop('Use HH:MM for the window clocks. 24:00 is allowed as the end.')
  from <- paste(day,start_clock)
  to <- if(end_clock=='24:00')paste(date+1L,'00:00') else paste(day,end_clock)
  times <- as.POSIXct(c(from,to),format='%Y-%m-%d %H:%M',tz=timezone)
  if(anyNA(times) || !identical(format(times,'%Y-%m-%d %H:%M',tz=timezone),c(from,to)) ||
     times[1]>=times[2])stop('The display window must have valid local times and end after it starts.')
  if(ambiguous_divvy_clock(times,timezone))stop('An OD window clock is ambiguous at a timezone transition.')
  list(start_time=times[1],end_time=times[2])
}

# Build the final input for the map from departures that have already been grouped.
divvy_window_input <- function(spatial_input, start_time, end_time, allow_empty = FALSE) {
  input <- .od_selection_input(spatial_input, timezone = spatial_input$timezone)
  .divvy_window_pairs(input, start_time, end_time, allow_empty)
}

.divvy_window_pairs <- function(input, start_time, end_time, allow_empty, time = NULL) {
  if(!inherits(start_time,'POSIXct') || !inherits(end_time,'POSIXct') ||
     length(start_time)!=1L || length(end_time)!=1L ||
     any(!is.finite(c(as.numeric(start_time),as.numeric(end_time)))) || start_time>=end_time)
    stop('Supply two ordered POSIXct bounds for the OD window.')
  selection <- .od_select_window(input, start_time, end_time, NULL, allow_empty, time)
  pairs <- traffic_od_pairs(selection, include_loops = TRUE, allow_empty = allow_empty)
  pairs$display_radius_m <- input$provenance$display_radius_m
  pairs
}

# Partition the local departure day into half-open windows.
divvy_report_windows <- function(day, timezone, window_minutes = 180L) {
  if (!is.numeric(window_minutes) || length(window_minutes) != 1L ||
      !is.finite(window_minutes) || window_minutes < 1L ||
      window_minutes != floor(window_minutes)) {
    stop("Supply a positive whole number of minutes for each window.")
  }
  day_bounds <- divvy_window_times(day, "00:00", "24:00", timezone)
  edges <- seq(day_bounds$start_time, day_bounds$end_time, by = window_minutes * 60)
  if (tail(edges, 1L) < day_bounds$end_time) edges <- c(edges, day_bounds$end_time)
  data.frame(start_time = head(edges, -1L), end_time = tail(edges, -1L))
}

# Keep one draw-ready object per requested interval, including intervals with no departures.
divvy_frame_inputs <- function(spatial_input, windows) {
  if (!is.data.frame(windows) || !nrow(windows) ||
      !all(c("start_time", "end_time") %in% names(windows))) {
    stop("Supply a nonempty table of start_time and end_time values.")
  }
  input <- .od_selection_input(spatial_input, timezone = spatial_input$timezone)
  time <- .od_selection_times(input)
  lapply(seq_len(nrow(windows)), function(i) {
    .divvy_window_pairs(
      input, windows$start_time[i], windows$end_time[i], allow_empty = TRUE, time = time
    )
  })
}

prepare_all_example <- function(base='.',output_dir=file.path(base,'prepared'),
                                source_file=file.path(base,'data','raw','202601-divvy-tripdata.csv'),
                                day='2026-01-01',timezone='America/Chicago',radius_m=1500,fields=divvy_fields,
                                start_clock=NULL,end_clock=NULL) {
  if (xor(is.null(start_clock), is.null(end_clock))) stop('Supply both --start and --end.')
  window <- if (!is.null(start_clock)) divvy_window_times(day,start_clock,end_clock,timezone)
  target <- .prep_destination(output_dir, source_file)
  parent <- dirname(target)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(parent)) stop('Cannot create the Divvy output parent directory.')
  lock <- paste0(target,'.preparation-lock')
  if (!dir.create(lock, showWarnings = FALSE)) stop('Another preparation is using the Divvy output path.')
  on.exit(unlink(lock, recursive = TRUE, expand = FALSE), add = TRUE)
  .prep_destination(target, source_file)
  stage <- tempfile('divvy-preparation-',tmpdir=parent)
  if(!dir.create(stage))stop('Cannot stage the Divvy outputs.')
  on.exit(unlink(stage,recursive=TRUE,expand=FALSE),add=TRUE)
  spatial_dir <- paste0('spatial-',format(radius_m,trim=TRUE),'m')
  station_path <- prepare_example(base,file.path(stage,'station'),source_file,day,timezone,NULL,fields)
  spatial_path <- prepare_spatial_example(base,file.path(stage,spatial_dir),station_path,radius_m)
  windows <- if (is.null(window)) divvy_report_windows(day, timezone) else as.data.frame(window)
  frames <- divvy_frame_inputs(readRDS(spatial_path), windows)
  frame_file <- file.path(spatial_dir, 'od-frames.rds')
  saveRDS(frames, file.path(stage, frame_file), compress='xz')
  .prep_destination(target, source_file)
  if (!file.rename(stage, target)) stop('Cannot publish the new Divvy output directory.')
  c(station = file.path(target, 'station', 'input.rds'),
    spatial = file.path(target, spatial_dir, 'input.rds'),
    frames = file.path(target, frame_file))
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
  if(length(script)!=1L)stop('Rscript entrypoint is required.')
  if(!file.exists(script))script<-gsub('~+~',' ',script,fixed=TRUE)
  base<-dirname(normalizePath(script,mustWork=TRUE))
  usage <- paste('Usage: Rscript prepare.R [--day YYYY-MM-DD] [--start HH:MM --end HH:MM]',
    '[NEW_DIRECTORY | --raw ONE_DAY_TRIP_CSV NEW_DIRECTORY]')
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
  source(file.path(base, 'scripts', 'prepare-input.R'))
  result <- do.call(prepare_all_example,c(list(base=base),config))
  cat('Prepared',paste(names(result),result,sep='=',collapse='\n'),"\n")
}
