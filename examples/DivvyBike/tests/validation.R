args<-commandArgs(TRUE)
if(length(args)>2L)stop('Usage: Rscript tests/validation.R [CASE_DIRECTORY [ORIGINAL_MONTH_CSV]]')
base<-normalizePath(if(length(args))args[1] else '.')
original_month<-if(length(args)==2L)normalizePath(args[2],mustWork=TRUE) else NULL
source(file.path(base,'..','..','bdynets','inst','preparation-session.R'))
preparation_session(base)
library(bdynets)
source(file.path(base,'prepare.R'))
reject<-function(expr)stopifnot(inherits(try(force(expr),silent=TRUE),'try-error'))
raw<-as.data.frame(data.table::fread(file.path(base,'data/trips-2026-01-01.csv'),colClasses='character',na.strings=''))
full<-ride_input(raw)
# The prepared OD input has only OD tables and metadata, without placeholders.
assert_od_input <- function(input, interval=FALSE) {
 required <- c('nodes','flows','unit','timezone','time_kind','provenance')
 allowed <- c(required,'title','description','preparation','notes',if(interval)'interval_seconds')
 stopifnot(is.list(input),identical(class(input),'list'),all(required%in%names(input)),
  all(names(input)%in%allowed),!anyDuplicated(names(input)),
  !any(vapply(input,is.null,logical(1))),is.data.frame(input$nodes),is.data.frame(input$flows),
  identical(input$unit,'trips'),identical(input$timezone,'America/Chicago'),
  inherits(input$flows$time,'POSIXct'))
 if(interval)stopifnot('interval_seconds'%in%names(input))
 else stopifnot(!'interval_seconds'%in%names(input))
 invisible(TRUE)
}
assert_od_input(full)
# Auxiliary source and generated attributes must survive conversion and export.
stopifnot(identical(names(full$nodes),c('node_id','latitude','longitude','display_name','node_idx',
 'representative_station_id','n_raw_stations','member_weight','coordinate_available')),
 identical(names(full$flows),c('ride_id','origin_id','destination_id','time','flow','origin_node_idx','destination_node_idx')),
 identical(full$nodes$node_idx,seq_len(nrow(full$nodes))-1L),
 identical(full$nodes$representative_station_id,full$nodes$node_id),
 identical(full$flows$origin_node_idx,full$nodes$node_idx[match(full$flows$origin_id,full$nodes$node_id)]),
 identical(full$flows$destination_node_idx,full$nodes$node_idx[match(full$flows$destination_id,full$nodes$node_id)]))
source_at <- match(full$flows$ride_id,raw$ride_id)
source_clocks <- as.POSIXct(strptime(raw$started_at[source_at],'%Y-%m-%d %H:%M:%OS',tz=full$timezone))
stopifnot(!anyNA(source_at),identical(full$flows$ride_id,raw$ride_id[source_at]),
 identical(full$flows$origin_id,raw$start_station_id[source_at]),
 identical(full$flows$destination_id,raw$end_station_id[source_at]),
 identical(full$flows$time,source_clocks),
 any(as.numeric(full$flows$time)%%1!=0))
stopifnot(nrow(raw)==1950L,nrow(full$nodes)==512L,nrow(full$flows)==1230L,sum(full$flows$flow)==1230L,
 sum(full$flows$origin_id==full$flows$destination_id)==85L)
bad<-raw;bad$ride_id[1]<-' ';reject(ride_input(bad))
bad<-raw;bad$started_at[1]<-'2026-01-01 25:00:00';reject(ride_input(bad))
renamed<-raw;names(renamed)[names(renamed)=='started_at']<-'departure_clock'
fields<-divvy_fields;fields['departure']<-'departure_clock'
stopifnot(identical(ride_input(renamed,fields=fields)$flows,full$flows))
gold<-readRDS(file.path(base,'data/input.rds'))
assert_od_input(gold)
# CSV preparation can round a coordinate at machine precision, while preserving
# the same station register.  Keep exact checks for every other top-level field.
same_nodes<-function(x,y)isTRUE(all.equal(x,y,tolerance=.Machine$double.eps^.5))
stopifnot(same_nodes(full$nodes,gold$nodes))
for(key in setdiff(names(full),c('nodes','provenance')))stopifnot(identical(full[[key]],gold[[key]]))
for(key in names(full$provenance))stopifnot(identical(full$provenance[[key]],gold$provenance[[key]]))
source_files<-list.files(file.path(base,'data'),full.names=TRUE,recursive=TRUE)
before<-tools::md5sum(source_files)
reject(prepare_example(base,output_dir=file.path(base,'data/input.rds')))
qa<-tempfile('original-method-test-');dir.create(qa)
file<-prepare_example(base,output_dir=file.path(qa,'prepared'))
assert_od_input(readRDS(file))
export_dictionary<-read.csv(file.path(dirname(file),'data-dictionary.csv'))
export_metadata<-jsonlite::read_json(file.path(dirname(file),'metadata.json'),simplifyVector=TRUE)
stopifnot(setequal(unique(export_dictionary$table),c('nodes','flows')),
 setequal(export_dictionary$field[export_dictionary$table=='flows'],names(full$flows)),
 file.exists(file.path(dirname(file),'nodes.csv')),file.exists(file.path(dirname(file),'flows.csv')),
 !file.exists(file.path(dirname(file),'od.csv')),export_metadata$input_type=='od',
 export_metadata$unit=='trips',export_metadata$time_kind=='instant',
 !any(c('schema','roads','flow','events','measurements','interval_seconds')%in%names(export_metadata)))
stopifnot(same_nodes(readRDS(file)$nodes,gold$nodes),identical(readRDS(file)$flows,gold$flows))
reject(prepare_example(base,output_dir=dirname(file)))
stopifnot(identical(before,tools::md5sum(source_files)))
unlink(qa,recursive=TRUE)
cat('Established source method, OD fields, numerical agreement and source protection passed.\n')

# Original-format source import and intermediate representation.
raw_dir<-file.path(base,'data/raw');original<-file.path(raw_dir,'202601-trip-excerpt.csv')
original_day<-read_divvy_source(original)
name_fields<-unname(divvy_fields[c('origin_name','destination_name')])
cleaned_day<-clean_divvy_station_names(original_day)
unchanged_fields<-setdiff(names(original_day),name_fields)
changed_names<-vapply(name_fields,function(field)sum(original_day[[field]]!=cleaned_day[[field]],na.rm=TRUE),0L)
stopifnot(identical(unname(changed_names),c(13L,11L)),identical(cleaned_day,raw),
 identical(original_day[unchanged_fields],cleaned_day[unchanged_fields]))
# Leading/internal spaces, missing names, IDs and unrelated source text are retained.
probe<-original_day[1:3,];probe$start_station_name<-c(' Leading  inner  ',NA_character_,'Unchanged')
probe$end_station_name<-c('End ', 'Two  spaces  ',NA_character_)
probe$start_station_id[1]<-' ID with spaces '
clean_probe<-clean_divvy_station_names(probe)
stopifnot(identical(clean_probe$start_station_name,c(' Leading  inner',NA_character_,'Unchanged')),
 identical(clean_probe$end_station_name,c('End','Two  spaces',NA_character_)),
 identical(clean_probe[unchanged_fields],probe[unchanged_fields]),
 identical(clean_divvy_station_names(clean_probe),clean_probe))
renamed_probe<-probe;names(renamed_probe)[match(name_fields,names(renamed_probe))]<-c('from_name','to_name')
name_map<-divvy_fields;name_map[c('origin_name','destination_name')]<-c('from_name','to_name')
stopifnot(identical(clean_divvy_station_names(renamed_probe,name_map)$from_name,clean_probe$start_station_name))
bad_map<-name_map;bad_map['destination_name']<-'from_name';reject(clean_divvy_station_names(renamed_probe,bad_map))
daily_csv<-tempfile('selected-day-',fileext='.csv')
extract_divvy_day(original,daily_csv,day='2026-01-01')
native_raw<-as.data.frame(data.table::fread(daily_csv,colClasses='character',na.strings=''))
stopifnot(identical(native_raw,raw),nrow(native_raw)==1950,ncol(native_raw)==13)
raw_input<-ride_input(native_raw)
reject(extract_divvy_day(original,original));reject(extract_divvy_day(original,daily_csv))
alias<-tempfile('raw-alias-')
if(file.symlink(original,alias))reject(extract_divvy_day(original,alias))
unlink(alias)
empty_day<-tempfile(fileext='.csv');reject(extract_divvy_day(original,empty_day,day='2026-02-01'))
stopifnot(!file.exists(empty_day));unlink(daily_csv)
raw_files<-list.files(raw_dir,full.names=TRUE)
raw_before<-tools::md5sum(raw_files)
stopifnot(same_nodes(raw_input$nodes,gold$nodes),identical(raw_input$flows,gold$flows))
prepared_dir<-tempfile('raw-source-roundtrip-')
prepared_file<-write_traffic_input(raw_input,prepared_dir,source_files=raw_files,native=NULL)

stopifnot(identical(readRDS(prepared_file)$nodes,raw_input$nodes),
 identical(readRDS(prepared_file)$flows,raw_input$flows),identical(raw_before,tools::md5sum(raw_files)))
unlink(prepared_dir,recursive=TRUE)
cat('Original-format import, declared source selection, intermediate readback, expected values and source preservation passed.\n')

# Independent derivatives retain the same source-day cohort and primary input.
hourly <- ride_input(raw, bin_seconds=3600)
assert_od_input(hourly,interval=TRUE)
stopifnot(nrow(hourly$flows)==1157L,identical(hourly$nodes,full$nodes),
 hourly$time_kind=='interval_start',hourly$interval_seconds==3600,
 sum(hourly$flows$flow)==sum(full$flows$flow),
 sum(hourly$flows$flow[hourly$flows$origin_id==hourly$flows$destination_id])==85,
 !anyDuplicated(hourly$flows[c('time','origin_id','destination_id')]))
expected_hour <- aggregate(list(flow=full$flows$flow),
 list(hour=format(full$flows$time,'%Y-%m-%d %H',tz=full$timezone),
      origin=full$flows$origin_id,destination=full$flows$destination_id),sum)
expected_key <- paste(expected_hour$hour,expected_hour$origin,expected_hour$destination)
hourly_key <- paste(format(hourly$flows$time,'%Y-%m-%d %H',tz=hourly$timezone),hourly$flows$origin_id,hourly$flows$destination_id)
stopifnot(setequal(expected_key,hourly_key),
 identical(as.numeric(expected_hour$flow[match(hourly_key,expected_key)]),as.numeric(hourly$flows$flow)),
 all(format(hourly$flows$time,'%M:%S',tz=hourly$timezone)=='00:00'))
reject(ride_input(raw,bin_seconds=700))

# The 1500 m result is created during preparation and feeds traffic_od_map directly.
spatial <- spatial_od_input(full,1500)
metadata <- spatial$provenance$spatial_aggregation
membership <- as.data.frame(metadata$membership)
stopifnot(nrow(spatial$nodes)==62L,nrow(membership)==nrow(full$nodes),
 !anyDuplicated(membership$raw_node_id),setequal(membership$raw_node_id,full$nodes$node_id),
 all(membership$aggregate_node_id%in%spatial$nodes$node_id),
 metadata$max_assignment_distance_m<=1500+1e-7,
 sum(spatial$nodes$raw_node_count)==nrow(full$nodes),
 sum(spatial$flows$flow)==sum(full$flows$flow),
 sum(spatial$flows$flow[spatial$flows$origin_id==spatial$flows$destination_id])==435,
 identical(spatial$nodes$display_name,full$nodes$display_name[match(spatial$nodes$node_id,full$nodes$node_id)]))
member_map <- setNames(membership$aggregate_node_id,membership$raw_node_id)
expected_spatial <- aggregate(list(flow=full$flows$flow),
 list(time=as.numeric(full$flows$time),origin=unname(member_map[full$flows$origin_id]),
      destination=unname(member_map[full$flows$destination_id])),sum)
expected_key <- paste(sprintf('%.17g',expected_spatial$time),expected_spatial$origin,expected_spatial$destination)
spatial_key <- paste(sprintf('%.17g',as.numeric(spatial$flows$time)),spatial$flows$origin_id,spatial$flows$destination_id)
stopifnot(!anyDuplicated(expected_key),!anyDuplicated(spatial_key),setequal(expected_key,spatial_key),
 identical(as.numeric(expected_spatial$flow[match(spatial_key,expected_key)]),as.numeric(spatial$flows$flow)))
assert_od_input(spatial)
stopifnot(spatial$time_kind=='instant',!'interval_seconds'%in%names(spatial))
prepared_spatial <- readRDS(file.path(base,'data/spatial-1500m/input.rds'))
expected_prepared_spatial <- spatial_od_input(gold,1500)
prepared_metadata <- prepared_spatial$provenance$spatial_aggregation
expected_metadata <- expected_prepared_spatial$provenance$spatial_aggregation
stable_metadata <- setdiff(names(expected_metadata),'created_utc')
stopifnot(identical(prepared_spatial$nodes,expected_prepared_spatial$nodes),
 identical(prepared_spatial$flows,expected_prepared_spatial$flows),
 identical(prepared_spatial$provenance$display_radius_m,expected_prepared_spatial$provenance$display_radius_m),
 identical(prepared_spatial$provenance$station_preparation,expected_prepared_spatial$provenance$station_preparation),
 identical(prepared_metadata[stable_metadata],expected_metadata[stable_metadata]))
bounds <- divvy_window_times('2026-01-01','00:00','03:00',full$timezone)
shipped_window <- readRDS(file.path(base,'data/spatial-1500m/window-0000-0300.rds'))
rebuilt_window <- divvy_window_input(prepared_spatial,bounds$start_time,bounds$end_time)
stopifnot(identical(shipped_window,rebuilt_window),inherits(shipped_window,'bdynets_od_pairs'),
 inherits(shipped_window$start_time,'POSIXct'),inherits(shipped_window$end_time,'POSIXct'),
 identical(shipped_window$timezone,full$timezone),identical(shipped_window$unit,'trips'),
 is.null(shipped_window$time_window),shipped_window$display_radius_m==1500)
routes <- shipped_window$routes
plotted_nodes <- shipped_window$nodes
origin <- match(routes$origin_id,plotted_nodes$node_id)
destination <- match(routes$destination_id,plotted_nodes$node_id)
stopifnot(!anyNA(origin),!anyNA(destination),
 !anyDuplicated(routes[c('origin_id','destination_id')]),all(routes$origin_id!=routes$destination_id),
 identical(routes$origin_longitude,plotted_nodes$longitude[origin]),
 identical(routes$origin_latitude,plotted_nodes$latitude[origin]),
 identical(routes$destination_longitude,plotted_nodes$longitude[destination]),
 identical(routes$destination_latitude,plotted_nodes$latitude[destination]),
 shipped_window$selected_flow==288,shipped_window$within_node_flow==87,
 sum(routes$flow)==201,nrow(routes)==129)

# The shipped RDS supplies everything the existing map needs.
mapped <- traffic_od_map(shipped_window,basemap=NULL,draw=FALSE)
stopifnot(identical(mapped$routes,routes),identical(mapped$nodes,plotted_nodes),
 mapped$summary$selected_flow==288,mapped$summary$within_node_flow==87)

# Exact boundaries, direction and within-group counts on a small prepared day.
sample_spatial <- prepared_spatial
a <- sample_spatial$nodes$node_id[1L]
b <- sample_spatial$nodes$node_id[2L]
sample_spatial$flows <- data.frame(
 origin_id=c(a,a,a,b,a,a),destination_id=c(b,b,b,a,a,b),flow=c(40,1,2,5,7,60),
 time=bounds$start_time+c(-1,0,1,2,3,10800))
boundary_window <- divvy_window_input(sample_spatial,bounds$start_time,bounds$end_time)
stopifnot(boundary_window$selected_flow==15,boundary_window$within_node_flow==7,
 identical(boundary_window$routes$flow,c(5,3)))
reject(divvy_window_times('2026-01-01','03:00','02:00',full$timezone))
reject(divvy_window_times('2026-11-01','01:30','02:00',full$timezone))
reject(divvy_window_times('2026-03-08','02:30','04:00',full$timezone))
reject(divvy_window_input(sample_spatial,bounds$end_time+10,bounds$end_time+20))
all_day <- divvy_window_times('2026-01-01','00:00','24:00',full$timezone)
stopifnot(format(all_day$end_time,'%Y-%m-%d %H:%M',tz=full$timezone)=='2026-01-02 00:00',
 divvy_window_input(prepared_spatial,all_day$start_time,all_day$end_time)$selected_flow==1230)

# Change the source date and the window independently of the bundled display.
alternate_source <- tempfile(fileext='.csv')
writeLines(gsub('2026-01-01','2026-01-02',
 readLines(file.path(base,'data/trips-2026-01-01.csv')),fixed=TRUE),alternate_source)
alternate_dir <- tempfile('divvy-prepared-day-')
alternate_paths <- prepare_all_example(base,output_dir=alternate_dir,source_file=alternate_source,
 day='2026-01-02',start_clock='01:00',end_clock='02:00')
alternate_window <- readRDS(alternate_paths[['window']])
original_bounds <- divvy_window_times('2026-01-01','01:00','02:00',full$timezone)
original_window <- divvy_window_input(prepared_spatial,original_bounds$start_time,original_bounds$end_time)
stopifnot(basename(alternate_paths[['window']])=='window-0100-0200.rds',
 all(file.exists(alternate_paths)),
 format(alternate_window$start_time,'%Y-%m-%d %H:%M',tz=full$timezone)=='2026-01-02 01:00',
 format(alternate_window$end_time,'%Y-%m-%d %H:%M',tz=full$timezone)=='2026-01-02 02:00',
 identical(alternate_window$routes,original_window$routes),
 identical(alternate_window$selected_flow,original_window$selected_flow))
reject(prepare_all_example(base,output_dir=alternate_dir))
failed_dir <- tempfile('divvy-missing-window-')
reject(prepare_all_example(base,output_dir=failed_dir,start_clock='00:00',end_clock='00:01'))
stopifnot(!file.exists(failed_dir),!file.exists(paste0(failed_dir,'.preparation-lock')))
unlink(c(alternate_source,alternate_dir),recursive=TRUE)
cat('Prepared OD windows, alternate dates, exact boundaries, counts and direct map readback passed.\n')

derived_qa <- tempfile('derived-od-check-');dir.create(derived_qa)
hourly_path <- prepare_example(base,output_dir=file.path(derived_qa,'hourly'),bin_seconds=3600)
stopifnot(identical(readRDS(hourly_path)$flows,hourly$flows),identical(readRDS(hourly_path)$nodes,hourly$nodes))
station_path <- prepare_example(base,output_dir=file.path(derived_qa,'station'))
spatial_path <- prepare_spatial_example(base,file.path(derived_qa,'spatial'),station_path,1500)
spatial <- spatial_od_input(readRDS(station_path),1500)
restored <- readRDS(spatial_path)
stopifnot(identical(restored$nodes,spatial$nodes),identical(restored$flows,spatial$flows),
 identical(restored$provenance$spatial_aggregation$membership,membership))
for(entry in list(list(hourly_path,hourly),list(spatial_path,spatial))) {
 directory <- dirname(entry[[1]]);expected <- entry[[2]]
 metadata <- jsonlite::read_json(file.path(directory,'metadata.json'),simplifyVector=TRUE)
 stopifnot(metadata$timezone==expected$timezone,metadata$unit=='trips',metadata$time_kind==expected$time_kind,
  identical(as.numeric(metadata$interval_seconds),as.numeric(expected$interval_seconds)))
 for(table in c('nodes','flows')) {
  target <- expected[[table]]
  classes <- vapply(target,function(v)if(inherits(v,'POSIXct')||is.double(v))'numeric' else if(is.integer(v))'integer' else if(is.logical(v))'logical' else 'character','')
  actual <- as.data.frame(data.table::fread(file.path(directory,paste0(table,'.csv')),colClasses=classes,na.strings=metadata$csv$missing))
  stopifnot(identical(names(actual),names(target)),nrow(actual)==nrow(target))
  for(field in names(target)) {
   if(inherits(target[[field]],'POSIXct')||is.numeric(target[[field]]))
    stopifnot(isTRUE(all.equal(as.numeric(actual[[field]]),as.numeric(target[[field]]),tolerance=.Machine$double.eps)))
   else stopifnot(identical(actual[[field]],target[[field]]))
   stopifnot(identical(is.na(actual[[field]]),is.na(target[[field]])))
  }
 }
 hashes <- read.csv(file.path(directory,'checksums.csv'))
 stopifnot(identical(unname(tools::md5sum(file.path(directory,hashes$file))),hashes$md5))
}
membership_file <- file.path(derived_qa,'membership.csv')
write.csv(membership,membership_file,row.names=FALSE)
stopifnot(identical(as.data.frame(data.table::fread(membership_file,
 colClasses=c(raw_node_id='character',aggregate_node_id='character',distance_to_representative_m='numeric'))),membership))
reject(write_traffic_input(spatial,dirname(spatial_path),source_files=file.path(base,'data/trips-2026-01-01.csv')))
stopifnot(identical(before,tools::md5sum(source_files)))
unlink(derived_qa,recursive=TRUE)
cat('Hourly and spatial derivatives preserve counts, clocks, membership, exports and the primary source.\n')

# Optional full monthly archive check uses the same path and all thirteen original columns.
if(!is.null(original_month)) {
  monthly_before<-tools::md5sum(original_month)
  monthly_day<-read_divvy_source(original_month)
  stopifnot(identical(monthly_day,original_day))
  monthly_daily_csv<-tempfile('monthly-day-readback-',fileext='.csv')
  extract_divvy_day(original_month,monthly_daily_csv)
  monthly_restored<-as.data.frame(data.table::fread(monthly_daily_csv,colClasses='character',na.strings='',strip.white=FALSE))
  stopifnot(identical(monthly_restored,raw),
    identical(monthly_restored[unchanged_fields],monthly_day[unchanged_fields]))
  monthly_input<-ride_input(monthly_restored)
  stopifnot(same_nodes(monthly_input$nodes,gold$nodes),identical(monthly_input$flows,gold$flows),
    identical(monthly_before,tools::md5sum(original_month)))
  unlink(monthly_daily_csv)
  cat('Complete original month -> selected day -> declared name cleaning -> CSV readback -> supplied standard nodes/OD passed.\n')
}


stopifnot(identical(before,tools::md5sum(source_files)))

# A current report source must replace an existing HTML on repeated renders.
render_test<-tempfile('report-rebuild-');dir.create(render_test)
dir.create(file.path(render_test,'examples'))
stopifnot(file.copy(file.path(base,'..','..','bdynets'),render_test,recursive=TRUE))
stopifnot(file.copy(base,file.path(render_test,'examples'),recursive=TRUE))
stopifnot(file.copy(file.path(base,'..','DATA-SOURCES.md'),
 file.path(render_test,'examples','DATA-SOURCES.md')))
case_copy<-file.path(render_test,'examples',basename(base))
report_source<-file.path(case_copy,'divvy-od-report.Rmd')
original_report<-readLines(report_source,warn=FALSE)
previous_marker<-NULL
for(marker in c('SOURCE_RENDER_FIRST','SOURCE_RENDER_SECOND')) {
 writeLines(c(original_report,'',paste('Source rebuild check:',marker)),report_source)
 log<-file.path(render_test,paste0(marker,'.log'))
 status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla',shQuote(file.path(case_copy,'run.R')),'--offline'),stdout=log,stderr=log)
 if(status!=0L)stop('Independent report rebuild failed: ',paste(tail(readLines(log,warn=FALSE),5),collapse=' '))
 html<-paste(readLines(file.path(case_copy,'divvy-od-report.html'),warn=FALSE),collapse='\n')
 if(!grepl(marker,html,fixed=TRUE))stop('The completed render retained an older HTML instead of the current source.')
 if(!is.null(previous_marker))stopifnot(!grepl(previous_marker,html,fixed=TRUE))
 previous_marker<-marker
}
unlink(render_test,recursive=TRUE)
stopifnot(identical(before,tools::md5sum(source_files)))
cat('Repeated independent report builds publish the current source, replacing stale HTML.\n')
