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
stopifnot(nrow(raw)==1950L,nrow(full$nodes)==512L,nrow(full$od)==1230L,sum(full$od$flow)==1230L,
 sum(full$od$origin_id==full$od$destination_id)==85L)
bad<-raw;bad$ride_id[1]<-' ';reject(ride_input(bad))
bad<-raw;bad$started_at[1]<-'2026-01-01 25:00:00';reject(ride_input(bad))
renamed<-raw;names(renamed)[names(renamed)=='started_at']<-'departure_clock'
fields<-divvy_fields;fields['departure']<-'departure_clock'
stopifnot(identical(ride_input(renamed,fields=fields)$od,full$od))
gold<-readRDS(file.path(base,'data/input.rds'))
for(key in c('roads','flow','events','nodes','od','measurements','times','time_labels','interval_seconds','unit','timezone'))
 stopifnot(identical(full[[key]],gold[[key]]))
if(!is.null(gold$trends))stopifnot(identical(full$trends$value,gold$trends$value))
source_files<-list.files(file.path(base,'data'),full.names=TRUE,recursive=TRUE)
before<-tools::md5sum(source_files)
reject(prepare_example(base,output_dir=file.path(base,'data/input.rds')))
qa<-tempfile('original-method-test-');dir.create(qa)
file<-prepare_example(base,output_dir=file.path(qa,'prepared'))
stopifnot(identical(readRDS(file)$flow,gold$flow),identical(readRDS(file)$od,gold$od))
reject(prepare_example(base,output_dir=dirname(file)))
stopifnot(identical(before,tools::md5sum(source_files)))
unlink(qa,recursive=TRUE)
cat('Established source method, standard fields, numerical agreement and source protection passed.\n')

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
if(!is.null(raw_input$flow)) {
 at<-match(paste(raw_input$flow$road_id,raw_input$flow$time),paste(gold$flow$road_id,gold$flow$time))
 stopifnot(!anyNA(at),identical(raw_input$flow$flow,gold$flow$flow[at]))
 if(!is.null(raw_input$measurements)) {
  at<-match(paste(raw_input$measurements$road_id,raw_input$measurements$time,raw_input$measurements$metric),paste(gold$measurements$road_id,gold$measurements$time,gold$measurements$metric))
  stopifnot(!anyNA(at),identical(raw_input$measurements$value,gold$measurements$value[at]))
 }
} else stopifnot(identical(raw_input$nodes,gold$nodes),identical(raw_input$od,gold$od))
prepared_dir<-tempfile('raw-source-roundtrip-')
prepared_file<-write_traffic_input(raw_input,prepared_dir,source_files=raw_files,native=NULL)

stopifnot(identical(readRDS(prepared_file)$flow,raw_input$flow),identical(raw_before,tools::md5sum(raw_files)))
unlink(prepared_dir,recursive=TRUE)
cat('Original-format import, declared source selection, intermediate readback, expected values and source preservation passed.\n')

# Independent derivatives retain the same source-day cohort and primary input.
hourly <- ride_input(raw, bin_seconds=3600)
stopifnot(nrow(hourly$od)==1157L,identical(hourly$nodes,full$nodes),
 hourly$od_time_kind=='interval_start',hourly$od_interval_seconds==3600,
 sum(hourly$od$flow)==sum(full$od$flow),
 sum(hourly$od$flow[hourly$od$origin_id==hourly$od$destination_id])==85,
 !anyDuplicated(hourly$od[c('time','origin_id','destination_id')]))
expected_hour <- aggregate(list(flow=full$od$flow),
 list(hour=format(full$od$time,'%Y-%m-%d %H',tz=full$timezone),
      origin=full$od$origin_id,destination=full$od$destination_id),sum)
expected_key <- paste(expected_hour$hour,expected_hour$origin,expected_hour$destination)
hourly_key <- paste(format(hourly$od$time,'%Y-%m-%d %H',tz=hourly$timezone),hourly$od$origin_id,hourly$od$destination_id)
stopifnot(setequal(expected_key,hourly_key),
 identical(as.numeric(expected_hour$flow[match(hourly_key,expected_key)]),as.numeric(hourly$od$flow)),
 all(format(hourly$od$time,'%M:%S',tz=hourly$timezone)=='00:00'))
reject(ride_input(raw,bin_seconds=700))

# 1000 m is the explicit radius in the preserved aggregation lesson.
grouped <- aggregate_od_data(full$nodes,full$od,radius_m=1000,
 node_columns=c(id='node_id',longitude='longitude',latitude='latitude',name='display_name',weight=NA_character_),
 flow_columns=c(origin='origin_id',destination='destination_id',count='flow',time='time'),timezone=full$timezone)
membership <- as.data.frame(grouped$membership)
stopifnot(nrow(grouped$nodes)==105L,nrow(membership)==nrow(full$nodes),
 !anyDuplicated(membership$raw_node_id),setequal(membership$raw_node_id,full$nodes$node_id),
 all(membership$aggregate_node_id%in%grouped$nodes$node_id),
 grouped$metadata$max_assignment_distance_m<=1000+1e-7,
 sum(grouped$nodes$raw_node_count)==nrow(full$nodes),
 sum(grouped$flows$flow)==sum(full$od$flow),
 sum(grouped$flows$flow[grouped$flows$origin_id==grouped$flows$destination_id])==317,
 identical(grouped$nodes$display_name,full$nodes$display_name[match(grouped$nodes$node_id,full$nodes$node_id)]))
member_map <- setNames(membership$aggregate_node_id,membership$raw_node_id)
expected_spatial <- aggregate(list(flow=full$od$flow),
 list(time=as.numeric(full$od$time),origin=unname(member_map[full$od$origin_id]),
      destination=unname(member_map[full$od$destination_id])),sum)
expected_key <- paste(sprintf('%.17g',expected_spatial$time),expected_spatial$origin,expected_spatial$destination)
spatial_key <- paste(sprintf('%.17g',as.numeric(grouped$flows$time)),grouped$flows$origin_id,grouped$flows$destination_id)
stopifnot(!anyDuplicated(expected_key),!anyDuplicated(spatial_key),setequal(expected_key,spatial_key),
 identical(as.numeric(expected_spatial$flow[match(spatial_key,expected_key)]),as.numeric(grouped$flows$flow)))
spatial <- traffic_input(nodes=as.data.frame(grouped$nodes),od=as.data.frame(grouped$flows),
 timezone=full$timezone,od_time_kind=full$od_time_kind,
 provenance=list(spatial_aggregation=c(grouped$metadata,list(membership=membership))))
stopifnot(spatial$od_time_kind=='instant',is.null(spatial$od_interval_seconds))

derived_qa <- tempfile('derived-od-check-');dir.create(derived_qa)
hourly_path <- prepare_example(base,output_dir=file.path(derived_qa,'hourly'),bin_seconds=3600)
stopifnot(identical(readRDS(hourly_path)$od,hourly$od),identical(readRDS(hourly_path)$nodes,hourly$nodes))
spatial_path <- write_traffic_input(spatial,file.path(derived_qa,'spatial'),source_files=file.path(base,'data/trips-2026-01-01.csv'))
restored <- readRDS(spatial_path)
stopifnot(identical(restored$nodes,spatial$nodes),identical(restored$od,spatial$od),
 identical(restored$provenance$spatial_aggregation$membership,membership))
for(entry in list(list(hourly_path,hourly),list(spatial_path,spatial))) {
 directory <- dirname(entry[[1]]);expected <- entry[[2]]
 metadata <- jsonlite::read_json(file.path(directory,'metadata.json'),simplifyVector=TRUE)
 stopifnot(metadata$timezone==expected$timezone,metadata$od_unit=='trips',metadata$od_time_kind==expected$od_time_kind,
  identical(as.numeric(metadata$od_interval_seconds),as.numeric(expected$od_interval_seconds)))
 for(table in c('nodes','od')) {
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
  stopifnot(identical(monthly_input$nodes,gold$nodes),identical(monthly_input$od,gold$od),
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
