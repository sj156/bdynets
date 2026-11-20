args<-commandArgs(TRUE)
base<-normalizePath(if(length(args))args[1] else '.')
source(file.path(base,'..','..','bdynets','inst','preparation-session.R'))
preparation_session(base)
library(bdynets)
source(file.path(base,'prepare.R'))
reject<-function(expr)stopifnot(inherits(try(force(expr),silent=TRUE),'try-error'))
raw<-jsonlite::read_json(file.path(base,'data/road-102-source.json'),simplifyVector=TRUE)
roads<-beijing_network(raw$network)
native<-beijing_records(raw$minutes,roads,0:9)
x<-beijing_input(native)
stopifnot(is.null(x[['schema']]),all(c('roads','flow','unit')%in%names(x)),
 !any(c('nodes','od','flows','od_unit','od_time_kind')%in%names(x)))
stopifnot(identical(x$flow$flow,c(24,0)),all(x$interval_seconds==300))
reject(beijing_records(rbind(raw$minutes,raw$minutes[1,]),roads,0:9))
reject(beijing_records(raw$minutes[-1,],roads,0:9))
bad<-raw$minutes;bad[[beijing_fields['rate']]][1]<- -1;reject(beijing_records(bad,roads,0:9))
renamed<-raw$minutes;names(renamed)[match(beijing_fields['rate'],names(renamed))]<-'source_rate'
fields<-beijing_fields;fields['rate']<-'source_rate'
stopifnot(identical(beijing_input(beijing_records(renamed,roads,0:9,fields))$flow,x$flow))
full<-beijing_input(readRDS(file.path(base,'data/source.rds')))
gold<-readRDS(file.path(base,'data/input.rds'))
map_path<-file.path(base,'data/map-frame-0800.rds')
stopifnot(file.exists(map_path))
map_input<-readRDS(map_path)
stopifnot(inherits(map_input,'trafficmap_data'),length(map_input$times)==1L,
  is.null(map_input$time_labels),identical(map_input$time_label_format,'%H:%M'))
day_view<-read_traffic(file.path(base,'data/input.rds'))
map_index<-which(day_view$times == 8*3600)
stopifnot(length(map_index)==1L)
map_view<-traffic_flow_map(map_path,basemap=NULL,draw=FALSE)$data
stopifnot(identical(map_view$time_labels,'08:00'),
  identical(map_view$times,day_view$times[map_index]),
  identical(map_view$interval_seconds,day_view$interval_seconds[map_index]),
  identical(map_view$unit,day_view$unit),identical(map_view$roads,day_view$roads),
  identical(map_view$values,day_view$values[map_index,,drop=FALSE]))
cat('The shipped 08:00 single-frame RDS is a direct Flow input.\n')
stopifnot(is.null(gold$time_labels),is.null(full$time_labels),
  is.null(jsonlite::read_json(file.path(base,'data','metadata.json'))$time_labels))
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
raw_dir<-file.path(base,'data/raw')
native_raw<-read_beijing_raw(file.path(raw_dir,'output_LinkTDMOE.csv'),file.path(raw_dir,'network.html'),day=10L,expected_minutes=0:9)
raw_input<-beijing_input(native_raw)
stopifnot(identical(as.numeric(raw_input$flow$flow),c(24,0)))
bad_source<-read.csv(file.path(raw_dir,'output_LinkTDMOE.csv'))
bad_file<-tempfile(fileext='.csv');write.csv(rbind(bad_source,bad_source[1,]),bad_file,row.names=FALSE)
reject(read_beijing_raw(bad_file,file.path(raw_dir,'network.html'),day=10L,expected_minutes=0:9));unlink(bad_file)
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
prepared_file<-write_traffic_input(raw_input,prepared_dir,source_files=raw_files,native=native_raw)
stopifnot(identical(readRDS(file.path(prepared_dir,'source.rds')),native_raw))
stopifnot(identical(readRDS(prepared_file)$flow,raw_input$flow),identical(raw_before,tools::md5sum(raw_files)))
unlink(prepared_dir,recursive=TRUE)
cat('Original-format import, declared source selection, intermediate readback, expected values and source preservation passed.\n')

stopifnot(identical(before,tools::md5sum(source_files)))
