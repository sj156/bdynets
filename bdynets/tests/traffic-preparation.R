library(bdynets)
options(bdynets.basemap.auto=FALSE)
stopifnot(exists('write_traffic_input',mode='function'))
qa <- tempfile('preparation-test-');dir.create(qa)
fail <- function(expr,pattern) {
  problem<-tryCatch({force(expr);NULL},error=conditionMessage)
  stopifnot(is.character(problem),grepl(pattern,problem,ignore.case=TRUE))
}
raw<-file.path(qa,'rides.csv');writeLines(c('id,value','001,0'),raw)
initial<-unname(tools::md5sum(raw))
nodes<-data.frame(node_id=c('001','002'),longitude=c(-87.6,-87.7),latitude=c(41.8,41.9))
od<-data.frame(ride_id='r',origin_id='001',destination_id='002',flow=1L)
x<-list(nodes=nodes,flows=od,unit='trips',time_kind='untimed',provenance=list(source='test records'))
fail(write_traffic_input(stop('must not read data'),raw,source_files=raw),'source|exist|protected')
fail(write_traffic_input(x,file.path(qa,'..',basename(qa),'rides.csv'),source_files=raw),'source|exist|protected')
if(file.symlink(raw,file.path(qa,'alias')))fail(write_traffic_input(x,file.path(qa,'alias'),source_files=raw),'source|exist|protected')
source_dir<-file.path(qa,'original');dir.create(source_dir)
fail(write_traffic_input(x,file.path(source_dir,'results'),source_files=source_dir),'source|protected')
if(file.symlink(source_dir,file.path(qa,'alias-directory')))fail(write_traffic_input(x,file.path(qa,'alias-directory','result'),source_files=source_dir),'source|protected')
fail(write_traffic_input(stop('conversion failed'),file.path(qa,'failed'),source_files=raw),'conversion failed')
stopifnot(!file.exists(file.path(qa,'failed')),identical(initial,unname(tools::md5sum(raw))))
out<-file.path(qa,'result');write_traffic_input(x,out,source_files=raw)
y<-readRDS(file.path(out,'input.rds'))
stopifnot(identical(x$nodes,y$nodes),identical(x$flows,y$flows),length(y$provenance$preparation_sources$sha256)==1L)
fail(write_traffic_input(x,out,source_files=raw),'exist')
expected<-c('input.rds','nodes.csv','flows.csv','metadata.json','data-dictionary.csv','validation.csv','source-files.csv','checksums.csv')
stopifnot(setequal(list.files(out),expected))
f<-read.csv(file.path(out,'flows.csv'),colClasses='character',check.names=FALSE)
stopifnot(f$origin_id=='001',f$destination_id=='002')
check<-read.csv(file.path(out,'checksums.csv'))
stopifnot(identical(unname(tools::md5sum(file.path(out,check$file))),check$md5))
stopifnot(identical(initial,unname(tools::md5sum(raw))),!any(grepl('preparing|lock',list.files(qa))))
unlink(qa,recursive=TRUE)
cat('New-directory publication, source aliases, failure cleanup, input validation and reusable export passed.\n')
qa<-tempfile('publication-boundaries-');dir.create(qa)
fail(write_traffic_input(x,file.path(qa,'missing-source'),source_files=file.path(qa,'absent.csv')),'source.*exist')
validation<-data.frame(check='Known failed check',observed=0,expected=1,passed=FALSE,detail='Fixture')
fail(write_traffic_input(x,file.path(qa,'invalid'),validation=validation),'validation')
stopifnot(!dir.exists(file.path(qa,'invalid')),!length(list.files(qa)))
if(.Platform$OS.type=='unix') {
  target<-file.path(qa,'concurrent')
  first<-parallel::mcparallel(write_traffic_input({Sys.sleep(.3);x},target),silent=TRUE)
  Sys.sleep(.1)
  fail(write_traffic_input(x,target),'another|exist')
  result<-parallel::mccollect(first)
  stopifnot(!inherits(result[[1]],'try-error'),file.exists(file.path(target,'input.rds')))
}
zeros<-list(unit='trips',time_kind='instant',nodes=data.frame(node_id=c('001','002','\\N'),longitude=1:3,latitude=1:3),
 flows=data.frame(ride_id=c('a','b'),origin_id=c('001','\\N'),destination_id=c('002','001'),
 time=as.POSIXct(c(1767247200.125,1767247200.5),origin='1970-01-01',tz='America/Chicago'),flow=c(0L,1L)))
write_traffic_input(zeros,file.path(qa,'precision'))
meta<-jsonlite::read_json(file.path(qa,'precision','metadata.json'),simplifyVector=TRUE)
actual<-read.csv(file.path(qa,'precision','flows.csv'),colClasses='character',na.strings=meta$csv$missing)
stopifnot(identical(actual$origin_id,zeros$flows$origin_id),identical(as.numeric(actual$time),as.numeric(zeros$flows$time)),
 identical(as.numeric(actual$flow),as.numeric(zeros$flows$flow)))
unlink(qa,recursive=TRUE)
cat('Failed validations, concurrent publication, literal missing tokens and fractional clocks passed.\n')

qa<-tempfile('canonical-source-');dir.create(qa)
source_dir<-file.path(qa,'original');dir.create(source_dir)
fail(write_traffic_input(stop('not evaluated'),file.path(qa,'uncreated','..','original','result'),source_files=source_dir),'protected')
stopifnot(!dir.exists(file.path(qa,'uncreated')))
unlink(qa,recursive=TRUE)
cat('Uncreated path components resolve before any directory creation.\n')

# An unsuccessful source install must not load another installed package.
qa<-tempfile('session-install-');dir.create(qa)
package_dir<-file.path(qa,'bdynets');dir.create(package_dir)
dir.create(file.path(package_dir,'R'))
writeLines(c('Package: bdynets','Version: 0.2.1','Title: Installation Failure Fixture',
 'Description: Test rejection of fallback after a failed source installation.',
 'Authors@R: person("Test", "Fixture", role=c("aut","cre"), email="test@example.org")',
 'License: MIT','Imports: bdynetsMissingDependencyForTest'),file.path(package_dir,'DESCRIPTION'))
writeLines('export(write_traffic_input)',file.path(package_dir,'NAMESPACE'))
writeLines('write_traffic_input <- function(...) NULL',file.path(package_dir,'R','fixture.R'))
example_dir<-file.path(qa,'examples','fixture');dir.create(example_dir,recursive=TRUE)
helper<-system.file('preparation-session.R',package='bdynets',mustWork=TRUE)
script<-file.path(qa,'verify.R');log<-file.path(qa,'verify.log')
writeLines(c(
 paste0('.libPaths(',paste(deparse(.libPaths()),collapse=''),')'),
 paste0('source(',encodeString(helper,quote='"'),')'),
 paste0('result <- try(preparation_session(',encodeString(example_dir,quote='"'),'), silent=TRUE)'),
 'stopifnot(inherits(result,"try-error"), grepl("Bundled package installation failed",as.character(result),fixed=TRUE))'
),script)
status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla',shQuote(script)),stdout=log,stderr=log)
if(status!=0L)stop('Failed-install session check: ',paste(readLines(log,warn=FALSE),collapse='\n'))
unlink(qa,recursive=TRUE)
cat('A failed session install cannot fall back to a different installed package.\n')

# Road and OD preparation outputs have independent table contracts.
qa<-tempfile('separate-inputs-');dir.create(qa)
roads<-data.frame(road_id='001',source_attribute='kept')
roads$geometry<-list(rbind(c(12,51),c(12.1,51.1)))
road<-list(roads=roads,
 flow=data.frame(road_id=c('001','001'),time=c(0,300),flow=c(0,NA_real_)),
 times=c(0,300),time_labels=c('00:00','00:05'),interval_seconds=c(300,300),
 unit='vehicles/hour',provenance=list(native_missing=c(FALSE,TRUE)))
legacy_path<-file.path(qa,'road-untagged.rds')
saveRDS(road,legacy_path)
legacy_view<-read_traffic(legacy_path)
stopifnot(identical(as.numeric(legacy_view$values),c(0,NA_real_)),
 identical(legacy_view$roads$road_id,'001'),identical(legacy_view$times,road$times),
 identical(legacy_view$time_labels,road$time_labels),
 identical(legacy_view$interval_seconds,road$interval_seconds),identical(legacy_view$unit,road$unit),
 identical(legacy_view$provenance,road$provenance),
 identical(traffic_flow_map(legacy_path,basemap=NULL,draw=FALSE)$data$values,legacy_view$values),
 identical(traffic_flow_map(road,basemap=NULL,draw=FALSE)$data$values,legacy_view$values))
path<-write_traffic_input(road,file.path(qa,'road'))
restored<-readRDS(path)
stopifnot(identical(restored$roads,road$roads),identical(restored$flow,road$flow),
 identical(restored[['schema']],'bdynets-traffic-input-v1'),
 !any(c('nodes','od','flows','od_unit')%in%names(restored)),
 identical(restored$provenance$native_missing,road$provenance$native_missing),
 identical(read_traffic(path)$values,legacy_view$values),
 identical(traffic_flow_map(path,basemap=NULL,draw=FALSE)$data$values,legacy_view$values))
metadata<-jsonlite::read_json(file.path(dirname(path),'metadata.json'),simplifyVector=TRUE)
stopifnot(metadata$input_type=='road',metadata$unit=='vehicles/hour',identical(metadata$schema,restored$schema))
unsupported_path<-file.path(qa,'road-unsupported.rds')
unsupported<-road
unsupported$schema<-'unrecognized-road-format'
saveRDS(unsupported,unsupported_path)
fail(read_traffic(unsupported_path),'Unsupported traffic input schema')
fail(traffic_flow_map(unsupported_path,basemap=NULL,draw=FALSE),'bdynets-traffic-input-v1')
od<-list(nodes=data.frame(node_id=c('001','017'),longitude=c(12,12.1),latitude=c(51,51.1),
 display_name=c('First','Second'),node_idx=0:1),
 flows=data.frame(ride_id='trip',origin_id='001',destination_id='017',flow=1L,
 time=as.POSIXct(1767247200.125,origin='1970-01-01',tz='America/Chicago')),
 unit='trips',timezone='America/Chicago',time_kind='instant',provenance=list(source='test'))
path<-write_traffic_input(od,file.path(qa,'od'))
restored<-readRDS(path)
fail(read_traffic(path),'Unsupported traffic input schema')
fail(traffic_flow_map(path,basemap=NULL,draw=FALSE),'bdynets-traffic-input-v1')
stopifnot(identical(restored$nodes,od$nodes),identical(restored$flows,od$flows),
 !any(c('schema','roads','flow','events','od')%in%names(restored)))
metadata<-jsonlite::read_json(file.path(dirname(path),'metadata.json'),simplifyVector=TRUE)
csv<-read.csv(file.path(dirname(path),'flows.csv'),colClasses=c(ride_id='character',origin_id='character',destination_id='character'))
stopifnot(metadata$input_type=='od',metadata$unit=='trips',metadata$time_kind=='instant',
 metadata$timezone=='America/Chicago',identical(csv$origin_id,od$flows$origin_id),
 identical(as.numeric(csv$time),as.numeric(od$flows$time)))
dictionary<-read.csv(file.path(dirname(path),'data-dictionary.csv'))
stopifnot(all(dictionary$unit[dictionary$table=='flows'&dictionary$field=='flow']=='trips'))
bad<-road;bad$nodes<-od$nodes;bad$flows<-od$flows
fail(write_traffic_input(bad,file.path(qa,'mixed')),'separate')
bad$schema<-'bdynets-traffic-input-v1'
fail(write_traffic_input(bad,file.path(qa,'mixed-tagged')),'separate')
fail(traffic_flow_map(bad,basemap=NULL,draw=FALSE),'separate')
fail(traffic_od_map(bad,basemap=NULL,draw=FALSE),'separate')
bad<-od;bad$interval_seconds<-3600
fail(write_traffic_input(bad,file.path(qa,'bad-interval')),'interval')
bad<-road;bad$flow$road_id[1]<-'missing'
fail(write_traffic_input(bad,file.path(qa,'bad-road')),'[Ii][Dd]|road')
bad<-road;bad$roads$road_id<-1;bad$flow$road_id<-c(1,1)
fail(write_traffic_input(bad,file.path(qa,'numeric-road-ids')),'character')
bad<-od;bad$nodes$node_id<-c(1,17);bad$flows$origin_id<-1;bad$flows$destination_id<-17
fail(write_traffic_input(bad,file.path(qa,'numeric-od-ids')),'character')
bad<-od;bad$flows$time<-'2026-01-01 08:00:00';bad$timezone<-NULL
fail(write_traffic_input(bad,file.path(qa,'unresolved-text-clock')),'timezone')
unlink(qa,recursive=TRUE)
cat('Separate road and OD inputs preserve source attributes, missingness, units and clocks.\n')
