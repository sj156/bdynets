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
x<-traffic_input(nodes=nodes,od=od,provenance=list(source='test records'))
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
stopifnot(identical(x$nodes,y$nodes),identical(x$od,y$od),length(y$provenance$preparation_sources$sha256)==1L)
fail(write_traffic_input(x,out,source_files=raw),'exist')
expected<-c('input.rds','nodes.csv','od.csv','metadata.json','data-dictionary.csv','validation.csv','source-files.csv','checksums.csv')
stopifnot(setequal(list.files(out),expected))
f<-read.csv(file.path(out,'od.csv'),colClasses='character',check.names=FALSE)
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
zeros<-traffic_input(nodes=data.frame(node_id=c('001','002','\\N'),longitude=1:3,latitude=1:3),
 od=data.frame(ride_id=c('a','b'),origin_id=c('001','\\N'),destination_id=c('002','001'),
 time=as.POSIXct(c(1767247200.125,1767247200.5),origin='1970-01-01',tz='America/Chicago'),flow=c(0L,1L)))
write_traffic_input(zeros,file.path(qa,'precision'))
meta<-jsonlite::read_json(file.path(qa,'precision','metadata.json'),simplifyVector=TRUE)
actual<-read.csv(file.path(qa,'precision','od.csv'),colClasses='character',na.strings=meta$csv$missing)
stopifnot(identical(actual$origin_id,zeros$od$origin_id),identical(as.numeric(actual$time),as.numeric(zeros$od$time)),
 identical(as.numeric(actual$flow),as.numeric(zeros$od$flow)))
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
