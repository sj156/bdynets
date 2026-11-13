options(bdynets.basemap.auto=FALSE)
library(bdynets)
r<-data.frame(road_id=c('a','b'),region=c('West','East'));r$geometry<-list(rbind(c(10,50),c(10.01,50)),rbind(c(10.01,50),c(10,50)))
f<-data.frame(road_id=c('a','b','a','b'),time=c(0,0,60,60),flow=c(0,NA,2,8))
e<-data.frame(id=c('e1','e2'),time=c(0,65),lon=c(10,10.01),lat=50,region=c('West','East'))
x<-traffic_data(r,f,e,times=c(0,60),interval_seconds=60,unit='records/hour')
# Some source archives keep observation codes as compact raw matrices.
codes <- matrix(as.raw(c(1,0,2,1,0,2)), nrow=2,
                dimnames=list(c("a","b"), c("t1","t2","t3")))
x$provenance <- list(coverage=list(status=codes), raw_file_id=as.raw(c(8,9,3)))
stopifnot(exists('traffic_export',mode='function'))
base<-tempfile('traffic-export-tests-');dir.create(base)
input<-file.path(base,'input.rds');saveRDS(list(schema='trafficmap-input-v1',roads=r,flow=f,events=e,times=c(0,60),interval_seconds=60),input)
out<-file.path(base,'mixed');traffic_export(x,out,index=2,region='East',input_files=c(input=input))
expected<-c('map.png','map.pdf','flow-trend.png','flow-trend.pdf','event-trend.png','event-trend.pdf','selected-frame.rds','summary.csv','trend.csv','provenance.json','inputs.csv','checksums.csv','session-info.txt')
stopifnot(setequal(list.files(out),expected))
sel<-readRDS(file.path(out,'selected-frame.rds'));stopifnot(identical(sel$roads$road_id,'b'),identical(sel$flow$flow,8),identical(sel$events$id,'e2'),identical(sel$scale,x$scale),identical(sel$unit,x$unit),identical(sel$clock,x$clock),identical(sel$title,x$title),identical(sel$time,x$times[2]))
manifest <- jsonlite::read_json(file.path(out,'provenance.json'), simplifyVector=TRUE)
stopifnot(identical(sel$provenance, x$provenance),
          identical(manifest$source$coverage$status, matrix(as.integer(codes), nrow=2)),
          identical(manifest$source$raw_file_id, jsonlite::base64_enc(x$provenance$raw_file_id)))
c<-read.csv(file.path(out,'checksums.csv'));stopifnot(setequal(c$file,setdiff(expected,'checksums.csv')),identical(unname(tools::md5sum(file.path(out,c$file))),c$md5))
i<-read.csv(file.path(out,'inputs.csv'));stopifnot(i$md5==unname(tools::md5sum(input)),!grepl('/',i$file))
fail<-function(expr)inherits(tryCatch(force(expr),error=identity),'error')
stopifnot(fail(traffic_export(x,out)),fail(traffic_export(x,file.path(base,'bad-index'),index=0)),!dir.exists(file.path(base,'bad-index')))
stopifnot(fail(traffic_export(x,file.path(base,'bad-input'),input_files='absent.rds')),!dir.exists(file.path(base,'bad-input')))
for(kind in c('flow','events')) {
 obj<-traffic_data(roads=if(kind=='flow')r,flow=if(kind=='flow')f,events=if(kind=='events')e,times=c(0,60),interval_seconds=60)
 traffic_export(obj,file.path(base,kind),index=2)
 stopifnot(file.exists(file.path(base,kind,'map.pdf')),
  file.exists(file.path(base,kind,paste0(if(kind=='flow')'flow' else 'event','-trend.png'))),
  !file.exists(file.path(base,kind,paste0(if(kind=='flow')'event' else 'flow','-trend.png'))))
}
unlink(base,recursive=TRUE)
cat('Unified exports: files, selected records, frozen scale, regions, checksums, input identities and input combinations passed.\n')
