options(bdynets.basemap.auto=FALSE)
library(bdynets)
fail<-function(e)inherits(tryCatch(force(e),error=identity),'error')
nodes<-data.frame(node_id=c('a','b','c'),longitude=c(-87.64,-87.6401,-87.60),latitude=c(41.88,41.8801,41.90))
flows<-data.frame(origin_id=c('a','a','b','c'),destination_id=c('b','c','c','a'),flow=c(2,4,3,1),time=c('2026-01-05 00:10:00','2026-01-05 06:10:00','2026-01-05 00:10:00','2026-01-05 23:10:00'))
cols<-c(origin='origin_id',destination='destination_id',count='flow',time='time')
out<-tempfile('od-time-');dir.create(out)
a<-aggregate_od_data(nodes,flows,100,flow_columns=cols,timezone='America/Chicago',output_dir=file.path(out,'prepared'))
inmemory<-traffic_od_map(a,hour_window=c(0,3),timezone='America/Chicago',draw=FALSE)
csv<-traffic_od_map(a$files[['nodes']],a$files[['flows']],hour_window=c(0,3),timezone='America/Chicago',draw=FALSE)
stopifnot(inmemory$summary$selected_flow==5,identical(inmemory$summary,csv$summary),identical(inmemory$routes,csv$routes))
saveRDS(a,file.path(out,'prepared.rds'));rds<-traffic_od_map(readRDS(file.path(out,'prepared.rds')),hour_window=c(0,3),timezone='America/Chicago',draw=FALSE)
stopifnot(identical(inmemory$summary,rds$summary))
# Explicit offsets identify instants; unzoned values use the supplied timezone.
offset<-flows;offset$time<-c('2026-01-05T06:10:00Z','2026-01-05T12:10:00+00:00','2026-01-05T00:10:00-0600','2026-01-06T10:40:00+05:30')
b<-aggregate_od_data(nodes,offset,100,flow_columns=cols,timezone='America/Chicago')
stopifnot(identical(a$flows,b$flows))
bad<-flows;bad$time[1]<-'2026-01-05T06:10:00Z trailing'
stopifnot(fail(aggregate_od_data(nodes,bad,100,flow_columns=cols,timezone='America/Chicago')),
 fail(traffic_od_map(nodes,bad,node_columns=c(id='node_id',longitude='longitude',latitude='latitude'),hour_window=c(0,3),timezone='America/Chicago',draw=FALSE)),
 fail(aggregate_od_data(nodes,flows,100,flow_columns=cols,timezone='Unknown/Zone')))
custom<-flows;custom$time<-c('05/01/2026 00:10','05/01/2026 06:10','05/01/2026 00:10','05/01/2026 23:10')
b<-aggregate_od_data(nodes,custom,100,flow_columns=cols,timezone='America/Chicago',time_format='%d/%m/%Y %H:%M')
stopifnot(identical(a$flows,b$flows))
# Absolute interval endpoints may also be supplied as ISO strings.
v<-traffic_od_map(a,start_time='2026-01-05T06:00:00Z',end_time='2026-01-05T09:00:00Z',timezone='America/Chicago',draw=FALSE)
stopifnot(v$summary$selected_flow==5)
unlink(out,recursive=TRUE)
cat('OD CSV/RDS round trips, ISO Z/offset/local clocks, custom formats and interval filters preserve source selections.\n')
