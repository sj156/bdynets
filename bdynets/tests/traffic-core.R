if(nzchar(Sys.getenv('TRAFFICMAP_SOURCE'))) source(Sys.getenv('TRAFFICMAP_SOURCE')) else library(bdynets)
fail <- function(e) inherits(tryCatch(force(e),error=identity),'error')
roads <- data.frame(road_id=c('a','b'),region=c('West','East'))
roads$geometry <- list(rbind(c(-118,34),c(-118.1,34.1)),rbind(c(-117,34),c(-117.1,34.1)))
f <- data.frame(road_id=c('b','a','a'),time=c(0,60,0),flow=c(10,0,5))
x <- traffic_data(roads,f,times=c(0,60),interval_seconds=60)
stopifnot(identical(x$values,matrix(c(5,0,10,NA_real_),2)),
 identical(traffic_summary(x,2)$missing,1L),traffic_summary(x,2)$zero==1,
 identical(traffic_trend(x)$mean,c(7.5,0)),
 identical(traffic_trend(x,'East')$mean,c(10,NA_real_)))
s <- flow_scale(c(0,1,10,100,NA))
stopifnot(length(s$breaks)==6,identical(s$palette,c('#16a43a','#7ad151','#f7ea2a','#ffc928','#f5a623','#f36b2b','#f01818')),
 isTRUE(all.equal(log1p(s$breaks),seq(0,log1p(100),length.out=8)[2:7])),
 flow_scale(rep(0,4))$kind=='zero',flow_scale(c(NA_real_,NA_real_))$kind=='missing',fail(flow_scale(-1)))
stopifnot(fail(traffic_data(roads,rbind(f,f[1,]))),
 fail(traffic_data(roads,transform(f,road_id='unknown'))),
 fail(traffic_data(roads,transform(f,flow=-1))),
 fail(traffic_data(roads,f,times=c(0,0))),
 fail(traffic_data(roads,f,times=c(0,60),interval_seconds=120)))
events <- data.frame(time=c(0,60,119),lon=c(-118,-117,-118),lat=34,category=c('A','B','A'))
e <- traffic_data(events=events,times=c(0,60),interval_seconds=60)
stopifnot(identical(traffic_trend(e)$events,c(1L,2L)),nrow(e$roads)==0,nrow(e$events)==3,
 traffic_summary(e,1)$events==1,traffic_summary(e,2)$available==0)
combined <- traffic_data(roads,f,events,times=c(0,60),interval_seconds=60)
stopifnot(traffic_summary(combined,1)$available==2,traffic_summary(combined,1)$events==1,
 identical(combined$scale,x$scale))
stopifnot(fail(traffic_data(events=transform(events,lat=100))),
 fail(traffic_data(events=transform(events,time=120),times=c(0,60),interval_seconds=60)),
 fail(traffic_data(roads,f,events=transform(events,time=as.POSIXct(time,origin='1970-01-01',tz='UTC')),times=c(0,60),interval_seconds=60)),
 fail(traffic_data(roads,f,palette='red')))
abs <- as.POSIXct(c('2026-06-16 00:00:00','2026-06-16 00:01:00'),tz='UTC')
a <- traffic_data(roads,data.frame(road_id=c('a','b'),time=abs,flow=c(1,2)),times=abs,interval_seconds=60)
stopifnot(a$clock=='absolute',identical(a$time_labels,format(abs,tz='UTC',usetz=FALSE)))
bundle <- list(schema='trafficmap-input-v1',roads=roads,flow=f,times=c(0,60),interval_seconds=60)
path <- tempfile(fileext='.rds');saveRDS(bundle,path)
y <- read_traffic(path);stopifnot(identical(y$values,x$values))
bundle$metric_info <- list(palette='red');saveRDS(bundle,path);stopifnot(fail(read_traffic(path)));unlink(path)
cat('Common records, joins, missing values, clocks, events and automatic log scale passed.\n')
# Displayed class limits stay compact while classification retains exact breaks.
labels <- if(exists('.tm_labels'))get('.tm_labels') else getFromNamespace('.tm_labels','bdynets')
pretty <- labels(flow_scale(c(0,17524)))
stopifnot(identical(pretty,c('0 to < 3','3 to < 15','15 to < 65','65 to < 265','265 to < 1,073','1,073 to < 4,338','>= 4,338')))
small <- labels(flow_scale(c(0,.001)))
stopifnot(length(small)==7,!anyDuplicated(small),!any(grepl('e[-+]',small)))
stopifnot(identical(labels(flow_scale(c(0,0))),'0'),length(labels(flow_scale(NA_real_)))==0)
cat('Compact class labels preserve seven distinguishable intervals.\n')
stopifnot(identical(labels(flow_scale(c(0,119.33))),c('0 to < 1','1 to < 3','3 to < 7','7 to < 14','14 to < 30','30 to < 60','>= 60')))
# An omitted unit conveys no physical-unit assumption or numeric conversion.
unknown <- traffic_data(roads,f,times=c(0,60),interval_seconds=60)
explicit <- traffic_data(roads,f,times=c(0,60),interval_seconds=60,unit='observations/interval')
stopifnot(unknown$unit=='unit unspecified',explicit$unit=='observations/interval',
 identical(unknown$values,explicit$values),identical(unknown$scale,explicit$scale))
bundle$metric_info <- NULL
saveRDS(bundle,path)
stopifnot(read_traffic(path)$unit=='unit unspecified')
unlink(path)
cat('Omitted units remain unspecified; declared labels do not alter observations.\n')
# Record scope is explicit and independent of source identifiers or field presence.
rr<-data.frame(road_id=c('west','east'),region=c('Left','Right'))
rr$geometry<-list(rbind(c(10,50),c(10.01,50)),rbind(c(10.01,50),c(10.01,50.01)))
ff<-data.frame(road_id=rep(rr$road_id,3),time=rep(c(0,60,120),each=2),flow=c(0,1,100,200,2,NA))
ee<-data.frame(time=c(0,61,121),lon=c(10,30,10.01),lat=c(50,60,50.01),region=c('Left','','Right'))
z<-traffic_data(rr,ff,ee,times=c(0,60,120),interval_seconds=60)
inside<-function(n)if(exists(n,mode='function'))get(n,mode='function') else getFromNamespace(n,'bdynets')
stopifnot(z$initial==1L,traffic_data(events=ee,times=c(0,60,120),interval_seconds=60)$initial==1L,
 identical(inside('.tm_select')(z,'Left')$events,1L),traffic_summary(z,1,'Left')$events==1,
 traffic_summary(z,2,'Left')$events==0,
 any(rowSums(abs(sweep(inside('.tm_locations')(z),2,c(30,60))))<1e-9),
 all(traffic_data(rr[,setdiff(names(rr),'region')],ff)$roads$region==''))
context<-list(type='FeatureCollection',features=list(),attribution='Independent survey')
zz<-traffic_data(rr,ff,ee,times=c(0,60,120),interval_seconds=60,basemap=context)
stopifnot(zz$basemap$attribution=='Independent survey')
context$attribution<-NULL
stopifnot(traffic_data(rr,ff,basemap=context)$basemap$attribution=='Basemap source unspecified')
context$features<-list(list(type='Feature',properties=list(label='Context caption',name='Ignored',population=1e9,highway='motorway'),
 geometry=list(type='Point',coordinates=c(10,50))))
stopifnot(!fail(traffic_data(rr,ff,basemap=context)))
context$features[[1]]$geometry$coordinates<-c(10,95)
stopifnot(fail(traffic_data(rr,ff,basemap=context)))
cat('First-frame defaults, exact regional scope, mixed locations and caller context semantics passed.\n')
