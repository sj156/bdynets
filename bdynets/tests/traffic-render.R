if(nzchar(Sys.getenv('TRAFFICMAP_SOURCE'))) {
 source(Sys.getenv('TRAFFICMAP_SOURCE'));options(trafficmap.assets=normalizePath('inst/package_assets'))
} else library(bdynets)
r <- data.frame(road_id=c('x','y'),region=c('A','B'))
r$geometry <- list(rbind(c(-118,34),c(-118.1,34.1)),rbind(c(-117,34),c(-117.1,34.1)))
f <- data.frame(road_id=rep(c('x','y'),2),time=rep(c(0,60),each=2),flow=c(0,5,10,NA))
e <- data.frame(time=c(0,60),lon=c(-118,-117),lat=34,region=c('A','B'))
water<-list(type='FeatureCollection',features=list(list(type='Feature',properties=list(kind='water'),geometry=list(type='Polygon',coordinates=list(rbind(c(-118.4,33.9),c(-117,33.9),c(-117,34.2),c(-118.4,33.9)))))))
x<-traffic_data(r,f,e,interval_seconds=60,title='Independent records',basemap=water)
out<-tempfile();dir.create(out)
map<-file.path(out,'map.html');report<-file.path(out,'report.html')
traffic_map(x,map);traffic_report(x,report)
a<-paste(readLines(map,warn=FALSE),collapse='\n');b<-paste(readLines(report,warn=FALSE),collapse='\n')
stopifnot(grepl('Independent records',a,fixed=TRUE),grepl('log1p',a,fixed=TRUE),
 grepl('Preparation',b,fixed=TRUE),grepl('Summary',b,fixed=TRUE),
 grepl('data:image/png;base64,',b,fixed=TRUE),
 !grepl('<script src=',b,fixed=TRUE),length(list.files(out))==2)
grDevices::pdf(file.path(out,'figure.pdf'));plot(x);grDevices::dev.off()
stopifnot(file.info(file.path(out,'figure.pdf'))$size>1000)
large<-x;large$description<-strrep('x',1100000)
traffic_map(large,file.path(out,'large.html'))
stopifnot(file.info(file.path(out,'large.html'))$size>1100000)
unlink(out,recursive=TRUE)
cat('One map/report/static interface renders flow and events without external assets.\n')
inside<-function(n)if(exists(n,mode='function'))get(n,mode='function') else getFromNamespace(n,'bdynets')
plain<-x;decorated<-x
plain$initial<-1L;decorated$initial<-1L
decorated$roads$direction<-c('N','S');decorated$roads$maxVolume<-c(1e12,0)
decorated$roads$lanes<-c(1,99);decorated$roads$color<-c('black','magenta')
stopifnot(identical(inside('.tm_image')(function()plot(plain)),inside('.tm_image')(function()plot(decorated))))
plain$basemap$attribution<-'Independent survey'
html<-inside('.tm_report')(plain)
stopifnot(grepl('Independent survey',html,fixed=TRUE),!grepl('OpenStreetMap|Protomaps',html),
 grepl('<h3>All records</h3>',html,fixed=TRUE),grepl('Flow trend',html,fixed=TRUE),grepl('Event trend',html,fixed=TRUE),
 lengths(regmatches(html,gregexpr('data:image/png;base64,',html,fixed=TRUE)))==3L)
# Inert context attributes leave static output unchanged as well.
base<-plain
base$basemap$features[[1]]$properties$highway<-'motorway'
base$basemap$features[[1]]$properties$population<-1e12
stopifnot(identical(inside('.tm_image')(function()plot(plain)),inside('.tm_image')(function()plot(base))))
cat('Descriptive-field invariance, neutral context and aggregate mixed reports passed.\n')

stopifnot(all(vapply(c("layerRoads","layerEvents","layerContext","layerCasing"),function(id)grepl(id,a,fixed=TRUE),FALSE)))
