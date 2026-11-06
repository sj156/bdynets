test_that('coordinate acquisition is cached, offline and integrity checked', {
  calls<-0L
  local_mocked_bindings(.bm_request=function(query,endpoint,timeout){
    calls<<-calls+1L
    list(data=list(osm3s=list(timestamp_osm_base='2026-01-01T00:00:00Z'),elements=list(
      list(type='way',id=7,geometry=list(list(lon=10,lat=50),list(lon=10.01,lat=50.01))))),md5='synthetic-unit-response',endpoint=endpoint)
  },.package='bdynets')
  cache<-tempfile();bounds<-c(10,10.02,50,50.02)
  a<-traffic_basemap(bounds=bounds,cache_dir=cache)
  expect_equal(calls,1L);expect_length(a$features,1L);expect_length(list.files(cache),1L)
  b<-traffic_basemap(bounds=bounds,cache_dir=cache,offline=TRUE,timeout=30)
  expect_identical(a,b);expect_equal(calls,1L)
  f<-list.files(cache,full.names=TRUE);bad<-readRDS(f);bad$basemap$features[[1]]$geometry$coordinates[1,1]<-11;saveRDS(bad,f)
  expect_error(traffic_basemap(bounds=bounds,cache_dir=cache,offline=TRUE),'Invalid basemap cache')
  expect_error(traffic_basemap(bounds=bounds,cache_dir=cache,offline=TRUE,refresh=TRUE),'initial acquisition')
  expect_length(traffic_basemap(bounds=bounds,cache_dir=cache,refresh=TRUE)$features,1L)
  expect_equal(calls,2L);unlink(cache,recursive=TRUE)
})
test_that('coordinate types use one generic rule and empty responses remain explicit', {
  local_mocked_bindings(.bm_request=function(query,endpoint,timeout)list(data=list(osm3s=list(timestamp_osm_base='unit'),elements=list()),md5='unit',endpoint=endpoint),.package='bdynets')
  cache<-tempfile()
  nodes<-data.table::data.table(node_id=c('u','v'),longitude=c(10,10.02),latitude=c(50,50.02))
  a<-traffic_basemap(nodes,cache_dir=cache)
  expect_length(a$features,0L);expect_true(is.list(a$provenance$requests))
  expect_identical(a,traffic_basemap(as.matrix(as.data.frame(nodes)[,c('longitude','latitude')]),cache_dir=cache,offline=TRUE))
  expect_error(traffic_basemap(bounds=c(10,9,50,51),cache_dir=cache),'Bounds')
  expect_error(traffic_basemap(data.frame(x=1,y=2),cache_dir=cache),'Coordinate')
  unlink(cache,recursive=TRUE)
})
test_that('regional traffic bounds keep separate spatial scopes', {
  r<-data.frame(road_id=c('a','b'),region=c('West','East'));r$geometry<-list(rbind(c(-118,34),c(-118.01,34.01)),rbind(c(-122,37),c(-122.01,37.01)))
  x<-traffic_data(r,data.frame(road_id=c('a','b'),time=0,flow=1),times=0)
  bounds<-bdynets:::.bm_bounds(x)
  expect_length(bounds,2L);expect_true(all(vapply(bounds,function(b)b[2]-b[1]<1,logical(1))))
})
test_that('rendering obtains missing context from coordinates by default', {
  (function(){
    previous<-options(bdynets.basemap.auto=TRUE);on.exit(options(previous))
    calls<-0L
    local_mocked_bindings(traffic_basemap=function(x=NULL,bounds=NULL,...){calls<<-calls+1L;list(type='FeatureCollection',features=list(),attribution='Synthetic test context',provenance=list(provider='unit'))},.package='bdynets')
    r<-data.frame(road_id='a');r$geometry<-list(rbind(c(10,50),c(10.01,50.01)))
    x<-traffic_data(r,data.frame(road_id='a',time=0,flow=1),times=0)
    expect_null(x$basemap)
    expect_equal(bdynets:::.tm_background(x)$basemap$attribution,'Synthetic test context')
    expect_equal(calls,1L)
    p<-tempfile(fileext='.png')
    traffic_od_map(data.frame(node_id=c('a','b'),longitude=c(10,10.01),latitude=c(50,50.01)),
      data.frame(origin_id='a',destination_id='b',flow=1),node_columns=c(id='node_id',longitude='longitude',latitude='latitude'),
      flow_columns=c(origin='origin_id',destination='destination_id',count='flow'),output_file=p,draw=FALSE)
    expect_equal(calls,2L);expect_true(file.exists(p));unlink(p)
  })()
})
test_that('partial ways are rejected before any cache is committed', {
  local_mocked_bindings(.bm_request=function(query,endpoint,timeout)list(data=list(elements=list(list(type='way',id=123))),md5='partial',endpoint=endpoint),.package='bdynets')
  cache<-tempfile()
  expect_error(traffic_basemap(bounds=c(10,10.01,50,50.01),cache_dir=cache),'geometry')
  expect_false(dir.exists(cache))
})
test_that('alternate endpoints preserve the same query', {
  attempts<-character()
  local_mocked_bindings(.bm_request=function(query,endpoint,timeout){
    attempts<<-c(attempts,query)
    if(length(attempts)==1L)stop('HTTP 502 capacity')
    list(data=list(elements=list()),md5='complete-empty',endpoint=endpoint)
  },.package='bdynets')
  cache<-tempfile()
  a<-traffic_basemap(bounds=c(10,10.01,50,50.01),cache_dir=cache,endpoints=c('https://unit1.invalid','https://unit2.invalid'))
  expect_length(a$features,0L);expect_length(attempts,2L);expect_identical(attempts[1],attempts[2]);unlink(cache,recursive=TRUE)
})
test_that('compressed and plain network responses decode identically', {
  a<-tempfile();b<-tempfile()
  txt<-'{"elements":[],"osm3s":{"timestamp_osm_base":"2026-01-01T00:00:00Z"}}'
  writeLines(txt,a);con<-gzfile(b,'wb');writeLines(txt,con);close(con)
  expect_identical(bdynets:::.bm_response(a),bdynets:::.bm_response(b))
  unlink(c(a,b))
})
test_that('HTTP errors retain service diagnostics and are not parsed as maps', {
  local_mocked_bindings(download.file=function(...)stop('HTTP 502'),.package='utils')
  local_mocked_bindings(curl_fetch_disk=function(url,path,handle){writeLines('All query workers are busy: capacity',path);list(status_code=502L)},.package='curl')
  expect_error(bdynets:::.bm_request('query','https://unit.invalid',5),'HTTP 502.*capacity')
})
test_that('background detail preserves declared small roads and never guesses unknown types', {
  broad<-c(-125,-115,30,40);close<-c(10,10.01,50,50.01)
  major<-bdynets:::.bm_detail(broad,character(),'auto',c(1024,768))
  minor<-bdynets:::.bm_detail(broad,'residential','auto',c(1024,768))
  path<-bdynets:::.bm_detail(broad,'footway','auto',c(1024,768))
  unknown<-bdynets:::.bm_detail(broad,NA_character_,'auto',c(1024,768))
  expect_false('residential'%in%major$classes)
  expect_true('residential'%in%minor$classes);expect_true('footway'%in%path$classes)
  expect_identical(unknown$filter,'["highway"]')
  expect_identical(bdynets:::.bm_detail(close,character(),'auto',c(1024,768))$filter,'["highway"]')
  expect_identical(bdynets:::.bm_detail(broad,'bad"class','auto',c(1024,768))$filter,'["highway"]')
})
test_that('traffic types come from supplied metadata and affect cache identity', {
  queries<-character()
  local_mocked_bindings(.bm_request=function(query,endpoint,timeout){queries<<-c(queries,query);list(data=list(elements=list()),md5='unit',endpoint=endpoint)},.package='bdynets')
  r<-data.frame(road_id=c('r','s'),road_class='residential');r$geometry<-list(rbind(c(-125,30),c(-124.99,30.01)),rbind(c(-115.01,39.99),c(-115,40)))
  x<-traffic_data(r,data.frame(road_id=c('r','s'),time=0,flow=1),times=0)
  cache<-tempfile();a<-traffic_basemap(x,cache_dir=cache)
  expect_match(queries[1],'residential');expect_identical(x$roads$road_class,c('residential','residential'))
  x$roads$road_class<-NULL;b<-traffic_basemap(x,cache_dir=cache)
  expect_match(queries[2],'way["highway"](',fixed=TRUE);expect_length(list.files(cache),2L)
  x$title<-'Unrelated user data';traffic_basemap(x,cache_dir=cache,offline=TRUE)
  expect_length(queries,2L);unlink(cache,recursive=TRUE)
})
test_that('unmapped road-type vocabularies cannot hide local roads', {
  z<-bdynets:::.bm_detail(c(-125,-115,30,40),'local','auto',c(1024,768))
  expect_identical(z$filter,'["highway"]')
})
test_that('unrecognized labels in any vocabulary request complete context safely', {
  for(label in c('local road','小路','1','bad"class'))expect_identical(
    bdynets:::.bm_detail(c(-125,-115,30,40),label,'auto',c(1024,768))$filter,'["highway"]')
})
test_that('HTTP diagnostics include query errors below HTML headers', {
  local_mocked_bindings(curl_fetch_disk=function(url,path,handle){writeLines(c(rep('<head>header</head>',10),'<p>Error: query timed out at line 1</p>'),path);list(status_code=504L)},.package='curl')
  expect_error(bdynets:::.bm_request('query','https://unit.invalid',5),'HTTP 504.*query timed out')
})
test_that('scale types use equivalent indexed literal selections', {
  b<-c(10,10.1,50,50.1)
  q<-bdynets:::.bm_query(b,90,c('motorway','residential','footway'))
  expect_match(q,'way["highway"="motorway"]',fixed=TRUE)
  expect_match(q,'way["highway"="residential"]',fixed=TRUE)
  expect_match(q,'way["highway"="footway"]',fixed=TRUE)
  expect_false(grepl('~',q,fixed=TRUE))
  expect_match(bdynets:::.bm_query(b,90),'way["highway"]',fixed=TRUE)
})
test_that('fully covered scopes are requested once without dropping finer road types', {
  boxes<-list(c(10,10.1,50,50.1),c(10.01,10.09,50.01,50.09))
  all_levels<-list(list(classes=NULL),list(classes=NULL))
  expect_identical(bdynets:::.bm_scopes(boxes,all_levels),1L)
  mixed<-list(list(classes=c('motorway','trunk')),list(classes=NULL))
  expect_identical(bdynets:::.bm_scopes(boxes,mixed),1:2)
  expect_identical(bdynets:::.bm_scopes(rep(boxes[1],2),all_levels),1L)
})
