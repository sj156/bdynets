traffic_test_artifact <- function() {
  feature <- function(id) list(type='Feature',geometry=list(type='LineString',coordinates=list(c(116,39),c(116.1,39.1))),properties=list(edge_index=id))
  list(schema_version=1L,source='beijing',provenance=list(source_html='fixture'),payload=list(
    network=list(features=list(feature(1),feature(2))),flowByBin=list('0'=list(c(1,.2)),'1'=list(c(1,.8),c(2,.1))),
    durationByBin=list('0'=list(),'1'=list(c(1,2))),timeMeta=list('0'=list(start_min=0),'1'=list(start_min=5)),
    timeBins=c(0,1),bounds=list(c(39,116),c(39.1,116.1)),timeBinMinutes=5,metricKey='saturation',
    congestionStops=list(list(min=0,color='#2c7bb6'),list(min=.2,color='#00a6ca'),list(min=.4,color='#28a745'),list(min=.6,color='#f46d43'),list(min=.8,color='#b2182b'))))
}

test_that('standalone traffic data retains source values and mode semantics', {
  skip_if_not_installed('jsonlite')
  f <- tempfile(fileext='.json.gz'); con <- gzfile(f,'wt')
  writeLines(jsonlite::toJSON(traffic_test_artifact(),auto_unbox=TRUE),con); close(con)
  x <- read_traffic_data(f)
  expect_s3_class(x,'traffic_snapshot')
  a <- traffic_frame(x,1); b <- traffic_frame(x,2)
  expect_equal(a$value,c(.2,0)); expect_equal(a$active,c(TRUE,FALSE))
  expect_equal(b$value,c(.8,.1)); expect_equal(b$color,c('#b2182b','#2c7bb6'))
  expect_equal(traffic_frame(x,2,mode='delta')$value,c(.6,.1))
  expect_equal(traffic_frame(x,2,mode='duration')$value,c(10,0))
  expect_equal(traffic_frame(x,1,mode='delta')$value,a$value)
  expect_equal(x$geometry[[1]],rbind(c(116,39),c(116.1,39.1)))
  expect_error(traffic_frame(x,0),'time_index')
  expect_error(traffic_frame(x,3),'time_index')
  expect_error(traffic_figures(f,'largest'),'Source')
  file <- tempfile(fileext='.pdf'); grDevices::pdf(file)
  old <- graphics::par('mar')
  tryCatch({expect_identical(plot(b),b); expect_identical(graphics::par('mar'),old)},finally=grDevices::dev.off())
  expect_gt(file.info(file)$size,0)
})

test_that('JSON import never evaluates executable JavaScript', {
  skip_if_not_installed('jsonlite')
  expect_error(bdynets:::.traffic_literal('const data = alert(1);','data'),'Missing')
  expect_equal(bdynets:::.traffic_literal(c('const bounds = [[1,2],[3,4]];','const bounds = layer.getBounds();'),'bounds'),list(list(1L,2L),list(3L,4L)))
  expect_error(bdynets:::.traffic_literal(c('const data = 1;','const data = 2;'),'data'),'ambiguous')
})

test_that('LargeST classes retain upper-bound equality and null observations', {
  skip_if_not_installed('jsonlite')
  sensor <- function(id) list(id=id,lat=34,lng=-118,values=as.list(rep(1,288)))
  link <- function(v) list(from='a',to='b',direction='E',coords=list(c(34,-118),c(34.1,-118.1)),values=rep(list(v),288))
  region <- list(sensors=list(sensor('a'),sensor('b')),links=list(link(75),link(NULL)),breaks=c(75,146,252,350,443,544),zoom=9.4,summary=list())
  d <- list(data=list(date='2019-07-31',regions=list(Test=region)),colors=letters[1:7],initialTimeIndex=194)
  x <- bdynets:::.traffic_decode(function(n)d[[n]],'largest',list())
  f <- traffic_frame(x,195)
  expect_equal(f$value,c(75,NA)); expect_equal(f$color,c('a','#94a3b8'))
  expect_equal(f$geometry[[1]],rbind(c(-118,34),c(-118.1,34.1)))
  expect_error(traffic_frame(x,1,mode='delta'),'current')
  expect_error(traffic_frame(x,1,region='absent'),'Unknown')
})

test_that('one-time extraction preserves standalone payloads', {
  skip_if_not_installed('jsonlite')
  payload <- traffic_test_artifact()$payload
  lines <- vapply(names(payload),function(n) paste0('const ',n,' = ',
    jsonlite::toJSON(payload[[n]],auto_unbox=TRUE,digits=NA),';'),'')
  html <- tempfile(fileext='.html'); writeLines(lines,html)
  file <- tempfile(fileext='.json.gz')
  extract_traffic_data(html,'beijing',file)
  a <- read_traffic_data(file); b <- read_traffic_snapshot(html,'beijing')
  expect_equal(a$flow,b$flow); expect_equal(a$geometry,b$geometry)
  expect_equal(traffic_frame(a,2)$value,traffic_frame(b,2)$value)
  expect_error(extract_traffic_data(html,'beijing',file),'new')
})
