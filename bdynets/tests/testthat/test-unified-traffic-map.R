road_map_fixture <- function(absolute = FALSE) {
  roads <- data.frame(road_id = c("a", "b"), stringsAsFactors = FALSE)
  roads$geometry <- list(
    rbind(c(10.00, 50.00), c(10.01, 50.00)),
    rbind(c(10.01, 50.00), c(10.02, 50.01)))
  times <- if (absolute) {
    as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + c(0, 60)
  } else c(0, 60)
  flow <- data.frame(road_id = rep(c("a", "b"), 2),
    time = rep(times, each = 2), flow = c(0, 5, 10, NA))
  list(roads = roads, flow = flow, times = times)
}

od_map_fixture <- function() {
  start <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  nodes <- data.frame(node_id = c("a", "b", "c"),
    longitude = c(10.00, 10.01, 10.02),
    latitude = c(50.00, 50.01, 50.00))
  od <- data.frame(origin_id = c("a", "a", "b"),
    destination_id = c("b", "b", "c"),
    time = start + c(10, 70, 20), flow = c(1L, 2L, 3L))
  list(nodes = nodes, od = od, start = start)
}

test_that("traffic_map renders a standard road input and preserves missing flow", {
  old <- options(bdynets.basemap.auto = FALSE); on.exit(options(old), add = TRUE)
  x <- road_map_fixture()
  input <- traffic_input(roads = x$roads, flow = x$flow, times = x$times,
    interval_seconds = 60, unit = "vehicles/hour")
  result <- traffic_map(input, index = 2, basemap = NULL, draw = FALSE)
  expect_identical(result$type, "flow")
  expect_equal(result$road_summary$missing, 1)
  expect_equal(result$road_summary$available, 1)

  image <- tempfile(fileext = ".png")
  on.exit(unlink(image), add = TRUE)
  written <- traffic_map(input, index = 1, basemap = NULL,
    output_file = image, draw = FALSE, width = 5, height = 4, dpi = 72)
  expect_true(file.exists(image))
  expect_gt(unname(file.info(image)$size), 500)
  expect_identical(written$output_file, normalizePath(image))
})

test_that("traffic_map filters and aggregates standard OD input", {
  old <- options(bdynets.basemap.auto = FALSE); on.exit(options(old), add = TRUE)
  x <- od_map_fixture()
  input <- traffic_input(nodes = x$nodes, od = x$od, timezone = "UTC",
    od_time_kind = "instant", title = "OD fixture")
  result <- traffic_map(input, start_time = x$start, end_time = x$start + 60,
    basemap = NULL, draw = FALSE)
  expect_equal(result$summary$selected_flow, 4)
  expect_equal(result$summary$displayed_pairs, 2)
  expect_equal(sum(result$summary$flow_classes), 2)
})

test_that("traffic_map overlays OD selected by an absolute road frame", {
  old <- options(bdynets.basemap.auto = FALSE); on.exit(options(old), add = TRUE)
  road <- road_map_fixture(absolute = TRUE)
  od <- od_map_fixture()
  input <- traffic_input(roads = road$roads, flow = road$flow,
    nodes = od$nodes, od = od$od, times = road$times,
    interval_seconds = 60, unit = "vehicles/hour", timezone = "UTC",
    od_time_kind = "instant", title = "Mixed fixture")
  image <- tempfile(fileext = ".png")
  on.exit(unlink(image), add = TRUE)
  result <- traffic_map(input, index = 1, basemap = NULL,
    output_file = image, draw = FALSE, width = 6, height = 4, dpi = 72)
  expect_identical(result$type, "combined")
  expect_equal(result$od$summary$selected_flow, 4)
  expect_true(file.exists(image))
  expect_gt(unname(file.info(image)$size), 500)
})

test_that("traffic_map rejects invalid standard input and layer selections", {
  expect_error(traffic_map(list(schema = "wrong"), draw = FALSE),
    "bdynets-traffic-input-v1")
  x <- road_map_fixture()
  input <- traffic_input(roads = x$roads, flow = x$flow, times = x$times,
    interval_seconds = 60)
  expect_error(traffic_map(input, layers = "unknown", draw = FALSE), "layers")
  expect_error(traffic_map(input, index = 3, basemap = NULL, draw = FALSE),
    "frame index")
})

mixed_map_fixture <- function(untimed = FALSE, sparse = FALSE) {
  road <- road_map_fixture(absolute = TRUE); od <- od_map_fixture()
  if (sparse) od$od <- od$od[od$od$time < od$start + 60, , drop = FALSE]
  if (untimed) od$od$time <- NULL
  traffic_input(roads=road$roads, flow=road$flow, nodes=od$nodes, od=od$od,
    times=road$times, interval_seconds=60, unit='vehicles/hour', timezone='UTC',
    od_time_kind=if(untimed)'untimed' else 'instant')
}

test_that('sparse mixed windows preserve roads without imputing OD flow', {
  input <- mixed_map_fixture(sparse=TRUE); original <- input
  result <- traffic_map(input,index=2,basemap=NULL,draw=FALSE)
  expect_identical(result$type,'combined')
  expect_identical(result$od$status,'no_records')
  expect_identical(result$od$summary$selected_records,0L)
  expect_true(is.na(result$od$summary$selected_flow))
  expect_true(is.na(result$od$summary$displayed_flow))
  expect_equal(nrow(result$od$routes),0)
  expect_equal(result$road_summary$available,1)
  expect_identical(input,original)
  file <- tempfile(fileext='.png');on.exit(unlink(file),add=TRUE)
  expect_silent(traffic_map(input,index=2,basemap=NULL,output_file=file,draw=FALSE,width=6,height=4,dpi=72))
  expect_gt(file.info(file)$size,500)
})

test_that('untimed mixed OD remains untimed and is never assigned a road clock', {
  input <- mixed_map_fixture(untimed=TRUE); original <- input
  first <- traffic_map(input,index=1,basemap=NULL,draw=FALSE)
  second <- traffic_map(input,index=2,basemap=NULL,draw=FALSE)
  expect_identical(first$od$time_alignment,'untimed')
  expect_identical(second$od$time_alignment,'untimed')
  expect_null(first$od$summary$time_window)
  expect_equal(first$od$summary$selected_flow,sum(input$od$flow))
  expect_identical(first$od$routes,second$od$routes)
  expect_identical(input,original)
  expect_error(traffic_map(input,start_time=input$times[1],end_time=input$times[2],basemap=NULL,draw=FALSE),'[Uu]ntimed')
})

map_payload <- function(path) {
  text <- paste(readLines(path,warn=FALSE),collapse='\n')
  found <- regmatches(text,regexec('<script type="application/octet-stream" id="records">([^<]+)</script>',text))[[1]]
  jsonlite::fromJSON(rawToChar(memDecompress(jsonlite::base64_dec(found[2]),'gzip')))
}

test_that('legacy HTML named and positional calls retain the requested measurement', {
  old <- options(bdynets.basemap.auto=FALSE);on.exit(options(old),add=TRUE)
  x <- road_map_fixture(); canonical <- traffic_data(x$roads,x$flow,times=x$times,interval_seconds=60)
  files <- replicate(3,tempfile(fileext='.html'));on.exit(unlink(files),add=TRUE)
  expect_silent(traffic_map(x=canonical,file=files[1]))
  expect_silent(traffic_map(canonical,files[2],'change'))
  expect_identical(map_payload(files[2])$measurement$mode,'change')
  expect_silent(traffic_map(canonical,files[3],'change',1L))
  expect_identical(map_payload(files[3])$measurement$reference,1L)
  expect_error(traffic_map(canonical,x=canonical,file=tempfile(fileext='.html')),'one|both|only')
})

test_that('standard road objects and RDS paths share HTML conversion', {
  old <- options(bdynets.basemap.auto=FALSE);on.exit(options(old),add=TRUE)
  x <- road_map_fixture();input <- traffic_input(roads=x$roads,flow=x$flow,times=x$times,interval_seconds=60)
  rds<-tempfile(fileext='.rds');a<-tempfile(fileext='.html');b<-tempfile(fileext='.html')
  on.exit(unlink(c(rds,a,b)),add=TRUE);saveRDS(input,rds)
  expect_silent(traffic_map(input,file=a));expect_silent(traffic_map(rds,file=b))
  expect_identical(map_payload(a),map_payload(b))
  expect_silent(traffic_map_html(input,a))
})

test_that('OD uses an embedded basemap unless explicitly disabled', {
  x<-od_map_fixture();base<-list(type='FeatureCollection',features=list(),attribution='Provided context')
  input<-traffic_input(nodes=x$nodes,od=x$od,timezone='UTC',basemap=base,od_time_kind='instant')
  result<-traffic_map(input,draw=FALSE)
  expect_identical(result$basemap,base)
  expect_null(traffic_map(input,basemap=NULL,draw=FALSE)$basemap)
})

test_that('custom road legend labels are actually written to the figure', {
  x<-road_map_fixture();input<-traffic_input(roads=x$roads,flow=x$flow,times=x$times,interval_seconds=60)
  file<-tempfile(fileext='.pdf');grDevices::pdf(file,compress=FALSE,width=8,height=6,useKerning=FALSE)
  tryCatch(traffic_map(input,basemap=NULL,flow_style=list(labels=paste0('CUSTOM',1:7))),finally=grDevices::dev.off())
  bytes<-readBin(file,'raw',n=file.info(file)$size);unlink(file)
  for(label in paste0('CUSTOM',1:7))expect_gt(length(grepRaw(charToRaw(label),bytes,fixed=TRUE)),0)
  expect_error(traffic_map(input,basemap=NULL,flow_style=list(labels='incomplete'),draw=FALSE),'labels|palette')
})

test_that('unified road drawing restores graphics state', {
  x<-road_map_fixture();input<-traffic_input(roads=x$roads,flow=x$flow,times=x$times,interval_seconds=60)
  file<-tempfile(fileext='.pdf');on.exit(unlink(file),add=TRUE);grDevices::pdf(file)
  on.exit(grDevices::dev.off(),add=TRUE);before<-graphics::par(c('mar','xpd'))
  traffic_map(input,basemap=NULL)
  expect_identical(graphics::par(c('mar','xpd')),before)
})


test_that('custom class boundaries and right closure reach road colours and legend', {
  x<-road_map_fixture();x$flow$flow[1:2]<-c(5.5,6)
  input<-traffic_input(roads=x$roads,flow=x$flow,times=x$times,interval_seconds=60)
  for(right in c(FALSE,TRUE)) {
    seen<-NULL
    local_mocked_bindings(.tm_map_restore_road_window=function(road,region){seen<<-road},.package='bdynets')
    file<-tempfile(fileext='.pdf');grDevices::pdf(file)
    tryCatch(traffic_map(input,basemap=NULL,flow_style=list(breaks=5.5,palette=c('green','red'),right=right)),finally=grDevices::dev.off())
    unlink(file)
    expect_identical(bdynets:::.tm_colors(c(5.5,6),seen$scale),if(right)c('green','red') else c('red','red'))
    expect_identical(bdynets:::.tm_labels(seen$scale),if(right)c('0 to <= 5.5','> 5.5') else c('0 to < 5.5','>= 5.5'))
  }
})

test_that('mixed loop-only and observed-zero OD preserve their actual values', {
  input<-mixed_map_fixture();input$od<-input$od[1,,drop=FALSE]
  input$od$destination_id<-input$od$origin_id
  result<-traffic_map(input,basemap=NULL,draw=FALSE)
  expect_identical(result$od$status,'no_routes')
  expect_equal(result$od$summary$selected_flow,1)
  expect_equal(result$od$summary$within_node_flow,1)
  expect_equal(nrow(result$od$routes),0)
  input$od$destination_id<-'b';input$od$flow<-0
  result<-traffic_map(input,basemap=NULL,draw=FALSE)
  expect_identical(result$od$status,'no_routes')
  expect_equal(result$od$summary$selected_flow,0)
  expect_equal(nrow(input$od),1)
  expect_equal(input$od$flow,0)
  input$od$destination_id<-'unknown'
  expect_error(traffic_map(input,basemap=NULL,draw=FALSE),'node|[Uu]nknown|match|identifier')
})

od_events_fixture <- function() {
  z<-od_map_fixture()
  events<-data.frame(time=z$start+c(30,90),lon=c(10.05,11),lat=c(50.05,51))
  traffic_input(nodes=z$nodes,od=z$od,events=events,times=z$start+c(0,60),
    interval_seconds=60,timezone='UTC',od_time_kind='instant')
}

test_that('OD with events and no roads fits the selected nodes and events together', {
  input<-od_events_fixture();before<-input;z<-od_map_fixture();viewport<-NULL
  context<-bdynets:::.tm_static_context
  local_mocked_bindings(.tm_static_context=function(base,limits){viewport<<-graphics::par('usr');context(base,limits)},.package='bdynets')
  image<-tempfile(fileext='.png');on.exit(unlink(image),add=TRUE)
  result<-traffic_map(input,start_time=z$start,end_time=z$start+60,basemap=NULL,
    draw=FALSE,output_file=image,width=6,height=4,dpi=72)
  expect_true(is.list(result$event_overlay))
  expect_equal(result$summary$selected_flow,4)
  if(is.list(result$event_overlay)) {
    expect_equal(nrow(result$event_overlay$events),1)
    expect_equal(result$event_overlay$events$lon,10.05)
  }
  points<-bdynets:::.tm_project(rbind(as.matrix(z$nodes[,c('longitude','latitude')]),c(10.05,50.05)))
  expect_true(all(points[,1]>=viewport[1]&points[,1]<=viewport[2]&points[,2]>=viewport[3]&points[,2]<=viewport[4]))
  expect_identical(input,before)
  expect_gt(file.info(image)$size,500)
})

test_that('OD data obey explicit clipping while annotations retain their margin', {
  z<-od_map_fixture();input<-traffic_input(nodes=z$nodes,od=z$od,timezone='UTC',od_time_kind='instant')
  clipping<-NULL;points<-graphics::points
  local_mocked_bindings(points=function(...){clipping<<-graphics::par('xpd');points(...)},.package='graphics')
  image<-tempfile(fileext='.png');on.exit(unlink(image),add=TRUE)
  traffic_map(input,basemap=NULL,bounds=c(10,10.005,50,50.005),
    output_file=image,draw=FALSE,width=6,height=4,dpi=72)
  expect_identical(clipping,FALSE)
})

test_that('pure OD empty windows report unknown totals and still render known locations', {
  z<-od_map_fixture();input<-traffic_input(nodes=z$nodes,od=z$od,timezone='UTC',od_time_kind='instant');before<-input
  image<-tempfile(fileext='.png');on.exit(unlink(image),add=TRUE)
  result<-traffic_map(input,start_time=z$start+3600,end_time=z$start+7200,basemap=NULL,
    output_file=image,draw=FALSE,width=6,height=4,dpi=72)
  expect_identical(result$status,'no_records')
  expect_identical(result$summary$selected_records,0L)
  expect_true(is.na(result$summary$selected_flow))
  expect_true(is.na(result$summary$displayed_flow))
  expect_equal(nrow(result$routes),0)
  expect_equal(nrow(result$nodes),nrow(input$nodes))
  expect_identical(input,before)
  expect_gt(file.info(image)$size,500)
})

test_that('pure OD recorded zeros and loop-only records remain actual observations', {
  z<-od_map_fixture();od<-z$od[1,,drop=FALSE];od$time<-NULL;od$flow<-0L
  input<-traffic_input(nodes=z$nodes,od=od);before<-input
  for(format in c('png','pdf')) {
    image<-tempfile(fileext=paste0('.',format))
    result<-traffic_map(input,basemap=NULL,output_file=image,draw=FALSE,width=6,height=4,dpi=72)
    expect_identical(result$status,'no_routes')
    expect_equal(result$summary$selected_flow,0)
    expect_equal(result$summary$selected_records,1)
    expect_equal(nrow(result$routes),0)
    expect_gt(file.info(image)$size,500);unlink(image)
  }
  expect_identical(input,before)
  input$od$destination_id<-input$od$origin_id;input$od$flow<-2L
  result<-traffic_map(input,basemap=NULL,draw=FALSE)
  expect_identical(result$status,'no_routes')
  expect_equal(result$summary$selected_flow,2)
  expect_equal(result$summary$within_node_flow,2)
  expect_equal(nrow(result$routes),0)
})

test_that('OD plus events handles a window with no OD but real event observations', {
  input<-od_events_fixture();z<-od_map_fixture();input$od<-input$od[input$od$time<z$start+60,,drop=FALSE]
  image<-tempfile(fileext='.png');on.exit(unlink(image),add=TRUE)
  result<-traffic_map(input,start_time=z$start+60,end_time=z$start+120,basemap=NULL,
    output_file=image,draw=FALSE,width=6,height=4,dpi=72)
  expect_identical(result$status,'no_records')
  expect_true(is.na(result$summary$selected_flow))
  expect_equal(nrow(result$event_overlay$events),1)
  expect_equal(result$event_overlay$events$lon,11)
  expect_gt(file.info(image)$size,500)
})

test_that('empty OD handling never absorbs invalid style, endpoint or timestamp errors', {
  z<-od_map_fixture();input<-traffic_input(nodes=z$nodes,od=z$od,timezone='UTC',od_time_kind='instant')
  input$od$destination_id[1]<-'unknown'
  expect_error(traffic_map(input,basemap=NULL,draw=FALSE),'endpoint|node|match')
  input$od<-z$od;input$od$time<-rep('not-a-time',nrow(input$od))
  expect_error(traffic_map(input,basemap=NULL,draw=FALSE),'timestamp|time')
  input$od<-z$od;input$od$flow<-0
  expect_error(traffic_map(input,basemap=NULL,draw=FALSE,od_style=list(flow_breaks=c(3,1))),'increasing|break')
})

test_that('an explicitly empty road table does not take over an OD event view', {
  input<-od_events_fixture()
  input$roads<-data.frame(road_id=character());input$roads$geometry<-list()
  input$flow<-data.frame(road_id=character(),time=as.POSIXct(character(),tz='UTC'),flow=numeric())
  result<-traffic_map(input,basemap=NULL,draw=FALSE)
  expect_true(is.list(result$event_overlay))
  expect_equal(result$summary$selected_flow,sum(input$od$flow))
})

test_that('loop inclusion does not turn recorded-zero OD into a normal route map', {
  z<-od_map_fixture();od<-z$od;od$flow<-0
  input<-traffic_input(nodes=z$nodes,od=od,timezone='UTC',od_time_kind='instant')
  result<-traffic_map(input,include_loops=TRUE,basemap=NULL,draw=FALSE)
  expect_identical(result$status,'no_routes')
  expect_equal(result$summary$selected_flow,0)
  expect_equal(nrow(result$routes),0)
})

test_that('OD event composition never assigns an absolute date to numeric event clocks', {
  input<-od_events_fixture();input$times<-c(0,60);input$events$time<-c(30,90);original<-input
  result<-traffic_map(input,basemap=NULL,draw=FALSE)
  expect_identical(result$event_overlay$events$time,input$events$time)
  expect_identical(input,original)
  z<-od_map_fixture()
  expect_error(traffic_map(input,start_time=z$start,end_time=z$start+60,basemap=NULL,draw=FALSE),'aligned|numeric source clocks')
})

test_that('OD with event composition is identical through an object and its RDS path', {
  input<-od_events_fixture();rds<-tempfile(fileext='.rds');saveRDS(input,rds)
  a<-tempfile(fileext='.png');b<-tempfile(fileext='.png');on.exit(unlink(c(rds,a,b)),add=TRUE)
  first<-traffic_map(input,basemap=NULL,output_file=a,draw=FALSE,width=6,height=4,dpi=72)
  second<-traffic_map(rds,basemap=NULL,output_file=b,draw=FALSE,width=6,height=4,dpi=72)
  expect_identical(unname(tools::md5sum(a)),unname(tools::md5sum(b)))
  expect_identical(first$routes,second$routes)
  expect_identical(first$event_overlay,second$event_overlay)
})

test_that('a mixed OD legend does not cover the north-arrow label', {
  input<-mixed_map_fixture();rect<-NULL;north<-NULL;legend<-graphics::legend
  local_mocked_bindings(legend=function(...){
    args<-list(...);value<-legend(...)
    if(identical(args$title,'Trips per directed OD pair')) {
      rect<<-value$rect;usr<-graphics::par('usr')
      north<<-c(usr[2]-.055*diff(usr[1:2]),usr[4]-.025*diff(usr[3:4]))
    }
    value
  },.package='graphics')
  file<-tempfile(fileext='.png');on.exit(unlink(file),add=TRUE)
  traffic_map(input,basemap=NULL,output_file=file,draw=FALSE,width=6,height=4,dpi=72)
  expect_false(north[1]>=rect$left&&north[1]<=rect$left+rect$w&&north[2]<=rect$top&&north[2]>=rect$top-rect$h)
})

test_that('mixed and standalone OD share no-route status when loops are included', {
  input<-mixed_map_fixture();input$od$flow<-0
  mixed<-traffic_map(input,basemap=NULL,include_loops=TRUE,draw=FALSE)
  expect_identical(mixed$od$status,'no_routes')
  expect_equal(mixed$od$summary$selected_flow,0)
})

test_that('standalone OD results remain invisible as documented', {
  z<-od_map_fixture();input<-traffic_input(nodes=z$nodes,od=z$od,timezone='UTC',od_time_kind='instant')
  expect_false(withVisible(traffic_map(input,basemap=NULL,draw=FALSE))$visible)
  expect_false(withVisible(traffic_map(input,start_time=z$start+3600,end_time=z$start+7200,basemap=NULL,draw=FALSE))$visible)
})

test_that('loop-inclusive pure OD timestamp filters do not require an event clock', {
  z<-od_map_fixture();input<-traffic_input(nodes=z$nodes,od=z$od,timezone='UTC',od_time_kind='instant')
  result<-traffic_map(input,start_time=z$start,end_time=z$start+60,include_loops=TRUE,basemap=NULL,draw=FALSE)
  expect_equal(result$summary$selected_flow,4)
})
