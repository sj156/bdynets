test_that("prepared OD routes, counts and the empty-window extent agree", {
  nodes <- data.frame(node_id = c("A", "B"), longitude = c(-87.65, -87.63),
    latitude = c(41.88, 41.89))
  routes <- data.frame(origin_id = c("A", "B", "A"), destination_id = c("B", "A", "A"),
    origin_longitude = nodes$longitude[c(1, 2, 1)],
    origin_latitude = nodes$latitude[c(1, 2, 1)],
    destination_longitude = nodes$longitude[c(2, 1, 1)],
    destination_latitude = nodes$latitude[c(2, 1, 1)], flow = c(4, 0, 2))
  routes$origin_id <- factor(routes$origin_id, levels = c("A", "B", "unused"))
  routes$destination_id <- factor(routes$destination_id, levels = c("B", "A"))
  frame <- structure(list(nodes = nodes, all_nodes = nodes, routes = routes,
    selected_flow = 6, within_node_flow = 2, selected_records = 3L,
    start_time = 0, end_time = 300, timezone = "UTC"), class = "bdynets_od_pairs")
  high <- frame
  high$routes$flow[1L] <- 1200
  high$selected_flow <- 1202
  high$start_time <- 86400
  high$end_time <- 86700
  zero <- frame
  zero$routes <- routes[2L, , drop = FALSE]
  zero$selected_flow <- zero$within_node_flow <- 0
  zero$selected_records <- 1L
  zero$start_time <- 86700
  zero$end_time <- 87000
  empty <- zero
  empty$nodes <- nodes[FALSE, ]
  empty$routes <- routes[FALSE, ]
  empty$selected_records <- 0L
  empty$status <- "no_records"
  empty$start_time <- 87000
  empty$end_time <- 87300
  frames <- list(frame, high, zero, empty)
  rds <- tempfile(fileext = ".rds")
  png <- tempfile(fileext = ".png")
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(rds, png, pdf)), add = TRUE)
  saveRDS(frames, rds)

  low <- traffic_od_map(rds, index = 1, draw = FALSE)
  expect_equal(low$summary$displayed_pairs, 1L)
  expect_equal(low$summary$nonloop_flow, 4)
  expect_equal(low$summary$displayed_flow, 4)
  high_result <- traffic_od_map(frames, time_label = "1970-01-02 00:00", draw = FALSE)
  expect_identical(high_result$index, 2L)
  expect_equal(high_result$summary$displayed_flow, 1200)
  expect_identical(names(low$summary$flow_classes), names(high_result$summary$flow_classes))
  expect_lt(which(low$summary$flow_classes > 0), which(high_result$summary$flow_classes > 0))
  expect_error(traffic_od_map(frames, time_label = "00:00", draw = FALSE), "exactly one")
  expect_error(traffic_od_map(frames, index = 2, time_label = "00:00", draw = FALSE), "not both")

  zero_result <- traffic_od_map(frames, index = 3, draw = FALSE, output_file = png)
  empty_result <- traffic_od_map(frames, time_label = "00:10", draw = FALSE, output_file = pdf)
  expect_identical(zero_result$summary$selected_records, 1L)
  expect_equal(zero_result$summary$selected_flow, 0)
  expect_identical(empty_result$status, "no_records")
  expect_identical(empty_result$summary$selected_records, 0L)
  expect_true(all(file.info(c(png, pdf))$size > 0))
  expect_equal(traffic_od_map(frame, draw = FALSE)$summary$displayed_flow, 4)
})

test_that("Flow and Speed select prepared starts and use separate static interfaces", {
  roads <- data.frame(road_id = "A", region = "West")
  roads$geometry <- list(rbind(c(-87.65, 41.88), c(-87.63, 41.89)))
  flow <- structure(list(roads = roads, input_type = "flow", unit = "vehicles/hour",
    values = matrix(c(6, 12)), times = c(28800, 29100), interval_seconds = c(300, 300),
    clock = "source_clock"), class = "trafficmap_data")
  speed <- flow
  speed$input_type <- "speed"
  speed$unit <- "mph"
  speed$values[, 1L] <- c(35, 65)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  expect_identical(traffic_flow_map(flow, time_label = "08:05", draw = FALSE)$index, 2L)
  result <- traffic_speed_map(speed, time_label = "08:05", draw = FALSE, output_file = file)
  expect_identical(result$index, 2L)
  expect_identical(result$type, "speed")
  expect_identical(result$data$scale$breaks, c(35, 40, 45, 50, 55, 60))
  expect_true(file.info(file)$size > 0)
  expect_error(traffic_flow_map(speed, draw = FALSE), "prepared Flow")
  expect_error(traffic_speed_map(flow, draw = FALSE), "prepared Speed")
})

test_that("static full extent includes a distant road without changing the default", {
  roads <- data.frame(road_id = letters[1:5])
  roads$geometry <- lapply(c(-87.64, -87.63, -87.62, -87.61, -71.06),
    function(x) {
      segment_span <- if (x == -71.06) .5 else .001
      rbind(c(x, 41.88), c(x + segment_span, 41.881))
    })
  input <- structure(list(roads = roads, input_type = "flow", unit = "vehicles/hour",
    values = matrix(c(6, 7, 11, 12, 1), nrow = 1L), times = 0,
    interval_seconds = 300, clock = "source_clock"), class = "trafficmap_data")
  files <- tempfile(paste0("extent-", 1:4, "-"), fileext = ".png")
  on.exit(unlink(files), add = TRUE)
  render <- function(value, index, full) {
    traffic_flow_map(value, output_file = files[index], draw = FALSE,
      full_extent = full, width = 5, height = 4, dpi = 90)
  }
  render(input, 1L, FALSE)
  render(input, 2L, TRUE)
  missing <- input
  missing$values[1L, 5L] <- NA_real_
  render(missing, 3L, FALSE)
  render(missing, 4L, TRUE)
  checksums <- unname(tools::md5sum(files))
  expect_identical(checksums[1L], checksums[3L])
  expect_false(identical(checksums[2L], checksums[4L]))
})

test_that("road frame starts stay exact at second and fractional precision", {
  roads <- data.frame(road_id = "A")
  roads$geometry <- list(rbind(c(-87.65, 41.88), c(-87.63, 41.89)))
  road <- structure(list(roads = roads, input_type = "flow", unit = "vehicles/hour",
    values = matrix(6), times = 28830.125, interval_seconds = 25,
    clock = "source_clock"), class = "trafficmap_data")
  expect_error(traffic_flow_map(road, time_label = "08:00", draw = FALSE), "exactly one")
  expect_error(traffic_flow_map(road, time_label = "08:00:30", draw = FALSE), "exactly one")
  expect_identical(traffic_flow_map(road, time_label = "08:00:30.125", draw = FALSE)$index, 1L)

  road$input_type <- "speed"
  road$unit <- "mph"
  road$times <- as.numeric(as.POSIXct("2026-01-01 08:00:30", tz = "UTC")) + 0.125
  road$time_label_format <- "%H:%M"
  expect_error(traffic_speed_map(road, time_label = "08:00", draw = FALSE), "exactly one")
  result <- traffic_speed_map(road, time_label = "2026-01-01 08:00:30.125", draw = FALSE)
  expect_identical(result$index, 1L)
  expect_identical(result$time_label, "08:00")

  road$times <- as.numeric(as.POSIXct("2026-01-01 08:00:00", tz = "UTC"))
  expect_identical(traffic_speed_map(road, time_label = "08:00", draw = FALSE)$index, 1L)
  expect_identical(traffic_speed_map(road, time_label = "2026-01-01 08:00", draw = FALSE)$index, 1L)

  road$clock <- "absolute"
  road$time_label_format <- NULL
  road$times <- road$times + 30.1
  result <- traffic_speed_map(road, index = 1L, draw = FALSE)
  expect_identical(result$time_label, "2026-01-01 08:00:30.1")
  expect_identical(traffic_speed_map(road, time_label = result$time_label, draw = FALSE)$index, 1L)

})

test_that("road and OD starts support exact calendar fractions and repeated wall times", {
  nodes <- data.frame(node_id = c("A", "B"), longitude = c(-87.65, -87.63),
    latitude = c(41.88, 41.89))
  routes <- data.frame(origin_id = "A", destination_id = "B", flow = 8L,
    origin_longitude = -87.65, origin_latitude = 41.88,
    destination_longitude = -87.63, destination_latitude = 41.89)
  starts <- as.POSIXct(c("2026-11-01 06:30:00", "2026-11-01 07:30:00"), tz = "UTC") + 0.125
  zone <- "America/Chicago"
  roads <- data.frame(road_id = "A")
  roads$geometry <- list(rbind(c(-87.65, 41.88), c(-87.63, 41.89)))
  flow <- structure(list(roads = roads, input_type = "flow", unit = "vehicles/hour",
    values = matrix(c(6, 12)), times = as.numeric(starts), interval_seconds = c(300, 300),
    clock = "absolute", timezone = zone, time_label_format = "%H:%M"), class = "trafficmap_data")
  od <- lapply(seq_along(starts), function(i) structure(list(nodes = nodes, all_nodes = nodes,
    routes = routes, selected_flow = 8L, within_node_flow = 0L, selected_records = 1L,
    start_time = starts[i], end_time = starts[i] + 300, timezone = zone), class = "bdynets_od_pairs"))
  for (plot in list(function(time) traffic_flow_map(flow, time_label = time, draw = FALSE),
                    function(time) traffic_od_map(od, time_label = time, draw = FALSE))) {
    expect_error(plot("01:30"), "exactly one")
    expect_error(plot("2026-11-01 01:30:00.125"), "exactly one")
    expect_identical(plot("2026-11-01 01:30:00.125 -0500")$index, 1L)
    expect_identical(plot("2026-11-01 01:30:00.125 -0600")$index, 2L)
  }
  expect_identical(traffic_od_map(od[1L], time_label = "01:30:00.125", draw = FALSE)$index, 1L)
  fractional <- od[[1L]]
  fractional$start_time <- floor(as.numeric(starts[1L])) + 0.1
  fractional$end_time <- fractional$start_time + 300
  actual <- traffic_od_map(fractional, time_label = "2026-11-01 01:30:00.1", draw = FALSE)
  expect_identical(actual$time_label, "2026-11-01 01:30:00.1 -0500 to 01:35:00.1 -0500")
  pooled <- od[[1L]]
  pooled$start_time <- pooled$end_time <- NULL
  pooled$hour_window <- c(8L, 9L)
  expect_identical(traffic_od_map(pooled, index = 1L, draw = FALSE)$time_label,
    "all dates pooled over 08:00-09:00 (America/Chicago)")
})

test_that("road padding keeps every vertex and event in a full-extent plot", {
  roads <- data.frame(road_id = "one")
  roads$geometry <- list(rbind(c(-87.65, 41.87), c(-87.65, 41.88)))
  events <- data.frame(id = "event", time = 0, frame = 1L, lon = -87.65, lat = 41.90)
  input <- structure(list(roads = roads, events = events, input_type = "flow",
    unit = "vehicles/hour", values = matrix(6), times = 0, interval_seconds = 300,
    clock = "source_clock"), class = "trafficmap_data")
  for (kind in c("flow", "speed")) {
    input$input_type <- kind
    input$unit <- if (kind == "flow") "vehicles/hour" else "mph"
    draw <- if (kind == "flow") traffic_flow_map else traffic_speed_map
    file <- tempfile(fileext = ".png")
    on.exit(unlink(file), add = TRUE)
    fixed <- draw(input, full_extent = TRUE, padding = 0,
      output_file = file, draw = FALSE, width = 5, height = 5, dpi = 72)
    wide <- draw(input, full_extent = TRUE, padding = .12, draw = FALSE)
    expect_lt(unname(fixed$extent["west"]), -87.65)
    expect_gt(unname(fixed$extent["east"]), -87.65)
    expect_equal(unname(fixed$extent["north"]), 41.90)
    expect_gte(unname(fixed$viewport["north"]) + 1e-10, 41.90)
    expect_lt(unname(wide$extent["south"]), unname(fixed$extent["south"]))
    expect_gt(unname(wide$extent["north"]), unname(fixed$extent["north"]))
  }
})

test_that("OD frame extent includes the curved route and event overlay", {
  nodes <- data.frame(node_id = c("A", "B", "C"), longitude = c(-87.65, -87.65, -81),
    latitude = c(41.88, 41.90, 41.90))
  route <- data.frame(origin_id = "A", destination_id = "B", flow = 8L,
    origin_longitude = -87.65, origin_latitude = 41.88,
    destination_longitude = -87.65, destination_latitude = 41.90)
  frame <- structure(list(nodes = nodes[1:2, ], all_nodes = nodes, routes = route,
    selected_flow = 8L, within_node_flow = 0L, selected_records = 1L), class = "bdynets_od_pairs")
  broader <- frame
  broader$routes$flow <- broader$selected_flow <- 1200L
  input <- list(frame, broader)
  series <- traffic_od_map(input, index = 1L, draw = FALSE)
  single <- traffic_od_map(frame, draw = FALSE)
  expect_identical(single$extent, traffic_od_map(list(frame), draw = FALSE)$extent)
  expect_gt(unname(single$extent["east"]), nodes$longitude[3L])
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  event <- data.frame(lon = -87.63, lat = 41.91)
  focus <- traffic_od_map(input, index = 1L, extent = "frame", padding = 0,
    events = event, output_file = file, draw = FALSE, width = 5, height = 5, dpi = 72)
  expect_lt(unname(focus$extent["west"]), -87.65)
  expect_equal(unname(focus$extent["east"]), event$lon)
  expect_gte(unname(focus$viewport["north"]) + 1e-10, event$lat)
  expect_lt(unname(focus$extent["east"]), unname(series$extent["east"]))
  expect_identical(focus$summary$flow_classes, series$summary$flow_classes)
  expect_identical(focus$index, 1L)
  bounds <- c(-88, -87.5, 41, 42)
  expect_equal(unname(traffic_od_map(input, bounds = bounds, extent = "frame",
    draw = FALSE)$extent), bounds)
  empty <- frame
  empty$routes <- route[FALSE, ]
  empty$nodes <- nodes[FALSE, ]
  empty$selected_flow <- empty$selected_records <- 0L
  empty$status <- "no_records"
  empty_map <- traffic_od_map(list(frame, empty), index = 2L, extent = "frame", draw = FALSE)
  expect_identical(empty_map$status, "no_records")
  expect_gt(unname(empty_map$extent["east"]), nodes$longitude[3L])
})

test_that("adjacent decimal road intervals do not overlap due to rounding", {
  roads <- data.frame(road_id = "A")
  roads$geometry <- list(rbind(c(-87.65, 41.88), c(-87.63, 41.89)))
  x <- structure(list(roads = roads, values = matrix(c(1, 2)), times = c(.1, .3),
    interval_seconds = c(.2, .2), clock = "source_clock"), class = "trafficmap_data")
  for (kind in c("flow", "speed")) {
    x$input_type <- kind
    x$unit <- if (kind == "flow") "vehicles/hour" else "mph"
    draw <- if (kind == "flow") traffic_flow_map else traffic_speed_map
    expect_identical(draw(x, time_label = "00:00:00.3", draw = FALSE)$index, 2L)
    overlap <- x
    overlap$interval_seconds[1L] <- .201
    expect_error(draw(overlap, draw = FALSE), "no overlap")
  }
})

test_that("pre-epoch absolute fractional times round-trip without cancellation", {
  roads <- data.frame(road_id = "A")
  roads$geometry <- list(rbind(c(-87.65, 41.88), c(-87.63, 41.89)))
  flow <- structure(list(roads = roads, input_type = "flow", unit = "vehicles/hour",
    values = matrix(c(1, 2)), times = c(-0.1, -1e-9), interval_seconds = c(.05, .05),
    clock = "absolute", timezone = "UTC"), class = "trafficmap_data")
  for (i in 1:2) {
    label <- c("1969-12-31 23:59:59.9", "1969-12-31 23:59:59.999999999")[i]
    expect_identical(traffic_flow_map(flow, index = i, draw = FALSE)$time_label, label)
    expect_identical(traffic_flow_map(flow, time_label = label, draw = FALSE)$index, i)
  }
  parsed_start <- "1969-12-31 23:59:59.9"
  parsed <- flow
  parsed$times[1L] <- as.numeric(as.POSIXct(parsed_start, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
  expect_identical(traffic_flow_map(parsed, index = 1L, draw = FALSE)$time_label, parsed_start)
  expect_identical(traffic_flow_map(parsed, time_label = parsed_start, draw = FALSE)$index, 1L)
  expect_error(traffic_flow_map(parsed, time_label = "1969-12-31 23:59:59.8", draw = FALSE),
    "exactly one")
  ambiguous <- flow
  ambiguous$times <- -.1 + c(-.Machine$double.eps, 0)
  ambiguous$interval_seconds <- rep(diff(ambiguous$times) / 2, 2)
  expect_error(traffic_flow_map(ambiguous, time_label = parsed_start, draw = FALSE), "exactly one")
  expect_identical(traffic_flow_map(ambiguous, index = 2L, draw = FALSE)$index, 2L)
  nodes <- data.frame(node_id = c("A", "B"), longitude = c(-87.65, -87.63),
    latitude = c(41.88, 41.89))
  route <- data.frame(origin_id = "A", destination_id = "B", flow = 2L,
    origin_longitude = -87.65, origin_latitude = 41.88,
    destination_longitude = -87.63, destination_latitude = 41.89)
  od <- structure(list(nodes = nodes, all_nodes = nodes, routes = route,
    start_time = -.1, end_time = 0, timezone = "UTC", selected_flow = 2L,
    within_node_flow = 0L, selected_records = 1L), class = "bdynets_od_pairs")
  expect_identical(traffic_od_map(od, time_label = "1969-12-31 23:59:59.9", draw = FALSE)$index, 1L)
})

test_that("a Unicode-capable PDF device accepts a Chinese title and place label", {
  skip_if_not(capabilities("aqua") ||
    capabilities("cairo") && nzchar(suppressWarnings(grDevices::grSoftVersion()[["cairo"]])))
  roads <- data.frame(road_id = "A")
  roads$geometry <- list(rbind(c(116.39, 39.90), c(116.40, 39.91)))
  flow <- structure(list(roads = roads, input_type = "flow", unit = "vehicles/hour",
    values = matrix(1), times = 0, interval_seconds = 60,
    clock = "source_clock"), class = "trafficmap_data")
  basemap <- list(type = "FeatureCollection", attribution = "OSM", features = list(list(
    type = "Feature", geometry = list(type = "Point", coordinates = c(116.395, 39.905)),
    properties = list(label = "天安门"))))
  path <- tempfile(fileext = ".pdf")
  on.exit(unlink(path), add = TRUE)
  expect_no_warning(traffic_flow_map(flow, basemap = basemap, title = "北京流量",
    output_file = path, draw = FALSE))
  expect_gt(file.info(path)$size, 0)
})
