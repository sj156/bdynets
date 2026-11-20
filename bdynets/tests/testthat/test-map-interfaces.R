flow_map_fixture <- function() {
  roads <- data.frame(road_id = c("a", "b"), stringsAsFactors = FALSE)
  roads$geometry <- list(
    rbind(c(10.00, 50.00), c(10.01, 50.00)),
    rbind(c(10.01, 50.00), c(10.02, 50.01))
  )
  times <- c(0, 60)
  flow <- data.frame(
    road_id = rep(c("a", "b"), 2),
    time = rep(times, each = 2),
    flow = c(0, 5, 10, NA)
  )
  list(
    schema = "bdynets-traffic-input-v1",
    roads = roads,
    flow = flow,
    times = times,
    interval_seconds = 60,
    unit = "vehicles/hour"
  )
}

prepared_od_fixture <- function() {
  start <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  nodes <- data.frame(
    node_id = c("a", "b", "c"),
    longitude = c(10.00, 10.01, 10.02),
    latitude = c(50.00, 50.01, 50.00)
  )
  flows <- data.frame(
    origin_id = c("a", "a", "b"),
    destination_id = c("b", "b", "c"),
    time = start + c(10, 70, 20),
    flow = c(1L, 2L, 3L)
  )
  list(
    data = traffic_od_data(nodes, flows, timezone = "UTC"),
    start = start
  )
}

test_that("the public map interfaces use separate inputs", {
  exports <- getNamespaceExports("bdynets")
  expect_true(all(c("traffic_flow_map", "traffic_od_map", "traffic_map_html") %in% exports))
  expect_false(any(c("traffic_map", "traffic_input") %in% exports))
})

test_that("traffic_flow_map renders a road frame and preserves missing flow", {
  old <- options(bdynets.basemap.auto = FALSE)
  on.exit(options(old), add = TRUE)
  input <- flow_map_fixture()
  result <- traffic_flow_map(input, index = 2, basemap = NULL, draw = FALSE)
  expect_identical(result$type, "flow")
  expect_equal(result$road_summary$missing, 1)
  expect_equal(result$road_summary$available, 1)

  image <- tempfile(fileext = ".png")
  on.exit(unlink(image), add = TRUE)
  written <- traffic_flow_map(
    input,
    index = 1,
    basemap = NULL,
    output_file = image,
    draw = FALSE,
    width = 5,
    height = 4,
    dpi = 72
  )
  expect_true(file.exists(image))
  expect_gt(unname(file.info(image)$size), 500)
  expect_identical(written$output_file, normalizePath(image))
})

test_that("traffic_od_map filters and aggregates directed OD records", {
  old <- options(bdynets.basemap.auto = FALSE)
  on.exit(options(old), add = TRUE)
  x <- prepared_od_fixture()
  result <- traffic_od_map(
    x$data,
    start_time = x$start,
    end_time = x$start + 60,
    basemap = NULL,
    draw = FALSE
  )
  expect_equal(result$summary$selected_flow, 4)
  expect_equal(result$summary$displayed_pairs, 2)
  expect_equal(sum(result$summary$flow_classes), 2)
})

test_that("flow and OD use the shared canvas and separate line renderers", {
  canvas_calls <- 0L
  flow_calls <- 0L
  od_calls <- 0L
  canvas <- bdynets:::.tm_map_canvas
  flow_lines <- bdynets:::.tm_draw_flow_lines
  od_lines <- bdynets:::.tm_draw_od_layer
  local_mocked_bindings(
    .tm_map_canvas = function(...) {
      canvas_calls <<- canvas_calls + 1L
      canvas(...)
    },
    .tm_draw_flow_lines = function(...) {
      flow_calls <<- flow_calls + 1L
      flow_lines(...)
    },
    .tm_draw_od_layer = function(...) {
      od_calls <<- od_calls + 1L
      od_lines(...)
    },
    .package = "bdynets"
  )

  flow_file <- tempfile(fileext = ".png")
  od_file <- tempfile(fileext = ".png")
  on.exit(unlink(c(flow_file, od_file)), add = TRUE)
  traffic_flow_map(
    flow_map_fixture(), basemap = NULL, output_file = flow_file,
    draw = FALSE, width = 5, height = 4, dpi = 72
  )
  x <- prepared_od_fixture()
  traffic_od_map(
    x$data, start_time = x$start, end_time = x$start + 60,
    basemap = NULL, output_file = od_file,
    draw = FALSE, width = 5, height = 4, dpi = 72
  )

  expect_equal(canvas_calls, 2L)
  expect_equal(flow_calls, 1L)
  expect_equal(od_calls, 1L)
})

test_that("both static interfaces restore graphics state", {
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  grDevices::pdf(file)
  on.exit(grDevices::dev.off(), add = TRUE)
  before <- graphics::par(c("mar", "xpd"))

  traffic_flow_map(flow_map_fixture(), basemap = NULL)
  expect_identical(graphics::par(c("mar", "xpd")), before)

  x <- prepared_od_fixture()
  traffic_od_map(
    x$data,
    start_time = x$start,
    end_time = x$start + 60,
    basemap = NULL
  )
  expect_identical(graphics::par(c("mar", "xpd")), before)
})

test_that("custom flow classes reach the road renderer", {
  input <- flow_map_fixture()
  seen <- NULL
  restore <- bdynets:::.tm_map_restore_road_window
  local_mocked_bindings(
    .tm_map_restore_road_window = function(road, region) {
      seen <<- road
      restore(road, region)
    },
    .package = "bdynets"
  )
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  traffic_flow_map(
    input,
    basemap = NULL,
    flow_style = list(
      breaks = 5.5,
      palette = c("green", "red"),
      right = FALSE
    ),
    output_file = file,
    draw = FALSE,
    width = 5,
    height = 4,
    dpi = 72
  )
  expect_identical(bdynets:::.tm_labels(seen$scale), c("0 to < 5.5", ">= 5.5"))
})

test_that("saved Flow frames format their actual time when drawn", {
  fixture <- flow_map_fixture()
  cases <- list(
    list(time = 28800, format = "%H:%M", label = "08:00"),
    list(time = 1781596800, format = "%Y-%m-%d %H:%M", label = "2026-06-16 08:00")
  )
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  for (case in cases) {
    flow <- fixture$flow[fixture$flow$time == 0, ]
    flow$time <- case$time
    frame <- traffic_data(fixture$roads, flow, times = case$time, interval_seconds = 300)
    frame$time_labels <- NULL
    frame$time_label_format <- case$format
    saveRDS(frame, file)
    result <- traffic_flow_map(file, basemap = NULL, draw = FALSE)
    expect_identical(result$time_label, case$label)
    expect_identical(result$data$values, frame$values)
    expect_identical(result$data$times, frame$times)
    expect_null(readRDS(file)$time_labels)
  }
})

test_that("map style overrides must have unique nonempty names", {
  flow <- flow_map_fixture()
  nodes <- data.frame(node_id = c("a", "b"), longitude = c(12, 12.1),
                      latitude = c(51, 51.1), member_weight = 1)
  trips <- data.frame(origin_id = "a", destination_id = "b", flow = 2)
  for (style in list(list("red"), list(right = TRUE, right = FALSE))) {
    expect_error(traffic_flow_map(flow, flow_style = style, draw = FALSE),
                 "flow_style must be a uniquely named list")
    expect_error(traffic_od_map(nodes, trips, flow_style = style, draw = FALSE),
                 "flow_style must be a uniquely named list")
  }
})

test_that("OD clock-hour shorthand agrees with the selected interval", {
  nodes <- data.frame(node_id = c("a", "b"), longitude = c(12, 12.1),
                      latitude = c(51, 51.1), member_weight = 1)
  trips <- data.frame(origin_id = "a", destination_id = "b", flow = c(1, 2, 4, 8),
    time = paste("2026-01-01", c("08:00:00", "08:29:59", "08:30:00", "09:00:00")))
  whole_hour <- traffic_od_select(nodes, trips, time_label = "08:00")
  expect_equal(whole_hour$flow, 7)
  expect_equal(whole_hour$time_window, "08:00-09:00 (UTC)")
  expect_error(traffic_od_select(nodes, trips, time_label = "08:30"), "whole clock hour")
  expect_error(traffic_od_select(nodes, trips, time_label = "08:60"), "whole clock hour")
  subhour <- traffic_od_select(nodes, trips, start_time = "2026-01-01 08:30:00",
                              end_time = "2026-01-01 09:00:00")
  expect_equal(subhour$flow, 4)
})

test_that("public map preparation reads numeric factor labels as values", {
  nodes <- data.frame(node_id = c("a", "b"),
    longitude = factor(c("12", "12.1")), latitude = factor(c("51", "51.1")),
    member_weight = factor(c("200", "100")))
  trips <- data.frame(origin_id = "a", destination_id = "b",
                      flow = factor("20"), time = "2026-01-01 08:00:00")
  selection <- traffic_od_select(nodes, trips)
  expect_equal(selection$nodes$longitude, c(12, 12.1))
  expect_equal(selection$nodes$latitude, c(51, 51.1))
  expect_equal(selection$nodes$size, c(200, 100))
  expect_equal(selection$flow, 20)
  expect_equal(unname(traffic_map_extent(nodes, padding = 0)), c(12, 12.1, 51, 51.1))
  streets <- transform(nodes, street_group = "local", street_class = "residential")
  base <- traffic_map_base(streets, c(11, 13, 50, 52))
  expect_length(base$features, 1L)
})
