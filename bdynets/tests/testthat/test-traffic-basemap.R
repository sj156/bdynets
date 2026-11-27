test_that("one named region and distant unassigned records reuse both caches", {
  cache_dir <- tempfile("traffic-basemap-")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  locations <- rbind(West = c(-123.12, 49.28), East = c(-122.12, 49.00))
  streets <- lapply(seq_len(nrow(locations)), function(i) {
    start <- locations[i, ]
    geometry <- rbind(start, start + c(.002, .003))
    bounds <- c(start[1L] + c(-.05, .05), start[2L] + c(-.05, .05))
    query <- bdynets:::.bm_query(bounds, timeout = 30)
    feature <- list(type = "Feature",
      geometry = list(type = "LineString", coordinates = geometry),
      properties = list(osm_id = rownames(locations)[i]))
    context <- list(type = "FeatureCollection", features = list(feature),
      provenance = list(bounds = bounds, query = query, highway_classes = NULL))
    bdynets:::.bm_cache_save(list(bounds = bounds,
      selector = bdynets:::.bm_selector(NULL), query = query, basemap = context),
      cache_dir)
    geometry
  })
  roads <- data.frame(road_id = c("A", "B"), region = c("West", NA_character_))
  roads$geometry <- streets
  input <- structure(list(input_type = "flow", unit = "vehicles/hour",
    roads = roads, values = matrix(c(5, 8), nrow = 1L),
    times = 0, interval_seconds = 300, clock = "source_clock"),
    class = "trafficmap_data")
  cache <- list(mode = "cache", cache_dir = cache_dir)
  feature_ids <- function(map) {
    vapply(map$basemap$features, function(feature) feature$properties$osm_id, "")
  }

  expect_no_error({
    map <- traffic_flow_map(input, basemap = cache, draw = FALSE)
    expect_setequal(feature_ids(map), c("West", "East"))
  })

  input$roads <- roads[1L, , drop = FALSE]
  input$values <- input$values[, 1L, drop = FALSE]
  input$events <- data.frame(id = "incident", time = 0, frame = 1L,
    lon = locations["East", 1L], lat = locations["East", 2L])
  expect_no_error({
    map <- traffic_flow_map(input, basemap = cache, draw = FALSE)
    expect_setequal(feature_ids(map), c("West", "East"))
  })
})

test_that("a damaged RDS cache does not block a verified covering cache", {
  cache_dir <- tempfile("traffic-basemap-")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  bounds <- c(-123.125, -123.12, 49.282, 49.287)

  write_cache <- function(extent, id) {
    query <- bdynets:::.bm_query(extent, timeout = 30)
    start <- c(mean(extent[1:2]), mean(extent[3:4]))
    feature <- list(type = "Feature",
      geometry = list(type = "LineString",
        coordinates = rbind(start, start + c(.0001, .0001))),
      properties = list(osm_id = id))
    context <- list(type = "FeatureCollection", features = list(feature),
      provenance = list(bounds = extent, query = query, highway_classes = NULL))
    bdynets:::.bm_cache_save(list(bounds = extent,
      selector = bdynets:::.bm_selector(NULL), query = query, basemap = context),
      cache_dir)
  }

  wide <- write_cache(bounds + c(-.2, .2, -.2, .2), "verified")
  small <- write_cache(bounds + c(-.02, .02, -.02, .02), "small")
  exact <- write_cache(bounds, "exact")
  read_context <- function() {
    traffic_basemap(bounds = bounds, cache_dir = cache_dir,
      detail = "all", offline = TRUE)
  }
  expect_identical(read_context()$features[[1L]]$properties$osm_id, "exact")
  for (file in c(small, exact)) {
    record <- readRDS(file)
    record$checksum <- "damaged"
    saveRDS(record, file)
  }
  expect_no_error({
    expect_identical(read_context()$features[[1L]]$properties$osm_id, "verified")
  })
  expect_error(traffic_basemap(exact), "Invalid basemap cache")
  expect_true(file.exists(wide))
})

test_that("map extent and local background accept the Web Mercator latitude limit", {
  limit <- 85.0511287798066
  bounds <- NULL
  expect_no_error({
    bounds <- traffic_map_extent(data.frame(longitude = 35, latitude = limit))
    expect_identical(unname(bounds["north"]), limit)
  })
  expect_no_error({
    context <- list(type = "FeatureCollection", features = list(list(type = "Feature",
      geometry = list(type = "LineString",
        coordinates = rbind(c(35, limit), c(35.001, limit - .001))))))
    extent <- c(34, 36, limit - .02, limit)
    expect_length(traffic_map_base(context, extent)$features, 1L)
  })
})

test_that("the preparation extent helper separates named and unassigned locations", {
  roads <- data.frame(road_id = c("A", "B"), region = c("West", NA_character_))
  roads$geometry <- list(rbind(c(-123.12, 49.28), c(-123.11, 49.29)),
    rbind(c(-122.12, 49), c(-122.11, 49.01)))
  events <- data.frame(id = "unassigned", time = 0, frame = 1L,
    lon = -122.10, lat = 49.02)
  input <- structure(list(roads = roads, events = events), class = "trafficmap_data")

  extents <- traffic_road_extents(input)
  expect_length(extents, 2L)
  expect_identical(traffic_road_extents(input, region = "West"), extents[1L])
  expect_lt(extents[[1L]][["east"]], extents[[2L]][["west"]])
  expect_gt(extents[[2L]][["north"]], events$lat)
  expect_gt(extents[[2L]][["east"]], events$lon)
})

test_that("character and factor regions select the same road and event scopes", {
  roads <- data.frame(road_id = c("A", "B", "C"),
    region = c("West", "East", NA_character_))
  roads$geometry <- list(rbind(c(-123.12, 49.28), c(-123.11, 49.29)),
    rbind(c(-122.12, 49), c(-122.11, 49.01)),
    rbind(c(-121.12, 49.17), c(-121.11, 49.18)))
  events <- data.frame(id = c("E1", "E2", "E3"), time = 0, frame = 1L,
    lon = c(-123.11, -122.11, -121.11), lat = c(49.29, 49.01, 49.18),
    region = c("West", "East", ""))
  input <- structure(list(input_type = "flow", unit = "vehicles/hour",
    roads = roads, events = events, values = matrix(c(5, 8, 3), nrow = 1L),
    times = 0, interval_seconds = 300, clock = "source_clock"),
    class = "trafficmap_data")
  reference <- traffic_road_extents(input)
  expect_length(reference, 3L)
  files <- tempfile(c("base-", "factor-"), fileext = ".png")
  on.exit(unlink(files), add = TRUE)

  for (kind in c("flow", "speed")) {
    input$input_type <- kind
    input$unit <- if (kind == "flow") "vehicles/hour" else "mph"
    draw <- if (kind == "flow") traffic_flow_map else traffic_speed_map
    draw(input, output_file = files[1L], draw = FALSE)
    for (columns in list("roads", "events", c("roads", "events"))) {
      mixed <- input
      for (column in columns) {
        labels <- mixed[[column]][["region"]]
        mixed[[column]][["region"]] <- ordered(labels,
          levels = c("Unused", "East", "West", ""))
      }
      expect_no_error({
        expect_identical(traffic_road_extents(mixed), reference)
        expect_identical(traffic_road_extents(mixed, "West"), reference[1L])
        expect_identical(traffic_summary(mixed, region = "West")$events, 1L)
        draw(mixed, output_file = files[2L], draw = FALSE)
        expect_length(unique(unname(tools::md5sum(files))), 1L)
      })
    }
  }
})

test_that("road backgrounds cover the viewport of the drawing device", {
  roads <- data.frame(road_id = "A")
  roads$geometry <- list(rbind(c(-119.15, 36.05), c(-114.60, 36.05)))
  input <- structure(list(input_type = "flow", unit = "vehicles/hour",
    roads = roads, values = matrix(5, nrow = 1L), times = 0,
    interval_seconds = 300, clock = "source_clock"), class = "trafficmap_data")
  image <- tempfile(fileext = ".png")
  on.exit(unlink(image), add = TRUE)

  for (kind in c("flow", "speed")) {
    input$input_type <- kind
    input$unit <- if (kind == "flow") "vehicles/hour" else "mph"
    draw <- if (kind == "flow") traffic_flow_map else traffic_speed_map
    for (dimensions in list(c(11, 9), c(5, 8))) {
      width <- dimensions[1L]
      height <- dimensions[2L]
      bounds <- traffic_road_extents(input, width = width, height = height)[[1L]]
      map <- draw(input, width = width, height = height,
        output_file = image, draw = FALSE)
      expect_equal(bounds, map$viewport, tolerance = 1e-12)
      expect_lt(bounds[["west"]], min(roads$geometry[[1L]][, 1L]))
      expect_gt(bounds[["east"]], max(roads$geometry[[1L]][, 1L]))
      expect_gt(diff(bounds[c("south", "north")]), 1)
    }
    focused <- draw(input, full_extent = FALSE, draw = FALSE)
    expect_lt(diff(focused$extent[c("west", "east")]), 0.01)
  }
})

test_that("OD context preparation can cover the full static viewport", {
  nodes <- data.frame(node_id = c("A", "B"),
    longitude = c(-119.15, -114.60), latitude = c(36.05, 36.05))
  routes <- data.frame(origin_id = "A", destination_id = "B",
    origin_longitude = nodes$longitude[1L], origin_latitude = nodes$latitude[1L],
    destination_longitude = nodes$longitude[2L], destination_latitude = nodes$latitude[2L],
    flow = 3L)
  start <- as.POSIXct("2026-01-01 08:00:00", tz = "UTC")
  frame <- structure(list(nodes = nodes, all_nodes = nodes, routes = routes,
    start_time = start, end_time = start + 300, timezone = "UTC", unit = "trips",
    selected_records = 1L, selected_flow = 3L, within_node_flow = 0L,
    active_pairs = 1L, status = "ok"), class = "bdynets_od_pairs")
  image <- tempfile(fileext = ".png")
  on.exit(unlink(image), add = TRUE)
  map <- traffic_od_map(frame, output_file = image, width = 8, height = 7, draw = FALSE)
  bounds <- traffic_map_extent(nodes, width = 8, height = 7)
  expect_equal(bounds, map$viewport, tolerance = 1e-12)
  expect_gt(diff(bounds[c("south", "north")]), 1)
})

write_viewport_test_cache <- function(directory, bounds, geometry, road_classes) {
  bounds <- unname(bounds)
  classes <- bdynets:::.bm_detail(bounds, road_classes, "auto", c(1024, 768))$classes
  query <- bdynets:::.bm_query(bounds, timeout = 30, classes = classes)
  feature <- list(type = "Feature",
    geometry = list(type = "LineString", coordinates = geometry),
    properties = list(osm_id = "viewport"))
  context <- list(type = "FeatureCollection", features = list(feature),
    provenance = list(bounds = bounds, query = query, highway_classes = classes))
  bdynets:::.bm_cache_save(list(bounds = bounds, selector = bdynets:::.bm_selector(classes),
    query = query, basemap = context), directory)
}

with_viewport_test_device <- function(code) {
  image <- tempfile(fileext = ".png")
  grDevices::png(image, width = 6, height = 11, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(image)
  })
  force(code)
}

test_that("Flow and Speed cache lookup follows the rendered portrait viewport", {
  directory <- tempfile("road-viewport-cache-")
  image <- tempfile(fileext = ".png")
  on.exit(unlink(c(directory, image), recursive = TRUE), add = TRUE)
  roads <- data.frame(road_id = "A", region = factor("West"))
  roads$geometry <- list(rbind(c(-118, 34), c(-118, 35)))
  input <- structure(list(input_type = "flow", unit = "vehicles/hour", roads = roads,
    values = matrix(8, 1L), times = 0, interval_seconds = 300, clock = "source_clock"),
    class = "trafficmap_data")
  bounds <- traffic_road_extents(input, region = "West", width = 6, height = 11)[[1L]]
  write_viewport_test_cache(directory, bounds, roads$geometry[[1L]], NA_character_)
  cache <- list(mode = "cache", cache_dir = directory)
  auto <- list(mode = "auto", cache_dir = directory)

  for (kind in c("flow", "speed")) {
    input$input_type <- kind
    input$unit <- if (kind == "flow") "vehicles/hour" else "mph"
    draw <- if (kind == "flow") traffic_flow_map else traffic_speed_map
    expect_no_error({
      exported <- draw(input, region = "West", basemap = cache, width = 6, height = 11,
        output_file = image, draw = FALSE)
      expect_equal(exported$viewport, bounds, tolerance = 1e-12)
      expect_length(exported$basemap$features, 1L)
    })
    current <- with_viewport_test_device(draw(input, region = "West", basemap = auto))
    expect_equal(current$viewport, bounds, tolerance = 1e-12)
    expect_length(current$basemap$features, 1L)
  }
})

test_that("OD device and file draws both require a cache covering the full viewport", {
  directory <- tempfile("od-viewport-cache-")
  image <- tempfile(fileext = ".png")
  on.exit(unlink(c(directory, image), recursive = TRUE), add = TRUE)
  nodes <- data.frame(node_id = c("A", "B"), longitude = -118, latitude = c(34, 35))
  routes <- data.frame(origin_id = "A", destination_id = "B",
    origin_longitude = -118, origin_latitude = 34,
    destination_longitude = -118, destination_latitude = 35, flow = 4L)
  start <- as.POSIXct("2026-01-01 08:00:00", tz = "UTC")
  frame <- structure(list(nodes = nodes, all_nodes = nodes, routes = routes,
    start_time = start, end_time = start + 300, timezone = "UTC", unit = "trips",
    selected_flow = 4L, selected_records = 1L, within_node_flow = 0L,
    active_pairs = 1L, status = "ok"), class = "bdynets_od_pairs")
  geometry <- as.matrix(nodes[c("longitude", "latitude")])
  write_viewport_test_cache(directory, traffic_map_extent(nodes), geometry, character())
  cache <- list(mode = "cache", cache_dir = directory)
  expect_error(with_viewport_test_device(traffic_od_map(frame, basemap = cache)),
    class = "bdynets_basemap_cache_miss")
  expect_error(traffic_od_map(frame, basemap = cache, width = 6, height = 11,
    output_file = image, draw = FALSE), class = "bdynets_basemap_cache_miss")
  expect_false(file.exists(image))

  bounds <- traffic_map_extent(nodes, width = 6, height = 11)
  write_viewport_test_cache(directory, bounds, geometry, character())
  current <- with_viewport_test_device(traffic_od_map(frame, basemap = cache))
  exported <- traffic_od_map(frame, basemap = cache, width = 6, height = 11,
    output_file = image, draw = FALSE)
  expect_equal(current$viewport, bounds, tolerance = 1e-12)
  expect_equal(current$viewport, exported$viewport)
  expect_identical(current$basemap, exported$basemap)
})

test_that("road plot, HTML report and export resolve caches after fitting their own canvas", {
  scratch <- tempfile("road-map-wrappers-")
  dir.create(scratch)
  previous <- setwd(scratch)
  settings <- options(bdynets.basemap.auto = TRUE)
  on.exit({
    options(settings)
    setwd(previous)
    unlink(scratch, recursive = TRUE)
  }, add = TRUE)

  directory <- file.path("data", "basemap-cache")
  roads <- data.frame(road_id = "A")
  roads$geometry <- list(rbind(c(-118, 34), c(-118, 35)))
  input <- structure(list(input_type = "flow", unit = "vehicles/hour", roads = roads,
    values = matrix(c(4, 8), ncol = 1L), times = c(0, 300), interval_seconds = c(300, 300),
    clock = "source_clock"), class = "trafficmap_data")

  cache_for <- function(x, width, height, line) {
    extent <- traffic_road_extents(x, width = width, height = height)[[1L]]
    write_viewport_test_cache(directory, extent, line, NA_character_)
    traffic_basemap(bounds = extent, detail = "all", offline = TRUE)
  }
  plot_hash <- function(x) {
    image <- tempfile(tmpdir = scratch, fileext = ".png")
    grDevices::png(image, width = 6, height = 11, units = "in", res = 72)
    tryCatch(plot(x), finally = grDevices::dev.off())
    unname(tools::md5sum(image))
  }
  attached <- input
  attached$basemap <- cache_for(input, 6, 11, rbind(c(-118.25, 34), c(-118.25, 35)))
  expect_identical(plot_hash(input), plot_hash(attached))

  input$roads$geometry <- list(rbind(c(-118.8, 34), c(-117.8, 34)))
  attached <- input
  attached$basemap <- cache_for(input, 1200 / 140, 800 / 140,
    rbind(c(-118.8, 34.1), c(-117.8, 34.1)))
  report <- function(x) {
    file <- tempfile(tmpdir = scratch, fileext = ".html")
    traffic_report(x, file)
    paste(readLines(file, warn = FALSE), collapse = "\n")
  }
  first_image <- function(html) regmatches(html, regexpr('data:image/png;base64,[^"]+', html))
  automatic <- report(input)
  expect_identical(first_image(automatic), first_image(report(attached)))
  expect_match(automatic, "Map context: OpenStreetMap", fixed = TRUE)
  expect_match(automatic, "Source identities", fixed = TRUE)

  attached$basemap <- cache_for(input, 10, 7.5, rbind(c(-118.8, 34.07), c(-117.8, 34.07)))
  output <- tempfile(tmpdir = scratch)
  reference <- tempfile(tmpdir = scratch)
  traffic_export(input, output)
  traffic_export(attached, reference)
  expect_identical(unname(tools::md5sum(file.path(output, "map.png"))),
    unname(tools::md5sum(file.path(reference, "map.png"))))
  info <- jsonlite::read_json(file.path(output, "provenance.json"), simplifyVector = FALSE)
  expected <- attached$basemap$provenance$requests[[1L]]$bounds
  expect_equal(unlist(info$source$basemap$requests[[1L]]$bounds), expected)
  for (name in c("map.png", "map.pdf")) {
    expect_equal(unlist(info$map_basemaps[[name]]$source$requests[[1L]]$bounds), expected)
  }
})
