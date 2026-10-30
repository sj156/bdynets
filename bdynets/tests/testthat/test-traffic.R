test_that("traffic functions share the installed bdynets namespace", {
  expected <- c("traffic_data", "read_traffic", "flow_scale", "traffic_summary",
                "traffic_trend", "traffic_map", "traffic_report")
  expect_true(all(expected %in% getNamespaceExports("bdynets")))
  expect_identical(environmentName(environment(traffic_map)), "bdynets")
  expect_identical(getS3method("plot", "trafficmap_data"), bdynets:::plot.trafficmap_data)
  expect_true(all(file.exists(system.file("package_assets",
    c("map.js", "style.css", "template.html", "leaflet.js", "leaflet.css", "LICENSE.leaflet"),
    package = "bdynets"))))
})

test_that("serialized input and the installed renderer agree", {
  roads <- data.frame(road_id = c("east", "west"))
  roads$geometry <- list(rbind(c(10, 50), c(10.01, 50)),
                        rbind(c(10.01, 50), c(10, 50)))
  flow <- data.frame(road_id = c("west", "east"), time = 0, flow = c(10, 25))
  bundle <- list(schema = "trafficmap-input-v1", roads = roads, flow = flow,
    events = data.frame(time = 30, lon = 10.005, lat = 50),
    times = 0, interval_seconds = 60, unit = "vehicles/hour")
  file <- tempfile(fileext = ".rds"); html <- tempfile(fileext = ".html")
  on.exit(unlink(c(file, html)), add = TRUE)
  saveRDS(bundle, file)
  x <- read_traffic(file)
  expect_identical(as.numeric(x$values), c(25, 10))
  expect_identical(x$roads$geometry, roads$geometry)
  traffic_report(x, html)
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "library(bdynets)", fixed = TRUE)
  expect_false(grepl("library(trafficmap)", text, fixed = TRUE))
  expect_match(text, "data:image/png;base64,", fixed = TRUE)
})
