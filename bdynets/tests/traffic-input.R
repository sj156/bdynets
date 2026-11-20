library(bdynets)
nodes <- data.frame(node_id = c("001", "017"), longitude = c(10, 11), latitude = c(20, 21))
trips <- data.frame(origin_id = "017", destination_id = "001", flow = 4L)
stops <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
od <- traffic_od_data(nodes, trips)
stopifnot(identical(od$flows$origin_id, "017"), identical(as.numeric(od$flows$flow), 4),
          stops(traffic_od_data(nodes, transform(trips, destination_id = "7"))),
          stops(traffic_od_data(nodes, transform(trips, flow = 2.5))))
roads <- data.frame(road_id = "segment")
roads$geometry <- list(rbind(c(10, 20), c(11, 21)))
flow <- data.frame(road_id = "segment", time = c(0, 300), flow = c(0, NA_real_))
events <- data.frame(time = 42, lon = 10, lat = 20)
road <- list(schema = "bdynets-traffic-input-v1", roads = roads, flow = flow,
             events = events, unit = "count", interval_seconds = 300)
path <- tempfile(fileext = ".rds")
saveRDS(road, path)
readback <- read_traffic(path)
stopifnot(identical(as.numeric(readback$values), c(0, NA_real_)),
          identical(readback$events$frame, 1L))
saveRDS(c(road, list(nodes = nodes, od = trips)), path)
stopifnot(stops(read_traffic(path)))
unlink(path)
