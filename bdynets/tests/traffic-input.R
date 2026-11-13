library(bdynets)
nodes <- data.frame(node_id = c("001", "017"), longitude = c(10, 11), latitude = c(20, 21),
                    node_idx = c(0L, 7L), display_name = c("North", "South"))
od <- data.frame(origin_id = "017", destination_id = "001", flow = 4L)
stops <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
od_only <- traffic_input(nodes = nodes, od = od)
stopifnot(identical(od_only$nodes$display_name, nodes$display_name),
          identical(od_only$od$origin_id, "017"), identical(od_only$od_time_kind, "untimed"),
          stops(traffic_input(nodes = nodes, od = transform(od, destination_id = "7"))),
          stops(traffic_input(nodes = nodes, od = transform(od, flow = 2.5))),
          stops(traffic_input(nodes = nodes, od = od, od_interval_seconds = 3600)))
roads <- data.frame(road_id = "segment")
roads$geometry <- list(rbind(c(10, 20), c(11, 21)))
flow <- data.frame(road_id = "segment", time = c(0, 300), flow = c(0, NA_real_))
events <- data.frame(time = 42, lon = 10, lat = 20)
mix <- traffic_input(roads, flow, events, nodes = nodes, od = od,
                     unit = "count", interval_seconds = 300)
path <- tempfile(fileext = ".rds")
saveRDS(mix, path)
stopifnot(identical(readRDS(path), mix),
          identical(as.numeric(read_traffic(path)$values), c(0, NA_real_)),
          identical(read_traffic(path)$events$frame, 1L),
          identical(mix$od_unit, "trips"))
unlink(path)
