# Run from the repository root:
# Rscript examples/DivvyBike/run.R /tmp/new-divvy-output

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply one new output directory", call. = FALSE)
output_dir <- path.expand(args[[1]])
if (file.exists(output_dir)) {
  stop("Output directory already exists; supply a new path", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE)

library(bdynets)

data_dir <- "examples/DivvyBike/data"
node_file <- file.path(data_dir, "nodes_r2000.csv")
flow_file <- file.path(data_dir, "od_flow_2026_01_dt01h_r2000.csv")
if (!file.exists(node_file) || !file.exists(flow_file)) {
  stop("The supplied r2000 node and OD files are required.", call. = FALSE)
}

# Pool every January date over the same 00:00-03:00 clock-hour window.
map_file <- file.path(output_dir, "divvy_od_r2000_month_0000_0300.png")
map_result <- traffic_od_map(
  nodes = node_file,
  flows = flow_file,
  basemap = file.path(data_dir, "chicago_streets_divvy.rds"),
  node_columns = c(
    id = "node_idx", longitude = "longitude", latitude = "latitude",
    size = "n_raw_stations"
  ),
  flow_columns = c(
    origin = "origin_node_idx", destination = "destination_node_idx",
    count = "trip_count", time = "timestamp"
  ),
  hour_window = c(0, 3),
  timezone = "America/Chicago",
  top_n = Inf,
  active_nodes_only = TRUE,
  title = "Divvy directed OD flows | 2 km grid aggregation",
  subtitle = paste(
    "January 2026 · all dates pooled over 00:00-03:00 ·",
    "active nodes only"
  ),
  caption = paste(
    "Background: City of Chicago street center lines.",
    "Curved arrows connect aggregate endpoints; they are not observed paths."
  ),
  output_file = map_file,
  width = 7.6,
  height = 9.2,
  dpi = 240,
  draw = FALSE
)

cat("Displayed", map_result$summary$displayed_nodes, "active nodes and",
    map_result$summary$displayed_pairs, "directed OD pairs.\n")
cat("Represented", map_result$summary$displayed_flow, "nonloop trips.\n")
print(map_result$summary$flow_classes)
cat("Map:", map_file, "\n")
