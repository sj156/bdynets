# Run the reproducible Divvy Bike example from the repository root:
# Rscript examples/DivvyBike/run.R /tmp/new-divvy-output

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop("Supply one new output directory", call. = FALSE)
}

output_dir <- path.expand(args[[1]])
if (file.exists(output_dir)) {
  stop("Output directory already exists; supply a new path", call. = FALSE)
}

library(bdynets)

result <- run_divvy_od_map(
  data_dir = "examples/DivvyBike/data",
  radius_m = 2500,
  start_time = "2026-01-05 06:00:00",
  end_time = "2026-01-05 09:00:00",
  aggregation_output_dir = file.path(output_dir, "aggregated"),
  street_file = "examples/DivvyBike/data/chicago_streets_divvy.rds",
  output_file = file.path(
    output_dir,
    "divvy_od_r2500_20260105_0600_0900.png"
  )
)

cat(
  "Created", nrow(result$aggregation$nodes), "aggregate nodes and",
  nrow(result$aggregation$flows), "hourly OD rows.\n"
)
cat(
  "Preserved", format(sum(result$aggregation$flows$trip_count), big.mark = ","),
  "trips.\n"
)
cat("Map:", result$map$output_file, "\n")
