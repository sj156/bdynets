# Packaged Divvy bike example.
#
# BDCN_BIKE_PROFILE may be smoke, analysis, or long. The default smoke profile
# validates the complete workflow but is not suitable for inference.

library(BDCN)

profile <- Sys.getenv("BDCN_BIKE_PROFILE", "smoke")
if (!profile %in% c("smoke", "analysis", "long")) {
  stop("BDCN_BIKE_PROFILE must be smoke, analysis, or long.")
}

output_dir <- Sys.getenv("BDCN_BIKE_OUTPUT", "")
if (!nzchar(output_dir)) output_dir <- NULL

result <- bdcn_run_bike_example(profile = profile, output_dir = output_dir)
print(result$fit)
print(result$dss)
print(result$scores)

