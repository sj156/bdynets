# Fits use static-plus-dynamic propagation and learned constant discount by default.
# See gls_control() for static_network/factor_evolution comparison switches.
# Packaged Divvy bike example.
#
# GLS_BIKE_PROFILE may be smoke, analysis, or long. The default smoke profile
# validates the complete workflow but is not suitable for inference.

library(GLS)

profile <- Sys.getenv("GLS_BIKE_PROFILE", "smoke")
if (!profile %in% c("smoke", "analysis", "long")) {
  stop("GLS_BIKE_PROFILE must be smoke, analysis, or long.")
}

output_dir <- Sys.getenv("GLS_BIKE_OUTPUT", "")
if (!nzchar(output_dir)) output_dir <- NULL

result <- gls_run_bike_example(profile = profile, output_dir = output_dir)
print(result$fit)
print(result$dss)
print(result$scores)

