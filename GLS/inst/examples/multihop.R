# Fits use static-plus-dynamic propagation and learned constant discount by default.
# See gls_control() for static_network/factor_evolution comparison switches.
# Sparse heterogeneous two-hop propagation example.
#
# Use GLS_EXAMPLE_PROFILE=quick or default only when the additional runtime is
# intended. The smoke profile verifies the workflow but is not inferential.

library(GLS)

profile <- Sys.getenv("GLS_EXAMPLE_PROFILE", "smoke")
if (!profile %in% c("smoke", "quick", "default")) {
  stop("GLS_EXAMPLE_PROFILE must be smoke, quick, or default.")
}

result <- gls_run_multihop_example(profile = profile, signal = 0.20, side = 3L)
print(result$fit)
print(result$dss)
print(result$scores)

save_to <- Sys.getenv("GLS_EXAMPLE_SAVE", "")
if (nzchar(save_to)) saveRDS(result, save_to, compress = "xz")

