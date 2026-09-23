# Sparse heterogeneous two-hop propagation example.
#
# Use BDCN_EXAMPLE_PROFILE=quick or default only when the additional runtime is
# intended. The smoke profile verifies the workflow but is not inferential.

library(BDCN)

profile <- Sys.getenv("BDCN_EXAMPLE_PROFILE", "smoke")
if (!profile %in% c("smoke", "quick", "default")) {
  stop("BDCN_EXAMPLE_PROFILE must be smoke, quick, or default.")
}

result <- bdcn_run_multihop_example(profile = profile, signal = 0.20, side = 3L)
print(result$fit)
print(result$dss)
print(result$scores)

save_to <- Sys.getenv("BDCN_EXAMPLE_SAVE", "")
if (nzchar(save_to)) saveRDS(result, save_to, compress = "xz")

