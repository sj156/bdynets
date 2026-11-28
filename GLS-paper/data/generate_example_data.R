# Generate a prepared input for run_application.R using the package's two-hop example.
generate_gls_example_data <- function() {
  design <- bdynets::gls_simulate_multihop(
    signal = 0.20, side = 2L, control = bdynets::gls_control("smoke")
  )
  sim <- design$simulation
  counts <- sim$Y
  colnames(counts) <- design$graph$edges$name
  list(
    Y = counts,
    graph = design$graph,
    covariates = cbind(trend = sim$time_covariate, season = sim$season_covariate),
    split = design$split,
    pre_history = sim$pre_history
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  usage <- "Rscript GLS-paper/data/generate_example_data.R <new-input.rds>"
  if (identical(args, "--help")) {
    cat(usage, "\n")
  } else {
    if (length(args) != 1L || !nzchar(args[[1L]])) stop(usage, call. = FALSE)
    path <- path.expand(args[[1L]])
    if (file.exists(path)) stop("Input file already exists: ", path, call. = FALSE)
    if (!dir.exists(dirname(path))) stop("Create the parent directory first: ", dirname(path))
    input <- generate_gls_example_data()
    saveRDS(input, path)
    cat("Saved", nrow(input$Y), "time points and", ncol(input$Y), "link series to", path, "\n")
  }
}
