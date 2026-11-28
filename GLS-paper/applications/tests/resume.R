#!/usr/bin/env Rscript
# An installed bdynets with the current GLS API is required.
here <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
launcher <- file.path(dirname(dirname(normalizePath(here))), "run.R")
tmp <- tempfile("application-resume-")
dir.create(tmp)
input <- file.path(tmp, "prepared.rds")
output <- file.path(tmp, "output")
set.seed(201)
graph <- bdynets::gls_grid(2L)
Y <- matrix(rpois(23L * nrow(graph$edges), 4), 23L)
saveRDS(list(Y = Y, graph = graph, covariates = matrix(1, 23, 1),
  split = list(train = 1:14, validation = 15:18, test = 19:23),
  control = list(n_iter = 10L, burn_mcmc = 4L, thin = 1L, n_chains = 1L,
    startup_min_static_acceptance = 0, startup_min_static_accepts = 0L,
    dss_n_lambda = 3L, dss_validation_draws = 2L, dss_max_draws = 2L,
    predictive_draws = 2L)), input)
invoke <- function(action, chunk = 2L, every = 2L, expect = NULL, script = launcher) {
  args <- c(script, paste0("--action=", action), "--profile=smoke", "--cores=1",
    "--benchmarks=none", paste0("--input=", input), paste0("--output=", output),
    paste0("--chunk-size=", chunk), paste0("--checkpoint-every=", every))
  res <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                  vapply(args, shQuote, character(1)), stdout = TRUE, stderr = TRUE))
  ok <- is.null(attr(res, "status"))
  if (if (is.null(expect)) !ok else ok || !any(grepl(expect, res, fixed = TRUE)))
    stop(paste(res, collapse = "\n"))
}
tryCatch({
  invoke("run")
  invoke("resume")
  invoke("resume", chunk = 4L, expect = "differs from the saved application")
  invoke("resume", every = 1L, expect = "differs from the saved application")
  # Moving a copy into its own repository layout changes only the launcher's code.
  other <- file.path(tmp, "other", "paper", "applications")
  dir.create(other, recursive = TRUE)
  modified <- file.path(other, "run.R")
  file.copy(launcher, modified)
  cat("\n# Rebuilt application driver\n", file = modified, append = TRUE)
  invoke("resume", script = modified, expect = "differs from the saved application")
  marker <- readRDS(file.path(output, "application_completion.rds"))
  chunk <- file.path(output, "draws", marker$store, marker$draw_files[1L])
  cat("changed", file = chunk, append = TRUE)
  invoke("resume", expect = "missing or corrupt")
  cat("changed", file = file.path(output, "application_results.rds"), append = TRUE)
  invoke("resume", expect = "result checksum differs")
  cat("Verified complete resume, mismatched options and launcher, and corrupt artifacts.\n")
}, finally = unlink(tmp, recursive = TRUE))
