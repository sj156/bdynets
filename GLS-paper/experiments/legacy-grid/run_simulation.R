#!/usr/bin/env Rscript
# Thin paper-study launcher. All model and reporting code lives in bdynets.
legacy_grid_main <- function(script) {
  args <- commandArgs(TRUE)
  if (identical(args, "--help")) {
    cat("Rscript GLS-paper/run_simulation.R --action=plan|run|report",
        "--profile=smoke|quick|full --cores=12 [--chunk-size=100]",
        "--output=/external/new-directory\n")
    return(invisible(NULL))
  }
  paper <- dirname(dirname(dirname(script)))
  helper_file <- file.path(paper, "R", "launcher_helpers.R")
  sys.source(helper_file, envir = environment())
  config <- gls_paper_options(args, dirname(paper))
  action <- config$action
  profile <- config$profile
  cores <- config$cores
  chunk_size <- config$chunk_size
  output <- config$output
  draw_dir <- config$draw_dir
  if (!requireNamespace("bdynets", quietly = TRUE)) stop("Install bdynets before using this launcher.")

  if (action == "report") {
    saved <- file.path(output, "GLS_simulation_summary.rds")
    study <- readRDS(saved)
    if (!inherits(study, "gls_grid_study")) stop("Saved object is not a GLS grid study.")
    for (i in seq_along(study$scenarios)) {
      store <- study$scenarios[[i]]$fit$draw_store
      if (!is.null(store)) {
        relocated <- file.path(draw_dir, basename(store$path))
        if (dir.exists(relocated)) study$scenarios[[i]]$fit$draw_store$path <- relocated
      }
    }
    # Export only: this package function reads the supplied saved study and
    # performs no fitting, prediction, continuation, or checkpoint cleanup.
    bdynets::gls_write_grid_study(study, output)
    cat("Rebuilt tables and figures from", saved, "\n")
    return(invisible(NULL))
  }
  control <- bdynets::gls_study_control(profile)
  workers <- gls_paper_chain_workers(control, cores)
  control$parallel_chains <- workers > 1L
  cat("Action:", action, "| profile:", profile,
      "| package:", as.character(utils::packageVersion("bdynets")), "\n")
  cat("Signals:", paste(control$signal_grid, collapse = ", "),
      "| times:", control$n_total, "| grid side:", control$grid_side, "\n")
  cat("Iterations per chain:", control$n_iter, "| warmup:", control$burn_mcmc,
      "| chains:", control$n_chains, "| chain workers:", workers,
      "| requested core limit:", cores, "\n")
  cat("Retained draws per chunk", chunk_size, "| draw directory",
      if (is.null(draw_dir)) "<output>/draws" else draw_dir, "\n")
  cat("Output:", if (is.null(output)) "<supply a new external directory>" else output, "\n")
  if (action == "plan") {
    cat("Plan only: no data generated, fitting, output writes, or benchmark processes.\n")
    return(invisible(NULL))
  }
  if (!dir.create(output)) stop("Could not create the new output directory.")
  provenance <- c(list(launcher = "GLS-paper/run_simulation.R", control = control),
                  gls_paper_provenance(config, script, helper_file, workers))
  saveRDS(provenance, file.path(output, "launcher_provenance.rds"))
  study <- tryCatch(bdynets::gls_run_grid_study(control, output_dir = output,
      draw_dir = draw_dir, chunk_size = chunk_size),
    error = function(e) {
      provenance$status <- "failed"; provenance$error <- conditionMessage(e)
      saveRDS(provenance, file.path(output, "launcher_provenance.rds"))
      stop(e)
    })
  expected_control <- study$control
  rm(study)
  invisible(gc())
  saved <- readRDS(file.path(output, "GLS_simulation_summary.rds"))
  stopifnot(inherits(saved, "gls_grid_study"), identical(saved$control, expected_control),
    all(vapply(saved$scenarios, function(scenario) !is.null(scenario$fit$draw_store), logical(1))))
  provenance$status <- "complete"; provenance$finished <- as.character(Sys.time())
  saveRDS(provenance, file.path(output, "launcher_provenance.rds"))
  cat("Saved study:", file.path(output, "GLS_simulation_summary.rds"), "\n")
}
if (sys.nframe() == 0L) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE))
  script <- gsub("~+~", " ", script, fixed = TRUE)
  legacy_grid_main(normalizePath(script))
}
