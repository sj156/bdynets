#!/usr/bin/env Rscript
# Thin paper-study launcher. All model and reporting code lives in bdynets.
main <- function() {
  args <- commandArgs(TRUE)
  if (identical(args, "--help")) {
    cat("Rscript GLS-paper/run_simulation.R --action=plan|run|report",
        "--profile=smoke|quick|full --cores=12 --output=/external/new-directory\n")
    return(invisible(NULL))
  }
  if (any(!grepl("^--[^=]+=.+$", args))) stop("Use --name=value options.")
  keys <- sub("^--([^=]+)=.*$", "\\1", args)
  if (anyDuplicated(keys) || any(!keys %in% c("action", "profile", "cores", "output")))
    stop("Unknown or repeated option.")
  opts <- stats::setNames(sub("^--[^=]+=", "", args), keys)
  option <- function(name, default = NULL) if (name %in% names(opts)) opts[[name]] else default
  action <- match.arg(option("action", "plan"), c("plan", "run", "report"))
  profile <- match.arg(option("profile", "smoke"), c("smoke", "quick", "full"))
  cores <- suppressWarnings(as.numeric(option("cores", "12")))
  if (length(cores) != 1L || !is.finite(cores) || cores < 1 || cores != floor(cores))
    stop("--cores must be a positive integer.")
  script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
  repo <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
  output <- option("output")
  if (!is.null(output)) {
    output <- path.expand(output)
    if (!grepl("^(/|[A-Za-z]:[/\\\\])", output))
      stop("--output must be an absolute external path.")
    parent <- normalizePath(dirname(output), winslash = "/", mustWork = TRUE)
    output <- file.path(parent, basename(output))
    if (startsWith(paste0(output, "/"), paste0(normalizePath(repo, winslash = "/"), "/")))
      stop("Keep run results outside the repository.")
    if (action != "report" && file.exists(output)) stop("Output already exists; choose a new directory.")
  } else if (action != "plan") stop("--output is required for run or report.")
  if (!requireNamespace("bdynets", quietly = TRUE)) stop("Install bdynets before using this launcher.")

  if (action == "report") {
    saved <- file.path(output, "GLS_simulation_summary.rds")
    study <- readRDS(saved)
    if (!inherits(study, "gls_grid_study")) stop("Saved object is not a GLS grid study.")
    # Export only: this package function reads the supplied saved study and
    # performs no fitting, prediction, continuation, or checkpoint cleanup.
    bdynets::gls_write_grid_study(study, output)
    cat("Rebuilt tables and figures from", saved, "\n")
    return(invisible(NULL))
  }
  control <- bdynets::gls_study_control(profile)
  physical <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (length(physical) != 1L || is.na(physical) || physical < 1L) physical <- 1L
  possible <- min(control$n_chains, physical)
  # The current package exposes an all-chains/serial switch. When the budget
  # cannot accommodate that parallel schedule, retain every chain and run
  # serially rather than reducing the declared chain count.
  control$parallel_chains <- isTRUE(control$parallel_chains) &&
    .Platform$OS.type != "windows" && cores >= possible && possible > 1L
  workers <- if (control$parallel_chains) possible else 1L
  cat("Action:", action, "| profile:", profile,
      "| package:", as.character(utils::packageVersion("bdynets")), "\n")
  cat("Signals:", paste(control$signal_grid, collapse = ", "),
      "| times:", control$n_total, "| grid side:", control$grid_side, "\n")
  cat("Iterations per chain:", control$n_iter, "| warmup:", control$burn_mcmc,
      "| chains:", control$n_chains, "| chain workers:", workers,
      "| requested core limit:", cores, "\n")
  cat("Output:", if (is.null(output)) "<supply a new external directory>" else output, "\n")
  if (action == "plan") {
    cat("Plan only: no data generated, fitting, output writes, or benchmark processes.\n")
    return(invisible(NULL))
  }
  if (!dir.create(output)) stop("Could not create the new output directory.")
  provenance <- list(
    launcher = "GLS-paper/run_simulation.R", profile = profile,
    control = control, requested_cores = cores, chain_workers = workers,
    package_version = as.character(utils::packageVersion("bdynets")),
    package_path = find.package("bdynets"), launcher_md5 = unname(tools::md5sum(script)),
    command = commandArgs(), started = as.character(Sys.time()),
    session = capture.output(utils::sessionInfo()), status = "started")
  saveRDS(provenance, file.path(output, "launcher_provenance.rds"))
  study <- tryCatch(bdynets::gls_run_grid_study(control, output_dir = output),
    error = function(e) {
      provenance$status <- "failed"; provenance$error <- conditionMessage(e)
      saveRDS(provenance, file.path(output, "launcher_provenance.rds"))
      stop(e)
    })
  saved <- readRDS(file.path(output, "GLS_simulation_summary.rds"))
  stopifnot(inherits(saved, "gls_grid_study"), identical(saved$control, study$control))
  provenance$status <- "complete"; provenance$finished <- as.character(Sys.time())
  saveRDS(provenance, file.path(output, "launcher_provenance.rds"))
  cat("Saved study:", file.path(output, "GLS_simulation_summary.rds"), "\n")
}
main()
