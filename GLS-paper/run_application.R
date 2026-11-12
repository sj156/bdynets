#!/usr/bin/env Rscript
# Prepared-data orchestration only. GLS fitting, DSS and prediction are bdynets APIs.
main <- function() {
  args <- commandArgs(TRUE)
  if (identical(args, "--help")) {
    cat("Rscript GLS-paper/run_application.R --action=plan|run|report",
        "--profile=smoke|quick|full --cores=12 [--chunk-size=100] --input=prepared.rds",
        "[--data-root=/external/data] --output=/external/new-directory\n")
    return(invisible(NULL))
  }
  if (any(!grepl("^--[^=]+=.+$", args))) stop("Use --name=value options.")
  keys <- sub("^--([^=]+)=.*$", "\\1", args)
  if (anyDuplicated(keys) ||
      any(!keys %in% c("action", "profile", "cores", "chunk-size", "input", "data-root", "output")))
    stop("Unknown or repeated option.")
  opts <- stats::setNames(sub("^--[^=]+=", "", args), keys)
  option <- function(name, default = NULL) if (name %in% names(opts)) opts[[name]] else default
  action <- match.arg(option("action", "plan"), c("plan", "run", "report"))
  profile <- match.arg(option("profile", "smoke"), c("smoke", "quick", "full"))
  cores <- suppressWarnings(as.numeric(option("cores", "12")))
  if (length(cores) != 1L || !is.finite(cores) || cores < 1 || cores != floor(cores))
    stop("--cores must be a positive integer.")
  chunk_size <- suppressWarnings(as.numeric(option("chunk-size", "100")))
  if (!is.finite(chunk_size) || chunk_size < 1 || chunk_size != floor(chunk_size) ||
      chunk_size > .Machine$integer.max)
    stop("--chunk-size must be a positive integer.")
  chunk_size <- as.integer(chunk_size)
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
  draw_dir <- if (is.null(output)) NULL else file.path(output, "draws")
  if (!requireNamespace("bdynets", quietly = TRUE)) stop("Install bdynets before using this launcher.")

  if (action == "report") {
    saved <- readRDS(file.path(output, "application_results.rds"))
    if (!identical(saved$schema, "GLS-paper-application-v1") ||
        !inherits(saved$fit, "gls_general_fit")) stop("Unrecognized saved application.")
    if (!is.null(saved$fit$draw_store)) {
      relocated <- file.path(draw_dir, basename(saved$fit$draw_store$path))
      if (dir.exists(relocated)) saved$fit$draw_store$path <- relocated
    }
    # Summary exports use saved posterior and predictive draws only.
    frames <- lapply(saved$predictions, function(pred) {
      mu <- apply(pred$lambda, c(1L, 2L), mean)
      lo <- apply(pred$y, c(1L, 2L), stats::quantile, .025)
      hi <- apply(pred$y, c(1L, 2L), stats::quantile, .975)
      data.frame(model = pred$label, row = rep(pred$rows, ncol(saved$fit$Y)),
        series = rep(seq_len(ncol(saved$fit$Y)), each = length(pred$rows)),
        observed = as.vector(saved$fit$Y[pred$rows, , drop = FALSE]),
        mean_intensity = as.vector(mu), count_lower95 = as.vector(lo),
        count_upper95 = as.vector(hi))
    })
    utils::write.csv(do.call(rbind, frames), file.path(output, "predictions.csv"), row.names = FALSE)
    utils::write.csv(bdynets::gls_diagnostics(saved$fit),
      file.path(output, "MCMC_diagnostics.csv"), row.names = FALSE)
    utils::write.csv(data.frame(saved$fit$pairs, multiplier = saved$dss$d),
      file.path(output, "DSS_selection.csv"), row.names = FALSE)
    dput(unclass(saved$control), file.path(output, "configuration.R"))
    cat("Exported saved prediction summaries, diagnostics and DSS selection to", output, "\n")
    return(invisible(NULL))
  }
  input_path <- option("input")
  if (is.null(input_path)) {
    if (action == "run") stop("--input is required for run.")
    cat("Plan only. Supply --input=prepared.rds with Y, graph, covariates,",
        "split=list(train=..., validation=..., test=...), and optional control.\n")
    cat("Profile:", profile, "| requested core limit:", cores,
        "| output:", if (is.null(output)) "<new external directory>" else output, "\n")
    cat("Retained draws per chunk", chunk_size, "| draw directory",
        if (is.null(draw_dir)) "<output>/draws" else draw_dir, "\n")
    return(invisible(NULL))
  }
  if (!is.null(option("data-root"))) {
    data_root <- normalizePath(path.expand(option("data-root")), mustWork = TRUE)
    if (!dir.exists(data_root)) stop("--data-root must name a directory.")
    if (!grepl("^(/|[A-Za-z]:[/\\\\])", path.expand(input_path)))
      input_path <- file.path(data_root, input_path)
  }
  input_path <- normalizePath(path.expand(input_path), mustWork = TRUE)
  if (dir.exists(input_path)) stop("--input must name an RDS file.")
  input <- readRDS(input_path)
  if (!is.list(input) || !all(c("Y", "graph", "covariates", "split") %in% names(input)))
    stop("Prepared input requires Y, graph, covariates and split.")
  Y <- as.matrix(input$Y); Q <- as.matrix(input$covariates)
  if (!is.numeric(Y) || nrow(Y) < 5L || ncol(Y) < 1L ||
      any(!is.finite(Y)) || any(Y < 0 | Y != floor(Y)))
    stop("Y must be a finite, nonnegative integer time-by-series matrix.")
  if (!is.numeric(Q) || nrow(Q) != nrow(Y) || any(!is.finite(Q)))
    stop("covariates must be finite and have nrow(Y) rows.")
  if (!is.list(input$split) || !all(c("train", "validation", "test") %in% names(input$split)))
    stop("split requires train, validation and test row vectors.")
  split <- input$split[c("train", "validation", "test")]
  for (name in names(split)) {
    z <- split[[name]]
    if (!is.numeric(z) || !length(z) || any(!is.finite(z)) ||
        any(z != floor(z)) || any(z < 1 | z > nrow(Y)) || any(diff(z) != 1))
      stop("Each split must contain consecutive, increasing valid row integers.")
    split[[name]] <- as.integer(z)
  }
  if (length(split$train) < 3L ||
      min(split$validation) != max(split$train) + 1L ||
      min(split$test) != max(split$validation) + 1L)
    stop("Require at least three training rows followed immediately by validation and test windows.")
  replacements <- if (is.null(input$control)) list() else input$control
  if (!is.list(replacements) || (length(replacements) &&
      (is.null(names(replacements)) || any(!nzchar(names(replacements))) || anyDuplicated(names(replacements)))))
    stop("Optional control must be a named list of gls_control settings.")
  control <- do.call(bdynets::gls_control,
    c(list(profile = if (profile == "full") "default" else profile), replacements))
  physical <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (length(physical) != 1L || is.na(physical) || physical < 1L) physical <- 1L
  possible <- min(control$n_chains, physical)
  windows_parallel <- utils::packageVersion("bdynets") >= "0.2.1"
  control$parallel_chains <- isTRUE(control$parallel_chains) &&
    (.Platform$OS.type != "windows" || windows_parallel) && cores >= possible && possible > 1L
  workers <- if (control$parallel_chains) possible else 1L
  input_md5 <- unname(tools::md5sum(input_path))
  cat("Action:", action, "| profile:", profile, "| input:", input_path, "\n")
  cat("Times:", nrow(Y), "| series:", ncol(Y), "| covariates:", ncol(Q),
      "| split sizes:", paste(vapply(split, length, integer(1)), collapse = "/"), "\n")
  cat("Iterations per chain:", control$n_iter, "| warmup:", control$burn_mcmc,
      "| chains:", control$n_chains, "| chain workers:", workers, "\n")
  cat("Retained draws per chunk", chunk_size, "| draw directory",
      if (is.null(draw_dir)) "<output>/draws" else draw_dir, "\n")
  cat("Input MD5:", input_md5, "| output:", if (is.null(output)) "<new external directory>" else output, "\n")
  if (action == "plan") {
    cat("Plan only: prepared input and controls read; no fit, prediction, or output writes.\n")
    return(invisible(NULL))
  }
  if (!"draw_dir" %in% names(formals(bdynets::gls_fit_general)))
    stop("Install bdynets with GLS draw storage before running this launcher.")
  if (!dir.create(output)) stop("Could not create the new output directory.")
  provenance <- list(input_path = input_path, input_md5 = input_md5,
    package_version = as.character(utils::packageVersion("bdynets")),
    package_path = find.package("bdynets"), launcher_md5 = unname(tools::md5sum(script)),
    profile = profile, requested_cores = cores, chain_workers = workers,
    draw_dir = draw_dir, chunk_size = chunk_size,
    command = commandArgs(), started = as.character(Sys.time()),
    session = capture.output(utils::sessionInfo()), status = "started")
  saveRDS(list(control = control, split = split, provenance = provenance),
    file.path(output, "application_configuration.rds"))
  results <- tryCatch(local({
    fit_args <- list(Y = Y, graph = input$graph, covariates = Q,
                     train_rows = split$train, control = control,
                     draw_dir = draw_dir, chunk_size = chunk_size)
    for (name in c("pre_history", "pairs", "means"))
      if (!is.null(input[[name]])) fit_args[[name]] <- input[[name]]
    fit <- do.call(bdynets::gls_fit_general, fit_args)
    if (all(vapply(fit$chains, function(chain) !is.null(chain$worker_pid), logical(1)))) {
      provenance$chain_worker_pids <- vapply(fit$chains, `[[`, integer(1), "worker_pid")
      provenance$chain_workers <- length(unique(provenance$chain_worker_pids))
    }
    posterior <- bdynets::gls_load_draws(fit)
    dss <- bdynets::gls_dss(posterior, split$validation)
    predictions <- list(
      continuous = stats::predict(posterior, rows = split$test,
        conditioning_rows = split$validation, label = "GLS continuous"),
      selected = stats::predict(posterior, rows = split$test,
        conditioning_rows = split$validation, d = dss$d, label = "GLS joint DSS"))
    rm(posterior)
    invisible(gc())
    list(schema = "GLS-paper-application-v1", fit = fit, dss = dss,
         predictions = predictions, control = control, split = split,
         provenance = provenance)
  }), error = function(e) {
    provenance$status <- "failed"; provenance$error <- conditionMessage(e)
    saveRDS(provenance, file.path(output, "application_failure.rds"))
    stop(e)
  })
  results$provenance$status <- "complete"
  results$provenance$finished <- as.character(Sys.time())
  temporary <- tempfile("application_", tmpdir = output, fileext = ".rds")
  saveRDS(results, temporary, compress = "xz")
  if (!file.rename(temporary, file.path(output, "application_results.rds")))
    stop("Could not finalize saved application; temporary results were preserved.")
  rm(results)
  invisible(gc())
  check <- readRDS(file.path(output, "application_results.rds"))
  stopifnot(identical(check$provenance$input_md5, input_md5),
            identical(check$control, control), inherits(check$fit, "gls_general_fit"),
            !is.null(check$fit$draw_store))
  cat("Saved fit, DSS and predictions:", file.path(output, "application_results.rds"), "\n")
}
main()
