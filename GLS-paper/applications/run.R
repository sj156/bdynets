#!/usr/bin/env Rscript
# Prepared-data orchestration only. GLS fitting, DSS and prediction are bdynets APIs.
# Commit files on the same filesystem so readers see complete artifacts.
application_atomic <- function(path, writer) {
  temp <- tempfile(".application-", tmpdir = dirname(path))
  on.exit(unlink(temp), add = TRUE)
  writer(temp)
  if (!file.rename(temp, path)) stop("Could not publish application artifact: ", path)
}

application_identity <- function(input_md5, control, profile, retention, benchmarks,
                                 chunk_size, checkpoint_every, cores, script, paper, legacy) {
  files <- c(launcher = script)
  if (any(benchmarks %in% c("sa-pgss", "graph-wavenet")))
    files["external"] <- file.path(paper, "experiments", "external.R")
  if ("graph-wavenet" %in% benchmarks)
    files["network"] <- file.path(paper, "experiments", "graph_wavenet.py")
  pkgs <- c("bdynets", "BayesLogit")
  for (pkg in pkgs) {
    root <- find.package(pkg)
    paths <- list.files(root, recursive = TRUE)
    paths <- paths[grepl("^(R/.*[.](rdb|rdx)|libs/.*[.](dll|so|dylib)|NAMESPACE)$", paths)]
    if (!length(paths)) stop("Cannot fingerprint the installed ", pkg, " package.")
    files <- c(files, setNames(file.path(root, paths), paste(pkg, paths, sep = "/")))
  }
  python <- NULL
  if ("graph-wavenet" %in% benchmarks) {
    exe <- Sys.getenv("BDYNETS_PYTHON", unset = "python3")
    code <- "import sys,torch,numpy; print(sys.version,torch.__version__,numpy.__version__)"
    python <- system2(exe, c("-c", shQuote(code)), stdout = TRUE)
    if (!is.null(attr(python, "status"))) stop("Cannot fingerprint the Graph WaveNet Python environment.")
  }
  hashes <- vapply(files, function(path) digest::digest(file = path, algo = "sha256"), character(1))
  list(format = 1L, input_md5 = input_md5, control = control, profile = profile,
       retention = retention, benchmarks = benchmarks, chunk_size = chunk_size,
       checkpoint_every = checkpoint_every, requested_cores = cores, legacy = legacy,
       source_sha256 = hashes, R = R.version.string, platform = R.version$platform,
       math_libraries = extSoftVersion(), rng = RNGkind(), python = python)
}

application_verify_completion <- function(output, identity) {
  result <- file.path(output, "application_results.rds")
  marker <- file.path(output, "application_completion.rds")
  if (!file.exists(result)) return(FALSE)
  if (!file.exists(marker)) stop("The saved result has no completion identity; use report for legacy results.")
  commit <- readRDS(marker)
  if (!identical(commit$identity, identity) ||
      !identical(commit$result_sha256, digest::digest(file = result, algo = "sha256")))
    stop("Completed application identity or result checksum differs.")
  files <- file.path(output, "draws", commit$store, commit$draw_files)
  if (!identical(unname(tools::md5sum(files)), commit$draw_hashes))
    stop("A completed application's posterior draw chunk is missing or corrupt.")
  TRUE
}

main <- function() {
  args <- commandArgs(TRUE)
  if (identical(args, "--help")) {
    cat("Rscript GLS-paper/applications/run.R --action=plan|run|resume|report",
        "--profile=smoke|quick|full --cores=12 [--chunk-size=100] --input=prepared.rds",
        "[--checkpoint-every=100] [--benchmarks=pnar1,pnar2] [--retention=compact|draws] --output=/external/run\n")
    return(invisible(NULL))
  }
  if (any(!grepl("^--[^=]+=.+$", args))) stop("Use --name=value options.")
  keys <- sub("^--([^=]+)=.*$", "\\1", args)
  if (anyDuplicated(keys) ||
      any(!keys %in% c("action", "profile", "cores", "chunk-size", "input", "data-root", "output",
                          "checkpoint-every", "benchmarks", "retention")))
    stop("Unknown or repeated option.")
  opts <- stats::setNames(sub("^--[^=]+=", "", args), keys)
  option <- function(name, default = NULL) if (name %in% names(opts)) opts[[name]] else default
  action <- match.arg(option("action", "plan"), c("plan", "run", "resume", "report"))
  profile <- match.arg(option("profile", "smoke"), c("smoke", "quick", "full"))
  cores <- suppressWarnings(as.numeric(option("cores", "12")))
  if (length(cores) != 1L || !is.finite(cores) || cores < 1 || cores > .Machine$integer.max || cores != floor(cores))
    stop("--cores must be a positive integer.")
  chunk_size <- suppressWarnings(as.numeric(option("chunk-size", "100")))
  if (!is.finite(chunk_size) || chunk_size < 1 || chunk_size != floor(chunk_size) ||
      chunk_size > .Machine$integer.max)
    stop("--chunk-size must be a positive integer.")
  chunk_size <- as.integer(chunk_size)
  script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
  script <- normalizePath(gsub("~+~", " ", script, fixed = TRUE), mustWork = TRUE)
  legacy <- basename(script) == "run_application.R"
  if (legacy) script <- file.path(dirname(script), "applications", "run.R")
  paper <- dirname(dirname(script))
  repo <- dirname(paper)
  retention <- match.arg(option("retention", if (legacy) "draws" else "compact"),
                         c("compact", "draws"))
  checkpoint_every <- suppressWarnings(as.numeric(option("checkpoint-every", "100")))
  if (!is.finite(checkpoint_every) || checkpoint_every < 1 ||
      checkpoint_every != floor(checkpoint_every) || checkpoint_every > .Machine$integer.max)
    stop("--checkpoint-every must be a positive integer.")
  benchmarks <- strsplit(option("benchmarks", if (legacy) "none" else "pnar1,pnar2"), ",", fixed = TRUE)[[1L]]
  if (identical(benchmarks, "none")) benchmarks <- character()
  if (anyDuplicated(benchmarks) || any(!benchmarks %in% c("pnar1", "pnar2", "sa-pgss", "graph-wavenet")))
    stop("Unknown or duplicate benchmark.")
  output <- option("output")
  if (!is.null(output)) {
    output <- path.expand(output)
    if (!grepl("^(/|[A-Za-z]:[/\\\\])", output))
      stop("--output must be an absolute external path.")
    parent <- normalizePath(dirname(output), winslash = "/", mustWork = TRUE)
    output <- file.path(parent, basename(output))
    if (startsWith(paste0(output, "/"), paste0(normalizePath(repo, winslash = "/"), "/")))
      stop("Keep run results outside the repository.")
    if (action %in% c("plan", "run") && file.exists(output)) stop("Output already exists; choose a new directory.")
  } else if (action != "plan") stop("--output is required for run or report.")
  draw_dir <- if (is.null(output)) NULL else file.path(output, "draws")
  if (!requireNamespace("bdynets", quietly = TRUE)) stop("Install bdynets before using this launcher.")

  if (action == "report") {
    saved <- readRDS(file.path(output, "application_results.rds"))
    if (!saved$schema %in% c("GLS-paper-application-v1", "GLS-paper-application-v2") ||
        !inherits(saved$fit, "gls_general_fit")) stop("Unrecognized saved application.")
    if (!is.null(saved$fit$draw_store)) {
      relocated <- file.path(draw_dir, basename(saved$fit$draw_store$path))
      if (dir.exists(relocated)) saved$fit$draw_store$path <- relocated
    }
    # Summary exports use saved posterior and predictive draws only.
    frames <- saved$forecast_summary
    if (is.null(frames)) frames <- do.call(rbind, lapply(saved$predictions,
      bdynets::gls_forecast_summary, Y = saved$fit$Y, quantile_type = 7L))
    names(frames)[match(c("count_lower", "count_upper"), names(frames))] <-
      c("count_lower95", "count_upper95")
    application_atomic(file.path(output, "predictions.csv"), function(path) utils::write.csv(frames, path, row.names = FALSE))
    diagnostics <- saved$diagnostics
    if (is.null(diagnostics)) diagnostics <- bdynets::gls_diagnostics(saved$fit)
    application_atomic(file.path(output, "MCMC_diagnostics.csv"), function(path) utils::write.csv(diagnostics, path, row.names = FALSE))
    application_atomic(file.path(output, "DSS_selection.csv"), function(path)
      utils::write.csv(data.frame(saved$fit$pairs, multiplier = saved$dss$d), path, row.names = FALSE))
    application_atomic(file.path(output, "configuration.R"), function(path) dput(unclass(saved$control), path))
    cat("Exported saved prediction summaries, diagnostics and DSS selection to", output, "\n")
    return(invisible(NULL))
  }
  input_path <- option("input")
  if (is.null(input_path)) {
    if (action != "plan") stop("--input is required for run or resume.")
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
  input_md5 <- unname(tools::md5sum(input_path))
  input <- readRDS(input_path)
  if (!identical(input_md5, unname(tools::md5sum(input_path)))) stop("Input changed while it was read.")
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
  if (!legacy && is.null(replacements$parallel_chains)) control$parallel_chains <- TRUE
  workers <- if (isTRUE(control$parallel_chains)) min(control$n_chains, physical, cores) else 1L
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
  if (!"max_workers" %in% names(formals(bdynets::gls_fit_general)))
    stop("Install bdynets with bounded GLS chain workers before running this launcher.")
  checkpoint_dir <- file.path(output, "checkpoints")
  configuration <- file.path(output, "application_configuration.rds")
  identity <- application_identity(input_md5, control, profile, retention, benchmarks,
    chunk_size, checkpoint_every, cores, script, paper, legacy)
  if (action == "run" && !dir.create(output)) stop("Could not create the new output directory.")
  run_lock <- filelock::lock(file.path(output, ".application.lock"), timeout = 0)
  if (is.null(run_lock)) stop("Another worker is running this application.")
  on.exit(filelock::unlock(run_lock), add = TRUE)
  if (action == "resume") {
    previous <- readRDS(configuration)
    if (!identical(previous$identity, identity))
      stop("Input, options, code, or runtime differs from the saved application configuration.")
    if (application_verify_completion(output, identity)) {
      cat("Application already complete:", output, "\n")
      return(invisible(NULL))
    }
  }
  provenance <- list(input_path = input_path, input_md5 = input_md5,
    package_version = as.character(utils::packageVersion("bdynets")),
    package_path = find.package("bdynets"), launcher_md5 = unname(tools::md5sum(script)),
    case_id = input$case_id, input_provenance = input$provenance,
    benchmarks = benchmarks, retention = retention, checkpoint_every = checkpoint_every,
    profile = profile, requested_cores = cores, chain_workers = workers,
    draw_dir = draw_dir, chunk_size = chunk_size,
    command = commandArgs(), started = as.character(Sys.time()),
    session = capture.output(utils::sessionInfo()), status = "started")
  if (action == "run") application_atomic(configuration, function(path)
    saveRDS(list(identity = identity, control = control, split = split, provenance = provenance), path))
  if (action == "resume") provenance$started <- previous$provenance$started
  results <- tryCatch(local({
    fit_args <- list(Y = Y, graph = input$graph, covariates = Q,
                     train_rows = split$train, control = control,
                     draw_dir = draw_dir, chunk_size = chunk_size, max_workers = cores,
                     checkpoint_dir = checkpoint_dir, checkpoint_every = checkpoint_every,
                     resume = action == "resume" && file.exists(file.path(checkpoint_dir, "run.rds")))
    for (name in c("pre_history", "pairs", "means"))
      if (!is.null(input[[name]])) fit_args[[name]] <- input[[name]]
    fit <- do.call(bdynets::gls_fit_general, fit_args)
    if (all(vapply(fit$chains, function(chain) !is.null(chain$worker_pid), logical(1)))) {
      provenance$chain_worker_pids <- vapply(fit$chains, `[[`, integer(1), "worker_pid")
    }
    dss <- bdynets::gls_dss(fit, split$validation)
    forecast <- function(d, label) stats::predict(fit, rows = split$test,
      conditioning_rows = split$validation, d = d, label = label)
    projections <- list(continuous = NULL, selected = dss$d)
    labels <- c(continuous = "GLS continuous", selected = "GLS joint DSS")
    predictions <- list()
    frames <- list()
    for (model in names(projections)) {
      prediction <- forecast(projections[[model]], labels[[model]])
      frames[[model]] <- bdynets::gls_forecast_summary(prediction, Y, quantile_type = if (legacy) 7L else 1L)
      if (retention == "draws") predictions[[model]] <- prediction
      rm(prediction)
    }
    if (any(benchmarks %in% c("sa-pgss", "graph-wavenet")))
      source(file.path(paper, "experiments", "external.R"), local = TRUE)
    obs <- Y[split$test, , drop = FALSE]
    for (method in benchmarks) {
      b <- if (method %in% c("pnar1", "pnar2")) {
        bdynets::gls_pnar_forecast(Y, input$graph, Q, split$train, split$test,
          order = if (method == "pnar1") 1L else 2L, standardize = TRUE,
          pre_history = input$pre_history)
      } else external_forecast(method, input, profile, control$seed, output)
      frames[[method]] <- data.frame(model = method, row = rep(split$test, ncol(Y)),
        series = rep(seq_len(ncol(Y)), each = nrow(obs)), observed = as.vector(obs),
        mean_intensity = as.vector(b$mean),
        count_lower = if (is.null(b$lower)) NA_real_ else as.vector(b$lower),
        count_upper = if (is.null(b$upper)) NA_real_ else as.vector(b$upper),
        level = .95, log_score = if (is.null(b$logprob)) NA_real_ else as.vector(b$logprob))
    }
    list(schema = if (retention == "draws") "GLS-paper-application-v1" else "GLS-paper-application-v2",
         fit = fit, dss = dss, predictions = predictions, forecast_summary = do.call(rbind, frames),
         diagnostics = bdynets::gls_diagnostics(fit), control = control, split = split,
         case_id = input$case_id, provenance = provenance)
  }), error = function(e) {
    provenance$status <- "failed"; provenance$error <- conditionMessage(e)
    application_atomic(file.path(output, "application_failure.rds"), function(path) saveRDS(provenance, path))
    stop(e)
  })
  results$provenance$status <- "complete"
  results$provenance$finished <- as.character(Sys.time())
  temporary <- tempfile("application_", tmpdir = output, fileext = ".rds")
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(results, temporary, compress = "xz")
  indexes <- lapply(results$fit$chains, "[[", "draw_chunks")
  completion <- list(identity = identity, result_sha256 = digest::digest(file = temporary, algo = "sha256"),
    store = basename(results$fit$draw_store$path), draw_files = unlist(lapply(indexes, "[[", "files"), use.names = FALSE),
    draw_hashes = unlist(lapply(indexes, "[[", "hashes"), use.names = FALSE))
  # A result appears only after its verifiable completion record is committed.
  application_atomic(file.path(output, "application_completion.rds"), function(path) saveRDS(completion, path))
  if (!file.rename(temporary, file.path(output, "application_results.rds")))
    stop("Could not finalize saved application.")
  rm(results)
  invisible(gc())
  check <- readRDS(file.path(output, "application_results.rds"))
  stopifnot(identical(check$provenance$input_md5, input_md5),
            identical(check$control, control), inherits(check$fit, "gls_general_fit"),
            !is.null(check$fit$draw_store))
  cat("Saved fit, DSS and predictions:", file.path(output, "application_results.rds"), "\n")
}
main()
