# A single-chain development runner. It is never called on source(), and the
# launcher defaults to deterministic checks. Historical checkpoints are rejected.
graphmode_digest <- function(object) {
    if (!requireNamespace("digest", quietly = TRUE))
        stop("Package 'digest' is required for source/checkpoint identities; do not auto-install.", call. = FALSE)
    digest::digest(object, algo = "sha256", serialize = TRUE)
}

graphmode_source_identity <- function(repository) {
    repository <- normalizePath(repository, mustWork = TRUE)
    files <- c("R/gmde-helpers.R", "R/gmde-state-update.R",
        file.path("R", sort(list.files(file.path(repository, "R"), pattern = "^graphmode-.*[.]R$"))),
        "scripts/graphmode.R", "scripts/tests/graphmode-deterministic.R")
    if (!requireNamespace("digest", quietly = TRUE))
        stop("Install-free preflight needs the existing 'digest' package.", call. = FALSE)
    if (!all(file.exists(file.path(repository, files)))) stop("Incomplete implementation source.", call. = FALSE)
    sha256 <- vapply(file.path(repository, files), function(f)
        digest::digest(file = f, algo = "sha256", serialize = FALSE), character(1))
    names(sha256) <- files
    git <- function(args) {
        out <- system2("git", c("-C", shQuote(repository), args), stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(out, "status"))) stop("Cannot resolve source Git identity.", call. = FALSE)
        out
    }
    commit <- git(c("rev-parse", "HEAD"))
    source_status <- git(c("status", "--porcelain", "--untracked-files=all", "--", shQuote(files)))
    tracked <- git(c("ls-files", "--", shQuote(files)))
    list(commit = commit, sha256 = sha256,
        committed = length(source_status) == 0L && setequal(files, tracked),
        sampler_version = graphmode_sampler_version, protocol = graphmode_protocol)
}

graphmode_run_plan <- function(config, initial_state, source_identity, seed,
    iterations, warmup, thin, checkpoint_every, budget_seconds,
    output_dir, registration_id, authorization_reference, role,
    diagnostic_thresholds, provenance) {
    graphmode_revalidate_config(config)
    graphmode_revalidate_state(initial_state, config)
    seed <- gmde_scalar_integer(seed, "registered chain seed", 1L)
    iterations <- gmde_scalar_integer(iterations, "iterations", 1L)
    warmup <- gmde_scalar_integer(warmup, "warmup", 0L, iterations - 1L)
    thin <- gmde_scalar_integer(thin, "thin", 1L, iterations - warmup)
    checkpoint_every <- gmde_scalar_integer(checkpoint_every, "checkpoint_every", 1L)
    graphmode_positive(budget_seconds, "budget_seconds")
    for (field in c("output_dir", "registration_id", "authorization_reference", "role")) {
        value <- get(field)
        if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value)))
            stop("Specify ", field, ".", call. = FALSE)
    }
    if (role != "development-pilot") stop("This entry supports separately authorized development pilots only; formal simulation is locked.", call. = FALSE)
    if (!is.list(diagnostic_thresholds) || !length(diagnostic_thresholds) ||
        is.null(names(diagnostic_thresholds)) || any(!nzchar(names(diagnostic_thresholds))))
        stop("Register named diagnostic thresholds before running.", call. = FALSE)
    # These are source/data identities and recorded choices, never truth labels
    # for initialization/tuning. Include data, graph, response and permutation
    # seeds (or their immutable source records) and calibration decision IDs.
    required <- c("data_identity", "geometry_identity", "data_seed_record",
                  "permutation_seed_record", "calibration_decision", "scientific_role")
    if (!is.list(provenance) || !all(required %in% names(provenance)) ||
        any(vapply(provenance[required], function(x)
            !is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)), logical(1))))
        stop("Complete data/geometry/seed/calibration provenance is required.", call. = FALSE)
    plan <- list(config = config, initial_state = initial_state, source_identity = source_identity,
        seed = seed, iterations = iterations, warmup = warmup, thin = thin,
        checkpoint_every = checkpoint_every, budget_seconds = budget_seconds,
        output_dir = output_dir, registration_id = registration_id,
        authorization_reference = authorization_reference, role = role,
        diagnostic_thresholds = diagnostic_thresholds, provenance = provenance,
        rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"),
        schema = "graphmode-development-run-v1")
    plan$signature <- graphmode_digest(plan)
    plan
}

graphmode_revalidate_plan <- function(plan) {
    if (!is.list(plan) || !identical(plan$schema, "graphmode-development-run-v1"))
        stop("Incompatible plan; old checkpoints/plans cannot be reused.", call. = FALSE)
    rebuilt <- do.call(graphmode_run_plan, plan[names(formals(graphmode_run_plan))])
    if (!identical(rebuilt, plan)) stop("Run plan signature or contents changed.", call. = FALSE)
    plan
}

graphmode_preflight <- function(plan, repository) {
    graphmode_revalidate_plan(plan)
    repository <- normalizePath(repository, mustWork = TRUE)
    identity <- graphmode_source_identity(repository)
    problems <- character()
    if (!identical(identity, plan$source_identity)) problems <- c(problems, "Recorded source identity differs from current files.")
    if (!isTRUE(identity$committed)) problems <- c(problems, "Implementation files are not committed at the recorded Git revision.")
    # A previously installed package or a modified RNG hook must not claim the
    # identity of freshly read source merely because version strings agree.
    expected <- new.env(parent = baseenv())
    for (file in names(identity$sha256)[startsWith(names(identity$sha256), "R/")])
        sys.source(file.path(repository, file), envir = expected)
    symbols <- ls(expected, all.names = TRUE)
    describe <- function(envir) lapply(mget(symbols, envir = envir, inherits = TRUE),
        function(x) if (is.function(x)) list(formals = formals(x), body = body(x)) else x)
    if (!identical(graphmode_digest(describe(expected)),
                   graphmode_digest(describe(environment(graphmode_preflight)))))
        problems <- c(problems, "Loaded functions differ from the registered source; use the fresh Terminal launcher.")
    if (plan$config$family == "poisson" && !requireNamespace("BayesLogit", quietly = TRUE))
        problems <- c(problems, "Exact BayesLogit PG backend is unavailable; no fallback allowed.")
    destination <- path.expand(plan$output_dir)
    if (!grepl("^/", destination)) problems <- c(problems, "Output path must be absolute.")
    parent <- dirname(destination)
    if (!dir.exists(parent)) {
        problems <- c(problems, "Output parent must already exist; preflight creates nothing.")
    } else {
        destination <- file.path(normalizePath(parent, mustWork = TRUE), basename(destination))
        if (file.exists(destination)) problems <- c(problems, "Output destination already exists; use a new external directory.")
        if (identical(destination, repository) || startsWith(destination, paste0(repository, "/")))
            problems <- c(problems, "Experiment outputs must be external to the repository.")
    }
    list(ready = !length(problems), problems = problems, output_dir = destination,
        signature = plan$signature, source_identity = identity,
        runtime = list(R = R.version.string, platform = R.version$platform,
            libraries = extSoftVersion(), rng_kind = plan$rng_kind,
            BayesLogit = if (requireNamespace("BayesLogit", quietly = TRUE))
                as.character(utils::packageVersion("BayesLogit")) else NA_character_,
            digest = as.character(utils::packageVersion("digest"))),
        note = "Read-only checks do not grant run authorization or certify convergence.")
}

graphmode_save_new <- function(object, path) {
    # Unique immutable files; never overwrite checkpoints or collaborator data.
    connection <- file(path, open = "wxb")
    closed <- FALSE
    on.exit(if (!closed) close(connection), add = TRUE)
    saveRDS(object, connection, version = 3)
    close(connection)
    closed <- TRUE
    if (!identical(graphmode_digest(readRDS(path)), graphmode_digest(object)))
        stop("Saved file failed its content audit; retain it for inspection.", call. = FALSE)
    invisible(path)
}

graphmode_run <- function(plan, repository, authorized = FALSE) {
    if (!identical(authorized, TRUE)) stop("Explicit separate run authorization is required.", call. = FALSE)
    checked <- graphmode_preflight(plan, repository)
    if (!checked$ready) stop(paste(checked$problems, collapse = "\n"), call. = FALSE)
    destination <- checked$output_dir
    if (!dir.create(destination, recursive = FALSE)) stop("Cannot create new output directory.", call. = FALSE)
    # Preserve the caller's random stream; checkpoint the run's stream itself.
    original_kind <- RNGkind()
    original_rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
        get(".Random.seed", .GlobalEnv) else NULL
    on.exit({
        do.call(RNGkind, as.list(original_kind))
        if (is.null(original_rng)) {
            if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
        } else assign(".Random.seed", original_rng, .GlobalEnv)
    }, add = TRUE)
    state <- plan$initial_state
    saved <- list()
    diagnostics <- list()
    started <- proc.time()[[3L]]
    checkpoint <- function(status, error = NULL) list(schema = "graphmode-checkpoint-v1",
        sampler_version = graphmode_sampler_version, protocol = graphmode_protocol,
        plan_signature = plan$signature, source_identity = checked$source_identity,
        runtime = checked$runtime, state = state,
        rng = get(".Random.seed", .GlobalEnv), saved = saved, diagnostics = diagnostics,
        status = status, error = error, elapsed_seconds = proc.time()[[3L]] - started,
        resume_supported = FALSE)
    do.call(RNGkind, as.list(plan$rng_kind))
    set.seed(plan$seed)
    tryCatch({
        graphmode_save_new(list(plan = plan, preflight = checked), file.path(destination, "registration.rds"))
        graphmode_save_new(checkpoint("initialized"), file.path(destination, "checkpoint-000000000.rds"))
        for (iteration in seq_len(plan$iterations)) {
            if (proc.time()[[3L]] - started >= plan$budget_seconds)
                stop("Registered wall-time budget exhausted; retain this incomplete run.", call. = FALSE)
            out <- graphmode_sweep(state, plan$config)
            state <- out$state
            diagnostics[[iteration]] <- list(iteration = iteration, sizes = out$sizes,
                sorted_sizes = out$sorted_sizes, Kocc = out$Kocc,
                fragmentation = out$fragmentation, events = out$events, potts_gross = out$potts_gross,
                ess_evaluations = out$ess_evaluations, guidance_accept = out$guidance_accept,
                expert = lapply(out$expert_updates, function(x) x[c("empty", "accepted", "log_acceptance",
                    "movement", "seconds", "factor_residual", "root_residual",
                    "root_reciprocal_condition")]))
            if (iteration > plan$warmup && (iteration - plan$warmup) %% plan$thin == 0L) {
                saved[[length(saved) + 1L]] <- list(iteration = iteration, Z = state$Z,
                    mean_profile = if (plan$config$family == "poisson") exp(out$eta) else out$eta,
                    theta = state$theta, sigma2 = state$sigma2, v = state$v,
                    guidance_weights = if (plan$config$adaptive)
                        graphmode_guidance_factors(state$v, plan$config$K, plan$config$guidance)$a else NULL,
                    pi = state$pi)
            }
            if (iteration %% plan$checkpoint_every == 0L)
                graphmode_save_new(checkpoint("in-progress"), file.path(destination,
                    sprintf("checkpoint-%09d.rds", iteration)))
        }
        M <- length(saved)
        means <- array(0, c(M, plan$config$K, nrow(plan$config$Fmat)))
        Z <- matrix(0, M, plan$config$n)
        for (m in seq_len(M)) {means[m, , ] <- saved[[m]]$mean_profile; Z[m, ] <- saved[[m]]$Z}
        summary <- graphmode_summarize(Z, means)
        graphmode_save_new(list(checkpoint = checkpoint("completed-not-convergence-certified"),
            summary = summary, summary_signature = graphmode_digest(summary)),
            file.path(destination, "result.rds"))
        invisible(list(output_dir = destination, iterations = state$iteration,
            retained_draws = M, status = "completed-not-convergence-certified"))
    }, error = function(e) {
        # The last complete state and the RNG at failure are forensic evidence,
        # not a silently resumable state (a failed sweep may have consumed RNG).
        failure <- checkpoint("failed-retained-do-not-resume", conditionMessage(e))
        tryCatch(graphmode_save_new(failure, file.path(destination, "failure.rds")),
                 error = function(save_error) warning(conditionMessage(save_error)))
        stop(conditionMessage(e), call. = FALSE)
    })
}
