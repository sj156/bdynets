# First real-backend/short-chain development screen. Source and prepare are
# deterministic. Only the separately authorized run entry consumes randomness.
# The independently audited v2 sampler files are not modified by this wrapper.
graphmode_pilot_version <- "graphmode-first-smoke-2026-09-11-v1"

graphmode_pilot_spec <- function() {
    list(version = graphmode_pilot_version, role = "development-pilot",
        geometry = "rings", n = 121L, TT = 168L, Ktrue = 5L, Kfit = 10L,
        method = "graphMoDE-W", q = 4L, nu = 1, kappa = sqrt(2), tau = 1, s_b = 1,
        amplitude = 0.3, innovation_variance = 0.002,
        m0 = c(log(25), 0, 0), C0 = diag(c(1, 0.5, 0.5)),
        guidance = "class-specific", guidance_prior = c(1, 1),
        guidance_proposal_sd = 0.25, rho = 2L, max_ess_steps = 1000L,
        iterations = 100L, warmup = 50L, thin = 1L, checkpoint_every = 25L,
        chain_budget_seconds = 600, total_budget_seconds = 1800,
        pg_budget_seconds = 300, workers = 1L,
        seeds = c(innovations = 2026091101L, profile_permutation = 2026091102L,
            unit_permutation = 2026091103L, response = 2026091104L,
            balanced_start = 2026091105L, pg = 2026091106L,
            chain_01 = 2026091111L, chain_02 = 2026091112L),
        starts = c("balanced-random-ten", "all-in-one-empty-nine"),
        pg_cases = data.frame(b = c(1, 20, 500, 500, 10000), z = c(0, 1, 5, -5, 5)),
        pg_draws_per_case = 2000L,
        thresholds = list(pg_mean_z_max = 6, pg_variance_relative_error_max = 0.2,
            root_residual_max = 1e-6, root_rcond_min = .Machine$double.eps,
            accepted_positive_moves_min = 1L, partition_moves_min = 1L,
            cross_start_psm_rms_flag = 0.2),
        calibration_decision = paste("D-032: provisional smoke values only; rho=2 is",
            "one uncalibrated candidate, not the legacy rho decision; no selection or tuning"),
        scientific_role = paste("Real PG moment screen and two short starts on one synthetic",
            "panel; no convergence certification, SBC, ranking, signal calibration or paper use"),
        rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"))
}

graphmode_pilot_identity <- function(repository) {
    core <- graphmode_source_identity(repository)
    extra <- c("scripts/graphmode-pilot.R", "scripts/tests/graphmode-pilot-deterministic.R",
        "docs/GRAPHMODE_PILOT_2026-09-11.md", "docs/provenance/graphmode-pilot-2026-09-11.md",
        "docs/provenance/graphmode-p2-fix-2026-09-11.sha256")
    paths <- file.path(repository, extra)
    if (!all(file.exists(paths))) stop("Incomplete pilot source/registration documents.", call. = FALSE)
    hashes <- vapply(paths, function(f) digest::digest(file = f, algo = "sha256",
        serialize = FALSE), character(1))
    names(hashes) <- extra
    git <- function(args) {
        out <- system2("git", c("-C", shQuote(repository), args), stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(out, "status"))) stop("Cannot inspect pilot Git identity.", call. = FALSE)
        out
    }
    dirty <- git(c("status", "--porcelain", "--untracked-files=all", "--", shQuote(extra)))
    tracked <- git(c("ls-files", "--", shQuote(extra)))
    # Bind to the reviewed v2 scientific implementation, without editing its
    # old manifest or requiring historical documentation to be today's guide.
    audited <- utils::read.table(file.path(repository, extra[5L]), stringsAsFactors = FALSE)
    audited <- setNames(audited[[1L]], audited[[2L]])
    common <- intersect(names(core$sha256), names(audited))
    audit_matches <- length(common) == 9L && identical(unname(core$sha256[common]), unname(audited[common]))
    list(core = core, extra_sha256 = hashes,
        committed = core$committed && !length(dirty) && setequal(extra, tracked),
        audited_v2_unchanged = audit_matches)
}

graphmode_pilot_runtime <- function() {
    packages <- c("digest", "BayesLogit")
    for (pkg in packages) if (!requireNamespace(pkg, quietly = TRUE))
        stop("Missing existing package: ", pkg, "; no automatic installation.", call. = FALSE)
    list(R = R.version.string, platform = R.version$platform, libraries = extSoftVersion(),
        packages = setNames(vapply(packages, function(pkg) as.character(utils::packageVersion(pkg)),
            character(1)), packages),
        threads = Sys.getenv(c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS")))
}

graphmode_pilot_registration <- function(repository, directory, identity, runtime) {
    record <- list(schema = graphmode_pilot_version, spec = graphmode_pilot_spec(),
        repository = repository, directory = directory, output_dir = file.path(directory, "run"),
        source_identity = identity, runtime = runtime,
        registration_id = basename(directory),
        preparation_authority = "User approved preparation and a local-only source freeze on 2026-09-11",
        launch_authority = "Not exercised by preparation; requires the user to invoke run --authorized",
        formal_authorized = FALSE)
    record$signature <- graphmode_digest(record)
    record
}

graphmode_pilot_validate <- function(record) {
    if (!is.list(record) || !identical(record$schema, graphmode_pilot_version))
        stop("Incompatible pilot registration.", call. = FALSE)
    rebuilt <- graphmode_pilot_registration(record$repository, record$directory,
        record$source_identity, record$runtime)
    if (!identical(record, rebuilt)) stop("Pilot registration/signature changed.", call. = FALSE)
    invisible(record)
}

graphmode_pilot_loaded_source_ok <- function(repository, core) {
    expected <- new.env(parent = baseenv())
    for (file in names(core$sha256)[startsWith(names(core$sha256), "R/")])
        sys.source(file.path(repository, file), envir = expected)
    symbols <- ls(expected, all.names = TRUE)
    describe <- function(envir) lapply(mget(symbols, envir = envir, inherits = TRUE),
        function(x) if (is.function(x)) list(formals = formals(x), body = body(x)) else x)
    # file(open="wxb") can change a literal's internal R representation after
    # use: byte serialization then differs despite identical expression trees.
    # Compare actual formals/bodies; disk SHA-256 and commit stay independent.
    # Replaced RNG hooks still fail this structural test.
    identical(describe(expected), describe(environment(graphmode_pilot_loaded_source_ok)))
}

graphmode_pilot_preflight <- function(record, repository) {
    graphmode_pilot_validate(record)
    repository <- normalizePath(repository, mustWork = TRUE)
    identity <- graphmode_pilot_identity(repository)
    runtime <- graphmode_pilot_runtime()
    problems <- character()
    if (!identical(repository, record$repository)) problems <- c(problems, "Repository path changed.")
    if (!identical(identity, record$source_identity)) problems <- c(problems, "Frozen source identity changed.")
    if (!isTRUE(identity$committed)) problems <- c(problems, "Pilot source files must be committed.")
    if (!isTRUE(identity$audited_v2_unchanged)) problems <- c(problems, "Reviewed v2 sampler no longer matches.")
    if (!graphmode_pilot_loaded_source_ok(repository, identity$core))
        problems <- c(problems, "Loaded functions differ from frozen files; use the fresh Terminal entry.")
    if (!identical(runtime, record$runtime)) problems <- c(problems, "Registered R/backend/runtime changed.")
    if (any(runtime$threads != "1")) problems <- c(problems, "Set all three numerical thread limits to 1.")
    directory <- record$directory
    if (!grepl("^/", directory) || !dir.exists(directory) ||
        !identical(normalizePath(directory, mustWork = TRUE), directory))
        problems <- c(problems, "Registration directory is missing or not canonical.")
    if (identical(directory, repository) || startsWith(directory, paste0(repository, "/")))
        problems <- c(problems, "Pilot output must be external to the repository.")
    if (file.exists(record$output_dir)) problems <- c(problems, "This registration was already launched; no reuse or resume.")
    list(ready = !length(problems), problems = problems, signature = record$signature,
        source_commit = identity$core$commit, output_dir = record$output_dir,
        note = "Read-only; no random draws, chains, calibration or run authorization.")
}

graphmode_pilot_prepare <- function(repository, directory) {
    repository <- normalizePath(repository, mustWork = TRUE)
    parent <- normalizePath(dirname(directory), mustWork = TRUE)
    directory <- file.path(parent, basename(directory))
    if (file.exists(directory) || identical(directory, repository) ||
        startsWith(directory, paste0(repository, "/")))
        stop("Use a nonexistent external registration directory.", call. = FALSE)
    identity <- graphmode_pilot_identity(repository)
    runtime <- graphmode_pilot_runtime()
    if (!isTRUE(identity$committed) || !isTRUE(identity$audited_v2_unchanged) ||
        any(runtime$threads != "1")) stop("Freeze the reviewed source and set single-thread runtime first.", call. = FALSE)
    if (!dir.create(directory)) stop("Cannot create new registration directory.", call. = FALSE)
    record <- graphmode_pilot_registration(repository, directory, identity, runtime)
    graphmode_save_new(record, file.path(directory, "pilot-registration.rds"))
    graphmode_pilot_preflight(record, repository)
}

# Pure deterministic transformation: truth is returned separately and is never
# passed as initial allocations, a stopping rule, a tuning score or a fit input.
graphmode_pilot_panel <- function(spec, innovations, profile_permutation, unit_permutation, Y_original) {
    if (!identical(spec, graphmode_pilot_spec())) stop("Unregistered pilot specification.", call. = FALSE)
    permutation <- function(x, n) identical(sort(as.integer(x)), seq_len(n)) &&
        length(x) == n && all(x == as.integer(x))
    if (!permutation(profile_permutation, spec$Ktrue) || !permutation(unit_permutation, spec$n))
        stop("Invalid input permutation.", call. = FALSE)
    road <- graphmode_simple_road(spec$geometry)
    dependency <- graphmode_dependency_graph(road$road_distance, road$ids, spec$q)
    kernel <- graphmode_graph_kernel(dependency$weight, spec$nu, spec$kappa, spec$tau)
    cells <- graphmode_voronoi(road$road_distance, road$ids, spec$Ktrue)
    profiles <- graphmode_normalized_profiles(innovations, spec$amplitude, spec$innovation_variance)
    truth <- profile_permutation[cells$Z]
    lambda <- profiles$lambda[truth, , drop = FALSE]
    if (is.null(Y_original)) return(list(lambda_original = lambda))
    if (!identical(dim(Y_original), c(spec$n, spec$TT)) || any(!is.finite(Y_original)) ||
        any(Y_original < 0 | Y_original != round(Y_original))) stop("Invalid generated counts.", call. = FALSE)
    list(fit = list(Y = Y_original[unit_permutation, , drop = FALSE], Fmat = profiles$Fmat,
            Phi = kernel$Phi[unit_permutation, , drop = FALSE], G = profiles$G, W = profiles$W),
        truth = list(Z = truth[unit_permutation], lambda = lambda[unit_permutation, , drop = FALSE],
            profiles = profiles, role = "retained generation evidence only; no recovery scores"),
        geometry = list(road = road, dependency = dependency,
            profile_permutation = profile_permutation, unit_permutation = unit_permutation))
}

graphmode_pilot_seeded <- function(seed, code) {
    old_kind <- RNGkind()
    old_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    on.exit({
        do.call(RNGkind, as.list(old_kind))
        if (is.null(old_seed)) {
            if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
        } else assign(".Random.seed", old_seed, .GlobalEnv)
    }, add = TRUE)
    do.call(RNGkind, as.list(graphmode_pilot_spec()$rng_kind))
    set.seed(seed)
    force(code)
}

graphmode_pilot_generate <- function(spec) {
    innovations <- graphmode_pilot_seeded(spec$seeds[["innovations"]], array(stats::rnorm(5 * 168 * 3), c(5L, 168L, 3L)))
    profile_order <- graphmode_pilot_seeded(spec$seeds[["profile_permutation"]], sample.int(spec$Ktrue))
    unit_order <- graphmode_pilot_seeded(spec$seeds[["unit_permutation"]], sample.int(spec$n))
    lambda <- graphmode_pilot_panel(spec, innovations, profile_order, unit_order, NULL)$lambda_original
    Y <- graphmode_pilot_seeded(spec$seeds[["response"]],
        matrix(stats::rpois(length(lambda), as.vector(lambda)), spec$n, spec$TT))
    graphmode_pilot_panel(spec, innovations, profile_order, unit_order, Y)
}

graphmode_pilot_starts <- function(spec) {
    list(graphmode_pilot_seeded(spec$seeds[["balanced_start"]],
        sample(rep(seq_len(spec$Kfit), length.out = spec$n))), rep(1L, spec$n))
}

graphmode_pilot_pg_moments <- function(b, z) {
    if (length(b) != 1L || length(z) != 1L || !is.finite(b) || b <= 0 ||
        !is.finite(z) || !(z == 0 || (abs(z) >= 0.1 && abs(z) <= 10)))
        stop("Moment screen supports zero or 0.1 <= abs(z) <= 10 only.", call. = FALSE)
    if (z == 0) return(c(mean = b / 4, variance = b / 24))
    z <- abs(z)
    c(mean = b * tanh(z / 2) / (2 * z),
        variance = b * (sinh(z) - z) / (2 * z^3 * (cosh(z) + 1)))
}

graphmode_pilot_pg_score <- function(draws, b, z, thresholds) {
    if (length(draws) < 2L || any(!is.finite(draws)) || any(draws <= 0))
        stop("Invalid real PG sample; retain failure.", call. = FALSE)
    target <- graphmode_pilot_pg_moments(b, z)
    mean_z <- (mean(draws) - target[["mean"]]) / sqrt(target[["variance"]] / length(draws))
    variance_error <- abs(stats::var(draws) / target[["variance"]] - 1)
    data.frame(b = b, z = z, draws = length(draws), mean = mean(draws), variance = stats::var(draws),
        expected_mean = target[["mean"]], expected_variance = target[["variance"]],
        mean_z = mean_z, variance_relative_error = variance_error,
        passed = abs(mean_z) <= thresholds$pg_mean_z_max && variance_error <= thresholds$pg_variance_relative_error_max)
}

graphmode_pilot_plan <- function(record, panel, initial_labels, chain) {
    spec <- record$spec
    fit <- panel$fit
    config <- graphmode_config(fit$Y, fit$Fmat, spec$m0, spec$C0, spec$method,
        family = "poisson", dynamics = "dynamic", K = spec$Kfit, G = fit$G, W = fit$W,
        Phi = fit$Phi, guidance = spec$guidance, tau = spec$tau, s_b = spec$s_b,
        guidance_prior = spec$guidance_prior, guidance_proposal_sd = spec$guidance_proposal_sd,
        rho = spec$rho, max_ess_steps = spec$max_ess_steps)
    initial <- graphmode_initial_state(config, initial_labels)
    graphmode_run_plan(config, initial, record$source_identity$core,
        seed = spec$seeds[[paste0("chain_0", chain)]], iterations = spec$iterations,
        warmup = spec$warmup, thin = spec$thin, checkpoint_every = spec$checkpoint_every,
        budget_seconds = spec$chain_budget_seconds,
        output_dir = file.path(record$output_dir, sprintf("chain-%02d", chain)),
        registration_id = paste0(record$registration_id, "-chain-", chain),
        authorization_reference = "User Terminal invocation of pilot run --authorized; not agent preparation",
        role = spec$role, diagnostic_thresholds = spec$thresholds,
        provenance = list(data_identity = graphmode_digest(fit$Y), geometry_identity = graphmode_digest(panel$geometry),
            data_seed_record = paste(names(spec$seeds), spec$seeds, collapse = ";"),
            permutation_seed_record = paste(spec$seeds[c("profile_permutation", "unit_permutation")], collapse = ";"),
            calibration_decision = spec$calibration_decision, scientific_role = spec$scientific_role))
}

graphmode_pilot_trace <- function(result) {
    do.call(rbind, lapply(result$checkpoint$diagnostics, function(d) {
        nonempty <- Filter(function(x) !x$empty, d$expert)
        get <- function(field) vapply(nonempty, function(x) x[[field]], numeric(1))
        data.frame(iteration = d$iteration, Kocc = d$Kocc, fragmentation = d$fragmentation,
            partition_move = unname(d$events[["pair_changes"]]),
            accepted = sum(vapply(nonempty, function(x) isTRUE(x$accepted), logical(1))),
            positive_moves = sum(get("movement") > 0), nonempty = length(nonempty),
            movement = sum(get("movement")), expert_seconds = sum(get("seconds")),
            root_residual = max(get("root_residual")), root_rcond = min(get("root_reciprocal_condition")),
            ess_evaluations = d$ess_evaluations, guidance_acceptance = mean(d$guidance_accept))
    }))
}

graphmode_pilot_report <- function(results, spec) {
    traces <- lapply(results, graphmode_pilot_trace)
    screens <- lapply(traces, function(trace) {
        scored <- trace[trace$iteration > spec$warmup, , drop = FALSE]
        list(retained_sweeps = nrow(scored), accepted_positive_moves = sum(scored$positive_moves),
            partition_moves = sum(scored$partition_move > 0),
            mh_acceptance = sum(scored$accepted) / sum(scored$nonempty),
            max_root_residual = max(trace$root_residual), min_root_rcond = min(trace$root_rcond),
            movement_screen = sum(scored$positive_moves) >= spec$thresholds$accepted_positive_moves_min &&
                sum(scored$partition_move > 0) >= spec$thresholds$partition_moves_min)
    })
    mask <- upper.tri(results[[1L]]$summary$similarity)
    psm_rms <- sqrt(mean((results[[1L]]$summary$similarity[mask] - results[[2L]]$summary$similarity[mask])^2))
    list(status = "completed-short-screen-not-convergence-certified", traces = traces, chains = screens,
        cross_start_psm_rms = psm_rms, cross_start_flag = psm_rms > spec$thresholds$cross_start_psm_rms_flag,
        convergence_certified = FALSE, rho_selected = FALSE, formal_authorized = FALSE,
        note = "Only 50 retained draws per start: movement and PSM flags are descriptive; no Rhat/ESS certification or recovery ranking.")
}

# Each core preflight/run starts fresh, preserving the original serialized
# loaded-source guard without inheriting preparation-time literal changes.
# Processes are sequential: still one numerical worker, not a pool.
graphmode_pilot_process <- function(command, plan_path, repository) {
    command <- match.arg(command, c("preflight", "run"))
    args <- c("--vanilla", shQuote(file.path(repository, "scripts/graphmode-pilot.R")),
        paste0("chain-", command), shQuote(plan_path), if (command == "run") "--authorized")
    output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), args,
        stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (length(output)) cat(paste(output, collapse = "\n"), "\n")
    if (!is.null(status) && status != 0L)
        stop("Fresh-process ", command, " failed (status ", status, "): ",
            paste(output, collapse = "\n"), call. = FALSE)
    invisible(output)
}

graphmode_pilot_run <- function(record, repository, authorized = FALSE) {
    if (!identical(authorized, TRUE)) stop("Only the user's explicit run --authorized may start this pilot.", call. = FALSE)
    checked <- graphmode_pilot_preflight(record, repository)
    if (!checked$ready) stop(paste(checked$problems, collapse = "\n"), call. = FALSE)
    destination <- record$output_dir
    if (!dir.create(destination)) stop("Cannot create new run destination.", call. = FALSE)
    connection <- file(file.path(destination, "console.log"), open = "wx")
    sink(connection, split = TRUE)
    on.exit({sink(); close(connection)}, add = TRUE)
    old_options <- options(warn = 2)
    on.exit(options(old_options), add = TRUE)
    started <- proc.time()[[3L]]
    stage <- "registration"
    budget <- function(reserve = 0) {
        if (proc.time()[[3L]] - started + reserve >= record$spec$total_budget_seconds)
            stop("Total registered budget exhausted or insufficient for next stage; no automatic extension.", call. = FALSE)
    }
    log <- function(message) {cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message, "\n"); flush.console()}
    tryCatch({
        # Persist authority and the complete seed/configuration/source contract
        # BEFORE the very first PG, input or initial-state random draw.
        graphmode_save_new(list(record = record, preflight = checked,
            authorization = "explicit user CLI --authorized", started = as.character(Sys.time())),
            file.path(destination, "launch-registration.rds"))
        spec <- record$spec
        stage <- "real-PG-screen"
        log("Starting registered exact-PG moment screen (not a distribution proof).")
        pg_started <- proc.time()[[3L]]
        pg_scores <- graphmode_pilot_seeded(spec$seeds[["pg"]], {
            rows <- vector("list", nrow(spec$pg_cases))
            for (j in seq_len(nrow(spec$pg_cases))) {
                budget()
                if (proc.time()[[3L]] - pg_started >= spec$pg_budget_seconds)
                    stop("PG screen budget exhausted.", call. = FALSE)
                b <- spec$pg_cases$b[j]; z <- spec$pg_cases$z[j]
                samples <- graphmode_pg(rep(b, spec$pg_draws_per_case), rep(z, spec$pg_draws_per_case))
                graphmode_save_new(list(b = b, z = z, samples = samples),
                    file.path(destination, sprintf("pg-case-%02d.rds", j)))
                rows[[j]] <- graphmode_pilot_pg_score(samples, b, z, spec$thresholds)
            }
            do.call(rbind, rows)
        })
        graphmode_save_new(pg_scores, file.path(destination, "pg-screen.rds"))
        if (proc.time()[[3L]] - pg_started > spec$pg_budget_seconds || !all(pg_scores$passed))
            stop("PG screen failed its registered moments/budget; retain samples, do not rerun automatically.", call. = FALSE)
        stage <- "input-generation"
        budget()
        panel <- graphmode_pilot_generate(spec)
        graphmode_save_new(panel, file.path(destination, "generated-input.rds"))
        starts <- graphmode_pilot_starts(spec)
        if (identical(outer(starts[[1L]], starts[[1L]], "=="), outer(starts[[2L]], starts[[2L]], "==")))
            stop("Registered starts unexpectedly have the same partition.", call. = FALSE)
        plans <- lapply(1:2, function(j) graphmode_pilot_plan(record, panel, starts[[j]], j))
        for (j in 1:2) {
            plan_path <- file.path(destination, sprintf("PLAN-%02d.rds", j))
            graphmode_save_new(plans[[j]], plan_path)
            graphmode_pilot_process("preflight", plan_path, repository)
        }
        results <- vector("list", 2L)
        for (j in 1:2) {
            stage <- paste0("chain-", j)
            budget(reserve = spec$chain_budget_seconds)
            log(sprintf("Starting chain %d/2: 100 sweeps, 50 warmup; checkpoint every 25; 600-second sweep-boundary budget.", j))
            chain_started <- proc.time()[[3L]]
            if (!identical(graphmode_pilot_identity(repository), record$source_identity))
                stop("Frozen source changed during this run; retain the partial run.", call. = FALSE)
            graphmode_pilot_process("run", file.path(destination, sprintf("PLAN-%02d.rds", j)), repository)
            if (proc.time()[[3L]] - chain_started > spec$chain_budget_seconds)
                stop("Chain completed after its registered budget; retain it as over-budget, not an on-time pass.", call. = FALSE)
            results[[j]] <- readRDS(file.path(plans[[j]]$output_dir, "result.rds"))
            budget()
            log(sprintf("Chain %d/2 completed; this does not certify convergence.", j))
        }
        stage <- "short-screen-summary"
        report <- graphmode_pilot_report(results, spec)
        report$elapsed_seconds <- proc.time()[[3L]] - started
        graphmode_save_new(report, file.path(destination, "pilot-report.rds"))
        log("Finished. Review pilot-report.rds and retained failures/flags before planning any additional run.")
        invisible(report[c("status", "chains", "cross_start_psm_rms", "cross_start_flag", "elapsed_seconds")])
    }, error = function(e) {
        failure <- list(status = "failed-retained-do-not-resume", stage = stage,
            error = conditionMessage(e), registration_signature = record$signature,
            elapsed_seconds = proc.time()[[3L]] - started, formal_authorized = FALSE)
        tryCatch(graphmode_save_new(failure, file.path(destination, "pilot-failure.rds")),
            error = function(save_error) message("Failure record could not be written: ", conditionMessage(save_error)))
        stop(conditionMessage(e), call. = FALSE)
    })
}
