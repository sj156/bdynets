# D-036: one separately authorized, noninferential r4 four-start screen.
# Preparation is deterministic. Existing audited kernels/launchers are unchanged.
graphmode4_pilot_version <- "graphmode-r4-four-chain-smoke-20260912-v1"

graphmode4_pilot_spec <- function() {
    list(version = graphmode4_pilot_version, protocol = graphmode4_protocol,
        role = "development-pilot", geometry = "intertwined-spiral", truth = "road-voronoi",
        method = "graphMoDE-W", n = 121L, TT = 168L, Ktrue = 5L, Kfit = 10L,
        amplitude = .3, innovation_variance = .002,
        expert = list(m0 = c(log(25), 0, 0), C0 = diag(c(1, .5, .5)),
            family = "poisson", dynamics = "dynamic", G = diag(3), W = diag(.3^2 * .002, 3),
            rho = 2L, variance_prior = NULL),
        gate = list(guidance_prior = c(1, 1), guidance_proposal_sd = .25, max_ess_steps = 1000L),
        start_occupancy = c(1L, 3L, 7L, 10L), workers = 1L,
        iterations = 400L, warmup = 200L, thin = 1L, checkpoint_every = 50L,
        chain_budget_seconds = 600, phase_budget_seconds = 300, total_budget_seconds = 3000,
        seeds = c(innovations = 2026091201L, profile_permutation = 2026091202L,
            unit_permutation = 2026091203L, response = 2026091204L, pg = 2026091205L,
            start_01 = 2026091211L, start_02 = 2026091212L, start_03 = 2026091213L, start_04 = 2026091214L,
            chain_01 = 2026091221L, chain_02 = 2026091222L, chain_03 = 2026091223L, chain_04 = 2026091224L),
        pg_cases = data.frame(b = c(1, 20, 500, 500, 10000), z = c(0, 1, 5, -5, 5)),
        pg_draws_per_case = 2000L,
        thresholds = list(pg_mean_z_max = 6, pg_variance_relative_error_max = .2,
            factor_residual_max = 1e-10, root_residual_max = 1e-6, root_rcond_min = .Machine$double.eps,
            rank_rhat_max = 1.01, folded_rhat_max = 1.01, bulk_ess_min = 400,
            tail_ess_min = 400, pairwise_psm_rms_max = .05),
        rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"),
        calibration_id = "D-036 provisional engineering screen; rho2/signal/priors NOT calibrated or selected",
        scientific_role = "One fresh synthetic panel and four truth-blind starts; no ARI, model selection, SBC, paper use or convergence certification")
}

graphmode4_pilot_identity <- function(repository) {
    r4 <- graphmode4_source_identity(repository)
    extra <- c("scripts/graphmode-r4-pilot.R", "scripts/tests/graphmode-r4-pilot-deterministic.R",
        "docs/GRAPHMODE_R4_PILOT_2026-09-12.md", "docs/provenance/graphmode-p2-fix-2026-09-11.sha256")
    sha <- vapply(file.path(repository, extra), function(f)
        digest::digest(file = f, algo = "sha256", serialize = FALSE), character(1))
    names(sha) <- extra
    git <- function(args) {
        out <- system2("git", c("-C", shQuote(repository), args), stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(out, "status"))) stop("Cannot inspect source freeze.", call. = FALSE)
        out
    }
    dirty <- git(c("status", "--porcelain", "--untracked-files=all", "--", shQuote(extra)))
    tracked <- git(c("ls-files", "--", shQuote(extra)))
    audited <- utils::read.table(file.path(repository, extra[4L]), stringsAsFactors = FALSE)
    audited <- setNames(audited[[1L]], audited[[2L]])
    common <- intersect(names(r4$core$sha256), names(audited))
    list(r4 = r4, extra_sha256 = sha, committed = r4$committed && !length(dirty) && setequal(extra, tracked),
        audited_core_unchanged = length(common) == 9L && identical(unname(r4$core$sha256[common]), unname(audited[common])))
}

graphmode4_pilot_runtime <- function() {
    runtime <- graphmode_pilot_runtime()
    if (!requireNamespace("posterior", quietly = TRUE)) stop("Existing posterior backend is required; no installation.", call. = FALSE)
    packages <- c("digest", "BayesLogit", "posterior")
    # Include declared transitive imports (also namespaces used through ::).
    index <- 1L
    while (index <= length(packages)) {
        description <- utils::packageDescription(packages[index])
        declarations <- paste(c(description$Imports, description$Depends), collapse = ",")
        dependencies <- trimws(gsub("\\s*\\([^)]*\\)", "", strsplit(declarations, ",", fixed = TRUE)[[1L]]))
        dependencies <- setdiff(dependencies[nzchar(dependencies)], c("R", "base", packages))
        for (pkg in dependencies) if (!requireNamespace(pkg, quietly = TRUE))
            stop("Missing registered backend dependency: ", pkg, "; no installation.", call. = FALSE)
        packages <- c(packages, dependencies); index <- index + 1L
    }
    packages <- sort(unique(packages))
    runtime$packages <- setNames(vapply(packages, function(pkg) as.character(utils::packageVersion(pkg)), character(1)), packages)
    runtime$package_files <- lapply(setNames(packages, packages), function(pkg) {
        root <- system.file(package = pkg)
        paths <- c("DESCRIPTION", sort(list.files(root, "[.](so|dll|dylib|rdb|rdx)$", recursive = TRUE)))
        setNames(vapply(file.path(root, paths), function(f)
            digest::digest(file = f, algo = "sha256", serialize = FALSE), character(1)), paths)
    })
    runtime$locale <- Sys.getenv("LC_ALL")
    runtime
}

graphmode4_pilot_registration <- function(repository, directory, identity, runtime) {
    record <- list(schema = graphmode4_pilot_version, spec = graphmode4_pilot_spec(),
        repository = repository, directory = directory, output_dir = file.path(directory, "run"),
        source_identity = identity, runtime = runtime, registration_id = basename(directory),
        preparation_authority = "User approved execution-code implementation, local source freeze and preregistration; no run yet",
        launch_authority = "Pending later user confirmation of this exact registration, then run --authorized",
        formal_authorized = FALSE, resume_supported = FALSE)
    record$signature <- graphmode_digest(record)
    record
}

graphmode4_pilot_validate <- function(record) {
    if (!is.list(record) || !identical(record$schema, graphmode4_pilot_version)) stop("Incompatible r4 pilot registration.", call. = FALSE)
    rebuilt <- graphmode4_pilot_registration(record$repository, record$directory, record$source_identity, record$runtime)
    if (!identical(rebuilt, record)) stop("Registered pilot contents/signature changed.", call. = FALSE)
    invisible(record)
}

graphmode4_pilot_guard <- function(record, repository) {
    graphmode4_pilot_validate(record)
    repository <- normalizePath(repository, mustWork = TRUE)
    if (!identical(repository, record$repository)) stop("Repository path changed.", call. = FALSE)
    identity <- graphmode4_pilot_identity(repository)
    if (!identical(identity, record$source_identity) || !isTRUE(identity$committed) ||
        !isTRUE(identity$audited_core_unchanged) || !isTRUE(identity$r4$requirements_match))
        stop("Source/specification not frozen or changed; retain any existing run.", call. = FALSE)
    if (!graphmode_pilot_loaded_source_ok(repository, identity$r4))
        stop("Loaded functions differ from source; use a fresh Terminal process.", call. = FALSE)
    runtime <- graphmode4_pilot_runtime()
    if (!identical(runtime, record$runtime) || any(runtime$threads != "1") || runtime$locale != "C")
        stop("Runtime/backend changed or single-thread C locale not set.", call. = FALSE)
    if (!dir.exists(record$directory) || !identical(normalizePath(record$directory), record$directory) ||
        identical(record$directory, repository) || startsWith(record$directory, paste0(repository, "/")))
        stop("Registration directory must be canonical and external.", call. = FALSE)
    path <- file.path(record$directory, "pilot-registration.rds")
    if (!file.exists(path) || !identical(readRDS(path), record)) stop("Persisted registration missing or changed.", call. = FALSE)
    invisible(TRUE)
}

graphmode4_pilot_preflight <- function(record, repository) {
    problem <- tryCatch({graphmode4_pilot_guard(record, repository)
        if (file.exists(record$output_dir)) stop("Run destination already exists; no retry or resume.", call. = FALSE)
        character()
    }, error = function(e) conditionMessage(e))
    list(ready = !length(problem), problems = problem, signature = record$signature,
        source_commit = record$source_identity$r4$core$commit, output_dir = record$output_dir,
        run_authorized = FALSE, note = "Read-only readiness; launch still requires the user's later confirmation.")
}

graphmode4_pilot_prepare <- function(repository, directory) {
    repository <- normalizePath(repository, mustWork = TRUE)
    if (!grepl("^/", directory)) stop("Use an absolute new external directory.", call. = FALSE)
    directory <- file.path(normalizePath(dirname(directory), mustWork = TRUE), basename(directory))
    if (file.exists(directory) || identical(directory, repository) || startsWith(directory, paste0(repository, "/")))
        stop("Use a nonexistent external registration directory.", call. = FALSE)
    identity <- graphmode4_pilot_identity(repository); runtime <- graphmode4_pilot_runtime()
    if (!isTRUE(identity$committed) || !isTRUE(identity$audited_core_unchanged) || !isTRUE(identity$r4$requirements_match) ||
        any(runtime$threads != "1") || runtime$locale != "C" || !graphmode_pilot_loaded_source_ok(repository, identity$r4))
        stop("Freeze reviewed source and establish a clean single-thread runtime before preparation.", call. = FALSE)
    if (!dir.create(directory)) stop("Cannot create registration directory.", call. = FALSE)
    record <- graphmode4_pilot_registration(repository, directory, identity, runtime)
    graphmode_save_new(record, file.path(directory, "pilot-registration.rds"))
    graphmode4_pilot_preflight(record, repository)
}

graphmode4_pilot_panel <- function(spec, innovations, profile_permutation, unit_permutation, counts = NULL) {
    if (!identical(spec, graphmode4_pilot_spec())) stop("Unregistered r4 pilot specification.", call. = FALSE)
    road <- graphmode4_road(spec$geometry)
    truth <- graphmode4_partition(road, spec$truth, profile_permutation)
    profiles <- graphmode4_profiles(innovations, spec$amplitude, spec$innovation_variance, "clustering")
    lambda <- profiles$lambda[truth$Z, , drop = FALSE]
    if (is.null(counts)) return(list(lambda_original = lambda))
    if (!identical(dim(counts), c(spec$n, spec$TT)) || any(!is.finite(counts) | counts < 0 | counts != round(counts)))
        stop("Invalid generated counts.", call. = FALSE)
    p <- graphmode4_permutation(unit_permutation, spec$n)
    reordered <- graphmode4_reorder(road, p, counts)
    fit <- graphmode4_config(reordered$road, reordered$Y, profiles$Fmat,
        spec$method, spec$expert, spec$gate, spec$calibration_id)
    list(fit = fit, geometry = reordered$road,
        provenance = list(profile_permutation = profile_permutation, unit_permutation = p),
        truth = list(Z = truth$Z[p], lambda = lambda[p, ], role = "generation evidence only; no recovery scores"))
}

graphmode4_pilot_generate <- function(spec) {
    innovations <- graphmode_pilot_seeded(spec$seeds[["innovations"]], array(stats::rnorm(5 * 168 * 3), c(5L, 168L, 3L)))
    profiles <- graphmode_pilot_seeded(spec$seeds[["profile_permutation"]], sample.int(5L))
    units <- graphmode_pilot_seeded(spec$seeds[["unit_permutation"]], sample.int(121L))
    lambda <- graphmode4_pilot_panel(spec, innovations, profiles, units)$lambda_original
    counts <- graphmode_pilot_seeded(spec$seeds[["response"]], matrix(stats::rpois(length(lambda), lambda), 121L, 168L))
    panel <- graphmode4_pilot_panel(spec, innovations, profiles, units, counts)
    starts <- lapply(1:4, function(j) graphmode_pilot_seeded(spec$seeds[[sprintf("start_%02d", j)]],
        sample(rep(seq_len(spec$start_occupancy[j]), length.out = spec$n))))
    list(panel = panel, starts = starts)
}

graphmode4_pilot_plans <- function(record, panel, starts) {
    spec <- record$spec
    if (!is.list(starts) || length(starts) != 4L) stop("Need four prescribed starts.", call. = FALSE)
    graphmode4_validate_config(panel$fit)
    lapply(1:4, function(j) {
        Z <- gmde_validate_labels(starts[[j]], spec$n, spec$Kfit)
        if (length(unique(Z)) != spec$start_occupancy[j]) stop("Initial occupancy differs from registration.", call. = FALSE)
        graphmode_run_plan(panel$fit$core, graphmode_initial_state(panel$fit$core, Z),
            record$source_identity$r4$core, spec$seeds[[sprintf("chain_%02d", j)]],
            spec$iterations, spec$warmup, spec$thin, spec$checkpoint_every, spec$chain_budget_seconds,
            file.path(record$output_dir, sprintf("chain-%02d", j)), paste0(record$registration_id, "-chain-", j),
            "Later explicit user confirmation of outer r4 registration and run --authorized", spec$role,
            spec$thresholds, list(data_identity = graphmode_digest(panel$fit$core$Y),
                geometry_identity = panel$fit$geometry_identity,
                data_seed_record = paste(names(spec$seeds), spec$seeds, collapse = ";"),
                permutation_seed_record = graphmode_digest(panel$provenance), calibration_decision = spec$calibration_id,
                scientific_role = spec$scientific_role))
    })
}

graphmode4_pilot_tree <- function(record) {
    path <- file.path(record$output_dir, "launch-registration.rds")
    if (!dir.exists(record$output_dir) || !identical(normalizePath(record$output_dir), record$output_dir) ||
        !file.exists(path) || !identical(readRDS(path)$record, record))
        stop("Active output tree/identity disappeared or changed; never recreate it.", call. = FALSE)
    invisible(TRUE)
}

graphmode4_pilot_child <- function(command, envelope_path, repository, timeout_seconds) {
    command <- match.arg(command, c("chain-preflight", "chain-run"))
    output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
        c("--vanilla", shQuote(file.path(repository, "scripts/graphmode-r4-pilot.R")), command,
          shQuote(envelope_path), if (command == "chain-run") "--authorized"),
        stdout = TRUE, stderr = TRUE, timeout = ceiling(timeout_seconds)))
    status <- attr(output, "status")
    list(status = if (is.null(status)) 0L else as.integer(status), output = output)
}

graphmode4_pilot_worker <- function(envelope, repository, authorized = FALSE, check_only = FALSE) {
    record <- envelope$record
    graphmode4_pilot_guard(record, repository); graphmode4_pilot_tree(record)
    graphmode_revalidate_plan(envelope$plan)
    original <- envelope$signature; unsigned <- envelope; unsigned$signature <- NULL
    if (!identical(original, graphmode_digest(unsigned))) stop("Child envelope changed.", call. = FALSE)
    j <- gmde_scalar_integer(envelope$chain, "chain", 1L, 4L)
    path <- file.path(record$output_dir, sprintf("PLAN-%02d.rds", j))
    if (!file.exists(path) || !identical(readRDS(path), envelope)) stop("Child plan not persisted before launch.", call. = FALSE)
    plan <- envelope$plan; spec <- record$spec
    if (!identical(plan$config, envelope$fit$core) || !identical(plan$source_identity, record$source_identity$r4$core) ||
        !identical(plan$output_dir, file.path(record$output_dir, sprintf("chain-%02d", j))) ||
        !isTRUE(plan$seed == spec$seeds[[sprintf("chain_%02d", j)]]) ||
        !identical(c(plan$iterations, plan$warmup, plan$thin, plan$checkpoint_every, plan$budget_seconds),
                   c(spec$iterations, spec$warmup, spec$thin, spec$checkpoint_every, spec$chain_budget_seconds)))
        stop("Child plan disagrees with outer registration.", call. = FALSE)
    graphmode4_validate_config(envelope$fit)
    if (check_only) return(graphmode_preflight(plan, repository))
    if (!isTRUE(authorized)) stop("Explicit child launch authorization is required.", call. = FALSE)
    result <- graphmode_run(plan, repository, authorized = TRUE)
    graphmode4_pilot_guard(record, repository); graphmode4_pilot_tree(record)
    result
}

graphmode4_pilot_chain_record <- function(result, plan, fit, thresholds) {
    cp <- result$checkpoint
    if (!is.list(cp) || !identical(cp$status, "completed-not-convergence-certified") ||
        !identical(cp$plan_signature, plan$signature) || !identical(cp$source_identity, plan$source_identity) ||
        !isTRUE(cp$state$iteration == plan$iterations) || !is.list(cp$diagnostics) ||
        length(cp$diagnostics) != plan$iterations || length(cp$elapsed_seconds) != 1L ||
        !is.finite(cp$elapsed_seconds) || cp$elapsed_seconds < 0 || cp$elapsed_seconds > plan$budget_seconds ||
        !identical(result$summary_signature, graphmode_digest(result$summary)))
        stop("Missing, incomplete, changed or over-budget chain result.", call. = FALSE)
    for (i in seq_len(plan$iterations)) {
        d <- cp$diagnostics[[i]]
        if (!isTRUE(d$iteration == i) || length(d$expert) != plan$config$K ||
            !all(c("node_moves", "pair_changes") %in% names(d$events)))
            stop("Missing per-sweep diagnostic evidence.", call. = FALSE)
        for (e in d$expert) {
            finite <- c(e$factor_residual, e$root_residual, e$movement, e$seconds)
            if (length(finite) != 4L || any(!is.finite(finite) | finite < 0) ||
                e$factor_residual > thresholds$factor_residual_max || e$root_residual > thresholds$root_residual_max ||
                !is.logical(e$empty) || length(e$empty) != 1L || is.na(e$empty))
                stop("Numerical protection evidence missing/failed.", call. = FALSE)
            if (!e$empty && (length(e$root_reciprocal_condition) != 1L ||
                !is.finite(e$root_reciprocal_condition) || e$root_reciprocal_condition <= thresholds$root_rcond_min))
                stop("Covariance-root conditioning evidence failed.", call. = FALSE)
            if (!e$empty && (!is.logical(e$accepted) || length(e$accepted) != 1L || is.na(e$accepted) ||
                length(e$log_acceptance) != 1L || !is.finite(e$log_acceptance) || e$log_acceptance > 0))
                stop("Poisson MH acceptance evidence missing/invalid.", call. = FALSE)
        }
    }
    list(complete = TRUE, config_signature = fit$signature, seed = plan$seed,
        iterations = plan$iterations, warmup = plan$warmup, thin = plan$thin,
        draws = cp$saved, numerical_guards_passed = TRUE, movement = cp$diagnostics,
        evidence_identity = graphmode_digest(result), failure = "")
}

graphmode4_pilot_run <- function(record, repository, authorized = FALSE) {
    if (!identical(authorized, TRUE)) stop("Later explicit user confirmation is required; preparation cannot launch.", call. = FALSE)
    checked <- graphmode4_pilot_preflight(record, repository)
    if (!checked$ready) stop(paste(checked$problems, collapse = "\n"), call. = FALSE)
    if (!dir.create(record$output_dir)) stop("Cannot create new exclusive run directory.", call. = FALSE)
    old_options <- options(warn = 2)
    on.exit(options(old_options), add = TRUE)
    started <- proc.time()[[3L]]; phase_started <- started; stage <- "launch-registration"
    spec <- record$spec
    check <- function(reserve = 0, phase = FALSE) {
        graphmode4_pilot_tree(record)
        if (proc.time()[[3L]] - started + reserve >= spec$total_budget_seconds ||
            phase && proc.time()[[3L]] - phase_started >= spec$phase_budget_seconds)
            stop("Registered budget exhausted; no automatic extension.", call. = FALSE)
    }
    log <- function(s) {cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), s, "\n"); flush.console()}
    save <- function(x, name) {graphmode4_pilot_tree(record); graphmode_save_new(x, file.path(record$output_dir, name))}
    tryCatch({
        # First persisted record precedes every PG, response and start draw.
        graphmode_save_new(list(record = record, preflight = checked,
            authorization = "later user-confirmed exact registration; explicit run --authorized",
            started = as.character(Sys.time())), file.path(record$output_dir, "launch-registration.rds"))
        stage <- "exact-PG-screen"; log("Starting exact-PG moment screen; not a distribution proof.")
        scores <- graphmode_pilot_seeded(spec$seeds[["pg"]], {
            rows <- vector("list", nrow(spec$pg_cases))
            for (j in seq_len(nrow(spec$pg_cases))) {
                check(phase = TRUE)
                b <- spec$pg_cases$b[j]; z <- spec$pg_cases$z[j]
                samples <- graphmode_pg(rep(b, spec$pg_draws_per_case), rep(z, spec$pg_draws_per_case))
                save(list(b = b, z = z, samples = samples), sprintf("pg-case-%02d.rds", j))
                rows[[j]] <- graphmode_pilot_pg_score(samples, b, z, spec$thresholds)
            }
            do.call(rbind, rows)
        })
        save(scores, "pg-screen.rds"); check(phase = TRUE)
        if (!all(scores$passed)) stop("PG moment screen failed; retain all samples, no retry.", call. = FALSE)
        stage <- "input-generation"; phase_started <- proc.time()[[3L]]
        graphmode4_pilot_guard(record, repository)
        generated <- graphmode4_pilot_generate(spec); check(phase = TRUE)
        save(generated, "generated-input.rds")
        fit <- generated$panel$fit
        plans <- graphmode4_pilot_plans(record, generated$panel, generated$starts)
        envelopes <- lapply(1:4, function(j) {
            e <- list(record = record, chain = j, fit = fit, plan = plans[[j]])
            e$signature <- graphmode_digest(e); e
        })
        # All four plans are durable before the first chain. Workers never see truth.
        for (j in 1:4) save(envelopes[[j]], sprintf("PLAN-%02d.rds", j))
        for (j in 1:4) {
            check(phase = TRUE)
            status <- graphmode4_pilot_child("chain-preflight", file.path(record$output_dir, sprintf("PLAN-%02d.rds", j)), repository, 30)
            save(status, sprintf("preflight-%02d.rds", j))
            if (status$status != 0L) stop("Fresh-process chain preflight failed.", call. = FALSE)
        }
        chains <- vector("list", 4L)
        for (j in 1:4) {
            stage <- paste0("chain-", j); check(reserve = spec$chain_budget_seconds)
            graphmode4_pilot_guard(record, repository)
            log(sprintf("Starting chain %d/4: %d sweeps, %d warmup; hard subprocess budget %d seconds.",
                j, spec$iterations, spec$warmup, spec$chain_budget_seconds))
            status <- graphmode4_pilot_child("chain-run", file.path(record$output_dir, sprintf("PLAN-%02d.rds", j)), repository, spec$chain_budget_seconds)
            save(status, sprintf("console-chain-%02d.rds", j))
            if (length(status$output)) cat(paste(status$output, collapse = "\n"), "\n")
            if (status$status != 0L) stop(paste0("Chain ", j, " failed or timed out (", status$status, "); remaining chains not launched."), call. = FALSE)
            graphmode4_pilot_guard(record, repository); check()
            result_path <- file.path(plans[[j]]$output_dir, "result.rds")
            if (!file.exists(result_path)) stop("Missing child result; retain partial run.", call. = FALSE)
            chains[[j]] <- graphmode4_pilot_chain_record(readRDS(result_path), plans[[j]], fit, spec$thresholds)
            save(chains[[j]], sprintf("chain-audit-%02d.rds", j))
            log(sprintf("Chain %d completed; not a convergence certificate.", j))
        }
        stage <- "four-chain-diagnostics"; phase_started <- proc.time()[[3L]]
        check(); graphmode4_pilot_guard(record, repository)
        report <- graphmode4_validity(chains, fit, spec$seeds[sprintf("chain_%02d", 1:4)], spec$iterations, spec$warmup, spec$thin)
        report$status <- if (report$valid) "completed-necessary-checks-passed-not-convergence-certified" else "completed-with-diagnostic-flags"
        report$registration_signature <- record$signature
        report$source_identity <- record$source_identity
        report$elapsed_seconds <- proc.time()[[3L]] - started
        report$rho_selected <- FALSE; report$paper_use <- FALSE
        check(phase = TRUE); save(report, "pilot-report.rds")
        check(phase = TRUE)
        save(list(status = report$status, registration_signature = record$signature), "completed.rds")
        log("Finished. Retain all four chains; no automatic extension, selection or formal release.")
        invisible(report[c("status", "valid", "failures", "psm_rms", "elapsed_seconds", "formal_authorized")])
    }, error = function(e) {
        failure <- list(status = "failed-retained-do-not-resume", stage = stage, error = conditionMessage(e),
            registration_signature = record$signature, elapsed_seconds = proc.time()[[3L]] - started,
            formal_authorized = FALSE, remaining_chains = "not automatically launched after runtime failure")
        tryCatch(save(failure, "pilot-failure.rds"), error = function(other)
            message("Could not save failure; ORIGINAL error retained: ", conditionMessage(e), "; persistence: ", conditionMessage(other)))
        stop(conditionMessage(e), call. = FALSE)
    })
}
