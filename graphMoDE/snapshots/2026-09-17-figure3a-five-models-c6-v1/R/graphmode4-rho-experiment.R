# D-041: orchestration only; reuse the audited panel, plan, worker and reducer.
graphmode4_rho_experiment_spec <- function() {
    list(schema = "graphmode-r4-rho-experiment-20260912-v1",
        panel_spec = graphmode4_pilot_spec(),
        rhos = c(1L, 2L, 4L, 8L), start_occupancy = c(3L, 7L),
        seeds = c(innovations = 2026091251L, profile_permutation = 2026091252L,
            unit_permutation = 2026091253L, response = 2026091254L,
            start_01 = 2026091261L, start_02 = 2026091262L,
            chain_01 = 2026091271L, chain_02 = 2026091272L),
        iterations = 400L, warmup = 200L, thin = 1L, checkpoint_every = 50L,
        worker_budget_seconds = 600, dispatch_budget_seconds = 630,
        total_budget_seconds = 3600, max_movement_share = .20,
        policy = NULL, workers = 1L,
        audited_base_commit = "7a6fda1e45d2810219de3103caf5408fbe9172b2",
        role = "Fresh single-method rho development comparison; not common-rho calibration, convergence certification or paper results",
        panel_spec_note = "Reuse only D-036 deterministic panel/model constants; its run counts/seeds/PG screen are NOT executed. New seeds above govern generation and two starts.")
}

graphmode4_rho_experiment_identity <- function(repository) {
    tuning <- graphmode4_tuning_identity(repository)
    files <- c("scripts/graphmode-r4-rho-experiment.R",
        "scripts/tests/graphmode-r4-rho-experiment-deterministic.R",
        "docs/GRAPHMODE_R4_RHO_EXPERIMENT_2026-09-12.md")
    git <- function(args) {
        x <- system2("git", c("-C", shQuote(repository), args), stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(x, "status"))) stop("Cannot inspect rho orchestration freeze.", call. = FALSE)
        x
    }
    list(tuning = tuning, sha256 = setNames(vapply(file.path(repository, files),
        function(f) digest::digest(file = f, algo = "sha256", serialize = FALSE), character(1)), files),
        committed = tuning$committed && !length(git(c("status", "--porcelain", "--", shQuote(files)))) &&
            setequal(files, git(c("ls-files", "--", shQuote(files)))))
}

graphmode4_rho_experiment_record <- function(repository, directory, identity, runtime) {
    x <- list(spec = graphmode4_rho_experiment_spec(), repository = repository,
        directory = directory, output_dir = file.path(directory, "run"),
        identity = identity, runtime = runtime,
        authority = "User confirmed 2026-09-12: rho 1/2/4/8 x two starts; 400/200; fixed guidance; single-thread serial; 60 minutes; read results and diagnose; no automatic extension",
        formal_authorized = FALSE, resume_supported = FALSE)
    x$signature <- graphmode_digest(x)
    x
}

graphmode4_rho_experiment_guard <- function(record, repository, active = FALSE) {
    rebuilt <- graphmode4_rho_experiment_record(record$repository, record$directory,
        record$identity, record$runtime)
    if (!identical(record, rebuilt) || !identical(normalizePath(repository), record$repository) ||
        !identical(graphmode4_rho_experiment_identity(repository), record$identity) ||
        !isTRUE(record$identity$committed) ||
        !isTRUE(record$identity$tuning$base$audited_core_unchanged) ||
        !isTRUE(record$identity$tuning$base$r4$requirements_match) ||
        !identical(graphmode4_pilot_runtime(), record$runtime) ||
        any(record$runtime$threads != "1") || record$runtime$locale != "C")
        stop("Rho registration/source/runtime changed or unfrozen.", call. = FALSE)
    if (!dir.exists(record$directory) ||
        !identical(normalizePath(record$directory), record$directory) ||
        !identical(readRDS(file.path(record$directory, "registration.rds")), record))
        stop("Rho registration tree changed; never recreate it.", call. = FALSE)
    if (active) {
        if (!dir.exists(record$output_dir) ||
            !identical(normalizePath(record$output_dir), record$output_dir) ||
            !identical(readRDS(file.path(record$output_dir, "launch.rds"))$record, record))
            stop("Active rho output tree changed; never recreate it.", call. = FALSE)
    } else if (file.exists(record$output_dir) || file.exists(file.path(record$directory, "batch-execution.rds")))
        stop("Rho run already attempted; no retry or resume.", call. = FALSE)
    invisible(TRUE)
}

graphmode4_rho_experiment_prepare <- function(repository, directory) {
    repository <- normalizePath(repository, mustWork = TRUE)
    directory <- graphmode4_output_target(directory)
    if (!dir.exists(dirname(directory)) || file.exists(directory) ||
        identical(directory, repository) || startsWith(directory, paste0(repository, "/")))
        stop("Use a new external directory with an existing parent.", call. = FALSE)
    identity <- graphmode4_rho_experiment_identity(repository); runtime <- graphmode4_pilot_runtime()
    if (!isTRUE(identity$committed) || !isTRUE(identity$tuning$base$audited_core_unchanged) ||
        !isTRUE(identity$tuning$base$r4$requirements_match) ||
        !graphmode_pilot_loaded_source_ok(repository, identity$tuning$base$r4) ||
        any(runtime$threads != "1") || runtime$locale != "C")
        stop("Freeze source and set the single-thread C runtime first.", call. = FALSE)
    record <- graphmode4_rho_experiment_record(repository, directory, identity, runtime)
    if (!dir.create(directory)) stop("Cannot create exclusive rho registration.", call. = FALSE)
    graphmode_save_new(record, file.path(directory, "registration.rds"))
    graphmode4_rho_experiment_guard(record, repository)
    list(ready = TRUE, signature = record$signature,
        commit = identity$tuning$base$r4$core$commit, directory = directory,
        scientific_draws = 0L, formal_authorized = FALSE)
}

graphmode4_rho_experiment_generate <- function(spec) {
    if (!identical(spec, graphmode4_rho_experiment_spec())) stop("Changed rho specification.", call. = FALSE)
    s <- spec$seeds; old <- spec$panel_spec
    innovations <- graphmode_pilot_seeded(s[["innovations"]], array(stats::rnorm(5 * 168 * 3), c(5L, 168L, 3L)))
    profiles <- graphmode_pilot_seeded(s[["profile_permutation"]], sample.int(5L))
    units <- graphmode_pilot_seeded(s[["unit_permutation"]], sample.int(121L))
    lambda <- graphmode4_pilot_panel(old, innovations, profiles, units)$lambda_original
    counts <- graphmode_pilot_seeded(s[["response"]], matrix(stats::rpois(length(lambda), lambda), 121L, 168L))
    panel <- graphmode4_pilot_panel(old, innovations, profiles, units, counts)
    starts <- lapply(seq_along(spec$start_occupancy), function(j)
        graphmode_pilot_seeded(s[[sprintf("start_%02d", j)]],
            sample(rep(seq_len(spec$start_occupancy[j]), length.out = old$n))))
    list(panel = panel, starts = starts)
}

graphmode4_rho_experiment_screen <- function(record, generated) {
    s <- record$spec; fit <- generated$panel$fit
    if (length(generated$starts) != 2L) stop("Require two registered starts.", call. = FALSE)
    states <- lapply(seq_along(generated$starts), function(j) {
        z <- generated$starts[[j]]
        if (length(unique(z)) != s$start_occupancy[j]) stop("Changed initial occupancy.", call. = FALSE)
        graphmode_initial_state(fit$core, z)
    })
    core <- graphmode_run_plan(fit$core, states[[1L]], record$identity$tuning$base$r4$core,
        unname(s$seeds[["chain_01"]]), s$iterations, s$warmup, s$thin, s$checkpoint_every,
        s$worker_budget_seconds, file.path(record$output_dir, "unused-template"),
        basename(record$directory), record$authority, "development-pilot", s$panel_spec$thresholds,
        list(data_identity = graphmode_digest(fit$core$Y), geometry_identity = fit$geometry_identity,
            data_seed_record = paste(names(s$seeds), s$seeds, collapse = ";"),
            permutation_seed_record = graphmode_digest(generated$panel$provenance),
            calibration_decision = "D-041 fixed guidance .25; no rho yet selected",
            scientific_role = s$role))
    template <- graphmode4_controlled_plan(core, fit, NULL, record$identity$tuning, record$runtime)
    graphmode4_rho_screen_plan(template, states, unname(s$seeds[c("chain_01", "chain_02")]),
        s$rhos, record$output_dir, s$max_movement_share)
}

graphmode4_rho_experiment_child <- function(command, path, repository, timeout_seconds) {
    args <- c("--vanilla", shQuote(file.path(repository, "scripts/graphmode-r4-tuning.R")),
        command, shQuote(path), if (command == "run") "--authorized")
    output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), args,
        stdout = TRUE, stderr = TRUE, timeout = ceiling(timeout_seconds)))
    status <- attr(output, "status")
    list(status = if (is.null(status)) 0L else as.integer(status), output = output)
}

graphmode4_rho_experiment_time_left <- function(spec, elapsed) {
    is.finite(elapsed) && elapsed >= 0 &&
        elapsed + spec$dispatch_budget_seconds + 30 < spec$total_budget_seconds
}

graphmode4_rho_experiment_batch <- function(record, repository, authorized = FALSE) {
    if (!identical(authorized, TRUE)) stop("Explicit run authorization required.", call. = FALSE)
    graphmode4_rho_experiment_guard(record, repository)
    started <- proc.time()[[3L]]; elapsed <- function() proc.time()[[3L]] - started
    if (!dir.create(record$output_dir)) stop("Cannot create exclusive rho run.", call. = FALSE)
    graphmode_save_new(list(record = record, authorized = TRUE), file.path(record$output_dir, "launch.rds"))
    save <- function(x, name) {
        graphmode4_rho_experiment_guard(record, repository, active = TRUE)
        graphmode_save_new(x, file.path(record$output_dir, name))
    }
    log <- function(...) {cat(format(Sys.time()), ..., "\n"); flush.console()}
    tryCatch({
        log("Generating registered independent development panel; not formal simulation.")
        generated <- graphmode4_rho_experiment_generate(record$spec)
        save(generated, "generation.rds")
        screen <- graphmode4_rho_experiment_screen(record, generated)
        save(screen, "screen.rds")
        # Persist every branch before any chain starts.
        for (id in names(screen$jobs)) save(screen$jobs[[id]]$plan, paste0("PLAN-", id, ".rds"))
        exits <- list(); stop_reason <- NULL
        for (id in names(screen$jobs)) {
            if (!graphmode4_rho_experiment_time_left(record$spec, elapsed())) {
                stop_reason <- "Insufficient registered total budget; remaining branches not launched."; break
            }
            graphmode4_rho_experiment_guard(record, repository, active = TRUE)
            path <- file.path(record$output_dir, paste0("PLAN-", id, ".rds"))
            log("Starting", id, "400 sweeps / 200 settling; fixed guidance; no resume.")
            x <- graphmode4_rho_experiment_child("run", path, repository, record$spec$dispatch_budget_seconds)
            exits[[id]] <- x; save(x, paste0("DISPATCH-", id, ".rds"))
            log("Finished", id, "dispatcher exit", x$status, "; total seconds", round(elapsed(), 1))
            if (!identical(x$status, 0L)) {
                log(paste(x$output, collapse = "\n"))
                stop_reason <- paste(id, "failed; stop this batch without retry."); break
            }
        }
        evidence <- lapply(screen$jobs, function(job) graphmode4_controlled_evidence(job$plan))
        for (id in names(evidence)) if (is.null(exits[[id]]) || !identical(exits[[id]]$status, 0L))
            evidence[[id]]$read_errors <- c(evidence[[id]]$read_errors, "Outer dispatcher did not successfully finish.")
        reduced <- graphmode4_rho_screen_reduce(screen, evidence)
        report <- list(registration_signature = record$signature, screen_signature = screen$signature,
            dispatched = names(exits), dispatch_status = vapply(exits, `[[`, integer(1), "status"),
            stop_reason = stop_reason, rho = reduced,
            movement = lapply(evidence, function(e) e$result$movement),
            elapsed_seconds = elapsed(), formal_authorized = FALSE, convergence_certified = FALSE)
        save(report, "report.rds")
        log("Batch finished. Rho resolved:", reduced$resolved, "; no automatic additional run.")
        print(reduced)
        invisible(report)
    }, error = function(e) {
        tryCatch(save(list(error = conditionMessage(e), elapsed_seconds = elapsed()), "batch-failure.rds"),
            error = function(other) message("Failure retained in console: ", conditionMessage(e), "; ", conditionMessage(other)))
        stop(conditionMessage(e), call. = FALSE)
    })
}
