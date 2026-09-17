# C6: the four additional models on the saved Figure 3(a) development panel.
# Reuse frozen kernels; this module is not loaded by historical source globs.
graphmode_compare_version <- "graphmode-four-comparators-20260917-v1"

graphmode_compare_spec <- function() {
    methods <- c("graphMoDE-C", "EucMoDE", "MoDE", "PottsMoDE")
    jobs <- do.call(rbind, lapply(1:4, function(j) data.frame(
        method = if (j %% 2L) methods else rev(methods), chain = j)))
    jobs$name <- sprintf("%s-chain-%02d", jobs$method, jobs$chain)
    jobs$seed <- as.integer(2026121101:2026121116)
    list(schema = graphmode_compare_version, methods = methods, jobs = jobs,
        iterations = 600L, warmup = 300L, thin = 1L, start_occupancy = c(1L, 3L, 7L, 10L),
        potts_beta = 1, euclidean_range_rule = "median of positive distinct-pair Euclidean distances; geometry only",
        parameter_role = "fixed development settings, not validation-selected optima; jointly redesign all methods for future server studies",
        worker_seconds = 1200, science_seconds = 14400, diagnostic_seconds = 1800,
        storage = c(stage_bytes = 32 * 1024^3, start_free_bytes = 64 * 1024^3, min_free_bytes = 8 * 1024^3),
        role = "four additional models on the existing c5 panel; reuse c5 W-reference as base comparison and W-joint as separate computational result")
}

graphmode_compare_configurations <- function(blind, spec) {
    old <- blind$fit; graphmode4_validate_config(old)
    road <- graphmode4_reorder(graphmode4_road("intertwined-spiral"), old$ids)$road
    core <- old$core
    expert <- core[c("m0", "C0", "family", "dynamics", "G", "W", "rho", "variance_prior")]
    gate <- core[c("guidance_prior", "guidance_proposal_sd", "max_ess_steps")]
    distances <- road$euclidean_distance[upper.tri(road$euclidean_distance)]
    range <- stats::median(distances[distances > 0])
    fits <- setNames(lapply(spec$methods, function(method) {
        f <- graphmode4_config(road, core$Y, core$Fmat, method, expert, gate,
            calibration_id = "c6-fixed-development-settings-not-formal-calibration",
            euclidean_range = if (method == "EucMoDE") range else NULL,
            range_rule = if (method == "EucMoDE") spec$euclidean_range_rule else NULL,
            potts_beta = if (method == "PottsMoDE") spec$potts_beta else NULL)
        common <- c("Y", "Fmat", names(expert), "K")
        if (!identical(f$core[common], core[common]) || !identical(f$ids, old$ids))
            stop("Comparison changed observations, shared expert model, capacity or unit order.", call. = FALSE)
        f
    }), spec$methods)
    list(fits = fits, euclidean_range = range, geometry_identity = graphmode_digest(road))
}

graphmode_compare_initial <- function(fit, source) {
    c <- fit$core
    if (source$iteration != 0L) stop("Only saved pre-run initial states may be reused.", call. = FALSE)
    state <- graphmode_initial_state(c, source$Z, source$theta, NULL,
        if (c$adaptive) source$x else NULL, if (c$adaptive) source$v else NULL)
    if (!identical(state$Z, source$Z) || !identical(state$theta, source$theta))
        stop("Common initialization was changed.", call. = FALSE)
    state
}

graphmode_compare_step <- function(state, config, policy) {
    if (config$adaptive) return(graphmode_joint_run_step(state, config, policy, NULL, FALSE, 10L))
    if (!config$method %in% c("MoDE", "PottsMoDE")) stop("Unknown nonadaptive method.", call. = FALSE)
    started <- proc.time()[[3L]]; offset <- NULL; calls <- 0L
    expert <- function(current, Yk, config, sigma2 = NULL) {
        if (!is.null(sigma2)) stop("This comparison uses Poisson experts.", call. = FALSE)
        if (is.null(offset)) offset <<- graphmode_expert_blocks_offset(nrow(config$Fmat), policy$block)
        calls <<- calls + 1L
        graphmode_expert_blocks_expert(current, Yk, config, policy$block, offset, TRUE)$update
    }
    # MoDE Dirichlet/Gibbs and Potts sequential Gibbs remain in the original driver.
    driver <- graphmode_dev_bind(graphmode_sweep, list(graphmode_update_expert = expert))
    out <- driver(state, config)
    if (calls != config$K) stop("Incomplete expert scan.", call. = FALSE)
    next_state <- out$state
    d <- list(iteration = next_state$iteration, policy = policy, block_offset = offset,
        before_signature = graphmode_digest(state), after_signature = graphmode_digest(next_state),
        expert = lapply(out$expert_updates, function(e) { e$theta <- NULL; e }),
        gate = NULL, potts_gross = out$potts_gross, events = out$events,
        transition_seconds = proc.time()[[3L]] - started)
    list(state = next_state, base_state = next_state, base_diagnostic = d,
        joint = NULL, candidate = NULL, events = out$events)
}

graphmode_compare_check <- function(before, out, config, record) {
    if (config$adaptive) {
        graphmode_joint_run_check_step(before, out, config, record, FALSE)
        return(invisible(TRUE))
    }
    s <- out$state; d <- out$base_diagnostic
    rebuilt <- graphmode_initial_state(config, s$Z, s$theta)
    rebuilt$iteration <- before$iteration + 1L
    if (config$method == "MoDE") {
        if (!is.numeric(s$pi) || length(s$pi) != config$K || any(!is.finite(s$pi) | s$pi <= 0) ||
            abs(sum(s$pi) - 1) > 1e-12) stop("Invalid MoDE mixing probabilities.", call. = FALSE)
        rebuilt$pi <- s$pi
    }
    if (!identical(s, rebuilt) || !identical(s, out$base_state) || !is.null(out$joint) ||
        !is.null(out$candidate) || !is.null(d$gate) || length(d$expert) != config$K ||
        !identical(d$before_signature, graphmode_digest(before)) ||
        !identical(d$after_signature, graphmode_digest(s)) ||
        !identical(out$events, graphmode_allocation_events(before$Z, s$Z, config$K)))
        stop("Nonadaptive state/scan handoff mismatch.", call. = FALSE)
    for (k in seq_len(config$K)) graphmode_blocks_run_expert_check(d$expert[[k]],
        matrix(before$theta[k, , ], nrow(config$Fmat), ncol(config$Fmat)),
        matrix(s$theta[k, , ], nrow(config$Fmat), ncol(config$Fmat)),
        config$Y[before$Z == k, , drop = FALSE], config, record$base_policy$block,
        d$block_offset, record$thresholds)
    if (config$method == "PottsMoDE") {
        # Gross births/deaths count intermediate single-site states, not net moves.
        z <- before$Z; sizes <- tabulate(z, config$K)
        gross <- c(births = 0, deaths = 0, upcrossings = 0, downcrossings = 0)
        for (i in seq_along(z)) if (z[i] != s$Z[i]) {
            old <- z[i]; new <- s$Z[i]
            gross <- gross + c(sizes[new] == 0L, sizes[old] == 1L,
                sizes[new] == 4L, sizes[old] == 5L)
            sizes[old] <- sizes[old] - 1L; sizes[new] <- sizes[new] + 1L; z[i] <- new
        }
        if (!identical(d$potts_gross, gross)) stop("Potts sequential event accounting changed.", call. = FALSE)
    }
    invisible(TRUE)
}

graphmode_compare_guard <- function(record, repository, loaded_sha) {
    u <- record; u$signature <- NULL
    if (!identical(record$schema, graphmode_compare_version) ||
        !identical(record$signature, graphmode_digest(u)) ||
        !identical(record$spec, graphmode_compare_spec()) ||
        !identical(record$identity, graphmode_joint_run_identity(repository, names(record$identity$sha256))) ||
        !identical(unname(loaded_sha), unname(record$identity$sha256[names(loaded_sha)])) ||
        !identical(record$repository, repository) ||
        !identical(normalizePath(record$directory, mustWork = TRUE), record$directory))
        stop("Comparison registration/source identity changed.", call. = FALSE)
    for (path in names(record$input_sha256)) if (!identical(
        digest::digest(file = path, algo = "sha256", serialize = FALSE), unname(record$input_sha256[path])))
        stop("Registered comparison input changed.", call. = FALSE)
    if (Sys.getenv("LC_ALL") != "C" || any(Sys.getenv(c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS",
        "MKL_NUM_THREADS", "VECLIB_MAXIMUM_THREADS")) != "1")) stop("Use C locale and one numeric thread.", call. = FALSE)
    if (!identical(record$runtime, graphmode4_pilot_runtime())) stop("Registered runtime changed.", call. = FALSE)
    invisible(TRUE)
}

graphmode_compare_prepare <- function(repository, destination, files, loaded_sha, c5_directory) {
    if (file.exists(destination) || !dir.exists(dirname(destination)) || startsWith(destination, paste0(repository, "/")))
        stop("Require a new external destination.", call. = FALSE)
    prior <- readRDS(file.path(c5_directory, "registration.rds"))
    unsigned_prior <- prior; unsigned_prior$signature <- NULL
    if (prior$identity$commit != "1520bd34de41414e2c3c2617a3802286372a0ea5" ||
        prior$signature != "1d2515ab6715ce8c1a09767cc3150c9720bc02bb29f347e4fe7ffe5685b72dba" ||
        !identical(prior$signature, graphmode_digest(unsigned_prior)))
        stop("Wrong c5 source registration.", call. = FALSE)
    for (f in names(prior$identity$sha256)) if (digest::digest(file = file.path(repository, f),
        algo = "sha256", serialize = FALSE) != prior$identity$sha256[[f]]) stop("Original c5 source changed: ", f)
    if (!file.exists(file.path(c5_directory, "completion.json")) || file.exists(file.path(c5_directory, "failure.json")))
        stop("The reused W comparison must be complete.", call. = FALSE)
    spec <- graphmode_compare_spec()
    collision <- graphmode_joint_run_seeds(dirname(destination), spec$jobs$seed)
    blind <- readRDS(prior$input); source_starts <- readRDS(file.path(c5_directory, "fresh-starts.rds"))
    built <- graphmode_compare_configurations(blind, spec)
    starts <- lapply(built$fits, function(f) lapply(source_starts, function(s) graphmode_compare_initial(f, s$state)))
    for (ss in starts) if (!identical(vapply(ss, function(s) as.integer(length(unique(s$Z))), integer(1)), spec$start_occupancy))
        stop("The four common initial occupancies changed.", call. = FALSE)
    truth_path <- file.path(dirname(prior$input), "generation.rds") # evaluation stage only
    pinned <- setNames(c("ce452d778c9aa47fe560117feedffd2731afd95e8851ba489dd6f62e3136580a",
        "db92197ccee45005cdf48893481e0ec0a612cce74606454562d91bd2f1986d07",
        "71d13e713f9f8b29ece19374ad25f8eaad34d941679d64b12a82ac3ccef31ff4"),
        c(prior$input, truth_path, file.path(c5_directory, "diagnostic-report.rds")))
    for (p in names(pinned)) if (digest::digest(file = p, algo = "sha256", serialize = FALSE) != pinned[[p]])
        stop("Previously identified c4/c5 evidence changed.", call. = FALSE)
    paths <- c(file.path(c5_directory, c("registration.rds", "fresh-starts.rds", "completion.json",
        "science-complete.rds", "diagnostic-report.rds")), prior$input, truth_path)
    record <- list(schema = graphmode_compare_version, repository = repository, directory = destination,
        spec = spec, identity = graphmode_joint_run_identity(repository, files),
        runtime = graphmode4_pilot_runtime(), input_sha256 = setNames(vapply(paths, function(p)
            digest::digest(file = p, algo = "sha256", serialize = FALSE), character(1)), paths),
        prior_directory = c5_directory, prior_signature = prior$signature, truth_path = truth_path,
        fits = built$fits, starts = starts, euclidean_range = built$euclidean_range,
        base_policy = prior$base_policy, thresholds = prior$thresholds, seed_check = collision,
        authority = "User authorized the four additional models on this panel; future large-server study jointly resets all settings",
        formal_authorized = FALSE, resume_supported = FALSE)
    record$signature <- graphmode_digest(record)
    graphmode_warmup_storage(record, TRUE)
    if (!dir.create(destination)) stop("Cannot create comparison destination.")
    graphmode_save_new(record, file.path(destination, "registration.rds"))
    graphmode_compare_guard(record, repository, loaded_sha)
    record
}

graphmode_compare_science <- function(record, repository, loaded_sha) {
    graphmode_compare_guard(record, repository, loaded_sha)
    dir <- record$directory; spec <- record$spec; receipts <- list()
    graphmode_save_new(list(signature = record$signature, started = Sys.time()), file.path(dir, "science-request.rds"))
    batch_start <- proc.time()[[3L]]
    for (j in seq_len(nrow(spec$jobs))) {
        job <- spec$jobs[j, ]; fit <- record$fits[[job$method]]; config <- fit$core
        path <- file.path(dir, job$name)
        if (!dir.create(path)) stop("Worker output already exists.")
        graphmode_compare_guard(record, repository, loaded_sha); graphmode_warmup_storage(record)
        state <- record$starts[[job$method]][[job$chain]]; draws <- diagnostics <- list(); started <- proc.time()[[3L]]
        graphmode_save_new(list(signature = record$signature, job = job, initial = state), file.path(path, "job.rds"))
        cat(format(Sys.time()), "START", job$name, "600 steps\n"); flush.console()
        graphmode_pilot_seeded(job$seed, tryCatch({
            for (i in seq_len(spec$iterations)) {
                if (proc.time()[[3L]] - started > spec$worker_seconds || proc.time()[[3L]] - batch_start > spec$science_seconds)
                    stop("Registered comparison time budget exhausted.")
                before <- state; out <- graphmode_compare_step(before, config, record$base_policy)
                graphmode_compare_check(before, out, config, record)
                state <- out$state; diagnostics[[i]] <- out
                if (i > spec$warmup) draws[[length(draws) + 1L]] <- state[c("iteration", "Z", "theta", "sigma2", "v", "pi")]
                if (i %% 100L == 0L) {
                    graphmode_warmup_storage(record)
                    graphmode_save_new(list(state = state, rng = .Random.seed, diagnostics = diagnostics,
                        draws = draws, resume_supported = FALSE), file.path(path, sprintf("checkpoint-%04d.rds", i)))
                }
                if (i %% 25L == 0L) {
                    graphmode_joint_run_json(list(phase = "science", worker = job$name, job = j, jobs = 16L,
                        iteration = i, iterations = spec$iterations, Kocc = length(unique(state$Z)),
                        updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")), file.path(dir, "progress.json"))
                    cat(format(Sys.time()), job$name, i, "/ 600 Kocc", length(unique(state$Z)), "\n"); flush.console()
                }
            }
            result <- list(schema = graphmode_compare_version, registration_signature = record$signature,
                method = job$method, chain = job$chain, seed = job$seed, state = state, draws = draws,
                diagnostics = diagnostics, rng = .Random.seed, resume_supported = FALSE)
            graphmode_save_new(result, file.path(path, "result.rds"))
        }, error = function(e) {
            graphmode_save_new(list(error = conditionMessage(e), state = state, diagnostics = diagnostics,
                draws = draws, resume_supported = FALSE), file.path(path, "failure.rds")); stop(e)
        }))
        elapsed <- proc.time()[[3L]] - started
        receipt <- list(worker = job$name, elapsed_seconds = elapsed,
            sha256 = digest::digest(file = file.path(path, "result.rds"), algo = "sha256", serialize = FALSE))
        graphmode_save_new(receipt, file.path(path, "receipt.rds"))
        if (elapsed > spec$worker_seconds) stop("Complete worker exceeded its budget.")
        receipts[[job$name]] <- receipt
        cat(format(Sys.time()), "DONE", job$name, "seconds", elapsed, "\n"); flush.console()
        rm(result, draws, diagnostics, out); invisible(gc())
    }
    graphmode_save_new(list(signature = record$signature, workers = receipts, completed = Sys.time()), file.path(dir, "science-complete.rds"))
}

graphmode_compare_summary <- function(draws, fit, truth) {
    # Same truth-blind Dahl representative and unit-level profile summary for all models.
    z <- do.call(rbind, lapply(draws, function(d) d$Z))
    means <- array(0, c(length(draws), fit$core$K, nrow(fit$core$Fmat)))
    for (i in seq_along(draws)) means[i, , ] <- exp(gmde_eta(draws[[i]]$theta, fit$core$Fmat))
    summary <- graphmode_summarize(z, means)
    score <- graphmode_evaluate(summary, truth$Z, truth$lambda)
    pairs <- upper.tri(summary$similarity); target <- outer(truth$Z, truth$Z, "==")[pairs]
    exact <- vapply(draws, function(d) identical(outer(d$Z, d$Z, "==")[pairs], target), logical(1))
    list(summary = summary, metrics = score, exact_by_chain = colSums(matrix(exact, 300L, 4L)),
        note = "One development panel; pooled retained summaries, not independent data replications or calibrated interval coverage")
}

graphmode_compare_diagnose <- function(record, repository, loaded_sha) {
    graphmode_compare_guard(record, repository, loaded_sha)
    dir <- record$directory; spec <- record$spec; science <- readRDS(file.path(dir, "science-complete.rds"))
    if (!identical(science$signature, record$signature) || length(science$workers) != 16L) stop("Incomplete comparison science.")
    graphmode_save_new(list(signature = record$signature, started = Sys.time()), file.path(dir, "diagnostic-request.rds"))
    graphmode_joint_run_json(list(phase = "diagnosis", job = 16L, jobs = 16L, updated = format(Sys.time())), file.path(dir, "progress.json"))
    generation <- readRDS(record$truth_path)
    if (!identical(generation$fit$ids, record$fits[[1L]]$ids) || !identical(generation$fit$core$Y, record$fits[[1L]]$core$Y))
        stop("Evaluation truth is not aligned to this observed panel.")
    reports <- results <- costs <- list()
    for (method in spec$methods) {
        fit <- record$fits[[method]]; chains <- all_draws <- list(); seconds <- numeric(4L)
        for (j in 1:4) {
            name <- sprintf("%s-chain-%02d", method, j); path <- file.path(dir, name, "result.rds")
            if (digest::digest(file = path, algo = "sha256", serialize = FALSE) != science$workers[[name]]$sha256)
                stop("Completed worker identity changed.")
            r <- readRDS(path); job <- spec$jobs[spec$jobs$name == name, ]; previous <- record$starts[[method]][[j]]
            if (!identical(r$registration_signature, record$signature) || !identical(r$seed, job$seed) ||
                r$state$iteration != spec$iterations || length(r$diagnostics) != spec$iterations || length(r$draws) != 300L)
                stop("Incomplete or mismatched comparator output.")
            for (i in seq_len(spec$iterations)) {
                d <- r$diagnostics[[i]]; graphmode_compare_check(previous, d, fit$core, record)
                if (i > spec$warmup && !identical(r$draws[[i - spec$warmup]], d$state[c("iteration", "Z", "theta", "sigma2", "v", "pi")]))
                    stop("Retained draw differs from completed outer state.")
                previous <- d$state
            }
            if (!identical(previous, r$state)) stop("Final comparator state changed.")
            chains[[j]] <- list(complete = TRUE, config_signature = fit$signature, seed = r$seed,
                iterations = spec$iterations, warmup = spec$warmup, thin = 1L, numerical_guards_passed = TRUE,
                movement = lapply(r$diagnostics, function(d) list(Z = d$state$Z,
                    sizes = tabulate(d$state$Z, fit$core$K), events = d$events)), draws = r$draws, failure = "")
            all_draws <- c(all_draws, r$draws); seconds[j] <- science$workers[[name]]$elapsed_seconds
            rm(r); invisible(gc())
        }
        jobs <- spec$jobs[match(sprintf("%s-chain-%02d", method, 1:4), spec$jobs$name), ]
        reports[[method]] <- graphmode4_validity(chains, fit, jobs$seed, spec$iterations, spec$warmup, 1L)
        results[[method]] <- graphmode_compare_summary(all_draws, fit, generation$truth)
        costs[[method]] <- seconds
        cat(format(Sys.time()), "SUMMARIZED", method, "\n"); flush.console()
        rm(chains, all_draws); invisible(gc())
    }
    # Reuse W's already completed diagnostic report. Only compute common descriptive scores.
    old <- readRDS(file.path(record$prior_directory, "diagnostic-report.rds"))
    old_science <- readRDS(file.path(record$prior_directory, "science-complete.rds"))
    for (arm in c("reference", "joint")) {
        name <- paste0("graphMoDE-W-", arm); draws <- list()
        for (j in 1:4) {
            worker <- sprintf("%s-chain-%02d", arm, j); p <- file.path(record$prior_directory, worker, "result.rds")
            if (digest::digest(file = p, algo = "sha256", serialize = FALSE) != old_science$workers[[worker]]$sha256)
                stop("Reused c5 result changed.")
            r <- readRDS(p); draws <- c(draws, r$draws); rm(r); invisible(gc())
        }
        reports[[name]] <- old$arms[[arm]]; costs[[name]] <- old$worker_seconds[[arm]]
        results[[name]] <- graphmode_compare_summary(draws, generation$fit, generation$truth)
        rm(draws); invisible(gc())
    }
    report <- list(registration_signature = record$signature, diagnostics = reports, results = results,
        worker_seconds = costs, settings = spec, formal_authorized = FALSE,
        note = "W-reference is the base five-model comparator; W-joint is a separate enhanced sampler result. Future server studies reset settings jointly.")
    graphmode_save_new(report, file.path(dir, "comparison-report.rds"))
    table <- do.call(rbind, lapply(names(results), function(m) data.frame(method = m,
        ARI = results[[m]]$metrics$ARI, Khat = results[[m]]$metrics$Khat,
        P_Kocc5 = results[[m]]$summary$Pr_Kocc_5, coclustering_brier = results[[m]]$metrics$coclustering_brier,
        profile_rmse = results[[m]]$metrics$profile_rmse,
        exact_partitions = sum(results[[m]]$exact_by_chain), retained_partitions = 1200L,
        scalar_failed = sum(!reports[[m]]$scalars$status %in% c("passed", "uninformative-constant-discrete")),
        psm_failed = sum(reports[[m]]$psm_rms$rms > .05), statistical_valid = reports[[m]]$valid,
        worker_seconds = sum(costs[[m]]))))
    write.csv(table, file.path(dir, "comparison-summary.csv"), row.names = FALSE)
    graphmode_joint_run_json(list(status = "completed", registration_signature = record$signature,
        completed = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), results = table,
        report_sha256 = digest::digest(file = file.path(dir, "comparison-report.rds"), algo = "sha256", serialize = FALSE),
        role = spec$role, formal_authorized = FALSE), file.path(dir, "completion.json"), TRUE)
}
