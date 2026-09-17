# C5: fresh chains on the fixed c4 blinded panel; never resume old checkpoints.
graphmode_joint_run_version <- "graphmode-joint-comparison-20260917-v1"

graphmode_joint_run_spec <- function() {
    components <- c("Z", "v", "x", "theta", "reference", "joint")
    seeds <- setNames(2026120101:2026120124,
        unlist(lapply(components, function(x) sprintf("%s_%02d", x, 1:4))))
    list(schema = graphmode_joint_run_version, iterations = 600L, warmup = 300L,
        thin = 1L, joint_every = 10L, start_occupancy = c(1L, 3L, 7L, 10L), seeds = seeds,
        jobs = data.frame(chain = rep(1:4, each = 2L),
            arm = c("reference", "joint", "joint", "reference", "reference", "joint", "joint", "reference")),
        worker_seconds = 900, science_seconds = 7200, diagnostic_seconds = 600,
        storage = c(stage_bytes = 16 * 1024^3, start_free_bytes = 32 * 1024^3, min_free_bytes = 5 * 1024^3),
        joint_policy = graphmode_joint_partition_policy(),
        role = "fixed c4 observed-panel development comparison, fresh dispersed starts and independent arm seeds; no checkpoint continuation or formal release")
}

graphmode_joint_run_json <- function(value, path, exclusive = FALSE) {
    if (exclusive && file.exists(path)) stop("Output already exists: ", path, call. = FALSE)
    temporary <- paste0(path, ".writing")
    jsonlite::write_json(value, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 16)
    if (!file.rename(temporary, path)) stop("Atomic JSON publication failed.", call. = FALSE)
}

graphmode_joint_run_identity <- function(repository, files) {
    command <- function(args) {
        out <- system2("git", c("-C", shQuote(repository), args), stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(out, "status"))) stop("Git identity check failed.", call. = FALSE)
        out
    }
    if (length(command(c("status", "--porcelain", "--", shQuote(files)))))
        stop("Execution source must be committed before registration.", call. = FALSE)
    if (!setequal(command(c("ls-files", "--", shQuote(files))), files))
        stop("Execution source is not fully tracked.", call. = FALSE)
    list(commit = command(c("rev-parse", "HEAD")), sha256 = setNames(vapply(file.path(repository, files),
        function(f) digest::digest(file = f, algo = "sha256", serialize = FALSE), character(1)), files))
}

graphmode_joint_run_guard <- function(record, repository, loaded_sha) {
    unsigned <- record; unsigned$signature <- NULL
    if (!identical(record$signature, graphmode_digest(unsigned)) ||
        !identical(record$spec, graphmode_joint_run_spec()) || !identical(record$repository, repository) ||
        !identical(record$identity, graphmode_joint_run_identity(repository, names(record$identity$sha256))) ||
        !identical(unname(loaded_sha), unname(record$identity$sha256[names(loaded_sha)])))
        stop("Registration or loaded execution identity changed.", call. = FALSE)
    if (!identical(normalizePath(record$directory, mustWork = TRUE), record$directory))
        stop("Output directory moved; never recreate it.", call. = FALSE)
    for (p in names(record$input_sha256)) if (!identical(
        digest::digest(file = p, algo = "sha256", serialize = FALSE), unname(record$input_sha256[p])))
        stop("Registered input changed.", call. = FALSE)
    if (Sys.getenv("LC_ALL") != "C" || any(Sys.getenv(c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS",
        "MKL_NUM_THREADS", "VECLIB_MAXIMUM_THREADS")) != "1")) stop("Use C locale and one numeric thread.", call. = FALSE)
    invisible(TRUE)
}

graphmode_joint_run_seeds <- function(root, proposed) {
    extract <- function(x, inside = FALSE) {
        if (is.list(x)) {
            nm <- names(x); if (is.null(nm)) nm <- rep("", length(x))
            return(unlist(lapply(seq_along(x), function(i) {
                if (grepl("Random.seed|rng_state", nm[i], ignore.case = TRUE)) return(numeric())
                extract(x[[i]], inside || grepl("seed", nm[i], ignore.case = TRUE))
            }), use.names = FALSE))
        }
        if (inside && is.numeric(x)) x[is.finite(x) & x >= 1 & x == floor(x)] else numeric()
    }
    files <- list.files(root, "registration.*[.]rds$", recursive = TRUE, full.names = TRUE)
    seen <- numeric(); locations <- character()
    for (f in files) { seen <- c(seen, extract(readRDS(f))); locations <- c(locations, f) }
    archives <- list.files(root, "[.]zip$", recursive = TRUE, full.names = TRUE)
    for (archive in archives) {
        members <- utils::unzip(archive, list = TRUE)$Name
        for (member in members[grepl("registration[^/]*[.]rds$", members)]) {
            con <- gzcon(unz(archive, member, open = "rb"))
            value <- tryCatch(readRDS(con), finally = close(con))
            seen <- c(seen, extract(value)); locations <- c(locations, paste(archive, member, sep = "::"))
        }
    }
    if (anyDuplicated(proposed) || length(intersect(proposed, seen)))
        stop("Seed collision; no registration or run.", call. = FALSE)
    list(registrations = locations, proposed = proposed, collision_count = 0L)
}

graphmode_joint_run_prepare <- function(repository, destination, files, loaded_sha) {
    if (file.exists(destination) || !dir.exists(dirname(destination)) ||
        startsWith(destination, paste0(repository, "/"))) stop("Require a new external destination.", call. = FALSE)
    parent <- "/Users/liuzw/countDLM-local-results/graphmode-gate-cache-20260916-c4"
    input <- file.path(parent, "run/blinded-inputs.rds")
    if (digest::digest(file = input, algo = "sha256", serialize = FALSE) !=
        "ce452d778c9aa47fe560117feedffd2731afd95e8851ba489dd6f62e3136580a") stop("Wrong c4 blinded input.", call. = FALSE)
    parent_record <- readRDS(file.path(parent, "registration.rds"))
    for (f in names(parent_record$identity$sha256)) if (digest::digest(file = file.path(repository, f),
        algo = "sha256", serialize = FALSE) != parent_record$identity$sha256[[f]])
        stop("Frozen parent source changed: ", f, call. = FALSE)
    if (digest::digest(file = file.path(repository, "R/graphmode_joint_partition.R"), algo = "sha256",
        serialize = FALSE) != "9649dd7f34dc1dc0f6bc61bf6af19bb5c38cf6fd9cabab66384c83aedda240eb")
        stop("Audited joint kernel changed.", call. = FALSE)
    blind <- readRDS(input); graphmode4_validate_config(blind$fit)
    spec <- graphmode_joint_run_spec()
    collision <- graphmode_joint_run_seeds(dirname(destination), spec$seeds)
    identity <- graphmode_joint_run_identity(repository, files)
    record <- list(schema = graphmode_joint_run_version, repository = repository, directory = destination,
        spec = spec, identity = identity, runtime = graphmode4_pilot_runtime(),
        input = input, input_sha256 = setNames(digest::digest(file = input, algo = "sha256", serialize = FALSE), input),
        fit_signature = blind$fit$signature, thresholds = parent_record$spec$panel_spec$thresholds,
        base_policy = list(gate = graphmode_gate_refresh_policy(4L, blind$fit$core$guidance_proposal_sd),
            block = graphmode_expert_blocks_policy(42L, "uniform"), ess_scheme = "contrast", factor_cache = TRUE),
        seed_check = collision, authority = "User explicitly authorized simulation and completion monitoring on 2026-09-17",
        resume_supported = FALSE, formal_authorized = FALSE)
    record$signature <- graphmode_digest(record)
    graphmode_warmup_storage(record, TRUE)
    if (!dir.create(destination)) stop("Cannot create new registration directory.", call. = FALSE)
    graphmode_save_new(record, file.path(destination, "registration.rds"))
    graphmode_joint_run_guard(record, repository, loaded_sha)
    record
}

# Compose the audited base update and optional audited joint update. Capture
# the candidate as separate evidence; final diagnostics never use stale base Z.
graphmode_joint_run_step <- function(state, config, base_policy, joint_policy, enabled, every) {
    base <- graphmode_gate_cache_run_step(state, config, base_policy)
    scored <- NULL; joint <- NULL; final <- base$state
    if (enabled && base$state$iteration %% every == 0L) {
        step <- graphmode_dev_bind(graphmode_joint_partition_step, list(
            graphmode_joint_partition_score = function(...) {
                scored <<- graphmode_joint_partition_score(...); scored
            }))
        joint <- step(base$state, config, joint_policy); final <- joint$state
    }
    list(state = final, base_state = base$state, base_diagnostic = base$diagnostic,
        joint = joint, candidate = if (is.null(scored)) NULL else scored$candidate,
        events = graphmode_allocation_events(state$Z, final$Z, config$K))
}

graphmode_joint_run_check_step <- function(before, out, config, record, enabled) {
    base <- out$base_state; d <- out$base_diagnostic; final <- out$state
    if (base$iteration != before$iteration + 1L || final$iteration != base$iteration ||
        !identical(d$before_signature, graphmode_digest(before)) || !identical(d$after_signature, graphmode_digest(base)))
        stop("Base state handoff mismatch.", call. = FALSE)
    graphmode_gate_cache_run_gate_check(d$gate, config, record$base_policy)
    for (k in seq_len(config$K)) graphmode_blocks_run_expert_check(d$expert[[k]],
        matrix(before$theta[k, , ], ncol(config$Y), ncol(config$Fmat)),
        matrix(base$theta[k, , ], ncol(config$Y), ncol(config$Fmat)),
        config$Y[before$Z == k, , drop = FALSE], config, record$base_policy$block,
        d$block_offset, record$thresholds)
    scheduled <- enabled && base$iteration %% record$spec$joint_every == 0L
    if (scheduled != !is.null(out$joint)) stop("Joint schedule mismatch.", call. = FALSE)
    if (!scheduled && !identical(base, final)) stop("Unscheduled scientific change.", call. = FALSE)
    if (scheduled) {
        j <- out$joint
        if (!identical(final, j$state) || !identical(final$v, base$v) ||
            !identical(j$sizes, tabulate(final$Z, config$K)) ||
            !identical(j$eta, gmde_eta(final$theta, config$Fmat)) ||
            !identical(j$utilities, graphmode_gate_utilities(final$x, final$v, config)))
            stop("Joint state/derived-value handoff mismatch.", call. = FALSE)
        if (j$move == "self") {
            if (j$accepted || !is.null(out$candidate) || !identical(final, base)) stop("Invalid self transition.", call. = FALSE)
        } else {
            if (is.null(out$candidate) || !identical(j$accepted, j$log_uniform <= min(0, j$log_ratio)) ||
                !identical(final, if (j$accepted) out$candidate else base)) stop("Nonatomic joint decision.", call. = FALSE)
        }
    }
    if (!identical(out$events, graphmode_allocation_events(before$Z, final$Z, config$K)))
        stop("Final allocation events mismatch.", call. = FALSE)
    invisible(TRUE)
}

graphmode_joint_run_science <- function(record, repository, loaded_sha) {
    graphmode_joint_run_guard(record, repository, loaded_sha)
    directory <- record$directory; spec <- record$spec
    graphmode_save_new(list(signature = record$signature, started = Sys.time()), file.path(directory, "science-request.rds"))
    blind <- readRDS(record$input); fit <- blind$fit; config <- fit$core
    # Only observations/configuration are reused. New starts are drawn AFTER registration.
    starts <- lapply(1:4, function(j) graphmode_warmup_initial(config, j, spec))
    graphmode_warmup_starts_check(starts, fit, spec)
    graphmode_save_new(starts, file.path(directory, "fresh-starts.rds"))
    batch_start <- proc.time()[[3L]]; receipts <- list()
    for (job in seq_len(nrow(spec$jobs))) {
        arm <- spec$jobs$arm[job]; chain <- spec$jobs$chain[job]
        name <- sprintf("%s-chain-%02d", arm, chain); path <- file.path(directory, name)
        if (!dir.create(path)) stop("Worker destination already exists.", call. = FALSE)
        graphmode_joint_run_guard(record, repository, loaded_sha)
        graphmode_warmup_storage(record)
        seed <- unname(spec$seeds[[sprintf("%s_%02d", arm, chain)]])
        state <- starts[[chain]]$state; saved <- diagnostics <- list(); started <- proc.time()[[3L]]
        graphmode_save_new(list(signature = record$signature, seed = seed, initial = state), file.path(path, "job.rds"))
        cat(format(Sys.time()), "START", name, "600 steps\n"); flush.console()
        graphmode_pilot_seeded(seed, tryCatch({
            for (i in seq_len(spec$iterations)) {
                if (proc.time()[[3L]] - started > spec$worker_seconds ||
                    proc.time()[[3L]] - batch_start > spec$science_seconds) stop("Registered time budget exhausted.", call. = FALSE)
                before <- state
                out <- graphmode_joint_run_step(before, config, record$base_policy, spec$joint_policy,
                    arm == "joint", spec$joint_every)
                graphmode_joint_run_check_step(before, out, config, record, arm == "joint")
                state <- out$state; diagnostics[[i]] <- out
                if (i > spec$warmup) saved[[length(saved) + 1L]] <- state[c("iteration", "Z", "theta", "sigma2", "v", "pi")]
                if (i %% 100L == 0L) {
                    graphmode_warmup_storage(record)
                    graphmode_save_new(list(state = state, rng = .Random.seed, diagnostics = diagnostics,
                        saved = saved, resume_supported = FALSE), file.path(path, sprintf("checkpoint-%04d.rds", i)))
                }
                if (i %% 25L == 0L) {
                    graphmode_joint_run_json(list(phase = "science", worker = name, job = job, jobs = 8L,
                        iteration = i, iterations = spec$iterations, Kocc = length(unique(state$Z)),
                        updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")), file.path(directory, "progress.json"))
                    cat(format(Sys.time()), name, i, "/ 600 Kocc", length(unique(state$Z)), "\n"); flush.console()
                }
            }
            result <- list(schema = graphmode_joint_run_version, registration_signature = record$signature,
                arm = arm, chain = chain, seed = seed, state = state, rng = .Random.seed, draws = saved,
                diagnostics = diagnostics, resume_supported = FALSE, formal_authorized = FALSE)
            graphmode_save_new(result, file.path(path, "result.rds"))
        }, error = function(error) {
            graphmode_save_new(list(error = conditionMessage(error), state = state, diagnostics = diagnostics,
                saved = saved, resume_supported = FALSE), file.path(path, "failure.rds"))
            stop(error)
        }))
        elapsed <- proc.time()[[3L]] - started
        receipt <- list(worker = name, elapsed_seconds = elapsed,
            sha256 = digest::digest(file = file.path(path, "result.rds"), algo = "sha256", serialize = FALSE))
        if (elapsed > spec$worker_seconds) stop("Complete worker exceeded registered budget.", call. = FALSE)
        graphmode_save_new(receipt, file.path(path, "receipt.rds")); receipts[[name]] <- receipt
        cat(format(Sys.time()), "DONE", name, "seconds", elapsed, "\n"); flush.console()
    }
    graphmode_save_new(list(signature = record$signature, workers = receipts, completed = Sys.time()),
        file.path(directory, "science-complete.rds"))
}

graphmode_joint_run_diagnose <- function(record, repository, loaded_sha) {
    graphmode_joint_run_guard(record, repository, loaded_sha)
    directory <- record$directory; spec <- record$spec; science <- readRDS(file.path(directory, "science-complete.rds"))
    if (!identical(science$signature, record$signature) || length(science$workers) != 8L) stop("Incomplete science.", call. = FALSE)
    graphmode_save_new(list(signature = record$signature, started = Sys.time()), file.path(directory, "diagnostic-request.rds"))
    graphmode_joint_run_json(list(phase = "diagnosis", jobs = 8L, job = 8L, updated = format(Sys.time())), file.path(directory, "progress.json"))
    fit <- readRDS(record$input)$fit; starts <- readRDS(file.path(directory, "fresh-starts.rds"))
    arms <- list(); mechanism <- list(); costs <- list()
    for (arm in c("reference", "joint")) {
        chains <- list(); metrics <- list(); seconds <- numeric(4L)
        for (j in 1:4) {
            name <- sprintf("%s-chain-%02d", arm, j); path <- file.path(directory, name, "result.rds")
            if (digest::digest(file = path, algo = "sha256", serialize = FALSE) != science$workers[[name]]$sha256)
                stop("Completed result fingerprint changed.", call. = FALSE)
            r <- readRDS(path); previous <- starts[[j]]$state
            if (!identical(r$registration_signature, record$signature) || r$state$iteration != spec$iterations ||
                length(r$diagnostics) != spec$iterations || length(r$draws) != spec$iterations - spec$warmup)
                stop("Incomplete or mismatched worker.", call. = FALSE)
            attempts <- accepted <- splits <- merges <- self <- 0L
            for (i in seq_len(spec$iterations)) {
                d <- r$diagnostics[[i]]
                graphmode_joint_run_check_step(previous, d, fit$core, record, arm == "joint")
                if (!is.null(d$joint)) {
                    attempts <- attempts + 1L; accepted <- accepted + as.integer(d$joint$accepted)
                    splits <- splits + as.integer(d$joint$accepted && d$joint$move == "split")
                    merges <- merges + as.integer(d$joint$accepted && d$joint$move == "merge")
                    self <- self + as.integer(d$joint$move == "self")
                }
                if (i > spec$warmup && !identical(r$draws[[i - spec$warmup]], d$state[c("iteration", "Z", "theta", "sigma2", "v", "pi")]))
                    stop("Retained draw is not the final composed state.", call. = FALSE)
                previous <- d$state
            }
            if (!identical(previous, r$state)) stop("Final state mismatch.", call. = FALSE)
            movement <- lapply(r$diagnostics, function(d) list(Z = d$state$Z,
                sizes = tabulate(d$state$Z, fit$core$K), events = d$events))
            chains[[j]] <- list(complete = TRUE, config_signature = fit$signature, seed = r$seed,
                iterations = spec$iterations, warmup = spec$warmup, thin = 1L, numerical_guards_passed = TRUE,
                movement = movement, draws = r$draws, failure = "")
            seconds[j] <- science$workers[[name]]$elapsed_seconds
            metrics[[j]] <- data.frame(chain = j, attempts = attempts, accepted = accepted,
                accepted_splits = splits, accepted_merges = merges, self = self,
                final_Kocc = length(unique(r$state$Z)),
                retained_node_moves = sum(vapply(movement[(spec$warmup + 1L):spec$iterations], function(d) d$events[["node_moves"]], numeric(1))))
        }
        arms[[arm]] <- graphmode4_validity(chains, fit, unname(spec$seeds[sprintf("%s_%02d", arm, 1:4)]),
            spec$iterations, spec$warmup, 1L)
        mechanism[[arm]] <- do.call(rbind, metrics); costs[[arm]] <- seconds
    }
    report <- list(registration_signature = record$signature, arms = arms, mechanism = mechanism,
        worker_seconds = costs, formal_authorized = FALSE, convergence_certified = FALSE)
    graphmode_save_new(report, file.path(directory, "diagnostic-report.rds"))
    summaries <- lapply(names(arms), function(a) list(arm = a, valid = arms[[a]]$valid,
        scalars = as.list(table(arms[[a]]$scalars$status)), psm_failures = sum(arms[[a]]$psm_rms$rms > .05),
        mechanism = mechanism[[a]], total_worker_seconds = sum(costs[[a]])))
    graphmode_joint_run_json(list(status = "completed", registration_signature = record$signature,
        completed = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), arms = summaries,
        report_sha256 = digest::digest(file = file.path(directory, "diagnostic-report.rds"), algo = "sha256", serialize = FALSE),
        convergence_certified = FALSE, formal_authorized = FALSE), file.path(directory, "completion.json"), TRUE)
}
