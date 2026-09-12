# D-037/D-038/D-039: tuning over the unchanged audited mathematical kernel.
# No sampling on source(); no new default rho, posterior target or run authority.
graphmode4_tuning_version <- "graphmode-r4-controlled-development-20260912-v3"

graphmode4_guidance_policy <- function(stop_at, batch_size, target, gain,
                                     offset, exponent, lower, upper) {
    stop_at <- gmde_scalar_integer(stop_at, "adaptation stop", 1L)
    batch_size <- gmde_scalar_integer(batch_size, "adaptation batch", 1L)
    for (name in c("target", "gain", "offset", "exponent", "lower", "upper"))
        graphmode_positive(get(name), name)
    if (target >= 1 || exponent <= .5 || exponent > 1 || lower >= upper ||
        stop_at %% batch_size != 0L)
        stop("Invalid bounded warmup policy.", call. = FALSE)
    list(stop_at = stop_at, batch_size = batch_size, target = target, gain = gain,
         offset = offset, exponent = exponent, lower = lower, upper = upper)
}

graphmode4_guidance_control <- function(config, warmup, policy = NULL) {
    graphmode_revalidate_config(config)
    warmup <- gmde_scalar_integer(warmup, "warmup", 0L)
    free <- if (!config$adaptive || config$guidance %in% c("none", "forced")) 0L else
        if (config$guidance == "shared") 1L else config$K
    if (!is.null(policy)) {
        if (!is.list(policy) || !identical(policy, do.call(graphmode4_guidance_policy, policy)) ||
            !free || policy$stop_at >= warmup || config$guidance_proposal_sd < policy$lower ||
            config$guidance_proposal_sd > policy$upper)
            stop("Adaptation needs free guidance, valid bounds and a frozen settling period before retention.", call. = FALSE)
    }
    list(sd = if (free) config$guidance_proposal_sd else NULL, free = free,
        policy = policy, iteration = 0L, batch_accepted = 0L, batch_proposals = 0L,
        updates = 0L, frozen = is.null(policy), last_rate = NA_real_)
}

graphmode4_guidance_observe <- function(control, accepted, iteration) {
    if (!isTRUE(iteration == control$iteration + 1L) ||
        !is.logical(accepted) || length(accepted) != control$free || anyNA(accepted))
        stop("Missing/nonsequential guidance adaptation evidence.", call. = FALSE)
    control$iteration <- as.integer(iteration)
    policy <- control$policy
    if (is.null(policy) || iteration > policy$stop_at) return(control)
    control$batch_accepted <- control$batch_accepted + sum(accepted)
    control$batch_proposals <- control$batch_proposals + length(accepted)
    if (iteration %% policy$batch_size == 0L) {
        control$updates <- control$updates + 1L
        control$last_rate <- control$batch_accepted / control$batch_proposals
        learning_rate <- policy$gain / (policy$offset + control$updates)^policy$exponent
        log_sd <- log(control$sd) + learning_rate * (control$last_rate - policy$target)
        control$sd <- exp(max(log(policy$lower), min(log(policy$upper), log_sd)))
        control$batch_accepted <- control$batch_proposals <- 0L
    }
    control$frozen <- iteration >= policy$stop_at
    control
}

graphmode4_controlled_sweep <- function(state, config, control) {
    if (!isTRUE(state$iteration == control$iteration)) stop("State/control iteration mismatch.", call. = FALSE)
    effective <- config
    if (control$free) effective$guidance_proposal_sd <- control$sd
    # The shared scalar scale is fixed throughout this sweep's coordinate MH
    # updates. Only the next warmup sweep may use the observed acceptance.
    out <- graphmode_sweep(state, effective)
    next_control <- graphmode4_guidance_observe(control, out$guidance_accept, out$state$iteration)
    out$tuning <- list(sd_used = control$sd, sd_next = next_control$sd,
        frozen_used = control$frozen, frozen_next = next_control$frozen,
        batch_rate = next_control$last_rate, updates = next_control$updates,
        rho = config$rho)
    list(out = out, control = next_control)
}

graphmode4_movement_report <- function(diagnostics, warmup) {
    warmup <- gmde_scalar_integer(warmup, "warmup", 0L)
    if (!is.list(diagnostics) || length(diagnostics) <= warmup)
        stop("Need complete post-warmup movement records.", call. = FALSE)
    K <- length(diagnostics[[1L]]$expert)
    for (i in seq_along(diagnostics)) {
        d <- diagnostics[[i]]
        if (!isTRUE(d$iteration == i) || length(d$expert) != K || !K)
            stop("Incomplete movement sequence.", call. = FALSE)
        for (e in d$expert) {
            if (!is.logical(e$empty) || length(e$empty) != 1L || is.na(e$empty) ||
                length(e$movement) != 1L || !is.finite(e$movement) || e$movement < 0 ||
                length(e$seconds) != 1L || !is.finite(e$seconds) || e$seconds < 0 ||
                (!e$empty && (!is.logical(e$accepted) || length(e$accepted) != 1L || is.na(e$accepted))) ||
                (!e$empty && !e$accepted && e$movement != 0))
                stop("Invalid Poisson movement evidence.", call. = FALSE)
        }
    }
    scored <- diagnostics[seq.int(warmup + 1L, length(diagnostics))]
    longest <- function(x) {r <- rle(x); if (any(r$values)) max(r$lengths[r$values]) else 0L}
    experts <- do.call(rbind, lapply(seq_len(K), function(k) {
        e <- lapply(scored, function(d) d$expert[[k]])
        occupied <- !vapply(e, `[[`, logical(1), "empty")
        accepted <- vapply(e, function(x) isTRUE(x$accepted) && !x$empty, logical(1))
        movement <- vapply(e, `[[`, numeric(1), "movement")
        seconds <- vapply(e, `[[`, numeric(1), "seconds")
        data.frame(expert = k, occupied_updates = sum(occupied), accepted = sum(accepted),
            acceptance = if (any(occupied)) mean(accepted[occupied]) else NA_real_,
            longest_rejection_streak = longest(occupied & !accepted),
            accepted_movement = sum(movement[occupied]), state_seconds = sum(seconds),
            occupied_state_seconds = sum(seconds[occupied]), empty_state_seconds = sum(seconds[!occupied]))
    }))
    # Expert index is local to a chain, not an across-chain component identity.
    list(experts = experts, scored_sweeps = length(scored),
        partition_moving_sweeps = sum(vapply(scored, function(d)
            d$events[["pair_changes"]] > 0, logical(1))),
        guidance_acceptance = if (length(unlist(lapply(scored, `[[`, "guidance_accept"))))
            mean(unlist(lapply(scored, `[[`, "guidance_accept"))) else NA_real_,
        note = "Exploratory movement evidence; not a replacement for r4 validity or permission to rerun.")
}

graphmode4_tuning_identity <- function(repository) {
    base <- graphmode4_pilot_identity(repository)
    files <- c("scripts/graphmode-r4-tuning.R", "scripts/tests/graphmode-r4-tuning-deterministic.R",
        "docs/GRAPHMODE_R4_TUNING_2026-09-12.md")
    sha <- setNames(vapply(file.path(repository, files), function(f)
        digest::digest(file = f, algo = "sha256", serialize = FALSE), character(1)), files)
    git <- function(args) {
        x <- system2("git", c("-C", shQuote(repository), args), stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(x, "status"))) stop("Cannot inspect tuning source identity.", call. = FALSE)
        x
    }
    changed <- git(c("status", "--porcelain", "--untracked-files=all", "--", shQuote(files)))
    tracked <- git(c("ls-files", "--", shQuote(files)))
    list(base = base, extra_sha256 = sha,
         committed = base$committed && !length(changed) && setequal(files, tracked))
}

# Resolve existing ancestors before signing, without creating output folders.
# Missing ordinary suffixes support compiling a rho grid before its parent is
# created. A missing dot/symlink component cannot be guessed or resolved away.
graphmode4_output_target <- function(path) {
    graphmode4_text(path, "controlled output path")
    path <- sub("/+$", "", path.expand(path))
    if (!startsWith(path, "/") || basename(path) %in% c(".", ".."))
        stop("Use an absolute output path with a new directory name.", call. = FALSE)
    suffix <- basename(path); parent <- dirname(path)
    while (!dir.exists(parent)) {
        link <- Sys.readlink(parent)
        if (file.exists(parent) || (!is.na(link) && nzchar(link)) || basename(parent) %in% c(".", ".."))
            stop("Output parent is not a resolvable directory.", call. = FALSE)
        suffix <- file.path(basename(parent), suffix)
        parent <- dirname(parent)
    }
    file.path(normalizePath(parent, mustWork = TRUE), suffix)
}

graphmode4_controlled_plan <- function(core_plan, fit, policy, identity, runtime) {
    graphmode_revalidate_plan(core_plan); graphmode4_validate_config(fit)
    if (!identical(core_plan$config, fit$core) || core_plan$config$family != "poisson" ||
        !identical(core_plan$source_identity, identity$base$r4$core))
        stop("Controlled plan must bind the same Poisson fit and source.", call. = FALSE)
    graphmode4_guidance_control(core_plan$config, core_plan$warmup, policy)
    plan <- list(schema = graphmode4_tuning_version, core_plan = core_plan, fit = fit,
        policy = policy, identity = identity, runtime = runtime,
        output_dir = graphmode4_output_target(core_plan$output_dir),
        formal_authorized = FALSE, resume_supported = FALSE)
    plan$signature <- graphmode_digest(plan)
    plan
}

graphmode4_controlled_validate <- function(plan) {
    if (!is.list(plan) || !identical(plan$schema, graphmode4_tuning_version))
        stop("Not a controlled-development plan; historical registrations cannot be resumed.", call. = FALSE)
    rebuilt <- do.call(graphmode4_controlled_plan, plan[names(formals(graphmode4_controlled_plan))])
    if (!identical(plan, rebuilt)) stop("Controlled plan was changed.", call. = FALSE)
    invisible(plan)
}

graphmode4_tuning_guard <- function(plan, repository) {
    graphmode4_controlled_validate(plan)
    identity <- graphmode4_tuning_identity(repository)
    if (!identical(identity, plan$identity) || !isTRUE(identity$committed) ||
        !isTRUE(identity$base$audited_core_unchanged) || !isTRUE(identity$base$r4$requirements_match) ||
        !graphmode_pilot_loaded_source_ok(repository, identity$base$r4))
        stop("Controlled source is unfrozen, modified or not loaded from registered files.", call. = FALSE)
    runtime <- graphmode4_pilot_runtime()
    if (!identical(runtime, plan$runtime) || any(runtime$threads != "1") || runtime$locale != "C")
        stop("Controlled runtime changed or is not single-thread C locale.", call. = FALSE)
    invisible(TRUE)
}

graphmode4_controlled_preflight <- function(plan, repository) {
    graphmode4_tuning_guard(plan, repository)
    checked <- graphmode_preflight(plan$core_plan, repository)
    if (!identical(checked$output_dir, plan$output_dir)) {
        checked$problems <- c(checked$problems, "Controlled output target changed after signing.")
        checked$ready <- FALSE
    }
    checked
}

graphmode4_controlled_run <- function(plan, repository, authorized = FALSE) {
    if (!identical(authorized, TRUE)) stop("Separate authorization for this new controlled run is required.", call. = FALSE)
    checked <- graphmode4_controlled_preflight(plan, repository)
    if (!checked$ready) stop(paste(checked$problems, collapse = "\n"), call. = FALSE)
    core <- plan$core_plan; destination <- checked$output_dir
    if (!dir.create(destination)) stop("Cannot create exclusive controlled output directory.", call. = FALSE)
    old_kind <- RNGkind()
    old_rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    on.exit({do.call(RNGkind, as.list(old_kind))
        if (is.null(old_rng)) {
            if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
        } else assign(".Random.seed", old_rng, .GlobalEnv)
    }, add = TRUE)
    options_before <- options(warn = 2); on.exit(options(options_before), add = TRUE)
    state <- core$initial_state; saved <- diagnostics <- list()
    control <- graphmode4_guidance_control(core$config, core$warmup, plan$policy)
    started <- proc.time()[[3L]]
    tree <- function() {
        registration <- file.path(destination, "registration.rds")
        if (!dir.exists(destination) || !identical(normalizePath(destination), destination) ||
            !file.exists(registration) || !identical(readRDS(registration)$plan, plan))
            stop("Controlled output tree/registration disappeared; never recreate it.", call. = FALSE)
    }
    elapsed <- function() proc.time()[[3L]] - started
    budget <- function() if (elapsed() >= core$budget_seconds)
        stop("Controlled sweep-boundary budget exhausted; no automatic extension.", call. = FALSE)
    checkpoint <- function(status, error = NULL) list(schema = graphmode4_tuning_version,
        plan_signature = plan$signature, core_plan_signature = core$signature,
        source_identity = plan$identity, runtime = plan$runtime, state = state,
        rng = get(".Random.seed", .GlobalEnv), saved = saved, diagnostics = diagnostics,
        control = control, status = status, error = error, elapsed_seconds = elapsed(),
        resume_supported = FALSE, formal_authorized = FALSE)
    save <- function(x, name) {tree(); graphmode_save_new(x, file.path(destination, name))}
    do.call(RNGkind, as.list(core$rng_kind)); set.seed(core$seed)
    tryCatch({
        # Persist the full tuning schedule before the first scientific draw.
        graphmode_save_new(list(plan = plan, preflight = checked, authorized = TRUE),
            file.path(destination, "registration.rds"))
        save(checkpoint("initialized"), "checkpoint-000000000.rds")
        for (i in seq_len(core$iterations)) {
            tree(); budget()
            step <- graphmode4_controlled_sweep(state, core$config, control)
            out <- step$out; state <- out$state; control <- step$control
            diagnostics[[i]] <- list(iteration = i, sizes = out$sizes, sorted_sizes = out$sorted_sizes,
                Kocc = out$Kocc, fragmentation = out$fragmentation, events = out$events,
                potts_gross = out$potts_gross, ess_evaluations = out$ess_evaluations,
                guidance_accept = out$guidance_accept, tuning = out$tuning,
                expert = lapply(out$expert_updates, function(x) x[c("empty", "accepted", "log_acceptance",
                    "movement", "seconds", "factor_residual", "root_residual", "root_reciprocal_condition")]))
            if (i > core$warmup) {
                if (!control$frozen || !out$tuning$frozen_used)
                    stop("Adaptation reached retained sampling.", call. = FALSE)
                if ((i - core$warmup) %% core$thin == 0L) saved[[length(saved) + 1L]] <-
                    list(iteration = i, Z = state$Z, theta = state$theta, sigma2 = state$sigma2,
                         v = state$v, pi = state$pi, mean_profile = exp(out$eta))
            }
            if (i %% core$checkpoint_every == 0L) {
                graphmode4_tuning_guard(plan, repository)
                save(checkpoint("in-progress"), sprintf("checkpoint-%09d.rds", i))
            }
        }
        graphmode4_tuning_guard(plan, repository); tree(); budget()
        cp <- checkpoint("completed-not-convergence-certified")
        # Reuse the existing numerical-evidence auditor on an internal view,
        # never persist a controlled checkpoint under the old checkpoint schema.
        view <- cp; view$plan_signature <- core$signature; view$source_identity <- core$source_identity
        record <- graphmode4_pilot_chain_record(list(checkpoint = view, summary = list(),
            summary_signature = graphmode_digest(list())), core, plan$fit, graphmode4_pilot_spec()$thresholds)
        record$tuning_plan_signature <- plan$signature
        record$frozen_guidance_sd <- control$sd
        movement <- graphmode4_movement_report(diagnostics, core$warmup)
        budget()
        result <- list(checkpoint = cp, chain_record = record, movement = movement,
            formal_authorized = FALSE, convergence_certified = FALSE)
        save(result, "result.rds")
        invisible(list(status = cp$status, retained_draws = length(saved), frozen_guidance_sd = control$sd,
            movement = movement, elapsed_seconds = elapsed(), formal_authorized = FALSE))
    }, error = function(e) {
        failure <- checkpoint("failed-retained-do-not-resume", conditionMessage(e))
        tryCatch(save(failure, "failure.rds"), error = function(other)
            message("Original error: ", conditionMessage(e), "; failure persistence: ", conditionMessage(other)))
        stop(conditionMessage(e), call. = FALSE)
    })
}

# Read existing evidence only. A saved worker result is not a successful launch.
# Even an unreadable/NULL failure file is retained as evidence of failure.
graphmode4_controlled_evidence <- function(plan) {
    graphmode4_controlled_validate(plan)
    destination <- plan$output_dir
    errors <- character()
    read <- function(name) tryCatch(suppressWarnings(readRDS(file.path(destination, name))), error = function(e) {
        errors <<- c(errors, paste(name, conditionMessage(e))); NULL
    })
    out <- list(registration = read("registration.rds"), result = read("result.rds"),
        execution = read("execution.rds"), acceptance = read("acceptance.rds"), failure = NULL)
    if (file.exists(file.path(destination, "failure.rds")))
        out$failure <- list(present = TRUE, record = read("failure.rds"))
    out$read_errors <- errors
    out
}

graphmode4_controlled_result_check <- function(plan, result) {
    cp <- result$checkpoint; core <- plan$core_plan
    if (!is.list(cp) || !identical(cp$schema, graphmode4_tuning_version) ||
        !identical(cp$plan_signature, plan$signature) || !is.null(cp$error) ||
        !identical(cp$source_identity, plan$identity) || !identical(cp$runtime, plan$runtime))
        stop("Incomplete/changed/failed controlled result.", call. = FALSE)
    view <- cp; view$plan_signature <- core$signature; view$source_identity <- core$source_identity
    graphmode4_pilot_chain_record(list(checkpoint = view, summary = list(),
        summary_signature = graphmode_digest(list())), core, plan$fit, graphmode4_pilot_spec()$thresholds)
    graphmode4_movement_report(cp$diagnostics, core$warmup)
    invisible(TRUE)
}

graphmode4_controlled_evidence_check <- function(plan, evidence, require_acceptance = TRUE) {
    if (!is.list(evidence) || !identical(evidence$read_errors, character()) ||
        !is.null(evidence$failure) || !identical(evidence$registration$plan, plan))
        stop("Missing/failed/mismatched controlled launch evidence.", call. = FALSE)
    execution <- evidence$execution
    status <- execution$status
    if (!is.list(execution) || !is.numeric(status) || length(status) != 1L ||
        is.na(status) || !is.finite(status) || status != 0 ||
        !identical(execution$signature, plan$signature) ||
        !identical(execution$hard_budget_seconds, plan$core_plan$budget_seconds))
        stop("Controlled child exit evidence missing, changed or unsuccessful.", call. = FALSE)
    graphmode4_controlled_result_check(plan, evidence$result)
    if (require_acceptance) {
        a <- evidence$acceptance
        if (!is.list(a) || !identical(a$schema, graphmode4_tuning_version) ||
            !identical(a$accepted, TRUE) || !identical(a$postflight_passed, TRUE) ||
            !identical(a$plan_signature, plan$signature) ||
            !identical(a$execution_signature, graphmode_digest(execution)) ||
            !identical(a$result_signature, graphmode_digest(evidence$result)))
            stop("Final controlled acceptance missing or not bound to this execution/result.", call. = FALSE)
    }
    invisible(TRUE)
}

graphmode4_controlled_launch <- function(plan, plan_path, repository, authorized = FALSE) {
    if (!identical(authorized, TRUE)) stop("Separate authorization for this launch is required.", call. = FALSE)
    if (!identical(readRDS(plan_path), plan)) stop("Persisted controlled plan changed.", call. = FALSE)
    checked <- graphmode4_controlled_preflight(plan, repository)
    if (!checked$ready) stop(paste(checked$problems, collapse = "\n"), call. = FALSE)
    output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
        c("--vanilla", shQuote(file.path(repository, "scripts/graphmode-r4-tuning.R")),
          "worker-run", shQuote(normalizePath(plan_path)), "--authorized"),
        stdout = TRUE, stderr = TRUE, timeout = ceiling(plan$core_plan$budget_seconds)))
    status <- attr(output, "status"); if (is.null(status)) status <- 0L
    destination <- checked$output_dir
    registration <- file.path(destination, "registration.rds")
    if (file.exists(registration) && identical(readRDS(registration)$plan, plan))
        graphmode_save_new(list(status = status, output = output, signature = plan$signature,
            hard_budget_seconds = plan$core_plan$budget_seconds, resume_supported = FALSE),
            file.path(destination, "execution.rds"))
    if (!is.numeric(status) || length(status) != 1L || is.na(status) || !is.finite(status) || status != 0L)
        stop("Controlled child failed/timed out (", paste(status, collapse = ","),
        "); retained checkpoints are not resumable.\n", paste(output, collapse = "\n"), call. = FALSE)
    graphmode4_tuning_guard(plan, repository)
    if (!identical(normalizePath(destination, mustWork = TRUE), plan$output_dir) ||
        !identical(readRDS(plan_path), plan)) stop("Controlled launch destination/plan changed.", call. = FALSE)
    # Final receipt is written LAST: exit 0 alone cannot bypass postflight,
    # a worker failure, changed registration or incomplete numerical evidence.
    evidence <- list(registration = readRDS(registration),
        execution = readRDS(file.path(destination, "execution.rds")),
        result = readRDS(file.path(destination, "result.rds")), read_errors = character(),
        failure = if (file.exists(file.path(destination, "failure.rds"))) TRUE else NULL)
    graphmode4_controlled_evidence_check(plan, evidence, require_acceptance = FALSE)
    graphmode_save_new(list(schema = graphmode4_tuning_version, accepted = TRUE,
        postflight_passed = TRUE, plan_signature = plan$signature,
        execution_signature = graphmode_digest(evidence$execution),
        result_signature = graphmode_digest(evidence$result)), file.path(destination, "acceptance.rds"))
    invisible(list(status = "completed-not-convergence-certified", output = output,
        output_dir = destination, formal_authorized = FALSE))
}

# Compile a one-method development comparison, never select a common rho for
# all five methods. The template must bind independently registered NEW data.
# Same start and seed within a rho triplet/grid; no inherited chain state.
graphmode4_rho_screen_plan <- function(template, starts, seeds, rhos, output_parent,
                                     max_movement_share) {
    graphmode4_controlled_validate(template)
    if (!is.null(template$policy)) stop("Hold guidance tuning fixed in isolated rho branches.", call. = FALSE)
    if (!is.numeric(rhos) || length(rhos) < 2L || anyNA(rhos) ||
        any(!is.finite(rhos) | rhos < 1 | rhos != floor(rhos) | rhos > .Machine$integer.max) ||
        anyDuplicated(rhos) || !identical(rhos, sort(rhos))) stop("Register an increasing exact-PG integer rho grid.", call. = FALSE)
    if (!is.list(starts) || length(starts) < 2L || length(seeds) != length(starts) ||
        !is.numeric(seeds) || anyNA(seeds) || any(!is.finite(seeds) | seeds < 1 |
        seeds != floor(seeds) | seeds > .Machine$integer.max) || anyDuplicated(seeds))
        stop("Register multiple starts and distinct start-stratum chain seeds.", call. = FALSE)
    graphmode4_text(output_parent, "new external rho output parent")
    if (!grepl("^/", output_parent)) stop("Use an absolute rho output parent.", call. = FALSE)
    graphmode_positive(max_movement_share, "maximum scored movement share")
    if (max_movement_share > 1) stop("Invalid movement-share bound.", call. = FALSE)
    core <- template$core_plan
    if (core$thin != 1L || (core$iterations - core$warmup) %% 2L != 0L ||
        core$iterations - core$warmup < 4L) stop("Use two equal scored halves and thin one.", call. = FALSE)
    for (s in starts) graphmode_revalidate_state(s, core$config)
    if (anyDuplicated(vapply(starts, function(s) graphmode_digest(outer(s$Z, s$Z, "==")), character(1))))
        stop("Rho starts must have different label-invariant partitions.", call. = FALSE)
    jobs <- list()
    for (h in seq_along(starts)) for (rho in rhos) {
        args <- core$config[names(formals(graphmode_config))]; args$rho <- rho
        config <- do.call(graphmode_config, args)
        fit <- template$fit; fit$core <- config; fit$signature <- NULL; fit$signature <- graphmode_digest(fit)
        args <- core[names(formals(graphmode_run_plan))]
        id <- paste0("start-", h, "-rho-", rho)
        args$config <- config; args$initial_state <- starts[[h]]; args$seed <- seeds[h]
        args$output_dir <- file.path(output_parent, id); args$registration_id <- id
        args$provenance$calibration_decision <- "D-037 isolated fixed-rho development screen; common rho remains unresolved"
        args$provenance$scientific_role <- "Same new data/initial state within candidates; no truth scoring or evaluation use"
        job <- graphmode4_controlled_plan(do.call(graphmode_run_plan, args), fit, NULL,
            template$identity, template$runtime)
        jobs[[id]] <- list(start = h, rho = rho, plan = job)
    }
    screen <- list(schema = "graphmode-r4-rho-screen-20260912-v3", jobs = jobs,
        rhos = rhos, starts = length(starts), max_movement_share = max_movement_share,
        rule = "maximize accepted occupied information movement / all expert state-update seconds (including empty prior updates); exact ties choose smaller rho; require start and half agreement and final successful launch evidence",
        common_rho_selected = FALSE, formal_authorized = FALSE)
    screen$signature <- graphmode_digest(screen)
    screen
}

graphmode4_rho_screen_reduce <- function(screen, results) {
    unsigned <- screen; unsigned$signature <- NULL
    if (!identical(screen$schema, "graphmode-r4-rho-screen-20260912-v3") ||
        !identical(screen$signature, graphmode_digest(unsigned))) stop("Rho screen changed or obsolete.", call. = FALSE)
    out <- list(resolved = FALSE, candidate_rho = NA_real_, common_rho_selected = FALSE,
        failures = character(), scores = NULL, formal_authorized = FALSE)
    if (!is.list(results) || anyDuplicated(names(results)) ||
        !setequal(names(results), names(screen$jobs))) {
        out$failures <- "Missing, duplicate or extra rho branches; do not drop failures."; return(out)
    }
    rows <- list()
    for (id in names(screen$jobs)) {
        job <- screen$jobs[[id]]; evidence <- results[[id]]; core <- job$plan$core_plan
        audit <- tryCatch({
            graphmode4_controlled_validate(job$plan)
            graphmode4_controlled_evidence_check(job$plan, evidence)
            cp <- evidence$result$checkpoint
            if (!is.null(cp$control$policy) || !isTRUE(cp$control$frozen))
                stop("Unfrozen rho branch.")
            fixed_sd <- graphmode4_guidance_control(core$config, core$warmup)$sd
            for (d in cp$diagnostics) if (!isTRUE(d$tuning$rho == job$rho) ||
                !identical(d$tuning$sd_used, fixed_sd) || !isTRUE(d$tuning$frozen_used))
                stop("Rho/guidance changed within a branch.")
            TRUE
        }, error = function(e) conditionMessage(e))
        if (!identical(audit, TRUE)) {out$failures <- c(out$failures, paste(id, audit)); next}
        scored <- cp$diagnostics[seq.int(core$warmup + 1L, core$iterations)]
        movement <- vapply(scored, function(d) sum(vapply(Filter(function(e) !e$empty, d$expert),
            `[[`, numeric(1), "movement")), numeric(1))
        # Movement excludes empty experts; cost includes their full prior draws.
        seconds <- vapply(scored, function(d) sum(vapply(d$expert,
            `[[`, numeric(1), "seconds")), numeric(1))
        occupied_seconds <- vapply(scored, function(d) sum(vapply(Filter(function(e) !e$empty, d$expert),
            `[[`, numeric(1), "seconds")), numeric(1))
        empty_seconds <- vapply(scored, function(d) sum(vapply(Filter(function(e) e$empty, d$expert),
            `[[`, numeric(1), "seconds")), numeric(1))
        if (any(!is.finite(c(movement, seconds, sum(movement), sum(seconds)))) ||
            sum(movement) <= 0 || sum(seconds) <= 0 || max(movement) / sum(movement) > screen$max_movement_share) {
            out$failures <- c(out$failures, paste(id, "no movement/time or single-sweep domination")); next
        }
        half <- length(scored) / 2L
        for (block in 1:2) {
            index <- (block - 1L) * half + seq_len(half)
            if (sum(seconds[index]) <= 0 || sum(movement[index]) <= 0)
                out$failures <- c(out$failures, paste(id, "empty scored half"))
            rows[[length(rows) + 1L]] <- data.frame(start = job$start, rho = job$rho, half = block,
                movement = sum(movement[index]), seconds = sum(seconds[index]),
                occupied_seconds = sum(occupied_seconds[index]), empty_seconds = sum(empty_seconds[index]))
        }
    }
    if (length(rows)) out$scores <- do.call(rbind, rows)
    if (length(out$failures)) return(out)
    choose <- function(rows) {
        efficiency <- vapply(screen$rhos, function(rho) {
            x <- rows[rows$rho == rho, ]; sum(x$movement) / sum(x$seconds)
        }, numeric(1))
        screen$rhos[which.max(efficiency)]
    }
    candidate <- choose(out$scores)
    strata <- c(vapply(seq_len(screen$starts), function(h) choose(out$scores[out$scores$start == h, ]), numeric(1)),
        vapply(1:2, function(b) choose(out$scores[out$scores$half == b, ]), numeric(1)),
        unlist(lapply(seq_len(screen$starts), function(h) vapply(1:2, function(b)
            choose(out$scores[out$scores$start == h & out$scores$half == b, ]), numeric(1)))))
    if (any(strata != candidate)) {out$failures <- "Rho choice disagrees across starts/scored halves."; return(out)}
    out$resolved <- TRUE; out$candidate_rho <- candidate
    out$note <- "Single-method development candidate only; independent multi-method calibration and four-chain validation still required."
    out
}
