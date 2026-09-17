# D-066: prospective paired reference/cached execution, both contrast ESS.
# No source-time effects; no frozen driver or audited D-065 file is modified.
graphmode_gate_cache_run_version <- "graphmode-gate-cache-comparison-20260916-v1"

graphmode_gate_cache_run_spec <- function(candidate_scale) {
    s <- graphmode_gate_blocks_run_spec(candidate_scale)
    s$schema <- graphmode_gate_cache_run_version
    s$arms <- c("reference", "cached")
    names(s$block_policies) <- s$arms
    s$jobs$arm <- ifelse(s$jobs$arm == "full", "reference", "cached")
    s$seeds <- s$seeds[!grepl("^contrast_chain_", names(s$seeds))] + 100L
    s$ess_scheme <- "contrast"
    s$stream_pairing <- "same chain seed and complete start within each pair; four independent streams, not eight"
    s$paired_equivalence <- "bitwise complete scientific records and final RNG state; fail closed on mismatch"
    s$role <- "D-066 fresh-panel reference/cached contrast-ESS engineering comparison, paired sampling streams; expert block42/m4/provisional rho4; no pooled chains, automatic selection or formal release"
    s
}

graphmode_gate_cache_run_identity <- function(repository) {
    base <- graphmode_gate_blocks_run_identity(repository)
    manifest <- "docs/provenance/graphmode-gate-block-freeze-2026-09-16.sha256"
    old <- utils::read.table(file.path(repository, manifest), stringsAsFactors = FALSE)
    expected <- setNames(old[[1L]], old[[2L]])
    pins <- c(
        "R/graphmode_gate_factor_cache.R" = "c55259b982b48ee37c60ebb4be2aea2d7a41734fa8c7c6a993036884e9bcc06c",
        "scripts/graphmode-gate-factor-cache.R" = "4322cf08a8341112d78c4083ea3224d0526821647c74bd1ab53a1da108399e37",
        "scripts/tests/graphmode-gate-factor-cache-deterministic.R" = "e608368eb238cc14c4cd64df46b0e40e7a2706cd15dfe671ad3c3af0f6cb12a2",
        "R/graphmode_receipt_repair.R" = "bf42af3108b9fdcfccd33e8a04a88c52b33c08d50d12b8252446e93e75f444be")
    files <- unique(c(names(base$sha256), names(expected), manifest, names(pins),
        "docs/GRAPHMODE_GATE_FACTOR_CACHE_2026-09-16.md",
        "docs/provenance/graphmode-gate-factor-cache-2026-09-16.md",
        "scripts/graphmode-receipt-repair.R", "scripts/tests/graphmode-receipt-repair-deterministic.R",
        "R/graphmode_gate_cache_run.R", "scripts/graphmode-gate-cache-run.R",
        "scripts/tests/graphmode-gate-cache-run-deterministic.R", "docs/GRAPHMODE_GATE_CACHE_COMPARISON_2026-09-16.md"))
    sha <- setNames(vapply(file.path(repository, files), function(f)
        digest::digest(file = f, algo = "sha256", serialize = FALSE), character(1)), files)
    git <- function(args) {
        x <- system2("git", c("-C", shQuote(repository), args), stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(x, "status"))) stop("Cannot inspect cache comparison source identity.", call. = FALSE)
        x
    }
    base$sha256 <- sha
    base$audited_unchanged <- base$audited_unchanged && length(expected) == 92L &&
        identical(unname(sha[names(expected)]), unname(expected)) &&
        identical(unname(sha[names(pins)]), unname(pins))
    base$committed <- base$committed &&
        !length(git(c("status", "--porcelain", "--untracked-files=all", "--", shQuote(files)))) &&
        setequal(files, git(c("ls-files", "--", shQuote(files))))
    base
}

graphmode_gate_cache_run_record <- function(repository, directory, identity, runtime, phase_A) {
    graphmode_validation_A_shape(phase_A)
    x <- list(schema = graphmode_gate_cache_run_version, spec = graphmode_refresh_run_spec(phase_A$candidate_scale),
        repository = repository, directory = directory, output_dir = file.path(directory, "run"),
        identity = identity, runtime = runtime, phase_A = phase_A,
        authority = "D-066 preparation only; approve exact paired design/budget before registration, then separately authorize run",
        formal_authorized = FALSE, resume_supported = FALSE, auto_continue = FALSE)
    x$signature <- graphmode_digest(x); x
}

graphmode_gate_cache_run_job <- function(record, chain, arm) {
    graphmode_refresh_run_validate(record)
    chain <- gmde_scalar_integer(chain, "chain", 1L, 4L)
    if (!is.character(arm) || length(arm) != 1L || is.na(arm) || !arm %in% record$spec$arms)
        stop("Unregistered cache comparison arm.", call. = FALSE)
    name <- sprintf("%s-chain-%02d", arm, chain)
    x <- list(schema = graphmode_gate_cache_run_version, record = record, chain = chain, arm = arm,
        seed = unname(record$spec$seeds[[sprintf("chain_%02d", chain)]]), name = name,
        directory = file.path(record$output_dir, name),
        policy = list(gate = graphmode_gate_refresh_policy(record$spec$gate_inner_steps, record$spec$candidate_scale),
            block = record$spec$block_policies[[arm]], ess_scheme = record$spec$ess_scheme,
            factor_cache = identical(arm, "cached")))
    x$signature <- graphmode_digest(x); x
}

graphmode_gate_cache_run_step <- function(state, config, policy) {
    if (!identical(policy$ess_scheme, "contrast") || !is.logical(policy$factor_cache) ||
        length(policy$factor_cache) != 1L || is.na(policy$factor_cache))
        stop("Explicit contrast/cache policy required.", call. = FALSE)
    ess <- graphmode_gate_blocks_policy(config, policy$ess_scheme)
    sweep <- if (policy$factor_cache) graphmode_gate_factor_cache_sweep else graphmode_gate_blocks_sweep
    step <- graphmode_dev_bind(graphmode_blocks_run_step, list(
        graphmode_expert_blocks_sweep = function(state, config, gate_policy, expert_policy, cache_ffbs)
            sweep(state, config, gate_policy, expert_policy, ess, cache_ffbs)))
    step(state, config, policy)
}

graphmode_gate_cache_run_gate_check <- function(g, config, policy) {
    if (!identical(policy$ess_scheme, "contrast") || !is.logical(policy$factor_cache) ||
        length(policy$factor_cache) != 1L || is.na(policy$factor_cache))
        stop("Invalid cache evidence policy.", call. = FALSE)
    if (policy$factor_cache) {
        nb <- length(graphmode_gate_blocks_policy(config, "contrast")$blocks)
        expected <- if (nb == 1L) list(enabled = FALSE, requests = 0L, builds = 0L, hits = 0L) else
            list(enabled = TRUE, requests = 1L + 3L * nb, builds = 1L, hits = 3L * nb)
        if (!identical(g$schema, graphmode_gate_factor_cache_version) ||
            !identical(g$factor_cache, rep(list(expected), policy$gate$inner_steps)))
            stop("Invalid per-scan cache schema/counts.", call. = FALSE)
        g$schema <- graphmode_gate_blocks_version; g$factor_cache <- NULL
    } else if ("factor_cache" %in% names(g)) {
        stop("Reference arm cannot claim cached evidence.", call. = FALSE)
    }
    graphmode_gate_blocks_run_gate_check(g, config, policy)
    invisible(TRUE)
}

graphmode_gate_cache_run_result_check <- function(job, result, blind) {
    ds <- result$checkpoint$diagnostics
    if (!is.list(ds) || length(ds) != job$record$spec$iterations) stop("Incomplete cache diagnostics.", call. = FALSE)
    for (d in ds) graphmode_gate_cache_run_gate_check(d$gate, blind$fit$core, job$policy)
    view <- result
    view$checkpoint$diagnostics <- lapply(ds, function(d) { d$gate$schema <- graphmode_gate_refresh_version; d })
    check <- graphmode_dev_bind(graphmode_blocks_run_result_check,
        list(graphmode_blocks_run_version = graphmode_gate_cache_run_version))
    chain <- check(job, view, blind)
    chain$movement <- ds; chain$evidence_identity <- graphmode_digest(result)
    chain
}

# Remove only measured timing and explicitly different policy/cache envelopes.
# Every other diagnostic field, including numerical guards and MH decisions,
# must be bitwise identical. Original persisted records are never rewritten.
graphmode_gate_cache_run_scientific_record <- function(d) {
    d$policy <- NULL; d$transition_seconds <- NULL; d$total_seconds <- NULL
    d$gate$schema <- NULL; d$gate$factor_cache <- NULL
    d$gate$records <- lapply(d$gate$records, function(g) { g$ess_seconds <- NULL; g$guidance_seconds <- NULL; g })
    d$expert <- lapply(d$expert, function(e) {
        e$seconds <- NULL
        e$blocks <- lapply(e$blocks, function(b) { b$kernel_seconds <- NULL; b$observation_seconds <- NULL; b })
        e
    })
    d
}

graphmode_gate_cache_run_pairs <- function(science, spec) {
    rows <- lapply(1:4, function(j) {
        at <- function(arm) {
            k <- which(vapply(science$jobs, function(x) identical(x$arm, arm) && identical(x$chain, j), logical(1)))
            if (length(k) != 1L) stop("Incomplete paired jobs.", call. = FALSE)
            k
        }
        a <- at("reference"); b <- at("cached")
        ca <- science$evidence[[a]]$result$checkpoint; cb <- science$evidence[[b]]$result$checkpoint
        if (!identical(science$jobs[[a]]$seed, science$jobs[[b]]$seed) ||
            !identical(ca$state, cb$state) || !identical(ca$saved, cb$saved) ||
            is.null(ca$rng_state) || !identical(ca$rng_state, cb$rng_state) ||
            length(ca$diagnostics) != spec$iterations || length(cb$diagnostics) != spec$iterations)
            stop("Paired scientific state/draw/RNG mismatch; no acceptance.", call. = FALSE)
        for (i in seq_len(spec$iterations)) if (!identical(
            graphmode_gate_cache_run_scientific_record(ca$diagnostics[[i]]),
            graphmode_gate_cache_run_scientific_record(cb$diagnostics[[i]])))
            stop("Paired scientific record mismatch at chain ", j, " iteration ", i, "; no acceptance.", call. = FALSE)
        data.frame(chain = j, reference_job = science$jobs[[a]]$signature,
            cached_job = science$jobs[[b]]$signature, checked_outer_steps = spec$iterations,
            scientific_records_identical = TRUE, retained_draws_identical = TRUE, final_rng_identical = TRUE)
    })
    do.call(rbind, rows)
}

graphmode_gate_cache_run_science_evidence <- function(record, repository, receipt = TRUE) {
    base <- graphmode_blocks_run_science_evidence; environment(base) <- environment()
    x <- base(record, repository, FALSE)
    x$paired_equivalence <- graphmode_gate_cache_run_pairs(x, record$spec)
    x$receipt$paired_equivalence_signature <- graphmode_digest(x$paired_equivalence)
    if (receipt && (file.exists(file.path(record$directory, "science-acceptance-pending.rds")) ||
        !identical(readRDS(file.path(record$directory, "science-acceptance.rds")), x$receipt)))
        stop("Paired science acceptance missing/changed.", call. = FALSE)
    x
}

graphmode_gate_cache_run_mechanism <- function(ds, spec) {
    out <- graphmode_gate_blocks_run_mechanism(ds, spec)
    enabled <- ds[[1L]]$policy$factor_cache
    totals <- function(records) {
        scans <- unlist(lapply(records, function(d) d$gate$factor_cache), recursive = FALSE)
        list(scans = length(scans), requests = sum(vapply(scans, `[[`, integer(1), "requests")),
            builds = sum(vapply(scans, `[[`, integer(1), "builds")), hits = sum(vapply(scans, `[[`, integer(1), "hits")))
    }
    out$factor_cache <- list(enabled = enabled,
        all_steps = if (enabled) totals(ds) else NULL,
        retained = if (enabled) totals(ds[seq.int(spec$warmup + 1L, spec$iterations)]) else NULL,
        note = "Recorded cache counts only; reference factor constructions are not instrumented. Complete worker time remains the efficiency denominator.")
    out
}

graphmode_gate_cache_run_pair_cost <- function(science) {
    do.call(rbind, lapply(1:4, function(j) {
        seconds <- function(arm) {
            k <- which(vapply(science$jobs, function(x) identical(x$arm, arm) && identical(x$chain, j), logical(1)))
            if (length(k) != 1L) stop("Incomplete paired cost.", call. = FALSE)
            value <- science$evidence[[k]]$execution$elapsed_seconds
            if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value <= 0)
                stop("Invalid paired complete-worker cost.", call. = FALSE)
            value
        }
        a <- seconds("reference"); b <- seconds("cached")
        data.frame(chain = j, reference_worker_seconds = a, cached_worker_seconds = b,
            reference_over_cached = a / b, cached_minus_reference_seconds = b - a)
    }))
}

graphmode_gate_cache_run_diagnose <- function(record, repository) {
    science <- graphmode_refresh_run_science_evidence(record, repository)
    driver <- graphmode_blocks_run_diagnose; environment(driver) <- environment()
    driver <- graphmode_dev_bind(driver, list(graphmode_refresh_run_science_evidence = function(...) science))
    out <- driver(record, repository)
    out$paired_equivalence <- science$paired_equivalence
    out$paired_worker_cost <- graphmode_gate_cache_run_pair_cost(science)
    out$note <- "Four paired streams on one fresh panel, not eight independent chains; identical scientific records required. Report complete-worker costs and unchanged validity, no pooling, automatic choice or formal release."
    out
}

graphmode_gate_cache_run_report_check <- function(report, record, science) {
    check <- graphmode_blocks_run_report_check; environment(check) <- environment()
    check(report, record, science)
    if (!identical(report$paired_equivalence, graphmode_gate_cache_run_pairs(science, record$spec)) ||
        !identical(report$paired_worker_cost, graphmode_gate_cache_run_pair_cost(science)))
        stop("Paired equivalence or complete-worker cost report changed.", call. = FALSE)
    for (field in c("scalars", "psm_rms", "valid", "failures"))
        if (!identical(report$arms$reference$validity[[field]], report$arms$cached$validity[[field]]))
            stop("Identical paired traces have different statistical summaries.", call. = FALSE)
    invisible(TRUE)
}

graphmode_gate_cache_run_child <- function(command, path, repository, timeout, log_file = NULL) {
    if (!command %in% c("batch", "worker", "diagnostic-worker")) stop("Invalid cache comparison child.", call. = FALSE)
    launch <- graphmode_dev_bind(graphmode_dev_run_child, list(file.path = function(...) {
        pieces <- lapply(list(...), function(x) if (identical(x, "scripts/graphmode-dev-run.R")) "scripts/graphmode-gate-cache-run.R" else x)
        do.call(base::file.path, pieces)
    }))
    launch(command, path, repository, timeout, log_file)
}

graphmode_gate_cache_run_context <- function() {
    ctx <- graphmode_gate_blocks_run_context()
    ctx$graphmode_blocks_run_version <- graphmode_gate_cache_run_version
    ctx$graphmode_refresh_run_version <- graphmode_gate_cache_run_version
    original <- environment(graphmode_gate_cache_run_context)
    for (name in c("spec", "identity", "record", "job", "step", "result_check", "child", "science_evidence", "diagnose", "report_check")) {
        value <- get(paste0("graphmode_gate_cache_run_", name), original); environment(value) <- ctx
        assign(paste0("graphmode_refresh_run_", name), value, ctx)
    }
    ctx$graphmode_blocks_run_mechanism <- graphmode_gate_cache_run_mechanism
    # Reuse only D-064's exact, tested empty-failure expression correction;
    # its c3-specific repair/output workflow is never called by this executor.
    corrected <- graphmode_receipt_repair_checker(graphmode_refresh_run_report_check)
    original_base <- graphmode_blocks_run_base
    ctx$graphmode_blocks_run_base <- function(name) if (identical(name, "report_check")) corrected else original_base(name)
    ctx
}
