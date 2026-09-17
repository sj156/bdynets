# Fixed arrays and mocked subprocess outputs only; no scientific RNG calls.
if (!exists("graphmode_root", inherits = FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent = globalenv())
    for (f in c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$"))))
        sys.source(file.path(graphmode_root, "R", f), envir = kernel)
    test_environment <- environment(); parent.env(test_environment) <- kernel
    rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    rng_kind <- RNGkind(); checks <- 0L
    assert <- function(x) if (!isTRUE(x)) stop("r4 pilot assertion: ", paste(deparse(substitute(x)), collapse = " "), call. = FALSE)
    fails <- function(code, pattern) {
        e <- tryCatch({force(code); NULL}, error = identity)
        assert(inherits(e, "error") && grepl(pattern, conditionMessage(e)))
    }
    test <- function(name, code) {force(code); checks <<- checks + 1L; cat(sprintf("ok %02d - %s\n", checks, name))}
    patched <- function(bindings, code) {
        originals <- mget(names(bindings), envir = kernel)
        on.exit(list2env(originals, envir = kernel), add = TRUE)
        list2env(bindings, envir = kernel); force(code)
    }
    fixture <- tempfile("graphmode-r4-pilot-fixed-"); dir.create(fixture)
    fixture <- normalizePath(fixture)
    # Exactly this test-owned directory; contains only tiny deterministic fixtures.
    on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
    spec <- graphmode4_pilot_spec()
    source_snapshot <- graphmode4_pilot_identity(graphmode_root)
    runtime <- graphmode4_pilot_runtime()
    record <- function(name) {
        directory <- file.path(fixture, name); dir.create(directory)
        r <- graphmode4_pilot_registration(graphmode_root, directory, source_snapshot, runtime)
        graphmode_save_new(r, file.path(directory, "pilot-registration.rds")); r
    }
    fixed <- list(innovations = array(sin(seq_len(5 * 168 * 3)) / 10, c(5L, 168L, 3L)),
        Y = matrix(rep(20:30, length.out = 121 * 168), 121L, 168L))
    panel <- graphmode4_pilot_panel(spec, fixed$innovations, 5:1, 121:1, fixed$Y)
    starts <- lapply(spec$start_occupancy, function(k) rep(seq_len(k), length.out = 121L))
    generated <- list(panel = panel, starts = starts)
    test("scope, budgets, all seeds and short-test limits are explicit", {
        assert(spec$geometry == "intertwined-spiral" && spec$n == 121L && spec$Kfit == 10L)
        assert(spec$iterations == 400L && spec$warmup == 200L && spec$workers == 1L)
        assert(!anyDuplicated(spec$seeds) && all(spec$seeds > 0))
        assert(spec$total_budget_seconds >= 4 * spec$chain_budget_seconds + 2 * spec$phase_budget_seconds)
        assert(spec$thresholds$pairwise_psm_rms_max == .05 && spec$thresholds$bulk_ess_min == 400)
        assert(grepl("NOT calibrated", spec$calibration_id))
    })
    test("registration mutation rejected and preparation is not launch authorization", {
        r <- record("signed"); graphmode4_pilot_validate(r)
        for (field in c("spec", "runtime", "source_identity", "output_dir", "formal_authorized", "signature")) {
            changed <- r; changed[[field]] <- "changed"
            fails(graphmode4_pilot_validate(changed), "changed")
        }
        fails(graphmode4_pilot_run(r, graphmode_root), "Later explicit")
        assert(!dir.exists(r$output_dir))
    })
    test("fixed panel follows r4 geometry and keeps truth outside worker configuration", {
        assert(panel$geometry$geometry == "intertwined-spiral" && panel$fit$protocol == graphmode4_protocol)
        assert(identical(panel$fit$core$Y, fixed$Y[121:1, ]))
        assert(max(abs(rowSums(panel$truth$lambda) - 4200)) < 1e-9)
        assert(!any(c("truth", "lambda", "Ztrue") %in% names(panel$fit$core)))
        fails(graphmode4_pilot_panel(spec, fixed$innovations, 5:1, 121:1, fixed$Y + .1), "counts")
    })
    test("four equal-length plans bind independent seeds and declared truth-blind starts", {
        r <- record("plans"); plans <- graphmode4_pilot_plans(r, panel, starts)
        assert(length(plans) == 4L && all(vapply(plans, function(p) p$iterations == 400L, logical(1))))
        for (j in 1:4) {
            graphmode_revalidate_plan(plans[[j]])
            assert(plans[[j]]$seed == spec$seeds[[sprintf("chain_%02d", j)]])
            assert(identical(plans[[j]]$config, panel$fit$core))
            assert(length(unique(plans[[j]]$initial_state$Z)) == spec$start_occupancy[j])
            assert(all(plans[[j]]$initial_state$x == 0) && all(plans[[j]]$initial_state$v == 0))
        }
        fails(graphmode4_pilot_plans(r, panel, rep(list(rep(1L, 121)), 4)), "occupancy")
    })
    test("source identity protects original nine audited files and includes new launcher/tests", {
        assert(source_snapshot$audited_core_unchanged && source_snapshot$r4$requirements_match)
        assert(all(c("scripts/graphmode-r4-pilot.R", "scripts/tests/graphmode-r4-pilot-deterministic.R") %in% names(source_snapshot$extra_sha256)))
        assert(graphmode_pilot_loaded_source_ok(graphmode_root, source_snapshot$r4))
        patched(list(graphmode_pg = function(...) stop("changed")), {
            assert(!graphmode_pilot_loaded_source_ok(graphmode_root, source_snapshot$r4))
        })
        assert("posterior" %in% names(runtime$package_files) && length(runtime$package_files$BayesLogit) > 1L)
    })
    test("preflight is read-only and source/runtime/output guards fail closed", {
        r <- record("preflight")
        a <- graphmode4_pilot_preflight(r, graphmode_root)
        assert(!a$run_authorized && !dir.exists(r$output_dir))
        changed <- r; changed$runtime$R <- "different"
        assert(!graphmode4_pilot_preflight(changed, graphmode_root)$ready)
        patched(list(graphmode4_pilot_guard = function(...) TRUE), {
            assert(graphmode4_pilot_preflight(r, graphmode_root)$ready)
            dir.create(r$output_dir)
            assert(!graphmode4_pilot_preflight(r, graphmode_root)$ready)
        })
    })
    test("prepare writes only deterministic registration, never data or chains", {
        good <- source_snapshot; good$committed <- TRUE
        rt <- runtime; rt$threads[] <- "1"; rt$locale <- "C"
        patched(list(graphmode4_pilot_identity = function(...) good, graphmode4_pilot_runtime = function() rt,
            graphmode_pilot_loaded_source_ok = function(...) TRUE), {
            checked <- graphmode4_pilot_prepare(graphmode_root, file.path(fixture, "prepared"))
            assert(checked$ready && !checked$run_authorized)
            assert(identical(list.files(file.path(fixture, "prepared")), "pilot-registration.rds"))
            fails(graphmode4_pilot_prepare(graphmode_root, file.path(fixture, "prepared")), "nonexistent")
        })
    })
    # Small loop counts for mocked orchestration, not a modified scientific run.
    tiny <- spec; tiny$iterations <- 8L; tiny$warmup <- 4L; tiny$checkpoint_every <- 2L
    fake_pg <- function(b, z) {
        m <- graphmode_pilot_pg_moments(b[1], z[1]); n <- length(b)
        rep(c(-1, 1), length.out = n) * sqrt(m[["variance"]] * (n-1)/n) + m[["mean"]]
    }
    fake_result <- function(plan) {
        theta <- plan$initial_state$theta
        draws <- lapply(5:8, function(i) list(iteration = i, theta = theta,
            Z = plan$initial_state$Z, v = plan$initial_state$v, sigma2 = NULL))
        d <- list(events = c(node_moves = 0, pair_changes = 0), expert = rep(list(list(empty = FALSE,
            factor_residual = 1e-14, root_residual = 1e-13, root_reciprocal_condition = .1,
            movement = 1, seconds = .001, accepted = TRUE, log_acceptance = 0)), 10))
        diagnostics <- lapply(1:8, function(i) {x <- d; x$iteration <- i; x})
        summary <- list(similarity = diag(121))
        list(checkpoint = list(status = "completed-not-convergence-certified", plan_signature = plan$signature,
            source_identity = plan$source_identity, state = list(iteration = 8L), diagnostics = diagnostics,
            saved = draws, elapsed_seconds = .1), summary = summary, summary_signature = graphmode_digest(summary))
    }
    simulated <- function(name, child_hook = NULL, pg_hook = fake_pg, before_input = NULL) {
        calls <- character(); saved_record <- NULL
        patched(list(graphmode4_pilot_spec = function() tiny,
            graphmode4_pilot_guard = function(...) TRUE,
            graphmode_pg = function(b, z) {
                assert(file.exists(file.path(saved_record$output_dir, "launch-registration.rds")))
                calls <<- c(calls, "PG"); pg_hook(b, z)
            },
            graphmode4_pilot_generate = function(...) {
                if (!is.null(before_input)) before_input(saved_record)
                calls <<- c(calls, "generate"); generated
            },
            graphmode4_pilot_child = function(command, envelope_path, repository, timeout_seconds) {
                e <- readRDS(envelope_path)
                assert(all(file.exists(file.path(e$record$output_dir, sprintf("PLAN-%02d.rds", 1:4)))))
                assert(!any(c("truth", "generated") %in% names(e)))
                calls <<- c(calls, paste(command, e$chain))
                if (!is.null(child_hook)) {
                    special <- child_hook(command, e)
                    if (!is.null(special)) return(special)
                }
                if (command == "chain-run") {
                    dir.create(e$plan$output_dir)
                    graphmode_save_new(fake_result(e$plan), file.path(e$plan$output_dir, "result.rds"))
                }
                list(status = 0L, output = "fixed child; NOT a stochastic result")
            }), {
            saved_record <- record(name)
            output <- tryCatch(graphmode4_pilot_run(saved_record, graphmode_root, authorized = TRUE), error = identity)
        })
        list(record = saved_record, calls = calls, output = output)
    }
    test("mocked flow registers before first draw, saves all plans first and runs exactly four sequential children", {
        out <- simulated("orchestration")
        assert(!inherits(out$output, "error") && out$output$status == "completed-with-diagnostic-flags")
        expected <- c(rep("PG", 5), "generate", paste("chain-preflight", 1:4), paste("chain-run", 1:4))
        assert(identical(out$calls, expected))
        report <- readRDS(file.path(out$record$output_dir, "pilot-report.rds"))
        assert(!report$valid && !report$formal_authorized && !report$paper_use && nrow(report$psm_rms) == 6L)
        assert(file.exists(file.path(out$record$output_dir, "completed.rds")))
    })
    test("failed PG samples retained and no data or chains generated", {
        out <- simulated("pg-failure", pg_hook = function(b, z) fake_pg(b, z) * 2)
        assert(inherits(out$output, "error") && !"generate" %in% out$calls)
        assert(file.exists(file.path(out$record$output_dir, "pg-screen.rds")))
        assert(file.exists(file.path(out$record$output_dir, "pilot-failure.rds")))
    })
    test("child timeout is retained, no subsequent chain or completion marker", {
        out <- simulated("timeout", child_hook = function(command, e)
            if (command == "chain-run" && e$chain == 2L) list(status = 124L, output = "fixed timeout") else NULL)
        assert(inherits(out$output, "error") && !"chain-run 3" %in% out$calls)
        assert(file.exists(file.path(out$record$output_dir, "chain-audit-01.rds")))
        assert(!file.exists(file.path(out$record$output_dir, "completed.rds")))
        failure <- readRDS(file.path(out$record$output_dir, "pilot-failure.rds"))
        assert(grepl("124", failure$error) && failure$stage == "chain-2")
    })
    test("preflight failure stops before first chain", {
        out <- simulated("child-preflight", child_hook = function(command, e)
            if (command == "chain-preflight" && e$chain == 3L) list(status = 2L, output = "fixed source failure") else NULL)
        assert(inherits(out$output, "error") && !any(startsWith(out$calls, "chain-run")))
    })
    test("global budget prevents the first random call without extending the registration", {
        saved <- tiny
        tiny$total_budget_seconds <- 0
        out <- simulated("budget")
        tiny <- saved
        assert(inherits(out$output, "error") && !length(out$calls))
        failure <- readRDS(file.path(out$record$output_dir, "pilot-failure.rds"))
        assert(grepl("budget exhausted", failure$error))
    })
    test("removed active tree is not recreated and original persistence error survives", {
        out <- simulated("lost-tree", before_input = function(r) {
            assert(file.rename(r$output_dir, paste0(r$output_dir, "-retained-test-fixture")))
        })
        assert(inherits(out$output, "error") && !dir.exists(out$record$output_dir))
        assert(dir.exists(paste0(out$record$output_dir, "-retained-test-fixture")))
        assert(!any(startsWith(out$calls, "chain-run")))
    })
    test("adapter rejects missing numerical roots, result identity and incomplete diagnostics", {
        patched(list(graphmode4_pilot_spec = function() tiny), {
            r <- record("adapter"); plan <- graphmode4_pilot_plans(r, panel, starts)[[1L]]
            result <- fake_result(plan)
            a <- graphmode4_pilot_chain_record(result, plan, panel$fit, spec$thresholds)
            assert(a$numerical_guards_passed && length(a$draws) == 4L)
            bad <- result; bad$checkpoint$diagnostics[[1]]$expert[[1]]$root_residual <- .001
            fails(graphmode4_pilot_chain_record(bad, plan, panel$fit, spec$thresholds), "protection")
            bad <- result; bad$checkpoint$diagnostics[[1]]$expert[[1]]$root_reciprocal_condition <- 0
            fails(graphmode4_pilot_chain_record(bad, plan, panel$fit, spec$thresholds), "conditioning")
            bad <- result; bad$checkpoint$plan_signature <- "changed"
            fails(graphmode4_pilot_chain_record(bad, plan, panel$fit, spec$thresholds), "changed")
            bad <- result; bad$checkpoint$elapsed_seconds <- 601
            fails(graphmode4_pilot_chain_record(bad, plan, panel$fit, spec$thresholds), "over-budget")
        })
    })
    test("fresh worker preflight reads an actual fixed envelope without random calls", {
        # This becomes a real source-guard/child-preflight check after the local
        # freeze. It never calls chain-run and never creates a chain directory.
        if (source_snapshot$committed && all(runtime$threads == "1") && runtime$locale == "C") {
            r <- record("real-worker-preflight")
            dir.create(r$output_dir)
            graphmode_save_new(list(record = r, authorization = "deterministic preflight fixture only"),
                file.path(r$output_dir, "launch-registration.rds"))
            plans <- graphmode4_pilot_plans(r, panel, starts)
            for (j in 1:4) {
                e <- list(record = r, chain = j, fit = panel$fit, plan = plans[[j]])
                e$signature <- graphmode_digest(e)
                path <- file.path(r$output_dir, sprintf("PLAN-%02d.rds", j))
                graphmode_save_new(e, path)
                result <- graphmode4_pilot_child("chain-preflight", path, graphmode_root, 30)
                if (result$status != 0L) stop(paste(result$output, collapse = "\n"), call. = FALSE)
                assert(!dir.exists(plans[[j]]$output_dir))
            }
        } else cat("  Real worker preflight deferred until committed single-thread freeze.\n")
    })
    test("no real PG, response, chain or RNG-state change in these tests", {
        now <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
        assert(identical(rng, now) && identical(rng_kind, RNGkind()))
    })
    cat(sprintf("PASS: %d fixed-input/mocked-I/O r4 pilot groups; no scientific random run.\n", checks))
})
