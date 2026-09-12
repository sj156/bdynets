# Fixed inputs and mocked dispatch only; no scientific draws or child launches.
if (!exists("graphmode_root", inherits = FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent = globalenv())
    for (f in c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$"))))
        sys.source(file.path(graphmode_root, "R", f), envir = kernel)
    testenv <- environment(); parent.env(testenv) <- kernel
    rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    kind <- RNGkind(); checks <- 0L
    test <- function(name, x) {stopifnot(isTRUE(x)); checks <<- checks + 1L; cat("ok", checks, "-", name, "\n")}
    fails <- function(x) inherits(tryCatch({force(x); NULL}, error = identity), "error")
    s <- graphmode4_rho_experiment_spec()
    test("fresh unique generation/start/chain seeds", !anyDuplicated(s$seeds) &&
        !length(intersect(s$seeds, s$panel_spec$seeds)))
    test("approved scope and fixed scale", identical(s$rhos, c(1L, 2L, 4L, 8L)) &&
        s$iterations == 400L && s$warmup == 200L && s$workers == 1L &&
        s$total_budget_seconds == 3600 && is.null(s$policy) && s$panel_spec$gate$guidance_proposal_sd == .25)
    test("budget reserves full dispatch and finalization", graphmode4_rho_experiment_time_left(s, 2939) &&
        !graphmode4_rho_experiment_time_left(s, 2940) && !graphmode4_rho_experiment_time_left(s, Inf))
    test("run rejects missing authority before any work", fails(graphmode4_rho_experiment_batch(NULL, NULL)))
    test("generation rejects changed spec before draws", fails(graphmode4_rho_experiment_generate(list())))
    fixture <- tempfile("graphmode-rho-fixed-"); dir.create(fixture); fixture <- normalizePath(fixture)
    on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
    id <- graphmode4_rho_experiment_identity(graphmode_root); runtime <- graphmode4_pilot_runtime()
    record <- graphmode4_rho_experiment_record(graphmode_root, fixture, id, runtime)
    copy <- record; copy$signature <- NULL
    test("outer signature binds full registration", identical(record$signature, graphmode_digest(copy)))
    generated <- list(panel = graphmode4_pilot_panel(s$panel_spec, array(0, c(5L, 168L, 3L)),
        1:5, 1:121, matrix(25, 121L, 168L)),
        starts = list(rep(1:3, length.out = 121), rep(1:7, length.out = 121)))
    screen <- graphmode4_rho_experiment_screen(record, generated)
    test("eight plans in fixed order", identical(names(screen$jobs),
        c(paste0("start-1-rho-", s$rhos), paste0("start-2-rho-", s$rhos))))
    test("same panel/start/seed within each candidate stratum", all(vapply(1:2, function(h) {
        jobs <- screen$jobs[(h - 1L) * 4L + 1:4]
        length(unique(vapply(jobs, function(j) graphmode_digest(j$plan$core_plan$initial_state), character(1)))) == 1L &&
            length(unique(vapply(jobs, function(j) j$plan$core_plan$seed, numeric(1)))) == 1L &&
            all(vapply(jobs, function(j) identical(j$plan$core_plan$config$Y, generated$panel$fit$core$Y), logical(1)))
    }, logical(1))))
    test("unrun branches cannot be accepted", !graphmode4_rho_screen_reduce(screen,
        lapply(screen$jobs, function(j) graphmode4_controlled_evidence(j$plan)))$resolved)
    bad <- generated; bad$starts[[2L]] <- rep(1L, 121)
    test("registered occupancies cannot drift", fails(graphmode4_rho_experiment_screen(record, bad)))
    # Exercise the REAL batch loop/save/collector with a fixed generator and
    # failed mock dispatcher. All signed plans must exist before mock dispatch.
    original_guard <- kernel$graphmode4_rho_experiment_guard
    kernel$graphmode4_rho_experiment_guard <- function(...) invisible(TRUE)
    kernel$graphmode4_rho_experiment_generate <- function(spec) generated
    calls <- 0L
    kernel$graphmode4_rho_experiment_child <- function(command, path, repository, timeout_seconds) {
        stopifnot(command == "run", timeout_seconds == 630,
            length(list.files(record$output_dir, "^PLAN-")) == 8L,
            identical(readRDS(path), screen$jobs[[1L]]$plan))
        calls <<- calls + 1L
        list(status = 124L, output = "fixed timeout; no child started")
    }
    report <- graphmode4_rho_experiment_batch(record, graphmode_root, authorized = TRUE)
    test("first failure retained; no retry; no rho selected", calls == 1L && !report$rho$resolved &&
        !is.null(report$stop_reason) && file.exists(file.path(record$output_dir, "DISPATCH-start-1-rho-1.rds")) &&
        file.exists(file.path(record$output_dir, "report.rds")))
    test("exclusive run tree prevents reuse", fails(suppressWarnings(
        graphmode4_rho_experiment_batch(record, graphmode_root, authorized = TRUE))))
    kernel$graphmode4_rho_experiment_guard <- original_guard
    test("no test draws or RNG changes", identical(RNGkind(), kind) && identical(rng,
        if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL))
    cat(checks, "deterministic orchestration checks passed; no simulation.\n")
})
