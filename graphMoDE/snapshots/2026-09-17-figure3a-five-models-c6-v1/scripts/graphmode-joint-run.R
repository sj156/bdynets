#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
entry <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(entry) != 1L) stop("Use Rscript --vanilla.")
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
command <- if (length(args)) args[1L] else "status"
if (command == "status" && length(args) <= 1L) {
    cat("C5: fixed c4 blinded observations; fresh 4 paired starts, independent arm seeds.\n",
        "reference versus joint-every10; 600/300 per chain; 8 workers; 7200s+600s caps.\n",
        "check | prepare NEW_EXTERNAL_DIR --authorized | preflight REGISTRATION.rds\n",
        "science/diagnose REGISTRATION.rds --authorized; no retry or resume.\n", sep = "")
} else {
    if (!(command == "check" && length(args) == 1L || command == "preflight" && length(args) == 2L ||
        command %in% c("prepare", "science", "diagnose") && length(args) == 3L && args[3L] == "--authorized"))
        stop("Invalid command or missing run authorization.", call. = FALSE)
    for (package in c("digest", "posterior", "BayesLogit", "jsonlite"))
        if (!requireNamespace(package, quietly = TRUE)) stop("Required installed package missing: ", package)
    runtime <- file.path("R", c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$")),
        "graphmode_dev.R", "graphmode_dev_run.R", "graphmode_warmup.R", "graphmode_validation.R",
        "graphmode_gate_refresh.R", "graphmode_refresh_run.R", "graphmode_expert_blocks.R", "graphmode_blocks_run.R",
        "graphmode_gate_blocks.R", "graphmode_gate_blocks_run.R", "graphmode_gate_factor_cache.R",
        "graphmode_receipt_repair.R", "graphmode_gate_cache_run.R", "graphmode_joint_partition.R", "graphmode_joint_run.R"))
    hashes <- function() setNames(vapply(file.path(graphmode_root, runtime), function(f)
        digest::digest(file = f, algo = "sha256", serialize = FALSE), character(1)), runtime)
    loaded_sha <- hashes()
    for (f in runtime) source(file.path(graphmode_root, f))
    stopifnot(identical(loaded_sha, hashes()))
    options(warn = 2, width = 160)
    files <- c(runtime, "scripts/graphmode-joint-run.R", "scripts/graphmode-joint-controller.py",
        "scripts/tests/graphmode-joint-run-deterministic.R", "scripts/graphmode-joint-partition.R",
        "scripts/tests/graphmode-joint-partition-deterministic.R")
    if (command == "check") source(file.path(graphmode_root, "scripts/tests/graphmode-joint-run-deterministic.R"))
    else if (command == "prepare") {
        record <- graphmode_joint_run_prepare(graphmode_root, args[2L], files, loaded_sha)
        print(list(signature = record$signature, commit = record$identity$commit,
            seed_registrations_checked = length(record$seed_check$registrations), ready = TRUE))
    } else {
        record <- readRDS(args[2L])
        if (command == "preflight") {
            graphmode_joint_run_guard(record, graphmode_root, loaded_sha)
            stopifnot(!file.exists(file.path(record$directory, "science-request.rds")))
            print(list(ready = TRUE, signature = record$signature))
        } else tryCatch({
            if (command == "science") graphmode_joint_run_science(record, graphmode_root, loaded_sha)
            else graphmode_joint_run_diagnose(record, graphmode_root, loaded_sha)
        }, error = function(error) {
            graphmode_joint_run_json(list(status = "failed", phase = command, error = conditionMessage(error),
                time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")), file.path(record$directory, "failure.json"), TRUE)
            stop(error)
        })
    }
}
