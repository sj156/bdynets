#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
entry <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(entry) != 1L) stop("Use Rscript --vanilla.", call. = FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
command <- if (length(args)) args[1L] else "help"
if (command == "help") {
    cat("check | prepare NEW_EXTERNAL_DIR | preflight REGISTRATION.rds | run REGISTRATION.rds --authorized\n")
} else if (command == "check" && length(args) == 1L) {
    source(file.path(graphmode_root, "scripts/tests/graphmode-r4-rho-experiment-deterministic.R"))
} else {
    if (!(command %in% c("prepare", "preflight") && length(args) == 2L ||
          command %in% c("run", "batch-worker") && length(args) == 3L && args[3L] == "--authorized"))
        stop("Invalid arguments; nothing launched.", call. = FALSE)
    for (f in c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$"))))
        source(file.path(graphmode_root, "R", f))
    options(warn = 2)
    if (command == "prepare") print(graphmode4_rho_experiment_prepare(graphmode_root, args[2L])) else {
        record <- readRDS(args[2L])
        if (command == "preflight") {
            graphmode4_rho_experiment_guard(record, graphmode_root); print(list(ready = TRUE, signature = record$signature))
        } else if (command == "batch-worker") {
            graphmode4_rho_experiment_batch(record, graphmode_root, authorized = TRUE)
        } else {
            graphmode4_rho_experiment_guard(record, graphmode_root)
            log_path <- file.path(record$directory, "terminal.log")
            if (file.exists(log_path)) stop("Launch already attempted; no retry.", call. = FALSE)
            status <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                c("--vanilla", shQuote(file.path(graphmode_root, "scripts/graphmode-r4-rho-experiment.R")),
                  "batch-worker", shQuote(normalizePath(args[2L])), "--authorized"),
                stdout = log_path, stderr = log_path, timeout = record$spec$total_budget_seconds))
            graphmode_save_new(list(status = status, registration_signature = record$signature,
                hard_budget_seconds = record$spec$total_budget_seconds), file.path(record$directory, "batch-execution.rds"))
            cat(readLines(log_path, warn = FALSE), sep = "\n")
            if (status != 0L) quit(status = 2L)
        }
    }
}
