#!/usr/bin/env Rscript
# Default action is deterministic verification, never a scientific run.
arguments <- commandArgs(trailingOnly = TRUE)
entry <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(entry) != 1L) stop("Invoke this file with Rscript --vanilla.", call. = FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
command <- if (length(arguments)) arguments[1L] else "check"
if (command == "check") {
    if (length(arguments) > 1L) stop("check takes no further arguments.", call. = FALSE)
    source(file.path(graphmode_root, "scripts", "tests", "graphmode-deterministic.R"))
} else if (command %in% c("preflight", "run")) {
    if (length(arguments) < 2L || length(arguments) > 3L ||
        (command == "preflight" && length(arguments) != 2L) ||
        (command == "run" && (length(arguments) != 3L || arguments[3L] != "--authorized")))
        stop("Usage: preflight PLAN.rds; or run PLAN.rds --authorized after separate approval.", call. = FALSE)
    for (file in c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), pattern = "^graphmode-.*[.]R$"))))
        source(file.path(graphmode_root, "R", file))
    plan <- readRDS(arguments[2L])
    if (command == "preflight") {
        checked <- graphmode_preflight(plan, graphmode_root)
        print(checked[c("ready", "problems", "signature", "note")])
        if (!checked$ready) quit(status = 2L)
    } else print(graphmode_run(plan, graphmode_root, authorized = TRUE))
} else stop("Unknown command. Available: check, preflight, run. No automatic simulation.", call. = FALSE)
