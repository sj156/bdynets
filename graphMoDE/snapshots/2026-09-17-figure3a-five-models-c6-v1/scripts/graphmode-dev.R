#!/usr/bin/env Rscript
# D-043 development entry: never launches an experiment.
args <- commandArgs(trailingOnly = TRUE)
entry <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(entry) != 1L) stop("Use Rscript --vanilla.", call. = FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
command <- if (length(args)) args[1L] else "status"
if (command == "status" && length(args) <= 1L) {
    cat("D-043 opt-in FFBS cache and passive proposal/allocation observations.\n",
        "check | inspect EXISTING_RHO_DIRECTORY\n",
        "No new run entry or package interface. Audit and fresh registration precede any future run.\n", sep = "")
} else if (command == "check" && length(args) == 1L) {
    source(file.path(graphmode_root, "scripts/tests/graphmode-dev-deterministic.R"))
} else if (command == "inspect" && length(args) == 2L) {
    for (f in c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$")), "graphmode_dev.R"))
        source(file.path(graphmode_root, "R", f))
    options(warn = 2, width = 200)
    print(graphmode_dev_inspect(args[2L]))
} else stop("Only status/check/read-only inspect; no stochastic run is exposed.", call. = FALSE)
