#!/usr/bin/env Rscript
# Fixed-input validation only. No prepare/run/resume or scientific executor.
args <- commandArgs(TRUE)
entry <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(entry) != 1L) stop("Use Rscript --vanilla.", call. = FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
if (!length(args) || identical(args, "status")) {
    cat("D-065 optional per-ESS guidance factor cache; frozen source unchanged.\n",
        "check: fixed-input equivalence and bounded microtiming only; no simulation.\n", sep = "")
} else if (identical(args, "check")) {
    source(file.path(graphmode_root, "scripts/tests/graphmode-gate-factor-cache-deterministic.R"))
} else stop("Only status/check are supported; no simulation starts here.", call. = FALSE)
