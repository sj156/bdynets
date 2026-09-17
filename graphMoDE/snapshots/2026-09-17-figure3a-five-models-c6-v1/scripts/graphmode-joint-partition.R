#!/usr/bin/env Rscript
# Candidate inspection only. No prepare/run/resume commands.
args <- commandArgs(TRUE)
entry <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(entry) != 1L) stop("Use Rscript --vanilla.", call. = FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
if (!length(args) || identical(args, "status")) {
    cat("Joint partition/path/gate MH candidate; no executor integration.\n",
        "check: deterministic inputs only; no simulation or real PG.\n", sep = "")
} else if (identical(args, "check")) {
    source(file.path(graphmode_root, "scripts/tests/graphmode-joint-partition-deterministic.R"))
} else stop("Only status/check are supported; no simulation starts here.", call. = FALSE)
