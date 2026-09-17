#!/usr/bin/env Rscript
# Inspection/fixed-input tests only. There is intentionally no run command.
args <- commandArgs(TRUE)
entry <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(entry) != 1L) stop("Use Rscript --vanilla.", call. = FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
if (!length(args) || identical(args, "status")) {
    cat("D-055 optional time-block expert proposals; old frozen source unchanged.\n",
        "Fixed-input check only: Rscript --vanilla scripts/graphmode-expert-blocks.R check\n",
        "No registered executor, default block size, new run or convergence claim.\n", sep = "")
} else if (identical(args, "check")) {
    source(file.path(graphmode_root, "scripts/tests/graphmode-expert-blocks-deterministic.R"))
} else stop("No run/prepare/resume command; only status or check is supported.", call. = FALSE)
