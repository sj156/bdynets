#!/usr/bin/env Rscript
# No simulation entry is provided by this implementation-stage CLI.
args <- commandArgs(trailingOnly = TRUE)
entry <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)), mustWork = TRUE)
graphmode_root <- dirname(dirname(entry))
if (!length(args) || identical(args, "status") || identical(args, "help")) {
    cat("D-050 optional fixed-count gate refresh: implementation stage only.\n",
        "Use check for fixed-input tests. No run, resume, tuning, registration or convergence release.\n", sep = "")
} else if (identical(args, "check")) {
    sys.source(file.path(graphmode_root, "scripts/tests/graphmode-gate-refresh-deterministic.R"), envir = globalenv())
} else stop("Only status/help/check are available; new simulation requires separate registration and authorization.", call. = FALSE)
