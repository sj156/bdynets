#!/usr/bin/env Rscript
# D-060 candidate only; no prepare/run/resume or package/default change.
args <- commandArgs(TRUE)
entry <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(entry) != 1L) stop("Use Rscript --vanilla.", call. = FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
if (!length(args) || identical(args, "status")) {
    cat("D-060 optional whitened-contrast ESS blocks; frozen source unchanged.\n",
        "check: fixed-input tests only. No registered executor or launch authority.\n", sep = "")
} else if (identical(args, "check")) {
    source(file.path(graphmode_root, "scripts/tests/graphmode-gate-blocks-deterministic.R"))
} else stop("Only status/check are supported; no simulation starts here.", call. = FALSE)
