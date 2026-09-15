#!/usr/bin/env Rscript
# r4 development entry: fixed-input verification only, no stochastic launcher.
arguments <- commandArgs(trailingOnly = TRUE)
entry <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(entry) != 1L) stop("Invoke with Rscript --vanilla.", call. = FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
command <- if (length(arguments)) arguments[1L] else "check"
if (length(arguments) > 1L || !command %in% c("check", "status"))
    stop("Only check/status are available. r4 runs need calibration, source freeze and separate authorization.", call. = FALSE)
if (command == "check") {
    source(file.path(graphmode_root, "scripts/tests/graphmode-r4-deterministic.R"))
} else {
    cat("r4 deterministic implementation layer; audited r2/v2 kernel unchanged.\n",
        "No stochastic execution is exposed by this entry.\n",
        "Pending: calibrated values; four-chain/checkpoint executor; full-prefix refit executor;\n",
        "STGNN/PNAR exact adapters; stage-specific source/environment/seed/budget registration.\n", sep = "")
}
