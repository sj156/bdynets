#!/usr/bin/env Rscript
# Compatibility entry point for the original directed-grid study.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE))
script <- gsub("~+~", " ", script, fixed = TRUE)
launcher <- file.path(dirname(normalizePath(script)), "experiments", "legacy-grid", "run_simulation.R")
source(launcher, local=TRUE)
legacy_grid_main(launcher)
