#!/usr/bin/env Rscript
# Compatibility entry point for the original prepared-data workflow.
entry <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
entry <- gsub("~+~", " ", entry, fixed = TRUE)
source(file.path(dirname(normalizePath(entry)), "applications", "run.R"), local = TRUE)
