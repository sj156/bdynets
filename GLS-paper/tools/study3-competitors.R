#!/usr/bin/env Rscript
# Study 3, competitors. Actions: plan (default), data, run, report.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!file.exists(script)) script <- gsub("~+~", " ", script, fixed = TRUE)
script <- normalizePath(script, mustWork = TRUE)
source(file.path(dirname(script), "..", "experiments", "study.R"))
study_arm_cli(3L, "competitors", script, commandArgs(TRUE))
