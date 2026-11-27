#!/usr/bin/env Rscript
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script <- gsub("~+~", " ", script, fixed = TRUE)
case_dir <- dirname(normalizePath(script))
shared <- dirname(case_dir)
for (name in c("designs.R", "methods.R", "workflow.R")) source(file.path(shared, name))
if (file.exists(file.path(shared, "external.R"))) source(file.path(shared, "external.R"))
run_experiment(basename(case_dir), "generate", commandArgs(TRUE))
