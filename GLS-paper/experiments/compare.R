#!/usr/bin/env Rscript
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE))
if (!file.exists(script)) script <- gsub("~+~", " ", script, fixed=TRUE)
shared <- dirname(normalizePath(script))
for (name in c("designs.R","methods.R","external.R","workflow.R","study.R"))
  source(file.path(shared,name))
run_study(shared,"compare",commandArgs(TRUE))
