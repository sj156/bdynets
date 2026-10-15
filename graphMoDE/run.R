#!/usr/bin/env Rscript
entry <- sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
root <- normalizePath(dirname(entry),mustWork=TRUE)
source(file.path(root,'R/api.R'));api <- graphmode_load(root)
a <- commandArgs(TRUE)
switch(a[1],
 register=api$graphmode_register(a[2],identical(a[3],'smoke')),
 worker=api$graphmode_worker(a[2],as.integer(a[3]),as.integer(a[4])),
 summary=api$graphmode_summary(a[2],as.integer(a[3])),
 test=source(file.path(root,'tests/check.R')),
 example=source(file.path(root,'examples/small.R')),
 stop('Unknown action.'))
