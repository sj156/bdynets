#!/usr/bin/env Rscript
entry <- sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
root <- normalizePath(dirname(entry));source(file.path(root,'R/api.R'));api <- graphmode_load(root)
a <- commandArgs(TRUE)
switch(a[1],register=api$graphmode_extra_register(a[2],a[3]),
       worker=api$graphmode_extra_worker(a[2],as.integer(a[3])),
       summary=api$graphmode_extra_summary(a[2]),
       stop('register OUTPUT CONFIG | worker OUTPUT INDEX | summary OUTPUT'))
