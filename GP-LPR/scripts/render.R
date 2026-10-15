#!/usr/bin/env Rscript
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop('From GP-LPR/: Rscript scripts/render.R /absolute/results/run/study.rds')
if(!file.exists(args[1])) stop('Saved results missing; rendering never refits')
rmarkdown::render('recovery.Rmd',params=list(results_file=normalizePath(args[1]),output_dir=normalizePath('reports')),
                  output_file='recovery.html',output_dir='reports',quiet=TRUE)
