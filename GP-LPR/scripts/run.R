#!/usr/bin/env Rscript
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<2L) stop('Usage: Rscript scripts/run.R report|quick|pilot /absolute/results/run/study.rds [/absolute/report-dir]')
library(bdynets)
mode<-match.arg(args[1],c('report','quick','pilot'))
path<-args[2]
out<-if(length(args)>=3L) args[3] else 'reports'
if(mode!='report') invisible(lpr_run_study(path,profile=mode))
print(lpr_report(path,out))
writeLines(capture.output(sessionInfo()),file.path(out,'session-info.txt'))
