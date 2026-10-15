# Rscript examples/largest/run.R path/to/data.json.gz /path/to/new/output
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L) stop('Supply standalone dataset and new output directory')
library(bdynets)
traffic_figures(args[1], 'largest', time_index=195L, output_dir=args[2])
