# Rscript examples/beijing/run.R path/to/data.json.gz /path/to/new/output
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L) stop('Supply standalone dataset and new output directory')
library(bdynets)
traffic_figures(args[1], 'beijing', time_index=107L, output_dir=args[2])
