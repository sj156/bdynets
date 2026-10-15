# Run from repository root: Rscript examples/shared-data/run.R /path/to/new/output
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop('Supply an explicit output directory')
if(dir.exists(args[1])) stop('Use a new output directory')
dir.create(args[1],recursive=TRUE)
library(bdynets)
examples <- lapply(c('beijing','largest'),network_example)
names(examples) <- c('beijing','largest')
for(name in names(examples)) {
  x <- examples[[name]]
  saveRDS(network_view_data(x),file.path(args[1],paste0(name,'-view.rds')))
  write.csv(panel_long(x),file.path(args[1],paste0(name,'-observations.csv')),row.names=FALSE)
  pdf(file.path(args[1],paste0(name,'.pdf')),width=7,height=5)
  plot(x,time_index=2)
  dev.off()
}
writeLines(capture.output(sessionInfo()),file.path(args[1],'session-info.txt'))
