# Deterministic preparation: input/design only; no old fit, start or truth reused.
a<-commandArgs(TRUE);repo<-a[1];archive<-a[2];out<-a[3]
library(Rcpp);dll<-dyn.load(file.path(out,'build','bdynets.so'))
for(n in names(getDLLRegisteredRoutines(dll)$.Call))assign(n,getDLLRegisteredRoutines(dll)$.Call[[n]],.GlobalEnv)
source(file.path(repo,'bdynets/R/RcppExports.R'));source(file.path(repo,'bdynets/R/gmde-dynamic-comparison.R'))
records<-list()
for(ds in c('D01','D02')) {
 input<-readRDS(file.path(archive,ds,'blind.rds'))$input;design<-readRDS(file.path(archive,ds,'design.rds'))
 xy<-design$road$coordinates[match(input$ids,design$road$ids),,drop=FALSE]
 stopifnot(nrow(xy)==nrow(input$Y),!anyNA(xy),!anyDuplicated(input$ids))
 input$euclidean_distance<-as.matrix(dist(xy));input$fixed_weight<-input$adjacency*exp(-input$edge_lengths)
 cal<-.gmde_dynamic_euclidean_center(input$euclidean_distance,3,4L)
 path<-file.path(out,ds);dir.create(path);saveRDS(input,file.path(path,'input.rds'))
 jsonlite::write_json(list(Y=unname(t(input$Y)),weight=unname(input$fixed_weight)),file.path(path,'training.json'),auto_unbox=TRUE,digits=17)
 records[[ds]]<-list(sha256=digest::digest(file=file.path(path,'input.rds'),algo='sha256'),training_sha256=digest::digest(file=file.path(path,'training.json'),algo='sha256'),euclidean_center=cal)
}
jsonlite::write_json(records,file.path(out,'prepared-inputs.json'),auto_unbox=TRUE,pretty=TRUE,digits=17)
