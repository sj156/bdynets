#!/usr/bin/env Rscript
# Execute one registered task; reusable calculations remain inside bdynets.
a<-commandArgs(TRUE);stopifnot(length(a)==3L)
repo<-normalizePath(a[1]);out<-normalizePath(a[2]);id<-a[3]
r<-jsonlite::fromJSON(file.path(out,'registration.json'),simplifyVector=FALSE);task<-r$tasks[[id]]
if(is.null(task))stop('Unregistered task.')
if(!identical(system2('git',c('-C',shQuote(repo),'rev-parse','HEAD'),stdout=TRUE),r$code_commit))stop('Commit changed.')
for(e in r$sources)if(digest::digest(file=file.path(repo,e$path),algo='sha256')!=e$sha256)stop('Source changed: ',e$path)
path<-file.path(out,'tasks',id);if(dir.exists(path))stop('No overwrite or resume.')
dir.create(path);jsonlite::write_json(task,file.path(path,'task.json'),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(path,'session.txt'))
library(Rcpp);lib<-file.path(out,'build','bdynets.so')
if(digest::digest(file=lib,algo='sha256')!=r$binary_sha256)stop('Changed binary.')
dll<-dyn.load(lib);symbols<-getDLLRegisteredRoutines(dll)$.Call
for(n in names(symbols))assign(n,symbols[[n]],.GlobalEnv)
for(n in c('RcppExports.R','gmde-validation.R','gmde-allocation.R','gmde-discount.R','gmde-dynamic.R','gmde-dynamic-comparison.R','gmde-pnar-benchmark.R'))source(file.path(repo,'bdynets/R',n))
input_path<-file.path(out,task$dataset,'input.rds')
if(digest::digest(file=input_path,algo='sha256')!=r$prepared_inputs[[task$dataset]]$sha256)stop('Changed input.')
input<-readRDS(input_path);full_training<-input$Y
cfg<-task$config
rows<-list();last<-0L;started<-proc.time()[3]
result<-tryCatch({
 if(task$kind %in% c('bayes','potts_validation','smoke')) {
  for(n in c('initial_mean','discount_prior'))cfg[[n]]<-as.numeric(unlist(cfg[[n]]))
  cfg$initial_covariance<-diag(as.numeric(unlist(cfg$initial_covariance_diagonal)));cfg$initial_covariance_diagonal<-NULL
  if(task$kind=='bayes' && task$method=='PottsMoDE')cfg$potts_beta<-jsonlite::fromJSON(file.path(out,task$dataset,'selection-PottsMoDE.json'))$selected$beta
  if(task$kind=='potts_validation') {
   origin<-task$origin;input$Y<-full_training[,seq_len(origin),drop=FALSE]
   input$Ffuture<-input$Fmat[origin+seq_len(48L),,drop=FALSE];input$Fmat<-input$Fmat[seq_len(origin),,drop=FALSE]
  }
  jsonlite::write_json(cfg,file.path(path,'effective-config.json'),auto_unbox=TRUE,pretty=TRUE,digits=17)
  RNGkind('Mersenne-Twister','Inversion','Rejection');set.seed(task$seed)
  fit<-gmde_dynamic_fit(input,task$method,cfg,progress=function(x){
   last<<-x$iteration;rows[[last]]<<-c(iteration=last,elapsed=x$elapsed,x$trace)
   if(last%%50L==0L || last==cfg$iterations){
    write.csv(do.call(rbind,rows),file.path(path,'progress.csv'),row.names=FALSE)
    saveRDS(list(iteration=last,state=x$state,rng=.Random.seed),file.path(path,'latest-checkpoint.rds'))
    cat(id,last,'/',cfg$iterations,'seconds',round(x$elapsed,1),'\n');flush.console()
   }
  })
  saveRDS(fit,file.path(path,'fit.rds'));saveRDS(.Random.seed,file.path(path,'fit-rng.rds'))
  jsonlite::write_json(list(stage='forecast',fit_iterations=last),file.path(path,'stage.json'),auto_unbox=TRUE)
  set.seed(task$forecast_seed)
  pred<-gmde_dynamic_predict(fit,input$Ffuture,as.integer(r$plan$forecast_histories))
  saveRDS(pred,file.path(path,'forecast.rds'));saveRDS(.Random.seed,file.path(path,'forecast-rng.rds'))
  jsonlite::write_json(list(prediction=unname(pred$mean)),file.path(path,'prediction.json'),auto_unbox=TRUE,digits=17)
  list(status='completed',iterations=last,seconds=unname(proc.time()[3]-started),
   fit_sha256=digest::digest(file=file.path(path,'fit.rds'),algo='sha256'),
   forecast_sha256=digest::digest(file=file.path(path,'forecast.rds'),algo='sha256'))
 } else if(task$kind %in% c('pnar_validation','pnar_final')) {
  p<-if(task$kind=='pnar_final')jsonlite::fromJSON(file.path(out,task$dataset,'selection-PNAR.json'))$selected$p else task$p
  train<-if(task$kind=='pnar_final')full_training else full_training[,seq_len(task$origin),drop=FALSE]
  author_path<-r$external$pnar
  if(digest::digest(file=author_path,algo='sha256')!=r$external_lock$PNAR$sha256)stop('Changed PNAR author source.')
  author<-new.env(parent=globalenv());sys.source(author_path,author)
  fitted<-.gmde_pnar_fit(train,input$fixed_weight,as.integer(p),author$lin_estimnarpq)
  saveRDS(fitted,file.path(path,'fit.rds'))
  jsonlite::write_json(list(prediction=unname(fitted$prediction),p=p,status=fitted$status),file.path(path,'prediction.json'),auto_unbox=TRUE,digits=17)
  list(status='completed',seconds=unname(proc.time()[3]-started),p=p)
 } else stop('Unknown task kind.')
},error=function(e)list(status='failed',message=conditionMessage(e),completed_iterations=last,seconds=unname(proc.time()[3]-started)))
if(length(rows))write.csv(do.call(rbind,rows),file.path(path,'progress.csv'),row.names=FALSE)
jsonlite::write_json(result,file.path(path,'status.json'),auto_unbox=TRUE,pretty=TRUE,digits=17)
if(result$status!='completed')quit(status=1L)
