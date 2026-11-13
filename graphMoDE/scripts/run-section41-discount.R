#!/usr/bin/env Rscript
# One preregistered task. The shared library is built externally before launch.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=5L)stop('Usage: REPOSITORY ARCHIVE REGISTRATION TASK_ID OUTPUT_ROOT')
repo<-normalizePath(args[1]);archive<-normalizePath(args[2]);registration_path<-normalizePath(args[3])
registration<-jsonlite::fromJSON(registration_path,simplifyVector=FALSE)
task<-registration$tasks[[args[4]]]
if(is.null(task))stop('Task ID missing from registration.')
output_root<-normalizePath(args[5]);destination<-file.path(output_root,args[4])
if(dir.exists(destination))stop('Task destination already exists; no resume or overwrite.')
head<-system2('git',c('-C',shQuote(repo),'rev-parse','HEAD'),stdout=TRUE)
if(!identical(head,registration$code_commit))stop('Code commit differs from registration.')
for(entry in registration$sources){
 path<-file.path(repo,entry$path)
 if(!identical(digest::digest(file=path,algo='sha256'),entry$sha256))stop('Source hash changed: ',entry$path)
}
blind_path<-file.path(archive,task$dataset,'blind.rds')
if(!identical(digest::digest(file=blind_path,algo='sha256'),task$input_sha256))stop('Input hash changed.')
input<-readRDS(blind_path)$input
# Deliberately discard historical fits and starting states from the input archive.
cfg<-task$config
for(name in c('initial_mean','discount_prior'))cfg[[name]]<-as.numeric(unlist(cfg[[name]]))
cfg$initial_covariance<-diag(as.numeric(unlist(cfg$initial_covariance_diagonal)))
cfg$initial_covariance_diagonal<-NULL
library(Rcpp)
dll_path<-file.path(output_root,'build','src','bdynets.so')
if(!identical(digest::digest(file=dll_path,algo='sha256'),registration$binary_sha256))stop('Binary hash changed.')
dll<-dyn.load(dll_path);symbols<-getDLLRegisteredRoutines(dll)$.Call
for(name in names(symbols))assign(name,symbols[[name]],envir=.GlobalEnv)
for(name in c('RcppExports.R','gmde-validation.R','gmde-allocation.R','gmde-discount.R','gmde-dynamic.R'))
 source(file.path(repo,'bdynets','R',name))
dir.create(destination)
jsonlite::write_json(task,file.path(destination,'task.json'),auto_unbox=TRUE,pretty=TRUE,digits=17)
writeLines(capture.output(sessionInfo()),file.path(destination,'session.txt'))
RNGkind('Mersenne-Twister','Inversion','Rejection');set.seed(task$seed)
trace_rows<-list();last_iteration<-0L
result<-tryCatch(gmde_dynamic_fit(input,task$method,cfg,progress=function(progress){
 last_iteration<<-progress$iteration
 trace_rows[[last_iteration]]<<-c(iteration=last_iteration,elapsed=progress$elapsed,progress$trace)
 if(last_iteration%%10L==0L||last_iteration==cfg$iterations){
  write.csv(do.call(rbind,trace_rows),file.path(destination,'progress.csv'),row.names=FALSE)
  saveRDS(list(state=progress$state,iteration=last_iteration,rng=.Random.seed),file.path(destination,'latest-checkpoint.rds'))
  cat(args[4],last_iteration,'/',cfg$iterations,'seconds',round(progress$elapsed,2),'\n');flush.console()
 }
}),error=function(e)e)
write.csv(do.call(rbind,trace_rows),file.path(destination,'progress.csv'),row.names=FALSE)
saveRDS(.Random.seed,file.path(destination,'final-rng.rds'))
if(inherits(result,'error')){
 jsonlite::write_json(list(status='failed',message=conditionMessage(result),completed_iterations=last_iteration),
  file.path(destination,'status.json'),auto_unbox=TRUE,pretty=TRUE)
 quit(status=1L)
}
saveRDS(result,file.path(destination,'fit.rds'))
jsonlite::write_json(list(status='completed',iterations=cfg$iterations,seconds=result$elapsed,
 fit_sha256=digest::digest(file=file.path(destination,'fit.rds'),algo='sha256'),convergence_certified=FALSE),
 file.path(destination,'status.json'),auto_unbox=TRUE,pretty=TRUE,digits=17)
