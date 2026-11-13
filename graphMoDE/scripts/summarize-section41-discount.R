args<-commandArgs(TRUE)
stopifnot(length(args)==3L)
repo<-normalizePath(args[1]);run<-normalizePath(args[2]);dest<-args[3]
stopifnot(!dir.exists(dest));dir.create(dest,recursive=TRUE)
library(Rcpp)
dll<-dyn.load(file.path(run,'build/src/bdynets.so'));symbols<-getDLLRegisteredRoutines(dll)$.Call
for(name in names(symbols))assign(name,symbols[[name]],envir=.GlobalEnv)
for(name in c('RcppExports.R','gmde-validation.R','gmde-allocation.R','gmde-discount.R','gmde-dynamic.R','gmde-dynamic-diagnostics.R'))source(file.path(repo,'bdynets/R',name))
registration<-jsonlite::fromJSON(file.path(run,'registration.json'),simplifyVector=FALSE)
all_status<-all_chains<-all_diag<-all_psm<-all_eval<-all_cache<-list()
for(id in names(registration$tasks)) {
 task<-registration$tasks[[id]];sp<-file.path(run,id,'status.json')
 status<-if(file.exists(sp))jsonlite::fromJSON(sp) else list(status='not_run')
 all_status[[id]]<-data.frame(task=id,phase=task$phase,dataset=task$dataset,method=task$method,
   chain=task$chain,seed=task$seed,status=status$status,seconds=if(is.null(status$seconds))NA_real_ else status$seconds)
}
write.csv(do.call(rbind,all_status),file.path(dest,'task-status.csv'),row.names=FALSE)
for(dataset in c('D01','D02'))for(method in c('Exp-W','SE-W','C')){
 ids<-sprintf('development-%s-%s-chain%02d',dataset,method,1:4)
 if(any(!vapply(ids,function(id)file.exists(file.path(run,id,'fit.rds')),logical(1)))) next
 fits<-lapply(ids,function(id)readRDS(file.path(run,id,'fit.rds')))
 diag<-gmde_dynamic_diagnostics(fits);key<-paste(dataset,method,sep='-')
 input<-readRDS(file.path(registration$source_archive,dataset,'blind.rds'))$input
 for(j in seq_along(fits)) {
   f<-fits[[j]];state<-f$terminal;ctx<-.gmde_dynamic_config(input,f$config)
   suff<-.gmde_sufficient(input$Y,state$Z,f$config$K)
   guide_error<-0
   for(k in seq_len(f$config$K)) {
     g<-.gmde_dynamic_guide(suff$S[k,],suff$N[k],state$d[k],ctx)
     guide_error<-max(guide_error,max(abs(g$innovation_covariance-state$guides[[k]]$innovation_covariance)))
   }
   spec<-.gmde_dynamic_spectrum(input,method,state$h,f$config$nu,f$config$tau,state$u)
   q_error<-abs(.gmde_dynamic_gp_density(state$field,spec)-.gmde_dynamic_gp_density(state$field,state$spec))
   labels_ok<-identical(suff$N,state$N)&&identical(suff$S,state$S)
   all_cache[[paste(key,j)]]<-data.frame(dataset=dataset,method=method,chain=j,
     summaries_match=labels_ok,max_W_error=guide_error,GP_density_error=q_error,
     passed=labels_ok&&guide_error<1e-12&&q_error<1e-8)
 }
 all_chains[[key]]<-cbind(dataset=dataset,method=method,diag$chains)
 all_diag[[key]]<-cbind(dataset=dataset,method=method,diag$diagnostics)
 all_psm[[key]]<-cbind(dataset=dataset,method=method,diag$psm_pairs)
 # Evaluation data are read only here, after these fits complete. No tuning uses them.
 evaluation<-readRDS(file.path(registration$source_archive,dataset,'evaluation-only.rds'))
 labels<-do.call(rbind,lapply(fits,`[[`,'labels'))
 psm<-Reduce(`+`,lapply(fits,`[[`,'psm'))/4
 loss<-apply(labels,1,function(z){B<-outer(z,z,'==');sum(B*(1-2*psm))})
 estimate<-labels[which.min(loss),]
 fitted<-Reduce(`+`,lapply(fits,`[[`,'fitted_intensity'))/4
 rmse<-sqrt(mean((fitted-evaluation$lambda[,seq_len(ncol(fitted))])^2))
 ari<-.gmde_adjusted_rand(estimate,evaluation$Z)
 all_eval[[key]]<-data.frame(dataset=dataset,method=method,ARI=ari,representative_groups=length(unique(estimate)),
   training_intensity_RMSE=rmse,interpretation='Unconverged development descriptor, not a method comparison')
 png(file.path(dest,paste0(key,'-traces.png')),width=1400,height=980,res=140)
 par(mfrow=c(2,2),mar=c(3,4,2,1))
 for(v in c('occupied','log_likelihood','log_kappa','mean_occupied_discount')){
  ys<-do.call(cbind,lapply(fits,function(f)f$trace[,v]))
  matplot(ys,type='l',lty=1,col=1:4,xlab='Iteration',ylab=v,main=paste(dataset,method))
  abline(v=100,lty=2,col='gray')
 }
 dev.off()
}
for(pair in list(c('chain-summary.csv','all_chains'),c('diagnostics.csv','all_diag'),c('psm-pairs.csv','all_psm'),c('descriptive-fit.csv','all_eval'),c('terminal-cache-checks.csv','all_cache'))){
 x<-get(pair[2]);if(length(x))write.csv(do.call(rbind,x),file.path(dest,pair[1]),row.names=FALSE)
}
writeLines(capture.output(sessionInfo()),file.path(dest,'analysis-session.txt'))
cat('Saved completed-fit diagnostics and descriptive evaluation to',dest,'\n')
