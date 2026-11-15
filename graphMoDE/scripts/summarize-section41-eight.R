#!/usr/bin/env Rscript
# Descriptive complete-method summary; do not substitute historical rows.
a<-commandArgs(TRUE);repo<-normalizePath(a[1]);out<-normalizePath(a[2]);r<-jsonlite::fromJSON(file.path(out,'registration.json'),simplifyVector=FALSE)
source(file.path(repo,'bdynets/R/gmde-dynamic-diagnostics.R'))
report<-file.path(out,'summary');if(dir.exists(report))stop('Fresh summary required.');dir.create(report)
rows<-diagnostics<-chains<-pairs<-list();task_rows<-list()
for(id in names(r$tasks)) {
 t<-r$tasks[[id]];p<-file.path(out,'tasks',id,'status.json');s<-if(file.exists(p))jsonlite::fromJSON(p)else list(status='missing')
 task_rows[[id]]<-data.frame(id=id,dataset=t$dataset,method=t$method,kind=t$kind,status=s$status,message=if(is.null(s$message))''else s$message)
}
tab<-do.call(rbind,task_rows);write.csv(tab,file.path(report,'task-status.csv'),row.names=FALSE)
for(ds in c('D01','D02')) {
 truth_path<-file.path(r$source_archive,ds,'evaluation-only.rds')
 stopifnot(digest::digest(file=truth_path,algo='sha256')==r$source_inputs[[ds]][['evaluation-only.rds']])
 ev<-readRDS(truth_path);input<-readRDS(file.path(out,ds,'input.rds'))
 stopifnot(identical(ev$ids,input$ids))
 for(method in unlist(r$plan$methods)) {
  kind<-if(method=='STGNN')'stgnn_final'else if(method=='PNAR')'pnar_final'else 'bayes'
  tasks<-tab[tab$dataset==ds&tab$method==method&tab$kind==kind,,drop=FALSE]
  row<-data.frame(Data=ds,Method=method,ARI=NA_real_,K_hat=NA_integer_,K_occ_mean=NA_real_,RMSE=NA_real_,Delta_PSM=NA_real_,Check=NA_character_,Status='unavailable',completed=sum(tasks$status=='completed'),expected=nrow(tasks))
  if(all(tasks$status=='completed') && nrow(tasks)>0L) {
   row$Status<-'completed';Z<-NULL
   if(kind=='bayes') {
    fits<-lapply(tasks$id,function(id){path<-file.path(out,'tasks',id,'fit.rds');status<-jsonlite::fromJSON(file.path(out,'tasks',id,'status.json'));stopifnot(digest::digest(file=path,algo='sha256')==status$fit_sha256);readRDS(path)})
    pred<-lapply(tasks$id,function(id){path<-file.path(out,'tasks',id,'forecast.rds');status<-jsonlite::fromJSON(file.path(out,'tasks',id,'status.json'));stopifnot(digest::digest(file=path,algo='sha256')==status$forecast_sha256);readRDS(path)$mean})
    prediction<-Reduce('+',pred)/length(pred);Z<-do.call(rbind,lapply(fits,`[[`,'labels'))
    d<-gmde_dynamic_diagnostics(fits)
    diagnostics[[length(diagnostics)+1]]<-cbind(dataset=ds,method=method,d$diagnostics)
    chains[[length(chains)+1]]<-cbind(dataset=ds,method=method,d$chains)
    pairs[[length(pairs)+1]]<-cbind(dataset=ds,method=method,d$psm_pairs)
    row$Delta_PSM<-max(d$psm_pairs$rms)
    row$Check<-if(all(d$diagnostics$passed)&&row$Delta_PSM<=.1)'P'else'F'
    png(file.path(report,paste0(ds,'-',method,'-traces.png')),width=1500,height=900,res=140)
    par(mfrow=c(2,2),mar=c(4,4,2,1));vv<-c('occupied','log_likelihood','mean_occupied_discount',if(method %in% c('MoDE','PottsMoDE'))'state_acceptance'else'log_kappa')
    for(v in vv){matplot(do.call(cbind,lapply(fits,function(f)f$trace[,v])),type='l',lty=1,col=1:4,xlab='Sweep',ylab=v,main=paste(ds,method));abline(v=2000,lty=2,col='grey');legend('topright',c('Start: 1','Start: 3','Start: 7','Start: 10'),col=1:4,lty=1,bty='n',cex=.7)}
    dev.off()
   } else if(kind=='stgnn_final') {
    fits<-lapply(tasks$id,function(id)jsonlite::fromJSON(file.path(out,'tasks',id,'result.json')))
    prediction<-t(Reduce('+',lapply(fits,`[[`,'prediction'))/length(fits));Z<-do.call(rbind,lapply(fits,`[[`,'labels'))
   } else prediction<-jsonlite::fromJSON(file.path(out,'tasks',tasks$id,'prediction.json'))$prediction
   stopifnot(identical(dim(prediction),dim(ev$Y)),all(is.finite(prediction)))
   row$RMSE<-sqrt(mean((prediction-ev$Y)^2))
   saveRDS(prediction,file.path(report,paste0(ds,'-',method,'-predictive-mean.rds')))
   if(!is.null(Z)) {
    psm<-if(kind=='bayes')Reduce('+',lapply(fits,`[[`,'psm'))/length(fits)else Reduce('+',lapply(seq_len(nrow(Z)),function(j)outer(Z[j,],Z[j,],'==')))/nrow(Z)
    # Dahl representative uses partitions only, with no truth in its selection.
    loss<-vapply(seq_len(nrow(Z)),function(j)sum((outer(Z[j,],Z[j,],'==')-psm)^2),numeric(1));representative<-Z[which.min(loss),]
    row$ARI<-.gmde_adjusted_rand(representative,ev$Z);row$K_hat<-length(unique(representative));row$K_occ_mean<-mean(apply(Z,1,function(z)length(unique(z))))
    write.csv(data.frame(id=input$ids,cluster=representative),file.path(report,paste0(ds,'-',method,'-representative.csv')),row.names=FALSE)
   }
  }
  rows[[length(rows)+1]]<-row
 }
}
results<-do.call(rbind,rows);write.csv(results,file.path(report,'comparison.csv'),row.names=FALSE)
for(n in c('diagnostics','chains','pairs')){x<-get(n);if(length(x))write.csv(do.call(rbind,x),file.path(report,paste0(n,'.csv')),row.names=FALSE)}
writeLines(c('# Fresh eight-method comparison','',
 'All rows belong to the registered current run. Historical results are not substituted.',
 'The two fixed synthetic datasets are D01 and D02. The six Bayesian methods share the current discount expert model, K=10, four chains and 4000/2000 sweeps.',
 'Classification uses the Dahl representative pooled across all four completed chains. Predictive RMSE is over the held-out 48-step count panel.',
 'A completed run is not a convergence certificate. Failed chains and failed validation candidates remain in task-status.csv. Unavailable methods are not replaced with old fits.',
 'All model selection uses training prefixes only. Bayesian forecasts simulate future guide histories; no held-out observations are used to construct predictions.',
 'STGNN and PNAR use pinned external author sources with fresh fits. Only the raw synthetic Exp reference graph at h=1 is supplied.',
 '',paste('Source commit:',r$code_commit)),file.path(report,'README.md'))
html<-paste0('<!doctype html><meta charset="utf-8"><title>Fresh eight-method comparison</title><style>body{font:16px system-ui;max-width:1300px;margin:40px auto}td,th{padding:8px;border-bottom:1px solid #ddd}table{border-collapse:collapse}img{max-width:100%}</style><h1>Fresh eight-method comparison</h1><p>D01 / D02, current discount model. Execution completion and convergence are reported separately. F indicates a failed diagnostic screen.</p><table><tr>',paste0('<th>',names(results),'</th>',collapse=''),'</tr>',paste(vapply(seq_len(nrow(results)),function(j)paste0('<tr>',paste0('<td>',vapply(results[j,],function(x)if(is.numeric(x))format(x,digits=4)else as.character(x),''),'</td>',collapse=''),'</tr>'),''),collapse=''),'</table>',paste0('<img src="',basename(list.files(report,pattern='traces.png',full.names=TRUE)),'">',collapse=''))
writeLines(html,file.path(report,'report.html'))
jsonlite::write_json(list(source_commit=r$code_commit,methods=16L,complete=sum(results$Status=='completed'),checks_passed=sum(results$Check=='P',na.rm=TRUE),summary_sha256=digest::digest(file=file.path(report,'comparison.csv'),algo='sha256')),file.path(report,'summary.json'),auto_unbox=TRUE,pretty=TRUE)
print(results)
