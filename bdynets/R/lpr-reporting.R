#' Summarize latent-position recovery without refitting
#' @param fits Nonempty list of lpr_fit objects, one per chain, with matching data/configuration.
#' @param truth True raw or centered n by d positions, for simulation evaluation only.
#' @return True and recovered centered positions, alignment reference, coordinate RMSE,
#' and posterior mean interactions. Truth is used only for evaluation alignment.
#' @export
lpr_recovery <- function(fits,truth) {
  if(!length(fits)) stop("No chains")
  first<-fits[[1]]
  if(any(!vapply(fits,function(f) identical(f$data,first$data) && identical(f$config,first$config),logical(1)))) stop("Incompatible chain data/configuration")
  n<-first$data$n; d<-first$config$dimension
  if(!is.matrix(truth) || !identical(dim(truth),c(n,as.integer(d))) || any(!is.finite(truth))) stop("Invalid truth")
  raw<-do.call(rbind,lapply(fits,function(f) f$draws$raw))
  alpha<-do.call(rbind,lapply(fits,function(f) f$draws$alpha))
  mu<-unlist(lapply(fits,function(f) f$draws$mu))
  ll<-unlist(lapply(fits,function(f) f$draws$loglik))
  if(!length(mu)) stop("No posterior draws")
  score<-ll-mu^2/(2*first$config$sd_mu^2)-rowSums(alpha^2)/(2*first$config$sd_alpha^2)
  ref<-matrix(raw[which.max(score),],n,d,byrow=TRUE)
  mean_z<-matrix(0,n,d); mean_g<-numeric(nrow(first$data$edges))
  for(s in seq_len(nrow(raw))) {
    z<-matrix(raw[s,],n,d,byrow=TRUE)
    mean_z<-mean_z+lpr_align(z,ref)/nrow(raw)
    mean_g<-mean_g+.lpr_inner(.lpr_center(z),first$data$edges)/nrow(raw)
  }
  estimate<-lpr_align(mean_z,truth); truth<-.lpr_center(truth)
  true_g<-.lpr_inner(truth,first$data$edges)
  list(truth=truth,estimate=estimate,reference=.lpr_center(ref),
       position_rmse=sqrt(mean((estimate-truth)^2)),interaction_mean=mean_g,
       interaction_rmse=sqrt(mean((mean_g-true_g)^2)),interaction_correlation=stats::cor(mean_g,true_g))
}

#' Plot matched true and recovered positions at different exposures
#' @param recoveries Named list of outputs from lpr_recovery(), with dimension two.
#' @param file PDF or PNG output path.
#' @return Invisible output filename. Lines join each true position to its estimate.
#' @export
lpr_plot_positions <- function(recoveries,file) {
  if(!length(recoveries) || any(vapply(recoveries,function(x) ncol(x$truth)!=2L,logical(1)))) stop("Need two-dimensional recoveries")
  dir.create(dirname(file),recursive=TRUE,showWarnings=FALSE)
  if(grepl("\\.pdf$",file,ignore.case=TRUE)) grDevices::pdf(file,width=4.4*length(recoveries),height=4.8,useDingbats=FALSE) else
    grDevices::png(file,width=880*length(recoveries),height=960,res=200)
  on.exit(grDevices::dev.off(),add=TRUE)
  graphics::par(mfrow=c(1,length(recoveries)),mar=c(4,4,4,1),oma=c(2,0,0,0))
  all<-unlist(lapply(recoveries,function(r) c(r$truth,r$estimate)))
  lim<-range(all); pad<-diff(lim)*.12; lim<-lim+c(-pad,pad)
  for(k in seq_along(recoveries)) {
    r<-recoveries[[k]]; a<-r$truth; b<-r$estimate
    graphics::plot(a,type='n',xlim=lim,ylim=lim,asp=1,xlab='Latent coordinate 1',ylab='Latent coordinate 2',
                   main=sprintf('%s\nAligned coordinate RMSE = %.3f',names(recoveries)[k],r$position_rmse))
    graphics::abline(h=0,v=0,col='grey92')
    graphics::segments(a[,1],a[,2],b[,1],b[,2],col='grey60',lwd=1)
    graphics::points(a,pch=1,col='#1764A0',cex=1.1,lwd=1.5)
    graphics::points(b,pch=4,col='#BF3A29',cex=1.1,lwd=1.5)
    graphics::legend('bottomleft',legend=c('True position','Recovered posterior mean'),pch=c(1,4),
                     col=c('#1764A0','#BF3A29'),bty='n',cex=.8)
  }
  graphics::mtext('Orthogonal alignment only; no rescaling. Segments connect the same node.',outer=TRUE,side=1,line=.5,cex=.8)
  invisible(file)
}

#' Compute modern chain diagnostics for scalar and invariant outputs
#' @param fits Equal-length independent chains with matching data/configuration.
#' @return Data frame with rank/folded Rhat and bulk/tail ESS. Requires posterior.
#' @export
lpr_diagnostics <- function(fits) {
  if(!requireNamespace('posterior',quietly=TRUE)) stop("Install posterior for diagnostics")
  if(length(fits)<2L) stop("Need at least two chains")
  first<-fits[[1]]; n<-first$data$n; d<-first$config$dimension
  if(any(!vapply(fits,function(f) identical(f$data,first$data) && identical(f$config,first$config),logical(1)))) stop("Incompatible chains")
  lengths<-vapply(fits,function(f) length(f$draws$mu),integer(1))
  if(length(unique(lengths))!=1L || lengths[1]<4L) stop("Need equal lengths >=4")
  matrices<-lapply(fits,function(f) {
    g<-t(vapply(seq_len(length(f$draws$mu)),function(s) .lpr_inner(.lpr_center(matrix(f$draws$raw[s,],n,d,byrow=TRUE)),f$data$edges),numeric(nrow(f$data$edges))))
    cbind(mu=f$draws$mu,loglik=f$draws$loglik,f$draws$alpha,g)
  })
  labels<-c('mu','loglik',paste0('alpha[',seq_len(n),']'),paste0('interaction[',seq_len(nrow(first$data$edges)),']'))
  values<-lapply(seq_along(labels),function(j) {
    x<-do.call(cbind,lapply(matrices,function(a) a[,j]))
    c(rhat=posterior::rhat(x),ess_bulk=posterior::ess_bulk(x),ess_tail=posterior::ess_tail(x))
  })
  data.frame(variable=labels,do.call(rbind,values),row.names=NULL)
}

#' Produce recovery figures and a compact summary from a saved study
#' @param results_file RDS study with named cases containing fits and truth.
#' @param output_dir Destination for small intended figures and scores.
#' @return Data frame of recovery metrics. Never runs a sampler.
#' @export
lpr_report <- function(results_file,output_dir) {
  study<-lpr_read(results_file)
  rec<-lapply(study$cases,function(x) lpr_recovery(x$fits,x$truth))
  names(rec)<-vapply(study$cases,function(x) x$label,character(1))
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  lpr_plot_positions(rec,file.path(output_dir,'aligned_positions.pdf'))
  lpr_plot_positions(rec,file.path(output_dir,'aligned_positions.png'))
  tab<-data.frame(regime=names(rec),position_rmse=vapply(rec,`[[`,numeric(1),'position_rmse'),
                  interaction_rmse=vapply(rec,`[[`,numeric(1),'interaction_rmse'),
                  interaction_correlation=vapply(rec,`[[`,numeric(1),'interaction_correlation'))
  utils::write.csv(tab,file.path(output_dir,'recovery.csv'),row.names=FALSE)
  tab
}

#' Run a small or pilot-sized Poisson recovery workflow
#' @param results_file New output RDS path. Existing results are never overwritten.
#' @param profile "quick" checks execution; "pilot" uses n=30 and the manuscript budget.
#' @param seed Configuration seed. R's RNG differs from the historical Python run.
#' @return Saved study. Pilot runs require separate diagnostic interpretation.
#' @export
lpr_run_study <- function(results_file,profile=c('quick','pilot'),seed=20261015L) {
  profile<-match.arg(profile)
  if(file.exists(results_file)) stop("Results already exist; use a distinct run ID")
  set.seed(seed)
  n<-if(profile=='quick') 8L else 30L; d<-2L
  truth<-.lpr_ball(n,d,1.5); alpha<-stats::rnorm(n,sd=.5); alpha<-alpha-mean(alpha)
  edges<-t(utils::combn(n,2)); cases<-list()
  for(k in seq_along(c(1,10))) {
    exposure<-c(1,10)[k]; set.seed(1000L+k)
    eta<--1+alpha[edges[,1]]+alpha[edges[,2]]+.lpr_inner(.lpr_center(truth),edges)
    data<-lpr_data(stats::rpois(nrow(edges),exposure*exp(eta)),edges,n,exposure)
    chains<-if(profile=='quick') 2L else 4L
    fits<-lapply(seq_len(chains),function(chain) lpr_fit(data,sweeps=if(profile=='quick') 120L else 9000L,
      warmup=if(profile=='quick') 60L else 3000L,thin=if(profile=='quick') 1L else 3L,
      seed=10000L+100L*k+chain-1L,provenance=list(profile=profile,configuration_seed=seed)))
    cases[[paste0('E',exposure)]]<-list(fits=fits,truth=truth,true_alpha=alpha,label=paste('Poisson, exposure',exposure))
  }
  study<-list(cases=cases,provenance=list(engine='R bdynets lpr',profile=profile,seed=seed,status='Completed; inspect diagnostics before inference'))
  lpr_save(study,results_file)
  study
}
