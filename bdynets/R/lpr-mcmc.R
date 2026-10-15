#' Fit a no-GP centered inner-product model
#'
#' Uses independent Gaussian priors for the intercept and raw endpoint effects,
#' and independent uniform-ball priors for raw positions. Position and endpoint
#' proposals change all centered values. Warmup tuning stops before sampling.
#' @param data A validated object from lpr_data().
#' @param sweeps Number of sweeps to execute now, including any warmup.
#' @param warmup Total initial warmup sweeps, fixed across continuation.
#' @param thin Save the first post-warmup state and then every thin sweeps.
#' @param dimension Positive latent dimension.
#' @param radius Positive radius of the raw-position prior ball.
#' @param sd_mu,sd_alpha Positive prior standard deviations.
#' @param seed Random seed. The caller's RNG state is restored.
#' @param provenance Named list identifying input data and experiment.
#' @return lpr_fit list with chain-resolved draws, complete checkpoint, configuration,
#' data, and provenance. Short defaults are execution checks, not converged inference.
#' @export
lpr_fit <- function(data, sweeps=100L, warmup=50L, thin=1L, dimension=2L,
                    radius=1.5, sd_mu=3, sd_alpha=.5, seed=1L, provenance=list()) {
  if (!inherits(data,"lpr_data")) stop("Use lpr_data()")
  for (v in list(sweeps,thin,dimension)) if (length(v)!=1L || !is.finite(v) || v<1 || v!=floor(v)) stop("Invalid integer control")
  if (length(warmup)!=1L || !is.finite(warmup) || warmup<0 || warmup!=floor(warmup)) stop("Invalid warmup")
  for (v in list(radius,sd_mu,sd_alpha)) if (length(v)!=1L || !is.finite(v) || v<=0) stop("Invalid prior scale")
  old <- if (exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
  on.exit(if(is.null(old)) {if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) rm(".Random.seed",envir=.GlobalEnv)} else assign(".Random.seed",old,.GlobalEnv),add=TRUE)
  set.seed(seed)
  config<-list(warmup=warmup,thin=thin,dimension=dimension,radius=radius,sd_mu=sd_mu,sd_alpha=sd_alpha)
  state<-list(raw=.lpr_ball(data$n,dimension,radius),b=stats::rnorm(data$n,sd=sd_alpha),mu=stats::rnorm(1))
  cp<-list(target_id=.lpr_identity(list(data=data,config=config)),code_id=.lpr_code_id(),state=state,sweep=0L,steps=c(z=.3,b=.15,mu=.15),batch=c(z=0,b=0,mu=0),accepted=c(z=0,b=0,mu=0),sampling_sweeps=0L)
  object<-structure(list(data=data,config=config,checkpoint=cp,draws=list(raw=matrix(numeric(),0,data$n*dimension),alpha=matrix(numeric(),0,data$n),mu=numeric(),loglik=numeric()),
    provenance=c(list(engine="R bdynets lpr",R=R.version.string,implementation_id=cp$code_id,package=as.character(utils::packageVersion("bdynets")),seed=seed),provenance)),class="lpr_fit")
  .lpr_run(object,sweeps)
}

#' Continue an LPR chain from its complete checkpoint
#' @param fit Result from lpr_fit() or lpr_continue(). Historical imports without
#' raw endpoint and RNG checkpoints cannot be continued and raise an error.
#' @param sweeps Additional sweeps; warmup is never restarted.
#' @return Updated fit, preserving all previous draws and provenance.
#' @export
lpr_continue <- function(fit,sweeps) {
  if (is.null(fit$checkpoint$rng)) stop("No complete restart checkpoint; cannot continue this fit")
  if (!identical(fit$checkpoint$target_id,.lpr_identity(list(data=fit$data,config=fit$config)))) stop("Checkpoint target changed")
  if (!identical(fit$checkpoint$code_id,.lpr_code_id())) stop("Checkpoint inference code changed; explicit migration required")
  if(length(sweeps)!=1L || !is.finite(sweeps) || sweeps<1 || sweeps!=floor(sweeps)) stop("Invalid sweeps")
  old <- if (exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
  on.exit(if(is.null(old)) {if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) rm(".Random.seed",envir=.GlobalEnv)} else assign(".Random.seed",old,.GlobalEnv),add=TRUE)
  assign(".Random.seed",fit$checkpoint$rng,.GlobalEnv)
  .lpr_run(fit,sweeps)
}

.lpr_run <- function(fit,sweeps) {
  data<-fit$data; cfg<-fit$config; cp<-fit$checkpoint; state<-cp$state
  eta<-.lpr_eta(data,state); ll<-.lpr_loglik(data,eta)
  n<-data$n; d<-cfg$dimension; saved<-list(); idx<-0L
  for (iteration in seq_len(sweeps)) {
    cp$sweep<-cp$sweep+1L; sampling<-cp$sweep>cfg$warmup
    for(k in sample.int(n)) {
      delta<-stats::rnorm(d,sd=cp$steps['z']); proposed<-state$raw[k,]+delta
      if(sum(proposed^2)>cfg$radius^2) next
      h<-.lpr_z_change(.lpr_center(state$raw),k,delta,data$edges)
      lp<-.lpr_loglik(data,eta+h)
      if(log(stats::runif(1))<lp-ll) {
        state$raw[k,]<-proposed; eta<-eta+h; ll<-lp
        cp$batch['z']<-cp$batch['z']+1
        if(sampling) cp$accepted['z']<-cp$accepted['z']+1
      }
    }
    for(k in sample.int(n)) {
      delta<-stats::rnorm(1,sd=cp$steps['b']); h<-.lpr_b_change(n,k,delta,data$edges)
      lp<-.lpr_loglik(data,eta+h)
      ratio<-lp-ll-((state$b[k]+delta)^2-state$b[k]^2)/(2*cfg$sd_alpha^2)
      if(log(stats::runif(1))<ratio) {
        state$b[k]<-state$b[k]+delta; eta<-eta+h; ll<-lp
        cp$batch['b']<-cp$batch['b']+1
        if(sampling) cp$accepted['b']<-cp$accepted['b']+1
      }
    }
    delta<-stats::rnorm(1,sd=cp$steps['mu']); lp<-.lpr_loglik(data,eta+delta)
    ratio<-lp-ll-((state$mu+delta)^2-state$mu^2)/(2*cfg$sd_mu^2)
    if(log(stats::runif(1))<ratio) {
      state$mu<-state$mu+delta; eta<-eta+delta; ll<-lp
      cp$batch['mu']<-cp$batch['mu']+1
      if(sampling) cp$accepted['mu']<-cp$accepted['mu']+1
    }
    if(!sampling && cp$sweep%%100L==0L) {
      cp$steps<-cp$steps*exp(.5*(cp$batch/c(z=100*n,b=100*n,mu=100)-c(z=.35,b=.44,mu=.44)))
      cp$batch[]<-0
    }
    if(sampling) cp$sampling_sweeps<-cp$sampling_sweeps+1L
    if(sampling && (cp$sweep-cfg$warmup-1L)%%cfg$thin==0L) {
      exact<-.lpr_eta(data,state)
      if(max(abs(eta-exact))>1e-8) stop("Predictor cache mismatch")
      idx<-idx+1L
      saved[[idx]]<-list(raw=as.vector(t(state$raw)),alpha=state$b-mean(state$b),mu=state$mu,loglik=ll)
    }
  }
  if(length(saved)) for(name in names(fit$draws)) {
    values<-lapply(saved,`[[`,name)
    fit$draws[[name]]<-if(name %in% c('raw','alpha')) rbind(fit$draws[[name]],do.call(rbind,values)) else c(fit$draws[[name]],unlist(values))
  }
  cp$state<-state; cp$rng<-get(".Random.seed",.GlobalEnv); cp$rng_kind<-RNGkind()
  fit$checkpoint<-cp
  fit$status<-if(cp$sampling_sweeps>0) "sampling completed; convergence not guaranteed" else "warmup only"
  fit
}
