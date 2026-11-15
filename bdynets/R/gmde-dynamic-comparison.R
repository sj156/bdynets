# Current dynamic-expert comparators. All reusable model code lives in bdynets.
.gmde_dynamic_euclidean <- function(input,nu,tau,u) {
  D <- input$euclidean_distance
  if(is.null(D))stop("Aligned Euclidean distances required.")
  C <- .gmde_matern_covariance_cpp(D,nu,tau,u)
  E <- eigen(C,symmetric=TRUE)
  if(any(!is.finite(E$values)) || any(E$values<=0))stop("Euclidean covariance is not numerically positive definite; no jitter applied.")
  list(vectors=E$vectors,variance=E$values,log_variance=log(E$values),nu=nu,tau=tau)
}

.gmde_dynamic_euclidean_center <- function(distance,nu=3,neighbors=4L) {
  n<-nrow(distance);A<-matrix(FALSE,n,n)
  if(neighbors>=n)stop("Too many calibration neighbors.")
  for(i in seq_len(n))A[i,order(replace(distance[i,],i,Inf))[seq_len(neighbors)]]<-TRUE
  A<-A|t(A);pairs<-which(A&upper.tri(A),arr.ind=TRUE)
  target<-function(u)median(.gmde_matern_covariance_cpp(distance,nu,1,u)[pairs])-.5
  centre<-uniroot(target,c(log(min(distance[distance>0]))-8,log(max(distance))+8),tol=1e-12)$root
  list(u0=centre,range0=exp(centre),median_neighbor_correlation=target(centre)+.5)
}

.gmde_dynamic_validate_potts <- function(W,beta,n) {
  if(!identical(dim(W),c(n,n)) || !isSymmetric(W) || any(!is.finite(W)) || any(W<0) || any(diag(W)!=0) ||
     length(beta)!=1L || !is.finite(beta) || beta<0)stop("Invalid fixed Potts graph/interaction.")
  invisible(TRUE)
}

#' Predict Poisson counts along simulated future discount-guide histories
#' @param fit A gmde_dynamic_fit result with save_predictive_state=TRUE.
#' @param future_design Horizon-by-p matrix of known future covariates.
#' @param histories Number of independent histories for each retained draw.
#' @param progress Optional callback after each 100 retained draws.
#' @return Per-unit predictive mean and four interleaved Monte Carlo batch means.
#' @details No held-out response is accepted. Each history simulates future states
#' and aggregate Poisson counts, then updates the guide. Rao-Blackwellized
#' one-step means are accumulated before the next state/response is sampled.
#' The caller supplies and records a separate predictive RNG seed.
#' @export
gmde_dynamic_predict <- function(fit,future_design,histories=4L,progress=NULL) {
  snapshots<-fit$predictive_states; B<-nrow(fit$labels);n<-ncol(fit$labels);H<-nrow(future_design)
  if(length(snapshots)!=B || B<1L)stop("Retained predictive states required; a terminal state is insufficient.")
  if(length(histories)!=1L || !is.finite(histories) || histories<1 || histories!=floor(histories))stop("Positive integer history count required.")
  p<-ncol(future_design);G<-diag(p);batches<-array(0,c(n,H,4L));nb<-integer(4L)
  for(b in seq_len(B)) {
    x<-snapshots[[b]];pred<-matrix(0,fit$config$K,H)
    for(k in which(x$N>0L))pred[k,]<-.gmde_discount_forecast_cpp(x$theta[k,],x$mean[k,],x$covariance[[k]],
      x$d[k],as.numeric(x$N[k]),future_design,G,fit$config$adjustment,as.integer(histories))
    j<-(b-1L)%%4L+1L;nb[j]<-nb[j]+1L;batches[,,j]<-batches[,,j]+pred[fit$labels[b,],,drop=FALSE]
    if(!is.null(progress) && (b%%100L==0L || b==B))progress(b,B)
  }
  prediction<-apply(batches,c(1,2),sum)/B
  for(j in 1:4)if(nb[j]>0L)batches[,,j]<-batches[,,j]/nb[j]
  list(mean=prediction,batch_means=batches,batch_counts=nb,histories_per_draw=histories,
       retained_draws=B,uses_test_outcomes=FALSE,conditional_one_step_rao_blackwellization=TRUE)
}

.gmde_dynamic_initial_seed <- function(ctx,input,method,seed) {
  .gmde_scalar(seed,"initial_seed",1,.Machine$integer.max,TRUE)
  previous <- get(".Random.seed",.GlobalEnv)
  on.exit(assign(".Random.seed",previous,.GlobalEnv),add=TRUE)
  set.seed(seed)
  .gmde_dynamic_initialize(ctx,input,method)
}
