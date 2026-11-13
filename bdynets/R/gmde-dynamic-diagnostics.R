# Development summaries use label-invariant traces. No diagnostics are suppressed.
.gmde_adjusted_rand <- function(a,b) {
  tab <- table(a,b); pairs <- function(x)x*(x-1)/2
  total <- pairs(sum(tab)); ai <- sum(pairs(rowSums(tab))); bi <- sum(pairs(colSums(tab)))
  expected <- ai*bi/total; upper <- (ai+bi)/2
  if(upper==expected) return(as.numeric(identical(outer(a,a,"=="),outer(b,b,"=="))))
  (sum(pairs(tab))-expected)/(upper-expected)
}

#' Summarize completed development chains without declaring convergence
#' @param fits Completed chains from gmde_dynamic_fit with identical settings.
#' @return A list containing per-chain summaries, label-invariant R-hat and ESS,
#' and pairwise posterior similarity discrepancies.
#' @export
gmde_dynamic_diagnostics <- function(fits) {
  if(length(fits)<2L)stop("At least two completed chains required.")
  if(!requireNamespace("posterior",quietly=TRUE))stop("The posterior package is required.")
  kept <- lapply(fits,function(x)x$trace[seq.int(x$config$warmup+1L,nrow(x$trace)),,drop=FALSE])
  if(length(unique(vapply(kept,nrow,integer(1))))!=1L)stop("Incompatible chain lengths.")
  variables <- c("occupied","log_likelihood","log_kappa","mean_occupied_discount")
  if(fits[[1]]$method!="C")variables<-c(variables,"log_h")
  rows <- lapply(variables,function(v){
    x<-vapply(kept,function(a)a[,v],numeric(nrow(kept[[1]])))
    constant<-apply(x,2,var)==0
    # Constants are reported, never converted to R-hat = 1.
    rhat<-posterior::rhat(x);bulk<-posterior::ess_bulk(x);tail<-posterior::ess_tail(x)
    data.frame(variable=v,rhat=rhat,ess_bulk=bulk,ess_tail=tail,
      constant_chains=sum(constant),passed=is.finite(rhat)&&is.finite(bulk)&&is.finite(tail)&&
        !any(constant)&&rhat<1.01&&bulk>=400&&tail>=400)
  })
  chain <- do.call(rbind,lapply(seq_along(fits),function(j){
    x<-kept[[j]]
    data.frame(chain=j,initial_groups=fits[[j]]$config$initial_groups,
      seconds=fits[[j]]$elapsed,occupied_min=min(x[,"occupied"]),occupied_max=max(x[,"occupied"]),
      occupied_mean=mean(x[,"occupied"]),state_acceptance=mean(x[,"state_acceptance"]),
      discount_acceptance=mean(x[,"discount_acceptance"]),
      allocation_proposals=sum(x[,"allocation_proposals"]),allocation_moves=sum(x[,"allocation_moves"]),
      kappa_acceptance=mean(x[,"kappa_accepted"]),h_acceptance=mean(x[,"h_accepted"]),
      mean_occupied_discount=mean(x[,"mean_occupied_discount"]))
  }))
  pairs<-combn(seq_along(fits),2)
  psm<-data.frame(chain_a=pairs[1,],chain_b=pairs[2,],
    rms=vapply(seq_len(ncol(pairs)),function(j){a<-fits[[pairs[1,j]]]$psm;b<-fits[[pairs[2,j]]]$psm;
      sqrt(mean((a[upper.tri(a)]-b[upper.tri(b)])^2))},numeric(1)))
  list(diagnostics=do.call(rbind,rows),chains=chain,psm_pairs=psm,
    thresholds=list(rhat=1.01,ess_bulk=400,ess_tail=400),
    convergence_certified=FALSE,role="Development diagnostics; passing individual checks is not a formal release.")
}
