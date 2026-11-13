# Deterministic arithmetic and geometry review; no observations enter calibration.
# Usage: Rscript evaluate-section41-priors.R ARCHIVE_ROOT REPOSITORY_ROOT NEW_OUTPUT
args <- commandArgs(TRUE)
stopifnot(length(args)==3L, dir.exists(args[1]), dir.exists(args[2]), !file.exists(args[3]))
w <- normalizePath(args[2],mustWork=TRUE)
out <- args[3]
dir.create(out,recursive=TRUE)

Rcpp::sourceCpp(file.path(w,'bdynets/src/gmde-discount.cpp'),cacheDir=file.path(tempdir(),'graphmode-prior-review-rcpp'))
rows <- list()
for(a in c(19,49,99)) {
 q <- qbeta(c(.025,.5,.975),a,1)
 rows[[length(rows)+1]] <- data.frame(shape1=a,shape2=1,mean=a/(a+1),
   q025=q[1],median=q[2],q975=q[3],
   baseline_inflation_at_median=1/q[2]-1,
   discount_half_weight_steps_at_median=log(.5)/log(q[2]))
}
write.csv(do.call(rbind,rows),file.path(out,'discount-prior-candidates.csv'),row.names=FALSE)
iota0 <- .gmde_inverse_trigamma_cpp(1.5,1e-12,200L)
threshold <- c(.5,1,2,5)
cs <- data.frame(information_half_protection=threshold,c=log(2)/threshold,
                protection_at_initial_variance_1_5=exp(-log(2)/threshold*iota0))
write.csv(cs,file.path(out,'adjustment-candidates.csv'),row.names=FALSE)
records <- list()
for(dataset in c('D01','D02')) {
 x <- readRDS(file.path(args[1],dataset,'blind.rds'))$input
 edge <- which(upper.tri(x$adjacency)&x$adjacency==1,arr.ind=TRUE)
 for(kind in c('Exp-reference','Connectivity')) {
  A <- if(kind=='Connectivity')x$adjacency else x$adjacency*exp(-x$edge_lengths)
  eig <- eigen(diag(rowSums(A))-A,symmetric=TRUE)
  stopifnot(min(eig$values)>-1e-12)
  lambda <- pmax(eig$values,0)
  # This study-level loop evaluates the stated calibration, not an MCMC kernel.
  # Bisection uses geometry only and does not instantiate a fitted model.
  bounds <- c(-10,8)
  for(step in seq_len(200L)) {
   u <- mean(bounds)
   weights <- (6/exp(2*u)+lambda)^(-3)
   vals <- tcrossprod(sweep(eig$vectors,2,sqrt(weights),'*'))
   error <- median(cov2cor(vals)[edge])-.5
   if(abs(error)<1e-12)break
   if(error<0)bounds[1] <- u else bounds[2] <- u
  }
  stopifnot(abs(error)<1e-12)
  records[[length(records)+1]] <- data.frame(dataset=dataset,reference=kind,
   nu=3,tau=1,reference_h=if(kind=='Connectivity')NA else 1,
   kappa0=exp(u),u0=u,median_edge_correlation=median(cov2cor(vals)[edge]),
   min_score_sd=min(sqrt(diag(vals))),median_score_sd=median(sqrt(diag(vals))),
   max_score_sd=max(sqrt(diag(vals))),distance_median=median(x$edge_lengths[edge]))
 }
}
write.csv(do.call(rbind,records),file.path(out,'geometry-calibration.csv'),row.names=FALSE)
cat('Initial-information proxy for variance 1.5:',iota0,'\n')
print(do.call(rbind,rows));print(cs);print(do.call(rbind,records))
