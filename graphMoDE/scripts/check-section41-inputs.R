# Read-only verification of the existing two-dataset Section 4.1 pilot.
# Usage: Rscript check-section41-inputs.R ARCHIVE_ROOT REPORT.json
# This script reads no chains, draws no randomness and never launches a fit.
args <- commandArgs(TRUE)
stopifnot(length(args)==2L,dir.exists(args[1]),!file.exists(args[2]))
registration <- jsonlite::fromJSON(file.path(args[1],"registration.json"))
stopifnot(registration$plan$n==121L,registration$plan$Kfit==10L,
          registration$plan$Ktrue==5L,registration$plan$amplitude==.05,
          identical(as.numeric(registration$plan$boundary_gap_factors),c(3,4,3,2)))
records <- list()
for(dataset in c("D01","D02")) {
  folder <- file.path(args[1],dataset)
  files <- file.path(folder,c("blind.rds","design.rds","evaluation-only.rds"))
  before <- vapply(files,digest::digest,character(1),algo="sha256",file=TRUE)
  x <- readRDS(files[1])$input
  design <- readRDS(files[2]);evaluation <- readRDS(files[3])
  stopifnot(identical(dim(x$Y),c(121L,168L)),identical(dim(evaluation$Y),c(121L,48L)),
    all(is.finite(x$Y)),all(x$Y>=0 & x$Y==round(x$Y)),
    all(is.finite(evaluation$Y)),all(evaluation$Y>=0 & evaluation$Y==round(evaluation$Y)),
    identical(x$ids,evaluation$ids),identical(x$ids,design$road$ids[design$units]),
    identical(as.integer(table(design$road$partition)),c(30L,20L,21L,20L,30L)))
  A <- x$adjacency
  expected <- outer(x$ids,x$ids,function(a,b)as.numeric(abs(a-b)==1))
  stopifnot(identical(dim(A),c(121L,121L)),all(A==expected),sum(A)==240,
            all(diag(A)==0),all(A==t(A)),all(x$edge_lengths[A==0]==0),
            all(x$edge_lengths[A==1]>0),all(x$edge_lengths==t(x$edge_lengths)))
  expected_lengths <- abs(outer(design$road$s[design$units],design$road$s[design$units],"-"))*A
  stopifnot(max(abs(x$edge_lengths-expected_lengths))<1e-12)
  F <- cbind(1,sin(2*pi*(1:216)/24),cos(2*pi*(1:216)/24))
  stopifnot(max(abs(x$Fmat-F[1:168,]))<1e-12,max(abs(x$Ffuture-F[169:216,]))<1e-12)
  truth <- matrix(0,5,216)
  for(k in 1:5) {
    phase <- 2*pi*(k-1)/5
    intercept <- log(25)-log(mean(exp(.05*sin(2*pi*(1:24)/24+phase))))
    truth[k,] <- exp(intercept+.05*sin(2*pi*(1:216)/24+phase))
  }
  region <- design$road$partition[design$units]
  expected_lambda <- truth[design$profiles[region],,drop=FALSE]
  stopifnot(max(abs(expected_lambda-evaluation$lambda))<1e-11,
            identical(as.integer(evaluation$Z),as.integer(region)))
  after <- vapply(files,digest::digest,character(1),algo="sha256",file=TRUE)
  stopifnot(identical(before,after))
  records[[dataset]] <- list(files=setNames(as.list(before),basename(files)),
    input_object_sha256=digest::digest(x,algo="sha256"),n=121L,edges=120L,
    train=168L,test=48L,group_sizes=c(30,20,21,20,30),
    edge_distance_range=range(x$edge_lengths[A==1]),
    edge_distance_median=median(x$edge_lengths[A==1]),
    maximum_profile_error=max(abs(expected_lambda-evaluation$lambda)),passed=TRUE)
}
report <- list(source_role="historical input data only; no fit or checkpoint reuse",
  manuscript_basis="Section 4.1, 2026-11-13",datasets=records,
  n=121L,K=10L,p=3L,amplitude=.05,period=24L,cycle_mean=25,
  no_data_regeneration=TRUE,no_sampling=TRUE,original_files_unchanged=TRUE)
jsonlite::write_json(report,args[2],pretty=TRUE,auto_unbox=TRUE,digits=16)
cat("Section 4.1 input checks passed for D01 and D02.\n")
