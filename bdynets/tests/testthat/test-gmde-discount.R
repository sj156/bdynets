# All fixtures are deterministic numerical checks, not calibrated study priors.
guide_reference <- function(S,N,F,G,m,C,d,c) {
  T <- nrow(F);p <- ncol(F)
  inv <- function(v) exp(uniroot(function(x) log(trigamma(exp(x)))-log(v),
                               c(-40,40),tol=1e-13)$root)
  out <- list(mean=matrix(0,T,p),innovation_covariance=array(0,c(p,p,T)),
              covariance=array(0,c(p,p,T)),discount=numeric(T),information=numeric(T))
  for(t in seq_len(T)) {
    a <- drop(G%*%m);P <- G%*%C%*%t(G)
    iota <- inv(drop(crossprod(F[t,],P%*%F[t,])))
    delta <- d+(1-d)*exp(-c*iota)
    R <- P/delta;W <- (1/delta-1)*P
    f <- sum(F[t,]*a);v <- drop(crossprod(F[t,],R%*%F[t,]))
    if(N>0) {
      alpha <- inv(v);beta <- exp(digamma(alpha)-f)
      g <- digamma(alpha+S[t])-log(beta+N);vp <- trigamma(alpha+S[t])
      gain <- drop(R%*%F[t,])/v
      m <- a+gain*(g-f);C <- R+tcrossprod(gain)*(vp-v)
    } else {m <- a;C <- R}
    out$mean[t,] <- m;out$covariance[,,t] <- C
    out$innovation_covariance[,,t] <- W
    out$discount[t] <- delta;out$information[t] <- iota
  }
  out
}

test_that("inverse trigamma agrees with the independent special-function identity", {
  v <- 10^seq(-8,8,length.out=33)
  x <- .gmde_inverse_trigamma_cpp(v,1e-12,200L)
  expect_lt(max(abs(log(trigamma(x)/v))),2e-11)
  expect_equal(.gmde_inverse_trigamma_cpp(pi^2/6,1e-12,200L),1,tolerance=1e-11)
})

test_that("three-dimensional guide agrees with an independent R recursion", {
  F <- cbind(1,sin(1:7),cos(1:7));G <- matrix(c(1,.02,0,0,.97,.01,.01,0,.96),3)
  C <- matrix(c(.7,.02,.03,.02,.3,.01,.03,.01,.2),3)
  S <- c(3,0,7,2,4,0,6);m <- c(.2,-.1,.05)
  out <- gmde_discount_guide(S,2,F,G,m,C,.94,.6)
  ref <- guide_reference(S,2,F,G,m,C,.94,.6)
  for(n in names(ref))expect_equal(out[[n]],ref[[n]],tolerance=2e-10,info=n)
  expect_equal(out$predictor_variance,
               trigamma(out$gamma_shape),tolerance=2e-11)
})

test_that("scalar one-step update has the analytic log-Gamma moments", {
  out <- gmde_discount_guide(4,2,matrix(1),matrix(1),0,matrix(1),.8,.5)
  v <- out$predictor_variance[1];a <- out$gamma_shape[1]
  expect_equal(out$mean[1,1],digamma(a+4)-log(exp(digamma(a))+2),tolerance=1e-11)
  expect_equal(out$covariance[1,1,1],trigamma(a+4),tolerance=1e-11)
  expect_gt(v,1)
})

test_that("zero counts and empty classes preserve their distinct update rules", {
  F <- cbind(1,sin(1:6),cos(1:6));C <- diag(c(1,.5,.5))
  empty <- gmde_discount_guide(rep(0,6),0,F,diag(3),c(1,0,0),C,.95,.5)
  low <- gmde_discount_guide(rep(0,6),1,F,diag(3),c(1,0,0),C,.95,.5)
  high <- gmde_discount_guide(rep(0,6),20,F,diag(3),c(1,0,0),C,.95,.5)
  expect_equal(empty$mean,matrix(rep(c(1,0,0),each=6),6))
  expect_true(all(empty$information>0 & empty$discount<1))
  expect_equal(low$covariance,high$covariance,tolerance=1e-12)
  expect_equal(low$covariance,empty$covariance,tolerance=1e-12)
  expect_lt(high$updated_predictor_mean[1],low$updated_predictor_mean[1])
})

test_that("innovations are causal and independent of the sampled state path", {
  F <- cbind(1,sin(1:6),cos(1:6));args <- list(exposure=2,design=F,
    transition=diag(3),initial_mean=c(0,0,0),initial_covariance=diag(3),baseline=.93,adjustment=.5)
  a <- do.call(gmde_discount_guide,c(list(sums=c(1,2,3,4,5,6)),args))
  b <- do.call(gmde_discount_guide,c(list(sums=c(1,2,30,4,5,6)),args))
  expect_equal(a$innovation_covariance[,,1:3],b$innovation_covariance[,,1:3],tolerance=0)
  expect_gt(max(abs(a$innovation_covariance[,,4]-b$innovation_covariance[,,4])),1e-6)
})

test_that("transition density matches an independent solve and determinant", {
  theta <- matrix(seq_len(18)/30,6,3);G <- diag(c(.98,1,.95))
  W <- array(0,c(3,3,5))
  for(t in 1:5)W[,,t] <- diag(c(.2,.3,.4))+(t/100)*matrix(1,3,3)
  expected <- sum(vapply(1:5,function(t) {
    e <- theta[t+1,]-G%*%theta[t,]
    -.5*(as.numeric(determinant(W[,,t],logarithm=TRUE)$modulus)+drop(t(e)%*%solve(W[,,t],e)))
  },numeric(1)))
  expect_equal(gmde_transition_log_density(theta,G,W),expected,tolerance=1e-12)
})

test_that("invalid inputs fail without silent repairs", {
  args <- list(sums=c(0,1),exposure=1,design=matrix(1,2,1),transition=matrix(1),
    initial_mean=0,initial_covariance=matrix(1),baseline=.95,adjustment=.5)
  expect_error(do.call(gmde_discount_guide,modifyList(args,list(exposure=0))),"empty class")
  expect_error(do.call(gmde_discount_guide,modifyList(args,list(baseline=1))),"baseline")
  expect_error(do.call(gmde_discount_guide,modifyList(args,list(initial_covariance=matrix(-1)))),"positive definite")
  expect_error(do.call(gmde_discount_guide,modifyList(args,list(design=matrix(0,2,1)))),"positive variance")
  expect_error(do.call(gmde_discount_guide,modifyList(args,list(adjustment=1e-300))),"floating-point boundary")
  expect_error(.gmde_inverse_trigamma_cpp(1.2,1e-12,1L),"iteration|converge|limit")
})

test_that("deterministic guide and density calls do not create or consume RNG state", {
  before <- if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))get(".Random.seed",.GlobalEnv) else NULL
  a <- gmde_discount_guide(c(1,2),1,matrix(1,2,1),matrix(1),0,matrix(1),.9,.5)
  gmde_transition_log_density(matrix(c(0,.1,.2),3,1),matrix(1),a$innovation_covariance)
  after <- if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))get(".Random.seed",.GlobalEnv) else NULL
  expect_identical(before,after)
})
