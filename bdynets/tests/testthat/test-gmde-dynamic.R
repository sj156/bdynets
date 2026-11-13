# Independent dense Gaussian and complete-target checks; no random draws.
dynamic_fixture <- function() {
  F <- cbind(1,sin(1:4),cos(1:4));p<-3L;T<-4L
  G<-matrix(c(.9,.02,0,.01,.95,.03,0,.01,1),p)
  W<-array(0,c(p,p,T));for(t in 1:T) W[,,t]<-diag(c(.1,.2,.15))+t/100
  list(F=F,G=G,W=W,m=c(.2,-.1,.1),C=diag(c(.5,.3,.2)),
    a=c(1e-12,2,3,400),b=c(.1,-.2,.3,1),state=matrix(seq_len((T+1)*p)/30,T+1,p))
}

dynamic_dense <- function(f) {
  p<-ncol(f$F);T<-nrow(f$F);P<-matrix(0,(T+1)*p,(T+1)*p);b<-numeric(nrow(P))
  at<-function(t)t*p+seq_len(p)
  P[at(0),at(0)]<-solve(f$C);b[at(0)]<-solve(f$C,f$m)
  for(t in seq_len(T)) {
    B<-matrix(0,p,(T+1)*p);B[,at(t)]<-diag(p);B[,at(t-1)]<--f$G
    P<-P+crossprod(B,solve(f$W[,,t],B))
    P[at(t),at(t)]<-P[at(t),at(t)]+f$a[t]*tcrossprod(f$F[t,])
    b[at(t)]<-b[at(t)]+f$b[t]*f$F[t,]
  }
  list(P=P,b=b)
}

test_that("multivariate FFBS matches dense joint Gaussian for every boundary case", {
  f<-dynamic_fixture();joint<-dynamic_dense(f);p<-3L;T<-4L
  call<-function(s,e,z) .gmde_information_ffbs_cpp(f$state,f$F,f$a,f$b,f$G,f$m,f$C,f$W,s,e,z)
  for(block in list(c(1L,4L),c(1L,2L),c(2L,3L),c(3L,4L))) {
    s<-block[1];e<-block[2];times<-seq.int(if(s==1L)0L else s,e)
    indices<-unlist(lapply(times,function(t)t*p+seq_len(p)));outside<-setdiff(seq_len(nrow(joint$P)),indices)
    nat<-joint$b[indices]
    if(length(outside))nat<-nat-joint$P[indices,outside,drop=FALSE]%*%as.vector(t(f$state))[outside]
    target_cov<-solve(joint$P[indices,indices]);target_mean<-drop(target_cov%*%nat)
    zero<-matrix(0,T+1,p);base<-call(s,e,zero)
    expect_equal(as.vector(t(base))[indices],target_mean,tolerance=1e-10)
    expect_equal(as.vector(t(base))[outside],as.vector(t(f$state))[outside],tolerance=0)
    transform<-matrix(0,length(indices),length(indices))
    for(j in seq_along(indices)) {
      z<-zero;at<-indices[j]-1L;z[at%/%p+1L,at%%p+1L]<-1
      transform[,j]<-as.vector(t(call(s,e,z)-base))[indices]
    }
    expect_equal(tcrossprod(transform),target_cov,tolerance=1e-10)
  }
})

test_that("Poisson correction equals independent normalized density ratios", {
  S<-c(0,1,5,1000);r<-c(2,4,20,4000);N<-3;old<-c(-5,0,1,5);new<-old+c(.1,-.1,.2,-.02)
  score<-function(eta)sum(dpois(S,N*exp(eta),log=TRUE)-dnbinom(S,size=r,mu=N*exp(eta),log=TRUE))
  expect_equal(.gmde_poisson_block_ratio_cpp(S,N,r,old,new),score(new)-score(old),tolerance=1e-9)
  expect_equal(.gmde_poisson_block_ratio_cpp(S,N,r,old,old),0,tolerance=0)
  expect_equal(.gmde_poisson_block_ratio_cpp(S,N,r,old,new),
    -.gmde_poisson_block_ratio_cpp(S,N,r,new,old),tolerance=1e-10)
})

test_that("raw graph covariance and full K Gaussian density match dense forms", {
  A<-matrix(0,5,5);A[cbind(1:4,2:5)]<-1;A<-A+t(A);D<-A*.7
  for(method in c("C","Exp-W","SE-W")) {
    weight<-switch(method,C=1,`Exp-W`=exp(-.7/1.3),`SE-W`=exp(-(.7/1.3)^2))
    expected<-diag(rowSums(A*weight))-A*weight
    expect_equal(.gmde_raw_laplacian_cpp(A,D,method,1.3),expected,tolerance=1e-15)
    spec<-.gmde_dynamic_spectrum(list(adjacency=A,edge_lengths=D),method,1.3,3,1,log(1.5))
    covariance<-spec$vectors%*%diag(spec$variance)%*%t(spec$vectors)
    B<-6/1.5^2*diag(5)+expected
    expect_equal(covariance,solve(B%*%B%*%B),tolerance=1e-12)
    field<-matrix(sin(1:20)/10,5,4)
    expected_q<--2*as.numeric(determinant(covariance,logarithm=TRUE)$modulus)-sum(field*solve(covariance,field))/2
    expect_equal(.gmde_dynamic_gp_density(field,spec),expected_q,tolerance=1e-10)
  }
  expect_error(.gmde_raw_laplacian_cpp(A,D,"Exp-W",1e-6),"underflow")
})

test_that("compiled emission and softmax calculations retain all categories", {
  Y<-matrix(1:12,3,4);eta<-matrix(sin(1:20),4,5)
  ref<-vapply(seq_len(5),function(k)drop(Y%*%eta[,k])-sum(exp(eta[,k])),numeric(3))
  expect_equal(.gmde_poisson_emission_cpp(Y,eta),ref,tolerance=1e-12)
  field<-matrix(c(1000,1001,999,1002,1000,999),3,2);Z<-c(1L,2L,1L)
  expect_equal(.gmde_softmax_loglik_cpp(field,Z),.gmde_allocation_loglik(field,Z),tolerance=1e-12)
})

test_that("label replay and logit-discount ratios equal complete target ratios", {
  F<-cbind(1,sin(1:5),cos(1:5));Y<-matrix(c(1,2,3,4,2,2,1,4,3,2,1,3,2,1,2),3,5)
  ctx<-list(F=F,G=diag(3),T=5L,p=3L,config=list(initial_mean=c(0,0,0),
    initial_covariance=diag(c(1,.5,.5)),adjustment=log(2),discount_prior=c(49,1)))
  Z<-c(1L,1L,2L);K<-3L;d<-c(.96,.98,.99);theta<-lapply(1:K,function(k)matrix(sin((1:18)+k)/10,6,3))
  suff<-.gmde_sufficient(Y,Z,K)
  guides<-lapply(1:K,function(k).gmde_dynamic_guide(suff$S[k,],suff$N[k],d[k],ctx))
  density<-function(th,g)sum(vapply(1:5,function(t){e<-th[t+1,]-th[t,];W<-g$innovation_covariance[,,t];
    -.5*(as.numeric(determinant(W,logarithm=TRUE)$modulus)+drop(crossprod(e,solve(W,e))))},numeric(1)))
  field<-matrix(sin(1:9)/10,3,3);eta<-vapply(theta,function(x)rowSums(x[-1,]*F),numeric(5))
  emissions<-.gmde_poisson_emission_cpp(Y,eta);logq<-.gmde_log_softmax(field+emissions)
  full<-function(labels){ss<-.gmde_sufficient(Y,labels,K);gg<-lapply(1:K,function(k).gmde_dynamic_guide(ss$S[k,],ss$N[k],d[k],ctx));
    sum(vapply(1:K,function(k)density(theta[[k]],gg[[k]]),numeric(1)))+
    .gmde_allocation_loglik(field,labels)+sum(emissions[cbind(1:3,labels)])}
  # Move the sole member of class 2 into an empty class: both empty boundaries.
  out<-.gmde_dynamic_allocation_candidate(3,2,3,suff$S,suff$N,Y,d,theta,guides,ctx)
  nextZ<-Z;nextZ[3]<-3L
  expect_equal(out$ratio,full(nextZ)-full(Z)+logq[3,2]-logq[3,3],tolerance=1e-10)
  dd<-.gmde_dynamic_discount_candidate(theta[[1]],suff$S[1,],suff$N[1],d[1],.97,guides[[1]],ctx)
  full_d<-function(x,g)density(theta[[1]],g)+dbeta(x,49,1,log=TRUE)+log(x)+log1p(-x)
  expect_equal(dd$ratio,full_d(.97,dd$guide)-full_d(d[1],guides[[1]]),tolerance=1e-10)
})
