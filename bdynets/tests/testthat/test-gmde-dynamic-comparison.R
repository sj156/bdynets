test_that('Euclidean Matern uses the unnormalized closed-form covariance', {
 D<-as.matrix(dist(cbind(c(0,.2,1,2),c(0,.3,.1,1))))
 for(nu in c(.5,1.5)) {
  z<-sqrt(2*nu)*D/1.3
  expected<-4*if(nu==.5)exp(-z) else (1+z)*exp(-z)
  expect_equal(.gmde_matern_covariance_cpp(D,nu,2,log(1.3)),unname(expected),tolerance=1e-12)
 }
 cal<-.gmde_dynamic_euclidean_center(D,3,2L)
 expect_equal(cal$median_neighbor_correlation,.5,tolerance=1e-10)
 C<-.gmde_matern_covariance_cpp(D,3,1,cal$u0)
 spec<-.gmde_dynamic_euclidean(list(euclidean_distance=D),3,1,cal$u0)
 field<-matrix(sin(1:12),4,3)
 dense<--1.5*as.numeric(determinant(C,logarithm=TRUE)$modulus)-sum(field*solve(C,field))/2
 expect_equal(.gmde_dynamic_gp_density(field,spec),dense,tolerance=1e-9)
})
test_that('Potts proposal ratio cancels the exact pairwise prior including empty classes', {
 W<-matrix(c(0,.7,0,.7,0,.2,0,.2,0),3,3);Z<-c(1L,1L,2L);K<-3L;beta<-4
 for(i in 1:3) for(b in 1:3) {
  nextZ<-Z;nextZ[i]<-b
  target<-function(z)beta*sum(W[upper.tri(W)]*outer(z,z,'==')[upper.tri(W)])
  scores<-.gmde_potts_scores_cpp(W,Z,as.integer(i),K,beta)
  expect_equal(scores[b]-scores[Z[i]],target(nextZ)-target(Z),tolerance=1e-13)
  expect_equal(scores,.gmde_potts_scores_cpp(W,nextZ,as.integer(i),K,beta),tolerance=0)
 }
})
test_that('predictive recursion matches replayed guides and analytic one-step means', {
 F<-cbind(1,sin(1:3),cos(1:3));G<-diag(3);C<-diag(c(.1,.03,.02));m<-c(log(25),.01,-.02);theta<-m+c(.01,-.01,0);d<-.98;N<-4;c0<-log(2)
 reference<-function() {
  th<-theta;S<-numeric();out<-numeric(3)
  for(t in 1:3) {
   g<-gmde_discount_guide(c(S,0),N,F[1:t,,drop=FALSE],G,m,C,d,c0)
   W<-g$innovation_covariance[,,t];a<-G%*%th
   out[t]<-exp(sum(F[t,]*a)+drop(t(F[t,])%*%W%*%F[t,])/2)
   th<-drop(a+t(chol(W))%*%rnorm(3));S<-c(S,rpois(1,N*exp(sum(F[t,]*th))))
  };out
 }
 set.seed(101);expected<-reference()
 set.seed(101);actual<-.gmde_discount_forecast_cpp(theta,m,C,d,N,F,G,c0,1L)
 expect_equal(actual,expected,tolerance=1e-10)
 g<-gmde_discount_guide(0,N,F[1,,drop=FALSE],G,m,C,d,c0)
 one<-exp(sum(F[1,]*theta)+drop(t(F[1,])%*%g$innovation_covariance[,,1]%*%F[1,])/2)
 set.seed(102);expect_equal(.gmde_discount_forecast_cpp(theta,m,C,d,N,F[1,,drop=FALSE],G,c0,4L)[1],one,tolerance=1e-12)
})
