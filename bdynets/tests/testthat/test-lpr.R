test_that('centering increments match full predictor recomputation on sparse edges', {
  set.seed(55); n<-7L; d<-2L; edges<-rbind(c(1,2),c(2,4),c(3,7),c(1,6))
  data<-lpr_data(c(0,2,1,4),edges,n,c(.2,1,3,2))
  state<-list(raw=matrix(rnorm(n*d),n,d),b=rnorm(n),mu=.4)
  for(k in c(1L,5L,7L)) {
    delta<-c(.1,-.08); new<-state; new$raw[k,]<-new$raw[k,]+delta
    expect_equal(bdynets:::.lpr_eta(data,new)-bdynets:::.lpr_eta(data,state),
      bdynets:::.lpr_z_change(bdynets:::.lpr_center(state$raw),k,delta,edges),tolerance=1e-12)
    new<-state; new$b[k]<-new$b[k]+.2
    expect_equal(bdynets:::.lpr_eta(data,new)-bdynets:::.lpr_eta(data,state),
      bdynets:::.lpr_b_change(n,k,.2,edges),tolerance=1e-12)
  }
  eta<-bdynets:::.lpr_eta(data,state); h<-c(.01,-.1,.3,.04)
  expected<-sum(data$y*h-data$exposure*exp(eta)*expm1(h))
  expect_equal(bdynets:::.lpr_loglik(data,eta+h)-bdynets:::.lpr_loglik(data,eta),expected,tolerance=1e-10)
})

test_that('alignment preserves interactions and does not fit scale', {
  set.seed(4); z<-matrix(rnorm(20),10,2); z<-scale(z,scale=FALSE)
  q<-matrix(c(0,1,1,0),2,2)
  expect_equal(unname(lpr_align(z%*%q,z)),unname(z),ignore_attr=TRUE,tolerance=1e-12)
  aligned<-lpr_align(2*z,z)
  expect_equal(sum(aligned^2),4*sum(z^2),tolerance=1e-12)
})

test_that('checkpoint continuation equals an uninterrupted run across tuning boundary', {
  data<-lpr_data(c(0,2,1,0,3,1),t(combn(4,2)),4L)
  set.seed(777); original_rng<-.Random.seed
  one<-lpr_fit(data,sweeps=160,warmup=110,thin=2,seed=12)
  first<-lpr_fit(data,sweeps=90,warmup=110,thin=2,seed=12)
  path<-tempfile(); lpr_save(first,path); restored<-lpr_read(path)
  two<-lpr_continue(restored,70)
  expect_equal(one$draws,two$draws,tolerance=1e-10)
  expect_equal(one$checkpoint,two$checkpoint,tolerance=1e-10)
  expect_identical(.Random.seed,original_rng)
  expect_true(all(rowSums(one$checkpoint$state$raw^2)<=1.5^2))
  expect_equal(rowSums(one$draws$alpha),rep(0,nrow(one$draws$alpha)),tolerance=1e-12)
  altered<-two; altered$config$radius<-2
  expect_error(lpr_continue(altered,5),"target changed")
  one$checkpoint<-NULL
  expect_error(lpr_continue(one,5),'No complete restart')
})

test_that('input checks reject invalid support and exposure', {
  expect_error(lpr_data(1,matrix(c(1,1),1,2),2),'edge')
  expect_error(lpr_data(1,matrix(c(1,2),1,2),2,0),'exposures')
  expect_error(lpr_data(2,matrix(c(1,2),1,2),2,family='bernoulli'),'binary')
})
