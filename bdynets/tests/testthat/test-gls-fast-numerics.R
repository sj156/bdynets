test_that("weighted Gram matrices retain collinearity, zero weights and priors", {
  set.seed(620)
  Z <- matrix(rnorm(150), 15, 10)
  Z[, 4] <- Z[, 3]; Z[, 8] <- 0
  w <- c(0, 1e-10, exp(seq(-3, 3, length.out = 13)))
  prior <- exp(seq(-4, 4, length.out = 10))
  expected <- crossprod(Z, Z * w) + diag(prior)
  actual <- bdynets:::.gls_weighted_precision(Z, w, prior)
  expect_equal(actual, expected, tolerance = 1e-12)
  expect_true(isSymmetric(actual))
  current <- rnorm(10); rhs <- rnorm(10)
  for (step in c(.002, .8, 1)) {
    set.seed(71)
    mu <- as.numeric(bdynets:::solve_spd(expected) %*% rhs)
    old_draw <- bdynets:::rmvn_precision(expected, rhs)
    expected_draw <- mu + sqrt(1-step^2)*(current-mu) + step*(old_draw-mu)
    set.seed(71)
    actual_draw <- bdynets:::.gls_pcn_precision(actual, rhs, current, step)
    expect_equal(actual_draw, expected_draw, tolerance = 1e-10)
  }
})

test_that("one-factor precision draws have the prescribed mean and covariance", {
  P <- matrix(c(3, .8, .8, 2), 2)
  rhs <- c(.3, -.5); current <- c(.2, -.1); step <- .8
  V <- solve(P + diag(1e-10, 2)); mu <- as.numeric(V %*% rhs)
  set.seed(621)
  draws <- replicate(8000, bdynets:::.gls_pcn_precision(P, rhs, current, step))
  expect_equal(rowMeans(draws), mu + sqrt(1-step^2)*(current-mu), tolerance = .025)
  expect_equal(cov(t(draws)), step^2 * V, tolerance = .025)
})

test_that("cached designs preserve source order, duplicates and multiple factors", {
  set.seed(622)
  X <- matrix(rnorm(30), 10, 3); Q <- matrix(numeric(), 10, 0)
  pairs <- data.frame(target = c(1L,1L,1L,2L,2L,3L), source = c(3L,1L,3L,2L,1L,2L))
  for (r in 1:2) for (static in c(TRUE, FALSE)) {
    beta <- matrix(rnorm(10*r), 10, r)
    ids <- split(seq_len(nrow(pairs)), pairs$target)
    cache <- bdynets:::.gls_target_design_cache(X,Q,beta,pairs,ids,static)
    for (ii in ids) {
      src <- pairs$source[ii]
      expected <- cbind(Intercept=1,Q,if (static) X[,src,drop=FALSE],
        do.call(cbind,lapply(src,function(j) X[,j]*beta)))
      expect_equal(bdynets:::.gls_target_design(cache,src),expected)
    }
  }
  pairs <- expand.grid(source=1:3,target=1:3)
  cache <- bdynets:::.gls_target_design_cache(X,Q,beta,pairs,
    split(seq_len(nrow(pairs)),pairs$target))
  expect_false(is.null(cache$shared))
})

test_that("native reference agrees with bracketed R roots and covariance paths", {
  cfg <- gls_control()
  set.seed(623)
  Y <- matrix(rpois(60*7, 8), 60, 7)
  H <- array(rnorm(60*7,sd=.3),c(60,7,1))
  offset <- matrix(rnorm(60*7,log(8),.4),60,7)
  for (d in c(.5, .9, .995, .9999)) {
    expected <- bdynets:::.gls_reference_path_R(Y,H,offset,d,cfg,TRUE)
    actual <- bdynets:::reference_path(Y,H,offset,d,cfg,TRUE)
    expect_equal(actual$mean_path,expected$mean_path,tolerance=2e-9)
    expect_equal(actual$W,expected$W,tolerance=2e-9)
    expect_equal(actual$C,expected$C,tolerance=2e-9)
    beta <- matrix(rnorm(60),60); attr(beta,"initial") <- .3
    expect_equal(bdynets:::factor_log_density(beta,actual),
      bdynets:::factor_log_density(beta,expected),tolerance=2e-9)
  }
  cfg$reference_backend <- "R"
  expect_identical(bdynets:::reference_path(Y,H,offset,.95,cfg,TRUE),
    bdynets:::.gls_reference_path_R(Y,H,offset,.95,cfg,TRUE))
  expect_error(gls_control(reference_backend="unknown"),"reference_backend")
})

test_that("native scalar roots retain flat, tiny and extreme-offset behavior", {
  cases <- list(
    list(y=c(0,0), h=c(0,0), offset=c(0,0), m=2, C=1),
    list(y=c(0,1), h=c(1e-12,-1e-12), offset=c(-600,-500), m=-3, C=1),
    list(y=c(0,0), h=c(.1,-.2), offset=c(-50,-60), m=4, C=1e-12),
    list(y=c(100000,2),h=c(2,-1),offset=c(6,1),m=0,C=10),
    list(y=c(0,0),h=c(1,-1),offset=c(500,-500),m=0,C=1))
  for (f in cases) {
    old <- do.call(bdynets:::.gls_reference_step_R,c(f,list(discount=.95)))
    fast <- do.call(bdynets:::reference_step,c(f,list(discount=.95)))
    expect_equal(fast,old,tolerance=2e-8)
    h <- f$h; b <- fast$m; R <- f$C/.95
    scaled_error <- abs(((b-f$m)/R + sum(h*(exp(f$offset+h*b)-f$y))) /
      (1/R+sum(h^2*exp(f$offset+h*b))))
    expect_lt(scaled_error,1e-9)
  }
  expect_error(bdynets:::reference_step(1,NA_real_,0,0,1,.95),"finite")
  expect_error(bdynets:::reference_step(1,1,0,0,1,1),"Invalid reference")
})

test_that("native and R references give the same MH decisions", {
  set.seed(624); cfg <- gls_control()
  Y <- matrix(rpois(24*4,5),24,4)
  H <- array(rnorm(24*4,sd=.2),c(24,4,1)); offset <- matrix(log(5),24,4)
  beta <- matrix(rnorm(24,sd=.1),24);attr(beta,"initial") <- 0
  ref <- bdynets:::reference_path(Y,H,offset,.95,cfg)
  for (s in 1:12) {
    proposed <- H + rnorm(length(H),sd=.01)
    set.seed(800+s)
    a <- bdynets:::accept_reference(Y,proposed,offset,beta,.95,ref,cfg)
    cfg$reference_backend <- "R"
    set.seed(800+s)
    b <- bdynets:::accept_reference(Y,proposed,offset,beta,.95,ref,cfg)
    cfg$reference_backend <- "native"
    expect_identical(a$accepted,b$accepted)
    expect_equal(a$ref$W,b$ref$W,tolerance=2e-9)
  }
})

test_that("batched prediction designs match pointwise directed-pair products", {
  set.seed(625); k <- 4L; n <- 7L
  X <- matrix(rnorm(n*k),n,k);Q <- matrix(rnorm(n*2),n,2)
  pairs <- data.frame(source=c(1L,3L,1L,2L,4L,4L),target=c(2L,1L,2L,4L,2L,3L))
  alpha <- rnorm(k);gamma <- matrix(rnorm(k*2),k,2)
  B <- rnorm(nrow(pairs));L <- matrix(rnorm(nrow(pairs)),ncol=1)
  for (d in list(NULL,c(1,0,.2,1,0,.4))) {
    batched <- bdynets:::.gls_general_prediction_design(X,Q,pairs,alpha,gamma,B,L,d)
    study <- bdynets:::.gls_study_pair_components(X,pairs,B,L,k,1L,d)
    expect_equal(batched$H,study$H)
    expect_equal(batched$base,bdynets:::.gls_study_baseline_matrix(alpha,gamma,Q)+study$static)
    for (t in seq_len(n)) {
      H <- bdynets:::factor_design_one_time(X[t,],pairs,L,k,d)
      base <- alpha+as.numeric(gamma%*%Q[t,])+
        as.numeric(bdynets:::static_network(X[t,,drop=FALSE],pairs,B,d))
      expect_equal(batched$H[t,,1],as.numeric(H),tolerance=1e-12)
      expect_equal(batched$base[t,],base,tolerance=1e-12)
    }
  }
})

test_that("scalar study evolution vectorization preserves the original jitter", {
  set.seed(626); W <- array(exp(seq(-25,2,length.out=15)),c(1,1,15))
  beta <- matrix(rnorm(16),16,1)
  old <- 0
  for (t in 1:15) {
    z <- beta[t+1,]-beta[t,]
    old <- old-.5*(log(2*pi)+bdynets:::.gls_study_logdet_spd(W[,,t])+
      sum(z*(bdynets:::solve_spd(W[,,t])%*%z)))
  }
  expect_equal(bdynets:::.gls_study_log_factor_evolution(beta,W),old,tolerance=1e-12)
})
