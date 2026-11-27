test_that("static GLS resumes from warmup and retained samples with bounded draws", {
  ctrl <- gls_control("smoke",n_iter=12L,burn_mcmc=6L,n_chains=1L,
                      predictive_draws=100L)
  graph <- gls_grid(2)
  k <- nrow(graph$A)
  set.seed(142)
  y <- matrix(rpois(20*k,5),20,k)
  x <- log1p(rbind(y[1,,drop=FALSE],head(y,-1)))
  q <- matrix(sin(seq_len(20)),ncol=1)
  p <- gls_all_pairs(graph)
  ref <- gls_static_forecast(y,x,q,p,1:15,16:20,ctrl)
  original <- .gls_commit_chain
  fail_at <- 3L
  local_mocked_bindings(.gls_commit_chain=function(record,dir) {
    original(record,dir)
    if(record$iteration==fail_at) stop("simulated power loss")
  })
  for(t in c(3L,9L)) {
    fail_at <- t
    path <- tempfile()
    on.exit(unlink(path,recursive=TRUE),add=TRUE)
    args <- list(Y=y,X=x,covariates=q,pairs=p,train_rows=1:15,test_rows=16:20,
                 control=ctrl,checkpoint_dir=path,chunk_size=2L,checkpoint_every=3L)
    expect_error(do.call(gls_static_forecast,args),"simulated power loss")
    fail_at <- 0L
    args$resume <- TRUE
    got <- do.call(gls_static_forecast,args)
    expect_identical(got$B,ref$B)
    expect_identical(got$lambda,ref$lambda)
    expect_identical(got$chain_id,ref$chain_id)
  }
})

test_that("pooled PNAR uses chronological observed lags and a shared coefficient vector", {
  set.seed(1866)
  graph <- gls_grid(2)
  y <- matrix(rpois(100*nrow(graph$A),4),100)
  fit <- gls_pnar_forecast(y,graph,matrix(numeric(),100,0),1:75,76:100,pooled=TRUE)
  expect_equal(fit$coefficients[[1]],fit$coefficients[[2]])
  y[85:100,] <- y[85:100,]+10L
  later <- gls_pnar_forecast(y,graph,matrix(numeric(),100,0),1:75,76:100,pooled=TRUE)
  expect_identical(fit$mean[1:10,],later$mean[1:10,])
})

test_that("Poisson lasso path gives the unpenalized likelihood optimum at zero", {
  set.seed(32)
  X <- matrix(rnorm(240),80,3)
  Q <- matrix(sin(seq_len(80)),ncol=1)
  Y <- matrix(rpois(240,exp(.8+.15*X)),80,3)
  tr <- 1:60; te <- 61:80
  fit <- gls_poisson_lasso_fit(Y,X,Q,tr,penalties=c(10,0),gradient_tolerance=1e-7)
  pred <- predict(fit,X=X[te,],covariates=Q[te,,drop=FALSE],index=2)
  Z <- cbind(1,Q,X)
  for(a in 1:3) {
    glm <- stats::glm.fit(Z[tr,],Y[tr,a],family=stats::poisson())
    expect_equal(pred[,a],as.vector(exp(Z[te,]%*%glm$coefficients)),tolerance=1e-6)
  }
  expect_true(all(fit$coefficients[[1]][fit$lag_rows,]==0))
})
