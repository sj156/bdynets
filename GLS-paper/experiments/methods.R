# Convex Poisson lasso fitted jointly in matrix notation. Intercepts and
# seasonal terms are unpenalized, and the tuning loss uses only validation.
poisson_lasso <- function(panel, X, smoke) {
  va <- panel$split$validation
  te <- panel$split$test
  model <- bdynets::gls_poisson_lasso_fit(panel$Y,X,panel$covariates,panel$split$train,
                                         n_penalties=if(smoke) 5L else 30L)
  bring <- function(rows,index) stats::predict(model,X=X[rows,,drop=FALSE],
                                      covariates=panel$covariates[rows,,drop=FALSE],index=index)
  valid <- vapply(seq_along(model$penalties), function(i) {
    mu <- bring(va,i)
    mean(mu-panel$Y[va,,drop=FALSE]*log(mu))
  },numeric(1))
  best <- which.min(valid)
  mu <- bring(te,best)
  chosen <- model$coefficients[[best]]
  list(mean=mu,logprob=dpois(panel$Y[te,,drop=FALSE],mu,log=TRUE),
    lower=qpois(.025,mu),upper=qpois(.975,mu),B=t(chosen[model$lag_rows,,drop=FALSE]),
    tuning=data.frame(penalty=model$penalties,validation_nll=valid,
                      chosen=seq_along(valid)==best))
}

forecast_metrics <- function(panel, fc) {
  rows <- panel$split$test
  windows <- lapply(panel$windows, function(mask) mask[rows, , drop = FALSE])
  bdynets::gls_forecast_metrics(panel$Y[rows, , drop = FALSE], fc,
    intensity = panel$lambda[rows, , drop = FALSE], windows = windows)
}

coefficient_metrics <- function(panel, pairs, trajectory, selected, scale) {
  # Compare on the fitted standardized lag scale; omitted dictionary entries
  # have zero estimated effect. Fit and posterior mean are evaluated in training.
  full <- bdynets::gls_all_pairs(panel$graph)
  ids <- match(paste(pairs$target,pairs$source),paste(full$target,full$source))
  tr <- panel$split$train
  k <- ncol(panel$Y)
  cells <- cbind(full$target,full$source)
  ratio <- scale[full$source]/panel$generating_scale[full$source]
  truth <- outer(rep(1,length(tr)),panel$base[cells]*ratio) +
    outer(panel$factor[tr],panel$modulation[cells]*ratio)
  estimate <- matrix(0,length(tr),nrow(full))
  estimate[,ids] <- trajectory
  sel <- rep(FALSE,nrow(full)); sel[ids] <- selected
  cats <- list(all=rep(TRUE,nrow(full)),cross=full$source!=full$target)
  cats <- c(cats,split(seq_len(nrow(full)),full$pair_class))
  cats <- lapply(cats,function(x) if(is.logical(x)) x else seq_len(nrow(full)) %in% x)
  if (!is.null(panel$true_A)) {
    cats$removed <- panel$true_A[cells]==1 & panel$graph$A[cells]==0
    cats$inserted <- panel$true_A[cells]==0 & panel$graph$A[cells]==1
  }
  if (!is.null(panel$upstream)) {
    cats$planted <- panel$graph$A[cells]==1
    cats$reverse <- t(panel$graph$A)[cells]==1
    cats$unaffected <- !full$target %in% c(panel$upstream,panel$downstream)
  }
  names(cats) <- paste0("pairs-", names(cats))
  bdynets::gls_trajectory_metrics(truth, estimate, sel, cats)
}

fit_experiment_method <- function(method,panel,profile,seed,dir,cores,checkpoint_every,
                                  chunk_size,resume,fit_id) {
  train <- panel$split$train; val <- panel$split$validation; test <- panel$split$test
  if (method %in% c("pooled-pnar","pnar")) {
    p <- bdynets::gls_pnar_forecast(panel$Y,panel$graph,panel$covariates,train,test,
      pooled=method=="pooled-pnar",standardize=method!="pooled-pnar",pre_history=panel$pre_history)
    return(setNames(list(forecast_metrics(panel,p)),method))
  }
  if (method %in% c("sa-pgss","graph-wavenet")) {
    if (!exists("external_forecast",mode="function")) stop("External benchmark adapter is unavailable")
    p <- external_forecast(method,panel,profile,seed,dir)
    return(setNames(list(forecast_metrics(panel,p)),method))
  }
  scaled <- bdynets::gls_lag_predictors(panel$Y, train, panel$pre_history)
  pairs <- bdynets::gls_all_pairs(panel$graph)
  if (method=="poisson-lasso") {
    f <- poisson_lasso(panel,scaled$X,profile=="smoke")
    b <- f$B[cbind(pairs$target,pairs$source)]
    write.csv(f$tuning,file.path(dir,"validation.csv"),row.names=FALSE)
    out <- rbind(forecast_metrics(panel,f), coefficient_metrics(panel,pairs,
      outer(rep(1,length(train)),b),b!=0,scaled$scale))
    return(setNames(list(out),method))
  }
  if (method=="gls-restricted") pairs <- pairs[pairs$hop<=1,,drop=FALSE]
  if (method=="gls-own") pairs <- pairs[pairs$target==pairs$source,,drop=FALSE]
  if (method=="gls-flat") pairs$affinity <- 1
  control <- bdynets::gls_control(if(profile=="smoke") "smoke" else "default",
    seed=seed,n_chains=if(profile=="smoke") 1L else 3L, parallel_chains=cores>1L,
    n_iter=if(profile=="smoke") 16L else 5000L,burn_mcmc=if(profile=="smoke") 8L else 4000L,
    predictive_draws=if(profile=="smoke") 6L else 600L,
    dss_n_lambda=if(profile=="smoke") 5L else 100L,
    dss_validation_draws=if(profile=="smoke") 4L else 150L,
    dss_max_draws=if(profile=="smoke") 6L else 600L,dss_predictive_tolerance=.007)
  observed <- panel$Y[test,,drop=FALSE]
  if (method=="gls-static") {
    physical <- parallel::detectCores(logical=FALSE)
    if (!is.finite(physical) || physical<1L) physical <- 1L
    p <- bdynets::gls_static_forecast(panel$Y,scaled$X,panel$covariates,pairs,train,test,control,
      checkpoint_dir=file.path(dir,"checkpoint"),checkpoint_every=checkpoint_every,
      resume=resume && file.exists(file.path(dir,"checkpoint","run.rds")),
      chunk_size=chunk_size,cores=min(cores,physical))
    b <- colMeans(p$B)
    fc <- bdynets::gls_poisson_mixture_forecast(p$lambda, observed, seed = seed)
    select <- colMeans(abs(p$B)>control$delta_A)>.5
    out <- rbind(forecast_metrics(panel,fc),coefficient_metrics(panel,pairs,
      outer(rep(1,length(train)),b),select,scaled$scale))
    saveRDS(list(B=p$B,chain_id=p$chain_id,control=control,draw_store=p$draw_store,chains=p$chains),file.path(dir,"static-draws.rds"))
    return(setNames(list(out),method))
  }
  fit_dir <- dir
  cache_flat <- method == "gls-flat" && !is.null(panel$true_A)
  if (cache_flat) {
    # A flat-affinity prior and full dictionary make this fit invariant to the
    # supplied graph. Key the cache by the actual panel, split, options and seed.
    key <- digest::digest(list(panel$Y,panel$pre_history,panel$covariates,
      panel$split,control,fit_id),algo="sha256")
    case_dir <- dirname(dirname(dirname(dir)))
    fit_dir <- file.path(case_dir,"shared-flat",key)
    lock <- job_lock(fit_dir,timeout=Inf)
    on.exit(filelock::unlock(lock),add=TRUE)
  }
  cache <- file.path(fit_dir,"scored-posterior.rds")
  identity <- digest::digest(list(panel$Y,panel$pre_history,panel$covariates,panel$split,
    if (!cache_flat) panel$graph,fit_id),algo="sha256")
  if (file.exists(cache)) {
    saved <- readRDS(cache)
    if (!identical(saved$identity,identity)) stop("Scored posterior identity mismatch")
  } else {
    checkpoint <- file.path(fit_dir,"checkpoint")
    fit_graph <- if(cache_flat) network_graph(matrix(0,ncol(panel$Y),ncol(panel$Y))) else panel$graph
    fit_pairs <- if(cache_flat) bdynets::gls_all_pairs(fit_graph) else pairs
    if(cache_flat) fit_pairs$affinity <- 1
    fit <- bdynets::gls_fit_general(panel$Y,fit_graph,panel$covariates,train,
      pre_history=panel$pre_history,pairs=fit_pairs,control=control,label=method,
      checkpoint_dir=checkpoint,checkpoint_every=checkpoint_every,chunk_size=chunk_size,max_workers=cores,
      resume=resume && file.exists(file.path(checkpoint,"run.rds")))
    save_atomic(fit,file.path(fit_dir,"fit.rds"))
    diagnostics <- bdynets::gls_diagnostics(fit)
    write_table(diagnostics,file.path(fit_dir,"diagnostics.csv"))
    draws <- bdynets::gls_read_general_draws(fit,c("B","L","beta_path","amplitude"),
                                             max_draws=control$predictive_draws,seed=seed)
    trajectory <- crossprod(draws$beta_path[,,1L],draws$L[,,1L])/nrow(draws$B)
    trajectory <- sweep(trajectory,2,colMeans(draws$B),"+")
    marginal <- colMeans(draws$amplitude>control$delta_A)>.5
    dss <- bdynets::gls_experiment_dss(fit,val)
    save_atomic(dss,file.path(fit_dir,"dss.rds"))
    if (!is.null(panel$signal)) {
      # Decisions are frozen above. Use the same draws and innovations across
      # candidate graphs and carry the validation history through each forecast.
      path_control <- control
      path_control$predictive_draws <- control$dss_validation_draws
      dss$path$test_deviance <- vapply(seq_len(ncol(dss$multipliers)),function(i) {
        p <- predict(fit,test,conditioning_rows=val,d=dss$multipliers[,i],
                     control=path_control,label="frozen support path")
        mu <- apply(p$lambda,1:2,mean)
        mean(bdynets::poisson_deviance(observed, mu))
      },numeric(1))
      dss$path$test_draws <- min(path_control$predictive_draws,
        control$n_chains*floor((control$n_iter-control$burn_mcmc)/control$thin))
    }
    decisions <- list(continuous=rep(1,nrow(pairs)),dss=dss$d,tolerance=dss$d_tolerance)
    forecasts <- lapply(decisions,function(d){
      pred <- predict(fit,test,conditioning_rows=val,d=d,label="paired test forecast")
      fc <- bdynets::gls_poisson_mixture_forecast(pred$lambda, observed, draws = pred$y)
      forecast_metrics(panel,fc)
    })
    saved <- list(identity=identity,trajectory=trajectory,marginal=marginal,decisions=decisions,
                  forecasts=forecasts,validation=dss$path)
    save_atomic(saved,cache)
  }
  write_table(saved$validation,file.path(dir,"validation.csv"))
  if(cache_flat) writeLines(fit_dir,file.path(dir,"shared-fit.txt"))
  ans <- lapply(names(saved$decisions),function(rule) {
    d <- saved$decisions[[rule]]
    sel <- if(rule=="continuous") saved$marginal else d>control$dss_zero_tolerance
    rbind(saved$forecasts[[rule]],coefficient_metrics(panel,pairs,
      sweep(saved$trajectory,2,d,"*"),sel,scaled$scale))
  })
  setNames(ans,c(method,paste0(method,"-dss"),paste0(method,"-tolerance")))
}
