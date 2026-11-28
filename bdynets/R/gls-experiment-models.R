# One-step benchmark forecasts use only observations preceding the scored row.
gls_pnar_forecast <- function(Y, graph, covariates, train_rows, test_rows,
                              order = 1L, pooled = FALSE, standardize = FALSE,
                              pre_history = NULL) {
  Y <- .validate_count_matrix(Y)
  graph <- .validate_gls_graph(graph)
  Q <- as.matrix(covariates)
  k <- ncol(Y)
  stopifnot(nrow(Q) == nrow(Y), all(is.finite(Q)), nrow(graph$W) == k,
    length(order)==1L, order %in% 1:2, length(train_rows) > 2L, length(test_rows) > 0L,
    all(diff(train_rows) == 1), all(diff(test_rows) > 0), min(train_rows) >= 1,
    max(train_rows) < min(test_rows), max(test_rows) <= nrow(Y))
  if (is.null(pre_history)) pre_history <- matrix(rep(Y[1, ], 2), nrow = 2, byrow = TRUE)
  stopifnot(ncol(pre_history) == k, nrow(pre_history) >= order,
    all(is.finite(pre_history)), all(pre_history >= 0))
  past <- log1p(rbind(pre_history, Y))
  X <- lapply(seq_len(order), function(lag) {
    x <- past[nrow(pre_history) + seq_len(nrow(Y)) - lag, , drop = FALSE]
    if (standardize) {
      z <- x[train_rows, , drop = FALSE]
      s <- apply(z, 2, stats::sd)
      x <- sweep(sweep(x, 2, colMeans(z)), 2, ifelse(s > 0, s, 1), "/")
    }
    x
  })
  network <- lapply(X, function(x) x %*% t(graph$W))
  design <- lapply(seq_len(k), function(a) {
    lags <- lapply(seq_len(order), function(i) cbind(X[[i]][, a], network[[i]][, a]))
    do.call(cbind, c(list(rep(1, nrow(Y)), Q), lags))
  })
  fit <- function(Z, y) {
    # All-zero training responses have their Poisson MLE at the boundary.
    if (all(y == 0)) return(c(-Inf, rep(0, ncol(Z) - 1L)))
    g <- stats::glm.fit(Z, y, family = stats::poisson())
    if (!g$converged) stop("PNAR fit failed to converge")
    b <- stats::coef(g)
    b[is.na(b)] <- 0
    b
  }
  train <- lapply(design, function(z) z[train_rows, , drop = FALSE])
  coefficients <- if (pooled) {
    rep(list(fit(do.call(rbind, train), as.vector(Y[train_rows,,drop=FALSE]))), k)
  } else lapply(seq_len(k), function(a) fit(train[[a]], Y[train_rows, a]))
  mean <- vapply(seq_len(k), function(a)
    exp(design[[a]][test_rows, , drop = FALSE] %*% coefficients[[a]]), numeric(length(test_rows)))
  dim(mean) <- c(length(test_rows), k)
  if (any(!is.finite(mean))) stop("Nonfinite PNAR forecast")
  list(mean = mean, coefficients = coefficients,
    logprob = stats::dpois(Y[test_rows,,drop=FALSE], mean, log=TRUE),
    lower = stats::qpois(.025, mean), upper = stats::qpois(.975, mean))
}

# An SE for the mean of matched losses based on circular overlapping blocks.
gls_experiment_dss <- function(fit, validation_rows, block_length = 8L,
                               tolerance = .007, control = fit$control) {
  stopifnot(inherits(fit, "gls_general_fit"), length(validation_rows) >= 2,
    block_length >= 1, tolerance >= 0)
  tr <- fit$train_rows
  stopifnot(all(diff(validation_rows)==1L), min(validation_rows)==max(tr)+1L,
            max(validation_rows)<=nrow(fit$Y))
  candidate <- run_general_dss(fit, fit$X[tr,,drop=FALSE], fit$covariates[tr,,drop=FALSE],
    fit$X,fit$covariates,fit$Y,validation_rows,fit$signal,control,path_only=TRUE)
  D <- candidate$multipliers
  ids <- .gls_general_draw_indices(.gls_general_chain_ids(fit), control$dss_validation_draws,
    method_seed("general DSS validation draws", fit$signal, control))
  view <- fit
  if (!is.null(fit$draw_store)) {
    view <- .gls_general_draw_view(fit, ids, .gls_general_prediction_fields)
    ids <- seq_along(ids)
  }
  seed <- method_seed("general DSS validation CRN", fit$signal, control)
  loss <- vapply(seq_len(ncol(D)),function(i)
    validation_general_deviance(view,fit$X,fit$covariates,fit$Y,validation_rows,
      D[,i],ids,seed,control,by_time=TRUE),numeric(length(validation_rows)))
  avg <- colMeans(loss)
  best <- which.min(avg)
  n <- nrow(loss)
  b <- min(as.integer(block_length), max(1L, floor(n/2)))
  se <- apply(loss - loss[, best], 2, function(diff) {
    means <- vapply(seq_len(n), function(start)
      mean(diff[1L + (start - 1L + seq_len(b) - 1L) %% n]), numeric(1))
    sqrt(b / n * mean((means - mean(diff))^2))
  })
  size <- candidate$path$n_selected
  choose <- function(pass) pass[order(size[pass], avg[pass], pass)[1]]
  full <- which(candidate$path$candidate_type == "continuous full")
  one <- choose(which(avg - avg[best] <= se + 1e-12))
  alt <- choose(which(avg <= avg[full] + tolerance * max(avg[full], 1e-12)))
  data <- data.frame(candidate = seq_along(avg), n_selected = size, deviance = avg,
    paired_se = se, one_se = seq_along(avg) == one, tolerance = seq_along(avg) == alt)
  list(d = D[, one], d_tolerance = D[, alt], path = data, multipliers = D, block_length = b,
       relative_tolerance = tolerance)
}

# Static Poisson global-local model. Elliptical slice sampling conditions on
# horseshoe scales; the half-Cauchy scales use inverse-gamma auxiliary updates.
gls_static_forecast <- function(Y, X, covariates, pairs, train_rows, test_rows,
                                control = gls_control(), checkpoint_dir = NULL,
                                chunk_size = 100L, checkpoint_every = 100L,
                                resume = FALSE, cores = 1L) {
  Y <- .validate_count_matrix(Y)
  control <- .validate_gls_control(control)
  k <- ncol(Y)
  stopifnot(identical(dim(Y), dim(X)), all(is.finite(X)),
    nrow(covariates) == nrow(Y), all(is.finite(covariates)),
    all(diff(train_rows)==1L), max(train_rows) < min(test_rows),
    min(train_rows)>=1L, max(test_rows)<=nrow(Y), all(diff(test_rows)>0L),
    length(cores)==1L, is.finite(cores), cores>=1L, cores==as.integer(cores),
    all(is.finite(pairs$affinity)), all(pairs$affinity > 0),
    all(pairs$target %in% seq_len(k)), all(pairs$source %in% seq_len(k)),
    !anyDuplicated(pairs[c("target","source")]))
  ephemeral <- is.null(checkpoint_dir)
  if (ephemeral) {
    if(resume) stop("Specify an existing checkpoint directory to resume.")
    checkpoint_dir <- tempfile("static-gls-")
    on.exit(unlink(checkpoint_dir, recursive=TRUE), add=TRUE)
  }
  checkpoint_dir <- .gls_checkpoint_directory(checkpoint_dir, checkpoint_every)
  lock <- .gls_checkpoint_lock(file.path(checkpoint_dir, "run.lock"))
  on.exit(filelock::unlock(lock), add=TRUE, after=FALSE)
  store <- .gls_checkpoint_store(checkpoint_dir, NULL, chunk_size, checkpoint_every,
    resume, list(engine="static-finite-shape-elliptical-slice-v1", Y=Y, X=X, Q=covariates,
      pairs=pairs, train=train_rows, test=test_rows, control=control))
  idx <- split(seq_len(nrow(pairs)), factor(pairs$target, levels=seq_len(k)))
  design <- lapply(idx, function(ii) cbind(1,covariates,X[,pairs$source[ii],drop=FALSE]))
  inputs <- list(Z=lapply(design,function(z) z[train_rows,,drop=FALSE]),
    Y=Y[train_rows,,drop=FALSE],r_nb=choose_r_nb_time(Y[train_rows,,drop=FALSE],control),
    idx=idx,pairs=pairs,base_n=1L+ncol(covariates),
    center=log(pmax(colMeans(Y[train_rows,,drop=FALSE]),.5/length(train_rows))))
  ids <- seq_len(control$n_chains)
  workers <- if (isTRUE(control$parallel_chains)) min(cores,length(ids)) else 1L
  if (workers==1L) {
    chains <- lapply(ids,.gls_static_chain,inputs,control,store)
  } else if (.Platform$OS.type=="windows") {
    cluster <- parallel::makePSOCKcluster(workers)
    on.exit(parallel::stopCluster(cluster),add=TRUE)
    parallel::clusterCall(cluster,.libPaths,.libPaths())
    chains <- parallel::parLapply(cluster,ids,.gls_static_chain,inputs,control,store)
  } else chains <- parallel::mclapply(ids,.gls_static_chain,inputs,control,store,
                                      mc.cores=workers,mc.set.seed=FALSE)
  .gls_check_chains(chains,ids)
  chain_ids <- rep(ids,vapply(chains,function(ch) ch$draw_chunks$n_saved,integer(1)))
  draws <- .gls_general_draw_indices(chain_ids,control$predictive_draws,control$seed)
  s <- .gls_read_disk_draws(chains,store$path,control,draws,c("B","baseline"))
  mu <- array(0,c(length(test_rows),k,length(draws)))
  for (a in seq_len(k)) {
    coef <- cbind(matrix(s$baseline[,a,,drop=FALSE],length(draws),inputs$base_n),
                  s$B[,idx[[a]],drop=FALSE])
    mu[,a,] <- exp(design[[a]][test_rows,,drop=FALSE] %*% t(coef))
  }
  list(lambda=mu, B=s$B,chain_id=chain_ids[draws],
       draw_store=if(!ephemeral)store,chains=if(!ephemeral)chains)
}

.gls_static_chain <- function(id, inputs, control, store) {
  set.seed(control$seed + id)
  idx <- inputs$idx
  p <- nrow(inputs$pairs)
  init <- function(a) c(inputs$center[a],rep(0,inputs$base_n+length(idx[[a]])-1L))
  theta <- lapply(seq_along(idx),init)
  aux_global <- 1
  tau2 <- if(is.null(control$fixed_tau)) 1 else control$fixed_tau^2
  local2 <- aux_local <- rep(1,p)
  total <- floor((control$n_iter-control$burn_mcmc)/control$thin)
  buffer <- .gls_draw_buffer(store,id,total)
  state_dir <- file.path(store$checkpoint_dir,sprintf("chain-%04d",id))
  dir.create(state_dir,showWarnings=FALSE)
  chain_lock <- .gls_checkpoint_lock(file.path(state_dir,"chain.lock"))
  on.exit(filelock::unlock(chain_lock),add=TRUE)
  previous <- .gls_restore_chain(state_dir,store,id)
  start <- 1L
  state_names <- c("theta","tau2","aux_global","local2","aux_local")
  if (!is.null(previous)) {
    list2env(previous$state,environment())
    assign(".Random.seed",previous$rng,.GlobalEnv)
    buffer[names(previous$index)] <- previous$index
    start <- previous$iteration+1L
  }
  B <- matrix(0,buffer$capacity,p)
  baseline <- array(0,c(buffer$capacity,length(idx),inputs$base_n))
  position <- 0L
  last_draw <- buffer$last_iteration
  if(start<=control$n_iter) for(iter in seq.int(start,control$n_iter)) {
    b <- numeric(p)
    for(a in seq_along(idx)) {
      ii <- idx[[a]]
      prior_sd <- c(control$alpha_prior_sd,rep(control$season_prior_sd,inputs$base_n-1),
                    sqrt(tau2*local2[ii])*inputs$pairs$affinity[ii])
      center <- init(a)
      current <- theta[[a]]
      Z <- inputs$Z[[a]]; y <- inputs$Y[,a]
      likelihood <- function(v) {
        eta <- drop(Z %*% v)
        if(any(!is.finite(eta))) return(-Inf)
        # Same finite-shape observation kernel as the dynamic sampler.
        # Terms independent of coefficients cancel in the slice comparison.
        z <- eta-log(inputs$r_nb)
        softplus <- pmax(z,0)+log1p(exp(-abs(z)))
        sum(y*z-(y+inputs$r_nb)*softplus)
      }
      threshold <- likelihood(current)+log(stats::runif(1))
      direction <- stats::rnorm(length(current),sd=prior_sd)
      angle <- stats::runif(1,0,2*pi)
      left <- angle-2*pi; right <- angle
      repeat {
        proposal <- center+(current-center)*cos(angle)+direction*sin(angle)
        if(likelihood(proposal)>=threshold) break
        if(angle<0) left <- angle else right <- angle
        angle <- stats::runif(1,left,right)
      }
      theta[[a]] <- proposal
      b[ii] <- proposal[inputs$base_n+seq_along(ii)]
    }
    invgamma <- function(shape,rate) 1/stats::rgamma(length(rate),shape,rate)
    local2 <- invgamma(1,1/aux_local+(b/inputs$pairs$affinity)^2/(2*tau2))
    aux_local <- invgamma(1,1+1/local2)
    if(is.null(control$fixed_tau)) {
      tau2 <- invgamma((p+1)/2,1/aux_global+sum((b/inputs$pairs$affinity)^2/local2)/2)
      aux_global <- invgamma(1,1/control$discount_tau_scale^2+1/tau2)
    } else tau2 <- control$fixed_tau^2
    if(iter>control$burn_mcmc && (iter-control$burn_mcmc)%%control$thin==0L) {
      position <- position+1L
      B[position,] <- b
      for(a in seq_along(idx)) baseline[position,a,] <- theta[[a]][seq_len(inputs$base_n)]
      last_draw <- iter
    }
    commit <- iter%%store$checkpoint_every==0L || iter==control$n_iter
    if(position==buffer$capacity || (commit && position>0L)) {
      buffer <- .gls_write_draw_chunk(buffer,list(B=B,baseline=baseline),position,last_draw)
      position <- 0L
    }
    if(commit) .gls_commit_chain(list(iteration=iter,chain_id=id,index=.gls_draw_chunks(buffer),
      rng=get(".Random.seed",envir=.GlobalEnv),
      state=mget(state_names,envir=environment(),inherits=FALSE)),state_dir)
  }
  list(chain_id=id,draw_chunks=.gls_draw_chunks(buffer))
}
