# Current Poisson discount model. This API does not change the static gmde_fit API.
# Reuses the package's allocation helpers and compiled deterministic guide.

.gmde_dynamic_config <- function(input, config) {
  required <- c("K", "initial_mean", "initial_covariance", "discount_prior", "adjustment",
    "nu", "tau", "u0", "rho", "block_size", "discount_step", "kappa_step", "h_step",
    "iterations", "warmup", "initial_groups", "initial_h", "initial_log_kappa", "ess_limit")
  absent <- setdiff(required, names(config))
  if (length(absent)) stop("Missing explicit configuration: ", paste(absent, collapse = ", "))
  Y <- as.matrix(input$Y); F <- as.matrix(input$Fmat)
  if (!is.numeric(Y) || any(!is.finite(Y)) || any(Y < 0 | Y != floor(Y)) ||
      ncol(Y) != nrow(F) || any(!is.finite(F))) stop("Invalid training counts/design.")
  p <- ncol(F); n <- nrow(Y)
  for (name in c("K", "rho", "block_size", "iterations", "warmup", "initial_groups", "ess_limit")) {
    x <- config[[name]]
    if (length(x) != 1L || !is.finite(x) || x != floor(x) || x < 1L)
      stop("Require a positive integer: ", name)
  }
  if (config$warmup >= config$iterations || config$initial_groups > min(config$K, n))
    stop("Invalid warmup or initialization.")
  for (name in c("adjustment", "nu", "tau", "discount_step", "kappa_step", "h_step", "initial_h"))
    if (length(config[[name]]) != 1L || !is.finite(config[[name]]) || config[[name]] <= 0)
      stop("Require a positive scalar: ", name)
  if (length(config$discount_prior) != 2L || any(!is.finite(config$discount_prior)) ||
      any(config$discount_prior <= 0) || length(config$initial_mean) != p ||
      any(!is.finite(config$initial_mean)) ||
      !identical(dim(config$initial_covariance), c(p,p))) stop("Invalid expert prior.")
  if (!isSymmetric(config$initial_covariance)) stop("Initial covariance must be symmetric.")
  chol(config$initial_covariance)
  if (length(config$u0) != 1L || !is.finite(config$u0) ||
      length(config$initial_log_kappa) != 1L || !is.finite(config$initial_log_kappa))
    stop("Invalid graph range configuration.")
  list(Y=Y, F=F, p=p, n=n, T=ncol(Y), G=diag(p), config=config)
}

.gmde_dynamic_guide <- function(S, N, d, ctx) {
  gmde_discount_guide(S, N, ctx$F, ctx$G, ctx$config$initial_mean,
    ctx$config$initial_covariance, d, ctx$config$adjustment)
}

.gmde_dynamic_spectrum <- function(input, method, h, nu, tau, u) {
  L <- .gmde_raw_laplacian_cpp(input$adjacency, input$edge_lengths, method, h)
  E <- eigen(L, symmetric=TRUE)
  # Keep all computed eigenvalues, including roundoff-sized signed zeros.
  # The strictly positive diagonal shift makes the raw covariance full rank.
  if (min(E$values) < -1e-10 * max(1,max(abs(E$values)))) stop("Invalid Laplacian spectrum.")
  out <- list(vectors=E$vectors, values=E$values, nu=nu, tau=tau)
  .gmde_dynamic_spectral_weights(out, u)
}

.gmde_dynamic_spectral_weights <- function(spec, u) {
  shifted <- 2*spec$nu*exp(-2*u)+spec$values
  if (any(!is.finite(shifted)) || any(shifted <= 0)) stop("Unrepresentable shifted spectrum.")
  spec$log_variance <- 2*log(spec$tau)-spec$nu*log(shifted)
  spec$variance <- exp(spec$log_variance)
  if (any(!is.finite(spec$variance)) || any(spec$variance <= 0)) stop("Invalid covariance spectrum.")
  spec
}

.gmde_dynamic_gp_density <- function(field, spec) {
  projected <- crossprod(spec$vectors, field)
  ans <- -ncol(field)/2*sum(spec$log_variance)-sum(rowSums(projected^2)/spec$variance)/2
  if (!is.finite(ans)) stop("Nonfinite GP density.")
  ans
}

.gmde_dynamic_pg <- function(shape, tilt) {
  # Integer Devroye route migrated from core_02_state_update.R; no hybrid fallback.
  if (any(!is.finite(shape)) || any(shape <= 0 | shape != round(shape)) ||
      any(shape > .Machine$integer.max) || any(!is.finite(tilt))) stop("Invalid exact PG parameters.")
  omega <- as.numeric(BayesLogit::rpg.devroye(length(shape), as.integer(shape), tilt))
  if (length(omega)!=length(shape) || any(!is.finite(omega)) || any(omega<=0))
    stop("Invalid PG draw; no clipping applied.")
  omega
}

.gmde_dynamic_prior_path <- function(guide, ctx) {
  theta <- matrix(0,ctx$T+1L,ctx$p)
  theta[1,] <- ctx$config$initial_mean + drop(t(chol(ctx$config$initial_covariance)) %*% rnorm(ctx$p))
  for (t in seq_len(ctx$T)) theta[t+1L,] <- drop(ctx$G %*% theta[t,]) +
    drop(t(chol(guide$innovation_covariance[,,t])) %*% rnorm(ctx$p))
  theta
}

.gmde_dynamic_state <- function(theta, S, N, guide, ctx) {
  r <- ctx$config$rho*pmax(S,1)
  accepted <- 0L; attempted <- 0L; jump <- 0
  # Fixed, state-independent partition, with both conditional boundary factors.
  for (s in seq.int(1L, ctx$T, by=ctx$config$block_size)) {
    e <- min(ctx$T,s+ctx$config$block_size-1L); times <- seq.int(s,e)
    eta <- rowSums(theta[times+1L,,drop=FALSE]*ctx$F[times,,drop=FALSE])
    offset <- log(N)-log(r[times])
    omega <- .gmde_dynamic_pg(S[times]+r[times],eta+offset)
    precision <- natural <- numeric(ctx$T)
    precision[times] <- omega
    natural[times] <- (S[times]-r[times])/2-omega*offset
    proposal <- .gmde_information_ffbs_cpp(theta,ctx$F,precision,natural,ctx$G,
      ctx$config$initial_mean,ctx$config$initial_covariance,guide$innovation_covariance,
      as.integer(s),as.integer(e),matrix(rnorm((ctx$T+1L)*ctx$p),ctx$T+1L,ctx$p))
    new_eta <- rowSums(proposal[times+1L,,drop=FALSE]*ctx$F[times,,drop=FALSE])
    ratio <- .gmde_poisson_block_ratio_cpp(S[times],N,r[times],eta,new_eta)
    attempted <- attempted+1L
    if (log(runif(1)) < min(0,ratio)) {
      accepted <- accepted+1L; jump <- jump+sum(pmax(S[times],1)*(new_eta-eta)^2)
      theta <- proposal
    }
  }
  list(theta=theta,accepted=accepted,attempted=attempted,jump=jump)
}

.gmde_dynamic_discount_candidate <- function(theta,S,N,d,candidate,guide,ctx) {
  next_guide <- .gmde_dynamic_guide(S,N,candidate,ctx)
  before <- gmde_transition_log_density(theta,ctx$G,guide$innovation_covariance)
  after <- gmde_transition_log_density(theta,ctx$G,next_guide$innovation_covariance)
  ab <- ctx$config$discount_prior
  # Beta density plus the logit Jacobian: a log(d) + b log(1-d).
  ratio <- after-before+ab[1]*(log(candidate)-log(d))+ab[2]*(log1p(-candidate)-log1p(-d))
  list(guide=next_guide,ratio=ratio)
}

.gmde_dynamic_allocation_candidate <- function(i,a,b,S,N,Y,d,theta,guides,ctx) {
  Sa <- S[a,]-Y[i,]; Sb <- S[b,]+Y[i,]
  ga <- .gmde_dynamic_guide(Sa,N[a]-1L,d[a],ctx)
  gb <- .gmde_dynamic_guide(Sb,N[b]+1L,d[b],ctx)
  ratio <- gmde_transition_log_density(theta[[a]],ctx$G,ga$innovation_covariance)+
    gmde_transition_log_density(theta[[b]],ctx$G,gb$innovation_covariance)-
    gmde_transition_log_density(theta[[a]],ctx$G,guides[[a]]$innovation_covariance)-
    gmde_transition_log_density(theta[[b]],ctx$G,guides[[b]]$innovation_covariance)
  list(Sa=Sa,Sb=Sb,ga=ga,gb=gb,ratio=ratio)
}

.gmde_dynamic_ess <- function(field,Z,spec,limit) {
  evaluations <- 0L
  for(k in seq_len(ncol(field))) {
    original <- field[,k]
    direction <- drop(spec$vectors %*% (sqrt(spec$variance)*rnorm(nrow(field))))
    threshold <- .gmde_softmax_loglik_cpp(field,Z)+log(runif(1))
    angle <- runif(1,0,2*pi); lower <- angle-2*pi; upper <- angle
    for(attempt in seq_len(limit)) {
      field[,k] <- original*cos(angle)+direction*sin(angle)
      evaluations <- evaluations+1L
      if(.gmde_softmax_loglik_cpp(field,Z)>threshold) break
      if(attempt==limit) stop("ESS bracket limit exhausted; transition not silently retained.")
      if(angle<0) lower <- angle else upper <- angle
      angle <- runif(1,lower,upper)
    }
  }
  list(field=field,evaluations=evaluations)
}

.gmde_dynamic_initialize <- function(ctx,input,method) {
  cfg <- ctx$config; K <- cfg$K
  # Training-only harmonic regression defines starting partitions, never a prior.
  features <- t(qr.coef(qr(ctx$F), t(log(ctx$Y+0.5))))
  if (cfg$initial_groups==1L) Z <- rep(1L,ctx$n) else {
    Z <- stats::kmeans(features[,seq.int(2L,ctx$p),drop=FALSE],
      centers=cfg$initial_groups,nstart=5L,iter.max=100L)$cluster
  }
  suff <- .gmde_sufficient(ctx$Y,Z,K); d <- rbeta(K,cfg$discount_prior[1],cfg$discount_prior[2])
  guides <- theta <- vector("list",K)
  for(k in seq_len(K)) {
    guides[[k]] <- .gmde_dynamic_guide(suff$S[k,],suff$N[k],d[k],ctx)
    if(suff$N[k]==0L) theta[[k]] <- .gmde_dynamic_prior_path(guides[[k]],ctx) else {
      coef <- qr.coef(qr(ctx$F),log((suff$S[k,]+0.5)/suff$N[k]))
      theta[[k]] <- matrix(rep(coef,each=ctx$T+1L),ctx$T+1L,ctx$p)
    }
  }
  spec <- .gmde_dynamic_spectrum(input,method,cfg$initial_h,cfg$nu,cfg$tau,cfg$initial_log_kappa)
  field <- spec$vectors %*% (sqrt(spec$variance)*matrix(rnorm(ctx$n*K),ctx$n,K))
  list(Z=as.integer(Z),N=suff$N,S=suff$S,d=d,guides=guides,theta=theta,
       field=field,spec=spec,u=cfg$initial_log_kappa,h=cfg$initial_h)
}

#' Development sampler for the full-rank Poisson discount graphMoDE model
#' @param input Training counts, designs, binary adjacency and edge lengths.
#' @param method One of Exp-W, SE-W or C.
#' @param config Explicit scientific and computational settings (see details).
#' @param progress Optional callback receiving each completed iteration.
#' @return Traces, saved labels, posterior similarity matrix, fitted intensity
#'   mean, terminal state, and computational diagnostics. No convergence claim.
#' @export
gmde_dynamic_fit <- function(input, method, config, progress=NULL) {
  method <- match.arg(method,c("Exp-W","SE-W","C"))
  ctx <- .gmde_dynamic_config(input,config); cfg <- ctx$config
  state <- .gmde_dynamic_initialize(ctx,input,method)
  K <- cfg$K; n <- ctx$n; T <- ctx$T
  trace <- matrix(NA_real_,cfg$iterations,13L,dimnames=list(NULL,
    c("occupied","log_likelihood","log_kappa","log_h","mean_occupied_discount",
      "state_acceptance","discount_acceptance","allocation_proposals","allocation_moves",
      "kappa_accepted","h_accepted","ess_evaluations","state_information_jump")))
  saved_labels <- matrix(NA_integer_,cfg$iterations-cfg$warmup,n)
  psm <- matrix(0,n,n); fitted <- matrix(0,n,T); draws <- 0L
  discount <- matrix(NA_real_,cfg$iterations,K)
  started <- proc.time()[[3L]]
  for(iteration in seq_len(cfg$iterations)) {
    sa <- st <- da <- dt <- moves <- proposed <- ess_eval <- 0L; jump <- 0
    # Expert state update followed by baseline-discount MH. Empty experts receive
    # a joint prior refresh; conditioning on the retained path would be invalid.
    for(k in seq_len(K)) {
      if(state$N[k]==0L) {
        state$d[k] <- rbeta(1,cfg$discount_prior[1],cfg$discount_prior[2])
        state$guides[[k]] <- .gmde_dynamic_guide(state$S[k,],0,state$d[k],ctx)
        state$theta[[k]] <- .gmde_dynamic_prior_path(state$guides[[k]],ctx)
      } else {
        up <- .gmde_dynamic_state(state$theta[[k]],state$S[k,],state$N[k],state$guides[[k]],ctx)
        state$theta[[k]] <- up$theta; sa <- sa+up$accepted; st <- st+up$attempted; jump <- jump+up$jump
        candidate <- plogis(qlogis(state$d[k])+cfg$discount_step*rnorm(1))
        if(!(candidate>0 && candidate<1)) stop("Discount proposal rounded to a boundary.")
        up <- .gmde_dynamic_discount_candidate(state$theta[[k]],state$S[k,],state$N[k],
          state$d[k],candidate,state$guides[[k]],ctx)
        dt <- dt+1L
        if(log(runif(1))<min(0,up$ratio)) {state$d[k]<-candidate;state$guides[[k]]<-up$guide;da<-da+1L}
      }
    }
    up <- .gmde_dynamic_ess(state$field,state$Z,state$spec,cfg$ess_limit)
    state$field <- up$field; ess_eval <- up$evaluations
    eta <- vapply(state$theta,function(x)rowSums(x[-1,,drop=FALSE]*ctx$F),numeric(T))
    emissions <- .gmde_poisson_emission_cpp(ctx$Y,eta)
    # At fixed fields and paths this proposal matrix is constant across the
    # sequential scan. Guide factors are recomputed after every proposed move.
    probabilities <- exp(.gmde_log_softmax(emissions+state$field))
    for(i in seq_len(n)) {
      a <- state$Z[i]; b <- sample.int(K,1L,prob=probabilities[i,])
      if(a==b) next
      proposed <- proposed+1L
      up <- .gmde_dynamic_allocation_candidate(i,a,b,state$S,state$N,ctx$Y,state$d,
        state$theta,state$guides,ctx)
      if(log(runif(1))<min(0,up$ratio)) {
        state$Z[i]<-b; state$S[a,]<-up$Sa;state$S[b,]<-up$Sb
        state$N[a]<-state$N[a]-1L;state$N[b]<-state$N[b]+1L
        state$guides[[a]]<-up$ga;state$guides[[b]]<-up$gb;moves<-moves+1L
      }
    }
    # Log-coordinate MH: the following prior expressions include each Jacobian once.
    q <- .gmde_dynamic_gp_density(state$field,state$spec)
    next_u <- state$u+cfg$kappa_step*rnorm(1)
    spec <- .gmde_dynamic_spectral_weights(state$spec,next_u)
    next_q <- .gmde_dynamic_gp_density(state$field,spec)
    kr <- next_q-q-((next_u-cfg$u0)^2-(state$u-cfg$u0)^2)/2
    ka <- log(runif(1))<min(0,kr)
    if(ka){state$u<-next_u;state$spec<-spec;q<-next_q}
    ha <- NA
    if(method!="C") {
      next_v <- log(state$h)+cfg$h_step*rnorm(1); next_h <- exp(next_v)
      spec <- .gmde_dynamic_spectrum(input,method,next_h,cfg$nu,cfg$tau,state$u)
      next_q <- .gmde_dynamic_gp_density(state$field,spec)
      hr <- next_q-q+0.5*(next_v-log(state$h))-0.5*(next_h-state$h)
      ha <- log(runif(1))<min(0,hr)
      if(ha){state$h<-next_h;state$spec<-spec}
    }
    trace[iteration,] <- c(sum(state$N>0),sum(emissions[cbind(seq_len(n),state$Z)]),state$u,
      if(method=="C")NA_real_ else log(state$h),mean(state$d[state$N>0]),sa/st,da/dt,
      proposed,moves,ka,ha,ess_eval,jump)
    discount[iteration,] <- state$d
    if(iteration>cfg$warmup) {
      draws <- draws+1L; saved_labels[draws,] <- state$Z
      psm <- psm+outer(state$Z,state$Z,"==")
      fitted <- fitted+t(exp(eta[,state$Z,drop=FALSE]))
    }
    if(!is.null(progress)) progress(list(iteration=iteration,trace=trace[iteration,],
      elapsed=proc.time()[[3L]]-started,state=state))
  }
  list(method=method,config=cfg,trace=trace,discount=discount,labels=saved_labels,
    psm=psm/draws,fitted_intensity=fitted/draws,terminal=state,
    elapsed=proc.time()[[3L]]-started,convergence_certified=FALSE,
    role="Development run; no formal simulation or predictive-performance claim.")
}
