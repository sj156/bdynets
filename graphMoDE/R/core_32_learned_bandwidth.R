# Exact selected definitions from R/graphmode_h_learning.R
graphmode_h_version <- "graphmode-learned-bandwidth-20261009-v1"

graphmode_h_geometry <- function(distance, ids, support = c("direct", "1nn")) {
    support <- match.arg(support)
    graphmode_matrix(distance, "road distances")
    D <- (distance + t(distance))/2
    graphmode_validate_distance(D)
    n <- nrow(D); ids <- graphmode_geometry_ids(ids, n)
    if(n < 2L) stop("At least two sites required.")
    if(support == "direct") {
        position <- match(ids, sort(ids))
        mask <- abs(outer(position, position, "-")) == 1L
        selected <- which(upper.tri(D) & mask, arr.ind=TRUE)
        factor <- 1*mask
    } else {
        neighbor <- vapply(seq_len(n), function(i) {
            j <- setdiff(seq_len(n), i); j[order(D[i,j], ids[j])[1L]]
        }, 1L)
        selected <- cbind(seq_len(n), neighbor)
        directed <- matrix(0, n, n); directed[selected] <- 1
        factor <- (directed + t(directed))/2
    }
    d_ref <- median(D[selected]); graphmode_positive(d_ref, "distance reference")
    ij <- which(upper.tri(D) & factor > 0, arr.ind=TRUE)
    if(any(D[ij] <= 0)) stop("Selected edges must have positive length.")
    list(support=support, ids=ids, distance=D, selected=selected, edges=ij,
        factor=factor[ij], d_ref=d_ref, normalization=if(support=="direct") "positive-mean-one" else "none")
}

graphmode_h_weight <- function(geometry, family=c("exp", "se"), log_eta=0) {
    family <- match.arg(family)
    if(length(log_eta)!=1L || !is.finite(log_eta)) stop("Invalid log bandwidth multiplier.")
    log_h <- log(geometry$d_ref) + log_eta
    power <- if(family=="exp") 1 else 2
    penalty <- exp(power*(log(geometry$distance[geometry$edges])-log_h))
    if(any(!is.finite(penalty))) stop("Bandwidth weight overflow; preserve failure, no truncation.")
    log_w <- -penalty + log(geometry$factor)
    if(geometry$normalization=="positive-mean-one") {
        # Stable log-mean-exp; rescale at every proposal, not only at initialization.
        largest <- max(log_w)
        log_w <- log_w-largest-log(mean(exp(log_w-largest)))
    }
    values <- exp(log_w)
    if(any(!is.finite(values) | values<=0))
        stop("Bandwidth edge-weight underflow; preserve failure, no clipping or edge removal.")
    W <- matrix(0, length(geometry$ids), length(geometry$ids))
    W[geometry$edges] <- values; W <- W+t(W)
    graphmode_validate_weight(W)
    W
}

graphmode_h_spectrum <- function(fit, log_eta) {
    W <- graphmode_h_weight(fit$bandwidth$geometry, fit$bandwidth$family, log_eta)
    L <- diag(rowSums(W))-W
    e <- eigen(L, symmetric=TRUE)
    tolerance <- 64*.Machine$double.eps*max(1,norm(L,"I"))
    if(min(e$values)< -tolerance) stop("Invalid learned-bandwidth graph spectrum.")
    list(kind="graph",values=pmax(e$values,0),vectors=e$vectors,nu=1,tau=1)
}

graphmode_h_root <- function(context, log_scale) {
    # Principal symmetric root is invariant to eigenvector signs/rotations.
    # This fixes node-indexed white coordinates while h changes the eigenbasis.
    spectral <- graphmode_c8_kernel(context, log_scale)
    Phi <- spectral %*% t(context$vectors)
    if(any(!is.finite(Phi))) stop("Invalid covariance root.")
    Phi
}

graphmode_h_config <- function(road, Y, support, family, unit_order=seq_len(nrow(Y))) {
    geometry <- graphmode_h_geometry(road$road_distance, road$ids, support)
    reference <- graphmode_h_weight(geometry, "exp", 0)
    fit <- graphmode_c9_config(road,Y,"graphMoDE-W",reference,unit_order=unit_order)
    # Retain the original Exp reference kappa prior centre for both families;
    # it is frozen here and is never recalibrated inside an h proposal.
    fit$bandwidth <- list(family=match.arg(family,c("exp","se")),
        geometry=graphmode_h_geometry(road$road_distance[unit_order,unit_order],road$ids[unit_order],support),
        shape=4,rate=4,proposal_sd=.15,start_log_offsets=c(-1,-.3,.3,1))
    fit$version <- graphmode_h_version
    fit$core$Phi <- graphmode_h_root(graphmode_h_spectrum(fit,0),fit$kernel$log_center)
    graphmode_revalidate_config(fit$core)
    fit
}

graphmode_h_current_config <- function(fit, log_scale, log_q, log_eta, spectrum=NULL) {
    if(!identical(fit$version,graphmode_h_version)) stop("Learned bandwidth requires its own fit schema.")
    q <- exp(log_q); graphmode_positive(q,"innovation variance")
    c <- fit$core; c$W <- diag(q,ncol(c$Fmat))
    if(is.null(spectrum)) spectrum <- graphmode_h_spectrum(fit,log_eta)
    c$Phi <- graphmode_h_root(spectrum,log_scale)
    graphmode_revalidate_config(c); c
}

graphmode_h_validate_state <- function(state, fit, config=NULL) {
    if(!identical(state$version,graphmode_h_version) || length(state$log_eta)!=1L ||
        !is.finite(state$log_eta)) stop("Invalid learned-bandwidth state; old checkpoints forbidden.")
    if(is.null(config)) config <- graphmode_h_current_config(fit,state$log_scale,state$log_q,state$log_eta)
    converted <- state; converted$version <- graphmode_c8_version
    graphmode_c8_validate_state(converted,fit,config)
    invisible(TRUE)
}

graphmode_h_common_start <- function(fit, source, chain) {
    chain <- gmde_scalar_integer(chain,"chain",1L,4L)
    if(!identical(source$iteration,0L)) stop("Only freshly generated initial states allowed.")
    offsets <- graphmode_c9_spec()$bayes
    state <- list(version=graphmode_h_version,
        core=graphmode_initial_state(fit$core,source$Z,source$theta,x=source$x,v=source$v),
        log_scale=fit$kernel$log_center+offsets$start_scale_log_offsets[chain],
        log_q=log(fit$prior$q_shape/fit$prior$q_rate)+offsets$start_q_log_offsets[chain],
        log_eta=fit$bandwidth$start_log_offsets[chain])
    graphmode_h_validate_state(state,fit); state
}

graphmode_h_logprior <- function(log_eta, bandwidth) {
    # Gamma prior on eta plus d eta/d log(eta); d_ref is a fixed unit.
    bandwidth$shape*log_eta-bandwidth$rate*exp(log_eta)
}

graphmode_h_gate_target <- function(core_state, Phi, config) {
    candidate <- config; candidate$Phi <- Phi
    graphmode_gate_loglik(graphmode_gate_utilities(core_state$x,core_state$v,candidate),core_state$Z)
}

graphmode_h_step <- function(state, fit, base_step=graphmode_compare_step,
    normal=graphmode_normal, uniform=graphmode_uniform) {
    spectrum <- graphmode_h_spectrum(fit,state$log_eta)
    config <- graphmode_h_current_config(fit,state$log_scale,state$log_q,state$log_eta,spectrum)
    graphmode_h_validate_state(state,fit,config)
    out <- base_step(state$core,config,fit$policy)
    if(out$state$iteration!=state$core$iteration+1L) stop("Incomplete base transition.")
    next_state <- state; next_state$core <- out$state
    scale <- graphmode_c8_scalar_mh(state$log_scale,function(z)
        graphmode_h_gate_target(next_state$core,graphmode_h_root(spectrum,z),config)+
        dnorm(z,fit$kernel$log_center,fit$prior$scale_log_sd,log=TRUE),
        fit$scale_proposal_sd,normal,uniform)
    next_state$log_scale <- scale$value
    # A complete covariance/root rebuild accompanies every proposed h.
    h <- graphmode_c8_scalar_mh(state$log_eta,function(z)
        graphmode_h_gate_target(next_state$core,
            graphmode_h_root(graphmode_h_spectrum(fit,z),next_state$log_scale),config)+
        graphmode_h_logprior(z,fit$bandwidth),fit$bandwidth$proposal_sd,normal,uniform)
    next_state$log_eta <- h$value
    q <- graphmode_c8_scalar_mh(state$log_q,function(z)
        graphmode_c8_q_logtarget(z,next_state$core$theta,config,fit$prior),
        fit$log_q_proposal_sd,normal,uniform)
    next_state$log_q <- q$value
    updated <- graphmode_h_current_config(fit,next_state$log_scale,next_state$log_q,next_state$log_eta)
    list(state=next_state,scale_update=scale,h_update=h,q_update=q,base=out,
        utilities=graphmode_gate_utilities(next_state$core$x,next_state$core$v,updated))
}

graphmode_h_check_transition <- function(before,out,fit,r) {
    config <- graphmode_h_current_config(fit,before$log_scale,before$log_q,before$log_eta)
    graphmode_compare_check(before$core,out$base,config,r)
    graphmode_h_validate_state(out$state,fit)
    if(!identical(out$base$state,out$state$core)) stop("Hyperparameter update changed the base state.")
    fields <- c(q_update="log_q",scale_update="log_scale",h_update="log_eta")
    for(name in names(fields)) {
        mh <- out[[name]];field <- fields[[name]]
        if(is.null(mh) || length(mh$accepted)!=1L || !is.logical(mh$accepted) || is.na(mh$accepted) ||
            any(!is.finite(c(mh$value,mh$proposed,mh$log_ratio))) ||
            !identical(mh$value,if(mh$accepted) mh$proposed else before[[field]]) ||
            !identical(out$state[[field]],mh$value)) stop("Invalid hyperparameter MH handoff.")
    }
    updated <- graphmode_h_current_config(fit,out$state$log_scale,out$state$log_q,out$state$log_eta)
    expected <- graphmode_gate_utilities(out$state$core$x,out$state$core$v,updated)
    if(!identical(out$utilities,expected)) stop("Stale covariance/utility handoff.")
    invisible(TRUE)
}
