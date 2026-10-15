# Exact selected definitions from R/graphmode_warmup.R
graphmode_warmup_initial <- function(config,chain,spec) {
    chain <- gmde_scalar_integer(chain,"chain",1L,4L)
    graphmode_revalidate_config(config)
    seed <- function(component,code)
        graphmode_pilot_seeded(spec$seeds[[sprintf("%s_%02d",component,chain)]],code)
    K <- config$K; n <- config$n
    if(K!=10L || config$guidance!="class-specific" || config$family!="poisson" ||
        config$dynamics!="dynamic") stop("Phase A requires ten dynamic Poisson experts.",call.=FALSE)
    raw <- list()
    tryCatch({
    Z <- raw$Z <- seed("Z", {
        labels <- sample.int(K,spec$start_occupancy[chain],replace=FALSE)
        rep(labels,length.out=n)[sample.int(n)]
    })
    a <- raw$a <- seed("v",switch(chain,.05+.2*graphmode_uniform(K),.75+.2*graphmode_uniform(K),
        seq(.05,.95,length.out=K)[sample.int(K)],.1+.8*graphmode_uniform(K)))
    d <- graphmode_gate_dimension(config)
    x <- raw$x <- seed("x",graphmode_normal(d))
    theta <- raw$theta <- seed("theta", {
        paths <- array(NA_real_,c(K,nrow(config$Fmat),ncol(config$Fmat)))
        for(k in seq_len(K)) {
            paths[k,,] <- graphmode_prior_expert(config$Fmat,
                config$m0,config$C0,config$G,config$W,config$dynamics)
            raw$theta <- paths
        }
        paths
    })
        if(length(a)!=K || any(!is.finite(a)) || any(a<=0 | a>=1))
            stop("Illegal initial guidance; no redraw or clipping.",call.=FALSE)
        state <- graphmode_initial_state(config,Z,theta=theta,x=x,v=qlogis(a))
        if(length(unique(Z))!=spec$start_occupancy[chain] ||
            any(!is.finite(graphmode_gate_utilities(state$x,state$v,config))))
            stop("Illegal dispersed initial state.",call.=FALSE)
        list(state=state,seeds=spec$seeds[sprintf("%s_%02d",c("Z","v","x","theta"),chain)],
            state_signature=graphmode_digest(state))
    },error=function(e) stop(structure(list(message=conditionMessage(e),call=NULL,
        chain=chain,raw=raw),class=c("graphmode_initial_failure","error","condition"))))
}

graphmode_warmup_starts_check <- function(starts,fit,spec) {
    if(length(starts)!=4L) stop("Require all four complete starts.",call.=FALSE)
    for(j in 1:4) {
        s <- starts[[j]]
        graphmode_revalidate_state(s$state,fit$core)
        if(s$state$iteration!=0L || !identical(s$state_signature,graphmode_digest(s$state)) ||
            !identical(s$seeds,spec$seeds[sprintf("%s_%02d",c("Z","v","x","theta"),j)]) ||
            length(unique(s$state$Z))!=spec$start_occupancy[j]) stop("Changed initial-state evidence.",call.=FALSE)
        a <- plogis(s$state$v)
        valid <- switch(j,all(a>=.05 & a<=.25),all(a>=.75 & a<=.95),
            max(abs(sort(a)-seq(.05,.95,length.out=10)))<1e-14,all(a>=.1 & a<=.9))
        if(!isTRUE(valid)) stop("Initial guidance outside its registered start region.",call.=FALSE)
    }
    for(field in c("x","v","theta")) if(anyDuplicated(vapply(starts,function(s)
        graphmode_digest(s$state[[field]]),character(1)))) stop("Duplicated continuous starts.",call.=FALSE)
    invisible(TRUE)
}
