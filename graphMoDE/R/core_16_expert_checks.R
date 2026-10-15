# Exact selected definitions from R/graphmode_blocks_run.R
graphmode_blocks_run_step <- function(state,config,policy) {
    start <- proc.time()[[3L]]
    out <- graphmode_expert_blocks_sweep(state,config,policy$gate,policy$block,cache_ffbs=TRUE)
    transition_seconds <- proc.time()[[3L]]-start
    next_state <- out$transition$state
    allocation <- graphmode_dev_allocation_parts(next_state,config,state$Z)
    d <- list(iteration=next_state$iteration,policy=policy,block_offset=out$block_offset,
        before_signature=graphmode_digest(state),after_signature=graphmode_digest(next_state),
        before_occupancy=tabulate(state$Z,config$K),x=next_state$x,v=next_state$v,Z=next_state$Z,
        theta=next_state$theta,theta_signature=graphmode_digest(next_state$theta),sizes=out$transition$sizes,
        events=out$transition$events,
        expert=lapply(out$transition$expert_updates,function(e) {e$theta <- NULL;e}),
        gate=out$gate_refresh,allocation=allocation,transition_seconds=transition_seconds,
        total_seconds=proc.time()[[3L]]-start,
        outer_x_jump_squared=sum((next_state$x-state$x)^2),outer_v_jump_squared=(next_state$v-state$v)^2)
    list(state=next_state,diagnostic=d)
}

graphmode_blocks_run_expert_check <- function(e,before,after,Yk,config,policy,offset,thresholds) {
    bad <- function() stop("Block decision/boundary/numerical evidence mismatch.",call.=FALSE)
    equal <- function(a,b) isTRUE(all.equal(a,b,tolerance=1e-12,check.attributes=TRUE))
    finite <- function(x,n) is.numeric(x) && length(x)==n && all(is.finite(x) & x>=0)
    numerical <- function(x,empty=FALSE) {
        if(!finite(c(x$factor_residual,x$root_residual,x$movement,x$seconds),4L) ||
            x$factor_residual>thresholds$factor_residual_max || x$root_residual>thresholds$root_residual_max ||
            !empty && (!finite(x$root_reciprocal_condition,1L) || x$root_reciprocal_condition<=thresholds$root_rcond_min)) bad()
    }
    TT <- nrow(config$Fmat);layout <- graphmode_expert_blocks_layout(TT,policy$block_length,offset)
    empty <- nrow(Yk)==0L
    if(!identical(e$schema,graphmode_expert_blocks_version) || !identical(e$empty,empty) ||
        !is.null(e$accepted) || !is.null(e$log_acceptance) || !is.list(e$blocks) ||
        !identical(e$timing_scope,"complete expert update including observations; do not add observation_seconds again")) bad()
    numerical(e,empty)
    if(empty) {
        if(length(e$blocks) || length(e$r) || e$movement!=0 ||
            !identical(e$counts,list(proposals=0L,accepted=0L,acceptance=NA_real_,prior_refreshes=1L))) bad()
        return(invisible(TRUE))
    }
    S <- colSums(Yk);r <- gmde_make_nb_r(S,config$rho)
    if(length(e$blocks)!=nrow(layout) || !identical(e$r,r)) bad()
    accepted <- logical(nrow(layout));move <- numeric(nrow(layout))
    for(j in seq_len(nrow(layout))) {
        b <- e$blocks[[j]];idx <- seq.int(layout$start[j],layout$end[j]);o <- b$observation
        left <- if(min(idx)>1L) after[min(idx)-1L,] else NULL
        right <- if(max(idx)<TT) before[max(idx)+1L,] else NULL
        if(!identical(b$block,j) || !identical(b$start,layout$start[j]) || !identical(b$end,layout$end[j]) ||
            !identical(b$left,left) || !identical(b$right,right) || !identical(b$r,r[idx]) ||
            !is.logical(b$accepted) || length(b$accepted)!=1L || is.na(b$accepted) ||
            !is.numeric(b$log_acceptance) || length(b$log_acceptance)!=1L || !is.finite(b$log_acceptance) || b$log_acceptance>0 ||
            !b$accepted && !identical(after[idx,,drop=FALSE],before[idx,,drop=FALSE]) ||
            !identical(o$schema,graphmode_expert_blocks_version) || !identical(o$occupied_series,nrow(Yk)) ||
            !identical(o$accepted,b$accepted) || !identical(o$log_acceptance,b$log_acceptance) ||
            length(o$correction_by_time)!=length(idx) || any(!is.finite(o$correction_by_time)) ||
            !identical(sum(o$correction_by_time),o$log_ratio) || !identical(min(0,o$log_ratio),b$log_acceptance) ||
            !finite(c(b$kernel_seconds,b$observation_seconds,o$proposed_information_movement,o$accepted_information_movement),4L)) bad()
        move[j] <- sum(pmax(S[idx],1)*(rowSums(after[idx,,drop=FALSE]*config$Fmat[idx,,drop=FALSE])-
            rowSums(before[idx,,drop=FALSE]*config$Fmat[idx,,drop=FALSE]))^2)
        if(!equal(move[j],o$accepted_information_movement)) bad()
        view <- b;view$movement <- move[j];view$seconds <- b$kernel_seconds;numerical(view)
        accepted[j] <- b$accepted
    }
    expected <- list(proposals=length(accepted),accepted=sum(accepted),acceptance=mean(accepted),prior_refreshes=0L)
    if(!identical(e$counts,expected) || !equal(e$movement,sum(move)) ||
        !identical(e$factor_residual,max(vapply(e$blocks,`[[`,numeric(1),"factor_residual"))) ||
        !identical(e$root_residual,max(vapply(e$blocks,`[[`,numeric(1),"root_residual"))) ||
        !identical(e$root_reciprocal_condition,min(vapply(e$blocks,`[[`,numeric(1),"root_reciprocal_condition")))) bad()
    invisible(TRUE)
}
