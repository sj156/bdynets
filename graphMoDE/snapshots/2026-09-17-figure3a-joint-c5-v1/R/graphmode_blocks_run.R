# D-056: prospective full-path/block comparison. No effects on source().
# The audited D-055 module and frozen D-051 controller files are not modified.
graphmode_blocks_run_version <- "graphmode-block-comparison-20260915-v1"

graphmode_blocks_run_base <- function(name) {
    get(paste0("graphmode_refresh_run_",name),environment(graphmode_blocks_run_context))
}

graphmode_blocks_run_spec <- function(candidate_scale) {
    s <- graphmode_blocks_run_base("spec")(candidate_scale)
    s$schema <- graphmode_blocks_run_version
    s$inner_steps <- NULL
    s$arms <- c("full","block42");s$gate_inner_steps <- 4L
    s$block_policies <- list(full=graphmode_expert_blocks_policy(168L,"fixed-zero"),
        block42=graphmode_expert_blocks_policy(42L,"uniform"))
    s$worker_seconds <- 1500;s$sweep_budget_seconds <- 1440
    s$total_seconds <- 13500;s$diagnostic_seconds <- 900;s$reserve_seconds <- 1620
    s$storage <- c(stage_bytes=16*1024^3,start_free_bytes=32*1024^3,min_free_bytes=5*1024^3)
    s$seeds <- s$seeds+100L
    names(s$seeds) <- sub("^m4_chain_","block_chain_",names(s$seeds))
    s$jobs <- data.frame(chain=rep(1:4,each=2),
        arm=c("full","block42","block42","full","full","block42","block42","full"))
    s$role <- "D-056 fresh paired expert-kernel engineering comparison, not adoption of the paper fallback; fixed m4/provisional rho4; no automatic choice, formal inference or extension"
    s
}

graphmode_blocks_run_identity <- function(repository) {
    base <- graphmode_blocks_run_base("identity")(repository)
    manifest <- "docs/provenance/graphmode-refresh-freeze-2026-09-15.sha256"
    old <- utils::read.table(file.path(repository,manifest),stringsAsFactors=FALSE)
    expected <- setNames(old[[1]],old[[2]])
    pins <- c("R/graphmode_expert_blocks.R"="cc3bb0c18afe25adb11a988e185fba149b250b733406bfd49da8428bf5214122",
        "scripts/graphmode-expert-blocks.R"="5183e33ddf42c4b5a18b6cabd4ae46b381c9923e4f3a9fb70486313e9d222bdc",
        "scripts/tests/graphmode-expert-blocks-deterministic.R"="d5822964a79df5a92c843e678b721e35c05807395b39dda852da04861c71e699",
        "docs/GRAPHMODE_EXPERT_BLOCKS_2026-09-15.md"="31b9398b3c6c45026d1cb82e9c64ceec0470256fc4d974281e3e01990ab08256")
    files <- unique(c(names(base$sha256),names(expected),manifest,names(pins),
        "R/graphmode_blocks_run.R","scripts/graphmode-blocks-run.R",
        "scripts/tests/graphmode-blocks-run-deterministic.R","docs/GRAPHMODE_BLOCK_COMPARISON_2026-09-15.md"))
    sha <- setNames(vapply(file.path(repository,files),function(f)
        digest::digest(file=f,algo="sha256",serialize=FALSE),character(1)),files)
    git <- function(args) {
        x <- system2("git",c("-C",shQuote(repository),args),stdout=TRUE,stderr=TRUE)
        if(!is.null(attr(x,"status"))) stop("Cannot inspect block source identity.",call.=FALSE)
        x
    }
    base$sha256 <- sha
    base$audited_unchanged <- base$audited_unchanged && length(expected)==68L &&
        identical(unname(sha[names(expected)]),unname(expected)) && identical(unname(sha[names(pins)]),unname(pins))
    base$committed <- base$committed &&
        !length(git(c("status","--porcelain","--untracked-files=all","--",shQuote(files)))) &&
        setequal(files,git(c("ls-files","--",shQuote(files))))
    base
}

graphmode_blocks_run_record <- function(repository,directory,identity,runtime,phase_A) {
    graphmode_validation_A_shape(phase_A)
    x <- list(schema=graphmode_blocks_run_version,spec=graphmode_blocks_run_spec(phase_A$candidate_scale),
        repository=repository,directory=directory,output_dir=file.path(directory,"run"),
        identity=identity,runtime=runtime,phase_A=phase_A,
        authority="D-056 preparation only; approve exact block comparison/budget before registration, then separately authorize run",
        formal_authorized=FALSE,resume_supported=FALSE,auto_continue=FALSE)
    x$signature <- graphmode_digest(x);x
}

graphmode_blocks_run_job <- function(record,chain,arm) {
    graphmode_refresh_run_validate(record)
    chain <- gmde_scalar_integer(chain,"chain",1L,4L)
    if(!is.character(arm) || length(arm)!=1L || !arm %in% record$spec$arms)
        stop("Unregistered block-comparison arm.",call.=FALSE)
    name <- sprintf("%s-chain-%02d",arm,chain)
    seed_name <- sprintf(if(arm=="full") "chain_%02d" else "block_chain_%02d",chain)
    x <- list(schema=graphmode_blocks_run_version,record=record,chain=chain,arm=arm,
        seed=unname(record$spec$seeds[[seed_name]]),name=name,directory=file.path(record$output_dir,name),
        policy=list(gate=graphmode_gate_refresh_policy(record$spec$gate_inner_steps,record$spec$candidate_scale),
            block=record$spec$block_policies[[arm]]))
    x$signature <- graphmode_digest(x);x
}

graphmode_blocks_run_jobs <- function(record) {
    lapply(seq_len(nrow(record$spec$jobs)),function(i)
        graphmode_refresh_run_job(record,record$spec$jobs$chain[i],record$spec$jobs$arm[i]))
}

graphmode_blocks_run_job_guard <- function(job,repository) {
    if(!identical(job,graphmode_refresh_run_job(job$record,job$chain,job$arm)))
        stop("Changed block-comparison job.",call.=FALSE)
    r <- job$record
    graphmode_refresh_run_guard(r,repository);graphmode_refresh_run_tree(r,TRUE)
    if(!identical(readRDS(file.path(r$output_dir,paste0(job$name,"-plan.rds"))),job))
        stop("Persisted block job changed.",call.=FALSE)
    blind <- readRDS(file.path(r$output_dir,"blinded-inputs.rds"))
    unsigned <- blind;unsigned$signature <- NULL
    if(!identical(blind$signature,graphmode_digest(unsigned)) || !identical(blind$schema,graphmode_blocks_run_version) ||
        !identical(readRDS(file.path(r$output_dir,"inputs-identity.rds")),
            list(registration_signature=r$signature,blinded_signature=blind$signature)))
        stop("Paired block input identity changed.",call.=FALSE)
    graphmode4_validate_config(blind$fit)
    graphmode_gate_refresh_validate(blind$fit$core,job$policy$gate)
    graphmode_expert_blocks_validate(blind$fit$core,job$policy$block)
    if(blind$fit$core$rho!=r$spec$rho || blind$fit$core$method!="graphMoDE-W" ||
        !identical(blind$fit$core$guidance_proposal_sd,r$spec$candidate_scale))
        stop("Wrong block comparison target/scale.",call.=FALSE)
    graphmode_warmup_starts_check(blind$starts,blind$fit,r$spec)
    for(j in 1:4) if(!identical(blind$starts[[j]],readRDS(file.path(r$output_dir,sprintf("INITIAL-%02d.rds",j)))))
        stop("Complete block-comparison start changed.",call.=FALSE)
    blind
}

# Persist complete outer theta to verify each block's boundary/rejection handoff
# after execution, including warmup. Inner proposal states are NOT saved draws.
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

graphmode_blocks_run_result_check <- function(job,result,blind) {
    cp <- result$checkpoint;s <- job$record$spec;config <- blind$fit$core
    if(!identical(result$schema,graphmode_blocks_run_version) || !identical(result$job_signature,job$signature) ||
        !identical(result$input_signature,blind$signature) || !identical(result$formal_authorized,FALSE) ||
        !identical(cp$schema,graphmode_blocks_run_version) || !identical(cp$policy,job$policy) ||
        !identical(cp$status,"completed-not-convergence-certified") || !identical(cp$plan_signature,job$signature) ||
        !identical(cp$source_identity,job$record$identity) || !identical(cp$completed_steps,s$iterations) ||
        !identical(cp$state$iteration,s$iterations) || !identical(cp$resume_supported,FALSE) ||
        !is.null(cp$error) || !is.list(cp$diagnostics) || length(cp$diagnostics)!=s$iterations ||
        !isTRUE(is.finite(cp$elapsed_seconds) && cp$elapsed_seconds>=0 && cp$elapsed_seconds<=s$sweep_budget_seconds))
        stop("Incomplete/changed block result.",call.=FALSE)
    expected <- seq.int(s$warmup+1L,s$iterations)
    if(length(cp$saved)!=length(expected) || !identical(vapply(cp$saved,`[[`,integer(1),"iteration"),expected))
        stop("Wrong retained outer states; blocks are not draws.",call.=FALSE)
    previous <- blind$starts[[job$chain]]$state
    for(i in seq_len(s$iterations)) {
        d <- cp$diagnostics[[i]];g <- d$gate
        # Reconstruct the complete scientific state, not a mutable diagnostic surrogate.
        state <- graphmode_initial_state(config,d$Z,d$theta,NULL,d$x,d$v);state$iteration <- i
        if(!identical(d$iteration,i) || !identical(d$policy,job$policy) ||
            !identical(d$before_signature,graphmode_digest(previous)) || !identical(d$after_signature,graphmode_digest(state)) ||
            !identical(d$theta_signature,graphmode_digest(d$theta)) ||
            !identical(d$before_occupancy,tabulate(previous$Z,config$K)) ||
            !identical(d$sizes,tabulate(d$Z,config$K)) || !identical(d$events,graphmode_allocation_events(previous$Z,d$Z,config$K)) ||
            !identical(d$allocation$combined$observed_node_moves,d$events[["node_moves"]]) ||
            !identical(d$outer_x_jump_squared,sum((d$x-previous$x)^2)) || !identical(d$outer_v_jump_squared,(d$v-previous$v)^2) ||
            !isTRUE(is.finite(d$total_seconds) && is.finite(d$transition_seconds) && d$transition_seconds>=0 && d$total_seconds>=d$transition_seconds) ||
            !identical(g$schema,graphmode_gate_refresh_version) || !identical(g$policy,job$policy$gate) ||
            !identical(g$inner_states_are_retained_draws,FALSE) ||
            !identical(g$counts,graphmode_gate_refresh_counts(g$records,job$policy$gate,length(previous$v))) ||
            !is.null(d$guidance_accept) || length(d$expert)!=config$K)
            stop("Block outer state/count handoff mismatch.",call.=FALSE)
        layout <- graphmode_expert_blocks_layout(ncol(config$Y),job$policy$block$block_length,d$block_offset)
        if(job$policy$block$offset_rule=="fixed-zero" && d$block_offset!=0L) stop("Wrong fixed offset.",call.=FALSE)
        for(gd in g$records) {
            vals <- c(gd$x_jump_squared,gd$logit_jump_squared,gd$weight_jump_squared,gd$ess_seconds,gd$guidance_seconds)
            if(length(gd$x_jump_squared)!=1L || length(gd$logit_jump_squared)!=config$K ||
                length(gd$weight_jump_squared)!=config$K || length(gd$ess_seconds)!=1L || length(gd$guidance_seconds)!=1L ||
                any(!is.finite(vals) | vals<0) || any(gd$logit_jump_squared[!gd$accepted]!=0) ||
                any(gd$weight_jump_squared[!gd$accepted]!=0)) stop("Invalid gate substep evidence.",call.=FALSE)
        }
        for(k in seq_len(config$K)) graphmode_blocks_run_expert_check(d$expert[[k]],
            matrix(previous$theta[k,,],ncol(config$Y),ncol(config$Fmat)),matrix(d$theta[k,,],ncol(config$Y),ncol(config$Fmat)),
            config$Y[previous$Z==k,,drop=FALSE],config,job$policy$block,d$block_offset,s$panel_spec$thresholds)
        if(i>s$warmup && !identical(cp$saved[[i-s$warmup]],state[c("iteration","Z","theta","sigma2","v","pi")]))
            stop("Retained block outer state changed.",call.=FALSE)
        previous <- state
    }
    if(!identical(previous,cp$state)) stop("Block final state mismatch.",call.=FALSE)
    list(complete=TRUE,config_signature=blind$fit$signature,seed=job$seed,iterations=s$iterations,
        warmup=s$warmup,thin=s$thin,draws=cp$saved,numerical_guards_passed=TRUE,
        movement=cp$diagnostics,evidence_identity=graphmode_digest(result),failure="")
}

graphmode_blocks_run_child <- function(command,path,repository,timeout,log_file=NULL) {
    if(!command %in% c("batch","worker","diagnostic-worker")) stop("Invalid block child.",call.=FALSE)
    launch <- graphmode_dev_bind(graphmode_dev_run_child,list(file.path=function(...) {
        pieces <- lapply(list(...),function(x) if(identical(x,"scripts/graphmode-dev-run.R")) "scripts/graphmode-blocks-run.R" else x)
        do.call(base::file.path,pieces)
    }))
    launch(command,path,repository,timeout,log_file)
}

# Only job enumeration differs from D-051; retain once-only launch, reserve,
# new-output checks and preservation of all failed/partial branches.
graphmode_blocks_run_batch <- function(record,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit block batch authorization required.",call.=FALSE)
    r <- record;graphmode_refresh_run_guard(r,repository);graphmode_refresh_run_tree(r)
    if(file.exists(r$output_dir) || !identical(readRDS(file.path(r$directory,"science-request.rds")),
        list(registration_signature=r$signature,budget_seconds=r$spec$total_seconds)))
        stop("Block batch already attempted or request missing.",call.=FALSE)
    graphmode_warmup_storage(r,TRUE)
    if(!dir.create(r$output_dir)) stop("Cannot create exclusive block batch.",call.=FALSE)
    save <- function(x,n,emergency=FALSE) graphmode_refresh_run_save(r,x,file.path(r$output_dir,n),emergency)
    save(list(record=r,authorized=TRUE),"launch.rds")
    start <- proc.time()[[3L]];elapsed <- function() proc.time()[[3L]]-start
    tryCatch({
        blind <- graphmode_refresh_run_generate(r,save)
        save(list(registration_signature=r$signature,blinded_signature=blind$signature),"inputs-identity.rds")
        jobs <- graphmode_blocks_run_jobs(r)
        for(j in jobs) save(j,paste0(j$name,"-plan.rds"))
        for(j in jobs) {
            graphmode_refresh_run_guard(r,repository);graphmode_warmup_storage(r)
            if(elapsed()+r$spec$reserve_seconds>=r$spec$total_seconds) stop("Block batch reserve exhausted; no extension.",call.=FALSE)
            cat(format(Sys.time()),"Starting",j$name,";",r$spec$iterations,"outer steps\n");flush.console()
            graphmode_refresh_run_launch(j,file.path(r$output_dir,paste0(j$name,"-plan.rds")),repository,TRUE)
            cat(format(Sys.time()),"Accepted",j$name,"(not convergence)\n");flush.console()
        }
        if(elapsed()>=r$spec$total_seconds) stop("Block batch budget exhausted.",call.=FALSE)
        save(list(schema=graphmode_blocks_run_version,registration_signature=r$signature,elapsed_seconds=elapsed(),
            job_signatures=vapply(jobs,`[[`,character(1),"signature"),formal_authorized=FALSE),"completed.rds")
    },error=function(e) {
        tryCatch({
            if(inherits(e,"graphmode_initial_failure")) save(unclass(e),"initial-failure.rds",TRUE)
            save(list(error=conditionMessage(e),registration_signature=r$signature),"batch-failure.rds",TRUE)
        },error=function(other) message("Original batch failure: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
}

graphmode_blocks_run_science_evidence <- function(record,repository,receipt=TRUE) {
    r <- record;graphmode_refresh_run_guard(r,repository);graphmode_refresh_run_tree(r,TRUE)
    if(any(file.exists(file.path(r$directory,c("science-failure.rds","run/batch-failure.rds")))))
        stop("Failed block comparison cannot be accepted.",call.=FALSE)
    request <- readRDS(file.path(r$directory,"science-request.rds"))
    execution <- readRDS(file.path(r$directory,"science-execution.rds"))
    completed <- readRDS(file.path(r$output_dir,"completed.rds"))
    if(!identical(request,list(registration_signature=r$signature,budget_seconds=r$spec$total_seconds)) ||
        !identical(execution$status,0L) || !identical(execution$registration_signature,r$signature) ||
        !identical(execution$budget_seconds,r$spec$total_seconds) ||
        !isTRUE(is.finite(execution$elapsed_seconds) && execution$elapsed_seconds>=0 && execution$elapsed_seconds<=r$spec$total_seconds) ||
        !identical(completed$schema,graphmode_blocks_run_version) || !identical(completed$registration_signature,r$signature) ||
        !identical(completed$formal_authorized,FALSE) ||
        !isTRUE(is.finite(completed$elapsed_seconds) && completed$elapsed_seconds>=0 && completed$elapsed_seconds<=r$spec$total_seconds))
        stop("Block comparison exit/identity/budget mismatch.",call.=FALSE)
    jobs <- graphmode_blocks_run_jobs(r);evidence <- lapply(jobs,graphmode_refresh_run_job_evidence,repository=repository)
    if(!identical(completed$job_signatures,vapply(jobs,`[[`,character(1),"signature"))) stop("Wrong block jobs.",call.=FALSE)
    expected <- list(schema=graphmode_blocks_run_version,registration_signature=r$signature,
        execution_signature=graphmode_digest(execution),request_signature=graphmode_digest(request),
        completed_signature=graphmode_digest(completed),job_receipts=vapply(evidence,function(e) graphmode_digest(e$receipt),character(1)),
        postflight_passed=TRUE,formal_authorized=FALSE)
    if(receipt && (file.exists(file.path(r$directory,"science-acceptance-pending.rds")) ||
        !identical(readRDS(file.path(r$directory,"science-acceptance.rds")),expected))) stop("Block science acceptance missing/changed.",call.=FALSE)
    list(receipt=expected,jobs=jobs,evidence=evidence,completed=completed)
}

# Offset changes block identities, so report per-time no-MH-accept streaks in
# OUTER sweeps, not concatenated blocks, and break the streak on empty experts.
graphmode_blocks_run_mechanism <- function(ds,spec) {
    retained <- ds[seq.int(spec$warmup+1L,spec$iterations)]
    K <- length(retained[[1]]$expert);TT <- dim(retained[[1]]$theta)[2L]
    streak <- longest <- matrix(0L,K,TT)
    proposals <- accepts <- prior <- seconds <- obs_seconds <- movement <- numeric(K)
    for(d in retained) for(k in seq_len(K)) {
        e <- d$expert[[k]];proposals[k] <- proposals[k]+e$counts$proposals
        accepts[k] <- accepts[k]+e$counts$accepted;prior[k] <- prior[k]+e$counts$prior_refreshes
        seconds[k] <- seconds[k]+e$seconds;movement[k] <- movement[k]+e$movement
        for(b in e$blocks) obs_seconds[k] <- obs_seconds[k]+b$observation_seconds
        if(e$empty) streak[k,] <- 0L else for(b in e$blocks) {
            at <- seq.int(b$start,b$end)
            streak[k,at] <- if(b$accepted) 0L else streak[k,at]+1L
        }
        longest[k,] <- pmax(longest[k,],streak[k,])
    }
    gp <- Reduce(`+`,lapply(retained,function(d) d$gate$counts$proposals_by_coordinate))
    ga <- Reduce(`+`,lapply(retained,function(d) d$gate$counts$accepted_by_coordinate))
    list(guidance_proposals=gp,guidance_accepted=ga,guidance_acceptance=ga/gp,
        expert=data.frame(expert=seq_len(K),block_proposals=proposals,block_accepts=accepts,
            block_acceptance=ifelse(proposals>0,accepts/proposals,NA_real_),empty_prior_refreshes=prior,
            retained_complete_seconds=seconds,observation_seconds_subset=obs_seconds,accepted_information_movement=movement),
        longest_time_rejection_outer_sweeps=longest,
        node_moves=sum(vapply(retained,function(d) d$events[["node_moves"]],numeric(1))),
        expected_node_moves=sum(vapply(retained,function(d) d$allocation$combined$expected_node_moves,numeric(1))),
        all_step_seconds=c(experts=sum(vapply(ds,function(d) sum(vapply(d$expert,`[[`,numeric(1),"seconds")),numeric(1))),
            gate=sum(vapply(ds,function(d) sum(vapply(d$gate$records,function(g) g$ess_seconds+g$guidance_seconds,numeric(1))),numeric(1))),
            total=sum(vapply(ds,`[[`,numeric(1),"total_seconds"))),
        note="Local MH counts only; time-wise streak counts outer sweeps and resets at empty prior refresh. Expert seconds already include observations; full worker time is the ESS denominator.")
}

graphmode_blocks_run_diagnose <- function(record,repository) {
    r <- record;x <- graphmode_refresh_run_science_evidence(r,repository)
    blind <- readRDS(file.path(r$output_dir,"blinded-inputs.rds"));arms <- setNames(vector("list",length(r$spec$arms)),r$spec$arms)
    for(arm in r$spec$arms) {
        indices <- which(vapply(x$jobs,`[[`,character(1),"arm")==arm)
        indices <- indices[order(vapply(x$jobs[indices],`[[`,integer(1),"chain"))];ev <- x$evidence[indices]
        a <- graphmode_refresh_run_arm_report(lapply(ev,`[[`,"chain"),blind$fit,
            vapply(x$jobs[indices],`[[`,integer(1),"seed"),r$spec,vapply(ev,function(e) e$execution$elapsed_seconds,numeric(1)))
        a$mechanism <- lapply(ev,function(e) graphmode_blocks_run_mechanism(e$result$checkpoint$diagnostics,r$spec))
        arms[[arm]] <- a
    }
    list(schema=graphmode_blocks_run_version,registration_signature=r$signature,science_receipt_signature=graphmode_digest(x$receipt),
        arms=arms,batch_elapsed_seconds=x$completed$elapsed_seconds,automatic_selection=FALSE,auto_continue=FALSE,formal_authorized=FALSE,
        note="Prospective block/full engineering comparison only, not proof of the paper fallback trigger or adoption; no pooled arms, reused old results or changed validity thresholds.")
}

graphmode_blocks_run_report_check <- function(report,record,science) {
    if(!identical(names(report$arms),record$spec$arms)) stop("Wrong block report arms.",call.=FALSE)
    for(arm in record$spec$arms) {
        idx <- which(vapply(science$jobs,`[[`,character(1),"arm")==arm)
        idx <- idx[order(vapply(science$jobs[idx],`[[`,integer(1),"chain"))]
        # Reuse the strict ORIGINAL 37-scalar/PSM/ESS-cost checker on a read-only
        # single-arm view. This label is not stored and does not change m (both4).
        sr <- record;sr$spec$inner_steps <- record$spec$gate_inner_steps
        sc <- science;sc$jobs <- lapply(science$jobs[idx],function(j) {j$m <- record$spec$gate_inner_steps;j})
        sc$evidence <- science$evidence[idx]
        rr <- list(arms=setNames(list(report$arms[[arm]]),paste0("m",record$spec$gate_inner_steps)))
        graphmode_blocks_run_base("report_check")(rr,sr,sc)
        expected <- lapply(sc$evidence,function(e) graphmode_blocks_run_mechanism(e$result$checkpoint$diagnostics,record$spec))
        if(!identical(report$arms[[arm]]$mechanism,expected)) stop("Block mechanism/timing/rejection report changed.",call.=FALSE)
    }
    invisible(TRUE)
}

graphmode_blocks_run_diagnostic_evidence <- function(record,repository,receipt=TRUE) {
    r <- record;sci <- graphmode_refresh_run_science_evidence(r,repository)
    if(file.exists(file.path(r$directory,"diagnostic-failure.rds"))) stop("Block diagnostic failed.",call.=FALSE)
    request <- readRDS(file.path(r$directory,"diagnostic-request.rds"));execution <- readRDS(file.path(r$directory,"diagnostic-execution.rds"))
    report <- readRDS(file.path(r$directory,"diagnostic-report.rds"))
    if(!identical(request,list(registration_signature=r$signature,budget_seconds=r$spec$diagnostic_seconds)) ||
        !identical(execution$status,0L) || !identical(execution$registration_signature,r$signature) ||
        !identical(execution$budget_seconds,r$spec$diagnostic_seconds) ||
        !isTRUE(is.finite(execution$elapsed_seconds) && execution$elapsed_seconds>=0 && execution$elapsed_seconds<=r$spec$diagnostic_seconds) ||
        !identical(report$schema,graphmode_blocks_run_version) || !identical(report$registration_signature,r$signature) ||
        !identical(report$science_receipt_signature,graphmode_digest(sci$receipt)) ||
        !identical(report$batch_elapsed_seconds,sci$completed$elapsed_seconds) ||
        !identical(report$automatic_selection,FALSE) || !identical(report$auto_continue,FALSE) || !identical(report$formal_authorized,FALSE))
        stop("Block diagnostic exit/identity/budget mismatch.",call.=FALSE)
    graphmode_refresh_run_report_check(report,r,sci)
    expected <- list(schema=graphmode_blocks_run_version,registration_signature=r$signature,
        execution_signature=graphmode_digest(execution),report_signature=graphmode_digest(report),request_signature=graphmode_digest(request),
        postflight_passed=TRUE,formal_authorized=FALSE)
    if(receipt && (file.exists(file.path(r$directory,"diagnostic-acceptance-pending.rds")) ||
        !identical(readRDS(file.path(r$directory,"diagnostic-acceptance.rds")),expected))) stop("Block diagnostic acceptance missing/changed.",call.=FALSE)
    expected
}

# Reuse D-051's unchanged path, source/runtime, worker, save/publish and timeout
# controllers in a private namespace. Never rebind its global implementation.
graphmode_blocks_run_context <- function() {
    original <- environment(graphmode_blocks_run_context);ctx <- new.env(parent=original)
    for(name in ls(original,pattern="^graphmode_refresh_run_")) {
        value <- get(name,original)
        if(is.function(value)) environment(value) <- ctx
        assign(name,value,ctx)
    }
    ctx$graphmode_refresh_run_version <- graphmode_blocks_run_version
    overrides <- c("spec","identity","record","job","job_guard","step","result_check","child","batch",
        "science_evidence","diagnose","report_check","diagnostic_evidence")
    for(name in overrides) {
        value <- get(paste0("graphmode_blocks_run_",name),original);environment(value) <- ctx
        assign(paste0("graphmode_refresh_run_",name),value,ctx)
    }
    for(name in c("jobs","expert_check","mechanism")) {
        value <- get(paste0("graphmode_blocks_run_",name),original);environment(value) <- ctx
        assign(paste0("graphmode_blocks_run_",name),value,ctx)
    }
    ctx
}
