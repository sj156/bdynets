# D-051: new, fixed-count comparison executor. source() does nothing stochastic.
# Never route this through A/B's single-scan guidance acceptance accounting.
graphmode_refresh_run_version <- "graphmode-refresh-comparison-20260915-v1"

graphmode_refresh_run_spec <- function(candidate_scale) {
    s <- graphmode_validation_spec(candidate_scale)
    s$schema <- graphmode_refresh_run_version
    s$iterations <- 1200L; s$warmup <- 600L; s$checkpoint_every <- 300L
    s$inner_steps <- c(1L,4L)
    s$worker_seconds <- 1320; s$sweep_budget_seconds <- 1260
    s$total_seconds <- 11700; s$diagnostic_seconds <- 900; s$reserve_seconds <- 1380
    s$dispatch_seconds <- NULL
    s$storage <- c(stage_bytes=12*1024^3,start_free_bytes=24*1024^3,min_free_bytes=5*1024^3)
    s$seeds <- c(innovations=2026091701L,profile_permutation=2026091702L,
        unit_permutation=2026091703L,response=2026091704L,
        setNames(2026091711:2026091714,sprintf("Z_%02d",1:4)),
        setNames(2026091721:2026091724,sprintf("v_%02d",1:4)),
        setNames(2026091731:2026091734,sprintf("x_%02d",1:4)),
        setNames(2026091741:2026091744,sprintf("theta_%02d",1:4)),
        setNames(2026091751:2026091754,sprintf("chain_%02d",1:4)),
        setNames(2026091761:2026091764,sprintf("m4_chain_%02d",1:4)))
    s$jobs <- data.frame(chain=rep(1:4,each=2),m=c(1L,4L,4L,1L,1L,4L,4L,1L))
    s$role <- "D-051 one fresh spiral graphMoDE-W panel, paired complete starts, independent arm streams; fixed m1 versus m4 engineering screen; no formal inference, automatic selection or extension"
    s
}

graphmode_refresh_run_identity <- function(repository) {
    base <- graphmode_validation_identity(repository)
    old_files <- names(base$sha256)
    manifest <- "docs/provenance/graphmode-validation-freeze-2026-09-14.sha256"
    old <- utils::read.table(file.path(repository,manifest),stringsAsFactors=FALSE)
    expected <- setNames(old[[1]],old[[2]])
    extra <- c(manifest,"R/graphmode_gate_refresh.R","scripts/graphmode-gate-refresh.R",
        "scripts/tests/graphmode-gate-refresh-deterministic.R","docs/GRAPHMODE_GATE_REFRESH_2026-09-15.md",
        "R/graphmode_refresh_run.R","scripts/graphmode-refresh-run.R",
        "scripts/tests/graphmode-refresh-run-deterministic.R","docs/GRAPHMODE_REFRESH_COMPARISON_2026-09-15.md")
    files <- unique(c(names(base$sha256),names(expected),extra))
    sha <- setNames(vapply(file.path(repository,files),function(f)
        digest::digest(file=f,algo="sha256",serialize=FALSE),character(1)),files)
    git <- function(args) {
        x <- system2("git",c("-C",shQuote(repository),args),stdout=TRUE,stderr=TRUE)
        if(!is.null(attr(x,"status"))) stop("Cannot inspect refresh source freeze.",call.=FALSE)
        x
    }
    base$sha256 <- sha
    old_blobs <- git(c("rev-parse",shQuote(paste0("2c5b862a3f70f618789d3bee59824594ccb767cc:",old_files))))
    current_blobs <- git(c("hash-object","--",shQuote(old_files)))
    base$audited_unchanged <- base$audited_unchanged && length(old_files)==55L &&
        identical(old_blobs,current_blobs) &&
        identical(unname(sha[names(expected)]),unname(expected)) &&
        identical(unname(sha[["R/graphmode_gate_refresh.R"]]),
            "7f1533088e16a0ee167d9cd9815c505e1dd8cfd82ec52d619e73e07c60c9bf41")
    base$committed <- base$committed &&
        !length(git(c("status","--porcelain","--untracked-files=all","--",shQuote(files)))) &&
        setequal(files,git(c("ls-files","--",shQuote(files))))
    base
}

graphmode_refresh_run_record <- function(repository,directory,identity,runtime,phase_A) {
    graphmode_validation_A_shape(phase_A)
    x <- list(schema=graphmode_refresh_run_version,spec=graphmode_refresh_run_spec(phase_A$candidate_scale),
        repository=repository,directory=directory,output_dir=file.path(directory,"run"),
        identity=identity,runtime=runtime,phase_A=phase_A,
        authority="D-051 preparation only; exact comparison/budget approval and separate run --authorized required",
        formal_authorized=FALSE,resume_supported=FALSE,auto_continue=FALSE)
    x$signature <- graphmode_digest(x); x
}

graphmode_refresh_run_validate <- function(record) {
    if(!is.list(record) || !identical(record,do.call(graphmode_refresh_run_record,
        record[names(formals(graphmode_refresh_run_record))])))
        stop("Changed or legacy comparison registration.",call.=FALSE)
    invisible(TRUE)
}

graphmode_refresh_run_guard <- function(record,repository) {
    graphmode_refresh_run_validate(record)
    guard <- graphmode_dev_bind(graphmode_dev_run_source_guard,list(
        graphmode_dev_run_validate=graphmode_refresh_run_validate,
        graphmode_dev_run_identity=graphmode_refresh_run_identity))
    guard(record,repository)
    graphmode_validation_A_check(record$phase_A,repository)
    invisible(TRUE)
}

graphmode_refresh_run_tree <- function(record,active=FALSE) {
    graphmode_dev_run_tree(record,active)
    if(!identical(graphmode4_output_target(record$directory),record$directory))
        stop("Comparison path changed; never recreate it.",call.=FALSE)
    invisible(TRUE)
}

graphmode_refresh_run_save <- function(record,object,path,emergency=FALSE) {
    graphmode_refresh_run_tree(record)
    if(!identical(graphmode4_output_target(path),path) ||
        !startsWith(path,paste0(record$directory,"/")) || !dir.exists(dirname(path)))
        stop("Comparison output moved or escaped.",call.=FALSE)
    if(!emergency) graphmode_warmup_storage(record)
    graphmode_save_new(object,path)
    if(!emergency) graphmode_warmup_storage(record)
    invisible(path)
}

# Publication is the LAST fallible step. A pending receipt is never acceptance.
graphmode_refresh_run_publish <- function(record,receipt,path,verify) {
    pending <- sub("[.]rds$","-pending.rds",path)
    if(identical(pending,path) || file.exists(path) || file.exists(pending))
        stop("Receipt already attempted.",call.=FALSE)
    graphmode_refresh_run_save(record,receipt,pending)
    verify()
    if(!identical(readRDS(pending),receipt)) stop("Pending receipt changed.",call.=FALSE)
    graphmode_warmup_storage(record)
    if(file.exists(path) || !file.rename(pending,path)) stop("Receipt publication failed.",call.=FALSE)
    invisible(NULL)
}

graphmode_refresh_run_prepare <- function(repository,directory,A_path,approved_plan=FALSE) {
    if(!identical(approved_plan,TRUE)) stop("Approve this comparison and budget before registration.",call.=FALSE)
    repository <- normalizePath(repository,mustWork=TRUE);directory <- graphmode4_output_target(directory)
    if(!dir.exists(dirname(directory)) || file.exists(directory) || identical(directory,repository) ||
        startsWith(directory,paste0(repository,"/"))) stop("Use a new external comparison directory.",call.=FALSE)
    evidence <- graphmode_validation_A_import(A_path,repository)
    r <- graphmode_refresh_run_record(repository,directory,graphmode_refresh_run_identity(repository),
        graphmode4_pilot_runtime(),evidence)
    graphmode_refresh_run_guard(r,repository);graphmode_warmup_storage(r,TRUE)
    if(!dir.create(directory)) stop("Cannot create exclusive registration.",call.=FALSE)
    graphmode_save_new(r,file.path(directory,"registration.rds"))
    graphmode_refresh_run_tree(r)
    list(ready=TRUE,signature=r$signature,scientific_draws=0L,run_authorized=FALSE)
}

graphmode_refresh_run_preflight <- function(record,repository) {
    graphmode_refresh_run_guard(record,repository);graphmode_refresh_run_tree(record)
    graphmode_warmup_storage(record,TRUE)
    if(any(file.exists(file.path(record$directory,c("run","science-request.rds","science-execution.rds",
        "science-failure.rds","science-acceptance-pending.rds","science-acceptance.rds")))))
        stop("Comparison already attempted; no retry/resume.",call.=FALSE)
    list(ready=TRUE,signature=record$signature,formal_authorized=FALSE)
}

# Reuse the frozen generator and initializer in a private context. It generates
# ONE new panel/four starts. The two arms consume those exact same frozen inputs.
graphmode_refresh_run_generate <- function(record,persist) {
    s <- record$spec
    make <- graphmode_dev_bind(graphmode_warmup_generate,list(
        graphmode_warmup_spec=function() s,
        graphmode_warmup_version=graphmode_refresh_run_version,
        graphmode4_config=function(road,Y,Fmat,method,expert,gate,calibration_id,...) {
            gate$guidance_proposal_sd <- s$candidate_scale
            graphmode4_config(road,Y,Fmat,method,expert,gate,calibration_id,...)
        }))
    make(s,persist)
}

graphmode_refresh_run_job <- function(record,chain,m) {
    graphmode_refresh_run_validate(record)
    chain <- gmde_scalar_integer(chain,"chain",1L,4L)
    m <- gmde_scalar_integer(m,"registered m",1L,4L)
    if(!m %in% record$spec$inner_steps) stop("Unregistered inner count.",call.=FALSE)
    name <- sprintf("m%d-chain-%02d",m,chain)
    seed_name <- sprintf(if(m==1L) "chain_%02d" else "m4_chain_%02d",chain)
    x <- list(schema=graphmode_refresh_run_version,record=record,chain=chain,m=m,
        seed=unname(record$spec$seeds[[seed_name]]),name=name,
        directory=file.path(record$output_dir,name),
        policy=graphmode_gate_refresh_policy(m,record$spec$candidate_scale))
    x$signature <- graphmode_digest(x);x
}

graphmode_refresh_run_job_guard <- function(job,repository) {
    if(!identical(job,graphmode_refresh_run_job(job$record,job$chain,job$m)))
        stop("Changed comparison job.",call.=FALSE)
    r <- job$record
    graphmode_refresh_run_guard(r,repository);graphmode_refresh_run_tree(r,TRUE)
    if(!identical(readRDS(file.path(r$output_dir,paste0(job$name,"-plan.rds"))),job))
        stop("Persisted comparison job changed.",call.=FALSE)
    blind <- readRDS(file.path(r$output_dir,"blinded-inputs.rds"))
    unsigned <- blind; unsigned$signature <- NULL
    if(!identical(blind$signature,graphmode_digest(unsigned)) ||
        !identical(blind$schema,graphmode_refresh_run_version) ||
        !identical(readRDS(file.path(r$output_dir,"inputs-identity.rds")),
            list(registration_signature=r$signature,blinded_signature=blind$signature)))
        stop("Paired blinded input identity changed.",call.=FALSE)
    graphmode4_validate_config(blind$fit)
    graphmode_gate_refresh_validate(blind$fit$core,job$policy)
    if(blind$fit$core$rho!=r$spec$rho || blind$fit$core$method!="graphMoDE-W")
        stop("Wrong comparison target.",call.=FALSE)
    graphmode_warmup_starts_check(blind$starts,blind$fit,r$spec)
    for(j in 1:4) if(!identical(blind$starts[[j]],readRDS(file.path(r$output_dir,sprintf("INITIAL-%02d.rds",j)))))
        stop("Complete start record changed.",call.=FALSE)
    blind
}

# Passive observation around the AUDITED D-050 entry; no second allocation draw.
graphmode_refresh_run_step <- function(state,config,policy) {
    start <- proc.time()[[3L]]
    out <- graphmode_gate_refresh_sweep(state,config,policy,cache_ffbs=TRUE)
    transition_seconds <- proc.time()[[3L]]-start
    next_state <- out$transition$state
    allocation <- graphmode_dev_allocation_parts(next_state,config,state$Z)
    d <- list(iteration=next_state$iteration,policy=policy,
        before_signature=graphmode_digest(state),after_signature=graphmode_digest(next_state),
        before_occupancy=tabulate(state$Z,config$K),x=next_state$x,v=next_state$v,Z=next_state$Z,
        theta_signature=graphmode_digest(next_state$theta),sizes=out$transition$sizes,
        events=out$transition$events,
        expert=lapply(out$transition$expert_updates,function(x) x[c("empty","accepted","log_acceptance",
            "movement","seconds","factor_residual","root_residual","root_reciprocal_condition")]),
        expert_observations=out$expert_observations,allocation=allocation,
        gate=out$gate_refresh,transition_seconds=transition_seconds,
        total_seconds=proc.time()[[3L]]-start,
        outer_x_jump_squared=sum((next_state$x-state$x)^2),
        outer_v_jump_squared=(next_state$v-state$v)^2)
    list(state=next_state,diagnostic=d)
}

graphmode_refresh_run_core_view <- function(job,blind) {
    s <- job$record$spec
    list(signature=job$signature,source_identity=job$record$identity,config=blind$fit$core,
        iterations=s$iterations,warmup=s$warmup,thin=s$thin,seed=job$seed,
        budget_seconds=s$sweep_budget_seconds)
}

# New persisted format; only an in-memory numerical-audit view uses the old
# accessor. Counts are always D-050 counts, never old one-scan guidance fields.
graphmode_refresh_run_result_check <- function(job,result,blind) {
    cp <- result$checkpoint; s <- job$record$spec; config <- blind$fit$core
    if(!identical(result$schema,graphmode_refresh_run_version) ||
        !identical(result$job_signature,job$signature) ||
        !identical(result$input_signature,blind$signature) ||
        !identical(result$formal_authorized,FALSE) ||
        !identical(cp$schema,graphmode_refresh_run_version) ||
        !identical(cp$policy,job$policy) || !identical(cp$completed_steps,s$iterations))
        stop("Wrong/incomplete refresh result identity.",call.=FALSE)
    view <- list(checkpoint=cp,summary=list(),summary_signature=graphmode_digest(list()))
    chain <- graphmode4_pilot_chain_record(view,graphmode_refresh_run_core_view(job,blind),
        blind$fit,s$panel_spec$thresholds)
    expected <- seq.int(s$warmup+1L,s$iterations)
    if(length(cp$saved)!=length(expected) || !identical(vapply(cp$saved,`[[`,integer(1),"iteration"),expected))
        stop("Wrong retained outer iterations; inner states cannot be draws.",call.=FALSE)
    previous <- blind$starts[[job$chain]]$state
    for(i in seq_len(s$iterations)) {
        d <- cp$diagnostics[[i]]; g <- d$gate
        if(!identical(d$iteration,i) || !identical(d$policy,job$policy) ||
            !identical(g$schema,graphmode_gate_refresh_version) || !identical(g$policy,job$policy) ||
            !identical(g$inner_states_are_retained_draws,FALSE) ||
            !identical(g$counts,graphmode_gate_refresh_counts(g$records,job$policy,length(previous$v))) ||
            !is.null(d$guidance_accept) || !identical(d$before_signature,
                if(i==1L) graphmode_digest(previous) else cp$diagnostics[[i-1L]]$after_signature) ||
            !identical(d$before_occupancy,tabulate(previous$Z,config$K)) ||
            length(d$x)!=length(previous$x) || length(d$v)!=length(previous$v) ||
            any(!is.finite(c(d$x,d$v,d$total_seconds,d$transition_seconds))) ||
            length(d$total_seconds)!=1L || length(d$transition_seconds)!=1L ||
            d$transition_seconds<0 || d$total_seconds<d$transition_seconds ||
            !identical(d$outer_x_jump_squared,sum((d$x-previous$x)^2)) ||
            !identical(d$outer_v_jump_squared,(d$v-previous$v)^2) ||
            !identical(d$sizes,tabulate(d$Z,config$K)) ||
            !identical(d$events,graphmode_allocation_events(previous$Z,d$Z,config$K)) ||
            !identical(d$allocation$combined$observed_node_moves,d$events[["node_moves"]]) ||
            length(d$expert_observations)!=config$K)
            stop("Refresh trace/count/state handoff mismatch.",call.=FALSE)
        gmde_validate_labels(d$Z,config$n,config$K)
        for(gd in g$records) {
            values <- c(gd$x_jump_squared,gd$logit_jump_squared,gd$weight_jump_squared,
                gd$ess_seconds,gd$guidance_seconds)
            if(length(gd$x_jump_squared)!=1L || length(gd$logit_jump_squared)!=config$K ||
                length(gd$weight_jump_squared)!=config$K || length(gd$ess_seconds)!=1L ||
                length(gd$guidance_seconds)!=1L || any(!is.finite(values) | values<0) ||
                any(gd$logit_jump_squared[!gd$accepted]!=0) ||
                any(gd$weight_jump_squared[!gd$accepted]!=0))
                stop("Invalid inner movement/timing evidence.",call.=FALSE)
        }
        for(k in seq_len(config$K)) {
            e <- d$expert[[k]]; o <- d$expert_observations[[k]]$observation
            if(!identical(e$empty,d$before_occupancy[k]==0L)) stop("Wrong empty-expert evidence.",call.=FALSE)
            if(e$empty) {
                if(!is.null(o)) stop("Empty expert has MH observations.",call.=FALSE)
            } else if(!identical(o$accepted,e$accepted) || !identical(o$log_acceptance,e$log_acceptance) ||
                !identical(o$accepted_information_movement,e$movement) ||
                !identical(sum(o$correction_by_time),o$log_ratio) ||
                !identical(min(0,o$log_ratio),e$log_acceptance) ||
                length(o$correction_by_time)!=ncol(config$Y) ||
                any(!is.finite(c(o$correction_by_time,o$proposed_information_movement))))
                stop("Original expert proposal evidence mismatch.",call.=FALSE)
        }
        previous <- d
    }
    if(!identical(tail(cp$diagnostics,1)[[1]]$after_signature,graphmode_digest(cp$state)))
        stop("Final state digest mismatch.",call.=FALSE)
    for(draw in cp$saved) {
        d <- cp$diagnostics[[draw$iteration]]
        if(!identical(draw$Z,d$Z) || !identical(draw$v,d$v) ||
            !identical(graphmode_digest(draw$theta),d$theta_signature))
            stop("Retained outer state changed.",call.=FALSE)
    }
    chain
}

graphmode_refresh_run_worker <- function(job,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit comparison worker authorization required.",call.=FALSE)
    blind <- graphmode_refresh_run_job_guard(job,repository);r <- job$record;s <- r$spec
    if(file.exists(job$directory) || !identical(graphmode4_output_target(job$directory),job$directory))
        stop("Worker already attempted or path changed.",call.=FALSE)
    graphmode_warmup_storage(r)
    if(!dir.create(job$directory)) stop("Cannot create exclusive worker output.",call.=FALSE)
    save <- function(x,n,emergency=FALSE) graphmode_refresh_run_save(r,x,file.path(job$directory,n),emergency)
    save(job,"registration.rds")
    state <- blind$starts[[job$chain]]$state;diagnostics <- saved <- list();completed <- 0L
    start <- proc.time()[[3L]];elapsed <- function() proc.time()[[3L]]-start
    budget <- function() if(elapsed()>=s$sweep_budget_seconds) stop("Sweep-boundary budget exhausted.",call.=FALSE)
    cp <- function(status,error=NULL) list(schema=graphmode_refresh_run_version,status=status,
        plan_signature=job$signature,source_identity=r$identity,policy=job$policy,completed_steps=completed,
        state=state,diagnostics=diagnostics,saved=saved,elapsed_seconds=elapsed(),error=error,
        rng_state=if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL,
        resume_supported=FALSE)
    graphmode_pilot_seeded(job$seed,tryCatch({
        save(cp("initialized"),"checkpoint-000000000.rds")
        for(i in seq_len(s$iterations)) {
            graphmode_refresh_run_tree(r,TRUE);budget()
            step <- graphmode_refresh_run_step(state,blind$fit$core,job$policy)
            state <- step$state;diagnostics[[i]] <- step$diagnostic;completed <- i
            if(i>s$warmup) saved[[length(saved)+1L]] <- state[c("iteration","Z","theta","sigma2","v","pi")]
            if(i%%s$checkpoint_every==0L) {
                graphmode_refresh_run_job_guard(job,repository);budget()
                save(cp("in-progress"),sprintf("checkpoint-%09d.rds",i))
                cat(format(Sys.time()),job$name,i,"/",s$iterations,"outer steps\n");flush.console()
            }
        }
        budget();graphmode_refresh_run_job_guard(job,repository)
        result <- list(schema=graphmode_refresh_run_version,job_signature=job$signature,
            input_signature=blind$signature,checkpoint=cp("completed-not-convergence-certified"),formal_authorized=FALSE)
        graphmode_refresh_run_result_check(job,result,blind);budget()
        # Final elapsed includes worker validation, not just transitions.
        result$checkpoint$elapsed_seconds <- elapsed()
        save(result,"result.rds");budget()
        invisible(NULL)
    },error=function(e) {
        tryCatch(save(cp("failed-retained-do-not-resume",conditionMessage(e)),"failure.rds",TRUE),
            error=function(other) message("Original failure: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    }))
}

graphmode_refresh_run_child <- function(command,path,repository,timeout,log_file=NULL) {
    if(!command %in% c("batch","worker","diagnostic-worker")) stop("Invalid comparison child.",call.=FALSE)
    launch <- graphmode_dev_bind(graphmode_dev_run_child,list(file.path=function(...) {
        pieces <- lapply(list(...),function(x) if(identical(x,"scripts/graphmode-dev-run.R")) "scripts/graphmode-refresh-run.R" else x)
        do.call(base::file.path,pieces)
    }))
    launch(command,path,repository,timeout,log_file)
}

graphmode_refresh_run_job_evidence <- function(job,repository,receipt=TRUE) {
    blind <- graphmode_refresh_run_job_guard(job,repository)
    d <- job$directory
    if(!identical(graphmode4_output_target(d),d) ||
        any(file.exists(file.path(d,c("failure.rds","controller-failure.rds")))) ||
        !identical(readRDS(file.path(d,"registration.rds")),job)) stop("Failed/moved worker.",call.=FALSE)
    execution <- readRDS(file.path(d,"execution.rds"));result <- readRDS(file.path(d,"result.rds"))
    if(!identical(execution$status,0L) || !identical(execution$job_signature,job$signature) ||
        !identical(execution$budget_seconds,job$record$spec$worker_seconds) ||
        length(execution$elapsed_seconds)!=1L || !is.finite(execution$elapsed_seconds) ||
        execution$elapsed_seconds<0 || execution$elapsed_seconds>execution$budget_seconds)
        stop("Worker exit/budget mismatch; existing results cannot override failure.",call.=FALSE)
    chain <- graphmode_refresh_run_result_check(job,result,blind)
    expected <- list(schema=graphmode_refresh_run_version,job_signature=job$signature,
        input_signature=blind$signature,result_signature=graphmode_digest(result),
        execution_signature=graphmode_digest(execution),postflight_passed=TRUE,formal_authorized=FALSE)
    if(receipt && (file.exists(file.path(d,"acceptance-pending.rds")) ||
        !identical(readRDS(file.path(d,"acceptance.rds")),expected))) stop("Worker acceptance missing/changed.",call.=FALSE)
    list(receipt=expected,chain=chain,result=result,execution=execution)
}

graphmode_refresh_run_launch <- function(job,path,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit worker launch authorization required.",call.=FALSE)
    graphmode_refresh_run_job_guard(job,repository)
    if(file.exists(job$directory) || !identical(readRDS(path),job)) stop("Worker already attempted/changed.",call.=FALSE)
    r <- job$record
    tryCatch({
        start <- proc.time()[[3L]]
        x <- graphmode_refresh_run_child("worker",path,repository,r$spec$worker_seconds,
            file.path(r$output_dir,paste0(job$name,".log")))
        x$elapsed_seconds <- proc.time()[[3L]]-start
        x$job_signature <- job$signature;x$budget_seconds <- r$spec$worker_seconds
        graphmode_refresh_run_save(r,x,file.path(r$output_dir,paste0(job$name,"-dispatch.rds")),TRUE)
        graphmode_refresh_run_job_guard(job,repository)
        graphmode_refresh_run_save(r,x,file.path(job$directory,"execution.rds"),TRUE)
        if(!identical(x$status,0L)) stop("Worker failed/timed out; no retry.",call.=FALSE)
        receipt <- graphmode_refresh_run_job_evidence(job,repository,FALSE)$receipt
        graphmode_refresh_run_publish(r,receipt,file.path(job$directory,"acceptance.rds"),function()
            graphmode_refresh_run_job_evidence(job,repository,FALSE))
    },error=function(e) {
        tryCatch(graphmode_refresh_run_save(r,list(error=conditionMessage(e),job_signature=job$signature),
            file.path(job$directory,"controller-failure.rds"),TRUE),error=function(other)
            message("Original controller failure: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
}

graphmode_refresh_run_batch <- function(record,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit comparison batch authorization required.",call.=FALSE)
    r <- record;graphmode_refresh_run_guard(r,repository);graphmode_refresh_run_tree(r)
    if(file.exists(r$output_dir) || !identical(readRDS(file.path(r$directory,"science-request.rds")),
        list(registration_signature=r$signature,budget_seconds=r$spec$total_seconds)))
        stop("Batch already attempted or request missing.",call.=FALSE)
    graphmode_warmup_storage(r,TRUE)
    if(!dir.create(r$output_dir)) stop("Cannot create exclusive batch.",call.=FALSE)
    save <- function(x,n,emergency=FALSE) graphmode_refresh_run_save(r,x,file.path(r$output_dir,n),emergency)
    save(list(record=r,authorized=TRUE),"launch.rds")
    started <- proc.time()[[3L]];elapsed <- function() proc.time()[[3L]]-started
    tryCatch({
        blind <- graphmode_refresh_run_generate(r,save)
        save(list(registration_signature=r$signature,blinded_signature=blind$signature),"inputs-identity.rds")
        jobs <- lapply(seq_len(nrow(r$spec$jobs)),function(i)
            graphmode_refresh_run_job(r,r$spec$jobs$chain[i],r$spec$jobs$m[i]))
        for(j in jobs) save(j,paste0(j$name,"-plan.rds"))
        for(j in jobs) {
            graphmode_refresh_run_guard(r,repository);graphmode_warmup_storage(r)
            if(elapsed()+r$spec$reserve_seconds>=r$spec$total_seconds)
                stop("Batch reserve exhausted; retain partial comparison, no extension.",call.=FALSE)
            cat(format(Sys.time()),"Starting",j$name,";",r$spec$iterations,"outer steps\n");flush.console()
            graphmode_refresh_run_launch(j,file.path(r$output_dir,paste0(j$name,"-plan.rds")),repository,TRUE)
            cat(format(Sys.time()),"Accepted",j$name,"(not convergence)\n");flush.console()
        }
        if(elapsed()>=r$spec$total_seconds) stop("Batch budget exhausted.",call.=FALSE)
        save(list(schema=graphmode_refresh_run_version,registration_signature=r$signature,
            elapsed_seconds=elapsed(),job_signatures=vapply(jobs,`[[`,character(1),"signature"),
            formal_authorized=FALSE),"completed.rds")
    },error=function(e) {
        tryCatch({
            if(inherits(e,"graphmode_initial_failure")) save(unclass(e),"initial-failure.rds",TRUE)
            save(list(error=conditionMessage(e),registration_signature=r$signature),"batch-failure.rds",TRUE)
        },error=function(other) message("Original batch failure: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
}

graphmode_refresh_run_science_evidence <- function(record,repository,receipt=TRUE) {
    r <- record;graphmode_refresh_run_guard(r,repository);graphmode_refresh_run_tree(r,TRUE)
    if(any(file.exists(file.path(r$directory,c("science-failure.rds","run/batch-failure.rds")))))
        stop("Failed comparison cannot be accepted.",call.=FALSE)
    request <- readRDS(file.path(r$directory,"science-request.rds"))
    execution <- readRDS(file.path(r$directory,"science-execution.rds"))
    completed <- readRDS(file.path(r$output_dir,"completed.rds"))
    if(!identical(request,list(registration_signature=r$signature,budget_seconds=r$spec$total_seconds)) ||
        !identical(execution$status,0L) || !identical(execution$registration_signature,r$signature) ||
        !identical(execution$budget_seconds,r$spec$total_seconds) ||
        !isTRUE(is.finite(execution$elapsed_seconds) && execution$elapsed_seconds>=0 && execution$elapsed_seconds<=r$spec$total_seconds) ||
        !identical(completed$schema,graphmode_refresh_run_version) ||
        !identical(completed$registration_signature,r$signature) || !identical(completed$formal_authorized,FALSE) ||
        !isTRUE(is.finite(completed$elapsed_seconds) && completed$elapsed_seconds>=0 && completed$elapsed_seconds<=r$spec$total_seconds))
        stop("Comparison exit/identity/budget mismatch.",call.=FALSE)
    jobs <- lapply(seq_len(nrow(r$spec$jobs)),function(i)
        graphmode_refresh_run_job(r,r$spec$jobs$chain[i],r$spec$jobs$m[i]))
    evidence <- lapply(jobs,graphmode_refresh_run_job_evidence,repository=repository)
    if(!identical(completed$job_signatures,vapply(jobs,`[[`,character(1),"signature")))
        stop("Wrong comparison jobs.",call.=FALSE)
    expected <- list(schema=graphmode_refresh_run_version,registration_signature=r$signature,
        execution_signature=graphmode_digest(execution),request_signature=graphmode_digest(request),
        completed_signature=graphmode_digest(completed),
        job_receipts=vapply(evidence,function(x) graphmode_digest(x$receipt),character(1)),
        postflight_passed=TRUE,formal_authorized=FALSE)
    if(receipt && (file.exists(file.path(r$directory,"science-acceptance-pending.rds")) ||
        !identical(readRDS(file.path(r$directory,"science-acceptance.rds")),expected)))
        stop("Comparison acceptance missing/changed.",call.=FALSE)
    list(receipt=expected,jobs=jobs,evidence=evidence,completed=completed)
}

graphmode_refresh_run_arm_report <- function(chains,fit,seeds,spec,seconds) {
    validity <- graphmode4_validity(chains,fit,seeds,spec$iterations,spec$warmup,spec$thin)
    if(length(seconds)!=4L || any(!is.finite(seconds) | seconds<=0)) stop("Invalid complete arm cost.",call.=FALSE)
    efficiency <- validity$scalars
    if(!is.null(efficiency)) {
        efficiency$bulk_ess_per_worker_second <- efficiency$bulk_ess/sum(seconds)
        efficiency$tail_ess_per_worker_second <- efficiency$tail_ess/sum(seconds)
        efficiency$precision_certified <- FALSE
    }
    list(validity=validity,efficiency=efficiency,worker_seconds=seconds,
        total_worker_seconds=sum(seconds),automatic_selection=FALSE,
        note="Pooled ESS divided by all four complete worker times including warmup/I/O. Failed Rhat or undefined ESS remain flags; rates are descriptive, not certified precision.")
}

# Structural postflight only: do not rerun posterior diagnostics at receipt time.
graphmode_refresh_run_report_check <- function(report,record,science) {
    s <- record$spec
    points <- expand.grid(id=c(1L,31L,61L,91L,121L),time=c(1L,84L,168L))
    expected_scalars <- c("log_likelihood","Kocc",paste0("proportion_",1:10),paste0("guidance_",1:10),
        paste0("unit_log_mean_",points$id,"_t",points$time))
    for(m in s$inner_steps) {
        arm <- report$arms[[paste0("m",m)]];v <- arm$validity;tab <- v$scalars
        indices <- which(vapply(science$jobs,`[[`,integer(1),"m")==m)
        indices <- indices[order(vapply(science$jobs[indices],`[[`,integer(1),"chain"))]
        seconds <- vapply(science$evidence[indices],function(e) e$execution$elapsed_seconds,numeric(1))
        if(!is.list(v) || !identical(v$protocol,graphmode4_protocol) ||
            !identical(v$formal_authorized,FALSE) || !identical(v$convergence_certified,FALSE) ||
            !is.data.frame(tab) || !identical(tab$scalar,expected_scalars) ||
            !identical(names(tab),c("scalar","status","rank_rhat","folded_rhat","bulk_ess","tail_ess")) ||
            anyNA(tab$status) || !all(tab$status %in% c("passed","failed","uninformative-constant-discrete",
                "failed-different-chain-constants","failed-undefined-constant")) ||
            !is.data.frame(v$psm_rms) || !identical(names(v$psm_rms),c("chain_a","chain_b","rms")) ||
            !identical(v$psm_rms$chain_a,combn(1:4,2)[1,]) || !identical(v$psm_rms$chain_b,combn(1:4,2)[2,]) ||
            any(!is.finite(v$psm_rms$rms) | v$psm_rms$rms<0) ||
            !identical(arm$worker_seconds,seconds) || !identical(arm$total_worker_seconds,sum(seconds)) ||
            !identical(arm$automatic_selection,FALSE) || length(arm$mechanism)!=4L)
            stop("Incomplete/mismatched arm diagnostic report.",call.=FALSE)
        pass <- tab$status=="passed"
        values <- as.matrix(tab[,c("rank_rhat","folded_rhat","bulk_ess","tail_ess")])
        if(any(!is.finite(values[pass,,drop=FALSE])) || any(tab$rank_rhat[pass]>1.01) ||
            any(tab$folded_rhat[pass]>1.01) || any(tab$bulk_ess[pass]<400) || any(tab$tail_ess[pass]<400) ||
            any(tab$status=="uninformative-constant-discrete" & !tab$scalar %in% c("Kocc",paste0("proportion_",1:10))))
            stop("Arm report incorrectly passes undefined/failed quantities.",call.=FALSE)
        failures <- paste0("statistical: ",tab$scalar[!tab$status %in% c("passed","uninformative-constant-discrete")])
        if(any(v$psm_rms$rms>.05)) failures <- c(failures,"statistical: pairwise PSM RMS exceeds 0.05")
        if(!identical(v$failures,failures) || !identical(v$valid,!length(failures)))
            stop("Original necessary validity flags changed.",call.=FALSE)
        efficiency <- tab
        efficiency$bulk_ess_per_worker_second <- tab$bulk_ess/sum(seconds)
        efficiency$tail_ess_per_worker_second <- tab$tail_ess/sum(seconds)
        efficiency$precision_certified <- FALSE
        if(!identical(arm$efficiency,efficiency)) stop("Efficiency denominator or flags changed.",call.=FALSE)
        for(j in 1:4) {
            mech <- arm$mechanism[[j]]
            proposals <- rep(as.numeric(m*(s$iterations-s$warmup)),10)
            if(!identical(mech$guidance_proposals,proposals) || length(mech$guidance_accepted)!=10L ||
                any(!is.finite(mech$guidance_accepted) | mech$guidance_accepted<0 | mech$guidance_accepted>proposals) ||
                !identical(mech$guidance_acceptance,mech$guidance_accepted/proposals))
                stop("Wrong multi-update diagnostic denominator.",call.=FALSE)
        }
    }
    invisible(TRUE)
}

graphmode_refresh_run_diagnose <- function(record,repository) {
    r <- record;x <- graphmode_refresh_run_science_evidence(r,repository)
    blind <- readRDS(file.path(r$output_dir,"blinded-inputs.rds"))
    arms <- setNames(vector("list",2),c("m1","m4"))
    for(m in r$spec$inner_steps) {
        indices <- which(vapply(x$jobs,`[[`,integer(1),"m")==m)
        indices <- indices[order(vapply(x$jobs[indices],`[[`,integer(1),"chain"))]
        evidence <- x$evidence[indices]
        report <- graphmode_refresh_run_arm_report(lapply(evidence,`[[`,"chain"),blind$fit,
            vapply(x$jobs[indices],`[[`,integer(1),"seed"),r$spec,
            vapply(evidence,function(e) e$execution$elapsed_seconds,numeric(1)))
        report$mechanism <- lapply(evidence,function(e) {
            ds <- e$result$checkpoint$diagnostics
            retained <- ds[seq.int(r$spec$warmup+1L,r$spec$iterations)]
            proposals <- Reduce(`+`,lapply(retained,function(d) d$gate$counts$proposals_by_coordinate))
            accepted <- Reduce(`+`,lapply(retained,function(d) d$gate$counts$accepted_by_coordinate))
            expert_report <- graphmode4_movement_report(ds,r$spec$warmup)
            expert_report$guidance_acceptance <- NULL # obsolete one-pass slot
            list(guidance_proposals=proposals,guidance_accepted=accepted,guidance_acceptance=accepted/proposals,
                outer_logit_movement=Reduce(`+`,lapply(retained,`[[`,"outer_v_jump_squared")),
                inner_logit_movement=Reduce(`+`,lapply(retained,function(d)
                    Reduce(`+`,lapply(d$gate$records,`[[`,"logit_jump_squared")))),
                expert=expert_report,
                node_moves=sum(vapply(retained,function(d) d$events[["node_moves"]],numeric(1))),
                expected_node_moves=sum(vapply(retained,function(d) d$allocation$combined$expected_node_moves,numeric(1))),
                all_step_seconds=c(experts=sum(vapply(ds,function(d) sum(vapply(d$expert,`[[`,numeric(1),"seconds")),numeric(1))),
                    gate=sum(vapply(ds,function(d) sum(vapply(d$gate$records,function(g) g$ess_seconds+g$guidance_seconds,numeric(1))),numeric(1))),
                    total=sum(vapply(ds,`[[`,numeric(1),"total_seconds"))))
        })
        arms[[paste0("m",m)]] <- report
    }
    list(schema=graphmode_refresh_run_version,registration_signature=r$signature,
        science_receipt_signature=graphmode_digest(x$receipt),arms=arms,
        batch_elapsed_seconds=x$completed$elapsed_seconds,
        automatic_selection=FALSE,auto_continue=FALSE,formal_authorized=FALSE,
        note="Fresh paired engineering screen only. Do not pool arms, infer model superiority, replace old B, waive its two failures or automatically choose m from this short screen.")
}

graphmode_refresh_run_diagnostic_worker <- function(record,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit comparison diagnostic authorization required.",call.=FALSE)
    r <- record
    if(!identical(readRDS(file.path(r$directory,"diagnostic-request.rds")),
        list(registration_signature=r$signature,budget_seconds=r$spec$diagnostic_seconds)) ||
        any(file.exists(file.path(r$directory,c("diagnostic-report.rds","diagnostic-execution.rds","diagnostic-failure.rds")))))
        stop("Diagnostic request changed/already attempted.",call.=FALSE)
    report <- graphmode_refresh_run_diagnose(r,repository)
    graphmode_refresh_run_guard(r,repository)
    graphmode_refresh_run_save(r,report,file.path(r$directory,"diagnostic-report.rds"))
    for(name in names(report$arms)) {cat(name,"\n");print(report$arms[[name]]$validity[c("valid","failures")])}
    invisible(NULL)
}

graphmode_refresh_run_diagnostic_evidence <- function(record,repository,receipt=TRUE) {
    r <- record;sci <- graphmode_refresh_run_science_evidence(r,repository)
    if(file.exists(file.path(r$directory,"diagnostic-failure.rds"))) stop("Diagnostic failed.",call.=FALSE)
    request <- readRDS(file.path(r$directory,"diagnostic-request.rds"))
    execution <- readRDS(file.path(r$directory,"diagnostic-execution.rds"))
    report <- readRDS(file.path(r$directory,"diagnostic-report.rds"))
    if(!identical(request,list(registration_signature=r$signature,budget_seconds=r$spec$diagnostic_seconds)) ||
        !identical(execution$status,0L) || !identical(execution$registration_signature,r$signature) ||
        !identical(execution$budget_seconds,r$spec$diagnostic_seconds) ||
        !isTRUE(is.finite(execution$elapsed_seconds) && execution$elapsed_seconds>=0 && execution$elapsed_seconds<=r$spec$diagnostic_seconds) ||
        !identical(report$schema,graphmode_refresh_run_version) || !identical(report$registration_signature,r$signature) ||
        !identical(report$science_receipt_signature,graphmode_digest(sci$receipt)) ||
        !identical(names(report$arms),c("m1","m4")) ||
        !identical(report$automatic_selection,FALSE) || !identical(report$auto_continue,FALSE) || !identical(report$formal_authorized,FALSE))
        stop("Diagnostic exit/identity/budget mismatch.",call.=FALSE)
    graphmode_refresh_run_report_check(report,r,sci)
    expected <- list(schema=graphmode_refresh_run_version,registration_signature=r$signature,
        execution_signature=graphmode_digest(execution),report_signature=graphmode_digest(report),
        request_signature=graphmode_digest(request),postflight_passed=TRUE,formal_authorized=FALSE)
    if(receipt && (file.exists(file.path(r$directory,"diagnostic-acceptance-pending.rds")) ||
        !identical(readRDS(file.path(r$directory,"diagnostic-acceptance.rds")),expected)))
        stop("Diagnostic acceptance missing/changed.",call.=FALSE)
    expected
}

graphmode_refresh_run_execute <- function(record,path,repository,diagnostic=FALSE,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Separate explicit comparison launch authorization required.",call.=FALSE)
    r <- record;prefix <- if(diagnostic) "diagnostic" else "science"
    graphmode_refresh_run_guard(r,repository);graphmode_refresh_run_tree(r)
    if(!identical(readRDS(path),r)) stop("Registration argument changed.",call.=FALSE)
    if(diagnostic) {
        graphmode_refresh_run_science_evidence(r,repository)
        if(any(file.exists(file.path(r$directory,paste0(prefix,c("-request.rds","-execution.rds",
            "-report.rds","-failure.rds","-acceptance.rds","-acceptance-pending.rds"))))))
            stop("Diagnosis already attempted; no rerun.",call.=FALSE)
    } else graphmode_refresh_run_preflight(r,repository)
    seconds <- if(diagnostic) r$spec$diagnostic_seconds else r$spec$total_seconds
    save <- function(x,suffix,emergency=FALSE) graphmode_refresh_run_save(r,x,
        file.path(r$directory,paste0(prefix,suffix)),emergency)
    save(list(registration_signature=r$signature,budget_seconds=seconds),"-request.rds")
    tryCatch({
        start <- proc.time()[[3L]]
        x <- graphmode_refresh_run_child(if(diagnostic) "diagnostic-worker" else "batch",path,repository,
            seconds,file.path(r$directory,paste0(prefix,"-terminal.log")))
        x$elapsed_seconds <- proc.time()[[3L]]-start
        x$registration_signature <- r$signature;x$budget_seconds <- seconds
        save(x,"-execution.rds",TRUE)
        if(!identical(x$status,0L)) stop("Comparison process failed/timed out; retain all evidence.",call.=FALSE)
        verify <- if(diagnostic) function() graphmode_refresh_run_diagnostic_evidence(r,repository,FALSE) else
            function() graphmode_refresh_run_science_evidence(r,repository,FALSE)$receipt
        receipt <- verify()
        graphmode_refresh_run_publish(r,receipt,file.path(r$directory,paste0(prefix,"-acceptance.rds")),verify)
    },error=function(e) {
        tryCatch(save(list(error=conditionMessage(e),registration_signature=r$signature),"-failure.rds",TRUE),
            error=function(other) message("Original error: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
    # No automatic diagnostic, second experiment, retry, or formal release.
    invisible(NULL)
}
