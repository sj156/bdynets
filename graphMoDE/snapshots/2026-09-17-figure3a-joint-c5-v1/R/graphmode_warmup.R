# D-045 phase A. No sampling on source(); no phase-B implementation or release.
graphmode_warmup_version <- "graphmode-warmup-dispersed-20260914-a1"

graphmode_warmup_spec <- function() {
    s <- graphmode_dev_run_spec()
    s$schema <- graphmode_warmup_version
    s$iterations <- 1200L; s$warmup <- 1000L; s$checkpoint_every <- 100L
    s$policy <- graphmode4_guidance_policy(800L,25L,.44,1,1,.6,.05,4)
    s$worker_seconds <- 1200; s$dispatch_seconds <- 1260; s$total_seconds <- 5400
    s$diagnostic_seconds <- 600; s$reserve_seconds <- 1290
    s$storage <- c(stage_bytes=5*1024^3,start_free_bytes=15*1024^3,min_free_bytes=5*1024^3)
    s$seeds <- c(innovations=2026091501L,profile_permutation=2026091502L,
        unit_permutation=2026091503L,response=2026091504L,
        setNames(2026091511:2026091514,sprintf("Z_%02d",1:4)),
        setNames(2026091521:2026091524,sprintf("v_%02d",1:4)),
        setNames(2026091531:2026091534,sprintf("x_%02d",1:4)),
        setNames(2026091541:2026091544,sprintf("theta_%02d",1:4)),
        setNames(2026091551:2026091554,sprintf("chain_%02d",1:4)))
    s$screen <- list(last_updates=8L,within_ratio=1.5,between_ratio=2,acceptance=c(.25,.65))
    s$role <- "D-045 phase A warmup calibration/probe only; provisional rho4; no causal comparison, formal inference or automatic phase B"
    s
}

graphmode_warmup_identity <- function(repository) {
    base <- graphmode_dev_run_identity(repository)
    manifest <- "docs/provenance/graphmode-dev-run-2026-09-14.sha256"
    old <- utils::read.table(file.path(repository,manifest),stringsAsFactors=FALSE)
    expected <- setNames(old[[1L]],old[[2L]])
    files <- unique(c(names(base$sha256),manifest,"R/graphmode_warmup.R",
        "scripts/graphmode-warmup.R","scripts/tests/graphmode-warmup-deterministic.R",
        "docs/GRAPHMODE_WARMUP_IMPLEMENTATION_2026-09-14.md",
        "docs/GRAPHMODE_NEXT_WARMUP_PLAN_2026-09-14.md"))
    sha <- setNames(vapply(file.path(repository,files),function(f)
        digest::digest(file=f,algo="sha256",serialize=FALSE),character(1)),files)
    git <- function(args) {
        x <- system2("git",c("-C",shQuote(repository),args),stdout=TRUE,stderr=TRUE)
        if(!is.null(attr(x,"status"))) stop("Cannot inspect phase A source identity.",call.=FALSE)
        x
    }
    # Keep D-044's nested core identity shape for the unmodified lower adapters.
    base$sha256 <- sha
    base$audited_unchanged <- base$audited_unchanged &&
        identical(unname(sha[names(expected)]),unname(expected))
    base$committed <- base$committed && !length(git(c("status","--porcelain","--untracked-files=all","--",shQuote(files)))) &&
        setequal(files,git(c("ls-files","--",shQuote(files))))
    base
}

graphmode_warmup_record <- function(repository,directory,identity,runtime) {
    x <- list(schema=graphmode_warmup_version,spec=graphmode_warmup_spec(),repository=repository,
        directory=directory,output_dir=file.path(directory,"run"),identity=identity,runtime=runtime,
        authority="Phase A implementation approved 2026-09-14; this record is preparation only; a separate explicit run authorization is required",
        formal_authorized=FALSE,resume_supported=FALSE,phase_B_authorized=FALSE)
    x$signature <- graphmode_digest(x); x
}

# Initial-state draws have separate streams; the constructor sees only fit
# configuration, never generation labels, latent paths or fitted estimates.
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

graphmode_warmup_generate <- function(spec,persist) {
    if(!identical(spec,graphmode_warmup_spec())) stop("Changed phase A specification.",call.=FALSE)
    # Same deterministic panel constructor and four data component roles as
    # D-044, without calling its obsolete Z-only start sampler.
    s <- spec$seeds
    innovations <- graphmode_pilot_seeded(s[["innovations"]],array(graphmode_normal(5*168*3),c(5L,168L,3L)))
    profiles <- graphmode_pilot_seeded(s[["profile_permutation"]],sample.int(5L))
    units <- graphmode_pilot_seeded(s[["unit_permutation"]],sample.int(121L))
    lambda <- graphmode4_pilot_panel(spec$panel_spec,innovations,profiles,units)$lambda_original
    counts <- graphmode_pilot_seeded(s[["response"]],matrix(stats::rpois(length(lambda),lambda),121L,168L))
    panel <- graphmode4_pilot_panel(spec$panel_spec,innovations,profiles,units,counts)
    expert <- spec$panel_spec$expert;expert$rho <- spec$rho
    panel$fit <- graphmode4_config(panel$geometry,panel$fit$core$Y,panel$fit$core$Fmat,
        spec$panel_spec$method,expert,spec$panel_spec$gate,spec$role)
    persist(panel,"generation.rds")
    starts <- list()
    for(j in 1:4) {
        starts[[j]] <- graphmode_warmup_initial(panel$fit$core,j,spec)
        persist(starts[[j]],sprintf("INITIAL-%02d.rds",j))
    }
    graphmode_warmup_starts_check(starts,panel$fit,spec)
    blind <- list(schema=graphmode_warmup_version,fit=panel$fit,starts=starts)
    blind$signature <- graphmode_digest(blind)
    persist(blind,"blinded-inputs.rds")
    blind
}

# Passive timers wrap disjoint mathematical blocks. D-043 observers are
# accounted separately; all residual validation/allocation cost stays visible.
graphmode_warmup_sweep <- function(state,config,control,cache_ffbs=TRUE,observe=TRUE) {
    if(!isTRUE(cache_ffbs) || !isTRUE(observe)) stop("Phase A requires cache and complete observations.",call.=FALSE)
    started <- proc.time()[[3L]]
    times <- c(gate_ess=0,guidance=0,response_likelihood=0,categorical=0)
    timer <- function(fun,key) function(...) {
        t <- proc.time()[[3L]]; out <- fun(...)
        times[[key]] <<- times[[key]]+proc.time()[[3L]]-t
        out
    }
    timed <- graphmode_dev_bind(graphmode_sweep,list(
        graphmode_gate_ess=timer(graphmode_gate_ess,"gate_ess"),
        graphmode_guidance_update=timer(graphmode_guidance_update,"guidance"),
        graphmode_response_loglik=timer(graphmode_response_loglik,"response_likelihood"),
        graphmode_categorical=timer(graphmode_categorical,"categorical")))
    step <- graphmode_dev_bind(graphmode_dev_sweep,list(graphmode_sweep=timed))
    out <- step(state,config,control,cache_ffbs,observe)
    capture <- proc.time()[[3L]]; next_state <- out$transition$out$state
    trace <- list(schema=graphmode_warmup_version,iteration=next_state$iteration,
        before_signature=graphmode_digest(state),after_signature=graphmode_digest(next_state),
        x=next_state$x,v=next_state$v,Z=next_state$Z,
        theta_signature=graphmode_digest(next_state$theta),
        before_occupancy=tabulate(state$Z,nbins=config$K),
        logit_jump_squared=(next_state$v-state$v)^2,
        weight_jump_squared=(plogis(next_state$v)-plogis(state$v))^2)
    extra <- proc.time()[[3L]]-capture
    total <- proc.time()[[3L]]-started
    expert_seconds <- sum(vapply(out$transition$out$expert_updates,`[[`,numeric(1),"seconds"))
    measured <- c(experts=expert_seconds,times,observation=out$observation_seconds+extra)
    # Millisecond timer rounding can introduce tiny negative residuals.
    residual <- total-sum(measured)
    if(residual < -.02) stop("Overlapping phase A timing scopes.",call.=FALSE)
    trace$seconds <- c(measured,other=residual,total=total)
    trace$signature <- graphmode_digest(trace)
    out$observation$warmup <- trace
    out$observation_seconds <- out$observation_seconds+extra
    out
}

graphmode_warmup_trace_check <- function(e,result) {
    cp <- result$checkpoint; previous <- e$plan$core_plan$initial_state
    last_signature <- graphmode_digest(previous); K <- e$plan$fit$core$K
    for(i in seq_len(e$plan$core_plan$iterations)) {
        o <- cp$development$observations[[i]]$observation$warmup
        unsigned <- o;unsigned$signature <- NULL
        d <- cp$diagnostics[[i]]
        if(!identical(o$signature,graphmode_digest(unsigned)) ||
            !identical(o$schema,graphmode_warmup_version) || !identical(o$iteration,as.integer(i)) ||
            !identical(o$before_signature,last_signature) ||
            !is.character(o$after_signature) || length(o$after_signature)!=1L ||
            length(o$x)!=length(previous$x) || length(o$v)!=K || any(!is.finite(c(o$x,o$v))) ||
            !identical(o$before_occupancy,tabulate(previous$Z,nbins=K)) ||
            !identical(o$logit_jump_squared,(o$v-previous$v)^2) ||
            !identical(o$weight_jump_squared,(plogis(o$v)-plogis(previous$v))^2) ||
            any(o$logit_jump_squared[!d$guidance_accept]!=0) ||
            !identical(sum(o$logit_jump_squared),cp$development$observations[[i]]$observation$guidance$logit_jump_squared) ||
            !identical(names(o$seconds),c("experts","gate_ess","guidance","response_likelihood","categorical","observation","other","total")) ||
            any(!is.finite(o$seconds)) || any(o$seconds[-7L]<0) || o$seconds[["other"]] < -0.02 ||
            abs(sum(o$seconds[-8L])-o$seconds[["total"]])>1e-8)
            stop("Phase A passive trace mismatch.",call.=FALSE)
        gmde_validate_labels(o$Z,e$plan$fit$core$n,K)
        if(!identical(tabulate(o$Z,nbins=K),d$sizes) ||
            !identical(o$before_occupancy==0L,vapply(d$expert,`[[`,logical(1),"empty")))
            stop("Trace allocation/empty-expert classification mismatch.",call.=FALSE)
        previous <- o; last_signature <- o$after_signature
    }
    if(!identical(last_signature,graphmode_digest(cp$state)) || !identical(previous$x,cp$state$x) ||
        !identical(previous$v,cp$state$v) || !identical(previous$Z,cp$state$Z))
        stop("Final trace/state mismatch.",call.=FALSE)
    for(s in cp$saved) {
        o <- cp$development$observations[[s$iteration]]$observation$warmup
        if(!identical(s$v,o$v) || !identical(s$Z,o$Z) ||
            !identical(graphmode_digest(s$theta),o$theta_signature)) stop("Retained state/trace mismatch.",call.=FALSE)
    }
    invisible(TRUE)
}

graphmode_warmup_disk <- function(directory) {
    parent <- directory
    while(!dir.exists(parent)) parent <- dirname(parent)
    x <- system2("/bin/df",c("-Pk",shQuote(normalizePath(parent))),stdout=TRUE,stderr=TRUE)
    if(!is.null(attr(x,"status")) || length(x)<2L) stop("Cannot inspect disk space.",call.=FALSE)
    fields <- strsplit(trimws(tail(x,1L)),"[[:space:]]+")[[1L]]
    free <- suppressWarnings(as.numeric(fields[4L])*1024)
    files <- if(dir.exists(directory)) list.files(directory,recursive=TRUE,full.names=TRUE,all.files=TRUE) else character()
    sizes <- file.info(files)$size
    if(!is.finite(free) || anyNA(sizes)) stop("Incomplete disk usage measurement.",call.=FALSE)
    c(free_bytes=free,stage_bytes=sum(sizes))
}

graphmode_warmup_storage <- function(record,start=FALSE) {
    if(!identical(graphmode4_output_target(record$directory),record$directory))
        stop("Phase A storage destination changed.",call.=FALSE)
    x <- graphmode_warmup_disk(record$directory); limits <- record$spec$storage
    minimum <- if(start) limits[["start_free_bytes"]] else limits[["min_free_bytes"]]
    if(x[["free_bytes"]]<minimum || x[["stage_bytes"]]>limits[["stage_bytes"]])
        stop("Phase A storage budget exhausted; preserve files, no deletion/retry.",call.=FALSE)
    invisible(x)
}

# These emergency receipts remain writable when the normal storage guard
# fails. Path/source identity guards still apply; no directory is recreated.
graphmode_warmup_save <- function(record,object,path) {
    if(!identical(graphmode4_output_target(path),path) ||
        !startsWith(path,paste0(record$directory,"/")) || !dir.exists(dirname(path)))
        stop("Phase A save destination moved or escaped.",call.=FALSE)
    emergency <- basename(path) %in% c("failure.rds","batch-failure.rds","initial-failure.rds",
        "diagnostic-failure.rds","diagnostic-controller-failure.rds",
        "execution.rds","batch-execution.rds","diagnostic-execution.rds") ||
        grepl("^DISPATCH-[0-9]{2}[.]rds$",basename(path))
    if(!emergency) graphmode_warmup_storage(record)
    graphmode_save_new(object,path)
    if(!emergency) graphmode_warmup_storage(record)
    invisible(path)
}

graphmode_warmup_batch <- function(record,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Separate explicit phase A run authorization required.",call.=FALSE)
    api <- graphmode_warmup_engine(record)
    api$graphmode_dev_run_preflight(record,repository)
    graphmode_warmup_storage(record,TRUE)
    if(!dir.create(record$output_dir)) stop("Cannot create exclusive phase A output.",call.=FALSE)
    graphmode_warmup_save(record,list(record=record,authorized=TRUE),file.path(record$output_dir,"launch.rds"))
    started <- proc.time()[[3L]]; elapsed <- function() proc.time()[[3L]]-started
    save <- function(x,n) {api$graphmode_dev_run_tree(record,TRUE);graphmode_warmup_save(record,x,file.path(record$output_dir,n))}
    tryCatch({
        blind <- graphmode_warmup_generate(record$spec,save)
        plans <- lapply(1:4,function(j) api$graphmode_dev_run_envelope(record,j,blind$fit,blind$starts[[j]]$state))
        for(j in 1:4) save(plans[[j]],sprintf("PLAN-%02d.rds",j))
        for(j in 1:4) {
            api$graphmode_dev_run_source_guard(record,repository); graphmode_warmup_storage(record)
            if(!graphmode_warmup_time_left(record$spec,elapsed())) stop("Phase A reserve exhausted; no extension.",call.=FALSE)
            cat(format(Sys.time()),"Phase A chain",j,"/4:",record$spec$iterations,"sweeps; adaptation stops at",record$spec$policy$stop_at,"\n");flush.console()
            x <- graphmode_warmup_child("chain",file.path(record$output_dir,sprintf("PLAN-%02d.rds",j)),
                repository,record$spec$dispatch_seconds)
            save(x,sprintf("DISPATCH-%02d.rds",j))
            cat(paste(x$output,collapse="\n"),"\n");flush.console()
            if(!identical(x$status,0L)) stop("Phase A dispatcher failed/timed out; no retry.",call.=FALSE)
            api$graphmode_dev_run_worker_guard(plans[[j]],repository)
            api$graphmode_dev_run_evidence_check(plans[[j]],api$graphmode_dev_run_evidence(plans[[j]]))
        }
        if(elapsed()>=record$spec$total_seconds) stop("Phase A total budget exhausted.",call.=FALSE)
        save(list(schema=graphmode_warmup_version,registration_signature=record$signature,
            elapsed_seconds=elapsed(),formal_authorized=FALSE,phase_B_authorized=FALSE),"completed.rds")
        cat("Phase A completed. STOP: diagnostic review and separate approval required; no phase B.\n")
    },error=function(e) {
        tryCatch({
            if(inherits(e,"graphmode_initial_failure")) save(unclass(e),"initial-failure.rds")
            save(list(schema=graphmode_warmup_version,error=conditionMessage(e),elapsed_seconds=elapsed(),
                registration_signature=record$signature,formal_authorized=FALSE),"batch-failure.rds")
        },error=function(other) message("Original failure: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
}

graphmode_warmup_time_left <- function(spec,elapsed) is.finite(elapsed) && elapsed>=0 &&
    elapsed+spec$reserve_seconds<spec$total_seconds

graphmode_warmup_child <- function(command,path,repository,timeout,log_file=NULL) {
    if(!command %in% c("batch","chain","worker","diagnostic-worker")) stop("Not a phase A child.",call.=FALSE)
    child <- graphmode_dev_bind(graphmode_dev_run_child,list(
        # Change only the entry path, not quoting, timeout or exit capture.
        file.path=function(...) {
            parts <- list(...)
            parts <- lapply(parts,function(x) if(identical(x,"scripts/graphmode-dev-run.R")) "scripts/graphmode-warmup.R" else x)
            do.call(base::file.path,parts)
        }))
    child(command,path,repository,timeout,log_file)
}

# Private dependency injection: never alter a frozen function/global binding.
# Original adapter entry points call each other inside this namespace; the old
# controlled runner, path/timeout/receipt and numerical evidence stay in use.
graphmode_warmup_engine <- function(record=NULL) {
    original <- parent.env(environment())
    ctx <- new.env(parent=original)
    for(name in ls(original,pattern="^graphmode_dev_run_")) {
        value <- get(name,original)
        if(is.function(value)) environment(value) <- ctx
        assign(name,value,ctx)
    }
    ctx$graphmode_dev_run_version <- graphmode_warmup_version
    ctx$graphmode_dev_run_spec <- graphmode_warmup_spec
    ctx$graphmode_dev_run_identity <- graphmode_warmup_identity
    ctx$graphmode_dev_run_loaded <- get("graphmode_dev_run_loaded",original)
    ctx$graphmode_dev_run_record <- graphmode_warmup_record
    ctx$graphmode_dev_sweep <- graphmode_warmup_sweep
    ctx$graphmode_dev_run_child <- graphmode_warmup_child
    ctx$graphmode_dev_run_batch <- graphmode_warmup_batch
    old_preflight <- ctx$graphmode_dev_run_preflight
    ctx$graphmode_dev_run_preflight <- function(record,repository) {
        checked <- old_preflight(record,repository)
        graphmode_warmup_storage(record,TRUE)
        checked
    }
    old_result_check <- ctx$graphmode_dev_run_result_check
    ctx$graphmode_dev_run_result_check <- function(e,result) {
        old_result_check(e,result); graphmode_warmup_trace_check(e,result)
    }
    old_guard <- ctx$graphmode_dev_run_worker_guard
    ctx$graphmode_dev_run_worker_guard <- function(e,repository) {
        old_guard(e,repository)
        blind <- readRDS(file.path(e$record$output_dir,"blinded-inputs.rds"))
        unsigned <- blind;unsigned$signature <- NULL
        if(!identical(blind$signature,graphmode_digest(unsigned)) ||
            !identical(blind$schema,graphmode_warmup_version) ||
            !identical(blind$fit,e$plan$fit) || !identical(blind$starts[[e$chain]]$state,e$plan$core_plan$initial_state))
            stop("Changed blinded initialization/configuration.",call.=FALSE)
        graphmode_warmup_starts_check(blind$starts,blind$fit,e$record$spec)
        for(j in 1:4) if(!identical(blind$starts[[j]],
            readRDS(file.path(e$record$output_dir,sprintf("INITIAL-%02d.rds",j)))))
            stop("Initial-state archive changed or missing.",call.=FALSE)
        graphmode_warmup_storage(e$record)
        invisible(TRUE)
    }
    if(!is.null(record)) ctx$graphmode_save_new <- function(object,path) graphmode_warmup_save(record,object,path)
    ctx
}

# Pure, prospective reducer. Numerical/source/receipt checks must precede it.
graphmode_warmup_reduce <- function(metrics,spec) {
    if(length(metrics)!=4L) stop("Require four phase A chain summaries.",call.=FALSE)
    for(m in metrics) if(length(m$last_scales)!=spec$screen$last_updates ||
        length(m$final_scale)!=1L || length(m$acceptance)!=1L || length(m$guidance_movement)!=10L ||
        length(m$expert_updates)!=10L || length(m$expert_movement)!=10L ||
        any(!is.finite(unlist(m))) || any(m$last_scales<=0) || m$final_scale<=0 ||
        m$acceptance<0 || m$acceptance>1 || any(c(m$guidance_movement,m$expert_updates,m$expert_movement)<0) ||
        !identical(tail(m$last_scales,1L)[[1L]],m$final_scale)) stop("Malformed phase A screen evidence.",call.=FALSE)
    finals <- vapply(metrics,`[[`,numeric(1),"final_scale")
    per_chain <- lapply(metrics,function(m) c(
        late_scale_stable=all(m$last_scales>spec$policy$lower & m$last_scales<spec$policy$upper) &&
            max(m$last_scales)/min(m$last_scales)<=spec$screen$within_ratio,
        probe_acceptance=m$acceptance>=spec$screen$acceptance[1L] && m$acceptance<=spec$screen$acceptance[2L],
        guidance_moves=all(m$guidance_movement>0),
        occupied_experts_move=all(m$expert_movement[m$expert_updates>0]>0)))
    between <- max(finals)/min(finals)<=spec$screen$between_ratio
    resolved <- between && all(unlist(per_chain))
    list(status=if(resolved) "candidate-for-user-review" else "unresolved",resolved=resolved,
        candidate_scale=if(resolved) exp(mean(log(finals))) else NA_real_,final_scales=finals,
        chain_checks=per_chain,between_chain_scale_check=between,metrics=metrics,
        auto_continue=FALSE,phase_B_authorized=FALSE,convergence_certified=FALSE,formal_authorized=FALSE)
}

graphmode_warmup_diagnose <- function(record,repository) {
    api <- graphmode_warmup_engine(record)
    api$graphmode_dev_run_source_guard(record,repository)
    # Original 37-scalar criteria are unmodified and retained even if failed;
    # this short phase's engineering candidate is NOT a convergence verdict.
    report <- api$graphmode_dev_run_diagnose(record)
    metrics <- traces <- list()
    for(j in 1:4) {
        e <- readRDS(file.path(record$output_dir,sprintf("PLAN-%02d.rds",j)))
        api$graphmode_dev_run_worker_guard(e,repository)
        cp <- readRDS(file.path(e$plan$output_dir,"result.rds"))$checkpoint
        probe <- seq.int(record$spec$warmup+1L,record$spec$iterations)
        observations <- lapply(cp$development$observations,function(x) x$observation$warmup)
        accepts <- do.call(rbind,lapply(cp$diagnostics[probe],`[[`,"guidance_accept"))
        occupancy <- do.call(rbind,lapply(observations[probe],`[[`,"before_occupancy"))
        logit <- do.call(rbind,lapply(observations[probe],`[[`,"logit_jump_squared"))
        experts <- graphmode4_movement_report(cp$diagnostics,record$spec$warmup)$experts
        updates <- seq.int(record$spec$policy$batch_size,record$spec$policy$stop_at,by=record$spec$policy$batch_size)
        scales <- vapply(cp$diagnostics[updates],function(d) d$tuning$sd_next,numeric(1))
        metrics[[j]] <- list(last_scales=tail(scales,record$spec$screen$last_updates),final_scale=cp$control$sd,
            acceptance=mean(accepts),guidance_movement=colSums(logit),expert_updates=experts$occupied_updates,
            expert_movement=experts$accepted_movement)
        v <- do.call(rbind,lapply(observations,`[[`,"v"))
        x <- do.call(rbind,lapply(observations,`[[`,"x"))
        scalar <- graphmode4_trace(cp$saved,e$plan$fit)
        acf <- function(z) if(length(unique(z))<2L) rep(NA_real_,21L) else
            as.numeric(stats::acf(z,lag.max=20L,plot=FALSE)$acf)
        traces[[j]] <- list(x=x,v=v,
            Z=do.call(rbind,lapply(observations,`[[`,"Z")),
            guidance_probe_acceptance=colMeans(accepts),
            acceptance_by_occupancy=lapply(c(FALSE,TRUE),function(empty) {
                mask <- (occupancy==0L)==empty
                list(empty=empty,proposals=colSums(mask),accepted=colSums(accepts & mask))
            }),guidance_probe_acf=apply(plogis(v[probe,,drop=FALSE]),2L,acf),
            x_probe_acf=apply(x[probe,,drop=FALSE],2L,acf),
            required_scalars=scalar$scalars,required_scalar_acf=apply(scalar$scalars,2L,acf),
            similarity=scalar$similarity,representative_points=scalar$points,
            seconds=do.call(rbind,lapply(observations,`[[`,"seconds")),
            note="Raw guidance/x coordinates are chain-local; raw ACF is not sorted cross-chain Rhat/ESS or identifiability evidence.")
    }
    report$phase_A_screen <- graphmode_warmup_reduce(metrics,record$spec)
    report$traces <- traces
    steps <- sum(vapply(traces,function(t) sum(t$seconds[,"total"]),numeric(1)))
    report$timing <- list(batch_seconds=report$elapsed_seconds,measured_sweep_seconds=steps,
        batch_remainder_seconds=report$elapsed_seconds-steps,
        note="Batch remainder includes generation, initialization, process startup, serialization, validation and final acceptance; not cache speedup evidence.")
    report$note <- "Phase A stops regardless of candidate status. Short-probe Rhat/ESS is descriptive; no calibrated rho, speedup/causal claim, convergence certification or automatic phase B."
    api$graphmode_dev_run_source_guard(record,repository)
    report
}

graphmode_warmup_prepare <- function(repository,directory) {
    repository <- normalizePath(repository,mustWork=TRUE)
    directory <- graphmode4_output_target(directory)
    proposed <- graphmode_warmup_record(repository,directory,
        graphmode_warmup_identity(repository),graphmode4_pilot_runtime())
    graphmode_warmup_storage(proposed,TRUE)
    api <- graphmode_warmup_engine(proposed)
    api$graphmode_dev_run_prepare(repository,directory)
}

graphmode_warmup_diagnostic_worker <- function(record,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit diagnostic authorization required.",call.=FALSE)
    api <- graphmode_warmup_engine(record)
    api$graphmode_dev_run_source_guard(record,repository);api$graphmode_dev_run_tree(record,TRUE)
    ticket <- readRDS(file.path(record$directory,"diagnostic-request.rds"))
    if(!identical(ticket$registration_signature,record$signature) ||
        !identical(ticket$seconds,record$spec$diagnostic_seconds) ||
        any(file.exists(file.path(record$directory,c("diagnostic-execution.rds",
            "diagnostic-report.rds","diagnostic-failure.rds","diagnostic-controller-failure.rds",
            "diagnostic-acceptance-pending.rds","diagnostic-acceptance.rds")))))
        stop("Diagnostic request changed/already attempted; no retry.",call.=FALSE)
    tryCatch({
        report <- graphmode_warmup_diagnose(record,repository)
        graphmode_warmup_save(record,report,file.path(record$directory,"diagnostic-report.rds"))
        print(report$phase_A_screen)
        invisible(report$phase_A_screen)
    },error=function(e) {
        tryCatch(graphmode_warmup_save(record,list(error=conditionMessage(e),registration_signature=record$signature),
            file.path(record$directory,"diagnostic-failure.rds")),
            error=function(other) message("Diagnostic failure: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
}

# A child exit of zero is not controller acceptance. The receipt is first
# written under a pending name, so its own bytes must pass the storage guard.
# Only the final path, bound evidence and absence of BOTH failure markers count.
graphmode_warmup_diagnostic_receipt <- function(record,repository) {
    api <- graphmode_warmup_engine(record)
    api$graphmode_dev_run_source_guard(record,repository)
    api$graphmode_dev_run_tree(record,TRUE)
    if(any(file.exists(file.path(record$directory,c("diagnostic-failure.rds",
        "diagnostic-controller-failure.rds"))))) stop("Diagnostic failure evidence exists.",call.=FALSE)
    request <- readRDS(file.path(record$directory,"diagnostic-request.rds"))
    execution <- readRDS(file.path(record$directory,"diagnostic-execution.rds"))
    report <- readRDS(file.path(record$directory,"diagnostic-report.rds"))
    if(!identical(request,list(registration_signature=record$signature,seconds=record$spec$diagnostic_seconds)) ||
        !identical(execution,list(status=0L,registration_signature=record$signature,
            hard_budget_seconds=record$spec$diagnostic_seconds)) ||
        !identical(report$schema,graphmode_warmup_version) ||
        !identical(report$registration_signature,record$signature) ||
        !identical(report$phase_A_screen$auto_continue,FALSE) ||
        !identical(report$phase_A_screen$phase_B_authorized,FALSE)) stop("Diagnostic postflight failed.",call.=FALSE)
    list(schema="graphmode-warmup-diagnostic-acceptance-v2",registration_signature=record$signature,
        report_signature=graphmode_digest(report),execution_signature=graphmode_digest(execution),
        request_signature=graphmode_digest(request),postflight_passed=TRUE,phase_B_authorized=FALSE)
}

# Read-only consumer contract; never infer success from a report/boolean alone.
graphmode_warmup_diagnostic_acceptance <- function(record,repository) {
    expected <- graphmode_warmup_diagnostic_receipt(record,repository)
    final <- file.path(record$directory,"diagnostic-acceptance.rds")
    if(file.exists(file.path(record$directory,"diagnostic-acceptance-pending.rds")) ||
        !file.exists(final) || !identical(readRDS(final),expected))
        stop("Diagnostic acceptance missing, pending or changed.",call.=FALSE)
    graphmode_warmup_storage(record)
    invisible(TRUE)
}

graphmode_warmup_execute <- function(record,path,repository,diagnostic=FALSE,authorized=FALSE) {
    if(!identical(authorized,TRUE) || !is.logical(diagnostic) || length(diagnostic)!=1L || is.na(diagnostic))
        stop("Explicit phase A run/diagnostic authorization required.",call.=FALSE)
    api <- graphmode_warmup_engine(record)
    api$graphmode_dev_run_source_guard(record,repository)
    api$graphmode_dev_run_tree(record,diagnostic)
    if(!identical(normalizePath(path,mustWork=TRUE),file.path(record$directory,"registration.rds")) ||
        !identical(readRDS(path),record)) stop("Wrong registered entry path.",call.=FALSE)
    if(!diagnostic) api$graphmode_dev_run_preflight(record,repository)
    name <- if(diagnostic) "diagnostic" else "batch"
    log <- file.path(record$directory,if(diagnostic) "diagnostic.log" else "terminal.log")
    attempted <- c(log,file.path(record$directory,paste0(name,"-execution.rds")))
    if(diagnostic) attempted <- c(attempted,file.path(record$directory,c("diagnostic-request.rds",
        "diagnostic-report.rds","diagnostic-failure.rds","diagnostic-controller-failure.rds",
        "diagnostic-acceptance-pending.rds","diagnostic-acceptance.rds")))
    if(any(file.exists(attempted)))
        stop("Already attempted; no retry.",call.=FALSE)
    budget <- if(diagnostic) record$spec$diagnostic_seconds else record$spec$total_seconds
    stopped <- list(status="phase-A-stopped-for-review",diagnostic=diagnostic,phase_B_authorized=FALSE)
    tryCatch({
    if(diagnostic) graphmode_warmup_save(record,list(registration_signature=record$signature,seconds=budget),
        file.path(record$directory,"diagnostic-request.rds"))
    x <- graphmode_warmup_child(if(diagnostic) "diagnostic-worker" else "batch",
        normalizePath(path),repository,budget,log)
    api$graphmode_dev_run_tree(record)
    execution <- list(status=x$status,registration_signature=record$signature,hard_budget_seconds=budget)
    graphmode_warmup_save(record,execution,file.path(record$directory,paste0(name,"-execution.rds")))
    cat(x$output,sep="\n")
    if(!identical(x$status,0L)) stop("Phase A process failed/timed out; retain output, no retry.",call.=FALSE)
    api$graphmode_dev_run_source_guard(record,repository)
    api$graphmode_dev_run_tree(record,TRUE)
    if(diagnostic) {
        pending <- file.path(record$directory,"diagnostic-acceptance-pending.rds")
        final <- file.path(record$directory,"diagnostic-acceptance.rds")
        graphmode_warmup_save(record,graphmode_warmup_diagnostic_receipt(record,repository),pending)
        if(!identical(readRDS(pending),graphmode_warmup_diagnostic_receipt(record,repository)))
            stop("Diagnostic evidence changed before publication.",call.=FALSE)
        graphmode_warmup_storage(record)
        # The exclusive request reserves this once-only controller. Same-directory
        # rename adds no payload bytes; never overwrite or remove old evidence.
        # Publication is the last fallible operation: no later storage check or
        # receipt write can turn a committed success into an unrecorded failure.
        if(!identical(graphmode4_output_target(final),final) || file.exists(final) ||
            !file.rename(pending,final)) stop("Cannot publish diagnostic acceptance.",call.=FALSE)
    }
    invisible(stopped)
    },error=function(e) {
        if(diagnostic) tryCatch({
            api$graphmode_dev_run_tree(record)
            graphmode_warmup_save(record,list(schema=graphmode_warmup_version,
                registration_signature=record$signature,error=conditionMessage(e),accepted=FALSE),
                file.path(record$directory,"diagnostic-controller-failure.rds"))
        },error=function(other) message("Diagnostic controller failure: ",conditionMessage(e),
            "; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
}
