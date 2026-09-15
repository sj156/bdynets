# D-044: registered execution adapter over the unchanged audited D-043 layer.
graphmode_dev_run_version <- "graphmode-development-execution-20260914-v1"

graphmode_dev_run_spec <- function() {
    list(schema = graphmode_dev_run_version, panel_spec = graphmode4_pilot_spec(),
        rho = 4L, start_occupancy = c(1L,3L,7L,10L),
        iterations = 600L, warmup = 300L, thin = 1L, checkpoint_every = 50L,
        policy = graphmode4_guidance_policy(200L,25L,.44,1,1,.6,.05,4),
        cache_ffbs = TRUE, observe = TRUE, workers = 1L,
        worker_seconds = 600, dispatch_seconds = 630, total_seconds = 2700,
        seeds = c(innovations=2026091401L,profile_permutation=2026091402L,
            unit_permutation=2026091403L,response=2026091404L,
            start_01=2026091411L,start_02=2026091412L,start_03=2026091413L,start_04=2026091414L,
            chain_01=2026091421L,chain_02=2026091422L,chain_03=2026091423L,chain_04=2026091424L),
        role = "D-044 four-start development validation; rho4 is provisional, not calibrated; no paper use, cache-speed comparison or formal release")
}

graphmode_dev_run_identity <- function(repository) {
    base <- graphmode4_tuning_identity(repository)
    manifests <- c("docs/provenance/graphmode-rho-freeze-2026-09-12.sha256",
        "docs/provenance/graphmode-development-2026-09-14.sha256")
    expected <- unlist(lapply(manifests, function(f) {
        x <- utils::read.table(file.path(repository,f),stringsAsFactors=FALSE)
        setNames(x[[1L]],x[[2L]])
    }))
    files <- unique(c(names(expected),manifests,"R/graphmode_dev_run.R",
        "scripts/graphmode-dev-run.R","scripts/tests/graphmode-dev-run-deterministic.R",
        "docs/GRAPHMODE_DEV_RUN_2026-09-14.md"))
    sha <- setNames(vapply(file.path(repository,files),function(f)
        digest::digest(file=f,algo="sha256",serialize=FALSE),character(1)),files)
    git <- function(args) {
        x <- system2("git",c("-C",shQuote(repository),args),stdout=TRUE,stderr=TRUE)
        if(!is.null(attr(x,"status"))) stop("Cannot inspect execution freeze.",call.=FALSE)
        x
    }
    list(base=base,sha256=sha, audited_unchanged=identical(unname(sha[names(expected)]),unname(expected)),
        committed=base$committed && !length(git(c("status","--porcelain","--untracked-files=all","--",shQuote(files)))) &&
            setequal(files,git(c("ls-files","--",shQuote(files)))))
}

graphmode_dev_run_loaded <- function(repository, identity) {
    expected <- new.env(parent=baseenv())
    for(f in names(identity$sha256)[startsWith(names(identity$sha256),"R/")])
        sys.source(file.path(repository,f),expected)
    symbols <- ls(expected,all.names=TRUE)
    describe <- function(env) lapply(mget(symbols,env,inherits=TRUE),function(x)
        if(is.function(x)) list(formals=formals(x),body=body(x)) else x)
    identical(describe(expected),describe(environment(graphmode_dev_run_loaded)))
}

graphmode_dev_run_record <- function(repository,directory,identity,runtime) {
    x <- list(schema=graphmode_dev_run_version,spec=graphmode_dev_run_spec(),repository=repository,
        directory=directory,output_dir=file.path(directory,"run"),identity=identity,runtime=runtime,
        authority="User requested 2026-09-14: connect audited development layer, freeze/register, run a bounded small test in Terminal and diagnose; no formal run or extension",
        formal_authorized=FALSE,resume_supported=FALSE)
    x$signature <- graphmode_digest(x); x
}

graphmode_dev_run_validate <- function(record) {
    if(!is.list(record) || !identical(record$schema,graphmode_dev_run_version) ||
        !identical(record,do.call(graphmode_dev_run_record,
            record[names(formals(graphmode_dev_run_record))]))) stop("Changed development registration.",call.=FALSE)
    invisible(TRUE)
}

graphmode_dev_run_source_guard <- function(record,repository) {
    graphmode_dev_run_validate(record)
    identity <- graphmode_dev_run_identity(repository)
    if(!identical(normalizePath(repository),record$repository) || !identical(identity,record$identity) ||
        !isTRUE(identity$committed) || !isTRUE(identity$audited_unchanged) ||
        !isTRUE(identity$base$base$audited_core_unchanged) || !isTRUE(identity$base$base$r4$requirements_match) ||
        !graphmode_dev_run_loaded(repository,identity)) stop("Development source changed, not loaded, or unfrozen.",call.=FALSE)
    runtime <- graphmode4_pilot_runtime()
    if(!identical(runtime,record$runtime) || any(runtime$threads!="1") || runtime$locale!="C")
        stop("Development runtime changed; require single-thread C locale.",call.=FALSE)
    invisible(TRUE)
}

graphmode_dev_run_tree <- function(record,active=FALSE) {
    if(!dir.exists(record$directory) || !identical(normalizePath(record$directory),record$directory) ||
        !identical(readRDS(file.path(record$directory,"registration.rds")),record))
        stop("Development registration tree changed; never recreate it.",call.=FALSE)
    if(active && (!dir.exists(record$output_dir) || !identical(normalizePath(record$output_dir),record$output_dir) ||
        !identical(readRDS(file.path(record$output_dir,"launch.rds"))$record,record)))
        stop("Development output tree changed; never recreate it.",call.=FALSE)
    invisible(TRUE)
}

graphmode_dev_run_prepare <- function(repository,directory) {
    repository <- normalizePath(repository,mustWork=TRUE)
    directory <- graphmode4_output_target(directory)
    if(!dir.exists(dirname(directory)) || file.exists(directory) || identical(directory,repository) ||
        startsWith(directory,paste0(repository,"/"))) stop("Use a new external registration directory.",call.=FALSE)
    record <- graphmode_dev_run_record(repository,directory,graphmode_dev_run_identity(repository),graphmode4_pilot_runtime())
    graphmode_dev_run_source_guard(record,repository)
    if(!dir.create(directory)) stop("Cannot create exclusive registration.",call.=FALSE)
    graphmode_save_new(record,file.path(directory,"registration.rds"))
    graphmode_dev_run_tree(record)
    list(ready=TRUE,signature=record$signature,commit=record$identity$base$base$r4$core$commit,scientific_draws=0L)
}

graphmode_dev_run_preflight <- function(record,repository) {
    graphmode_dev_run_source_guard(record,repository); graphmode_dev_run_tree(record)
    if(file.exists(record$output_dir) || file.exists(file.path(record$directory,"batch-execution.rds")))
        stop("Already attempted; no retry or resume.",call.=FALSE)
    list(ready=TRUE,signature=record$signature,formal_authorized=FALSE)
}

# New independent inputs. Only the deterministic D-036 panel constructor is
# reused; its old seeds, PG moment screen and chain executor are not invoked.
graphmode_dev_run_generate <- function(spec) {
    if(!identical(spec,graphmode_dev_run_spec())) stop("Changed generation specification.",call.=FALSE)
    s <- spec$seeds
    innovations <- graphmode_pilot_seeded(s[["innovations"]],array(stats::rnorm(5*168*3),c(5L,168L,3L)))
    profiles <- graphmode_pilot_seeded(s[["profile_permutation"]],sample.int(5L))
    units <- graphmode_pilot_seeded(s[["unit_permutation"]],sample.int(121L))
    lambda <- graphmode4_pilot_panel(spec$panel_spec,innovations,profiles,units)$lambda_original
    counts <- graphmode_pilot_seeded(s[["response"]],matrix(stats::rpois(length(lambda),lambda),121L,168L))
    panel <- graphmode4_pilot_panel(spec$panel_spec,innovations,profiles,units,counts)
    # Rebuild the fit explicitly, not by mutating signed config fields.
    expert <- spec$panel_spec$expert; expert$rho <- spec$rho
    panel$fit <- graphmode4_config(panel$geometry,panel$fit$core$Y,panel$fit$core$Fmat,
        spec$panel_spec$method,expert,spec$panel_spec$gate,spec$role)
    starts <- lapply(1:4,function(j) graphmode_pilot_seeded(s[[sprintf("start_%02d",j)]],
        sample(rep(seq_len(spec$start_occupancy[j]),length.out=121L))))
    list(panel=panel,starts=starts)
}

graphmode_dev_run_envelope <- function(record,chain,fit,state) {
    graphmode_dev_run_validate(record); graphmode4_validate_config(fit)
    chain <- gmde_scalar_integer(chain,"chain",1L,4L); s <- record$spec
    if(fit$core$rho!=s$rho || fit$core$method!="graphMoDE-W" ||
        length(unique(state$Z))!=s$start_occupancy[chain]) stop("Wrong registered fit/start.",call.=FALSE)
    core <- graphmode_run_plan(fit$core,state,record$identity$base$base$r4$core,
        unname(s$seeds[[sprintf("chain_%02d",chain)]]),s$iterations,s$warmup,s$thin,s$checkpoint_every,
        s$worker_seconds,file.path(record$output_dir,sprintf("chain-%02d",chain)),
        paste0(basename(record$directory),"-",chain),record$authority,"development-pilot",s$panel_spec$thresholds,
        list(data_identity=graphmode_digest(fit$core$Y),geometry_identity=fit$geometry_identity,
            data_seed_record=paste(names(s$seeds),s$seeds,collapse=";"),
            permutation_seed_record=graphmode_digest(fit$ids),calibration_decision=s$role,scientific_role=s$role))
    plan <- graphmode4_controlled_plan(core,fit,s$policy,record$identity$base,record$runtime)
    e <- list(schema=graphmode_dev_run_version,record=record,chain=chain,plan=plan,
        cache_ffbs=s$cache_ffbs,observe=s$observe)
    e$signature <- graphmode_digest(e); e
}

graphmode_dev_run_envelope_check <- function(e) {
    if(!is.list(e) || !identical(e$schema,graphmode_dev_run_version)) stop("Not a development envelope.",call.=FALSE)
    rebuilt <- graphmode_dev_run_envelope(e$record,e$chain,e$plan$fit,e$plan$core_plan$initial_state)
    if(!identical(e,rebuilt)) stop("Development envelope changed.",call.=FALSE)
    invisible(TRUE)
}

graphmode_dev_run_worker_guard <- function(e,repository) {
    graphmode_dev_run_envelope_check(e)
    graphmode_dev_run_source_guard(e$record,repository); graphmode_dev_run_tree(e$record,TRUE)
    if(!identical(readRDS(file.path(e$record$output_dir,sprintf("PLAN-%02d.rds",e$chain))),e))
        stop("Persisted development envelope changed.",call.=FALSE)
    invisible(TRUE)
}

graphmode_dev_run_worker_preflight <- function(e,repository) {
    graphmode_dev_run_worker_guard(e,repository)
    graphmode4_controlled_preflight(e$plan,repository)
}

# Validate the separately versioned observation sequence AND the original
# numerical evidence. The compatibility view is in-memory only, never persisted.
graphmode_dev_run_result_check <- function(e,result) {
    graphmode_dev_run_envelope_check(e)
    cp <- result$checkpoint; core <- e$plan$core_plan; dev <- cp$development
    if(!identical(result$schema,graphmode_dev_run_version) || !identical(result$envelope_signature,e$signature) ||
        !identical(cp$schema,graphmode_dev_run_version) || !identical(dev$identity,e$record$identity) ||
        !identical(dev$version,graphmode_dev_version) || !identical(dev$envelope_signature,e$signature) ||
        !identical(dev$calls,core$iterations) || length(dev$observations)!=core$iterations)
        stop("Missing/changed actual development execution evidence.",call.=FALSE)
    view <- result; view$checkpoint$schema <- graphmode4_tuning_version
    graphmode4_controlled_result_check(e$plan,view)
    control <- graphmode4_guidance_control(core$config,core$warmup,e$plan$policy)
    for(i in seq_len(core$iterations)) {
        d <- cp$diagnostics[[i]]; o <- dev$observations[[i]]
        if(!identical(o$schema,graphmode_dev_version) || !identical(o$cache_ffbs,e$cache_ffbs) ||
            !is.finite(o$observation_seconds) || o$observation_seconds<0 ||
            !identical(d$tuning$sd_used,control$sd) || !identical(d$tuning$frozen_used,control$frozen) ||
            !identical(d$tuning$rho,core$config$rho) ||
            !identical(o$observation$guidance$scale,d$tuning) ||
            !identical(o$observation$guidance$acceptance,d$guidance_accept) ||
            length(o$observation$experts)!=core$config$K)
            stop("Development observation/tuning sequence mismatch.",call.=FALSE)
        for(k in seq_len(core$config$K)) {
            actual <- d$expert[[k]]; observed <- o$observation$experts[[k]]$observation
            if(actual$empty) {if(!is.null(observed)) stop("Empty expert has MH observation.",call.=FALSE)} else {
                if(!identical(observed$accepted,actual$accepted) ||
                    !identical(observed$log_acceptance,actual$log_acceptance) ||
                    !identical(observed$accepted_information_movement,actual$movement) ||
                    !identical(sum(observed$correction_by_time),observed$log_ratio) ||
                    !identical(min(0,observed$log_ratio),actual$log_acceptance) ||
                    length(observed$correction_by_time)!=ncol(core$config$Y) ||
                    any(!is.finite(c(observed$correction_by_time,observed$proposed_information_movement))) ||
                    observed$proposed_information_movement<0)
                    stop("Actual expert proposal record mismatch.",call.=FALSE)
            }
        }
        a <- o$observation$allocation$combined
        if(!identical(a$observed_node_moves,d$events[["node_moves"]]) ||
            length(a$log_switch_probability)!=core$config$n ||
            anyNA(a$log_switch_probability) || any(a$log_switch_probability>0) ||
            !is.finite(a$expected_node_moves) || a$expected_node_moves<0 || a$expected_node_moves>core$config$n)
            stop("Allocation observation mismatch.",call.=FALSE)
        control <- graphmode4_guidance_observe(control,d$guidance_accept,i)
        if(!identical(d$tuning$sd_next,control$sd) || !identical(d$tuning$frozen_next,control$frozen) ||
            i>core$warmup && !isTRUE(d$tuning$frozen_used)) stop("Guidance freeze evidence failed.",call.=FALSE)
    }
    if(!identical(control,cp$control)) stop("Final guidance control mismatch.",call.=FALSE)
    invisible(TRUE)
}

graphmode_dev_run_worker <- function(e,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit development worker authorization required.",call.=FALSE)
    checked <- graphmode_dev_run_worker_preflight(e,repository)
    if(!checked$ready) stop(paste(checked$problems,collapse="\n"),call.=FALSE)
    observations <- list(); calls <- 0L
    sweep <- function(state,config,control) {
        x <- graphmode_dev_sweep(state,config,control,e$cache_ffbs,e$observe)
        calls <<- calls+1L
        observations[[calls]] <<- x[c("schema","cache_ffbs","observation","observation_seconds")]
        x$transition
    }
    save <- function(object,path) {
        name <- basename(path)
        if(name=="registration.rds") {
            object$schema <- graphmode_dev_run_version; object$envelope <- e
        } else {
            graphmode_dev_run_tree(e$record,TRUE)
            registered <- readRDS(file.path(e$plan$output_dir,"registration.rds"))
            if(!identical(registered$envelope,e)) stop("Worker development envelope changed.",call.=FALSE)
            stamp <- function(cp) {
                cp$development <- list(version=graphmode_dev_version,identity=e$record$identity,
                    envelope_signature=e$signature,calls=calls,observations=observations)
                cp
            }
            if(name=="result.rds") {
                object$checkpoint <- stamp(object$checkpoint)
                object$schema <- graphmode_dev_run_version; object$envelope_signature <- e$signature
                graphmode_dev_run_result_check(e,object)
            } else object <- stamp(object)
        }
        graphmode_save_new(object,path)
        if(startsWith(name,"checkpoint-") && calls>0L)
            {cat(format(Sys.time()),"chain",e$chain,"sweep",calls,"cached development steps; frozen",object$control$frozen,"\n");flush.console()}
    }
    # Reuse audited persistence/failure/loop machinery; only version, preflight,
    # guard, saved observation envelope and actual sweep dependency are rebound.
    driver <- graphmode_dev_bind(graphmode4_controlled_run,list(
        graphmode4_tuning_version=graphmode_dev_run_version,
        graphmode4_controlled_preflight=function(...) checked,
        graphmode4_tuning_guard=function(...) graphmode_dev_run_worker_guard(e,repository),
        graphmode4_controlled_sweep=sweep,graphmode_save_new=save))
    driver(e$plan,repository,authorized=TRUE)
}

graphmode_dev_run_child <- function(command,path,repository,timeout,log_file=NULL) {
    if(!is.null(log_file) && file.exists(log_file)) stop("Child log already exists; no retry.",call.=FALSE)
    output <- suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
        c("--vanilla",shQuote(file.path(repository,"scripts/graphmode-dev-run.R")),command,
            shQuote(path),"--authorized"),stdout=if(is.null(log_file)) TRUE else log_file,
        stderr=if(is.null(log_file)) TRUE else log_file,timeout=ceiling(timeout)))
    status <- if(is.null(log_file)) attr(output,"status") else as.integer(output)
    if(!is.null(log_file)) output <- readLines(log_file,warn=FALSE)
    list(status=if(is.null(status)) 0L else as.integer(status),output=output)
}

graphmode_dev_run_evidence <- function(e) {
    errors <- character(); destination <- e$plan$output_dir
    read <- function(n) tryCatch(readRDS(file.path(destination,n)),error=function(x) {
        errors <<- c(errors,paste(n,conditionMessage(x))); NULL
    })
    x <- list(registration=read("registration.rds"),result=read("result.rds"),execution=read("execution.rds"),
        acceptance=read("acceptance.rds"),failure=file.exists(file.path(destination,"failure.rds")))
    x$read_errors <- errors; x
}

graphmode_dev_run_evidence_check <- function(e,x,receipt=TRUE) {
    if(!identical(x$read_errors,character()) || !identical(x$failure,FALSE) ||
        !identical(x$registration$envelope,e) || !identical(x$registration$plan,e$plan) ||
        !identical(x$registration$schema,graphmode_dev_run_version) ||
        !identical(x$execution$status,0L) || !identical(x$execution$envelope_signature,e$signature) ||
        !identical(x$execution$budget_seconds,e$record$spec$worker_seconds))
        stop("Missing/failed development launch evidence.",call.=FALSE)
    graphmode_dev_run_result_check(e,x$result)
    if(receipt && (!identical(x$acceptance$schema,graphmode_dev_run_version) ||
        !identical(x$acceptance$envelope_signature,e$signature) || !isTRUE(x$acceptance$postflight_passed) ||
        !identical(x$acceptance$result_signature,graphmode_digest(x$result)) ||
        !identical(x$acceptance$execution_signature,graphmode_digest(x$execution))))
        stop("Development final receipt missing or changed.",call.=FALSE)
    invisible(TRUE)
}

graphmode_dev_run_launch <- function(e,path,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit development launch authorization required.",call.=FALSE)
    checked <- graphmode_dev_run_worker_preflight(e,repository)
    if(!checked$ready || !identical(readRDS(path),e)) stop("Development preflight failed.",call.=FALSE)
    x <- graphmode_dev_run_child("worker",path,repository,e$record$spec$worker_seconds,
        file.path(e$record$output_dir,sprintf("WORKER-%02d.log",e$chain)))
    destination <- checked$output_dir
    if(dir.exists(destination) && identical(readRDS(file.path(destination,"registration.rds"))$envelope,e)) {
        x$envelope_signature <- e$signature; x$budget_seconds <- e$record$spec$worker_seconds
        graphmode_save_new(x,file.path(destination,"execution.rds"))
    }
    if(!identical(x$status,0L)) stop("Development worker failed/timed out; no retry.\n",paste(x$output,collapse="\n"),call.=FALSE)
    graphmode_dev_run_worker_guard(e,repository)
    if(!identical(normalizePath(destination),e$plan$output_dir) || !identical(readRDS(path),e))
        stop("Development output/envelope moved during execution.",call.=FALSE)
    evidence <- list(registration=readRDS(file.path(destination,"registration.rds")),
        result=readRDS(file.path(destination,"result.rds")),execution=x,
        failure=file.exists(file.path(destination,"failure.rds")),read_errors=character())
    graphmode_dev_run_evidence_check(e,evidence,FALSE)
    graphmode_save_new(list(schema=graphmode_dev_run_version,envelope_signature=e$signature,
        result_signature=graphmode_digest(evidence$result),execution_signature=graphmode_digest(x),postflight_passed=TRUE),
        file.path(destination,"acceptance.rds"))
    cat(paste(x$output,collapse="\n"),"\n")
    invisible(list(status="completed-not-convergence-certified",chain=e$chain))
}

graphmode_dev_run_time_left <- function(spec,elapsed) is.finite(elapsed) && elapsed>=0 &&
    elapsed+spec$dispatch_seconds+30<spec$total_seconds

graphmode_dev_run_batch <- function(record,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit development batch authorization required.",call.=FALSE)
    graphmode_dev_run_preflight(record,repository)
    if(!dir.create(record$output_dir)) stop("Cannot create exclusive development run.",call.=FALSE)
    graphmode_save_new(list(record=record,authorized=TRUE),file.path(record$output_dir,"launch.rds"))
    started <- proc.time()[[3L]]; elapsed <- function() proc.time()[[3L]]-started
    save <- function(x,n) {graphmode_dev_run_tree(record,TRUE); graphmode_save_new(x,file.path(record$output_dir,n))}
    log <- function(...) {cat(format(Sys.time()),...,"\n");flush.console()}
    tryCatch({
        log("Generating new registered D-044 input; no old checkpoint or response reuse.")
        generated <- graphmode_dev_run_generate(record$spec); save(generated,"generation.rds")
        envelopes <- lapply(1:4,function(j) graphmode_dev_run_envelope(record,j,generated$panel$fit,
            graphmode_initial_state(generated$panel$fit$core,generated$starts[[j]])))
        for(j in 1:4) save(envelopes[[j]],sprintf("PLAN-%02d.rds",j))
        for(j in 1:4) {
            graphmode_dev_run_source_guard(record,repository)
            if(!graphmode_dev_run_time_left(record$spec,elapsed())) stop("Total budget reserve exhausted; no extension.",call.=FALSE)
            log("Starting chain",j,"of 4; 600/300; cached FFBS; warmup tuning stops at 200.")
            x <- graphmode_dev_run_child("chain",file.path(record$output_dir,sprintf("PLAN-%02d.rds",j)),repository,record$spec$dispatch_seconds)
            save(x,sprintf("DISPATCH-%02d.rds",j)); log("Chain",j,"dispatcher exit",x$status,"elapsed",round(elapsed(),1))
            if(length(x$output)) cat(paste(x$output,collapse="\n"),"\n")
            if(!identical(x$status,0L)) stop("A development chain failed; stop without retry.",call.=FALSE)
            graphmode_dev_run_evidence_check(envelopes[[j]],graphmode_dev_run_evidence(envelopes[[j]]))
        }
        graphmode_dev_run_source_guard(record,repository)
        save(list(schema=graphmode_dev_run_version,registration_signature=record$signature,
            elapsed_seconds=elapsed(),formal_authorized=FALSE),"completed.rds")
        log("All four branches completed; diagnostics are separate, no convergence certification.")
    },error=function(e) {
        tryCatch(save(list(schema=graphmode_dev_run_version,error=conditionMessage(e),elapsed_seconds=elapsed(),
            registration_signature=record$signature,formal_authorized=FALSE),"batch-failure.rds"),
            error=function(other) message("Original failure: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
}

graphmode_dev_run_diagnose <- function(record) {
    graphmode_dev_run_validate(record); graphmode_dev_run_tree(record,TRUE)
    batch <- readRDS(file.path(record$directory,"batch-execution.rds"))
    completed <- readRDS(file.path(record$output_dir,"completed.rds"))
    if(!identical(batch$status,0L) || !identical(batch$registration_signature,record$signature) ||
        !identical(completed$registration_signature,record$signature) ||
        file.exists(file.path(record$output_dir,"batch-failure.rds"))) stop("Batch incomplete or failed; inspect retained logs.",call.=FALSE)
    chains <- details <- list(); envelopes <- lapply(1:4,function(j) readRDS(file.path(record$output_dir,sprintf("PLAN-%02d.rds",j))))
    for(j in 1:4) {
        e <- envelopes[[j]]
        if(!identical(e$record,record) || e$chain!=j) stop("Changed diagnostic input envelope.",call.=FALSE)
        x <- graphmode_dev_run_evidence(e); graphmode_dev_run_evidence_check(e,x)
        dispatcher <- readRDS(file.path(record$output_dir,sprintf("DISPATCH-%02d.rds",j)))
        if(!identical(dispatcher$status,0L)) stop("Dispatcher failed.",call.=FALSE)
        cp <- x$result$checkpoint; core <- e$plan$core_plan
        view <- cp; view$plan_signature <- core$signature; view$source_identity <- core$source_identity
        chains[[j]] <- graphmode4_pilot_chain_record(list(checkpoint=view,summary=list(),summary_signature=graphmode_digest(list())),
            core,e$plan$fit,record$spec$panel_spec$thresholds)
        retained <- seq.int(core$warmup+1L,core$iterations)
        obs <- cp$development$observations[retained]
        experts <- unlist(lapply(cp$diagnostics[retained],`[[`,"expert"),recursive=FALSE)
        values <- function(name) vapply(experts,function(z) z[[name]],numeric(1))
        accepted <- vapply(experts,function(z) !z$empty && isTRUE(z$accepted),logical(1))
        occupied <- !vapply(experts,`[[`,logical(1),"empty")
        details[[j]] <- list(chain=j,worker_seconds=cp$elapsed_seconds,
            expert_seconds=sum(values("seconds")),extra_observation_seconds=sum(vapply(obs,`[[`,numeric(1),"observation_seconds")),
            mh_acceptance=mean(accepted[occupied]),guidance_sd=cp$control$sd,adaptation_updates=cp$control$updates,
            guidance_acceptance=mean(unlist(lapply(cp$diagnostics[retained],`[[`,"guidance_accept"))),
            mean_guidance_logit_jump=mean(vapply(obs,function(o) o$observation$guidance$logit_jump_squared,numeric(1))),
            allocation_log_expected_range=range(vapply(obs,function(o) o$observation$allocation$combined$log_expected_node_moves,numeric(1))),
            numerical=list(max_root=max(values("root_residual")),max_factor=max(values("factor_residual")),
                min_rcond=min(values("root_reciprocal_condition")[occupied])),
            movement=graphmode4_movement_report(cp$diagnostics,core$warmup),
            result_sha256=digest::digest(file=file.path(e$plan$output_dir,"result.rds"),algo="sha256",serialize=FALSE))
    }
    s <- record$spec
    validity <- graphmode4_validity(chains,envelopes[[1L]]$plan$fit,s$seeds[sprintf("chain_%02d",1:4)],s$iterations,s$warmup,s$thin)
    validity$movement <- NULL
    list(schema=graphmode_dev_run_version,registration_signature=record$signature,details=details,validity=validity,
        elapsed_seconds=completed$elapsed_seconds,rho_selected=FALSE,convergence_certified=FALSE,formal_authorized=FALSE,
        note="Four independent starts, same provisional rho4 and frozen-within-chain guidance. No matched uncached arm: no speedup factor or causal mixing-improvement claim.")
}
