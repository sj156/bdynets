# D-048 phase B: fixed-scale validation over immutable A/development kernels.
# Sourcing this file performs no draws, file writes, launches or continuation.
graphmode_validation_version <- "graphmode-validation-dispersed-20260914-b1"

graphmode_validation_spec <- function(candidate_scale) {
    graphmode_positive(candidate_scale,"A candidate scale")
    if(length(candidate_scale)!=1L || candidate_scale<=.05 || candidate_scale>=4)
        stop("Invalid reviewed A scale.",call.=FALSE)
    s <- graphmode_warmup_spec()
    s$schema <- graphmode_validation_version
    s$iterations <- 6000L; s$warmup <- 1500L; s$checkpoint_every <- 500L
    s["policy"] <- list(NULL); s$screen <- NULL
    s$candidate_scale <- candidate_scale
    s$worker_seconds <- 5640; s$dispatch_seconds <- 5760; s$total_seconds <- 24000
    s$diagnostic_seconds <- 1200; s$reserve_seconds <- 5820
    s$storage <- c(stage_bytes=20*1024^3,start_free_bytes=40*1024^3,min_free_bytes=5*1024^3)
    s$seeds <- s$seeds+100L
    s$role <- "D-048 independent phase B fixed-scale four-chain development validation; provisional rho4; no formal inference, automatic extension or causal comparison"
    s
}

graphmode_validation_A_shape <- function(evidence) {
    required <- c("directory","registration_signature","source_commit","source_sha256",
        "file_sha256","candidate_scale","reviewed")
    if(!is.list(evidence) || !identical(names(evidence),required) ||
        !identical(evidence$reviewed,TRUE) ||
        !identical(evidence$source_commit,"b698fd83ef70e60e3b922229f88ee26464149655") ||
        !identical(evidence$registration_signature,"f248b516d6de752aa7e8b259d26023911256bf85e181001a13dd7a9aa18aefff") ||
        !identical(names(evidence$file_sha256),c("registration.rds","diagnostic-request.rds",
            "diagnostic-execution.rds","diagnostic-report.rds","diagnostic-acceptance.rds")) ||
        length(evidence$source_sha256)!=47L || anyDuplicated(names(evidence$source_sha256)) ||
        any(!grepl("^[0-9a-f]{64}$",c(evidence$source_sha256,evidence$file_sha256))))
        stop("Malformed or unreviewed A evidence.",call.=FALSE)
    graphmode_validation_spec(evidence$candidate_scale)
    invisible(TRUE)
}

# Historical A is allowed to precede HEAD, but its exact files and accepted
# evidence must remain unchanged. Never weaken the active B source guard.
graphmode_validation_A_check <- function(evidence,repository) {
    graphmode_validation_A_shape(evidence)
    directory <- evidence$directory
    if(!identical(normalizePath(directory,mustWork=TRUE),directory) ||
        any(file.exists(file.path(directory,c("diagnostic-failure.rds",
            "diagnostic-controller-failure.rds","diagnostic-acceptance-pending.rds","run/batch-failure.rds")))))
        stop("A evidence moved, pending or failed.",call.=FALSE)
    hash <- function(files) vapply(files,function(f) digest::digest(file=f,algo="sha256",serialize=FALSE),character(1))
    if(!identical(unname(hash(file.path(directory,names(evidence$file_sha256)))),unname(evidence$file_sha256)) ||
        !identical(unname(hash(file.path(repository,names(evidence$source_sha256)))),unname(evidence$source_sha256)))
        stop("Reviewed A evidence/source changed.",call.=FALSE)
    a <- readRDS(file.path(directory,"registration.rds"))
    report <- readRDS(file.path(directory,"diagnostic-report.rds"))
    if(!identical(evidence$source_sha256,a$identity$sha256) ||
        !identical(evidence$registration_signature,a$signature) ||
        !identical(evidence$source_commit,a$identity$base$base$r4$core$commit) ||
        !identical(evidence$candidate_scale,report$phase_A_screen$candidate_scale) ||
        !isTRUE(report$phase_A_screen$resolved) ||
        !identical(unname(evidence$file_sha256[c(1,4,5)]),c(
            "54b3ee5f66af2cdff6dd40f2999b2c9155b1269564a8e1aca89592e244305393",
            "2c49aa7602a7e1cdffd21a41eaf8fb1450b0f8d8f355a3730836205ae8c259fe",
            "ca2077a516e0aa7c8c6ac830b39e82e26df1f2dd7196e7c89c1b78353b95185b")))
        stop("A metadata/candidate is not bound to the reviewed files.",call.=FALSE)
    invisible(TRUE)
}

graphmode_validation_A_import <- function(path,repository) {
    path <- normalizePath(path,mustWork=TRUE); a <- readRDS(path)
    # Anchor to the reviewed A, not an arbitrary report with a plausible scale.
    pins <- c(registration.rds="54b3ee5f66af2cdff6dd40f2999b2c9155b1269564a8e1aca89592e244305393",
        diagnostic.report="2c49aa7602a7e1cdffd21a41eaf8fb1450b0f8d8f355a3730836205ae8c259fe",
        diagnostic.acceptance="ca2077a516e0aa7c8c6ac830b39e82e26df1f2dd7196e7c89c1b78353b95185b")
    files <- c("registration.rds","diagnostic-request.rds","diagnostic-execution.rds",
        "diagnostic-report.rds","diagnostic-acceptance.rds")
    hashes <- setNames(vapply(file.path(dirname(path),files),function(f)
        digest::digest(file=f,algo="sha256",serialize=FALSE),character(1)),files)
    if(!identical(unname(hashes[c(1,4,5)]),unname(pins)) ||
        !identical(path,file.path(a$directory,"registration.rds")))
        stop("Not the reviewed A registration/report/receipt.",call.=FALSE)
    graphmode_warmup_engine(a)$graphmode_dev_run_validate(a)
    report <- readRDS(file.path(a$directory,"diagnostic-report.rds"))
    screen <- graphmode_warmup_reduce(report$phase_A_screen$metrics,a$spec)
    if(!identical(screen,report$phase_A_screen) || !isTRUE(screen$resolved))
        stop("A candidate is unresolved or changed.",call.=FALSE)
    evidence <- list(directory=a$directory,registration_signature=a$signature,
        source_commit=a$identity$base$base$r4$core$commit,source_sha256=a$identity$sha256,
        file_sha256=hashes,candidate_scale=screen$candidate_scale,reviewed=TRUE)
    graphmode_validation_A_check(evidence,repository)
    # Reuse A's final receipt contract with a historical-file identity check.
    old_engine <- graphmode_warmup_engine
    historical <- function(record=NULL) {
        api <- old_engine(record)
        api$graphmode_dev_run_source_guard <- function(record,repository) {
            api$graphmode_dev_run_validate(record)
            graphmode_validation_A_check(evidence,repository)
            if(!identical(record,a) || !graphmode_dev_run_loaded(repository,a$identity))
                stop("Historical A loaded identity mismatch.",call.=FALSE)
            invisible(TRUE)
        }
        api
    }
    receipt <- graphmode_dev_bind(graphmode_warmup_diagnostic_receipt,
        list(graphmode_warmup_engine=historical))
    accept <- graphmode_dev_bind(graphmode_warmup_diagnostic_acceptance,
        list(graphmode_warmup_diagnostic_receipt=receipt))
    accept(a,repository)
    evidence
}

graphmode_validation_identity <- function(repository) {
    base <- graphmode_warmup_identity(repository)
    manifest <- "docs/provenance/graphmode-warmup-freeze-2026-09-14.sha256"
    old <- utils::read.table(file.path(repository,manifest),stringsAsFactors=FALSE)
    expected <- setNames(old[[1L]],old[[2L]])
    files <- unique(c(names(base$sha256),names(expected),manifest,"R/graphmode_validation.R",
        "scripts/graphmode-validation.R","scripts/tests/graphmode-validation-deterministic.R",
        "docs/GRAPHMODE_VALIDATION_IMPLEMENTATION_2026-09-14.md"))
    sha <- setNames(vapply(file.path(repository,files),function(f)
        digest::digest(file=f,algo="sha256",serialize=FALSE),character(1)),files)
    git <- function(args) {
        x <- system2("git",c("-C",shQuote(repository),args),stdout=TRUE,stderr=TRUE)
        if(!is.null(attr(x,"status"))) stop("Cannot inspect B source identity.",call.=FALSE)
        x
    }
    base$sha256 <- sha
    base$audited_unchanged <- base$audited_unchanged && identical(unname(sha[names(expected)]),unname(expected))
    base$committed <- base$committed && !length(git(c("status","--porcelain","--untracked-files=all","--",shQuote(files)))) &&
        setequal(files,git(c("ls-files","--",shQuote(files))))
    base
}

graphmode_validation_record <- function(repository,directory,identity,runtime,phase_A) {
    graphmode_validation_A_shape(phase_A)
    x <- list(schema=graphmode_validation_version,spec=graphmode_validation_spec(phase_A$candidate_scale),
        repository=repository,directory=directory,output_dir=file.path(directory,"run"),
        identity=identity,runtime=runtime,phase_A=phase_A,
        authority="User explicitly authorized phase B 2026-09-14: implement, freeze/register, four6000/1500 at reviewed fixed A scale, total7h, diagnose once and stop",
        formal_authorized=FALSE,resume_supported=FALSE,auto_continue=FALSE)
    x$signature <- graphmode_digest(x); x
}

# Clone only routing functions into a private namespace. Mathematical functions
# and all persisted A files remain unchanged. The loaded-source comparator keeps
# its original environment, so private routing cannot mask edited global source.
graphmode_validation_context <- function(record=NULL,phase_A=if(!is.null(record)) record$phase_A else NULL) {
    graphmode_validation_A_shape(phase_A)
    original <- environment(graphmode_validation_context)
    ctx <- new.env(parent=original)
    for(name in ls(original,pattern="^graphmode_(warmup_|dev_run_)")) {
        value <- get(name,original)
        if(is.function(value) && startsWith(name,"graphmode_warmup_")) environment(value) <- ctx
        assign(name,value,ctx)
    }
    ctx$graphmode_warmup_version <- graphmode_validation_version
    ctx$graphmode_warmup_spec <- function() graphmode_validation_spec(phase_A$candidate_scale)
    ctx$graphmode_warmup_record <- graphmode_validation_record
    ctx$graphmode_warmup_identity <- graphmode_validation_identity
    ctx$graphmode_warmup_execute_original <- ctx$graphmode_warmup_execute
    # The immutable panel constructor insists on its original generating spec.
    # Change only the subsequent fit-config proposal tuning, before full starts.
    ctx$graphmode_warmup_generate <- graphmode_dev_bind(ctx$graphmode_warmup_generate,
        list(graphmode4_config=function(road,Y,Fmat,method,expert,gate,calibration_id,...) {
            gate$guidance_proposal_sd <- phase_A$candidate_scale
            original$graphmode4_config(road,Y,Fmat,method,expert,gate,calibration_id,...)
        }))
    for(pair in list(c("batch","batch"),c("child","child"),c("diagnose","diagnose"),
        c("diagnostic_worker","diagnostic_worker"),c("diagnostic_receipt","diagnostic_receipt"),c("execute","execute"))) {
        fun <- get(paste0("graphmode_validation_",pair[2L]),original);environment(fun) <- ctx
        assign(paste0("graphmode_warmup_",pair[1L]),fun,ctx)
    }
    inherited_engine <- ctx$graphmode_warmup_engine
    ctx$graphmode_warmup_engine <- function(record=NULL) {
        api <- inherited_engine(record)
        old_guard <- api$graphmode_dev_run_source_guard
        api$graphmode_dev_run_source_guard <- function(record,repository) {
            old_guard(record,repository);graphmode_validation_A_check(record$phase_A,repository)
        }
        old_envelope <- api$graphmode_dev_run_envelope
        api$graphmode_dev_run_envelope <- function(record,chain,fit,state) {
            if(!identical(fit$core$guidance_proposal_sd,record$phase_A$candidate_scale) || !is.null(record$spec$policy))
                stop("B requires the exact common A scale and no adaptation.",call.=FALSE)
            old_envelope(record,chain,fit,state)
        }
        api
    }
    ctx
}

graphmode_validation_prepare <- function(repository,directory,A_path) {
    repository <- normalizePath(repository,mustWork=TRUE);directory <- graphmode4_output_target(directory)
    evidence <- graphmode_validation_A_import(A_path,repository)
    record <- graphmode_validation_record(repository,directory,graphmode_validation_identity(repository),
        graphmode4_pilot_runtime(),evidence)
    ctx <- graphmode_validation_context(record);api <- ctx$graphmode_warmup_engine(record)
    if(!dir.exists(dirname(directory)) || file.exists(directory) || identical(directory,repository) ||
        startsWith(directory,paste0(repository,"/"))) stop("Use a new external B directory.",call.=FALSE)
    api$graphmode_dev_run_source_guard(record,repository);ctx$graphmode_warmup_storage(record,TRUE)
    if(!dir.create(directory)) stop("Cannot create exclusive B registration.",call.=FALSE)
    ctx$graphmode_warmup_save(record,record,file.path(directory,"registration.rds"))
    api$graphmode_dev_run_tree(record)
    list(ready=TRUE,signature=record$signature,commit=record$identity$base$base$r4$core$commit,
        candidate_scale=evidence$candidate_scale,scientific_draws=0L)
}

graphmode_validation_child <- function(command,path,repository,timeout,log_file=NULL) {
    if(!command %in% c("batch","chain","worker","diagnostic-worker")) stop("Not a B child.",call.=FALSE)
    child <- graphmode_dev_bind(graphmode_dev_run_child,list(file.path=function(...) {
        parts <- lapply(list(...),function(x) if(identical(x,"scripts/graphmode-dev-run.R")) "scripts/graphmode-validation.R" else x)
        do.call(base::file.path,parts)
    }))
    child(command,path,repository,timeout,log_file)
}

# The functions below execute in the private context above and reuse A guards.
graphmode_validation_batch <- function(record,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit B run authorization required.",call.=FALSE)
    api <- graphmode_warmup_engine(record)
    api$graphmode_dev_run_preflight(record,repository);graphmode_warmup_storage(record,TRUE)
    if(!dir.create(record$output_dir)) stop("Cannot create exclusive B output.",call.=FALSE)
    graphmode_warmup_save(record,list(record=record,authorized=TRUE),file.path(record$output_dir,"launch.rds"))
    started <- proc.time()[[3L]];elapsed <- function() proc.time()[[3L]]-started
    save <- function(x,n) {api$graphmode_dev_run_tree(record,TRUE);graphmode_warmup_save(record,x,file.path(record$output_dir,n))}
    tryCatch({
        blind <- graphmode_warmup_generate(record$spec,save)
        plans <- lapply(1:4,function(j) api$graphmode_dev_run_envelope(record,j,blind$fit,blind$starts[[j]]$state))
        for(j in 1:4) save(plans[[j]],sprintf("PLAN-%02d.rds",j))
        for(j in 1:4) {
            api$graphmode_dev_run_source_guard(record,repository);graphmode_warmup_storage(record)
            if(!graphmode_warmup_time_left(record$spec,elapsed())) stop("B reserve exhausted; no extension.",call.=FALSE)
            cat(format(Sys.time()),"Phase B chain",j,"/4:",record$spec$iterations,"sweeps; scale fixed from step1\n");flush.console()
            x <- graphmode_warmup_child("chain",file.path(record$output_dir,sprintf("PLAN-%02d.rds",j)),repository,record$spec$dispatch_seconds)
            save(x,sprintf("DISPATCH-%02d.rds",j));cat(paste(x$output,collapse="\n"),"\n");flush.console()
            if(!identical(x$status,0L)) stop("B dispatcher failed/timed out; no retry.",call.=FALSE)
            api$graphmode_dev_run_worker_guard(plans[[j]],repository)
            api$graphmode_dev_run_evidence_check(plans[[j]],api$graphmode_dev_run_evidence(plans[[j]]))
        }
        if(elapsed()>=record$spec$total_seconds) stop("B total budget exhausted.",call.=FALSE)
        save(list(schema=graphmode_validation_version,registration_signature=record$signature,
            elapsed_seconds=elapsed(),formal_authorized=FALSE,auto_continue=FALSE),"completed.rds")
        cat("Phase B completed. Diagnose once, then STOP; no extension or formal simulation.\n")
    },error=function(e) {
        tryCatch({
            if(inherits(e,"graphmode_initial_failure")) save(unclass(e),"initial-failure.rds")
            save(list(schema=graphmode_validation_version,error=conditionMessage(e),elapsed_seconds=elapsed(),
                registration_signature=record$signature,formal_authorized=FALSE),"batch-failure.rds")
        },error=function(other) message("Original failure: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
}

graphmode_validation_diagnose <- function(record,repository) {
    api <- graphmode_warmup_engine(record);api$graphmode_dev_run_source_guard(record,repository)
    report <- api$graphmode_dev_run_diagnose(record)
    traces <- list()
    for(j in 1:4) {
        e <- readRDS(file.path(record$output_dir,sprintf("PLAN-%02d.rds",j)))
        api$graphmode_dev_run_worker_guard(e,repository)
        cp <- readRDS(file.path(e$plan$output_dir,"result.rds"))$checkpoint
        if(cp$control$updates!=0L || !isTRUE(cp$control$frozen) ||
            !identical(cp$control$sd,record$phase_A$candidate_scale)) stop("B fixed-scale verification failed.",call.=FALSE)
        retained <- seq.int(record$spec$warmup+1L,record$spec$iterations)
        observations <- lapply(cp$development$observations,function(x) x$observation$warmup)
        accepts <- do.call(rbind,lapply(cp$diagnostics[retained],`[[`,"guidance_accept"))
        occupancy <- do.call(rbind,lapply(observations[retained],`[[`,"before_occupancy"))
        v <- do.call(rbind,lapply(observations,`[[`,"v"));x <- do.call(rbind,lapply(observations,`[[`,"x"))
        scalar <- graphmode4_trace(cp$saved,e$plan$fit)
        acf <- function(z) if(length(unique(z))<2L) rep(NA_real_,21L) else as.numeric(stats::acf(z,lag.max=20L,plot=FALSE)$acf)
        traces[[j]] <- list(x=x,v=v,Z=do.call(rbind,lapply(observations,`[[`,"Z")),
            guidance_retained_acceptance=colMeans(accepts),
            acceptance_by_occupancy=lapply(c(FALSE,TRUE),function(empty) {
                mask <- (occupancy==0L)==empty
                list(empty=empty,proposals=colSums(mask),accepted=colSums(accepts & mask))
            }),guidance_retained_acf=apply(plogis(v[retained,,drop=FALSE]),2L,acf),
            x_retained_acf=apply(x[retained,,drop=FALSE],2L,acf),required_scalars=scalar$scalars,
            required_scalar_acf=apply(scalar$scalars,2L,acf),similarity=scalar$similarity,representative_points=scalar$points,
            seconds=do.call(rbind,lapply(observations,`[[`,"seconds")),
            note="Raw chain-local coordinates are not cross-chain sorted Rhat/ESS or structural-identifiability evidence.")
    }
    report$traces <- traces
    report$fixed_scale <- record$phase_A$candidate_scale
    report$phase_A_evidence <- record$phase_A
    report$auto_continue <- FALSE
    steps <- sum(vapply(traces,function(t) sum(t$seconds[,"total"]),numeric(1)))
    report$timing <- list(batch_seconds=report$elapsed_seconds,measured_sweep_seconds=steps,
        batch_remainder_seconds=report$elapsed_seconds-steps)
    report$note <- "B completed; original necessary statistical gates retained, not proof of global convergence, calibrated rho or formal release. STOP regardless of validity; no extension."
    api$graphmode_dev_run_source_guard(record,repository)
    report
}

graphmode_validation_diagnostic_worker <- function(record,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit B diagnostic authorization required.",call.=FALSE)
    api <- graphmode_warmup_engine(record)
    api$graphmode_dev_run_source_guard(record,repository);api$graphmode_dev_run_tree(record,TRUE)
    ticket <- readRDS(file.path(record$directory,"diagnostic-request.rds"))
    if(!identical(ticket,list(registration_signature=record$signature,seconds=record$spec$diagnostic_seconds)) ||
        any(file.exists(file.path(record$directory,c("diagnostic-execution.rds","diagnostic-report.rds",
            "diagnostic-failure.rds","diagnostic-controller-failure.rds","diagnostic-acceptance-pending.rds","diagnostic-acceptance.rds")))))
        stop("Diagnostic already attempted or request changed; no retry.",call.=FALSE)
    tryCatch({
        report <- graphmode_warmup_diagnose(record,repository)
        graphmode_warmup_save(record,report,file.path(record$directory,"diagnostic-report.rds"))
        print(report$validity[c("valid","failures")]);invisible(NULL)
    },error=function(e) {
        tryCatch(graphmode_warmup_save(record,list(error=conditionMessage(e),registration_signature=record$signature),
            file.path(record$directory,"diagnostic-failure.rds")),
            error=function(other) message("Diagnostic failure: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
}

graphmode_validation_diagnostic_receipt <- function(record,repository) {
    api <- graphmode_warmup_engine(record)
    api$graphmode_dev_run_source_guard(record,repository);api$graphmode_dev_run_tree(record,TRUE)
    if(any(file.exists(file.path(record$directory,c("diagnostic-failure.rds","diagnostic-controller-failure.rds")))))
        stop("B diagnostic failure evidence exists.",call.=FALSE)
    request <- readRDS(file.path(record$directory,"diagnostic-request.rds"))
    execution <- readRDS(file.path(record$directory,"diagnostic-execution.rds"))
    report <- readRDS(file.path(record$directory,"diagnostic-report.rds"))
    if(!identical(request,list(registration_signature=record$signature,seconds=record$spec$diagnostic_seconds)) ||
        !identical(execution,list(status=0L,registration_signature=record$signature,hard_budget_seconds=record$spec$diagnostic_seconds)) ||
        !identical(report$schema,graphmode_validation_version) || !identical(report$registration_signature,record$signature) ||
        !identical(report$phase_A_evidence,record$phase_A) || !identical(report$fixed_scale,record$phase_A$candidate_scale) ||
        !identical(report$auto_continue,FALSE) || !identical(report$formal_authorized,FALSE) ||
        !is.logical(report$validity$valid) || length(report$validity$valid)!=1L || is.na(report$validity$valid))
        stop("B diagnostic postflight failed.",call.=FALSE)
    list(schema="graphmode-validation-diagnostic-acceptance-v1",registration_signature=record$signature,
        report_signature=graphmode_digest(report),execution_signature=graphmode_digest(execution),
        request_signature=graphmode_digest(request),postflight_passed=TRUE,auto_continue=FALSE,formal_authorized=FALSE)
}

graphmode_validation_execute <- function(record,path,repository,diagnostic=FALSE,authorized=FALSE) {
    # Reuse D-046's exact pending-to-final protocol. Guard/receipt/child lookups
    # resolve to B. Discard its legacy in-memory A status; perform no later I/O.
    graphmode_warmup_execute_original(record,path,repository,diagnostic,authorized)
    invisible(NULL)
}
