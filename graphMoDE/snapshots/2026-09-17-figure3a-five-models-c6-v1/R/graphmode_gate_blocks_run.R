# D-061: separate prospective whole/contrast gate comparison on expert block42.
# Source has no effects. Audited D-060 and all frozen controllers stay unchanged.
graphmode_gate_blocks_run_version <- "graphmode-gate-block-comparison-20260916-v1"

graphmode_gate_blocks_run_spec <- function(candidate_scale) {
    s <- graphmode_blocks_run_spec(candidate_scale)
    s$schema <- graphmode_gate_blocks_run_version
    s$arms <- c("full", "contrast")
    s$block_policies <- setNames(rep(list(graphmode_expert_blocks_policy(42L,"uniform")),2L),s$arms)
    s$gate_max_ess_steps <- 1000L
    s$worker_seconds <- 1800; s$sweep_budget_seconds <- 1740
    s$total_seconds <- 16200; s$diagnostic_seconds <- 1800; s$reserve_seconds <- 1920
    s$seeds <- s$seeds+100L
    names(s$seeds) <- sub("^block_chain_","contrast_chain_",names(s$seeds))
    s$jobs$arm <- sub("block42","contrast",s$jobs$arm,fixed=TRUE)
    s$role <- "D-061 fresh paired full/contrast white-gate ESS engineering comparison; expert block42 in BOTH arms, fixed m4/provisional rho4; no automatic choice, formal inference or extension"
    s
}

graphmode_gate_blocks_run_identity <- function(repository) {
    base <- graphmode_blocks_run_identity(repository)
    manifest <- "docs/provenance/graphmode-block-freeze-2026-09-15.sha256"
    old <- utils::read.table(file.path(repository,manifest),stringsAsFactors=FALSE)
    expected <- setNames(old[[1L]],old[[2L]])
    pins <- c("R/graphmode_gate_blocks.R"="f9a36d04681b33f4a2229c77d86b7eadc7ce41353f42f7beba6c302ec0403258",
        "scripts/graphmode-gate-blocks.R"="86ecc09735392f2fcbdf243577e55acae81e36f905db0277480b9e4afb5a3dc9",
        "scripts/tests/graphmode-gate-blocks-deterministic.R"="c3ea2d149e78b22fde1df512dfab3768f2a99df4e35837ce5127b31341f840a6",
        "docs/GRAPHMODE_GATE_BLOCKS_2026-09-16.md"="d64d84b3e6be04db1cc47c32342f175fec241ec0bcf477c929df5a854d817aff",
        "docs/provenance/graphmode-gate-blocks-2026-09-16.md"="30895535e30e367abe0c499b853fa3662d874a850fb1cc9b9e45456ed919be59")
    files <- unique(c(names(base$sha256),names(expected),manifest,names(pins),
        "R/graphmode_gate_blocks_run.R","scripts/graphmode-gate-blocks-run.R",
        "scripts/tests/graphmode-gate-blocks-run-deterministic.R","docs/GRAPHMODE_GATE_BLOCK_COMPARISON_2026-09-16.md"))
    sha <- setNames(vapply(file.path(repository,files),function(f)
        digest::digest(file=f,algo="sha256",serialize=FALSE),character(1)),files)
    git <- function(args) {
        x <- system2("git",c("-C",shQuote(repository),args),stdout=TRUE,stderr=TRUE)
        if(!is.null(attr(x,"status"))) stop("Cannot inspect gate comparison source identity.",call.=FALSE)
        x
    }
    base$sha256 <- sha
    base$audited_unchanged <- base$audited_unchanged && length(expected)==80L &&
        identical(unname(sha[names(expected)]),unname(expected)) && identical(unname(sha[names(pins)]),unname(pins))
    base$committed <- base$committed &&
        !length(git(c("status","--porcelain","--untracked-files=all","--",shQuote(files)))) &&
        setequal(files,git(c("ls-files","--",shQuote(files))))
    base
}

graphmode_gate_blocks_run_record <- function(repository,directory,identity,runtime,phase_A) {
    graphmode_validation_A_shape(phase_A)
    x <- list(schema=graphmode_gate_blocks_run_version,spec=graphmode_refresh_run_spec(phase_A$candidate_scale),
        repository=repository,directory=directory,output_dir=file.path(directory,"run"),
        identity=identity,runtime=runtime,phase_A=phase_A,
        authority="D-061 preparation only; approve exact gate comparison/budget before registration, then separately authorize run",
        formal_authorized=FALSE,resume_supported=FALSE,auto_continue=FALSE)
    x$signature <- graphmode_digest(x);x
}

graphmode_gate_blocks_run_job <- function(record,chain,arm) {
    graphmode_refresh_run_validate(record)
    chain <- gmde_scalar_integer(chain,"chain",1L,4L)
    if(!is.character(arm) || length(arm)!=1L || is.na(arm) || !arm %in% record$spec$arms)
        stop("Unregistered gate-comparison arm.",call.=FALSE)
    name <- sprintf("%s-chain-%02d",arm,chain)
    seed_name <- sprintf(if(arm=="full") "chain_%02d" else "contrast_chain_%02d",chain)
    x <- list(schema=graphmode_gate_blocks_run_version,record=record,chain=chain,arm=arm,
        seed=unname(record$spec$seeds[[seed_name]]),name=name,directory=file.path(record$output_dir,name),
        policy=list(gate=graphmode_gate_refresh_policy(record$spec$gate_inner_steps,record$spec$candidate_scale),
            block=record$spec$block_policies[[arm]],ess_scheme=arm))
    x$signature <- graphmode_digest(x);x
}

graphmode_gate_blocks_run_job_guard <- function(job,repository) {
    # Reuse D-056 input, full-start, persisted-job and target guards in this context.
    guard <- graphmode_blocks_run_job_guard; environment(guard) <- environment()
    blind <- guard(job,repository)
    config <- blind$fit$core
    graphmode_gate_blocks_policy(config,job$policy$ess_scheme)
    if(!identical(config$max_ess_steps,job$record$spec$gate_max_ess_steps))
        stop("Changed shared ESS scan budget.",call.=FALSE)
    blind
}

graphmode_gate_blocks_run_step <- function(state,config,policy) {
    ess <- graphmode_gate_blocks_policy(config,policy$ess_scheme)
    step <- graphmode_dev_bind(graphmode_blocks_run_step,list(
        graphmode_expert_blocks_sweep=function(state,config,gate_policy,expert_policy,cache_ffbs)
            graphmode_gate_blocks_sweep(state,config,gate_policy,expert_policy,ess,cache_ffbs)))
    step(state,config,policy)
}

# Validate new records BEFORE constructing the in-memory D-056 expert/state view.
# A completed scan covers disjoint coordinates once, so its block squared jumps
# sum to that scan's full white-vector squared jump (not the multi-scan net jump).
graphmode_gate_blocks_run_gate_check <- function(g,config,policy) {
    bad <- function() stop("Invalid gate-block scan evidence/budget.",call.=FALSE)
    expected <- graphmode_gate_blocks_policy(config,policy$ess_scheme)
    m <- policy$gate$inner_steps; nb <- length(expected$blocks)
    if(!identical(g$schema,graphmode_gate_blocks_version) || !identical(g$ess_policy,expected) ||
        !is.list(g$ess_scans) || length(g$ess_scans)!=m || !is.list(g$records) || length(g$records)!=m) bad()
    for(j in seq_len(m)) {
        scan <- g$ess_scans[[j]]
        if(!is.list(scan) || length(scan)!=nb) bad()
        for(h in seq_len(nb)) {
            b <- scan[[h]]
            if(!is.list(b) || !identical(names(b),c("block","evaluations","jump_squared")) || !identical(b$block,h) ||
                !is.numeric(b$evaluations) || length(b$evaluations)!=1L || !is.finite(b$evaluations) ||
                b$evaluations<1 || b$evaluations!=floor(b$evaluations) ||
                !is.numeric(b$jump_squared) || length(b$jump_squared)!=1L || !is.finite(b$jump_squared) || b$jump_squared<0) bad()
        }
        ne <- sum(vapply(scan,`[[`,numeric(1),"evaluations"))
        jump <- sum(vapply(scan,`[[`,numeric(1),"jump_squared"))
        if(ne>config$max_ess_steps || !identical(ne,as.numeric(g$records[[j]]$ess_evaluations)) ||
            !isTRUE(all.equal(jump,g$records[[j]]$x_jump_squared,tolerance=1e-12))) bad()
    }
    invisible(TRUE)
}

graphmode_gate_blocks_run_result_check <- function(job,result,blind) {
    ds <- result$checkpoint$diagnostics
    if(!is.list(ds) || length(ds)!=job$record$spec$iterations) stop("Incomplete gate diagnostics.",call.=FALSE)
    for(d in ds) graphmode_gate_blocks_run_gate_check(d$gate,blind$fit$core,job$policy)
    view <- result
    view$checkpoint$diagnostics <- lapply(ds,function(d) {d$gate$schema <- graphmode_gate_refresh_version;d})
    check <- graphmode_dev_bind(graphmode_blocks_run_result_check,
        list(graphmode_blocks_run_version=graphmode_gate_blocks_run_version))
    chain <- check(job,view,blind)
    chain$movement <- ds;chain$evidence_identity <- graphmode_digest(result)
    chain
}

graphmode_gate_blocks_run_child <- function(command,path,repository,timeout,log_file=NULL) {
    if(!command %in% c("batch","worker","diagnostic-worker")) stop("Invalid gate comparison child.",call.=FALSE)
    launch <- graphmode_dev_bind(graphmode_dev_run_child,list(file.path=function(...) {
        pieces <- lapply(list(...),function(x) if(identical(x,"scripts/graphmode-dev-run.R")) "scripts/graphmode-gate-blocks-run.R" else x)
        do.call(base::file.path,pieces)
    }))
    launch(command,path,repository,timeout,log_file)
}

graphmode_gate_blocks_run_mechanism <- function(ds,spec) {
    out <- graphmode_blocks_run_mechanism(ds,spec)
    retained <- ds[seq.int(spec$warmup+1L,spec$iterations)]
    scans <- unlist(lapply(retained,function(d) d$gate$ess_scans),recursive=FALSE)
    nb <- length(scans[[1L]])
    out$gate_ess <- list(scheme=retained[[1L]]$gate$ess_policy$scheme,
        retained_scans=length(scans),retained_block_updates=length(scans)*nb,
        evaluations_by_block=vapply(seq_len(nb),function(h) sum(vapply(scans,function(s) s[[h]]$evaluations,numeric(1))),numeric(1)),
        squared_jump_by_block=vapply(seq_len(nb),function(h) sum(vapply(scans,function(s) s[[h]]$jump_squared,numeric(1))),numeric(1)),
        max_evaluations_per_scan=max(vapply(scans,function(s) sum(vapply(s,`[[`,numeric(1),"evaluations")),numeric(1))),
        all_step_ess_seconds=sum(vapply(ds,function(d) sum(vapply(d$gate$records,`[[`,numeric(1),"ess_seconds")),numeric(1))),
        all_step_guidance_seconds=sum(vapply(ds,function(d) sum(vapply(d$gate$records,`[[`,numeric(1),"guidance_seconds")),numeric(1))),
        note="One shared bracket budget per scan; block moves and inner scans are not posterior draws or an MH acceptance rate. Complete worker cost remains the ESS denominator.")
    out
}

graphmode_gate_blocks_run_diagnose <- function(record,repository) {
    diagnose <- graphmode_blocks_run_diagnose;environment(diagnose) <- environment()
    out <- diagnose(record,repository)
    out$note <- "Prospective full/contrast gate comparison on identical expert block42; computational variant, not proven improvement or paper adoption. Original 37 scalars/six PSM pairs; no pooling, automatic selection or extension."
    out
}

# Copy only controller bindings into a private context; original globals and
# old persisted identities stay intact. D-056 expert/state and D-051 report
# checks remain authoritative for their unchanged portions of the new schema.
graphmode_gate_blocks_run_context <- function() {
    ctx <- graphmode_blocks_run_context()
    ctx$graphmode_blocks_run_version <- graphmode_gate_blocks_run_version
    ctx$graphmode_refresh_run_version <- graphmode_gate_blocks_run_version
    original <- environment(graphmode_gate_blocks_run_context)
    for(name in c("spec","identity","record","job","job_guard","step","result_check","child","diagnose")) {
        value <- get(paste0("graphmode_gate_blocks_run_",name),original);environment(value) <- ctx
        assign(paste0("graphmode_refresh_run_",name),value,ctx)
    }
    # The extension calls the old mechanism directly through its original binding.
    value <- graphmode_gate_blocks_run_mechanism
    ctx$graphmode_blocks_run_mechanism <- value
    ctx
}
