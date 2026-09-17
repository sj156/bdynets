# D-064: supplement the known c3 receipt failure without changing frozen inputs.
# This module has no sampler, diagnostic-statistic, retry or resume entry point.
graphmode_receipt_repair_version <- "graphmode-c3-receipt-repair-20260916-v1"

graphmode_receipt_repair_checker <- function(original=graphmode_refresh_run_report_check) {
    # One explicit AST substitution retains every other frozen checker expression.
    target <- quote(failures <- paste0("statistical: ",tab$scalar[!tab$status %in% c("passed","uninformative-constant-discrete")]))
    replacement <- quote(failures <- if(any(!tab$status %in% c("passed","uninformative-constant-discrete")))
        paste0("statistical: ",tab$scalar[!tab$status %in% c("passed","uninformative-constant-discrete")]) else character())
    changes <- 0L
    replace <- function(e) {
        if(identical(e,target)) {changes <<- changes+1L;return(replacement)}
        if(is.call(e)) for(i in seq_along(e)) if(is.call(e[[i]])) e[[i]] <- replace(e[[i]])
        e
    }
    corrected <- original;body(corrected) <- replace(body(original))
    if(changes!=1L) stop("Frozen failure-list expression changed; repair does not apply.",call.=FALSE)
    corrected
}

graphmode_receipt_repair_source <- function(repository) {
    files <- c("R/graphmode_receipt_repair.R","scripts/graphmode-receipt-repair.R",
        "scripts/tests/graphmode-receipt-repair-deterministic.R")
    expected <- new.env(parent=baseenv())
    sys.source(file.path(repository,files[1L]),expected)
    symbols <- ls(expected,all.names=TRUE)
    describe <- function(env) lapply(mget(symbols,env,inherits=TRUE),function(x)
        if(is.function(x)) list(formals=formals(x),body=body(x)) else x)
    if(!identical(describe(expected),describe(environment(graphmode_receipt_repair_source))))
        stop("Repair definitions differ from recorded source.",call.=FALSE)
    list(schema=graphmode_receipt_repair_version,
        sha256=setNames(vapply(file.path(repository,files),function(f)
            digest::digest(file=f,algo="sha256",serialize=FALSE),character(1)),files),
        scope="source-hashed local receipt supplement; not a new scientific execution or committed freeze")
}

graphmode_receipt_repair_inventory <- function(directory) {
    directory <- normalizePath(directory,mustWork=TRUE)
    entries <- sort(list.files(directory,recursive=TRUE,all.files=TRUE,full.names=TRUE,
        include.dirs=TRUE,no..=TRUE))
    links <- Sys.readlink(entries)
    if(any(!is.na(links) & nzchar(links))) stop("Symlink in evidence inventory.",call.=FALSE)
    info <- file.info(entries)
    if(anyNA(info$isdir) || anyNA(info$size)) stop("Missing evidence during inventory.",call.=FALSE)
    relative <- substring(entries,nchar(directory)+2L);at <- !info$isdir
    list(directories=relative[!at],files=data.frame(path=relative[at],bytes=unname(info$size[at]),
        sha256=unname(vapply(entries[at],function(f) digest::digest(file=f,algo="sha256",serialize=FALSE),character(1))),
        stringsAsFactors=FALSE))
}

graphmode_receipt_repair_failure_check <- function(failure,signature) {
    if(!identical(failure,list(error="Original necessary validity flags changed.",registration_signature=signature)))
        stop("Not the specific preserved empty-failure-list error.",call.=FALSE)
    invisible(TRUE)
}

graphmode_receipt_repair_header_check <- function(report,record,science,request,execution) {
    r <- record
    if(!identical(request,list(registration_signature=r$signature,budget_seconds=r$spec$diagnostic_seconds)) ||
        !identical(execution$status,0L) || !identical(execution$registration_signature,r$signature) ||
        !identical(execution$budget_seconds,r$spec$diagnostic_seconds) ||
        !isTRUE(is.finite(execution$elapsed_seconds) && execution$elapsed_seconds>=0 && execution$elapsed_seconds<=r$spec$diagnostic_seconds) ||
        !identical(report$schema,graphmode_gate_blocks_run_version) || !identical(report$registration_signature,r$signature) ||
        !identical(report$science_receipt_signature,graphmode_digest(science$receipt)) ||
        !identical(report$batch_elapsed_seconds,science$completed$elapsed_seconds) ||
        !identical(report$automatic_selection,FALSE) || !identical(report$auto_continue,FALSE) || !identical(report$formal_authorized,FALSE))
        stop("Diagnostic exit/identity/budget mismatch; no supplemental acceptance.",call.=FALSE)
    invisible(TRUE)
}

graphmode_receipt_repair_report_check <- function(report,record,science,api) {
    corrected <- graphmode_receipt_repair_checker()
    base <- graphmode_blocks_run_base
    check <- graphmode_dev_bind(api$graphmode_refresh_run_report_check,list(
        graphmode_blocks_run_base=function(name) if(identical(name,"report_check")) corrected else base(name)))
    # This still checks all saved mechanism summaries against full worker records.
    check(report,record,science)
}

graphmode_receipt_repair_review <- function(registration,repository) {
    r <- readRDS(registration)
    if(!identical(r$signature,"b3721b30a946b8553ee09c9eda011177692cb284714123193a9f437fa888751c") ||
        !identical(normalizePath(registration,mustWork=TRUE),file.path(r$directory,"registration.rds")))
        stop("This repair is restricted to the registered c3 evidence.",call.=FALSE)
    source <- graphmode_receipt_repair_source(repository)
    api <- graphmode_gate_blocks_run_context()
    api$graphmode_refresh_run_guard(r,repository)
    api$graphmode_refresh_run_tree(r,TRUE)
    freeze <- utils::read.table(file.path(repository,"docs/provenance/graphmode-gate-block-freeze-2026-09-16.sha256"),stringsAsFactors=FALSE)
    if(nrow(freeze)!=92L || any(vapply(file.path(repository,freeze[[2L]]),function(f)
        digest::digest(file=f,algo="sha256",serialize=FALSE),character(1))!=freeze[[1L]]))
        stop("Original 92 frozen entries changed.",call.=FALSE)
    inventory <- graphmode_receipt_repair_inventory(r$directory)
    if(!identical(inventory$files$path[grepl("(^|/).*failure[.]rds$",inventory$files$path)],"diagnostic-failure.rds") ||
        any(c("diagnostic-acceptance.rds","diagnostic-acceptance-pending.rds") %in% inventory$files$path))
        stop("Unexpected failure or existing diagnostic receipt; repair refused.",call.=FALSE)
    report_sha <- inventory$files$sha256[match("diagnostic-report.rds",inventory$files$path)]
    if(!identical(report_sha,"58099497324217a10cda763e22a2621db41377c4e73029db76d643faf9b5c40a"))
        stop("Known c3 diagnostic report bytes changed.",call.=FALSE)
    failure <- readRDS(file.path(r$directory,"diagnostic-failure.rds"))
    graphmode_receipt_repair_failure_check(failure,r$signature)
    cat("Revalidating frozen source, eight saved workers and science receipts; no sampling.\n");flush.console()
    job_check <- api$graphmode_refresh_run_job_evidence
    api$graphmode_refresh_run_job_evidence <- function(job,repository,receipt=TRUE) {
        out <- job_check(job,repository,receipt)
        cat("Verified saved",job$name,"\n");flush.console();out
    }
    science <- api$graphmode_refresh_run_science_evidence(r,repository)
    cat("Reading original diagnostic report; no statistic recomputation.\n");flush.console()
    report <- readRDS(file.path(r$directory,"diagnostic-report.rds"))
    execution <- readRDS(file.path(r$directory,"diagnostic-execution.rds"))
    request <- readRDS(file.path(r$directory,"diagnostic-request.rds"))
    graphmode_receipt_repair_header_check(report,r,science,request,execution)
    original_error <- tryCatch({api$graphmode_refresh_run_report_check(report,r,science);NULL},error=function(e) conditionMessage(e))
    if(!identical(original_error,failure$error)) stop("Original failure no longer reproduces.",call.=FALSE)
    graphmode_receipt_repair_report_check(report,r,science,api)
    cat("Corrected structure and complete mechanism checks passed; verifying preserved bytes.\n");flush.console()
    if(!identical(inventory,graphmode_receipt_repair_inventory(r$directory)) ||
        !identical(source,graphmode_receipt_repair_source(repository))) stop("Evidence/source changed during repair review.",call.=FALSE)
    api$graphmode_refresh_run_guard(r,repository)
    list(schema=graphmode_receipt_repair_version,kind="supplemental-diagnostic-acceptance",
        original_directory=r$directory,registration_signature=r$signature,original_source_identity=r$identity,
        repair_source=source,original_inventory=inventory,
        original_science_receipt_signature=graphmode_digest(science$receipt),
        original_diagnostic_execution_signature=graphmode_digest(execution),
        original_diagnostic_request_signature=graphmode_digest(request),
        original_diagnostic_report_signature=graphmode_digest(report),
        preserved_failure=failure,original_diagnostic_receipt_exists=FALSE,
        original_failure_reproduced=TRUE,all_eight_workers_revalidated=TRUE,mechanisms_revalidated=TRUE,
        arms=lapply(report$arms,function(a) list(valid=a$validity$valid,failures=a$validity$failures,
            scalars=a$validity$scalars,psm_rms=a$validity$psm_rms,efficiency=a$efficiency,
            worker_seconds=a$worker_seconds,total_worker_seconds=a$total_worker_seconds)),
        postflight_passed=TRUE,statistical_diagnostics_recomputed=FALSE,scientific_draws=0L,
        convergence_certified=FALSE,formal_authorized=FALSE,
        note="Corrected empty-failure-list verification of the original report only. Original failure remains; this separate receipt does not replace old files or make failed arms valid.")
}

graphmode_receipt_repair_publish <- function(receipt,directory,verify) {
    pending <- file.path(directory,"supplemental-acceptance-pending.rds")
    final <- file.path(directory,"supplemental-acceptance.rds")
    if(file.exists(pending) || file.exists(final)) stop("Supplement already attempted.",call.=FALSE)
    graphmode_save_new(receipt,pending)
    verify()
    if(!identical(readRDS(pending),receipt)) stop("Pending supplement changed.",call.=FALSE)
    if(file.exists(final) || !file.rename(pending,final)) stop("Supplement publication failed.",call.=FALSE)
    invisible(final)
}

graphmode_receipt_repair_execute <- function(registration,directory,repository,authorized=FALSE) {
    if(!identical(authorized,TRUE)) stop("Explicit receipt-repair authorization required.",call.=FALSE)
    repository <- normalizePath(repository,mustWork=TRUE)
    registration <- normalizePath(registration,mustWork=TRUE)
    original <- dirname(registration);directory <- graphmode4_output_target(directory)
    link <- Sys.readlink(directory)
    if(file.exists(directory) || (!is.na(link) && nzchar(link)) || !dir.exists(dirname(directory)) ||
        identical(directory,repository) || startsWith(directory,paste0(repository,"/")) ||
        identical(directory,original) || startsWith(directory,paste0(original,"/")))
        stop("Use a new external supplemental directory, outside the original results.",call.=FALSE)
    source <- graphmode_receipt_repair_source(repository)
    if(!dir.create(directory)) stop("Cannot create exclusive repair directory.",call.=FALSE)
    save <- function(x,name) {
        if(!identical(normalizePath(directory,mustWork=TRUE),directory)) stop("Repair destination moved.",call.=FALSE)
        graphmode_save_new(x,file.path(directory,name))
    }
    tryCatch({
        save(list(schema=graphmode_receipt_repair_version,registration=registration,repair_source=source,
            authority="User authorized acceptance-layer repair and review of the saved c3 report; no new statistics or chains"),"repair-request.rds")
        receipt <- graphmode_receipt_repair_review(registration,repository)
        verify <- function() {
            if(!identical(normalizePath(directory,mustWork=TRUE),directory) ||
                !identical(source,receipt$repair_source) || !identical(source,graphmode_receipt_repair_source(repository)) ||
                !identical(receipt$original_inventory,graphmode_receipt_repair_inventory(original)))
                stop("Supplement evidence/source/destination changed before publication.",call.=FALSE)
        }
        graphmode_receipt_repair_publish(receipt,directory,verify)
        invisible(receipt)
    },error=function(e) {
        tryCatch(save(list(error=conditionMessage(e),schema=graphmode_receipt_repair_version),"repair-failure.rds"),
            error=function(other) message("Original repair failure: ",conditionMessage(e),"; persistence: ",conditionMessage(other)))
        stop(conditionMessage(e),call.=FALSE)
    })
}
