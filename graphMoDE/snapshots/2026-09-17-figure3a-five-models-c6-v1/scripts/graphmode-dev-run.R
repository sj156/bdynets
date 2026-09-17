#!/usr/bin/env Rscript
# Separate D-044 entry. The audited D-043 status/check/inspect entry is unchanged.
args <- commandArgs(TRUE)
entry <- grep("^--file=",commandArgs(FALSE),value=TRUE)
if(length(entry)!=1L) stop("Use Rscript --vanilla.",call.=FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=","",entry)),".."),mustWork=TRUE)
command <- if(length(args)) args[1L] else "status"
if(command=="status" && length(args)<=1L) {
    cat("D-044: check | prepare NEW_EXTERNAL_DIR | preflight REGISTRATION.rds | run REGISTRATION.rds --authorized | diagnose REGISTRATION.rds\nNo default run, resume, extension or formal simulation.\n")
} else if(command=="check" && length(args)==1L) {
    source(file.path(graphmode_root,"scripts/tests/graphmode-dev-run-deterministic.R"))
} else {
    if(!(command %in% c("prepare","preflight","diagnose") && length(args)==2L ||
        command %in% c("run","batch","chain","worker") && length(args)==3L && args[3L]=="--authorized"))
        stop("Invalid development command; no launch.",call.=FALSE)
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),"graphmode_dev.R","graphmode_dev_run.R"))
        source(file.path(graphmode_root,"R",f))
    options(warn=2,width=180)
    if(command=="prepare") print(graphmode_dev_run_prepare(graphmode_root,args[2L])) else {
        object <- readRDS(args[2L])
        if(command=="preflight") print(graphmode_dev_run_preflight(object,graphmode_root))
        else if(command=="diagnose") print(graphmode_dev_run_diagnose(object))
        else if(command=="worker") graphmode_dev_run_worker(object,graphmode_root,TRUE)
        else if(command=="chain") graphmode_dev_run_launch(object,args[2L],graphmode_root,TRUE)
        else if(command=="batch") graphmode_dev_run_batch(object,graphmode_root,TRUE)
        else {
            graphmode_dev_run_preflight(object,graphmode_root)
            log_path <- file.path(object$directory,"terminal.log")
            if(file.exists(log_path)) stop("Launch already attempted; no retry.",call.=FALSE)
            status <- suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
                c("--vanilla",shQuote(file.path(graphmode_root,"scripts/graphmode-dev-run.R")),"batch",shQuote(normalizePath(args[2L])),"--authorized"),
                stdout=log_path,stderr=log_path,timeout=object$spec$total_seconds))
            graphmode_dev_run_tree(object)
            graphmode_save_new(list(status=as.integer(status),registration_signature=object$signature,
                hard_budget_seconds=object$spec$total_seconds),file.path(object$directory,"batch-execution.rds"))
            cat(readLines(log_path,warn=FALSE),sep="\n")
            if(status!=0L) quit(status=2L)
        }
    }
}
