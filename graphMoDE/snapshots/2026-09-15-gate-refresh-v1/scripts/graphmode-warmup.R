#!/usr/bin/env Rscript
# Separate phase A entry; no implicit run, retry, phase B or formal release.
args <- commandArgs(TRUE)
entry <- grep("^--file=",commandArgs(FALSE),value=TRUE)
if(length(entry)!=1L) stop("Use Rscript --vanilla.",call.=FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=","",entry)),".."),mustWork=TRUE)
command <- if(length(args)) args[1L] else "status"
if(command=="status" && length(args)<=1L) {
    cat("D-045 phase A: check | prepare NEW_EXTERNAL_DIR | preflight REGISTRATION.rds\n",
        "After independent review/freezing and separate explicit authorization only:\n",
        "run REGISTRATION.rds --authorized | diagnose REGISTRATION.rds --authorized\n",
        "No phase B, resume, extension or formal simulation.\n",sep="")
} else if(command=="check" && length(args)==1L) {
    source(file.path(graphmode_root,"scripts/tests/graphmode-warmup-deterministic.R"))
} else {
    if(!(command %in% c("prepare","preflight") && length(args)==2L ||
        command %in% c("run","batch","chain","worker","diagnose","diagnostic-worker") &&
            length(args)==3L && args[3L]=="--authorized")) stop("Invalid phase A command; no launch.",call.=FALSE)
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),
        "graphmode_dev.R","graphmode_dev_run.R","graphmode_warmup.R"))
        source(file.path(graphmode_root,"R",f))
    options(warn=2,width=180)
    if(command=="prepare") print(graphmode_warmup_prepare(graphmode_root,args[2L])) else {
        object <- readRDS(args[2L])
        record <- if(command %in% c("chain","worker")) object$record else object
        api <- graphmode_warmup_engine(record)
        if(command=="preflight") print(api$graphmode_dev_run_preflight(record,graphmode_root))
        else if(command=="worker") api$graphmode_dev_run_worker(object,graphmode_root,TRUE)
        else if(command=="chain") api$graphmode_dev_run_launch(object,args[2L],graphmode_root,TRUE)
        else if(command=="batch") graphmode_warmup_batch(record,graphmode_root,TRUE)
        else if(command=="diagnostic-worker") graphmode_warmup_diagnostic_worker(record,graphmode_root,TRUE)
        else graphmode_warmup_execute(record,args[2L],graphmode_root,command=="diagnose",TRUE)
    }
}
