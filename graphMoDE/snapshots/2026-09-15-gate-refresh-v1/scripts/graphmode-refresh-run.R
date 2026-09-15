#!/usr/bin/env Rscript
# D-051 independent entry. Defaults to information; no launch on source/check.
args <- commandArgs(TRUE)
entry <- grep("^--file=",commandArgs(FALSE),value=TRUE)
if(length(entry)!=1L) stop("Use Rscript --vanilla.",call.=FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=","",entry)),".."),mustWork=TRUE)
command <- if(length(args)) args[1L] else "status"
if(command %in% c("status","help") && length(args)<=1L) {
    cat("D-051 preparation: m1 versus m4, four1200/600 each, fixed A scale/rho4.\n",
        "Proposed cap: 11700s science + 900s diagnosis; approval/freeze/registration still required.\n",
        "check | prepare NEW_EXTERNAL_DIR A_REGISTRATION.rds --approved-plan\n",
        "preflight REGISTRATION.rds | run REGISTRATION.rds --authorized\n",
        "diagnose REGISTRATION.rds --authorized | receipt REGISTRATION.rds\n",
        "No old commands/checkpoints, automatic continuation, or formal simulation.\n",sep="")
} else if(command=="check" && length(args)==1L) {
    source(file.path(graphmode_root,"scripts/tests/graphmode-refresh-run-deterministic.R"))
} else {
    if(!(command=="prepare" && length(args)==4L && args[4L]=="--approved-plan" ||
        command %in% c("preflight","receipt") && length(args)==2L ||
        command %in% c("run","batch","worker","diagnose","diagnostic-worker") &&
            length(args)==3L && args[3L]=="--authorized")) stop("Invalid comparison command; no launch.",call.=FALSE)
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),
        "graphmode_dev.R","graphmode_dev_run.R","graphmode_warmup.R","graphmode_validation.R",
        "graphmode_gate_refresh.R","graphmode_refresh_run.R")) source(file.path(graphmode_root,"R",f))
    options(warn=2,width=160)
    if(command=="prepare") print(graphmode_refresh_run_prepare(graphmode_root,args[2L],args[3L],TRUE)) else {
        object <- readRDS(args[2L])
        if(command=="preflight") print(graphmode_refresh_run_preflight(object,graphmode_root))
        else if(command=="receipt") print(graphmode_refresh_run_diagnostic_evidence(object,graphmode_root))
        else if(command=="worker") graphmode_refresh_run_worker(object,graphmode_root,TRUE)
        else if(command=="batch") graphmode_refresh_run_batch(object,graphmode_root,TRUE)
        else if(command=="diagnostic-worker") graphmode_refresh_run_diagnostic_worker(object,graphmode_root,TRUE)
        else graphmode_refresh_run_execute(object,args[2L],graphmode_root,command=="diagnose",TRUE)
    }
}
