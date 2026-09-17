#!/usr/bin/env Rscript
# D-061: informational by default, exact authority tokens on all write/run paths.
args <- commandArgs(TRUE)
entry <- grep("^--file=",commandArgs(FALSE),value=TRUE)
if(length(entry)!=1L) stop("Use Rscript --vanilla.",call.=FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=","",entry)),".."),mustWork=TRUE)
command <- if(length(args)) args[1L] else "status"
if(command %in% c("status","help") && length(args)<=1L) {
    cat("D-061 proposed full/contrast gate ESS; BOTH expert block42/m4/rho4, four1200/600 each.\n",
        "Cap16200s science +1800s diagnosis requires plan approval, frozen identity and new registration.\n",
        "check | prepare NEW_EXTERNAL_DIR A_REGISTRATION.rds --approved-plan\n",
        "preflight REGISTRATION.rds | run REGISTRATION.rds --authorized\n",
        "diagnose REGISTRATION.rds --authorized | receipt REGISTRATION.rds\n",
        "No automatic diagnosis, continuation, selection or formal simulation.\n",sep="")
} else if(command=="check" && length(args)==1L) {
    source(file.path(graphmode_root,"scripts/tests/graphmode-gate-blocks-run-deterministic.R"))
} else {
    if(!(command=="prepare" && length(args)==4L && args[4L]=="--approved-plan" ||
        command %in% c("preflight","receipt") && length(args)==2L ||
        command %in% c("run","batch","worker","diagnose","diagnostic-worker") && length(args)==3L && args[3L]=="--authorized"))
        stop("Invalid gate comparison command; no launch.",call.=FALSE)
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),
        "graphmode_dev.R","graphmode_dev_run.R","graphmode_warmup.R","graphmode_validation.R",
        "graphmode_gate_refresh.R","graphmode_refresh_run.R","graphmode_expert_blocks.R","graphmode_blocks_run.R","graphmode_gate_blocks.R","graphmode_gate_blocks_run.R")) source(file.path(graphmode_root,"R",f))
    options(warn=2,width=160);api <- graphmode_gate_blocks_run_context()
    if(command=="prepare") print(api$graphmode_refresh_run_prepare(graphmode_root,args[2L],args[3L],TRUE)) else {
        object <- readRDS(args[2L])
        if(command=="preflight") print(api$graphmode_refresh_run_preflight(object,graphmode_root))
        else if(command=="receipt") print(api$graphmode_refresh_run_diagnostic_evidence(object,graphmode_root))
        else if(command=="worker") api$graphmode_refresh_run_worker(object,graphmode_root,TRUE)
        else if(command=="batch") api$graphmode_refresh_run_batch(object,graphmode_root,TRUE)
        else if(command=="diagnostic-worker") api$graphmode_refresh_run_diagnostic_worker(object,graphmode_root,TRUE)
        else api$graphmode_refresh_run_execute(object,args[2L],graphmode_root,command=="diagnose",TRUE)
    }
}
