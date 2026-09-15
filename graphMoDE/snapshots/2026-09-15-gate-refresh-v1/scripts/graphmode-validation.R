#!/usr/bin/env Rscript
# Explicit B commands only. No automatic science run, retries or extensions.
args <- commandArgs(TRUE)
entry <- grep("^--file=",commandArgs(FALSE),value=TRUE)
if(length(entry)!=1L) stop("Use Rscript --vanilla.",call.=FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=","",entry)),".."),mustWork=TRUE)
command <- if(length(args)) args[1L] else "status"
if(command=="status" && length(args)<=1L) {
    cat("Phase B: check | prepare NEW_EXTERNAL_DIR A_REGISTRATION.rds | preflight B_REGISTRATION.rds\n",
        "run B_REGISTRATION.rds --authorized | diagnose B_REGISTRATION.rds --authorized\n",
        "Four6000/1500, fixed reviewed scale, total7h. Diagnose once and STOP; no formal simulation.\n",sep="")
} else if(command=="check" && length(args)==1L) {
    source(file.path(graphmode_root,"scripts/tests/graphmode-validation-deterministic.R"))
} else {
    if(!(command=="prepare" && length(args)==3L || command=="preflight" && length(args)==2L ||
        command %in% c("run","batch","chain","worker","diagnose","diagnostic-worker") &&
            length(args)==3L && args[3L]=="--authorized")) stop("Invalid B command; no launch.",call.=FALSE)
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),
        "graphmode_dev.R","graphmode_dev_run.R","graphmode_warmup.R","graphmode_validation.R"))
        source(file.path(graphmode_root,"R",f))
    options(warn=2,width=180)
    if(command=="prepare") print(graphmode_validation_prepare(graphmode_root,args[2L],args[3L])) else {
        object <- readRDS(args[2L]);record <- if(command %in% c("chain","worker")) object$record else object
        ctx <- graphmode_validation_context(record);api <- ctx$graphmode_warmup_engine(record)
        if(command=="preflight") print(api$graphmode_dev_run_preflight(record,graphmode_root))
        else if(command=="worker") api$graphmode_dev_run_worker(object,graphmode_root,TRUE)
        else if(command=="chain") api$graphmode_dev_run_launch(object,args[2L],graphmode_root,TRUE)
        else if(command=="batch") ctx$graphmode_warmup_batch(record,graphmode_root,TRUE)
        else if(command=="diagnostic-worker") ctx$graphmode_warmup_diagnostic_worker(record,graphmode_root,TRUE)
        else ctx$graphmode_warmup_execute(record,args[2L],graphmode_root,command=="diagnose",TRUE)
    }
}
