#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
entry <- grep("^--file=",commandArgs(FALSE),value=TRUE)
if(length(entry)!=1L) stop("Use Rscript --vanilla.",call.=FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=","",entry)),".."),mustWork=TRUE)
command <- if(length(args)) args[1L] else "status"
if(command %in% c("status","help") && length(args)<=1L) {
    cat("D-064 receipt repair only: status | check | repair C3_REGISTRATION.rds NEW_EXTERNAL_DIR --authorized\n",
        "Preserves original frozen code/report/failure; publishes a separate supplemental receipt.\n",
        "No sampler, diagnostic-statistic, retry, resume or threshold-change entry.\n",sep="")
} else if(command=="check" && length(args)==1L) {
    source(file.path(graphmode_root,"scripts/tests/graphmode-receipt-repair-deterministic.R"))
} else {
    if(command!="repair" || length(args)!=4L || args[4L]!="--authorized") stop("Invalid repair command; no action.",call.=FALSE)
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),
        "graphmode_dev.R","graphmode_dev_run.R","graphmode_warmup.R","graphmode_validation.R",
        "graphmode_gate_refresh.R","graphmode_refresh_run.R","graphmode_expert_blocks.R","graphmode_blocks_run.R",
        "graphmode_gate_blocks.R","graphmode_gate_blocks_run.R","graphmode_receipt_repair.R")) source(file.path(graphmode_root,"R",f))
    options(warn=2,width=160)
    out <- graphmode_receipt_repair_execute(args[2L],args[3L],graphmode_root,TRUE)
    cat("Supplement accepted; original failure retained. Stored arm valid flags:\n")
    print(vapply(out$arms,`[[`,logical(1),"valid"))
}
