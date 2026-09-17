# D-064 structural fixtures only; no scientific samples/statistics/subprocesses.
if(!exists("graphmode_root",inherits=FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent=globalenv())
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),
        "graphmode_dev.R","graphmode_dev_run.R","graphmode_warmup.R","graphmode_validation.R",
        "graphmode_gate_refresh.R","graphmode_refresh_run.R","graphmode_expert_blocks.R","graphmode_blocks_run.R",
        "graphmode_gate_blocks.R","graphmode_gate_blocks_run.R","graphmode_receipt_repair.R")) sys.source(file.path(graphmode_root,"R",f),kernel)
    test_env <- environment();parent.env(test_env) <- kernel
    seed_before <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
    kind <- RNGkind();count <- 0L
    assert <- function(x) if(!isTRUE(x)) stop("D-064 assertion: ",paste(deparse(substitute(x)),collapse=" "),call.=FALSE)
    fails <- function(code,pattern=NULL) {
        x <- tryCatch({force(code);NULL},error=function(e) e)
        assert(inherits(x,"error"));if(!is.null(pattern)) assert(grepl(pattern,conditionMessage(x),fixed=TRUE));invisible(x)
    }
    test <- function(name,code) {force(code);count <<- count+1L;cat("OK",count,name,"\n");flush.console()}
    old_body <- body(graphmode_refresh_run_report_check)
    corrected <- graphmode_receipt_repair_checker()
    points <- expand.grid(id=c(1L,31L,61L,91L,121L),time=c(1L,84L,168L))
    names <- c("log_likelihood","Kocc",paste0("proportion_",1:10),paste0("guidance_",1:10),paste0("unit_log_mean_",points$id,"_t",points$time))
    tab <- data.frame(scalar=names,status=rep("passed",37),rank_rhat=rep(1.005,37),folded_rhat=rep(1.003,37),bulk_ess=rep(500,37),tail_ess=rep(600,37))
    tab$status[2:12] <- "uninformative-constant-discrete";tab[2:12,3:6] <- NA_real_
    pairs <- combn(1:4,2);seconds <- as.numeric(1:4)
    m <- list(guidance_proposals=rep(2400,10),guidance_accepted=rep(1200,10),guidance_acceptance=rep(.5,10))
    arm <- list(validity=list(protocol=graphmode4_protocol,formal_authorized=FALSE,convergence_certified=FALSE,
        scalars=tab,psm_rms=data.frame(chain_a=pairs[1,],chain_b=pairs[2,],rms=rep(0,6)),failures=character(),valid=TRUE),
        worker_seconds=seconds,total_worker_seconds=sum(seconds),automatic_selection=FALSE,mechanism=rep(list(m),4))
    efficiency <- function(a) {
        e <- a$validity$scalars;e$bulk_ess_per_worker_second <- e$bulk_ess/sum(a$worker_seconds)
        e$tail_ess_per_worker_second <- e$tail_ess/sum(a$worker_seconds);e$precision_certified <- FALSE;a$efficiency <- e;a
    }
    arm <- efficiency(arm)
    record <- list(signature="fixture",spec=list(inner_steps=4L,gate_inner_steps=4L,iterations=1200L,warmup=600L,diagnostic_seconds=1800,arms=c("full","contrast")))
    science <- list(jobs=lapply(1:4,function(j) list(m=4L,chain=j)),evidence=lapply(seconds,function(x) list(execution=list(elapsed_seconds=x))))
    check <- function(a,fun=corrected) fun(list(arms=list(m4=a)),record,science)
    test("empty failure list bug reproduces and corrected checker accepts genuine success", {
        assert(identical(paste0("statistical: ",character()),"statistical: "))
        fails(check(arm,graphmode_refresh_run_report_check),"Original necessary validity flags changed.")
        check(arm);assert(arm$validity$valid && length(arm$validity$failures)==0L)
    })
    test("only one recognized source expression may change", {
        assert(identical(body(graphmode_refresh_run_report_check),old_body))
        fails(graphmode_receipt_repair_checker(function() TRUE),"does not apply")
        fails(graphmode_receipt_repair_checker(corrected),"does not apply")
    })
    failing <- arm;failing$validity$scalars$status[13L] <- "failed";failing$validity$scalars$rank_rhat[13L] <- 1.02
    failing$validity$failures <- "statistical: guidance_1";failing$validity$valid <- FALSE;failing <- efficiency(failing)
    test("genuine scalar failure accepted only with original false flag and failure list", {
        check(failing);check(failing,graphmode_refresh_run_report_check)
        b <- failing;b$validity$valid <- TRUE;fails(check(b),"flags changed")
        b <- failing;b$validity$failures <- character();fails(check(b),"flags changed")
    })
    test("PSM-only and combined failures retain false flags", {
        for(a in list(arm,failing)) {
            a$validity$psm_rms$rms[1] <- .051
            a$validity$failures <- c(a$validity$failures,"statistical: pairwise PSM RMS exceeds 0.05")
            a$validity$valid <- FALSE;check(a)
            a$validity$valid <- TRUE;fails(check(a),"flags changed")
        }
    })
    test("constant discrete fields remain uninformative; missing or NaN continuous fields cannot pass", {
        b <- arm;b$validity$scalars$status[2L] <- "passed";fails(check(b),"incorrectly passes")
        for(value in c(NA_real_,NaN,Inf)) {b <- arm;b$validity$scalars$bulk_ess[13L] <- value;fails(check(b),"incorrectly passes")}
        b <- arm;b$validity$scalars <- b$validity$scalars[-13L,];fails(check(b),"Incomplete")
    })
    test("rank folded bulk tail and PSM boundaries unchanged", {
        for(field in c("rank_rhat","folded_rhat","bulk_ess","tail_ess")) {
            limit <- if(grepl("rhat",field)) 1.01 else 400
            b <- arm;b$validity$scalars[[field]][13] <- limit;b <- efficiency(b);check(b)
            b$validity$scalars[[field]][13] <- limit+if(grepl("rhat",field)) 1e-6 else -1e-6
            fails(check(b),"incorrectly passes")
        }
        b <- arm;b$validity$psm_rms$rms[1] <- .05;check(b)
        b$validity$psm_rms$rms[1] <- .050001;fails(check(b),"flags changed")
    })
    test("full worker denominator guidance counts and retained endpoint set still checked", {
        b <- arm;b$efficiency$bulk_ess_per_worker_second <- 999;fails(check(b),"denominator")
        b <- arm;b$worker_seconds[1] <- 2;fails(check(b),"Incomplete")
        b <- arm;b$mechanism[[1]]$guidance_proposals <- rep(600,10);fails(check(b),"denominator")
        b <- arm;b$validity$psm_rms <- b$validity$psm_rms[-1,];fails(check(b),"Incomplete")
    })
    test("complete adapter still verifies mechanism against per-worker evidence", {
        ctx <- new.env(parent=kernel)
        ctx$graphmode_blocks_run_mechanism <- function(ds,spec) ds
        ctx$graphmode_refresh_run_report_check <- graphmode_blocks_run_report_check
        environment(ctx$graphmode_refresh_run_report_check) <- ctx
        sc <- list(jobs=unlist(lapply(record$spec$arms,function(a) lapply(1:4,function(j) list(arm=a,chain=j))),recursive=FALSE),
            evidence=rep(lapply(1:4,function(j) list(execution=list(elapsed_seconds=seconds[j]),result=list(checkpoint=list(diagnostics=m)))),2))
        rr <- list(arms=list(full=failing,contrast=arm))
        graphmode_receipt_repair_report_check(rr,record,sc,ctx)
        rr$arms$contrast$mechanism[[1]]$extra <- TRUE
        fails(graphmode_receipt_repair_report_check(rr,record,sc,ctx),"mechanism")
    })
    test("only exact original failure may be supplemented", {
        f <- list(error="Original necessary validity flags changed.",registration_signature="fixture")
        graphmode_receipt_repair_failure_check(f,"fixture")
        f$error <- "timeout";fails(graphmode_receipt_repair_failure_check(f,"fixture"))
        f$error <- "Original necessary validity flags changed.";fails(graphmode_receipt_repair_failure_check(f,"changed"))
    })
    test("diagnostic execution and source-report receipt bindings remain strict", {
        sci <- list(receipt=list(id="science"),completed=list(elapsed_seconds=12))
        rr <- list(schema=graphmode_gate_blocks_run_version,registration_signature=record$signature,
            science_receipt_signature=graphmode_digest(sci$receipt),batch_elapsed_seconds=12,
            automatic_selection=FALSE,auto_continue=FALSE,formal_authorized=FALSE)
        request <- list(registration_signature=record$signature,budget_seconds=1800)
        execution <- list(status=0L,registration_signature=record$signature,budget_seconds=1800,elapsed_seconds=20)
        graphmode_receipt_repair_header_check(rr,record,sci,request,execution)
        for(field in c("status","elapsed_seconds","budget_seconds")) {
            e <- execution;e[[field]] <- 9999L;fails(graphmode_receipt_repair_header_check(rr,record,sci,request,e))
        }
        rr$science_receipt_signature <- "changed";fails(graphmode_receipt_repair_header_check(rr,record,sci,request,execution))
    })
    fixture <- tempfile("graphmode-receipt-repair-fixed-",tmpdir="/private/tmp");dir.create(fixture);fixture <- normalizePath(fixture)
    original <- file.path(fixture,"original");dir.create(original)
    registration <- file.path(original,"registration.rds");graphmode_save_new(list(fixture=TRUE),registration)
    graphmode_save_new(list(error="preserved"),file.path(original,"diagnostic-failure.rds"))
    inv <- graphmode_receipt_repair_inventory(original);src <- graphmode_receipt_repair_source(graphmode_root)
    receipt <- list(schema=graphmode_receipt_repair_version,repair_source=src,original_inventory=inv,postflight_passed=TRUE)
    execute <- graphmode_dev_bind(graphmode_receipt_repair_execute,list(graphmode_receipt_repair_review=function(...) receipt))
    test("authorization and new external destination guard precede any write", {
        fails(execute(NULL,NULL,NULL),"authorization")
        fails(execute(registration,original,graphmode_root,TRUE),"new external")
        fails(execute(registration,file.path(original,"nested"),graphmode_root,TRUE),"new external")
        fails(execute(registration,file.path(graphmode_root,"bad-repair-test"),graphmode_root,TRUE),"new external")
    })
    test("new receipt publishes separately and cannot overwrite or retry", {
        dest <- file.path(fixture,"accepted");execute(registration,dest,graphmode_root,TRUE)
        assert(identical(readRDS(file.path(dest,"supplemental-acceptance.rds")),receipt))
        assert(!file.exists(file.path(dest,"supplemental-acceptance-pending.rds")))
        assert(identical(inv,graphmode_receipt_repair_inventory(original)))
        fails(execute(registration,dest,graphmode_root,TRUE),"new external")
    })
    test("review failure preserved without acceptance and without original changes", {
        fail <- graphmode_dev_bind(graphmode_receipt_repair_execute,list(graphmode_receipt_repair_review=function(...) stop("fixture check failed")))
        dest <- file.path(fixture,"failed");fails(fail(registration,dest,graphmode_root,TRUE),"fixture check failed")
        assert(file.exists(file.path(dest,"repair-failure.rds")) && !file.exists(file.path(dest,"supplemental-acceptance.rds")))
        assert(identical(inv,graphmode_receipt_repair_inventory(original)))
    })
    test("late evidence mutation prevents final receipt and retains pending evidence", {
        dest <- file.path(fixture,"pending");dir.create(dest)
        fails(graphmode_receipt_repair_publish(receipt,dest,function() stop("source changed")),"source changed")
        assert(file.exists(file.path(dest,"supplemental-acceptance-pending.rds")) && !file.exists(file.path(dest,"supplemental-acceptance.rds")))
        fails(graphmode_receipt_repair_publish(receipt,dest,function() TRUE),"already attempted")
    })
    test("inconsistent review identity cannot publish", {
        b <- receipt;b$repair_source$schema <- "changed"
        bad <- graphmode_dev_bind(graphmode_receipt_repair_execute,list(graphmode_receipt_repair_review=function(...) b))
        dest <- file.path(fixture,"bad-identity");fails(bad(registration,dest,graphmode_root,TRUE),"before publication")
        assert(!file.exists(file.path(dest,"supplemental-acceptance.rds")))
    })
    test("inventory detects changed files added failures and rejects symlinks", {
        d <- file.path(fixture,"inventory");dir.create(d);writeLines("one",file.path(d,"input"));one <- graphmode_receipt_repair_inventory(d)
        writeLines("two",file.path(d,"input"));assert(!identical(one,graphmode_receipt_repair_inventory(d)))
        writeLines("failure",file.path(d,"failure.rds"));assert(length(graphmode_receipt_repair_inventory(d)$files$path)==2L)
        assert(file.symlink(file.path(d,"input"),file.path(d,"link")));fails(graphmode_receipt_repair_inventory(d),"Symlink")
    })
    test("original92 frozen entries definitions and RNG remain unchanged", {
        manifest <- utils::read.table(file.path(graphmode_root,"docs/provenance/graphmode-gate-block-freeze-2026-09-16.sha256"),stringsAsFactors=FALSE)
        assert(nrow(manifest)==92L && all(vapply(file.path(graphmode_root,manifest[[2L]]),function(f) digest::digest(file=f,algo="sha256",serialize=FALSE),character(1))==manifest[[1L]]))
        assert(identical(body(graphmode_refresh_run_report_check),old_body))
        assert(identical(src,graphmode_receipt_repair_source(graphmode_root)))
        after <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
        assert(identical(seed_before,after) && identical(kind,RNGkind()))
    })
    cat("PASS",count,"D-064 fixed structural groups; no scientific run or statistic recalculation.\n")
    cat("Fixed artifacts:",fixture,"\n")
})
