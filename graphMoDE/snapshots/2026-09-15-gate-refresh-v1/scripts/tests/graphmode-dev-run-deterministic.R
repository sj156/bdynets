# Fixed fixtures, mock subprocesses and deterministic tapes only.
if(!exists("graphmode_root",inherits=FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent=globalenv())
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),"graphmode_dev.R","graphmode_dev_run.R"))
        sys.source(file.path(graphmode_root,"R",f),kernel)
    env <- environment(); parent.env(env) <- kernel
    rng <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
    kind <- RNGkind(); checks <- 0L
    assert <- function(x) if(!isTRUE(x)) stop("D-044 assertion: ",paste(deparse(substitute(x)),collapse=" "),call.=FALSE)
    test <- function(name,code) {force(code);checks <<- checks+1L;cat("ok",checks,"-",name,"\n")}
    fails <- function(x) assert(inherits(tryCatch({force(x);NULL},error=function(e)e),"error"))
    patched <- function(bindings,code) {
        existing <- intersect(names(bindings),ls(kernel,all.names=TRUE)); original <- mget(existing,kernel)
        on.exit({list2env(original,kernel); added <- setdiff(names(bindings),existing)
            if(length(added)) rm(list=added,envir=kernel)},add=TRUE)
        list2env(bindings,kernel); force(code)
    }
    s <- graphmode_dev_run_spec()
    test("fixed scope, independent seeds and no formal release", {
        assert(s$rho==4L && s$iterations==600L && s$warmup==300L && s$total_seconds==2700)
        assert(s$cache_ffbs && s$observe && !anyDuplicated(s$seeds) && !length(intersect(s$seeds,s$panel_spec$seeds)))
        assert(s$policy$stop_at==200L && s$policy$stop_at<s$warmup && s$workers==1L)
    })
    test("no default worker, launch or batch authorization", {
        fails(graphmode_dev_run_worker(NULL,NULL));fails(graphmode_dev_run_launch(NULL,NULL,NULL));fails(graphmode_dev_run_batch(NULL,NULL))
        fails(graphmode_dev_run_generate(list()))
    })
    test("full next-child budget is reserved without extension", {
        assert(graphmode_dev_run_time_left(s,2039) && !graphmode_dev_run_time_left(s,2040))
        assert(!graphmode_dev_run_time_left(s,Inf) && !graphmode_dev_run_time_left(s,-1))
    })
    identity <- graphmode_dev_run_identity(graphmode_root); runtime <- graphmode4_pilot_runtime()
    test("identity contains the audited layers and actual new entry", {
        assert(identity$audited_unchanged && all(c("R/graphmode_dev.R","R/graphmode_dev_run.R",
            "scripts/graphmode-dev-run.R") %in% names(identity$sha256)))
        assert(graphmode_dev_run_loaded(graphmode_root,identity))
        patched(list(graphmode_dev_sweep=function(...) stop("changed")),
            assert(!graphmode_dev_run_loaded(graphmode_root,identity)))
    })
    fixture <- tempfile("graphmode-dev-run-fixed-");dir.create(fixture);fixture <- normalizePath(fixture)
    # Only this exact test-owned directory is removed, never scientific results.
    on.exit(unlink(fixture,recursive=TRUE),add=TRUE)
    short <- s;short$iterations <- 6L;short$warmup <- 4L;short$checkpoint_every <- 2L
    short$policy <- graphmode4_guidance_policy(2L,1L,.44,1,1,.6,.05,4)
    patched(list(graphmode_dev_run_spec=function() short), {
        directory <- file.path(fixture,"experiment");dir.create(directory)
        record <- graphmode_dev_run_record(graphmode_root,directory,identity,runtime)
        graphmode_save_new(record,file.path(directory,"registration.rds"))
        dir.create(record$output_dir);graphmode_save_new(list(record=record,authorized=TRUE),file.path(record$output_dir,"launch.rds"))
        expert <- short$panel_spec$expert;expert$rho <- 4L
        fit <- graphmode4_config(graphmode4_road("intertwined-spiral"),matrix(2,121,168),
            cbind(1,sin(2*pi*seq_len(168)/24),cos(2*pi*seq_len(168)/24)),
            "graphMoDE-W",expert,short$panel_spec$gate,short$role)
        e <- graphmode_dev_run_envelope(record,1L,fit,graphmode_initial_state(fit$core,rep(1L,121)))
        path <- file.path(record$output_dir,"PLAN-01.rds");graphmode_save_new(e,path)
        test("new envelope binds schema, source, cache, warmup and outer registration", {
            assert(graphmode_dev_run_envelope_check(e))
            for(field in c("cache_ffbs","observe","signature")) {
                bad <- e;bad[[field]] <- FALSE;fails(graphmode_dev_run_envelope_check(bad))
            }
            bad <- e;bad$plan$core_plan$seed <- 123L;fails(graphmode_dev_run_envelope_check(bad))
            fails(graphmode_dev_run_envelope_check(e$plan))
        })
        test("registration/output identity and no-reuse rules are active", {
            assert(graphmode_dev_run_tree(record,TRUE))
            bad <- record;bad$directory <- file.path(fixture,"missing");fails(graphmode_dev_run_tree(bad))
            patched(list(graphmode_dev_run_source_guard=function(...) TRUE),fails(graphmode_dev_run_preflight(record,graphmode_root)))
        })
        checked <- list(ready=TRUE,problems=character(),output_dir=e$plan$output_dir)
        hooks <- list(graphmode_dev_run_worker_guard=function(...) TRUE,
            graphmode4_controlled_preflight=function(...) checked,
            graphmode_normal=function(n) rep(.05,n),graphmode_uniform=function(n) rep(.5,n),
            graphmode_pg=function(b,z) b/4,graphmode_gamma=function(shape,rate) shape/rate)
        test("REAL reused worker executes development sweep and stores new checkpoints on fixed tapes", {
            patched(hooks,graphmode_dev_run_worker(e,graphmode_root,TRUE))
            result <- readRDS(file.path(e$plan$output_dir,"result.rds"))
            assert(graphmode_dev_run_result_check(e,result))
            cp <- result$checkpoint
            assert(cp$development$calls==6L && length(cp$development$observations)==6L && cp$control$frozen)
            assert(cp$control$updates==2L && length(cp$saved)==2L && cp$schema==graphmode_dev_run_version)
            assert(readRDS(file.path(e$plan$output_dir,"checkpoint-000000002.rds"))$development$calls==2L)
            assert(identical(readRDS(file.path(e$plan$output_dir,"registration.rds"))$envelope,e))
            fails(graphmode4_controlled_result_check(e$plan,result))
        })
        result <- readRDS(file.path(e$plan$output_dir,"result.rds"))
        test("missing cache execution and dropped observations cannot pass", {
            bad <- result;bad$checkpoint$development$calls <- 5L;fails(graphmode_dev_run_result_check(e,bad))
            bad <- result;bad$checkpoint$development$observations[[3]]$cache_ffbs <- FALSE;fails(graphmode_dev_run_result_check(e,bad))
            bad <- result;bad$checkpoint$development$identity <- list();fails(graphmode_dev_run_result_check(e,bad))
        })
        test("tampered observed proposal and allocation records fail", {
            bad <- result;bad$checkpoint$development$observations[[1]]$observation$experts[[1]]$observation$log_ratio <- 42
            fails(graphmode_dev_run_result_check(e,bad))
            bad <- result;bad$checkpoint$development$observations[[1]]$observation$allocation$combined$observed_node_moves <- -1L
            fails(graphmode_dev_run_result_check(e,bad))
        })
        test("scale sequence and retained freeze cannot be fabricated", {
            bad <- result;bad$checkpoint$diagnostics[[5]]$tuning$frozen_used <- FALSE;fails(graphmode_dev_run_result_check(e,bad))
            bad <- result;bad$checkpoint$control$sd <- 1;fails(graphmode_dev_run_result_check(e,bad))
        })
        execution <- list(status=0L,envelope_signature=e$signature,budget_seconds=short$worker_seconds,output="fixed worker")
        receipt <- list(schema=graphmode_dev_run_version,envelope_signature=e$signature,postflight_passed=TRUE,
            execution_signature=graphmode_digest(execution),result_signature=graphmode_digest(result))
        evidence <- list(registration=readRDS(file.path(e$plan$output_dir,"registration.rds")),result=result,
            execution=execution,acceptance=receipt,failure=FALSE,read_errors=character())
        test("success needs execution and result-bound receipt", {
            assert(graphmode_dev_run_evidence_check(e,evidence))
            for(status in c(1L,124L,NA_integer_)) {bad <- evidence;bad$execution$status <- status;fails(graphmode_dev_run_evidence_check(e,bad))}
            bad <- evidence;bad$acceptance <- NULL;fails(graphmode_dev_run_evidence_check(e,bad))
            bad <- evidence;bad$failure <- TRUE;fails(graphmode_dev_run_evidence_check(e,bad))
            bad <- evidence;bad$read_errors <- "broken";fails(graphmode_dev_run_evidence_check(e,bad))
        })
        test("launcher cannot grant acceptance to a timed-out saved result", {
            patched(list(graphmode_dev_run_worker_preflight=function(...) checked,
                graphmode_dev_run_child=function(...) list(status=124L,output="fixed timeout after result")),
                fails(graphmode_dev_run_launch(e,path,graphmode_root,TRUE)))
            assert(file.exists(file.path(e$plan$output_dir,"execution.rds")) &&
                !file.exists(file.path(e$plan$output_dir,"acceptance.rds")))
        })
        test("worker preserves failure and never resumes an occupied output", {
            patched(hooks,fails(suppressWarnings(graphmode_dev_run_worker(e,graphmode_root,TRUE))))
            e2 <- graphmode_dev_run_envelope(record,2L,fit,graphmode_initial_state(fit$core,rep(1:3,length.out=121)))
            checked2 <- list(ready=TRUE,problems=character(),output_dir=e2$plan$output_dir)
            changed <- hooks;changed$graphmode4_controlled_preflight <- function(...) checked2
            changed$graphmode_pg <- function(...) stop("fixed backend failure")
            patched(changed,fails(graphmode_dev_run_worker(e2,graphmode_root,TRUE)))
            cp <- readRDS(file.path(e2$plan$output_dir,"failure.rds"))
            assert(cp$development$calls==0L && cp$state$iteration==0L && !file.exists(file.path(e2$plan$output_dir,"result.rds")))
        })
        test("parent output disappearance is not rebuilt by guarded writes", {
            missing <- record;missing$output_dir <- file.path(fixture,"gone")
            fails(graphmode_dev_run_tree(missing,TRUE));assert(!dir.exists(missing$output_dir))
        })
        test("real launcher issues a result-bound receipt only after successful postflight", {
            e3 <- graphmode_dev_run_envelope(record,3L,fit,graphmode_initial_state(fit$core,rep(1:7,length.out=121)))
            path3 <- file.path(record$output_dir,"PLAN-03.rds");graphmode_save_new(e3,path3)
            checked3 <- list(ready=TRUE,problems=character(),output_dir=e3$plan$output_dir)
            changed <- hooks;changed$graphmode4_controlled_preflight <- function(...) checked3
            patched(changed,graphmode_dev_run_worker(e3,graphmode_root,TRUE))
            patched(list(graphmode_dev_run_worker_preflight=function(...) checked3,
                graphmode_dev_run_worker_guard=function(...) TRUE,
                graphmode_dev_run_child=function(...) list(status=0L,output="fixed successful child")),
                graphmode_dev_run_launch(e3,path3,graphmode_root,TRUE))
            assert(graphmode_dev_run_evidence_check(e3,graphmode_dev_run_evidence(e3)))
            damaged <- graphmode_dev_run_evidence(e3);damaged$execution$output <- "changed"
            fails(graphmode_dev_run_evidence_check(e3,damaged))
        })
        test("successful child with failed postflight remains unaccepted", {
            e4 <- graphmode_dev_run_envelope(record,4L,fit,graphmode_initial_state(fit$core,rep(1:10,length.out=121)))
            path4 <- file.path(record$output_dir,"PLAN-04.rds");graphmode_save_new(e4,path4)
            checked4 <- list(ready=TRUE,problems=character(),output_dir=e4$plan$output_dir)
            changed <- hooks;changed$graphmode4_controlled_preflight <- function(...) checked4
            patched(changed,graphmode_dev_run_worker(e4,graphmode_root,TRUE))
            patched(list(graphmode_dev_run_worker_preflight=function(...) checked4,
                graphmode_dev_run_worker_guard=function(...) stop("fixed postflight change"),
                graphmode_dev_run_child=function(...) list(status=0L,output="fixed child completed")),
                fails(graphmode_dev_run_launch(e4,path4,graphmode_root,TRUE)))
            assert(file.exists(file.path(e4$plan$output_dir,"execution.rds")) &&
                !file.exists(file.path(e4$plan$output_dir,"acceptance.rds")))
        })
        test("preparation canonicalizes alias parents and writes registration only", {
            alias <- file.path(fixture,"alias");assert(file.symlink(directory,alias))
            target <- file.path(alias,"new-registration")
            patched(list(graphmode_dev_run_source_guard=function(...) TRUE), {
                prepared <- graphmode_dev_run_prepare(graphmode_root,target)
                assert(prepared$scientific_draws==0L)
                registered <- readRDS(file.path(target,"registration.rds"))
                assert(identical(registered$directory,file.path(directory,"new-registration")))
                assert(!dir.exists(registered$output_dir))
                fails(graphmode_dev_run_prepare(graphmode_root,target))
            })
        })
        test("batch writes all plans then retains first mock dispatcher failure without retry", {
            folder <- file.path(fixture,"batch");dir.create(folder)
            reg <- graphmode_dev_run_record(graphmode_root,folder,identity,runtime)
            graphmode_save_new(reg,file.path(folder,"registration.rds"))
            generated <- list(panel=list(fit=fit),starts=lapply(short$start_occupancy,function(k) rep(seq_len(k),length.out=121)))
            calls <- 0L
            patched(list(graphmode_dev_run_source_guard=function(...) TRUE,
                graphmode_dev_run_generate=function(...) generated,
                graphmode_dev_run_child=function(...) {
                    assert(length(list.files(reg$output_dir,"^PLAN-"))==4L);calls <<- calls+1L
                    list(status=124L,output="fixed dispatcher timeout")
                }),fails(graphmode_dev_run_batch(reg,graphmode_root,TRUE)))
            assert(calls==1L && file.exists(file.path(reg$output_dir,"batch-failure.rds")) &&
                !file.exists(file.path(reg$output_dir,"completed.rds")))
        })
    })
    test("all fixed checks leave RNG unchanged", {
        assert(identical(kind,RNGkind()) && identical(rng,
            if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL))
    })
    cat("PASS:",checks,"fixed-input/mocked-process groups; no scientific simulation.\n")
})
