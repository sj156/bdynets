# Fixed numeric tapes and mocked processes only; no scientific RNG draws.
if(!exists("graphmode_root",inherits=FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent=globalenv())
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),
        "graphmode_dev.R","graphmode_dev_run.R","graphmode_warmup.R"))
        sys.source(file.path(graphmode_root,"R",f),kernel)
    env <- environment();parent.env(env) <- kernel
    rng <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
    kind <- RNGkind();checks <- 0L
    assert <- function(x) if(!isTRUE(x)) stop("D-045 assertion: ",paste(deparse(substitute(x)),collapse=" "),call.=FALSE)
    fails <- function(code,pattern=NULL) {
        error <- tryCatch({force(code);NULL},error=function(e)e)
        assert(inherits(error,"error"))
        if(!is.null(pattern)) assert(grepl(pattern,conditionMessage(error),fixed=TRUE))
        invisible(error)
    }
    test <- function(name,code) {force(code);checks <<- checks+1L;cat("ok",checks,"-",name,"\n");flush.console()}
    patched <- function(bindings,code) {
        existing <- intersect(names(bindings),ls(kernel,all.names=TRUE));old <- mget(existing,kernel)
        on.exit({list2env(old,kernel);added <- setdiff(names(bindings),existing)
            if(length(added)) rm(list=added,envir=kernel)},add=TRUE)
        list2env(bindings,kernel);force(code)
    }
    fixture <- tempfile("graphmode-warmup-fixed-");dir.create(fixture);fixture <- normalizePath(fixture)
    on.exit(unlink(fixture,recursive=TRUE),add=TRUE) # Exact test-owned directory only.
    s <- graphmode_warmup_spec()
    expert <- s$panel_spec$expert;expert$rho <- s$rho
    fit <- graphmode4_config(graphmode4_road("intertwined-spiral"),matrix(2,121,168),
        cbind(1,sin(2*pi*seq_len(168)/24),cos(2*pi*seq_len(168)/24)),
        "graphMoDE-W",expert,s$panel_spec$gate,s$role)
    test("registered A scope, seeds and unchanged target/thresholds", {
        assert(s$iterations==1200L && s$warmup==1000L && s$checkpoint_every==100L && s$thin==1L)
        assert(s$policy$stop_at==800L && s$policy$batch_size==25L && fit$core$guidance_proposal_sd==.25)
        assert(s$worker_seconds==1200 && s$dispatch_seconds==1260 && s$total_seconds+s$diagnostic_seconds==6000)
        assert(length(s$seeds)==24L && !anyDuplicated(s$seeds) && all(s$seeds<=.Machine$integer.max))
        assert(!length(intersect(s$seeds,graphmode_dev_run_spec()$seeds)))
        assert(identical(s$panel_spec$thresholds,graphmode_dev_run_spec()$panel_spec$thresholds))
        assert(graphmode_gate_dimension(fit$core)==2187L)
    })
    test("exactly 32 updates, last warmup step frozen and no later adaptation", {
        c <- graphmode4_guidance_control(fit$core,s$warmup,s$policy);schedule <- list()
        for(i in 1:1200) {c <- graphmode4_guidance_observe(c,rep(TRUE,10L),i);schedule[[i]] <- c}
        assert(schedule[[799]]$updates==31L && !schedule[[799]]$frozen)
        assert(schedule[[800]]$updates==32L && schedule[[800]]$frozen)
        assert(c$updates==32L && identical(c$sd,schedule[[800]]$sd))
        expected <- exp(min(log(4),log(.25)+.56*sum((1+1:32)^(-.6))))
        assert(abs(c$sd-expected)<1e-12)
    })
    active_seed <- 0L;seen_seeds <- integer();normal_calls <- 0L
    initial_hooks <- list(
        graphmode_pilot_seeded=function(seed,code) {
            old <- active_seed;active_seed <<- seed;seen_seeds <<- c(seen_seeds,seed)
            on.exit(active_seed <<- old);force(code)
        },sample.int=function(n,size=n,replace=FALSE) {
            assert(!replace);((seq_len(n)+as.integer(active_seed %% n)-1L) %% n+1L)[seq_len(size)]
        },graphmode_normal=function(n) {
            normal_calls <<- normal_calls+1L
            rep(.01+(active_seed %% 10)*.003+(normal_calls %% 10)*.0001,n)
        },graphmode_uniform=function(n) rep(seq(.2,.8,length.out=10L),length.out=n))
    starts <- patched(initial_hooks,lapply(1:4,function(j) graphmode_warmup_initial(fit$core,j,s)))
    test("all four components use their own registered stream, ten prior paths each", {
        expected <- unlist(lapply(1:4,function(j) unname(s$seeds[sprintf("%s_%02d",c("Z","v","x","theta"),j)])))
        assert(identical(seen_seeds,expected))
        assert(normal_calls==4L*(1L+10L*169L))
        assert(graphmode_warmup_starts_check(starts,fit,s))
        assert(all(vapply(starts,function(x) length(x$state$x)==2187L,logical(1))))
        for(j in 1:4) {
            sizes <- tabulate(starts[[j]]$state$Z,10L);nonzero <- sizes[sizes>0]
            assert(length(nonzero)==s$start_occupancy[j] && max(nonzero)-min(nonzero)<=1L)
            assert(length(unique(as.numeric(starts[[j]]$state$theta)))>10L)
        }
    })
    test("initial guidance coverage, no posterior clipping, duplicate states rejected", {
        a <- lapply(starts,function(x) plogis(x$state$v))
        assert(all(a[[1]]>.05 & a[[1]]<.25) && all(a[[2]]>.75 & a[[2]]<.95))
        assert(max(abs(sort(a[[3]])-seq(.05,.95,length.out=10)))<1e-14)
        assert(all(a[[4]]>.1 & a[[4]]<.9))
        assert(all(is.finite(graphmode_gate_utilities(starts[[1]]$state$x,rep(qlogis(.99),10),fit$core))))
        bad <- starts;bad[[2]]$state$x <- bad[[1]]$state$x;bad[[2]]$state_signature <- graphmode_digest(bad[[2]]$state)
        fails(graphmode_warmup_starts_check(bad,fit,s),"Duplicated")
    })
    test("prior includes theta0 and exactly one G/W transition per recorded time", {
        path <- patched(list(graphmode_normal=function(n) rep(1,n)),
            graphmode_prior_expert(matrix(1,3,1),0,matrix(4),matrix(2),matrix(9),"dynamic"))
        assert(identical(as.numeric(path),c(7,17,37)))
    })
    test("invalid initialization retains raw evidence and never redraws", {
        hooks <- initial_hooks;calls <- 0L
        hooks$graphmode_normal <- function(n) {calls <<- calls+1L;rep(Inf,n)}
        error <- patched(hooks,fails(graphmode_warmup_initial(fit$core,1L,s)))
        assert(inherits(error,"graphmode_initial_failure") && error$chain==1L && calls>=1L)
        assert(!is.null(error$raw$x) && all(is.infinite(error$raw$x)))
    })
    test("generation saves new panel and all full starts, with truth excluded from blind input", {
        saved <- list();initial_seeded <- initial_hooks$graphmode_pilot_seeded;seen_seeds <- integer()
        hooks <- initial_hooks
        hooks$graphmode_pilot_seeded <- function(seed,code) {
            if(seed %in% s$seeds[1:4]) {
                seen_seeds <<- c(seen_seeds,seed)
                if(seed==s$seeds[[1]]) return(array(sin(seq_len(5*168*3)/17),c(5L,168L,3L)))
                if(seed==s$seeds[[2]]) return(5:1)
                if(seed==s$seeds[[3]]) return(121:1)
                if(seed==s$seeds[[4]]) return(matrix(2,121,168))
            }
            initial_seeded(seed,code)
        }
        blind <- patched(hooks,graphmode_warmup_generate(s,function(x,n) saved[[n]] <<- x))
        assert(identical(names(saved),c("generation.rds",sprintf("INITIAL-%02d.rds",1:4),"blinded-inputs.rds")))
        assert(length(seen_seeds)==20L && !anyDuplicated(seen_seeds) && !is.null(saved[[1]]$truth))
        assert(identical(names(blind),c("schema","fit","starts","signature")))
        assert(graphmode_warmup_starts_check(blind$starts,blind$fit,s))
        assert(blind$fit$core$rho==4L && identical(blind$fit$core$Y,matrix(2,121,168)))
    })
    test("budget reserve refuses the boundary and invalid elapsed times", {
        assert(graphmode_warmup_time_left(s,4109) && !graphmode_warmup_time_left(s,4110))
        assert(!graphmode_warmup_time_left(s,-1) && !graphmode_warmup_time_left(s,Inf))
    })
    m <- list(last_scales=rep(1,8),final_scale=1,acceptance=.44,
        guidance_movement=rep(1,10),expert_updates=c(1,rep(0,9)),expert_movement=c(1,rep(0,9)))
    test("prospective candidate uses all four final scales, no automatic B", {
        metrics <- lapply(c(1,1.2,1.3,1.4),function(v) {x <- m;x$last_scales <- rep(v,8);x$final_scale <- v;x})
        result <- graphmode_warmup_reduce(metrics,s)
        assert(result$resolved && abs(result$candidate_scale-exp(mean(log(c(1,1.2,1.3,1.4)))))<1e-15)
        assert(!result$auto_continue && !result$phase_B_authorized && !result$convergence_certified)
    })
    test("each unresolved condition blocks candidate without dropping a chain", {
        bad_metrics <- list(m,m,m,m)
        cases <- list(function(x) {x$last_scales[1] <- .05;x},
            function(x) {x$last_scales[1] <- 4;x},function(x) {x$last_scales[1] <- .6;x},
            function(x) {x$last_scales <- rep(2.1,8);x$final_scale <- 2.1;x},
            function(x) {x$acceptance <- .66;x},function(x) {x$acceptance <- .24;x},
            function(x) {x$guidance_movement[10] <- 0;x},function(x) {x$expert_movement[1] <- 0;x})
        for(change in cases) {
            bad <- bad_metrics;bad[[4]] <- change(m);out <- graphmode_warmup_reduce(bad,s)
            assert(!out$resolved && is.na(out$candidate_scale) && length(out$metrics)==4L && !out$auto_continue)
        }
        bad <- m;bad$acceptance <- NA_real_;fails(graphmode_warmup_reduce(list(m,m,m,bad),s))
        fails(graphmode_warmup_reduce(list(m,m,m),s))
    })
    identity <- graphmode_warmup_identity(graphmode_root);runtime <- graphmode4_pilot_runtime()
    test("all old frozen layers unchanged, new source visible, unfrozen launch refused", {
        assert(identity$audited_unchanged)
        assert(all(c("R/graphmode_warmup.R","R/graphmode_dev_run.R","R/graphmode_dev.R") %in% names(identity$sha256)))
        assert(graphmode_dev_run_loaded(graphmode_root,identity))
        reg <- graphmode_warmup_record(graphmode_root,file.path(fixture,"unprepared"),identity,runtime)
        api <- graphmode_warmup_engine(reg)
        assert(api$graphmode_dev_run_validate(reg))
        uncommitted <- identity;uncommitted$committed <- FALSE
        reg2 <- graphmode_warmup_record(graphmode_root,reg$directory,uncommitted,runtime)
        patched(list(graphmode_warmup_identity=function(...) uncommitted),
            fails(graphmode_warmup_engine(reg2)$graphmode_dev_run_source_guard(reg2,graphmode_root),"unfrozen"))
        patched(list(graphmode_warmup_sweep=function(...) NULL),
            assert(!graphmode_dev_run_loaded(graphmode_root,identity)))
    })
    test("private adapters leave original functions and versions intact", {
        fn <- graphmode_dev_run_worker;version <- graphmode_dev_run_version
        api <- graphmode_warmup_engine()
        assert(identical(fn,graphmode_dev_run_worker) && identical(version,graphmode_dev_run_version))
        assert(api$graphmode_dev_run_version==graphmode_warmup_version &&
            identical(body(api$graphmode_dev_run_worker),body(fn)))
        fails(api$graphmode_dev_run_worker(NULL,NULL));fails(api$graphmode_dev_run_launch(NULL,NULL,NULL))
        fails(graphmode_warmup_batch(NULL,NULL));fails(graphmode_warmup_child("phase-B",NULL,NULL,1))
    })
    record <- graphmode_warmup_record(graphmode_root,fixture,identity,runtime)
    test("storage thresholds, canonical aliases and emergency receipts", {
        patched(list(graphmode_warmup_disk=function(...) c(free_bytes=16*1024^3,stage_bytes=0)),
            assert(length(graphmode_warmup_storage(record,TRUE))==2L))
        patched(list(graphmode_warmup_disk=function(...) c(free_bytes=14*1024^3,stage_bytes=0)),
            fails(graphmode_warmup_storage(record,TRUE)))
        patched(list(graphmode_warmup_disk=function(...) c(free_bytes=4*1024^3,stage_bytes=0)), {
            fails(graphmode_warmup_storage(record))
            fails(graphmode_warmup_save(record,list(),file.path(fixture,"normal.rds")))
            graphmode_warmup_save(record,list(error="fixed space failure"),file.path(fixture,"batch-failure.rds"))
            assert(file.exists(file.path(fixture,"batch-failure.rds")))
        })
        patched(list(graphmode_warmup_disk=function(...) c(free_bytes=20*1024^3,stage_bytes=6*1024^3)),
            fails(graphmode_warmup_storage(record)))
        fails(graphmode_warmup_save(record,list(),file.path(dirname(fixture),"escape.rds")))
        alias <- file.path(fixture,"alias");assert(file.symlink(fixture,alias))
        bad <- record;bad$directory <- alias;fails(graphmode_warmup_storage(bad))
        assert(identical(graphmode4_output_target(file.path(alias,"new")),file.path(fixture,"new")))
        unlink(alias) # Exact test symlink, not its target.
    })
    tape <- list(graphmode_normal=function(n) rep(.05,n),graphmode_uniform=function(n) rep(.5,n),
        graphmode_pg=function(b,z) b/4,graphmode_gamma=function(shape,rate) shape/rate)
    test("passive tracing preserves actual transition and random-call counts on identical fixed tape", {
        calls <- integer(4);names(calls) <- names(tape)
        hooked <- lapply(names(tape),function(name) {
            fun <- tape[[name]];force(name);force(fun)
            function(...) {calls[[name]] <<- calls[[name]]+1L;fun(...)}
        });names(hooked) <- names(tape)
        state <- starts[[1]]$state;control <- graphmode4_guidance_control(fit$core,s$warmup,s$policy)
        original <- patched(hooked,graphmode_dev_sweep(state,fit$core,control,TRUE,TRUE));counts <- calls;calls[] <- 0L
        observed <- patched(hooked,graphmode_warmup_sweep(state,fit$core,control,TRUE,TRUE))
        assert(identical(original$transition$out$state,observed$transition$out$state) && identical(counts,calls))
        assert(identical(original$transition$control,observed$transition$control))
        assert(identical(observed$observation$warmup$x,observed$transition$out$state$x))
        assert(abs(sum(observed$observation$warmup$seconds[-8])-observed$observation$warmup$seconds[8])<1e-8)
    })
    short <- s;short$iterations <- 6L;short$warmup <- 4L;short$checkpoint_every <- 2L
    short$policy <- graphmode4_guidance_policy(2L,1L,.44,1,1,.6,.05,4)
    patched(list(graphmode_warmup_spec=function() short), {
        directory <- file.path(fixture,"worker-case");dir.create(directory)
        record <- graphmode_warmup_record(graphmode_root,directory,identity,runtime)
        graphmode_save_new(record,file.path(directory,"registration.rds"))
        dir.create(record$output_dir);graphmode_save_new(list(record=record,authorized=TRUE),file.path(record$output_dir,"launch.rds"))
        blind <- list(schema=graphmode_warmup_version,fit=fit,starts=starts);blind$signature <- graphmode_digest(blind)
        graphmode_save_new(blind,file.path(record$output_dir,"blinded-inputs.rds"))
        for(j in 1:4) graphmode_save_new(starts[[j]],file.path(record$output_dir,sprintf("INITIAL-%02d.rds",j)))
        api <- graphmode_warmup_engine(record)
        api$graphmode_dev_run_source_guard <- function(...) TRUE # Fixed-fixture source, not a live registration.
        e <- api$graphmode_dev_run_envelope(record,1L,fit,starts[[1]]$state)
        path <- file.path(record$output_dir,"PLAN-01.rds");graphmode_save_new(e,path)
        test("blinded complete states bound to envelope; altered initial state or blind file rejected", {
            assert(api$graphmode_dev_run_worker_guard(e,graphmode_root))
            bad <- e;bad$plan$core_plan$initial_state$x[1] <- 3
            fails(api$graphmode_dev_run_worker_guard(bad,graphmode_root))
            changed <- blind;changed$starts[[1]]$state$v[1] <- 0
            # No actual file rewrite: reader injection models corrupt content.
            read <- base::readRDS
            patched(list(readRDS=function(file,...) if(basename(file)=="blinded-inputs.rds") changed else read(file,...)),
                fails(api$graphmode_dev_run_worker_guard(e,graphmode_root)))
        })
        checked <- list(ready=TRUE,problems=character(),output_dir=e$plan$output_dir)
        hooks <- c(tape,list(graphmode4_controlled_preflight=function(...) checked))
        test("real reused worker with FIXED tapes saves phase A observations/checkpoints", {
            patched(hooks,api$graphmode_dev_run_worker(e,graphmode_root,TRUE))
            result <- readRDS(file.path(e$plan$output_dir,"result.rds"))
            assert(api$graphmode_dev_run_result_check(e,result))
            assert(length(result$checkpoint$saved)==2L && result$checkpoint$development$calls==6L)
            assert(result$checkpoint$control$frozen && result$checkpoint$control$updates==2L)
            assert(readRDS(file.path(e$plan$output_dir,"checkpoint-000000002.rds"))$development$calls==2L)
        })
        result <- readRDS(file.path(e$plan$output_dir,"result.rds"))
        test("trace tampering, schema confusion and freeze fabrication rejected", {
            for(field in c("x","v","Z","before_occupancy","logit_jump_squared")) {
                bad <- result;bad$checkpoint$development$observations[[6]]$observation$warmup[[field]][1] <- 99
                fails(api$graphmode_dev_run_result_check(e,bad))
            }
            bad <- result;bad$checkpoint$development$observations[[5]]$observation$warmup$seconds[1] <- 99
            fails(api$graphmode_dev_run_result_check(e,bad))
            bad <- result;bad$checkpoint$diagnostics[[5]]$tuning$frozen_used <- FALSE
            fails(api$graphmode_dev_run_result_check(e,bad))
            fails(graphmode_dev_run_result_check(e,result))
        })
        test("timed-out saved result retains execution, cannot acquire receipt", {
            api$graphmode_dev_run_worker_preflight <- function(...) checked
            api$graphmode_dev_run_child <- function(...) list(status=124L,output="fixed timeout after result")
            fails(api$graphmode_dev_run_launch(e,path,graphmode_root,TRUE))
            assert(file.exists(file.path(e$plan$output_dir,"execution.rds")) && !file.exists(file.path(e$plan$output_dir,"acceptance.rds")))
        })
        test("result-bound receipts required; existing failure and changed output rejected", {
            execution <- list(status=0L,envelope_signature=e$signature,budget_seconds=short$worker_seconds,output="fixed success")
            acceptance <- list(schema=graphmode_warmup_version,envelope_signature=e$signature,
                postflight_passed=TRUE,result_signature=graphmode_digest(result),execution_signature=graphmode_digest(execution))
            evidence <- list(registration=readRDS(file.path(e$plan$output_dir,"registration.rds")),result=result,
                execution=execution,acceptance=acceptance,failure=FALSE,read_errors=character())
            assert(api$graphmode_dev_run_evidence_check(e,evidence))
            for(field in c("failure","read_errors","acceptance")) {
                bad <- evidence;bad[[field]] <- TRUE;fails(api$graphmode_dev_run_evidence_check(e,bad))
            }
            bad <- evidence;bad$execution$output <- "modified";fails(api$graphmode_dev_run_evidence_check(e,bad))
        })
        test("failed fixed backend is retained and occupied output is never resumed", {
            e2 <- api$graphmode_dev_run_envelope(record,2L,fit,starts[[2]]$state)
            path2 <- file.path(record$output_dir,"PLAN-02.rds");graphmode_save_new(e2,path2)
            checked2 <- list(ready=TRUE,problems=character(),output_dir=e2$plan$output_dir)
            api$graphmode_dev_run_worker_preflight <- function(...) checked2
            fail_hooks <- tape;fail_hooks$graphmode_pg <- function(...) stop("fixed backend failure")
            patched(fail_hooks,fails(api$graphmode_dev_run_worker(e2,graphmode_root,TRUE)))
            assert(file.exists(file.path(e2$plan$output_dir,"failure.rds")) && !file.exists(file.path(e2$plan$output_dir,"result.rds")))
            patched(fail_hooks,fails(suppressWarnings(api$graphmode_dev_run_worker(e2,graphmode_root,TRUE))))
        })
    })
    test("batch makes all plans before one failing mock dispatcher, no retries", {
        folder <- file.path(fixture,"batch-case");dir.create(folder)
        record <- graphmode_warmup_record(graphmode_root,folder,identity,runtime)
        graphmode_save_new(record,file.path(folder,"registration.rds"))
        original_engine <- graphmode_warmup_engine;calls <- 0L
        patched(list(graphmode_warmup_engine=function(record=NULL) {
            api <- original_engine(record);api$graphmode_dev_run_source_guard <- function(...) TRUE;api
        },graphmode_warmup_generate=function(spec,persist) {
            blind <- list(schema=graphmode_warmup_version,fit=fit,starts=starts)
            blind$signature <- graphmode_digest(blind);persist(blind,"blinded-inputs.rds");blind
        },graphmode_warmup_child=function(...) {
            assert(length(list.files(record$output_dir,"^PLAN-"))==4L);calls <<- calls+1L
            list(status=124L,output="fixed dispatcher timeout")
        }),fails(graphmode_warmup_batch(record,graphmode_root,TRUE),"dispatcher failed"))
        assert(calls==1L && file.exists(file.path(record$output_dir,"batch-failure.rds")) &&
            !file.exists(file.path(record$output_dir,"completed.rds")))
    })
    test("storage growth after a write stops normal work and preserves the written file", {
        folder <- file.path(fixture,"growing");dir.create(folder)
        record <- graphmode_warmup_record(graphmode_root,folder,identity,runtime);calls <- 0L
        patched(list(graphmode_warmup_disk=function(...) {
            calls <<- calls+1L;c(free_bytes=20*1024^3,stage_bytes=if(calls==1L) 0 else 6*1024^3)
        }),fails(graphmode_warmup_save(record,list(fixed=TRUE),file.path(folder,"checkpoint-fixed.rds"))))
        assert(calls==2L && file.exists(file.path(folder,"checkpoint-fixed.rds")))
    })
    test("new child entry carries exact timeout and authorization, without a real process", {
        captured <- NULL
        patched(list(system2=function(command,args,stdout,stderr,timeout,...) {
            captured <<- list(command=command,args=args,timeout=timeout)
            structure("fixed child",status=124L)
        }), {
            x <- graphmode_warmup_child("worker",file.path(fixture,"path with spaces.rds"),graphmode_root,1200)
            assert(x$status==124L && captured$timeout==1200)
            assert(any(grepl("scripts/graphmode-warmup.R",captured$args,fixed=TRUE)) && tail(captured$args,1)=="--authorized")
            assert(!any(grepl("scripts/graphmode-dev-run.R",captured$args,fixed=TRUE)))
        })
    })
    test("diagnostic timeout after report write cannot issue acceptance or restart chains", {
        folder <- file.path(fixture,"diagnostic-timeout");dir.create(folder)
        record <- graphmode_warmup_record(graphmode_root,folder,identity,runtime)
        graphmode_save_new(record,file.path(folder,"registration.rds"));dir.create(record$output_dir)
        graphmode_save_new(list(record=record,authorized=TRUE),file.path(record$output_dir,"launch.rds"))
        original_engine <- graphmode_warmup_engine;calls <- 0L
        hooks <- list(graphmode_warmup_engine=function(record=NULL) {
            api <- original_engine(record);api$graphmode_dev_run_source_guard <- function(...) TRUE;api
        },graphmode_warmup_child=function(command,path,repository,timeout,log_file=NULL) {
            calls <<- calls+1L;assert(command=="diagnostic-worker" && timeout==600)
            graphmode_save_new(list(registration_signature=record$signature),file.path(folder,"diagnostic-report.rds"))
            list(status=124L,output="fixed diagnostic timeout after write")
        })
        patched(hooks, {
            fails(graphmode_warmup_execute(record,file.path(folder,"registration.rds"),graphmode_root,TRUE,TRUE),"timed out")
            fails(graphmode_warmup_execute(record,file.path(folder,"registration.rds"),graphmode_root,TRUE,TRUE),"Already attempted")
        })
        assert(calls==1L && file.exists(file.path(folder,"diagnostic-report.rds")) &&
            !file.exists(file.path(folder,"diagnostic-acceptance.rds")))
        assert(readRDS(file.path(folder,"diagnostic-execution.rds"))$status==124L)
    })
    test("successful diagnostic receipt binds report AND exit evidence; no automatic B", {
        folder <- file.path(fixture,"diagnostic-success");dir.create(folder)
        record <- graphmode_warmup_record(graphmode_root,folder,identity,runtime)
        graphmode_save_new(record,file.path(folder,"registration.rds"));dir.create(record$output_dir)
        graphmode_save_new(list(record=record,authorized=TRUE),file.path(record$output_dir,"launch.rds"))
        original_engine <- graphmode_warmup_engine
        report <- list(schema=graphmode_warmup_version,registration_signature=record$signature,
            phase_A_screen=graphmode_warmup_reduce(rep(list(m),4),s))
        patched(list(graphmode_warmup_engine=function(record=NULL) {
            api <- original_engine(record);api$graphmode_dev_run_source_guard <- function(...) TRUE;api
        },graphmode_warmup_child=function(command,path,repository,timeout,log_file=NULL) {
            assert(command=="diagnostic-worker" && timeout==600)
            graphmode_save_new(report,file.path(folder,"diagnostic-report.rds"))
            list(status=0L,output="fixed diagnostic success")
        }), {
            result <- graphmode_warmup_execute(record,file.path(folder,"registration.rds"),graphmode_root,TRUE,TRUE)
            assert(!result$phase_B_authorized && graphmode_warmup_diagnostic_acceptance(record,graphmode_root))
        })
        receipt <- readRDS(file.path(folder,"diagnostic-acceptance.rds"))
        assert(receipt$postflight_passed && !receipt$phase_B_authorized &&
            identical(receipt$report_signature,graphmode_digest(report)) &&
            identical(receipt$execution_signature,graphmode_digest(readRDS(file.path(folder,"diagnostic-execution.rds")))))
    })
    diagnostic_case <- function(name) {
        folder <- file.path(fixture,name);dir.create(folder)
        record <- graphmode_warmup_record(graphmode_root,folder,identity,runtime)
        path <- file.path(folder,"registration.rds")
        graphmode_save_new(record,path);dir.create(record$output_dir)
        graphmode_save_new(list(record=record,authorized=TRUE),file.path(record$output_dir,"launch.rds"))
        original_engine <- graphmode_warmup_engine;calls <- 0L
        report <- list(schema=graphmode_warmup_version,registration_signature=record$signature,
            phase_A_screen=list(auto_continue=FALSE,phase_B_authorized=FALSE))
        list(record=record,folder=folder,path=path,calls=function() calls,
            run=function() graphmode_warmup_execute(record,path,graphmode_root,TRUE,TRUE),
            check=function() graphmode_warmup_diagnostic_acceptance(record,graphmode_root),
            hooks=list(graphmode_warmup_engine=function(record=NULL) {
                api <- original_engine(record);api$graphmode_dev_run_source_guard <- function(...) TRUE;api
            },graphmode_warmup_child=function(command,path,repository,timeout,log_file=NULL) {
                calls <<- calls+1L;assert(command=="diagnostic-worker" && timeout==600)
                graphmode_save_new(report,file.path(folder,"diagnostic-report.rds"))
                list(status=0L,output="fixed controller postflight fixture")
            }))
    }
    test("P2 receipt bytes cross quota: pending retained, no success publication, emergency failure persisted", {
        d <- diagnostic_case("receipt-quota")
        d$hooks$graphmode_warmup_disk <- function(...) c(free_bytes=20*1024^3,
            stage_bytes=5*1024^3+if(file.exists(file.path(d$folder,"diagnostic-acceptance-pending.rds"))) 1 else -1)
        patched(d$hooks, {
            fails(d$run(),"storage budget exhausted")
            assert(readRDS(file.path(d$folder,"diagnostic-execution.rds"))$status==0L)
            assert(file.exists(file.path(d$folder,"diagnostic-acceptance-pending.rds")))
            assert(!file.exists(file.path(d$folder,"diagnostic-acceptance.rds")))
            assert(identical(readRDS(file.path(d$folder,"diagnostic-controller-failure.rds"))$accepted,FALSE))
            fails(d$check(),"failure evidence");fails(d$run(),"Already attempted")
            assert(d$calls()==1L)
        })
    })
    test("late pre-publication storage failure remains unaccepted even after space becomes available", {
        d <- diagnostic_case("late-receipt-quota");checks_after_write <- 0L
        d$hooks$graphmode_warmup_disk <- function(...) {
            if(file.exists(file.path(d$folder,"diagnostic-acceptance-pending.rds"))) checks_after_write <<- checks_after_write+1L
            c(free_bytes=20*1024^3,stage_bytes=5*1024^3+if(checks_after_write>=2L) 1 else -1)
        }
        patched(d$hooks,fails(d$run(),"storage budget exhausted"))
        assert(checks_after_write==2L && !file.exists(file.path(d$folder,"diagnostic-acceptance.rds")))
        assert(file.exists(file.path(d$folder,"diagnostic-acceptance-pending.rds")))
        assert(file.exists(file.path(d$folder,"diagnostic-controller-failure.rds")))
        d$hooks$graphmode_warmup_disk <- function(...) c(free_bytes=20*1024^3,stage_bytes=0)
        patched(d$hooks,{fails(d$check(),"failure evidence");fails(d$run(),"Already attempted")})
        assert(d$calls()==1L)
    })
    test("successful atomic publication is the final controller I/O, then read-only acceptance is available", {
        d <- diagnostic_case("receipt-commit-boundary");hooks <- d$hooks;original_disk <- graphmode_warmup_disk
        hooks$graphmode_warmup_disk <- function(directory) {
            assert(!file.exists(file.path(directory,"diagnostic-acceptance.rds")))
            original_disk(directory)
        }
        patched(hooks,d$run())
        patched(d$hooks,assert(d$check()))
        assert(!file.exists(file.path(d$folder,"diagnostic-acceptance-pending.rds")))
    })
    test("failure-receipt I/O error cannot publish pending acceptance or permit retry", {
        d <- diagnostic_case("receipt-full-disk");original_save <- graphmode_warmup_save
        d$hooks$graphmode_warmup_disk <- function(...) c(free_bytes=20*1024^3,
            stage_bytes=if(file.exists(file.path(d$folder,"diagnostic-acceptance-pending.rds"))) 6*1024^3 else 0)
        d$hooks$graphmode_warmup_save <- function(record,object,path) {
            if(basename(path)=="diagnostic-controller-failure.rds") stop("fixed emergency write failure")
            original_save(record,object,path)
        }
        patched(d$hooks, {
            fails(suppressMessages(d$run()),"storage budget exhausted")
            assert(!file.exists(file.path(d$folder,"diagnostic-controller-failure.rds")))
            assert(!file.exists(file.path(d$folder,"diagnostic-acceptance.rds")))
            fails(d$check(),"missing, pending or changed");fails(d$run(),"Already attempted")
        })
    })
    test("request write failure never dispatches; direct worker cannot bypass controller failure", {
        d <- diagnostic_case("receipt-request-quota")
        d$hooks$graphmode_warmup_disk <- function(...) c(free_bytes=20*1024^3,
            stage_bytes=if(file.exists(file.path(d$folder,"diagnostic-request.rds"))) 6*1024^3 else 0)
        patched(d$hooks, {
            fails(d$run(),"storage budget exhausted")
            assert(d$calls()==0L && file.exists(file.path(d$folder,"diagnostic-controller-failure.rds")))
            fails(graphmode_warmup_diagnostic_worker(d$record,graphmode_root,TRUE),"already attempted")
            fails(d$run(),"Already attempted")
        })
    })
    test("zero child exit plus parent source-postflight failure is not diagnostic acceptance", {
        d <- diagnostic_case("receipt-source-change");original_engine <- graphmode_warmup_engine;calls <- 0L
        d$hooks$graphmode_warmup_engine <- function(record=NULL) {
            api <- original_engine(record)
            api$graphmode_dev_run_source_guard <- function(...) {
                calls <<- calls+1L;if(calls>1L) stop("fixed source changed");TRUE
            };api
        }
        patched(d$hooks,fails(d$run(),"fixed source changed"))
        assert(readRDS(file.path(d$folder,"diagnostic-execution.rds"))$status==0L)
        assert(file.exists(file.path(d$folder,"diagnostic-controller-failure.rds")))
        assert(!file.exists(file.path(d$folder,"diagnostic-acceptance.rds")))
    })
    test("publication failure preserves pending evidence and fails closed", {
        d <- diagnostic_case("receipt-publication-failure")
        d$hooks$file.rename <- function(from,to) {assert(basename(from)=="diagnostic-acceptance-pending.rds");FALSE}
        patched(d$hooks, {
            fails(d$run(),"Cannot publish")
            assert(file.exists(file.path(d$folder,"diagnostic-acceptance-pending.rds")))
            assert(!file.exists(file.path(d$folder,"diagnostic-acceptance.rds")))
            fails(d$check(),"failure evidence")
        })
    })
    test("accepted diagnostic requires matching final receipt, request, report, exit and no failure evidence", {
        for(name in c("report","execution","request","acceptance","missing","worker-failure","parent-failure")) {
            d <- diagnostic_case(paste0("receipt-tamper-",name))
            patched(d$hooks, {
                d$run();assert(d$check())
                if(name %in% c("worker-failure","parent-failure")) {
                    file <- if(name=="worker-failure") "diagnostic-failure.rds" else "diagnostic-controller-failure.rds"
                    graphmode_save_new(list(error="fixed failure"),file.path(d$folder,file))
                } else {
                    file <- file.path(d$folder,paste0("diagnostic-",if(name=="missing") "acceptance" else name,".rds"))
                    value <- readRDS(file)
                    # Move only this test-owned fixture to preserve the original evidence.
                    assert(file.rename(file,paste0(file,".original")))
                    if(name!="missing") {
                        if(name=="execution") value$status <- 124L else value$changed <- TRUE
                        graphmode_save_new(value,file)
                    }
                }
                fails(d$check());fails(d$run(),"Already attempted");assert(d$calls()==1L)
            })
        }
    })
    test("unfrozen preparation and unauthorized execution create no output", {
        folder <- file.path(fixture,"not-created")
        uncommitted <- identity;uncommitted$committed <- FALSE
        patched(list(graphmode_warmup_identity=function(...) uncommitted),
            fails(graphmode_warmup_prepare(graphmode_root,folder),"unfrozen"))
        assert(!dir.exists(folder))
        fails(graphmode_warmup_execute(NULL,NULL,NULL));fails(graphmode_warmup_diagnostic_worker(NULL,NULL))
    })
    test("end-to-end four FIXED-tape records produce descriptive diagnostics and unresolved A, not release", {
        short <- s;short$iterations <- 8L;short$warmup <- 4L;short$checkpoint_every <- 4L
        short$policy <- graphmode4_guidance_policy(2L,1L,.44,1,1,.6,.05,4)
        short$screen$last_updates <- 2L
        folder <- file.path(fixture,"fixed-diagnostic-integration");dir.create(folder)
        original_engine <- graphmode_warmup_engine
        hooks <- c(tape,list(graphmode_warmup_spec=function() short,
            graphmode_warmup_engine=function(record=NULL) {
                api <- original_engine(record);api$graphmode_dev_run_source_guard <- function(...) TRUE;api
            },graphmode4_controlled_preflight=function(plan,...) list(ready=TRUE,problems=character(),output_dir=plan$output_dir)))
        patched(hooks, {
            record <- graphmode_warmup_record(graphmode_root,folder,identity,runtime)
            api <- graphmode_warmup_engine(record)
            graphmode_save_new(record,file.path(folder,"registration.rds"));dir.create(record$output_dir)
            graphmode_save_new(list(record=record,authorized=TRUE),file.path(record$output_dir,"launch.rds"))
            blind <- list(schema=graphmode_warmup_version,fit=fit,starts=starts);blind$signature <- graphmode_digest(blind)
            graphmode_save_new(blind,file.path(record$output_dir,"blinded-inputs.rds"))
            for(j in 1:4) graphmode_save_new(starts[[j]],file.path(record$output_dir,sprintf("INITIAL-%02d.rds",j)))
            for(j in 1:4) {
                e <- api$graphmode_dev_run_envelope(record,j,fit,starts[[j]]$state)
                path <- file.path(record$output_dir,sprintf("PLAN-%02d.rds",j));graphmode_save_new(e,path)
                api$graphmode_dev_run_worker(e,graphmode_root,TRUE)
                api$graphmode_dev_run_child <- function(...) list(status=0L,output="fixed-tape worker, mocked exit")
                api$graphmode_dev_run_worker_preflight <- function(...) list(ready=TRUE,output_dir=e$plan$output_dir)
                api$graphmode_dev_run_launch(e,path,graphmode_root,TRUE)
                graphmode_save_new(list(status=0L,output="fixed dispatcher"),file.path(record$output_dir,sprintf("DISPATCH-%02d.rds",j)))
                # Restore ordinary worker preflight for the next unused folder.
                api <- graphmode_warmup_engine(record)
            }
            graphmode_save_new(list(status=0L,registration_signature=record$signature),file.path(folder,"batch-execution.rds"))
            graphmode_save_new(list(schema=graphmode_warmup_version,registration_signature=record$signature,
                elapsed_seconds=100),file.path(record$output_dir,"completed.rds"))
            report <- graphmode_warmup_diagnose(record,graphmode_root)
            assert(nrow(report$validity$scalars)==37L && nrow(report$validity$psm_rms)==6L)
            assert(length(report$traces)==4L && identical(dim(report$traces[[1]]$x),c(8L,2187L)))
            assert(!report$phase_A_screen$resolved && !report$phase_A_screen$auto_continue && !report$convergence_certified)
            assert(identical(dim(report$traces[[1]]$required_scalars),c(4L,37L)))
            assert(length(report$traces[[1]]$acceptance_by_occupancy)==2L)
        })
    })
    test("all fixed checks leave RNG state and kind unchanged", {
        assert(identical(kind,RNGkind()) && identical(rng,
            if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL))
    })
    cat("PASS:",checks,"fixed-input/mocked-process groups; no scientific simulation.\n")
})
