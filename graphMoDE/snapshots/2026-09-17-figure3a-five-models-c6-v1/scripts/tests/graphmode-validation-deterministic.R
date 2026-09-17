# Fixed-input B tests; no scientific RNG or real worker subprocess.
if(!exists("graphmode_root",inherits=FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent=globalenv())
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),
        "graphmode_dev.R","graphmode_dev_run.R","graphmode_warmup.R","graphmode_validation.R"))
        sys.source(file.path(graphmode_root,"R",f),kernel)
    env <- environment();parent.env(env) <- kernel
    rng <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
    kind <- RNGkind();checks <- 0L
    assert <- function(x) if(!isTRUE(x)) stop("B assertion: ",paste(deparse(substitute(x)),collapse=" "),call.=FALSE)
    fails <- function(code,pattern=NULL) {
        e <- tryCatch({force(code);NULL},error=function(e)e);assert(inherits(e,"error"))
        if(!is.null(pattern)) assert(grepl(pattern,conditionMessage(e),fixed=TRUE));invisible(e)
    }
    test <- function(name,code) {force(code);checks <<- checks+1L;cat("ok",checks,"-",name,"\n");flush.console()}
    patched <- function(bindings,code) {
        existing <- intersect(names(bindings),ls(kernel,all.names=TRUE));old <- mget(existing,kernel)
        on.exit({list2env(old,kernel);added <- setdiff(names(bindings),existing)
            if(length(added)) rm(list=added,envir=kernel)},add=TRUE)
        list2env(bindings,kernel);force(code)
    }
    fixture <- tempfile("graphmode-B-fixed-");dir.create(fixture);fixture <- normalizePath(fixture)
    # Retain this exact test-owned directory for inspection; no broad cleanup.
    evidence <- list(directory=file.path(fixture,"A"),
        registration_signature="f248b516d6de752aa7e8b259d26023911256bf85e181001a13dd7a9aa18aefff",
        source_commit="b698fd83ef70e60e3b922229f88ee26464149655",
        source_sha256=setNames(rep(strrep("a",64),47),paste0("source",1:47)),
        file_sha256=setNames(rep(strrep("b",64),5),c("registration.rds","diagnostic-request.rds",
            "diagnostic-execution.rds","diagnostic-report.rds","diagnostic-acceptance.rds")),
        candidate_scale=2.26889626877594,reviewed=TRUE)
    s <- graphmode_validation_spec(evidence$candidate_scale)
    identity <- graphmode_validation_identity(graphmode_root);runtime <- graphmode4_pilot_runtime()
    record <- graphmode_validation_record(graphmode_root,fixture,identity,runtime,evidence)
    ctx <- graphmode_validation_context(record);api <- ctx$graphmode_warmup_engine(record)
    test("B scope, exact scale, independent seeds and original thresholds", {
        assert(s$iterations==6000L && s$warmup==1500L && s$checkpoint_every==500L && s$thin==1L && is.null(s$policy))
        assert(identical(s$candidate_scale,evidence$candidate_scale))
        assert(s$total_seconds+s$diagnostic_seconds==25200 && s$worker_seconds==5640 && s$dispatch_seconds==5760)
        assert(length(s$seeds)==24L && !anyDuplicated(s$seeds) && !length(intersect(s$seeds,graphmode_warmup_spec()$seeds)))
        assert(identical(s$panel_spec$thresholds,graphmode_warmup_spec()$panel_spec$thresholds))
        assert(api$graphmode_dev_run_validate(record))
        bad <- record;bad$spec$candidate_scale <- 2;fails(api$graphmode_dev_run_validate(bad))
        bad <- evidence;bad$reviewed <- FALSE;fails(graphmode_validation_A_shape(bad))
    })
    test("unmodified source layers and private B routing", {
        assert(identity$audited_unchanged && graphmode_dev_run_loaded(graphmode_root,identity))
        assert(graphmode_warmup_version!=ctx$graphmode_warmup_version)
        assert(identical(body(api$graphmode_dev_run_worker),body(graphmode_dev_run_worker)))
        assert(identical(body(ctx$graphmode_warmup_execute_original),body(graphmode_warmup_execute)))
        assert(!ctx$graphmode_warmup_time_left(s,18180) && ctx$graphmode_warmup_time_left(s,18179))
        fails(ctx$graphmode_warmup_batch(NULL,NULL));fails(ctx$graphmode_warmup_execute(NULL,NULL,NULL))
    })
    expert <- s$panel_spec$expert;expert$rho <- s$rho
    gate <- s$panel_spec$gate;gate$guidance_proposal_sd <- evidence$candidate_scale
    fit <- graphmode4_config(graphmode4_road("intertwined-spiral"),matrix(2,121,168),
        cbind(1,sin(2*pi*seq_len(168)/24),cos(2*pi*seq_len(168)/24)),
        "graphMoDE-W",expert,gate,s$role)
    test("6000 consecutive steps remain fixed with zero adaptations for all acceptance extremes", {
        for(accept in list(rep(TRUE,10),rep(FALSE,10),rep(c(TRUE,FALSE),5))) {
            control <- graphmode4_guidance_control(fit$core,s$warmup,s$policy)
            for(i in 1:6000) {
                assert(control$frozen && identical(control$sd,evidence$candidate_scale))
                control <- graphmode4_guidance_observe(control,accept,i)
            }
            assert(control$updates==0L && control$batch_proposals==0L && control$iteration==6000L)
        }
    })
    active_seed <- 0L;seen <- integer();calls <- 0L
    initial_hooks <- list(graphmode_pilot_seeded=function(seed,code) {
        old <- active_seed;active_seed <<- seed;seen <<- c(seen,seed);on.exit(active_seed <<- old);force(code)
    },sample.int=function(n,size=n,replace=FALSE) {assert(!replace);((seq_len(n)+active_seed%%n-1L)%%n+1L)[seq_len(size)]},
        graphmode_normal=function(n) {calls <<- calls+1L;rep(.01+active_seed%%10*.003+calls%%10*.0001,n)},
        graphmode_uniform=function(n) rep(seq(.2,.8,length.out=10),length.out=n))
    starts <- patched(initial_hooks,lapply(1:4,function(j) ctx$graphmode_warmup_initial(fit$core,j,s)))
    test("immutable generator uses B data streams and binds candidate before complete initialization", {
        hooks <- initial_hooks;initial_seeded <- hooks$graphmode_pilot_seeded;saved <- list();data_seen <- integer()
        hooks$graphmode_pilot_seeded <- function(seed,code) {
            if(seed %in% s$seeds[1:4]) {
                data_seen <<- c(data_seen,seed)
                if(seed==s$seeds[[1]]) return(array(sin(seq_len(5*168*3)/17),c(5L,168L,3L)))
                if(seed==s$seeds[[2]]) return(5:1)
                if(seed==s$seeds[[3]]) return(121:1)
                if(seed==s$seeds[[4]]) return(matrix(2,121,168))
            }
            initial_seeded(seed,code)
        }
        blind <- patched(hooks,ctx$graphmode_warmup_generate(s,function(x,n) saved[[n]] <<- x))
        assert(identical(data_seen,unname(s$seeds[1:4])))
        assert(identical(blind$fit$core$guidance_proposal_sd,evidence$candidate_scale))
        assert(identical(names(blind),c("schema","fit","starts","signature")) && blind$schema==graphmode_validation_version)
        assert(ctx$graphmode_warmup_starts_check(blind$starts,blind$fit,s))
        assert(identical(names(saved),c("generation.rds",sprintf("INITIAL-%02d.rds",1:4),"blinded-inputs.rds")))
    })
    test("complete independent B start streams and no A-state transfer", {
        assert(identical(seen[1:16],unlist(lapply(1:4,function(j) unname(s$seeds[sprintf("%s_%02d",c("Z","v","x","theta"),j)])))))
        assert(ctx$graphmode_warmup_starts_check(starts,fit,s))
        assert(!any(c("state","Y","theta","x","v") %in% names(record$phase_A)))
        badfit <- graphmode4_config(graphmode4_road("intertwined-spiral"),fit$core$Y,fit$core$Fmat,
            "graphMoDE-W",expert,graphmode_warmup_spec()$panel_spec$gate,s$role)
        fails(api$graphmode_dev_run_envelope(record,1L,badfit,starts[[1]]$state),"exact common A scale")
    })
    test("B child routes to new entry and preserves timeout without launching", {
        captured <- NULL
        patched(list(system2=function(command,args,stdout,stderr,timeout,...) {
            captured <<- list(args=args,timeout=timeout);structure("fixed child",status=124L)
        }), {
            x <- ctx$graphmode_warmup_child("worker",file.path(fixture,"spaced input.rds"),graphmode_root,5640)
            assert(x$status==124L && captured$timeout==5640 && any(grepl("scripts/graphmode-validation.R",captured$args,fixed=TRUE)))
        })
    })
    test("B storage limits preserve files and reject insufficient capacity", {
        ctx$graphmode_warmup_disk <- function(...) c(free_bytes=39*1024^3,stage_bytes=0)
        fails(ctx$graphmode_warmup_storage(record,TRUE))
        ctx$graphmode_warmup_disk <- function(...) c(free_bytes=41*1024^3,stage_bytes=21*1024^3)
        fails(ctx$graphmode_warmup_storage(record))
        ctx$graphmode_warmup_disk <- graphmode_warmup_disk
    })
    # Integration tests below deliberately shorten ONLY the isolated fixture.
    # Both RNG primitives and subprocess calls are replaced by fixed tapes.
    short <- s;short$iterations <- 8L;short$warmup <- 4L;short$checkpoint_every <- 4L
    tape <- list(graphmode_normal=function(n) rep(.05,n),graphmode_uniform=function(n) rep(.5,n),
        graphmode_pg=function(b,z) b/4,graphmode_gamma=function(shape,rate) shape/rate)
    patched(c(tape,list(graphmode_validation_spec=function(...) short,
        graphmode_validation_A_check=function(...) TRUE,
        graphmode4_controlled_preflight=function(plan,...) list(ready=TRUE,problems=character(),output_dir=plan$output_dir))), {
        folder <- file.path(fixture,"integration");dir.create(folder)
        record <- graphmode_validation_record(graphmode_root,folder,identity,runtime,evidence)
        ctx <- graphmode_validation_context(record);engine <- ctx$graphmode_warmup_engine
        ctx$graphmode_warmup_engine <- function(record=NULL) {
            api <- engine(record);api$graphmode_dev_run_source_guard <- function(...) TRUE;api
        }
        api <- ctx$graphmode_warmup_engine(record)
        graphmode_save_new(record,file.path(folder,"registration.rds"));dir.create(record$output_dir)
        graphmode_save_new(list(record=record,authorized=TRUE),file.path(record$output_dir,"launch.rds"))
        blind <- list(schema=graphmode_validation_version,fit=fit,starts=starts);blind$signature <- graphmode_digest(blind)
        graphmode_save_new(blind,file.path(record$output_dir,"blinded-inputs.rds"))
        for(j in 1:4) graphmode_save_new(starts[[j]],file.path(record$output_dir,sprintf("INITIAL-%02d.rds",j)))
        test("four actual reused workers on fixed tapes retain B identity and constant scale", {
            for(j in 1:4) {
                e <- api$graphmode_dev_run_envelope(record,j,fit,starts[[j]]$state)
                path <- file.path(record$output_dir,sprintf("PLAN-%02d.rds",j));graphmode_save_new(e,path)
                api$graphmode_dev_run_worker(e,graphmode_root,TRUE)
                result <- readRDS(file.path(e$plan$output_dir,"result.rds"))
                assert(result$schema==graphmode_validation_version && result$checkpoint$control$updates==0L)
                assert(length(result$checkpoint$saved)==4L && api$graphmode_dev_run_result_check(e,result))
                bad <- result;bad$checkpoint$diagnostics[[1]]$tuning$frozen_used <- FALSE
                fails(api$graphmode_dev_run_result_check(e,bad))
                bad <- result;bad$checkpoint$development$observations[[8]]$observation$warmup$v[1] <- 99
                fails(api$graphmode_dev_run_result_check(e,bad))
                api$graphmode_dev_run_child <- function(...) list(status=0L,output="fixed worker/mock exit")
                api$graphmode_dev_run_worker_preflight <- function(...) list(ready=TRUE,output_dir=e$plan$output_dir)
                api$graphmode_dev_run_launch(e,path,graphmode_root,TRUE)
                graphmode_save_new(list(status=0L),file.path(record$output_dir,sprintf("DISPATCH-%02d.rds",j)))
                api <- ctx$graphmode_warmup_engine(record)
            }
        })
        graphmode_save_new(list(status=0L,registration_signature=record$signature),file.path(folder,"batch-execution.rds"))
        graphmode_save_new(list(schema=graphmode_validation_version,registration_signature=record$signature,
            elapsed_seconds=100),file.path(record$output_dir,"completed.rds"))
        test("B diagnostic retains all original scalars and PSM gates, not an A screen", {
            report <- ctx$graphmode_warmup_diagnose(record,graphmode_root)
            assert(nrow(report$validity$scalars)==37L && nrow(report$validity$psm_rms)==6L)
            assert(is.null(report$phase_A_screen) && !report$auto_continue && !report$formal_authorized)
            assert(identical(report$fixed_scale,evidence$candidate_scale) && length(report$traces)==4L)
            assert(identical(dim(report$traces[[1]]$x),c(8L,2187L)))
        })
        test("diagnostic final receipt via reused atomic controller accepts statistical failure without release", {
            ctx$graphmode_warmup_child <- function(command,path,repository,timeout,log_file=NULL) {
                assert(command=="diagnostic-worker" && timeout==1200)
                ctx$graphmode_warmup_diagnostic_worker(record,repository,TRUE)
                list(status=0L,output="fixed diagnostic child")
            }
            ctx$graphmode_warmup_execute(record,file.path(folder,"registration.rds"),graphmode_root,TRUE,TRUE)
            assert(ctx$graphmode_warmup_diagnostic_acceptance(record,graphmode_root))
            receipt <- readRDS(file.path(folder,"diagnostic-acceptance.rds"))
            assert(receipt$schema=="graphmode-validation-diagnostic-acceptance-v1" && !receipt$auto_continue)
            fails(ctx$graphmode_warmup_execute(record,file.path(folder,"registration.rds"),graphmode_root,TRUE,TRUE),"Already attempted")
        })
    })
    diagnostic_case <- function(name,status=0L) {
        folder <- file.path(fixture,name);dir.create(folder)
        r <- graphmode_validation_record(graphmode_root,folder,identity,runtime,evidence)
        c <- graphmode_validation_context(r);engine <- c$graphmode_warmup_engine;calls <- 0L
        c$graphmode_warmup_engine <- function(record=NULL) {
            api <- engine(record);api$graphmode_dev_run_source_guard <- function(...) TRUE;api
        }
        path <- file.path(folder,"registration.rds");graphmode_save_new(r,path);dir.create(r$output_dir)
        graphmode_save_new(list(record=r,authorized=TRUE),file.path(r$output_dir,"launch.rds"))
        report <- list(schema=graphmode_validation_version,registration_signature=r$signature,
            phase_A_evidence=r$phase_A,fixed_scale=r$phase_A$candidate_scale,
            auto_continue=FALSE,formal_authorized=FALSE,validity=list(valid=FALSE,failures="fixed statistical flag"))
        c$graphmode_warmup_child <- function(command,path,repository,timeout,log_file=NULL) {
            calls <<- calls+1L;assert(command=="diagnostic-worker" && timeout==1200)
            graphmode_save_new(report,file.path(folder,"diagnostic-report.rds"))
            list(status=status,output="fixed B child")
        }
        list(ctx=c,record=r,folder=folder,run=function() c$graphmode_warmup_execute(r,path,graphmode_root,TRUE,TRUE),
            check=function() c$graphmode_warmup_diagnostic_acceptance(r,graphmode_root),calls=function() calls)
    }
    test("B timed-out child with saved report is not accepted and cannot retry", {
        d <- diagnostic_case("timeout",124L);fails(d$run(),"timed out")
        assert(file.exists(file.path(d$folder,"diagnostic-report.rds")) && !file.exists(file.path(d$folder,"diagnostic-acceptance.rds")))
        fails(d$check());fails(d$run(),"Already attempted");assert(d$calls()==1L)
    })
    test("B pending receipt crossing 20GiB retains failure and does not publish", {
        d <- diagnostic_case("quota")
        d$ctx$graphmode_warmup_disk <- function(...) c(free_bytes=50*1024^3,
            stage_bytes=20*1024^3+if(file.exists(file.path(d$folder,"diagnostic-acceptance-pending.rds"))) 1 else -1)
        fails(d$run(),"storage budget exhausted")
        assert(file.exists(file.path(d$folder,"diagnostic-acceptance-pending.rds")) &&
            !file.exists(file.path(d$folder,"diagnostic-acceptance.rds")))
        fails(d$check());fails(d$run(),"Already attempted")
    })
    test("B request-write quota failure never starts the child", {
        d <- diagnostic_case("request-quota")
        d$ctx$graphmode_warmup_disk <- function(...) c(free_bytes=50*1024^3,
            stage_bytes=if(file.exists(file.path(d$folder,"diagnostic-request.rds"))) 21*1024^3 else 0)
        fails(d$run(),"storage budget exhausted");assert(d$calls()==0L)
        assert(file.exists(file.path(d$folder,"diagnostic-controller-failure.rds")))
    })
    test("B atomic publication remains final I/O; a failed statistical gate is retained", {
        d <- diagnostic_case("final-io")
        d$ctx$graphmode_warmup_disk <- function(directory) {
            assert(!file.exists(file.path(directory,"diagnostic-acceptance.rds")))
            graphmode_warmup_disk(directory)
        }
        d$run();d$ctx$graphmode_warmup_disk <- graphmode_warmup_disk
        assert(d$check() && !readRDS(file.path(d$folder,"diagnostic-report.rds"))$validity$valid)
    })
    test("B changed report scale, failure marker or receipt never passes final consumption", {
        for(case_name in c("scale","receipt","failure")) {
            d <- diagnostic_case(paste0("tamper-",case_name));d$run();assert(d$check())
            if(case_name=="failure") graphmode_save_new(list(error="fixed failure"),file.path(d$folder,"diagnostic-failure.rds")) else {
                file <- file.path(d$folder,if(case_name=="scale") "diagnostic-report.rds" else "diagnostic-acceptance.rds")
                value <- readRDS(file);assert(file.rename(file,paste0(file,".original")))
                if(case_name=="scale") value$fixed_scale <- 1 else value$postflight_passed <- FALSE
                graphmode_save_new(value,file)
            }
            fails(d$check());fails(d$run(),"Already attempted")
        }
    })
    test("unfrozen B identity cannot launch or create a registration tree", {
        uncommitted <- identity;uncommitted$committed <- FALSE
        folder <- file.path(fixture,"unregistered")
        r <- graphmode_validation_record(graphmode_root,folder,uncommitted,runtime,evidence)
        patched(list(graphmode_validation_identity=function(...) uncommitted), {
            c <- graphmode_validation_context(r)
            fails(c$graphmode_warmup_engine(r)$graphmode_dev_run_source_guard(r,graphmode_root),"unfrozen")
        })
        assert(!dir.exists(folder))
    })
    test("no scientific RNG state/kind changes", {
        assert(identical(kind,RNGkind()) && identical(rng,
            if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL))
    })
    cat("PASS:",checks,"fixed-input/mocked-process B groups; fixture:",fixture,"\n")
})
