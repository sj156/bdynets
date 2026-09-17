# D-051 fixed-input/mock-process checks only. No scientific draws/subprocesses.
if(!exists("graphmode_root",inherits=FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent=globalenv())
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),
        "graphmode_dev.R","graphmode_dev_run.R","graphmode_warmup.R","graphmode_validation.R",
        "graphmode_gate_refresh.R","graphmode_refresh_run.R")) sys.source(file.path(graphmode_root,"R",f),kernel)
    test_env <- environment();parent.env(test_env) <- kernel
    rng <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
    kind <- RNGkind();checks <- 0L
    assert <- function(x) if(!isTRUE(x)) stop("D-051 assertion: ",paste(deparse(substitute(x)),collapse=" "),call.=FALSE)
    fails <- function(code,pattern=NULL) {
        e <- suppressWarnings(tryCatch({force(code);NULL},error=function(e)e));assert(inherits(e,"error"))
        if(!is.null(pattern)) assert(grepl(pattern,conditionMessage(e),fixed=TRUE))
        invisible(e)
    }
    test <- function(name,code) {force(code);checks <<- checks+1L;cat("OK",checks,name,"\n");flush.console()}
    patched <- function(bindings,code) {
        existing <- intersect(names(bindings),ls(kernel,all.names=TRUE));old <- mget(existing,kernel)
        on.exit({list2env(old,kernel);added <- setdiff(names(bindings),existing)
            if(length(added)) rm(list=added,envir=kernel)},add=TRUE)
        list2env(bindings,kernel);force(code)
    }
    fixture <- tempfile("graphmode-refresh-run-fixed-",tmpdir="/private/tmp");dir.create(fixture);fixture <- normalizePath(fixture)
    # Keep all mock artifacts in this newly created test-owned temporary tree.
    evidence <- list(directory=file.path(fixture,"A"),
        registration_signature="f248b516d6de752aa7e8b259d26023911256bf85e181001a13dd7a9aa18aefff",
        source_commit="b698fd83ef70e60e3b922229f88ee26464149655",
        source_sha256=setNames(rep(strrep("a",64),47),paste0("source",1:47)),
        file_sha256=setNames(rep(strrep("b",64),5),c("registration.rds","diagnostic-request.rds",
            "diagnostic-execution.rds","diagnostic-report.rds","diagnostic-acceptance.rds")),
        candidate_scale=2.26889626877594,reviewed=TRUE)
    identity <- graphmode_refresh_run_identity(graphmode_root)
    runtime <- graphmode4_pilot_runtime()
    r <- graphmode_refresh_run_record(graphmode_root,file.path(fixture,"draft"),identity,runtime,evidence)
    s <- r$spec
    test("proposed paired scope, lengths, fixed policy and hard budgets", {
        assert(s$iterations==1200L && s$warmup==600L && s$thin==1L && s$checkpoint_every==300L)
        assert(identical(s$inner_steps,c(1L,4L)) && is.null(s$policy) && s$rho==4L && s$workers==1L)
        assert(s$total_seconds+s$diagnostic_seconds==12600 && s$sweep_budget_seconds<s$worker_seconds)
        assert(s$reserve_seconds>s$worker_seconds && 8*s$reserve_seconds<s$total_seconds)
        assert(identical(s$candidate_scale,evidence$candidate_scale))
        assert(identical(s$panel_spec$thresholds,graphmode_validation_spec(evidence$candidate_scale)$panel_spec$thresholds))
        assert(length(s$seeds)==28L && !anyDuplicated(s$seeds))
        assert(!length(intersect(s$seeds,graphmode_warmup_spec()$seeds)))
        assert(!length(intersect(s$seeds,graphmode_validation_spec(evidence$candidate_scale)$seeds)))
        assert(identical(s$jobs$m,c(1L,4L,4L,1L,1L,4L,4L,1L)))
    })
    test("old55 and audited D050 immutable; loaded closure and unfrozen refusal", {
        assert(identity$audited_unchanged && graphmode_dev_run_loaded(graphmode_root,identity))
        assert(length(identity$sha256)>55L)
        bad_identity <- identity;bad_identity$committed <- FALSE
        unfrozen <- graphmode_refresh_run_record(graphmode_root,r$directory,bad_identity,runtime,evidence)
        fails(graphmode_refresh_run_guard(unfrozen,graphmode_root))
    })
    test("explicit authority required before side effects", {
        fails(graphmode_refresh_run_prepare(NULL,NULL,NULL),"Approve")
        fails(graphmode_refresh_run_execute(NULL,NULL,NULL),"authorization")
        fails(graphmode_refresh_run_worker(NULL,NULL),"authorization")
        fails(graphmode_refresh_run_launch(NULL,NULL,NULL),"authorization")
        fails(graphmode_refresh_run_batch(NULL,NULL),"authorization")
        fails(graphmode_refresh_run_diagnostic_worker(NULL,NULL),"authorization")
    })
    test("legacy registrations, changed scale/budget/m and job signature rejected", {
        fails(graphmode_refresh_run_validate(graphmode_validation_record(graphmode_root,fixture,identity,runtime,evidence)))
        for(field in c("iterations","warmup","rho","candidate_scale","total_seconds")) {
            bad <- r;bad$spec[[field]] <- bad$spec[[field]]+1;fails(graphmode_refresh_run_validate(bad))
        }
        bad <- r;bad$formal_authorized <- TRUE;fails(graphmode_refresh_run_validate(bad))
        fails(graphmode_refresh_run_job(r,1L,2L))
        assert(graphmode_refresh_run_job(r,1L,1L)$seed!=graphmode_refresh_run_job(r,1L,4L)$seed)
    })
    test("child entry quoting and hard timeout preserved without a subprocess", {
        captured <- NULL
        patched(list(system2=function(command,args,stdout,stderr,timeout,...) {
            captured <<- list(args=args,timeout=timeout);structure("mock timeout",status=124L)
        }), {
            out <- graphmode_refresh_run_child("worker",file.path(fixture,"input with spaces.rds"),graphmode_root,1320)
            assert(out$status==124L && captured$timeout==1320)
            assert(any(grepl("scripts/graphmode-refresh-run.R",captured$args,fixed=TRUE)))
            assert(any(grepl("input with spaces.rds",captured$args,fixed=TRUE)))
        })
        fails(graphmode_refresh_run_child("resume","x",graphmode_root,1320))
    })
    # Actual D050+new observational wrapper using fixed RNG interface returns.
    tiny <- graphmode_config(matrix(1:12,4,3),cbind(1,c(-1,0,1)),c(0,0),diag(2),
        "graphMoDE-W",K=3L,G=diag(2),W=diag(.02,2),Phi=diag(4),guidance_proposal_sd=.4,rho=4L)
    initial <- graphmode_initial_state(tiny,c(1L,2L,1L,3L),array(0,c(3,3,2)),v=c(-.3,.1,.5))
    tape <- function(code) {
        calls <- c(normal=0L,uniform=0L,pg=0L,gamma=0L)
        patched(list(graphmode_normal=function(n) {i <- calls["normal"]+seq_len(n);calls["normal"] <<- calls["normal"]+n;.12*sin(i/5)},
            graphmode_uniform=function(n) {i <- calls["uniform"]+seq_len(n);calls["uniform"] <<- calls["uniform"]+n;.25+.5*(i%%13)/13},
            graphmode_pg=function(b,z) {calls["pg"] <<- calls["pg"]+length(b);rep(.25,length(b))},
            graphmode_gamma=function(shape,rate) {calls["gamma"] <<- calls["gamma"]+length(shape);shape/rate}), {
                value <- force(code);list(value=value,calls=calls)
            })
    }
    for(m in c(1L,4L)) test(paste("real new step routes audited m",m,"without changing state/RNG calls"), {
        policy <- graphmode_gate_refresh_policy(m,.4)
        direct <- tape(graphmode_gate_refresh_sweep(initial,tiny,policy,TRUE))
        wrapped <- tape(graphmode_refresh_run_step(initial,tiny,policy))
        assert(identical(direct$value$transition$state,wrapped$value$state))
        assert(identical(direct$calls,wrapped$calls))
        d <- wrapped$value$diagnostic
        assert(d$gate$counts$total_proposals==m*3 && is.null(d$guidance_accept))
        assert(identical(d$allocation$combined$observed_node_moves,d$events[["node_moves"]]))
        assert(d$total_seconds>=d$transition_seconds && d$iteration==1L)
    })
    # Real immutable generator with fixed component returns; never generate data.
    active <- 0L;normal_calls <- 0L;seen <- integer();captured <- list()
    hooks <- list(graphmode_pilot_seeded=function(seed,code) {
        seen <<- c(seen,seed)
        if(seed %in% s$seeds[1:4]) {
            if(seed==s$seeds[[1]]) return(array(sin(seq_len(5*168*3)/17),c(5L,168L,3L)))
            if(seed==s$seeds[[2]]) return(5:1)
            if(seed==s$seeds[[3]]) return(121:1)
            if(seed==s$seeds[[4]]) return(matrix(2,121,168))
        }
        previous <- active;active <<- seed;on.exit(active <<- previous);force(code)
    },sample.int=function(n,size=n,replace=FALSE) {assert(!replace);((seq_len(n)+active%%n-1L)%%n+1L)[seq_len(size)]},
        graphmode_normal=function(n) {normal_calls <<- normal_calls+1L;rep(.01+active%%10*.003+normal_calls%%10*.0001,n)},
        graphmode_uniform=function(n) rep(seq(.2,.8,length.out=10),length.out=n))
    blind <- patched(hooks,graphmode_refresh_run_generate(r,function(x,n) captured[[n]] <<- x))
    test("one new fixed panel and four complete dispersed starts shared by both arms", {
        assert(identical(seen[1:4],unname(s$seeds[1:4])))
        assert(identical(names(captured),c("generation.rds",sprintf("INITIAL-%02d.rds",1:4),"blinded-inputs.rds")))
        assert(graphmode_warmup_starts_check(blind$starts,blind$fit,s))
        assert(identical(blind$fit$core$guidance_proposal_sd,s$candidate_scale) && blind$fit$core$rho==4L)
        assert(blind$schema==graphmode_refresh_run_version)
        assert(identical(vapply(blind$starts,function(x) length(unique(x$state$Z)),integer(1)),c(1L,3L,7L,10L)))
    })
    # Tiny control-flow fixture; no edits to the real 1200/600 specification.
    original_spec <- graphmode_refresh_run_spec
    short_spec <- function(scale) {z <- original_spec(scale);z$iterations <- 8L;z$warmup <- 4L;z$checkpoint_every <- 4L;z}
    disk_ok <- function(...) c(free_bytes=100*1024^3,stage_bytes=0)
    source_ok <- function(record,repository) {graphmode_refresh_run_validate(record);invisible(TRUE)}
    fixed_sweep <- function(state,config,policy,cache_ffbs) {
        assert(cache_ffbs);before <- state$Z;K <- config$K;TT <- nrow(config$Fmat)
        accepted <- rep(state$iteration%%2L==0L,K)
        records <- lapply(seq_len(policy$inner_steps),function(j) list(inner_step=j,accepted=accepted,
            proposals=rep(1L,K),ess_evaluations=1L,x_jump_squared=.01,
            logit_jump_squared=ifelse(accepted,.01,0),weight_jump_squared=ifelse(accepted,.001,0),
            ess_seconds=.001,guidance_seconds=.002))
        state$v <- state$v+ifelse(accepted,.1*policy$inner_steps,0)
        state$x <- state$x+.0001;state$iteration <- state$iteration+1L
        es <- os <- vector("list",K)
        for(k in seq_len(K)) {
            empty <- !any(before==k)
            es[[k]] <- list(empty=empty,accepted=if(empty) NA else TRUE,log_acceptance=if(empty) NA_real_ else 0,
                movement=if(empty) 0 else .01,seconds=.01,factor_residual=0,root_residual=0,
                root_reciprocal_condition=if(empty) NA_real_ else 1)
            os[[k]] <- list(expert=k,observation=if(empty) NULL else list(accepted=TRUE,log_acceptance=0,
                accepted_information_movement=.01,correction_by_time=numeric(TT),log_ratio=0,
                proposed_information_movement=.01),observation_seconds=0)
        }
        list(transition=list(state=state,sizes=tabulate(state$Z,K),events=graphmode_allocation_events(before,state$Z,K),expert_updates=es),
            gate_refresh=list(schema=graphmode_gate_refresh_version,records=records,
                counts=graphmode_gate_refresh_counts(records,policy,K),policy=policy,inner_states_are_retained_draws=FALSE),
            expert_observations=os)
    }
    fixtures <- list();results <- list()
    with_short <- list(graphmode_refresh_run_spec=short_spec,graphmode_refresh_run_guard=source_ok,
        graphmode_warmup_disk=disk_ok,graphmode_gate_refresh_sweep=fixed_sweep,
        graphmode_pilot_seeded=function(seed,code) force(code))
    make_fixture <- function(name,chain=1L,m=4L) {
        dir <- file.path(fixture,name);dir.create(dir)
        rec <- graphmode_refresh_run_record(graphmode_root,dir,identity,runtime,evidence)
        graphmode_save_new(rec,file.path(dir,"registration.rds"));dir.create(rec$output_dir)
        graphmode_save_new(list(record=rec,authorized=TRUE),file.path(rec$output_dir,"launch.rds"))
        for(n in names(captured)) graphmode_save_new(captured[[n]],file.path(rec$output_dir,n))
        graphmode_save_new(list(registration_signature=rec$signature,blinded_signature=blind$signature),
            file.path(rec$output_dir,"inputs-identity.rds"))
        job <- graphmode_refresh_run_job(rec,chain,m)
        path <- file.path(rec$output_dir,paste0(job$name,"-plan.rds"));graphmode_save_new(job,path)
        list(record=rec,job=job,path=path)
    }
    patched(with_short, {
        test("worker fixed transitions retain outer draws only; new checkpoint and counts", {
            f <- make_fixture("worker-success");fixtures$success <- f
            graphmode_refresh_run_worker(f$job,graphmode_root,TRUE)
            result <- readRDS(file.path(f$job$directory,"result.rds"));results$success <- result
            chain <- graphmode_refresh_run_result_check(f$job,result,blind)
            assert(chain$complete && length(chain$draws)==4L)
            assert(identical(vapply(chain$draws,`[[`,integer(1),"iteration"),5:8))
            assert(all(vapply(result$checkpoint$diagnostics,function(d) d$gate$counts$total_proposals==40,logical(1))))
            assert(file.exists(file.path(f$job$directory,"checkpoint-000000004.rds")))
            fails(graphmode_refresh_run_worker(f$job,graphmode_root,TRUE),"already attempted")
        })
        test("missing substeps, altered acceptance, retained indices and numerical failures rejected", {
            f <- fixtures$success;res <- results$success
            bad <- res;bad$checkpoint$diagnostics[[2]]$gate$records <- bad$checkpoint$diagnostics[[2]]$gate$records[-1]
            fails(graphmode_refresh_run_result_check(f$job,bad,blind))
            bad <- res;bad$checkpoint$diagnostics[[1]]$gate$counts$total_proposals <- 10
            fails(graphmode_refresh_run_result_check(f$job,bad,blind))
            bad <- res;bad$checkpoint$diagnostics[[1]]$guidance_accept <- rep(TRUE,10)
            fails(graphmode_refresh_run_result_check(f$job,bad,blind))
            bad <- res;bad$checkpoint$saved[[1]]$iteration <- 1L
            fails(graphmode_refresh_run_result_check(f$job,bad,blind))
            bad <- res;bad$checkpoint$diagnostics[[1]]$expert[[1]]$root_residual <- .01
            fails(graphmode_refresh_run_result_check(f$job,bad,blind))
            bad <- res;bad$checkpoint$diagnostics[[2]]$before_signature <- "changed"
            fails(graphmode_refresh_run_result_check(f$job,bad,blind))
            bad <- res;bad$checkpoint$diagnostics[[2]]$gate$records[[1]]$guidance_seconds <- NA_real_
            fails(graphmode_refresh_run_result_check(f$job,bad,blind))
        })
        test("inner execution error retains last complete state and failure without retries", {
            f <- make_fixture("worker-failure");calls <- 0L
            patched(list(graphmode_gate_refresh_sweep=function(...) {
                calls <<- calls+1L;if(calls==3L) stop("fixed inner failure");fixed_sweep(...)
            }),fails(graphmode_refresh_run_worker(f$job,graphmode_root,TRUE),"fixed inner failure"))
            cp <- readRDS(file.path(f$job$directory,"failure.rds"))
            assert(calls==3L && cp$completed_steps==2L && cp$state$iteration==2L && !file.exists(file.path(f$job$directory,"result.rds")))
        })
        test("canonical /tmp alias accepted, later parent redirection rejected", {
            f <- fixtures$success
            alias <- sub("^/private/tmp/","/tmp/",f$record$directory)
            assert(identical(graphmode4_output_target(alias),f$record$directory))
            target <- file.path(fixture,"link-target");dir.create(target)
            link <- file.path(fixture,"link");assert(file.symlink(target,link))
            canonical <- graphmode4_output_target(file.path(link,"future"))
            assert(identical(canonical,file.path(target,"future")))
            moved <- file.path(fixture,"moved-target");assert(file.rename(target,moved))
            assert(dir.create(target))
            # Even a reconstructed same-spelling root lacks its registration.
            bad <- f$record;bad$directory <- canonical
            fails(graphmode_refresh_run_tree(bad))
        })
        test("success exit and result require parent receipt; consumer rejects absent receipt", {
            f <- fixtures$success
            x <- list(status=0L,output="fixed",elapsed_seconds=1,job_signature=f$job$signature,budget_seconds=f$record$spec$worker_seconds)
            graphmode_save_new(x,file.path(f$job$directory,"execution.rds"))
            expected <- graphmode_refresh_run_job_evidence(f$job,graphmode_root,FALSE)$receipt
            fails(graphmode_refresh_run_job_evidence(f$job,graphmode_root,TRUE))
            graphmode_refresh_run_publish(f$record,expected,file.path(f$job$directory,"acceptance.rds"),function()
                graphmode_refresh_run_job_evidence(f$job,graphmode_root,FALSE))
            assert(identical(graphmode_refresh_run_job_evidence(f$job,graphmode_root)$receipt,expected))
        })
        for(status in c(0L,124L)) test(paste("mock parent launcher status",status,"with existing completed result"), {
            f <- make_fixture(paste0("launch-",status))
            patched(list(graphmode_refresh_run_child=function(command,path,repository,timeout,log_file=NULL) {
                assert(command=="worker" && timeout==1320)
                graphmode_refresh_run_worker(readRDS(path),repository,TRUE)
                list(status=status,output="fixed mock process")
            }), {
                if(status==0L) {
                    graphmode_refresh_run_launch(f$job,f$path,graphmode_root,TRUE)
                    assert(graphmode_refresh_run_job_evidence(f$job,graphmode_root)$receipt$postflight_passed)
                } else {
                    fails(graphmode_refresh_run_launch(f$job,f$path,graphmode_root,TRUE),"timed out")
                    assert(file.exists(file.path(f$job$directory,"result.rds")))
                    assert(file.exists(file.path(f$job$directory,"controller-failure.rds")))
                    assert(!file.exists(file.path(f$job$directory,"acceptance.rds")))
                    fails(graphmode_refresh_run_job_evidence(f$job,graphmode_root))
                }
            })
        })
        test("pending publication fails closed if storage fills after write", {
            f <- fixtures$success;target <- file.path(f$job$directory,"extra-acceptance.rds");calls <- 0L
            patched(list(graphmode_warmup_storage=function(...) {
                calls <<- calls+1L;if(calls>=2L) stop("fixed storage full");invisible(TRUE)
            }),fails(graphmode_refresh_run_publish(f$record,list(ok=TRUE),target,function() TRUE),"storage full"))
            assert(!file.exists(target) && file.exists(sub("[.]rds$","-pending.rds",target)))
        })
        test("postflight path/source failure cannot publish acceptance", {
            f <- fixtures$success;target <- file.path(f$job$directory,"verify-acceptance.rds")
            fails(graphmode_refresh_run_publish(f$record,list(ok=TRUE),target,function() stop("fixed source moved")),"source moved")
            assert(!file.exists(target) && file.exists(sub("[.]rds$","-pending.rds",target)))
        })
        # Full eight-job batch plus parent and diagnostic receipts, mocked children
        # and transitions but real binding, writes, validators and original reducer.
        full_dir <- file.path(fixture,"full-fixed-batch");dir.create(full_dir)
        full <- graphmode_refresh_run_record(graphmode_root,full_dir,identity,runtime,evidence)
        full_path <- file.path(full_dir,"registration.rds");graphmode_save_new(full,full_path)
        job_order <- character();diagnostic_calls <- 0L
        process <- function(command,path,repository,timeout,log_file=NULL) {
            object <- readRDS(path)
            if(command=="worker") {job_order <<- c(job_order,object$name);graphmode_refresh_run_worker(object,repository,TRUE)}
            else if(command=="batch") graphmode_refresh_run_batch(object,repository,TRUE)
            else if(command=="diagnostic-worker") {diagnostic_calls <<- diagnostic_calls+1L;graphmode_refresh_run_diagnostic_worker(object,repository,TRUE)}
            else stop("Unknown mock child")
            list(status=0L,output="fixed-process-no-scientific-draws")
        }
        patched(list(graphmode_refresh_run_child=process,graphmode_refresh_run_generate=function(record,persist) {
            for(n in names(captured)) persist(captured[[n]],n);blind
        }), {
            test("integrated eight mocked workers match starts/inputs and prescribed order", {
                graphmode_refresh_run_execute(full,full_path,graphmode_root,FALSE,TRUE)
                assert(identical(job_order,sprintf("m%d-chain-%02d",full$spec$jobs$m,full$spec$jobs$chain)))
                x <- graphmode_refresh_run_science_evidence(full,graphmode_root)
                assert(length(x$evidence)==8L && diagnostic_calls==0L)
                assert(length(unique(vapply(x$evidence,function(e) e$result$input_signature,character(1))))==1L)
                assert(!file.exists(file.path(full_dir,"diagnostic-report.rds")))
            })
            test("one explicit fixed-input diagnosis separates arms and retains original flags", {
                graphmode_refresh_run_execute(full,full_path,graphmode_root,TRUE,TRUE)
                receipt <- graphmode_refresh_run_diagnostic_evidence(full,graphmode_root)
                report <- readRDS(file.path(full_dir,"diagnostic-report.rds"))
                assert(receipt$postflight_passed && diagnostic_calls==1L && !report$automatic_selection)
                assert(identical(names(report$arms),c("m1","m4")))
                for(a in report$arms) {
                    assert(!a$validity$valid && nrow(a$validity$scalars)==37L && nrow(a$validity$psm_rms)==6L)
                    assert(identical(a$efficiency$bulk_ess_per_worker_second,a$efficiency$bulk_ess/sum(a$worker_seconds)))
                    assert(!any(a$efficiency$precision_certified))
                }
                p1 <- report$arms$m1$mechanism[[1]]$guidance_proposals
                p4 <- report$arms$m4$mechanism[[1]]$guidance_proposals
                assert(identical(p4,4*p1))
            })
            test("science and diagnostic requests are once-only; no resume", {
                fails(graphmode_refresh_run_execute(full,full_path,graphmode_root,FALSE,TRUE),"already attempted")
                fails(graphmode_refresh_run_execute(full,full_path,graphmode_root,TRUE,TRUE),"already attempted")
                assert(diagnostic_calls==1L && length(job_order)==8L)
            })
            test("diagnostic postflight rejects dropped scalars, waived flags and incorrect ESS cost", {
                sci <- graphmode_refresh_run_science_evidence(full,graphmode_root)
                report <- readRDS(file.path(full_dir,"diagnostic-report.rds"))
                assert(graphmode_refresh_run_report_check(report,full,sci))
                bad <- report;bad$arms$m4$validity$scalars <- bad$arms$m4$validity$scalars[-1,]
                fails(graphmode_refresh_run_report_check(bad,full,sci))
                bad <- report;bad$arms$m1$validity$valid <- TRUE
                fails(graphmode_refresh_run_report_check(bad,full,sci))
                bad <- report;bad$arms$m1$efficiency$bulk_ess_per_worker_second <- 1
                fails(graphmode_refresh_run_report_check(bad,full,sci))
                bad <- report;bad$arms$m4$mechanism[[1]]$guidance_proposals <- rep(4,10)
                fails(graphmode_refresh_run_report_check(bad,full,sci))
            })
        })
        test("late failure evidence invalidates otherwise complete scientific/diagnostic receipts", {
            graphmode_save_new(list(error="fixed late failure"),file.path(full_dir,"science-failure.rds"))
            fails(graphmode_refresh_run_science_evidence(full,graphmode_root),"Failed comparison")
            fails(graphmode_refresh_run_diagnostic_evidence(full,graphmode_root),"Failed comparison")
        })
    })
    test("original files, RNG state and RNG kind unchanged after all fixed checks", {
        assert(identical(graphmode_refresh_run_identity(graphmode_root),identity))
        assert(identical(RNGkind(),kind))
        after <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
        assert(identical(rng,after))
    })
    cat("PASS",checks,"fixed-input/mock-process groups; no scientific draws, actual child launch or old-result rerun.\n")
    cat("Retained fixed artifacts:",fixture,"\n")
})
