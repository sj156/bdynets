# D-056 fixed-input/mock-process checks. No real simulation/PG/subprocess.
if(!exists("graphmode_root",inherits=FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent=globalenv())
    for(f in c("gmde-helpers.R","gmde-state-update.R",
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$")),
        "graphmode_dev.R","graphmode_dev_run.R","graphmode_warmup.R","graphmode_validation.R",
        "graphmode_gate_refresh.R","graphmode_refresh_run.R","graphmode_expert_blocks.R","graphmode_blocks_run.R")) sys.source(file.path(graphmode_root,"R",f),kernel)
    test_env <- environment();parent.env(test_env) <- kernel
    seed_before <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
    kind <- RNGkind();checks <- 0L
    assert <- function(x) if(!isTRUE(x)) stop("D-056 assertion: ",paste(deparse(substitute(x)),collapse=" "),call.=FALSE)
    fails <- function(code,pattern=NULL) {
        e <- suppressWarnings(tryCatch({force(code);NULL},error=function(e) e));assert(inherits(e,"error"))
        if(!is.null(pattern)) assert(grepl(pattern,conditionMessage(e),fixed=TRUE))
        invisible(e)
    }
    patched <- function(bindings,code) {
        old <- mget(names(bindings),kernel);on.exit(list2env(old,kernel),add=TRUE)
        list2env(bindings,kernel);force(code)
    }
    test <- function(name,code) {
        patched(list(graphmode_normal=function(...) stop("Unexpected random normal"),
            graphmode_uniform=function(...) stop("Unexpected random uniform"),graphmode_pg=function(...) stop("Unexpected real PG"),
            graphmode_gamma=function(...) stop("Unexpected random gamma")),force(code))
        checks <<- checks+1L;cat("OK",checks,name,"\n");flush.console()
    }
    fixture <- tempfile("graphmode-blocks-run-fixed-",tmpdir="/private/tmp");dir.create(fixture);fixture <- normalizePath(fixture)
    evidence <- list(directory=file.path(fixture,"A"),
        registration_signature="f248b516d6de752aa7e8b259d26023911256bf85e181001a13dd7a9aa18aefff",
        source_commit="b698fd83ef70e60e3b922229f88ee26464149655",
        source_sha256=setNames(rep(strrep("a",64),47),paste0("source",1:47)),
        file_sha256=setNames(rep(strrep("b",64),5),c("registration.rds","diagnostic-request.rds","diagnostic-execution.rds","diagnostic-report.rds","diagnostic-acceptance.rds")),
        candidate_scale=2.2688962687759413,reviewed=TRUE)
    identity <- graphmode_blocks_run_identity(graphmode_root);runtime <- graphmode4_pilot_runtime()
    loaded_before_tapes <- graphmode_dev_run_loaded(graphmode_root,identity)
    api <- graphmode_blocks_run_context()
    r <- api$graphmode_refresh_run_record(graphmode_root,file.path(fixture,"draft"),identity,runtime,evidence);s <- r$spec
    test("scope, balanced order, fixed policies, independent seed candidates and budget", {
        assert(identical(s$arms,c("full","block42")) && s$gate_inner_steps==4L && is.null(s$inner_steps))
        assert(s$iterations==1200L && s$warmup==600L && s$thin==1L && s$checkpoint_every==300L)
        assert(s$block_policies$full$block_length==168L && s$block_policies$block42$block_length==42L)
        assert(s$block_policies$block42$offset_rule=="uniform" && is.null(s$policy) && s$rho==4L)
        assert(s$total_seconds+s$diagnostic_seconds==14400 && 8*s$reserve_seconds<s$total_seconds)
        assert(s$sweep_budget_seconds<s$worker_seconds && s$worker_seconds<s$reserve_seconds && s$workers==1L)
        assert(length(s$seeds)==28L && !anyDuplicated(s$seeds))
        assert(!length(intersect(s$seeds,graphmode_refresh_run_spec(evidence$candidate_scale)$seeds)))
        assert(identical(s$panel_spec$thresholds,graphmode_validation_spec(evidence$candidate_scale)$panel_spec$thresholds))
        assert(nrow(graphmode_expert_blocks_layout(168,42,0))==4L && nrow(graphmode_expert_blocks_layout(168,42,1))==5L)
    })
    test("audited68+D055 immutable; complete new definitions loaded; unfrozen refusal", {
        assert(identity$audited_unchanged && loaded_before_tapes)
        not_frozen <- identity;not_frozen$committed <- FALSE
        bad <- api$graphmode_refresh_run_record(graphmode_root,r$directory,not_frozen,runtime,evidence)
        fails(api$graphmode_refresh_run_guard(bad,graphmode_root),"unfrozen")
        assert(identical(environment(graphmode_refresh_run_worker),kernel))
    })
    test("approval tokens guard all write/launch paths", {
        fails(api$graphmode_refresh_run_prepare(NULL,NULL,NULL),"Approve")
        fails(api$graphmode_refresh_run_execute(NULL,NULL,NULL),"authorization")
        fails(api$graphmode_refresh_run_worker(NULL,NULL),"authorization")
        fails(api$graphmode_refresh_run_launch(NULL,NULL,NULL),"authorization")
        fails(api$graphmode_refresh_run_batch(NULL,NULL),"authorization")
        fails(api$graphmode_refresh_run_diagnostic_worker(NULL,NULL),"authorization")
    })
    test("legacy/changed registration and arm rejected", {
        fails(api$graphmode_refresh_run_validate(graphmode_refresh_run_record(graphmode_root,fixture,identity,runtime,evidence)))
        for(field in c("iterations","warmup","rho","candidate_scale","total_seconds")) {
            b <- r;b$spec[[field]] <- b$spec[[field]]+1;fails(api$graphmode_refresh_run_validate(b))
        }
        b <- r;b$spec$block_policies$block42$block_length <- 21L;fails(api$graphmode_refresh_run_validate(b))
        fails(api$graphmode_refresh_run_job(r,1L,"m4"))
        assert(api$graphmode_refresh_run_job(r,1L,"full")$seed!=api$graphmode_refresh_run_job(r,1L,"block42")$seed)
    })
    test("quoted new entry and hard timeout without starting a process", {
        captured <- NULL
        # The inherited system2 lookup occurs in the immutable parent environment.
        assign("system2",function(command,args,stdout,stderr,timeout,...) {
            captured <<- list(args=args,timeout=timeout);structure("fixed timeout",status=124L)
        },kernel)
        out <- api$graphmode_refresh_run_child("worker",file.path(fixture,"input with spaces.rds"),graphmode_root,1500)
        rm("system2",envir=kernel)
        assert(out$status==124L && captured$timeout==1500)
        assert(any(grepl("scripts/graphmode-blocks-run.R",captured$args,fixed=TRUE)))
        assert(any(grepl("input with spaces.rds",captured$args,fixed=TRUE)))
    })
    tiny <- graphmode_config(matrix(1:15,3,5),cbind(1,seq(-.5,.5,length.out=5)),c(0,0),diag(2),
        "graphMoDE-W",K=3L,G=diag(2),W=diag(.02,2),Phi=diag(3),guidance_proposal_sd=.4,rho=4L)
    initial <- graphmode_initial_state(tiny,c(1L,1L,2L),array(0,c(3,5,2)),v=c(-.3,.1,.5))
    tape <- function(code) {
        used <- c(normal=0L,uniform=0L,pg=0L)
        patched(list(graphmode_normal=function(n) {i <- used["normal"]+seq_len(n);used["normal"] <<- used["normal"]+n;.12*sin(i/5)},
            graphmode_uniform=function(n) {i <- used["uniform"]+seq_len(n);used["uniform"] <<- used["uniform"]+n;.25+.5*(i%%13)/13},
            graphmode_pg=function(b,z) {used["pg"] <<- used["pg"]+length(b);rep(.3,length(b))}), {
                value <- force(code);list(value=value,used=used)
            })
    }
    for(width in c(5L,2L)) test(paste("real audited step plus passive wrapper width",width), {
        pol <- list(gate=graphmode_gate_refresh_policy(4L,.4),block=graphmode_expert_blocks_policy(width,"uniform"))
        a <- tape(graphmode_expert_blocks_sweep(initial,tiny,pol$gate,pol$block,TRUE))
        b <- tape(api$graphmode_refresh_run_step(initial,tiny,pol))
        assert(identical(a$used,b$used) && identical(a$value$transition$state,b$value$state))
        for(k in 1:3) graphmode_blocks_run_expert_check(b$value$diagnostic$expert[[k]],matrix(initial$theta[k,,],5,2),
            matrix(b$value$state$theta[k,,],5,2),tiny$Y[initial$Z==k,,drop=FALSE],tiny,pol$block,b$value$diagnostic$block_offset,s$panel_spec$thresholds)
    })
    for(width in c(5L,2L)) test(paste("actual fixed-tape outer states pass complete result validation width",width), {
        pol <- list(gate=graphmode_gate_refresh_policy(4L,.4),block=graphmode_expert_blocks_policy(width,"uniform"))
        small <- r;small$spec$iterations <- 3L;small$spec$warmup <- 1L
        job <- list(signature="fixed-tape-job",record=small,policy=pol,chain=1L,seed=1L)
        input <- list(signature="fixed-tape-input",fit=list(core=tiny,signature=graphmode_digest(tiny)),starts=list(list(state=initial)))
        tape({
            state <- initial;ds <- saved <- list()
            for(i in 1:3) {
                a <- api$graphmode_refresh_run_step(state,tiny,pol);state <- a$state;ds[[i]] <- a$diagnostic
                if(i>1) saved[[i-1L]] <- state[c("iteration","Z","theta","sigma2","v","pi")]
            }
            result <- list(schema=graphmode_blocks_run_version,job_signature=job$signature,input_signature=input$signature,formal_authorized=FALSE,
                checkpoint=list(schema=graphmode_blocks_run_version,policy=pol,status="completed-not-convergence-certified",
                    plan_signature=job$signature,source_identity=identity,completed_steps=3L,state=state,diagnostics=ds,saved=saved,
                    elapsed_seconds=1,resume_supported=FALSE,error=NULL))
            chain <- api$graphmode_refresh_run_result_check(job,result,input)
            assert(chain$complete && length(chain$draws)==2L && chain$numerical_guards_passed)
        })
    })
    # Actual immutable generator with preset component values, not new data.
    captured <- list();active <- 0L;nc <- 0L;seen <- integer()
    hooks <- list(graphmode_pilot_seeded=function(seed,code) {
        seen <<- c(seen,seed)
        if(seed %in% s$seeds[1:4]) {
            if(seed==s$seeds[1]) return(array(sin(seq_len(5*168*3)/17),c(5L,168L,3L)))
            if(seed==s$seeds[2]) return(5:1)
            if(seed==s$seeds[3]) return(121:1)
            if(seed==s$seeds[4]) return(matrix(2,121,168))
        }
        previous <- active;active <<- seed;on.exit(active <<- previous);force(code)
    },graphmode_normal=function(n) {nc <<- nc+1L;rep(.01+active%%10*.003+nc%%10*.0001,n)},
        graphmode_uniform=function(n) rep(seq(.2,.8,length.out=10),length.out=n))
    assign("sample.int",function(n,size=n,replace=FALSE) {assert(!replace);((seq_len(n)+active%%n-1L)%%n+1L)[seq_len(size)]},kernel)
    blind <- patched(hooks,api$graphmode_refresh_run_generate(r,function(x,n) captured[[n]] <<- x))
    rm("sample.int",envir=kernel)
    test("one new fixed panel and full dispersed starts paired between arms", {
        assert(identical(seen[1:4],unname(s$seeds[1:4])))
        assert(identical(names(captured),c("generation.rds",sprintf("INITIAL-%02d.rds",1:4),"blinded-inputs.rds")))
        assert(blind$schema==graphmode_blocks_run_version && graphmode_warmup_starts_check(blind$starts,blind$fit,s))
        assert(identical(vapply(blind$starts,function(x) length(unique(x$state$Z)),integer(1)),c(1L,3L,7L,10L)))
        assert(identical(blind$fit$core$guidance_proposal_sd,s$candidate_scale))
    })
    # Tiny process fixtures still use n121/T168/K10 and real boundary validators.
    old_spec <- graphmode_blocks_run_spec
    short_spec <- function(scale) {z <- old_spec(scale);z$iterations <- 8L;z$warmup <- 4L;z$checkpoint_every <- 4L;z}
    fixed_step <- function(state,config,policy) {
        before <- state;K <- config$K;TT <- nrow(config$Fmat)
        offset <- if(policy$block$block_length==TT) 0L else as.integer((state$iteration*7L)%%policy$block$block_length)
        layout <- graphmode_expert_blocks_layout(TT,policy$block$block_length,offset)
        accept <- state$iteration%%3L==0L;es <- vector("list",K)
        for(k in 1:K) {
            N <- sum(state$Z==k);empty <- N==0L;S <- colSums(config$Y[state$Z==k,,drop=FALSE]);rs <- gmde_make_nb_r(S,config$rho)
            if(empty) state$theta[k,,] <- state$theta[k,,]+.001
            rows <- if(empty) list() else lapply(seq_len(nrow(layout)),function(j) {
                at <- layout$start[j]:layout$end[j]
                left <- if(min(at)>1) state$theta[k,min(at)-1,] else NULL
                right <- if(max(at)<TT) state$theta[k,max(at)+1,] else NULL
                old <- matrix(state$theta[k,at,],length(at),ncol(config$Fmat))
                if(accept) state$theta[k,at,] <<- state$theta[k,at,]+.001
                next_path <- matrix(state$theta[k,at,],length(at),ncol(config$Fmat))
                movement <- sum(pmax(S[at],1)*(rowSums(next_path*config$Fmat[at,,drop=FALSE])-rowSums(old*config$Fmat[at,,drop=FALSE]))^2)
                correction <- rep(if(accept) 0 else -.1,length(at));la <- min(0,sum(correction))
                list(block=j,start=layout$start[j],end=layout$end[j],left=left,right=right,accepted=accept,log_acceptance=la,r=rs[at],
                    observation=list(schema=graphmode_expert_blocks_version,occupied_series=N,accepted=accept,log_acceptance=la,
                        log_ratio=sum(correction),correction_by_time=correction,proposed_information_movement=1,accepted_information_movement=movement),
                    kernel_seconds=.001,observation_seconds=.0001,factor_residual=0,root_residual=0,root_reciprocal_condition=1)
            })
            ac <- if(empty) logical() else rep(accept,length(rows))
            es[[k]] <- list(schema=graphmode_expert_blocks_version,empty=empty,r=if(empty) numeric() else rs,
                movement=sum(vapply(rows,function(b) b$observation$accepted_information_movement,numeric(1))),seconds=.01,
                timing_scope="complete expert update including observations; do not add observation_seconds again",
                factor_residual=0,root_residual=0,root_reciprocal_condition=if(empty) NA_real_ else 1,
                counts=list(proposals=length(ac),accepted=sum(ac),acceptance=if(length(ac)) mean(ac) else NA_real_,prior_refreshes=as.integer(empty)),blocks=rows)
        }
        records <- lapply(seq_len(policy$gate$inner_steps),function(j) list(inner_step=j,accepted=rep(accept,K),
            proposals=rep(1L,K),ess_evaluations=1L,x_jump_squared=.01,logit_jump_squared=rep(if(accept) .01 else 0,K),
            weight_jump_squared=rep(if(accept) .001 else 0,K),ess_seconds=.001,guidance_seconds=.002))
        state$x <- state$x+.0001;state$v <- state$v+if(accept) .1 else 0;state$iteration <- state$iteration+1L
        d <- list(iteration=state$iteration,policy=policy,block_offset=offset,before_signature=graphmode_digest(before),
            after_signature=graphmode_digest(state),before_occupancy=tabulate(before$Z,K),x=state$x,v=state$v,Z=state$Z,theta=state$theta,
            theta_signature=graphmode_digest(state$theta),sizes=tabulate(state$Z,K),events=graphmode_allocation_events(before$Z,state$Z,K),
            expert=es,gate=list(schema=graphmode_gate_refresh_version,records=records,counts=graphmode_gate_refresh_counts(records,policy$gate,K),
                policy=policy$gate,inner_states_are_retained_draws=FALSE),allocation=graphmode_dev_allocation_parts(state,config,before$Z),
            transition_seconds=.2,total_seconds=.3,outer_x_jump_squared=sum((state$x-before$x)^2),outer_v_jump_squared=(state$v-before$v)^2)
        list(state=state,diagnostic=d)
    }
    patched(list(graphmode_blocks_run_spec=short_spec,graphmode_blocks_run_step=fixed_step,
        graphmode_pilot_seeded=function(seed,code) force(code),graphmode_warmup_disk=function(...) c(free_bytes=100*1024^3,stage_bytes=0)), {
        api <- graphmode_blocks_run_context()
        api$graphmode_refresh_run_guard <- function(record,repository) {api$graphmode_refresh_run_validate(record);invisible(TRUE)}
        make <- function(name,chain=1L,arm="block42") {
            directory <- file.path(fixture,name);dir.create(directory)
            rec <- api$graphmode_refresh_run_record(graphmode_root,directory,identity,runtime,evidence)
            graphmode_save_new(rec,file.path(directory,"registration.rds"));dir.create(rec$output_dir)
            graphmode_save_new(list(record=rec,authorized=TRUE),file.path(rec$output_dir,"launch.rds"))
            for(n in names(captured)) graphmode_save_new(captured[[n]],file.path(rec$output_dir,n))
            graphmode_save_new(list(registration_signature=rec$signature,blinded_signature=blind$signature),file.path(rec$output_dir,"inputs-identity.rds"))
            job <- api$graphmode_refresh_run_job(rec,chain,arm);path <- file.path(rec$output_dir,paste0(job$name,"-plan.rds"));graphmode_save_new(job,path)
            list(record=rec,job=job,path=path)
        }
        f <- make("worker-ok");res <- NULL
        test("worker fixed transitions persist new schema and only four outer draws", {
            api$graphmode_refresh_run_worker(f$job,graphmode_root,TRUE)
            res <- readRDS(file.path(f$job$directory,"result.rds"))
            c <- api$graphmode_refresh_run_result_check(f$job,res,blind)
            assert(c$complete && identical(vapply(c$draws,`[[`,integer(1),"iteration"),5:8))
            assert(all(vapply(res$checkpoint$diagnostics,function(d) d$gate$counts$total_proposals==40,logical(1))))
            fails(api$graphmode_refresh_run_worker(f$job,graphmode_root,TRUE),"already attempted")
        })
        test("missing blocks, boundaries, local corrections, numerical flags and counts fail closed", {
            k <- which(tabulate(blind$starts[[1]]$state$Z,10)>0)[1L]
            mutations <- list(
                function(x) {x$checkpoint$diagnostics[[1]]$expert[[k]]$blocks <- list();x},
                function(x) {x$checkpoint$diagnostics[[1]]$expert[[k]]$blocks[[1]]$right <- rep(99,3);x},
                function(x) {x$checkpoint$diagnostics[[2]]$expert[[k]]$blocks[[1]]$observation$correction_by_time[1] <- 8;x},
                function(x) {x$checkpoint$diagnostics[[1]]$expert[[k]]$counts$proposals <- 1L;x},
                function(x) {x$checkpoint$diagnostics[[1]]$expert[[k]]$root_residual <- .01;x},
                function(x) {x$checkpoint$diagnostics[[1]]$expert[[k]]$blocks[[1]]$root_reciprocal_condition <- 0;x},
                function(x) {x$checkpoint$diagnostics[[2]]$gate$counts$total_proposals <- 10L;x},
                function(x) {x$checkpoint$diagnostics[[2]]$before_signature <- "changed";x},
                function(x) {x$checkpoint$saved[[1]]$iteration <- 1L;x},
                function(x) {x$checkpoint$diagnostics[[1]]$block_offset <- 42L;x},
                function(x) {x$checkpoint$diagnostics[[1]]$expert[[k]]$accepted <- TRUE;x})
            for(mutate in mutations) fails(api$graphmode_refresh_run_result_check(f$job,mutate(res),blind))
        })
        test("per-time outer rejection streak follows changing offsets, not block index", {
            m <- graphmode_blocks_run_mechanism(res$checkpoint$diagnostics,f$record$spec)
            occupied <- which(tabulate(blind$starts[[1]]$state$Z,10)>0)
            assert(all(m$longest_time_rejection_outer_sweeps[occupied,]==2L))
            assert(all(m$longest_time_rejection_outer_sweeps[-occupied,]==0L))
            assert(identical(m$guidance_proposals,rep(16,10)))
            # Retained offsets 28,35,0,7 imply 5+5+4+5, not always five blocks.
            assert(all(m$expert$block_proposals[occupied]==19L))
            assert(all(m$expert$empty_prior_refreshes[-occupied]==4L))
        })
        test("later step error retains last complete outer state; no retry", {
            g <- make("step-error");calls <- 0L;old <- api$graphmode_refresh_run_step
            api$graphmode_refresh_run_step <- function(...) {calls <<- calls+1L;if(calls==3L) stop("fixed step failure");old(...)}
            fails(api$graphmode_refresh_run_worker(g$job,graphmode_root,TRUE),"fixed step failure")
            api$graphmode_refresh_run_step <- old
            cp <- readRDS(file.path(g$job$directory,"failure.rds"))
            assert(cp$completed_steps==2L && cp$state$iteration==2L && calls==3L)
            assert(!file.exists(file.path(g$job$directory,"result.rds")))
        })
        test("canonical alias, moved path and pending publication protections remain", {
            assert(identical(graphmode4_output_target(sub("^/private/tmp/","/tmp/",f$record$directory)),f$record$directory))
            target <- file.path(f$job$directory,"extra-acceptance.rds")
            fails(api$graphmode_refresh_run_publish(f$record,list(ok=TRUE),target,function() stop("fixed moved source")),"fixed moved source")
            assert(!file.exists(target) && file.exists(sub("[.]rds$","-pending.rds",target)))
        })
        for(exit_status in c(0L,124L)) test(paste("parent accepts only successful exit even with saved result",exit_status), {
            g <- make(paste0("launch-",exit_status))
            api$graphmode_refresh_run_child <- function(command,path,repository,timeout,log_file=NULL) {
                assert(command=="worker" && timeout==1500);api$graphmode_refresh_run_worker(readRDS(path),repository,TRUE)
                list(status=exit_status,output="fixed process")
            }
            if(exit_status==0L) {
                api$graphmode_refresh_run_launch(g$job,g$path,graphmode_root,TRUE)
                assert(api$graphmode_refresh_run_job_evidence(g$job,graphmode_root)$receipt$postflight_passed)
            } else {
                fails(api$graphmode_refresh_run_launch(g$job,g$path,graphmode_root,TRUE),"timed out")
                assert(file.exists(file.path(g$job$directory,"result.rds")) && !file.exists(file.path(g$job$directory,"acceptance.rds")))
                fails(api$graphmode_refresh_run_job_evidence(g$job,graphmode_root))
            }
        })
        directory <- file.path(fixture,"full-fixed-batch");dir.create(directory)
        rr <- api$graphmode_refresh_run_record(graphmode_root,directory,identity,runtime,evidence)
        path <- file.path(directory,"registration.rds");graphmode_save_new(rr,path)
        calls <- character();diagnoses <- 0L
        api$graphmode_refresh_run_generate <- function(record,persist) {for(n in names(captured)) persist(captured[[n]],n);blind}
        api$graphmode_refresh_run_child <- function(command,path,repository,timeout,log_file=NULL) {
            object <- readRDS(path)
            if(command=="batch") api$graphmode_refresh_run_batch(object,repository,TRUE)
            else if(command=="worker") {calls <<- c(calls,object$name);api$graphmode_refresh_run_worker(object,repository,TRUE)}
            else if(command=="diagnostic-worker") {diagnoses <<- diagnoses+1L;api$graphmode_refresh_run_diagnostic_worker(object,repository,TRUE)}
            else stop("unexpected child")
            list(status=0L,output="fixed mock process")
        }
        test("all eight mock workers share inputs/paired starts and complete parent receipts", {
            api$graphmode_refresh_run_execute(rr,path,graphmode_root,FALSE,TRUE)
            x <- api$graphmode_refresh_run_science_evidence(rr,graphmode_root)
            assert(length(x$evidence)==8L && diagnoses==0L)
            assert(identical(calls,sprintf("%s-chain-%02d",rr$spec$jobs$arm,rr$spec$jobs$chain)))
            assert(length(unique(vapply(x$evidence,function(e) e$result$input_signature,character(1))))==1L)
            assert(!file.exists(file.path(directory,"diagnostic-report.rds")))
        })
        test("explicit fixed-input diagnosis retains all original flags and complete worker ESS cost", {
            api$graphmode_refresh_run_execute(rr,path,graphmode_root,TRUE,TRUE)
            report <- readRDS(file.path(directory,"diagnostic-report.rds"))
            assert(api$graphmode_refresh_run_diagnostic_evidence(rr,graphmode_root)$postflight_passed && diagnoses==1L)
            assert(identical(names(report$arms),c("full","block42")))
            for(a in report$arms) {
                assert(!a$validity$valid && nrow(a$validity$scalars)==37L && nrow(a$validity$psm_rms)==6L)
                assert(identical(a$efficiency$bulk_ess_per_worker_second,a$efficiency$bulk_ess/sum(a$worker_seconds)))
            }
            sci <- api$graphmode_refresh_run_science_evidence(rr,graphmode_root)
            bad <- report;bad$arms$block42$validity$valid <- TRUE;fails(api$graphmode_refresh_run_report_check(bad,rr,sci))
            bad <- report;bad$arms$full$efficiency$bulk_ess_per_worker_second <- 99;fails(api$graphmode_refresh_run_report_check(bad,rr,sci))
            bad <- report;bad$arms$block42$mechanism[[1]]$longest_time_rejection_outer_sweeps[1,1] <- 99
            fails(api$graphmode_refresh_run_report_check(bad,rr,sci))
            bad <- report;bad$arms$full$mechanism[[1]]$expert$retained_complete_seconds[1] <- 99
            fails(api$graphmode_refresh_run_report_check(bad,rr,sci))
        })
        test("once-only science and diagnosis refuse retry; late failures invalidate receipts", {
            fails(api$graphmode_refresh_run_execute(rr,path,graphmode_root,FALSE,TRUE),"already attempted")
            fails(api$graphmode_refresh_run_execute(rr,path,graphmode_root,TRUE,TRUE),"already attempted")
            assert(diagnoses==1L && length(calls)==8L)
            graphmode_save_new(list(error="fixed late failure"),file.path(directory,"science-failure.rds"))
            fails(api$graphmode_refresh_run_diagnostic_evidence(rr,graphmode_root),"Failed block comparison")
        })
    })
    test("all original source identities and RNG state/kind unchanged", {
        assert(identical(identity,graphmode_blocks_run_identity(graphmode_root)))
        after <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
        assert(identical(after,seed_before) && identical(RNGkind(),kind))
    })
    cat("PASS",checks,"D-056 fixed-input/mock-process groups; no real data/PG/MCMC, subprocess or old-result rerun.\n")
    cat("Fixed artifacts retained:",fixture,"\n")
})
