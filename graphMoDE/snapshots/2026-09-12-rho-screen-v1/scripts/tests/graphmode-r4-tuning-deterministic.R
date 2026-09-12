# D-037: fixed algebraic tapes and mocked I/O only, never a scientific pilot.
if (!exists("graphmode_root", inherits = FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent = globalenv())
    for (f in c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$"))))
        sys.source(file.path(graphmode_root, "R", f), envir = kernel)
    testenv <- environment(); parent.env(testenv) <- kernel
    before_rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    before_kind <- RNGkind(); checks <- 0L
    assert <- function(x) if (!isTRUE(x)) stop("D-037 assertion: ", paste(deparse(substitute(x)), collapse = " "), call. = FALSE)
    fails <- function(code, pattern) {
        e <- tryCatch({force(code); NULL}, error = identity)
        if (inherits(e, "error") && !grepl(pattern, conditionMessage(e)))
            stop("Unexpected error: ", conditionMessage(e), call. = FALSE)
        assert(inherits(e, "error") && grepl(pattern, conditionMessage(e)))
    }
    test <- function(name, code) {force(code); checks <<- checks + 1L; cat(sprintf("ok %02d - %s\n", checks, name))}
    patched <- function(bindings, code) {
        local_names <- intersect(names(bindings), ls(kernel, all.names = TRUE))
        originals <- mget(local_names, envir = kernel)
        on.exit({list2env(originals, envir = kernel)
            added <- setdiff(names(bindings), local_names)
            if (length(added)) rm(list = added, envir = kernel)
        }, add = TRUE)
        list2env(bindings, envir = kernel); force(code)
    }
    fixture <- tempfile("graphmode-tuning-fixed-"); dir.create(fixture); fixture <- normalizePath(fixture)
    # Test-owned fixed fixtures only, never user results.
    on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
    spec <- graphmode4_pilot_spec()
    panel <- graphmode4_pilot_panel(spec, array(0, c(5L, 168L, 3L)), 1:5, 1:121,
        matrix(rep(20:30, length.out = 121 * 168), 121L, 168L))
    fit <- panel$fit; config <- fit$core
    policy <- graphmode4_guidance_policy(4L, 2L, .44, 1, 1, .6, .05, 4)
    identity_now <- graphmode4_tuning_identity(graphmode_root); runtime_now <- graphmode4_pilot_runtime()
    build <- function(name, policy_value = policy) {
        core <- graphmode_run_plan(config, graphmode_initial_state(config, rep(1:3, length.out = 121)),
            identity_now$base$r4$core, 103L, 12L, 8L, 1L, 4L, 30,
            file.path(fixture, name), name, "deterministic fixture only", "development-pilot",
            spec$thresholds, list(data_identity = graphmode_digest(config$Y),
                geometry_identity = fit$geometry_identity, data_seed_record = "fixed array; no draw",
                permutation_seed_record = "identity; no draw", calibration_decision = "not calibrated",
                scientific_role = "fixed mocked I/O; no scientific random calls"))
        graphmode4_controlled_plan(core, fit, policy_value, identity_now, runtime_now)
    }
    test("policy validates bounds, batch schedule and explicit fields", {
        assert(identical(policy, do.call(graphmode4_guidance_policy, policy)))
        for (bad in list(list(target = 1), list(exponent = .5), list(upper = .04),
                         list(stop_at = 3L), list(gain = Inf))) {
            args <- policy; args[names(bad)] <- bad
            fails(do.call(graphmode4_guidance_policy, args), "Invalid|finite")
        }
        fails(graphmode4_guidance_control(config, 4L, policy), "settling")
    })
    test("acceptance changes the next batch only and in the right direction", {
        c0 <- graphmode4_guidance_control(config, 8, policy)
        c1 <- graphmode4_guidance_observe(c0, rep(TRUE, 10), 1L)
        assert(c1$sd == c0$sd && c1$updates == 0L)
        c2 <- graphmode4_guidance_observe(c1, rep(TRUE, 10), 2L)
        expected <- .25 * exp((1-.44) / 2^.6)
        assert(abs(c2$sd - expected) < 1e-14 && c2$updates == 1)
        low <- graphmode4_guidance_observe(c0, rep(FALSE, 10), 1L)
        low <- graphmode4_guidance_observe(low, rep(FALSE, 10), 2L)
        assert(low$sd < .25)
    })
    test("scale freezes before retained sampling regardless of later acceptances", {
        c <- graphmode4_guidance_control(config, 8, policy)
        for (i in 1:4) c <- graphmode4_guidance_observe(c, rep(TRUE, 10), i)
        frozen <- c$sd; assert(c$frozen && c$updates == 2)
        for (i in 5:20) c <- graphmode4_guidance_observe(c, rep(FALSE, 10), i)
        assert(identical(c$sd, frozen) && c$updates == 2)
        fails(graphmode4_guidance_observe(c, rep(TRUE, 10), 22L), "nonsequential")
        fails(graphmode4_guidance_observe(c, c(NA, rep(TRUE, 9)), 21L), "evidence")
    })
    test("proposal bounds do not clip guidance parameter values", {
        big <- policy; big$gain <- 100
        c <- graphmode4_guidance_control(config, 8, big)
        for (i in 1:4) c <- graphmode4_guidance_observe(c, rep(TRUE, 10), i)
        assert(abs(c$sd - 4) < 1e-14)
        c <- graphmode4_guidance_control(config, 8, big)
        for (i in 1:4) c <- graphmode4_guidance_observe(c, rep(FALSE, 10), i)
        assert(abs(c$sd - .05) < 1e-14)
    })
    test("shared and endpoint configurations have the right adaptation dimension", {
        args <- config[names(formals(graphmode_config))]; args$guidance <- "shared"
        shared <- do.call(graphmode_config, args)
        c <- graphmode4_guidance_control(shared, 8, policy)
        assert(c$free == 1L)
        fails(graphmode4_guidance_observe(c, rep(TRUE, 10), 1L), "evidence")
        args$guidance <- "none"; endpoint <- do.call(graphmode_config, args)
        fails(graphmode4_guidance_control(endpoint, 8, policy), "free guidance")
        c <- graphmode4_guidance_control(endpoint, 8)
        assert(c$free == 0 && is.null(c$sd) && c$frozen)
    })
    test("unadapted wrapper exactly uses the old deterministic-tape mathematical sweep", {
        tiny <- graphmode_config(matrix(c(1,2,3,4,5,6), 3, 2), matrix(1,2,1), 0, matrix(1),
            "graphMoDE-W", K=3L, G=matrix(1), W=matrix(.2), Phi=diag(3),
            rho=2L, guidance_proposal_sd=.25)
        state <- graphmode_initial_state(tiny, 1:3)
        patched(list(graphmode_normal = function(n) rep(.1,n), graphmode_uniform = function(n) rep(.5,n),
            graphmode_pg = function(b,z) rep(.25,length(b))), {
            expected <- graphmode_sweep(state,tiny)
            actual <- graphmode4_controlled_sweep(state,tiny,graphmode4_guidance_control(tiny,0))$out
            actual$tuning <- NULL
            for (k in 1:3) {expected$expert_updates[[k]]$seconds <- 0; actual$expert_updates[[k]]$seconds <- 0}
            assert(identical(actual, expected))
        })
    })
    test("effective scale does not mutate registered config or rho", {
        state <- graphmode_initial_state(config, rep(1:3,length.out=121))
        c <- graphmode4_guidance_control(config,8,policy); c$sd <- 1.5
        patched(list(graphmode_sweep = function(state, effective) {
            assert(effective$guidance_proposal_sd == 1.5 && effective$rho == config$rho)
            state$iteration <- state$iteration + 1L
            list(state=state,guidance_accept=rep(TRUE,10))
        }), {s <- graphmode4_controlled_sweep(state,config,c); assert(s$out$tuning$sd_used == 1.5)})
        assert(config$guidance_proposal_sd == .25 && identical(fit$core,config))
    })
    test("new plan binds policy/source/runtime and rejects historical or altered plans", {
        p <- build("signed"); graphmode4_controlled_validate(p)
        fails(graphmode4_controlled_validate(p$core_plan), "historical")
        for (field in c("policy","runtime","identity","formal_authorized","signature")) {
            q <- p; q[[field]] <- "changed"
            fails(graphmode4_controlled_validate(q), "changed|Controlled|must bind|invalid|Invalid|operator|Adaptation")
        }
    })
    test("identity includes new code and preserves old audited core", {
        assert(identity_now$base$audited_core_unchanged && identity_now$base$r4$requirements_match)
        assert("R/graphmode4-tuning.R" %in% names(identity_now$base$r4$sha256))
        assert("scripts/graphmode-r4-tuning.R" %in% names(identity_now$extra_sha256))
        assert(graphmode_pilot_loaded_source_ok(graphmode_root, identity_now$base$r4))
        patched(list(graphmode4_controlled_sweep = function(...) stop("tampered")), {
            assert(!graphmode_pilot_loaded_source_ok(graphmode_root, identity_now$base$r4))
        })
    })
    test("no launch authority or new source freeze means no output", {
        p <- build("blocked")
        fails(graphmode4_controlled_run(p,graphmode_root), "Separate authorization")
        if (!identity_now$committed) fails(graphmode4_controlled_preflight(p,graphmode_root), "unfrozen")
        assert(!dir.exists(p$core_plan$output_dir))
    })
    expert <- function(accepted=TRUE,empty=FALSE,movement=1,seconds=.01) list(empty=empty,accepted=accepted,
        log_acceptance=if(empty) NA_real_ else 0, movement=movement,seconds=seconds,
        factor_residual=1e-14,root_residual=1e-13,root_reciprocal_condition=if(empty) NA_real_ else .1)
    fake_sweep <- function(state, config) {
        state$iteration <- state$iteration + 1L
        sizes <- tabulate(state$Z,config$K)
        list(state=state,eta=gmde_eta(state$theta,config$Fmat),sizes=sizes,sorted_sizes=sort(sizes,decreasing=TRUE),
            Kocc=sum(sizes>0),fragmentation=0,events=c(node_moves=0,pair_changes=0),potts_gross=NULL,
            ess_evaluations=1L,guidance_accept=rep(TRUE,10),expert_updates=rep(list(expert()),10))
    }
    mocked_run <- function(p, sweep=fake_sweep) patched(list(
        graphmode4_controlled_preflight=function(p,repository) list(ready=TRUE,problems=character(),output_dir=p$core_plan$output_dir),
        graphmode4_tuning_guard=function(...) TRUE,graphmode_sweep=sweep), {
        graphmode4_controlled_run(p,graphmode_root,authorized=TRUE)
    })
    test("runner checkpoints used/next scales and freezes the complete retained window", {
        p <- build("completed"); out <- mocked_run(p)
        result <- readRDS(file.path(p$core_plan$output_dir,"result.rds")); cp <- result$checkpoint
        assert(cp$schema == graphmode4_tuning_version && length(cp$saved)==4)
        assert(cp$control$frozen && cp$control$updates==2 && out$retained_draws==4)
        ds <- vapply(cp$diagnostics,function(d) d$tuning$sd_used,numeric(1))
        assert(ds[1]==.25 && ds[2]==.25 && ds[3]>.25 && all(ds[5:12]==ds[5]))
        assert(all(vapply(cp$diagnostics[9:12],function(d) d$tuning$frozen_used,logical(1))))
        assert(result$chain_record$numerical_guards_passed && !result$convergence_certified)
        assert(identical(readRDS(file.path(p$core_plan$output_dir,"registration.rds"))$plan,p))
        assert(file.exists(file.path(p$core_plan$output_dir,"checkpoint-000000008.rds")))
    })
    test("fixed-scale runner and no automatic overwrite/resume", {
        p <- build("fixed",NULL); mocked_run(p)
        cp <- readRDS(file.path(p$core_plan$output_dir,"result.rds"))$checkpoint
        assert(cp$control$updates==0 && cp$control$sd==.25)
        suppressWarnings(fails(mocked_run(p),"exclusive"))
        assert(!file.exists(file.path(p$core_plan$output_dir,"failure.rds")))
    })
    test("failed sweep retains last complete state and control without extension", {
        p <- build("failed")
        fails(mocked_run(p,function(state,config) {
            if(state$iteration==8) stop("fixed failure")
            fake_sweep(state,config)
        }),"fixed failure")
        cp <- readRDS(file.path(p$core_plan$output_dir,"failure.rds"))
        assert(cp$state$iteration==8 && cp$control$iteration==8 && length(cp$saved)==0)
        assert(!file.exists(file.path(p$core_plan$output_dir,"result.rds")) && !cp$resume_supported)
    })
    test("missing output tree is not silently recreated", {
        p <- build("disappeared"); moved <- file.path(fixture,"test-moved-output")
        fails(mocked_run(p,function(state,config) {
            if(state$iteration==2) assert(file.rename(p$core_plan$output_dir,moved))
            fake_sweep(state,config)
        }),"disappeared")
        assert(!dir.exists(p$core_plan$output_dir) && dir.exists(moved))
    })
    test("numerical failures still reject completion under original thresholds", {
        p <- build("bad-root")
        fails(mocked_run(p,function(state,config) {
            out <- fake_sweep(state,config); out$expert_updates[[1]]$root_residual <- 1e-3; out
        }),"Numerical protection")
        assert(!file.exists(file.path(p$core_plan$output_dir,"result.rds")))
        assert(file.exists(file.path(p$core_plan$output_dir,"failure.rds")))
    })
    test("sweep-boundary budget stops before a scientific update", {
        p <- build("budget"); args <- p$core_plan[names(formals(graphmode_run_plan))]
        args$budget_seconds <- .Machine$double.eps
        p <- graphmode4_controlled_plan(do.call(graphmode_run_plan,args),fit,policy,identity_now,runtime_now)
        ticks <- -1
        patched(list(proc.time=function() {ticks <<- ticks+1; c(0,0,ticks,0,0)}), {
            fails(mocked_run(p,function(...) stop("unexpected scientific call")),"budget exhausted")
        })
        cp <- readRDS(file.path(p$core_plan$output_dir,"failure.rds"))
        assert(cp$state$iteration==0 && length(cp$saved)==0 && cp$error != "unexpected scientific call")
    })
    test("launcher enforces child timeout and retains returned console without retry", {
        p <- build("timeout"); path <- file.path(fixture,"launch-plan.rds"); graphmode_save_new(p,path)
        calls <- 0L
        patched(list(graphmode4_controlled_preflight=function(...) list(ready=TRUE,output_dir=p$core_plan$output_dir),
            system2=function(command,args,stdout,stderr,timeout) {
                calls <<- calls+1L; assert(timeout==30 && "worker-run" %in% args)
                dir.create(p$core_plan$output_dir)
                graphmode_save_new(list(plan=p),file.path(p$core_plan$output_dir,"registration.rds"))
                structure("fixed child timeout",status=124L)
            }), {fails(graphmode4_controlled_launch(p,path,graphmode_root,TRUE),"timed out")})
        assert(calls==1 && readRDS(file.path(p$core_plan$output_dir,"execution.rds"))$status==124L)
    })
    test("movement report separates empty experts and rejects malformed movement", {
        make <- function(i,e) list(iteration=as.integer(i),expert=list(e),events=c(pair_changes=0),guidance_accept=TRUE)
        e0 <- expert(FALSE,FALSE,0); ee <- expert(NA,TRUE,0)
        ds <- list(make(1,expert()),make(2,e0),make(3,e0),make(4,ee),make(5,e0))
        m <- graphmode4_movement_report(ds,0)$experts
        assert(m$occupied_updates==4 && m$accepted==1 && m$longest_rejection_streak==2)
        assert(abs(m$state_seconds-.05)<1e-14 && abs(m$occupied_state_seconds-.04)<1e-14 &&
            abs(m$empty_state_seconds-.01)<1e-14)
        bad <- ds; bad[[3]]$expert[[1]]$movement <- 1
        fails(graphmode4_movement_report(bad,0),"movement evidence")
    })
    template <- build("rho-template",NULL)
    starts <- list(template$core_plan$initial_state,graphmode_initial_state(config,rep(1:5,length.out=121)))
    screen <- graphmode4_rho_screen_plan(template,starts,c(1001L,1002L),c(1L,2L,4L),file.path(fixture,"rho-jobs"),.6)
    test("rho branches use matched initial states and seeds, never sequential carry-over", {
        assert(length(screen$jobs)==6 && !screen$common_rho_selected && !dir.exists(file.path(fixture,"rho-jobs")))
        for(h in 1:2) for(rho in c(1L,2L,4L)) {
            j <- screen$jobs[[paste0("start-",h,"-rho-",rho)]]
            assert(identical(j$plan$core_plan$initial_state,starts[[h]]))
            assert(j$plan$core_plan$seed==1000L+h && j$plan$core_plan$config$rho==rho)
            assert(is.null(j$plan$policy)); graphmode4_controlled_validate(j$plan)
            assert(identical(j$plan$core_plan$config$Y,config$Y))
        }
    })
    test("rho screen rejects simultaneous guidance tuning, fractional rho and duplicate starts", {
        fails(graphmode4_rho_screen_plan(build("adaptive-template"),starts,c(1L,2L),c(1,2),fixture,.6),"Hold guidance")
        fails(graphmode4_rho_screen_plan(template,starts,c(1L,2L),c(1,2.5),fixture,.6),"integer rho")
        fails(graphmode4_rho_screen_plan(template,rep(starts[1],2),c(1L,2L),c(1,2),fixture,.6),"different label")
    })
    # Synthetic success receipts are created only for these fixed test records.
    fixture_evidence <- function(p,result) {
        execution <- list(status=0L,output="fixed success",signature=p$signature,
            hard_budget_seconds=p$core_plan$budget_seconds,resume_supported=FALSE)
        list(registration=list(plan=p),result=result,execution=execution,
            acceptance=list(schema=graphmode4_tuning_version,accepted=TRUE,postflight_passed=TRUE,
                plan_signature=p$signature,execution_signature=graphmode_digest(execution),
                result_signature=graphmode_digest(result)),failure=NULL,read_errors=character())
    }
    rebind_fixture <- function(e) {
        e$acceptance$result_signature <- graphmode_digest(e$result)
        e$acceptance$execution_signature <- graphmode_digest(e$execution)
        e
    }
    fake_rho_results <- function(score=function(rho,h,i) if(rho==2) 4 else 1,
                                 cost=function(rho,h,i) 1,screen_value=screen) {
        lapply(screen_value$jobs,function(job) {
            p <- job$plan; core <- p$core_plan
            ds <- lapply(1:12,function(i) list(iteration=as.integer(i),
                expert=rep(list(expert(movement=score(job$rho,job$start,i),seconds=cost(job$rho,job$start,i))),10),
                events=c(node_moves=0,pair_changes=0),guidance_accept=rep(TRUE,10),
                tuning=list(rho=job$rho,sd_used=.25,frozen_used=TRUE)))
            draws <- lapply(9:12,function(i) list(iteration=as.integer(i)))
            result <- list(checkpoint=list(schema=graphmode4_tuning_version,plan_signature=p$signature,
                status="completed-not-convergence-certified",source_identity=p$identity,runtime=p$runtime,
                state=list(iteration=12L),diagnostics=ds,saved=draws,elapsed_seconds=1,
                control=list(policy=NULL,frozen=TRUE)))
            fixture_evidence(p,result)
        })
    }
    test("rho reducer uses movement/time and exact smaller-rho tie, not acceptance alone", {
        result <- graphmode4_rho_screen_reduce(screen,fake_rho_results())
        assert(result$resolved && result$candidate_rho==2 && !result$common_rho_selected)
        expensive <- fake_rho_results(cost=function(rho,h,i) if(rho==2) 10 else 1)
        result <- graphmode4_rho_screen_reduce(screen,expensive)
        assert(result$resolved && result$candidate_rho==1)
        tie <- graphmode4_rho_screen_reduce(screen,fake_rho_results(score=function(...) 1))
        assert(tie$candidate_rho==1)
    })
    test("P2 complete expert time reverses the audited occupied-only rho ranking", {
        r <- fake_rho_results()
        for(id in names(r)) {
            rho <- screen$jobs[[id]]$rho
            occupied <- expert(movement=if(rho==2) 4 else 1,seconds=if(rho==2) .08 else .01)
            empty <- expert(NA,TRUE,0,.01)
            for(i in 1:12) r[[id]]$result$checkpoint$diagnostics[[i]]$expert <- c(list(occupied),rep(list(empty),9))
            r[[id]] <- rebind_fixture(r[[id]])
        }
        out <- graphmode4_rho_screen_reduce(screen,r)
        assert(out$resolved && out$candidate_rho==2)
        scores <- vapply(c(1,2),function(rho) {
            x <- out$scores[out$scores$rho==rho,]; sum(x$movement)/sum(x$seconds)
        },numeric(1))
        assert(max(abs(scores-c(10,4/.17)))<1e-12)
        assert(all(abs(out$scores$empty_seconds-.18)<1e-14))
        assert(max(abs(out$scores$seconds-out$scores$occupied_seconds-out$scores$empty_seconds))<1e-14)
    })
    test("empty movement is excluded but all-empty time remains charged", {
        r <- fake_rho_results()
        for(id in names(r)) {
            for(i in c(9,11)) r[[id]]$result$checkpoint$diagnostics[[i]]$expert <- rep(list(expert(NA,TRUE,99,.1)),10)
            r[[id]] <- rebind_fixture(r[[id]])
        }
        out <- graphmode4_rho_screen_reduce(screen,r)
        assert(out$resolved && out$candidate_rho==2)
        assert(max(abs(out$scores$empty_seconds-1))<1e-14 && all(out$scores$seconds==11))
        assert(all(out$scores$movement==ifelse(out$scores$rho==2,40,10)))
    })
    test("empty-expert missing/invalid time cannot be silently dropped", {
        for(seconds in list(NA_real_,-1,Inf,NULL)) {
            r <- fake_rho_results()
            r[[1]]$result$checkpoint$diagnostics[[9]]$expert[[10]] <- expert(NA,TRUE,0,seconds)
            r[[1]] <- rebind_fixture(r[[1]])
            out <- graphmode4_rho_screen_reduce(screen,r)
            assert(!out$resolved && length(out$failures)>0 && is.na(out$candidate_rho))
        }
    })
    test("rho failures/missing branches/zero movement are retained, not excluded", {
        r <- fake_rho_results(); r[[1]] <- list(failure="failed")
        assert(!graphmode4_rho_screen_reduce(screen,r)$resolved)
        assert(!graphmode4_rho_screen_reduce(screen,fake_rho_results()[-1])$resolved)
        zero <- fake_rho_results(score=function(rho,h,i) if(rho==4) 0 else 1)
        result <- graphmode4_rho_screen_reduce(screen,zero)
        assert(!result$resolved && is.na(result$candidate_rho))
        r <- fake_rho_results(); r[[1]] <- "bad output"
        assert(!graphmode4_rho_screen_reduce(screen,r)$resolved)
    })
    test("rho start and scoring-half disagreement remains unresolved", {
        by_start <- fake_rho_results(score=function(rho,h,i) if(rho==h) 3 else 1)
        assert(!graphmode4_rho_screen_reduce(screen,by_start)$resolved)
        by_half <- fake_rho_results(score=function(rho,h,i) if(rho==if(i<=10) 1 else 2) 3 else 1)
        assert(!graphmode4_rho_screen_reduce(screen,by_half)$resolved)
        one_jump <- fake_rho_results(score=function(rho,h,i) if(i==9) 100 else .01)
        assert(!graphmode4_rho_screen_reduce(screen,one_jump)$resolved)
    })
    test("rho reducer rejects changed tuning, source and numerical evidence", {
        for (what in c("rho","scale","source","root")) {
            r <- fake_rho_results()
            if(what=="rho") r[[1]]$result$checkpoint$diagnostics[[10]]$tuning$rho <- 8L
            if(what=="scale") r[[1]]$result$checkpoint$diagnostics[[10]]$tuning$sd_used <- 1
            if(what=="source") r[[1]]$result$checkpoint$source_identity <- "wrong"
            if(what=="root") r[[1]]$result$checkpoint$diagnostics[[10]]$expert[[1]]$root_residual <- 1e-3
            r[[1]] <- rebind_fixture(r[[1]])
            out <- graphmode4_rho_screen_reduce(screen,r)
            assert(!out$resolved && !any(grepl("acceptance",out$failures)))
        }
    })
    test("rho aggregate overflow remains unresolved rather than producing a winner", {
        result <- graphmode4_rho_screen_reduce(screen,fake_rho_results(score=function(...) 1e308))
        assert(!result$resolved && is.na(result$candidate_rho))
    })
    test("P2 completed results require valid successful exit evidence", {
        for(status in list(124L,1L,-1L,NA_integer_,Inf,c(0L,124L),"0",logical(),NULL)) {
            r <- fake_rho_results(); r[[1]]$execution$status <- status
            r[[1]] <- rebind_fixture(r[[1]])
            out <- graphmode4_rho_screen_reduce(screen,r)
            assert(!out$resolved && any(grepl("exit evidence",out$failures)))
        }
        r <- fake_rho_results(); r[[1]] <- r[[1]]$result
        assert(!graphmode4_rho_screen_reduce(screen,r)$resolved)
    })
    test("exit zero cannot replace final postflight acceptance", {
        for(what in c("missing","not-accepted","postflight","schema","signature")) {
            r <- fake_rho_results()
            if(what=="missing") r[[1]]$acceptance <- NULL
            if(what=="not-accepted") r[[1]]$acceptance$accepted <- FALSE
            if(what=="postflight") r[[1]]$acceptance$postflight_passed <- FALSE
            if(what=="schema") r[[1]]$acceptance$schema <- "old"
            if(what=="signature") r[[1]]$acceptance$plan_signature <- "other"
            out <- graphmode4_rho_screen_reduce(screen,r)
            assert(!out$resolved && any(grepl("Final controlled acceptance",out$failures)))
        }
    })
    test("launch/result hashes, registration, budget and failure evidence are bound", {
        for(what in c("result","execution","execution-plan","budget","registration","failure","read-error")) {
            r <- fake_rho_results()
            if(what=="result") r[[1]]$result$checkpoint$diagnostics[[9]]$expert[[1]]$movement <- 2
            if(what=="execution") r[[1]]$execution$output <- "changed after acceptance"
            if(what=="execution-plan") r[[1]]$execution$signature <- "other"
            if(what=="budget") r[[1]]$execution$hard_budget_seconds <- 999
            if(what=="registration") r[[1]]$registration$plan <- screen$jobs[[2]]$plan
            if(what=="failure") r[[1]]$failure <- list(present=TRUE,record=NULL)
            if(what=="read-error") r[[1]]$read_errors <- "unreadable failure"
            assert(!graphmode4_rho_screen_reduce(screen,r)$resolved)
        }
    })
    # Exercise the REAL parent/collector/reducer with a mocked system2. No
    # child process or scientific draw is made, including on success paths.
    real_path_preflight <- function(p) {
        # Match the fresh-source Terminal lifecycle; isolate this path check
        # from the preceding tests' repeatedly substituted kernel functions.
        fresh <- new.env(parent=globalenv())
        for(f in c("gmde-helpers.R","gmde-state-update.R",
            sort(list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$")),
            sort(list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$"))))
            sys.source(file.path(graphmode_root,"R",f),fresh)
        fresh$graphmode4_tuning_guard <- function(...) TRUE
        fresh$graphmode4_controlled_preflight(p,graphmode_root)
    }
    launch_fixture <- function(name,status=NULL,postflight_error=FALSE,worker_failure=FALSE,
                               missing_result=FALSE,changed_registration=FALSE,bad_result=FALSE,
                               alias=FALSE) {
        parent <- file.path(fixture,name); dir.create(parent)
        if(alias) {
            link <- file.path(fixture,paste0(name,"-alias")); assert(file.symlink(parent,link))
            parent <- link
        }
        s <- graphmode4_rho_screen_plan(template,starts,c(1001L,1002L),c(1L,2L,4L),parent,.6)
        r <- fake_rho_results(screen_value=s); p <- s$jobs[[1]]$plan
        path <- file.path(parent,"plan.rds"); graphmode_save_new(p,path)
        calls <- 0L; postflights <- 0L
        error <- patched(list(
            graphmode4_controlled_preflight=function(...) {
                # The alias regression uses BOTH real path preflight layers;
                # only the new source/runtime guard and child call are mocked.
                if(alias) {
                    checked <- real_path_preflight(p)
                    if(!checked$ready) stop(paste(checked$problems,collapse="; "),call.=FALSE)
                    return(checked)
                }
                list(ready=TRUE,output_dir=p$core_plan$output_dir)
            },
            graphmode4_tuning_guard=function(...) {postflights <<- postflights+1L
                if(postflight_error) stop("fixed postflight rejection"); TRUE},
            system2=function(command,args=character(),stdout="",stderr="",timeout=0,...) {
                if(basename(command)!="Rscript")
                    return(base::system2(command,args,stdout=stdout,stderr=stderr,timeout=timeout,...))
                calls <<- calls+1L; assert(timeout==30)
                dir.create(p$core_plan$output_dir)
                graphmode_save_new(list(plan=if(changed_registration) template else p),
                    file.path(p$core_plan$output_dir,"registration.rds"))
                result <- r[[1]]$result
                if(bad_result) result$checkpoint$status <- "failed-retained-do-not-resume"
                if(!missing_result) graphmode_save_new(result,file.path(p$core_plan$output_dir,"result.rds"))
                if(worker_failure) graphmode_save_new(NULL,file.path(p$core_plan$output_dir,"failure.rds"))
                structure("fixed child output",status=status)
            }), tryCatch({suppressWarnings(graphmode4_controlled_launch(p,path,graphmode_root,TRUE)); NULL},error=identity))
        r[[1]] <- graphmode4_controlled_evidence(p)
        list(error=error,calls=calls,postflights=postflights,evidence=r[[1]],
            reduced=graphmode4_rho_screen_reduce(s,r),plan=p)
    }
    test("P2 result saved before timeout stays on disk but cannot resolve rho", {
        x <- launch_fixture("saved-before-timeout",status=124L)
        assert(inherits(x$error,"error") && grepl("timed out",conditionMessage(x$error)))
        assert(x$calls==1 && x$postflights==0 && x$evidence$execution$status==124L)
        assert(is.list(x$evidence$result) && is.null(x$evidence$acceptance) && !x$reduced$resolved)
        assert(file.exists(file.path(x$plan$core_plan$output_dir,"result.rds")))
    })
    test("successful mocked launch writes a bound final receipt and resolves rho", {
        x <- launch_fixture("accepted-child")
        assert(is.null(x$error) && x$calls==1 && x$postflights==1)
        assert(x$evidence$acceptance$accepted && !length(x$evidence$read_errors))
        assert(x$reduced$resolved && x$reduced$candidate_rho==2)
    })
    test("postflight failure retains exit zero and result without final acceptance", {
        x <- launch_fixture("failed-postflight",postflight_error=TRUE)
        assert(inherits(x$error,"error") && grepl("postflight rejection",conditionMessage(x$error)))
        assert(x$calls==1 && x$postflights==1 && x$evidence$execution$status==0L)
        assert(is.list(x$evidence$result) && is.null(x$evidence$acceptance) && !x$reduced$resolved)
    })
    test("parent refuses worker failure, missing/failed result or changed registration", {
        for(what in c("worker_failure","missing_result","changed_registration","bad_result")) {
            args <- list(name=paste0("refused-",what)); args[[what]] <- TRUE
            x <- do.call(launch_fixture,args)
            assert(inherits(x$error,"error") && x$calls==1)
            assert(is.null(x$evidence$acceptance) && !x$reduced$resolved)
        }
    })
    test("collector rejects unreadable files and cannot retrofit a missing receipt", {
        x <- launch_fixture("collector-bad-file")
        dir <- x$plan$core_plan$output_dir
        assert(file.rename(file.path(dir,"acceptance.rds"),file.path(dir,"test-retained-acceptance.rds")))
        evidence <- graphmode4_controlled_evidence(x$plan)
        assert(is.null(evidence$acceptance) && length(evidence$read_errors)==1)
        fails(graphmode4_controlled_evidence_check(x$plan,evidence),"launch evidence")
        # A fixed non-RDS fixture, not an edit to user results.
        writeLines("invalid fixed RDS",file.path(dir,"failure.rds"))
        evidence <- graphmode4_controlled_evidence(x$plan)
        assert(!is.null(evidence$failure) && length(evidence$read_errors)==2)
        assert(!file.exists(file.path(dir,"acceptance.rds")))
    })
    test("P2 aliased parent accepted by real preflight also passes final acceptance", {
        x <- launch_fixture("aliased-parent",alias=TRUE)
        if(!is.null(x$error)) stop(conditionMessage(x$error),call.=FALSE)
        assert(is.null(x$error) && x$calls==1)
        assert(x$evidence$acceptance$accepted && x$reduced$resolved && x$reduced$candidate_rho==2)
    })
    plan_at <- function(path) {
        args <- template$core_plan[names(formals(graphmode_run_plan))]; args$output_dir <- path
        core <- do.call(graphmode_run_plan,args)
        p <- graphmode4_controlled_plan(core,fit,NULL,identity_now,runtime_now)
        assert(identical(p$core_plan,core)) # Never silently re-sign the old core contract.
        p
    }
    test("signed target unifies canonical, symlink, dot and slash spellings without writes", {
        parent <- file.path(fixture,"path-spellings"); dir.create(parent)
        child <- file.path(parent,"child"); dir.create(child)
        alias <- file.path(fixture,"path-spellings-alias"); assert(file.symlink(parent,alias))
        target <- file.path(parent,"new-output")
        paths <- c(target,file.path(alias,"new-output"),file.path(child,"..","new-output"),
            file.path(parent,".","new-output"),paste0(parent,"//new-output/"))
        for(path in paths) {
            p <- plan_at(path); assert(identical(p$output_dir,target))
            graphmode4_controlled_validate(p)
            checked <- real_path_preflight(p)
            assert(checked$ready && identical(checked$output_dir,target) && !dir.exists(target))
        }
    })
    test("macOS tmp alias uses the signed physical target through worker checkpoints", {
        # One independently named test-owned directory; never an existing result.
        tmp <- tempfile("graphmode-path-fixed-",tmpdir="/tmp"); assert(dir.create(tmp))
        on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
        p <- plan_at(file.path(tmp,"worker"))
        target <- file.path(normalizePath(tmp),"worker")
        assert(identical(p$output_dir,target))
        checked <- real_path_preflight(p); assert(checked$ready && identical(checked$output_dir,target))
        patched(list(graphmode4_controlled_preflight=function(...) checked,
            graphmode4_tuning_guard=function(...) TRUE,graphmode_sweep=fake_sweep), {
            graphmode4_controlled_run(p,graphmode_root,TRUE)
        })
        evidence <- graphmode4_controlled_evidence(p)
        assert(identical(evidence$registration$plan,p) &&
            evidence$result$checkpoint$status=="completed-not-convergence-certified")
        assert(file.exists(file.path(target,"checkpoint-000000012.rds")))
        assert(is.null(evidence$acceptance)) # Worker alone cannot grant parent acceptance.
    })
    test("future rho parents bind to existing ancestors and preflight creates nothing", {
        parent <- file.path(fixture,"future-parent","nested")
        p <- plan_at(file.path(parent,"output"))
        assert(identical(p$output_dir,file.path(parent,"output")) && !dir.exists(dirname(parent)))
        checked <- real_path_preflight(p)
        assert(!checked$ready && !dir.exists(dirname(parent)))
        assert(dir.create(parent,recursive=TRUE))
        assert(real_path_preflight(p)$ready)
        graphmode4_controlled_validate(p)
    })
    test("unresolvable or ambiguous output paths fail before any run", {
        ordinary <- file.path(fixture,"not-a-directory"); writeLines("fixed",ordinary)
        broken <- file.path(fixture,"broken-parent")
        assert(file.symlink(file.path(fixture,"absent-target"),broken))
        for(path in c("relative/output","/",file.path(fixture,"."),file.path(fixture,".."),
            file.path(ordinary,"output"),file.path(broken,"output"),
            file.path(fixture,"missing","..","output")))
            fails(plan_at(path),"absolute output|resolvable directory")
    })
    test("redirected parent after signing fails before subprocess dispatch", {
        a <- file.path(fixture,"parent-original"); b <- file.path(fixture,"parent-redirect")
        dir.create(a); dir.create(b)
        alias <- file.path(fixture,"redirected-alias"); assert(file.symlink(a,alias))
        p <- plan_at(file.path(alias,"new-output"))
        path <- file.path(fixture,"redirect-plan.rds"); graphmode_save_new(p,path)
        assert(file.rename(alias,paste0(alias,"-retained"))); assert(file.symlink(b,alias))
        fails(graphmode4_controlled_validate(p),"plan was changed")
        dispatched <- FALSE
        patched(list(system2=function(...) {dispatched <<- TRUE; stop("unexpected dispatch")}), {
            fails(graphmode4_controlled_launch(p,path,graphmode_root,TRUE),"plan was changed")
        })
        assert(!dispatched && !dir.exists(p$output_dir) && !dir.exists(file.path(b,"new-output")))
    })
    test("preflight rejects a target differing from the frozen mapping", {
        p <- plan_at(file.path(fixture,"preflight-mismatch"))
        patched(list(graphmode4_tuning_guard=function(...) TRUE,
            graphmode_preflight=function(...) list(ready=TRUE,problems=character(),
                output_dir=file.path(fixture,"different-output"))), {
            checked <- graphmode4_controlled_preflight(p,graphmode_root)
            assert(!checked$ready && any(grepl("target changed",checked$problems)))
        })
        changed <- p; changed$output_dir <- file.path(fixture,"different-output")
        fails(graphmode4_controlled_validate(changed),"plan was changed")
    })
    test("output moved during fixed worker execution still fails without recreation", {
        parent <- file.path(fixture,"move-worker-parent"); dir.create(parent)
        alias <- file.path(fixture,"move-worker-alias"); assert(file.symlink(parent,alias))
        p <- plan_at(file.path(alias,"output")); moved <- file.path(fixture,"retained-worker-output")
        checked <- real_path_preflight(p); assert(checked$ready)
        patched(list(graphmode4_controlled_preflight=function(...) checked,
            graphmode4_tuning_guard=function(...) TRUE,graphmode_sweep=function(state,config) {
            if(state$iteration==2L) assert(file.rename(p$output_dir,moved))
            fake_sweep(state,config)
        }), {fails(graphmode4_controlled_run(p,graphmode_root,TRUE),"disappeared")})
        assert(dir.exists(moved) && !dir.exists(p$output_dir))
        assert(file.exists(file.path(moved,"registration.rds")))
        assert(!file.exists(file.path(moved,"acceptance.rds")))
    })
    test("final acceptance rejects an output symlink redirected to another directory", {
        p <- plan_at(file.path(fixture,"final-redirect"))
        path <- file.path(fixture,"final-redirect-plan.rds"); graphmode_save_new(p,path)
        elsewhere <- file.path(fixture,"final-other"); dir.create(elsewhere)
        patched(list(graphmode4_tuning_guard=function(...) TRUE,
            graphmode4_controlled_preflight=function(...) real_path_preflight(p),
            system2=function(command,args=character(),stdout="",stderr="",timeout=0,...) {
                if(basename(command)!="Rscript")
                    return(base::system2(command,args,stdout=stdout,stderr=stderr,timeout=timeout,...))
                # No actual child. Redirect even matching registration evidence.
                graphmode_save_new(list(plan=p),file.path(elsewhere,"registration.rds"))
                assert(file.symlink(elsewhere,p$output_dir))
                structure("fixed redirect",status=0L)
            }), {fails(graphmode4_controlled_launch(p,path,graphmode_root,TRUE),"destination/plan changed")})
        assert(!file.exists(file.path(elsewhere,"acceptance.rds")))
    })
    test("old scoring schema cannot be reused even with a recomputed signature", {
        for(version in c("v1","v2")) {
            old <- screen; old$schema <- paste0("graphmode-r4-rho-screen-20260912-",version)
            old$signature <- NULL; old$signature <- graphmode_digest(old)
            fails(graphmode4_rho_screen_reduce(old,fake_rho_results()),"obsolete")
            p <- template; p$schema <- paste0("graphmode-r4-controlled-development-20260912-",version)
            p$signature <- NULL; p$signature <- graphmode_digest(p)
            fails(graphmode4_controlled_validate(p),"historical")
        }
    })
    after_rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    assert(identical(before_rng,after_rng) && identical(before_kind,RNGkind()))
    cat(sprintf("PASS: %d fixed-input/mocked-I/O groups; no PG draws, generated responses or scientific chains.\n",checks))
})
