# Fixed tapes and algebra only. No actual PG, Gaussian or response sampling.
if (!exists("graphmode_root", inherits = FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent = globalenv())
    files <- c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$")), "graphmode_dev.R")
    before_rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    before_kind <- RNGkind()
    for (f in files) sys.source(file.path(graphmode_root, "R", f), kernel)
    test_environment <- environment()
    parent.env(test_environment) <- kernel
    checks <- 0L
    assert <- function(x) if (!isTRUE(x)) stop("D-043 assertion: ", paste(deparse(substitute(x)), collapse = " "), call. = FALSE)
    close <- function(x, y, tol = 1e-10) {
        assert(identical(dim(x), dim(y)) && length(x) == length(y))
        assert(all(is.finite(c(x,y))) && max(abs(x-y), 0) <= tol * max(1, abs(y)))
    }
    fails <- function(expr, pattern) {
        e <- tryCatch({force(expr); NULL}, error = identity)
        assert(inherits(e,"error") && grepl(pattern, conditionMessage(e)))
    }
    patched <- function(bindings, code) {
        local_names <- intersect(names(bindings), ls(kernel, all.names=TRUE))
        old <- mget(local_names, kernel)
        on.exit({list2env(old, kernel); added <- setdiff(names(bindings), local_names)
            if (length(added)) rm(list=added, envir=kernel)}, add=TRUE)
        list2env(bindings, kernel); force(code)
    }
    test <- function(name, code) {
        patched(list(graphmode_normal=function(n) rep(.1,n),
            graphmode_uniform=function(n) rep(.5,n), graphmode_pg=function(b,z) rep(.25,length(b)),
            graphmode_gamma=function(shape,rate) shape/rate), force(code))
        checks <<- checks+1L; cat(sprintf("ok %02d - %s\n", checks, name))
    }
    test("uniform categorical probabilities match elementary expectations", {
        d <- graphmode_dev_allocation(matrix(0,2,3), c(1,2), c(1,3))
        close(d$expected_node_moves, 4/3); close(d$log_probability_no_node_moves, 2*log(1/3))
        assert(d$observed_node_moves==1 && !d$convergence_certified)
    })
    test("tiny alternative mass survives one-minus-probability cancellation", {
        d <- graphmode_dev_allocation(matrix(c(0,-100,-100),1), 1)
        assert(d$expected_node_moves > 0)
        close(d$log_expected_node_moves, log(2)-100)
        assert(d$log_probability_no_node_moves < 0)
    })
    test("huge common offsets preserve alternative category multiplicity", {
        d <- graphmode_dev_allocation(matrix(1e20,2,10),c(1,5))
        close(d$expected_node_moves,1.8)
        close(d$log_probability_no_node_moves,2*log(.1))
        close(d$mean_max_probability,.1)
    })
    test("underflowed linear probability still has a finite log diagnostic", {
        d <- graphmode_dev_allocation(matrix(c(0,-1000),1), 1)
        assert(d$expected_node_moves==0 && d$log_expected_node_moves == -1000)
    })
    test("an unlikely current label retains the correct log no-change probability", {
        d <- graphmode_dev_allocation(matrix(c(-1000,0),1), 1)
        assert(d$expected_node_moves==1 && d$log_probability_no_node_moves == -1000)
    })
    test("row shifts and corresponding label permutations leave metrics unchanged", {
        weights <- matrix(c(1,4,2,-3,3,2),2)
        x <- graphmode_dev_allocation(weights,c(1,3),c(2,3))
        shifted <- graphmode_dev_allocation(weights+c(30,-40),c(1,3),c(2,3))
        order <- c(3,1,2)
        permuted <- graphmode_dev_allocation(weights[,order],match(c(1,3),order),match(c(2,3),order))
        for (name in c("expected_node_moves","log_expected_node_moves","log_probability_no_node_moves",
                       "mean_max_probability","observed_node_moves")) {
            close(x[[name]], shifted[[name]]); close(x[[name]], permuted[[name]])
        }
    })
    test("one component is structurally immobile without a convergence claim", {
        x <- graphmode_dev_allocation(matrix(2,3,1),rep(1,3))
        assert(x$expected_node_moves==0 && x$log_expected_node_moves == -Inf &&
            x$log_probability_no_node_moves==0 && !x$convergence_certified)
    })
    test("invalid allocation weights or labels fail", {
        fails(graphmode_dev_allocation(matrix(NA_real_,2,2),1:2),"finite")
        fails(graphmode_dev_allocation(matrix(0,2,2),c(1,3)),"labels|between|1:K")
        fails(graphmode_dev_allocation(matrix(0,2,2),1:2,c(1,3)),"labels|between|1:K")
        fails(graphmode_dev_allocation(matrix(c(-1e308,1e308),1),1),"range")
    })
    ffbs_args <- list(precision=c(2,5,1), natural=c(-1,.2,2), Fmat=cbind(1,c(-1,.5,2)),
        m0=c(.4,-.2), C0=matrix(c(1.2,.2,.2,.8),2),
        G=matrix(c(.9,-.1,.2,.8),2), W=matrix(c(.3,.05,.05,.4),2))
    test("cached FFBS matches every original return field on fixed input", {
        assert(identical(do.call(graphmode_ffbs,ffbs_args), do.call(graphmode_dev_ffbs,ffbs_args)))
    })
    test("cached FFBS uses the same draw order and independent dense target", {
        p <- 2L; TT <- 3L; F <- ffbs_args$Fmat; C <- ffbs_args$C0; G <- ffbs_args$G; W <- ffbs_args$W
        prior_root <- matrix(0,p*TT,p*(TT+1L)); previous <- cbind(t(chol(C)),matrix(0,p,p*TT))
        prior_mean <- numeric(p*TT); design <- matrix(0,TT,p*TT); m <- ffbs_args$m0
        for(t in seq_len(TT)) {
            m <- G%*%m; pos <- (t-1L)*p+seq_len(p); prior_mean[pos] <- m
            previous <- G%*%previous; previous[,t*p+seq_len(p)] <- t(chol(W))
            prior_root[pos,] <- previous; design[t,pos] <- F[t,]
        }
        prior <- tcrossprod(prior_root)
        expected_cov <- solve(solve(prior)+crossprod(design,ffbs_args$precision*design))
        expected_mean <- as.numeric(expected_cov%*%(solve(prior,prior_mean)+crossprod(design,ffbs_args$natural)))
        probe <- function(tape, cached=TRUE) {
            cursor <- 0L
            patched(list(graphmode_normal=function(n) {
                out <- tape[cursor+seq_len(n)]; cursor <<- cursor+n; out
            }), {
                x <- do.call(if(cached) graphmode_dev_ffbs else graphmode_ffbs,ffbs_args)
                assert(cursor==p*TT); as.vector(t(x$theta))
            })
        }
        zero <- probe(numeric(p*TT)); close(zero,expected_mean)
        root <- vapply(seq_len(p*TT),function(i) {
            tape <- diag(p*TT)[,i]; value <- probe(tape)
            assert(identical(value,probe(tape,FALSE))); value-zero
        },numeric(p*TT))
        close(tcrossprod(root),expected_cov)
    })
    test("one-time-point FFBS needs no backward cache", {
        args <- ffbs_args; args$Fmat <- args$Fmat[1,,drop=FALSE]; args$precision <- 2; args$natural <- -1
        assert(identical(do.call(graphmode_dev_ffbs,args),do.call(graphmode_ffbs,args)))
    })
    test("W and G caches never leak across expert calls", {
        do.call(graphmode_dev_ffbs,ffbs_args)
        args <- ffbs_args; args$W <- 3*args$W; args$G <- .7*args$G
        assert(identical(do.call(graphmode_dev_ffbs,args),do.call(graphmode_ffbs,args)))
        assert(identical(do.call(graphmode_dev_ffbs,ffbs_args),do.call(graphmode_ffbs,ffbs_args)))
    })
    test("168-point cache removes 166 repeated evolution-root solves", {
        args <- ffbs_args; args$Fmat <- cbind(1,rep(c(-1,.5,2),56)); args$precision <- rep(2,168); args$natural <- rep(.1,168)
        original <- kernel$graphmode_whitener; W_root <- t(chol(args$W)); count <- 0L
        patched(list(graphmode_whitener=function(B) {if(identical(B,W_root)) count <<- count+1L; original(B)}), {
            baseline <- do.call(graphmode_ffbs,args); assert(count==167L)
            count <- 0L; cached <- do.call(graphmode_dev_ffbs,args); assert(count==1L)
            assert(identical(baseline,cached))
        })
    })
    test("cached FFBS still rejects the audited extreme precision before any draw", {
        patched(list(graphmode_normal=function(n) stop("unexpected draw")), {
            fails(graphmode_dev_ffbs(1e34,2e34,matrix(c(1,2,3),1),rep(0,3),diag(.5,3),diag(3),diag(.5,3)),
                "covariance-root accuracy")
        })
    })
    test("backward diagnostics still propagate through the original driver", {
        original <- kernel$graphmode_information_condition
        patched(list(graphmode_information_condition=function(mean,root,design,precision,natural) {
            out <- original(mean,root,design,precision,natural)
            if(nrow(design)==2L) {out$root_residual <- 5e-7; out$root_reciprocal_condition <- .01}
            out
        }), {
            x <- do.call(graphmode_dev_ffbs,ffbs_args)
            assert(x$root_residual==5e-7 && x$root_reciprocal_condition==.01)
        })
    })
    config <- graphmode_config(matrix(1:12,4,3),cbind(1,c(-1,0,1)),c(0,0),diag(2),
        "graphMoDE-W",K=3L,G=diag(2),W=diag(.2,2),Phi=diag(4),rho=2L,guidance_proposal_sd=.25)
    state <- graphmode_initial_state(config,c(1,1,2,2))
    clean_update <- function(x) {x$seconds <- 0; x}
    test("expert observation preserves accepted state and reports the actual proposal", {
        Y <- config$Y[state$Z==1,,drop=FALSE]; theta <- matrix(state$theta[1,,],3,2)
        a <- graphmode_update_expert(theta,Y,config)
        b <- graphmode_dev_expert(theta,Y,config,cache_ffbs=TRUE)
        assert(identical(clean_update(a),clean_update(b$update)))
        assert(identical(b$observation$log_acceptance,a$log_acceptance))
        close(sum(b$observation$correction_by_time),b$observation$log_ratio)
    })
    test("rejected proposal remains observable but never replaces accepted state", {
        cfg <- graphmode_config(matrix(1,1,4),matrix(1,4,1),0,matrix(1),"MoDE",K=1L,
            G=matrix(1),W=matrix(.2),rho=1L,dirichlet_alpha=1)
        fake <- function(...) list(theta=matrix(3,4,1),factor_residual=0,root_residual=0,root_reciprocal_condition=1)
        pg_calls <- 0L
        patched(list(graphmode_ffbs=fake,graphmode_pg=function(b,z){pg_calls <<- pg_calls+1L; rep(.25,length(b))}), {
            for(i in 1:2) {
                x <- graphmode_dev_expert(matrix(0,4,1),cfg$Y,cfg)
                assert(!x$update$accepted && identical(x$update$theta,matrix(0,4,1)))
                assert(x$observation$proposed_information_movement==36 && x$observation$accepted_information_movement==0)
            }
            assert(pg_calls==2L)
        })
    })
    test("empty expert prior refresh is retained and consumes no PG draw", {
        patched(list(graphmode_pg=function(...) stop("unexpected PG")), {
            theta <- matrix(state$theta[3,,],3,2); Y <- config$Y[FALSE,,drop=FALSE]
            a <- graphmode_update_expert(theta,Y,config)
            b <- graphmode_dev_expert(theta,Y,config,cache_ffbs=TRUE)
            assert(identical(clean_update(a),clean_update(b$update)) && is.null(b$observation))
            assert(b$update$empty && is.na(b$update$accepted) && b$update$seconds>=0)
        })
    })
    test("private dependency binding does not mutate the original function environment", {
        originals <- lapply(c("graphmode_ffbs","graphmode_update_expert","graphmode_sweep","graphmode4_controlled_sweep"),
            function(n) get(n,kernel))
        graphmode_dev_expert(matrix(state$theta[1,,],3,2),config$Y[1:2,,drop=FALSE],config,cache_ffbs=TRUE)
        current <- lapply(c("graphmode_ffbs","graphmode_update_expert","graphmode_sweep","graphmode4_controlled_sweep"),
            function(n) get(n,kernel))
        assert(identical(originals,current))
    })
    clean_step <- function(x) {x$out$expert_updates <- lapply(x$out$expert_updates,clean_update); x}
    test("complete development sweep matches original states and random-hook calls", {
        control <- graphmode4_guidance_control(config,4L); counts <- c(normal=0L,uniform=0L,pg=0L)
        patched(list(graphmode_normal=function(n){counts["normal"] <<- counts["normal"]+n; rep(.1,n)},
            graphmode_uniform=function(n){counts["uniform"] <<- counts["uniform"]+n; rep(.5,n)},
            graphmode_pg=function(b,z){counts["pg"] <<- counts["pg"]+length(b); rep(.25,length(b))}), {
            baseline <- graphmode4_controlled_sweep(state,config,control); original_counts <- counts
            for(cache in c(FALSE,TRUE)) for(observe in c(FALSE,TRUE)) {
                counts[] <- 0L
                x <- graphmode_dev_sweep(state,config,control,cache,observe)
                assert(identical(clean_step(baseline),clean_step(x$transition)))
                assert(identical(counts,original_counts) && !x$run_registered && !x$formal_authorized)
                assert(is.null(x$observation)==!observe)
            }
        })
    })
    test("existing guidance warmup adjusts next sweep and freezes before retention", {
        policy <- graphmode4_guidance_policy(2L,1L,.44,1,1,.6,.05,4)
        control <- graphmode4_guidance_control(config,4L,policy); s <- state
        for(i in 1:6) {
            old_sd <- control$sd
            x <- graphmode_dev_sweep(s,config,control,TRUE,TRUE)
            assert(x$transition$out$tuning$sd_used==old_sd)
            s <- x$transition$out$state; control <- x$transition$control
            if(i==2L) frozen_sd <- control$sd
            if(i>2L) assert(control$frozen && control$sd==frozen_sd && x$transition$out$tuning$frozen_used)
        }
        assert(control$updates==2L && config$guidance_proposal_sd==.25)
    })
    test("development observation includes allocation and guidance movement without selection", {
        x <- graphmode_dev_sweep(state,config,graphmode4_guidance_control(config,4L),TRUE)
        assert(length(x$observation$experts)==config$K && x$observation_seconds>=0)
        assert(x$observation$allocation$combined$observed_node_moves==x$transition$out$events[["node_moves"]])
        close(x$observation$guidance$logit_jump_squared,sum((x$transition$out$state$v-state$v)^2))
    })
    test("unsupported Potts/static development steps fail without stochastic hooks", {
        patched(list(graphmode_normal=function(...) stop("unexpected draw")), {
            bad <- config; bad$method <- "PottsMoDE"
            fails(graphmode_dev_allocation_parts(state,bad,state$Z),"Sequential Potts")
            bad <- config; bad$dynamics <- "static"
            fails(graphmode_dev_sweep(state,bad,list()),"dynamic Poisson")
            fails(graphmode_dev_sweep(state,config,list(),cache_ffbs=NA),"logical")
        })
    })
    test("MH profile distinguishes probability, observed decisions and empty priors", {
        e <- function(empty=FALSE,a=FALSE,la=-2) list(empty=empty,accepted=if(empty) NA else a,
            log_acceptance=if(empty) NA_real_ else la,movement=as.numeric(a&&!empty),seconds=.1)
        d <- lapply(1:4,function(i) list(iteration=i,expert=list(e(a=i==3),e(TRUE)),
            events=c(pair_changes=0),guidance_accept=TRUE))
        p <- graphmode_dev_mh_profile(d,1L)$experts
        close(p$mean_accept_probability[1],exp(-2)); assert(p$accepted[1]==1)
        assert(p$occupied_updates[2]==0 && is.na(p$mean_accept_probability[2]) && p$state_seconds[2]>.29)
        d[[4]]$expert[[1]]$log_acceptance <- .1
        fails(graphmode_dev_mh_profile(d,1L),"log MH")
    })
    test("read-only inspector rejects changed screen before following result pointers", {
        calls <- 0L
        patched(list(readRDS=function(path) {calls <<- calls+1L; list(schema="obsolete")}), {
            fails(graphmode_dev_inspect(graphmode_root),"changed screen")
            assert(calls==1L)
        })
    })
    test("inspection binds successful batch and report to the registered screen", {
        screen <- list(jobs=list(a=list(), b=list()), signature="screen")
        registration <- list(role="fixed evidence")
        registration$signature <- graphmode_digest(registration)
        batch <- list(status=0L,registration_signature=registration$signature)
        report <- list(registration_signature=registration$signature,screen_signature="screen",
            dispatched=c("a","b"),dispatch_status=c(a=0L,b=0L),stop_reason=NULL)
        assert(graphmode_dev_evidence_header(screen,registration,batch,report))
        changed <- registration; changed$role <- "changed"
        fails(graphmode_dev_evidence_header(screen,changed,batch,report),"mismatched")
        failed <- batch; failed$status <- 124L
        fails(graphmode_dev_evidence_header(screen,registration,failed,report),"mismatched")
        for(field in c("screen_signature","dispatched","dispatch_status","stop_reason")) {
            changed <- report; changed[[field]] <- "changed"
            fails(graphmode_dev_evidence_header(screen,registration,batch,changed),"mismatched")
        }
    })
    after_rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    assert(identical(before_rng,after_rng) && identical(before_kind,RNGkind()))
    cat(sprintf("Passed %d D-043 fixed-input groups; RNG unchanged; no scientific run.\n", checks))
})
