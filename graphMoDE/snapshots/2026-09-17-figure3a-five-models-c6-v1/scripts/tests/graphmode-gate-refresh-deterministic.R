# New D-050 tests only: fixed tapes, algebra and routing. No real PG/MCMC run.
if (!exists("graphmode_root", inherits = FALSE)) {
    this <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)), mustWork = TRUE)
    graphmode_root <- dirname(dirname(dirname(this)))
}
local({
    kernel <- new.env(parent = globalenv())
    files <- c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$")),
        "graphmode_dev.R", "graphmode_gate_refresh.R")
    before_kind <- RNGkind()
    before_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    for (f in files) sys.source(file.path(graphmode_root, "R", f), kernel)
    test_environment <- environment()
    parent.env(test_environment) <- kernel
    checks <- 0L
    assert <- function(ok) if (!isTRUE(ok)) stop("Fixed gate-refresh assertion failed.", call. = FALSE)
    close <- function(x, y) assert(length(x) == length(y) && identical(dim(x), dim(y)) &&
        all(is.finite(c(x, y))) && max(abs(x-y), 0) < 1e-12)
    fails <- function(code) assert(inherits(tryCatch({force(code); NULL}, error = identity), "error"))
    patched <- function(bindings, code) {
        old <- mget(names(bindings), kernel)
        on.exit(list2env(old, kernel), add = TRUE)
        list2env(bindings, kernel); force(code)
    }
    # Separate deterministic tapes. Reinitializing them proves the m=1 path
    # uses exactly the old draw order as well as returning the same state.
    tape <- function(code) {
        used <- c(normal = 0L, uniform = 0L, pg = 0L, gamma = 0L)
        patched(list(
            graphmode_normal = function(n) {
                i <- used["normal"] + seq_len(n); used["normal"] <<- used["normal"] + n
                .12 * sin(i / 5)
            },
            graphmode_uniform = function(n) {
                i <- used["uniform"] + seq_len(n); used["uniform"] <<- used["uniform"] + n
                .25 + .5 * (i %% 13) / 13
            },
            graphmode_pg = function(b,z) { used["pg"] <<- used["pg"] + length(b); rep(.25,length(b)) },
            graphmode_gamma = function(shape,rate) { used["gamma"] <<- used["gamma"] + length(shape); shape/rate }), {
                answer <- force(code); list(value = answer, used = used)
            })
    }
    test <- function(name, code) { force(code); checks <<- checks + 1L; cat("OK", checks, name, "\n") }
    config <- graphmode_config(matrix(1:12,4,3), cbind(1,c(-1,0,1)), c(0,0), diag(2),
        "graphMoDE-W", K=3L, G=diag(2), W=diag(.02,2), Phi=diag(4),
        guidance_proposal_sd=.4, rho=4L)
    initial <- graphmode_initial_state(config, c(1L,2L,1L,3L), array(0,c(3,3,2)),
        x=.05*sin(seq_len(graphmode_gate_dimension(config))), v=c(-.3,.1,.5))
    p1 <- graphmode_gate_refresh_policy(1L,.4)
    p4 <- graphmode_gate_refresh_policy(4L,.4)
    strip_time <- function(out) {
        if (!is.null(out$expert_updates)) out$expert_updates <- lapply(out$expert_updates, function(x) {x$seconds <- NULL; x})
        out
    }

    test("invalid or unregistered policy changes fail before draws", {
        for (n in c(0,1.5,65,NA_real_)) fails(graphmode_gate_refresh_policy(n,.4))
        for (s in c(0,-1,Inf,NA_real_)) fails(graphmode_gate_refresh_policy(1,s))
        bad <- p4; bad$adaptation <- "automatic"
        fails(graphmode_gate_refresh(initial$x,initial$v,initial$Z,config,bad))
        fails(graphmode_gate_refresh(initial$x,initial$v,initial$Z,config,graphmode_gate_refresh_policy(4,.5)))
    })
    test("one inner pass exactly matches original gate pair and draw order", {
        old <- tape({g <- graphmode_gate_ess(initial$x,initial$v,initial$Z,config)
            u <- graphmode_guidance_update(g$x,initial$v,initial$Z,config); list(x=g$x,u=u,e=g$evaluations)})
        new <- tape(graphmode_gate_refresh(initial$x,initial$v,initial$Z,config,p1))
        assert(identical(old$used,new$used)); assert(identical(old$value$x,new$value$x))
        assert(identical(old$value$u$v,new$value$v)); assert(identical(old$value$u$utilities,new$value$utilities))
        assert(identical(old$value$u$accepted,new$value$records[[1]]$accepted))
        assert(old$value$e == new$value$counts$ess_evaluations)
    })
    test("four passes equal a manual sequential composition with fixed Z", {
        old <- tape({x <- initial$x; v <- initial$v
            for (j in 1:4) {g <- graphmode_gate_ess(x,v,initial$Z,config)
                u <- graphmode_guidance_update(g$x,v,initial$Z,config); x <- g$x; v <- u$v}
            list(x=x,v=v,utilities=u$utilities)})
        new <- tape(graphmode_gate_refresh(initial$x,initial$v,initial$Z,config,p4))
        assert(identical(old$used,new$used)); assert(identical(old$value,new$value[c("x","v","utilities")]))
        assert(new$value$counts$total_proposals == 12 && length(new$value$records) == 4)
        assert(!new$value$inner_states_are_retained_draws)
    })
    test("m=1 full outer sweep matches original scientific state and draw order", {
        old <- tape(graphmode_sweep(initial,config))
        new <- tape(graphmode_gate_refresh_sweep(initial,config,p1,cache_ffbs=FALSE))
        assert(identical(old$used,new$used)); assert(identical(old$value$state,new$value$transition$state))
        reference <- old$value; reference$guidance_accept <- reference$ess_evaluations <- NULL
        assert(identical(strip_time(reference),strip_time(new$value$transition)))
        assert(identical(old$value$guidance_accept,new$value$gate_refresh$records[[1]]$accepted))
    })
    test("extra refreshes do not repeat expensive experts or PG draws", {
        one <- tape(graphmode_gate_refresh_sweep(initial,config,p1,cache_ffbs=FALSE))
        four <- tape(graphmode_gate_refresh_sweep(initial,config,p4,cache_ffbs=FALSE))
        assert(one$used["pg"] == four$used["pg"])
        assert(four$value$expert_update_count == config$K && four$value$completed_outer_steps == 1L)
        assert(four$value$transition$state$iteration == 1L)
        assert(four$value$gate_refresh$counts$total_proposals == 12)
        assert(four$used["normal"] > one$used["normal"])
    })
    test("categorical allocation is called once with final refreshed utilities", {
        original <- kernel$graphmode_categorical; calls <- 0L; weights <- NULL
        patched(list(graphmode_categorical=function(log_weights) {
            calls <<- calls+1L; weights <<- log_weights; original(log_weights)
        }), {
            z <- tape(graphmode_gate_refresh_sweep(initial,config,p4,cache_ffbs=FALSE))$value
            assert(calls == 1L)
            close(weights,graphmode_response_loglik(config$Y,z$transition$eta,config$family,NULL) + z$transition$utilities)
            assert(identical(z$transition$utilities,graphmode_gate_utilities(z$transition$state$x,z$transition$state$v,config)))
        })
    })
    test("cache switch preserves refreshed full state and draw sequence", {
        a <- tape(graphmode_gate_refresh_sweep(initial,config,p4,cache_ffbs=FALSE))
        b <- tape(graphmode_gate_refresh_sweep(initial,config,p4,cache_ffbs=TRUE))
        assert(identical(a$used,b$used)); assert(identical(a$value$transition$state,b$value$transition$state))
        assert(identical(strip_time(a$value$transition),strip_time(b$value$transition)))
    })
    test("existing rejected-proposal observations are retained with original MH diagnostics", {
        z <- tape(graphmode_gate_refresh_sweep(initial,config,p4,cache_ffbs=TRUE))$value
        assert(length(z$expert_observations)==config$K)
        for(k in seq_len(config$K)) {
            o <- z$expert_observations[[k]]$observation
            assert(!is.null(o))
            assert(identical(o$log_acceptance,z$transition$expert_updates[[k]]$log_acceptance))
            close(sum(o$correction_by_time),o$log_ratio)
        }
    })
    test("new envelope cannot silently expose old one-pass acceptance fields", {
        z <- tape(graphmode_gate_refresh_sweep(initial,config,p4,cache_ffbs=TRUE))$value
        assert(is.null(z$out) && is.null(z$control))
        assert(is.null(z$transition$guidance_accept) && is.null(z$transition$ess_evaluations))
        assert(!z$run_registered && !z$convergence_certified && !z$formal_authorized)
        assert(!z$gate_refresh$inner_states_are_retained_draws)
    })
    test("fixed Z is retained across every inner ESS and MH, including later states", {
        ess <- kernel$graphmode_gate_ess; mh <- kernel$graphmode_guidance_update
        seen <- list()
        patched(list(graphmode_gate_ess=function(x,v,Z,config) {
            seen[[length(seen)+1L]] <<- list(block="x",Z=Z,x=x,v=v); ess(x,v,Z,config)
        },graphmode_guidance_update=function(x,v,Z,config) {
            seen[[length(seen)+1L]] <<- list(block="v",Z=Z,x=x,v=v); mh(x,v,Z,config)
        }), {tape(graphmode_gate_refresh(initial$x,initial$v,initial$Z,config,p4))})
        assert(length(seen) == 8L)
        assert(all(vapply(seen,function(s) identical(s$Z,initial$Z),logical(1))))
        assert(identical(vapply(seen,`[[`,character(1),"block"),rep(c("x","v"),4)))
        assert(!identical(seen[[1]]$x,seen[[3]]$x))
    })
    test("all passes counted, not just last pass or any-accept probability", {
        rows <- list(c(TRUE,FALSE,FALSE),c(FALSE,TRUE,FALSE),c(FALSE,FALSE,TRUE),c(TRUE,FALSE,FALSE))
        records <- lapply(1:4,function(j) list(inner_step=j,accepted=rows[[j]],
            proposals=rep(1L,3),ess_evaluations=as.integer(j)))
        d <- graphmode_gate_refresh_counts(records,p4,3)
        close(d$accepted_by_coordinate,c(2,1,1)); close(d$proposals_by_coordinate,c(4,4,4))
        close(d$acceptance,1/3); close(d$acceptance_by_coordinate,c(.5,.25,.25)); assert(d$ess_evaluations==10)
        records[[4]]$proposals[1] <- 0L; fails(graphmode_gate_refresh_counts(records,p4,3))
        fails(graphmode_gate_refresh_counts(records[1:3],p4,3))
    })
    test("actual MH all-rejection tape still completes exactly the fixed pass count", {
        patched(list(graphmode_normal=function(n) rep(50,n),graphmode_uniform=function(n) rep(.5,n)), {
            z <- graphmode_gate_refresh(initial$x,initial$v,initial$Z,config,p4)
            assert(z$counts$total_accepted==0 && z$counts$total_proposals==12)
            assert(length(z$records)==4 && identical(z$v,initial$v))
        })
    })
    test("accepted round trips retain substep movement despite zero net displacement", {
        calls <- 0L; policy <- graphmode_gate_refresh_policy(2,.4)
        patched(list(graphmode_guidance_update=function(x,v,Z,config) {
            calls <<- calls+1L; v <- v+if(calls==1L) .125 else -.125
            list(v=v,utilities=graphmode_gate_utilities(x,v,config),accepted=rep(TRUE,length(v)))
        }), {
            z <- tape(graphmode_gate_refresh(initial$x,initial$v,initial$Z,config,policy))$value
            close(z$v,initial$v); assert(z$counts$total_accepted==6)
            assert(all(vapply(z$records,function(r) sum(r$logit_jump_squared)>0,logical(1))))
        })
    })
    test("shared binary guidance counts one coordinate per pass", {
        cfg <- graphmode_config(config$Y,config$Fmat,config$m0,config$C0,"graphMoDE-W",K=2L,
            G=config$G,W=config$W,Phi=config$Phi,guidance_proposal_sd=.4,rho=4L)
        s <- graphmode_initial_state(cfg,c(1L,2L,1L,2L),array(0,c(2,3,2)))
        z <- tape(graphmode_gate_refresh(s$x,s$v,s$Z,cfg,p4))$value
        assert(length(z$v)==1 && z$counts$total_proposals==4 && length(z$counts$acceptance_by_coordinate)==1)
    })
    test("K=10 includes empty-label guidance, never redraws it from its prior", {
        cfg <- graphmode_config(config$Y,config$Fmat,config$m0,config$C0,"graphMoDE-W",K=10L,
            G=config$G,W=config$W,Phi=config$Phi,guidance_proposal_sd=.4,rho=4L)
        s <- graphmode_initial_state(cfg,rep(1L,4),array(0,c(10,3,2)))
        z <- tape(graphmode_gate_refresh(s$x,s$v,s$Z,cfg,p4))
        assert(z$value$counts$total_proposals==40 && length(z$value$v)==10)
        assert(z$used["gamma"]==0 && z$used["pg"]==0)
    })
    test("unsupported fixed-endpoint gate rejected without sampling", {
        cfg <- graphmode_config(config$Y,config$Fmat,config$m0,config$C0,"graphMoDE-W",K=3L,
            G=config$G,W=config$W,Phi=config$Phi,guidance="forced",guidance_proposal_sd=.4,rho=4L)
        fails(graphmode_gate_refresh_validate(cfg,p4))
    })
    test("invalid state, missing cache flag or changed scale rejected", {
        bad <- initial; bad$x[1] <- NA_real_
        fails(graphmode_gate_refresh_sweep(bad,config,p4,TRUE))
        fails(graphmode_gate_refresh_sweep(initial,config,p4))
        bad <- initial; bad$iteration <- -1L; fails(graphmode_gate_refresh_sweep(bad,config,p4,TRUE))
        cfg <- config; cfg$guidance_proposal_sd <- .5
        fails(graphmode_gate_refresh_sweep(initial,cfg,p4,TRUE))
    })
    test("a failed inner step stops, with no retry or caller-state mutation", {
        old <- serialize(initial,NULL); calls <- 0L; ess <- kernel$graphmode_gate_ess
        patched(list(graphmode_gate_ess=function(...) {calls <<- calls+1L
            if(calls==2L) stop("fixed injected failure"); ess(...)}), {
            fails(tape(graphmode_gate_refresh_sweep(initial,config,p4,TRUE)))
        })
        assert(calls==2L && identical(serialize(initial,NULL),old))
    })
    test("rejected guidance cannot change its coordinate", {
        patched(list(graphmode_guidance_update=function(x,v,Z,config) {
            v <- v+.1; list(v=v,utilities=graphmode_gate_utilities(x,v,config),accepted=rep(FALSE,length(v)))
        }), {fails(tape(graphmode_gate_refresh(initial$x,initial$v,initial$Z,config,p4)))})
    })
    test("finite-state algebra: repeated conditional kernels preserve target", {
        # Independent exact transition-matrix check, not samples or MCMC evidence.
        states <- expand.grid(x=0:1,v=0:1,z=0:1)
        pi <- c(1,3,2,8,5,2,7,4); pi <- pi/sum(pi)
        conditional <- function(column) {
            P <- matrix(0,8,8)
            keep <- setdiff(names(states),column)
            for(i in 1:8) {
                idx <- which(apply(states[,keep,drop=FALSE],1,function(s) all(s==as.numeric(states[i,keep]))))
                P[i,idx] <- pi[idx]/sum(pi[idx])
            }; P
        }
        X <- conditional("x"); V <- conditional("v"); Z <- conditional("z")
        joint <- X%*%V; P <- diag(8)
        for(m in 1:64) {
            P <- P%*%joint
            close(as.numeric(pi%*%P),pi)
            close(as.numeric(pi%*%(P%*%Z)),pi)
            close(rowSums(P),rep(1,8))
        }
    })
    test("original functions and all 55 frozen B files remain unchanged", {
        for (name in c("graphmode_sweep","graphmode_gate_ess","graphmode_guidance_update"))
            assert(identical(environment(get(name,kernel)),kernel))
        registration_path <- file.path(dirname(graphmode_root),"countDLM-local-results",
            "graphmode-validation-dispersed-20260914-b1","registration.rds")
        # Optional read-only historical identity check; no outputs/checkpoints loaded.
        if(file.exists(registration_path)) {
            old <- readRDS(registration_path)$identity$sha256
            got <- vapply(file.path(graphmode_root,names(old)),function(f) digest::digest(file=f,algo="sha256"),character(1))
            assert(length(old)==55L && identical(unname(old),unname(got)))
        }
    })
    test("RNG kind/state unchanged across all fixed-input checks", {
        after <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
        assert(identical(RNGkind(),before_kind) && identical(after,before_seed))
    })
    cat("Passed",checks,"D-050 fixed-input groups. No statistical performance or convergence claim.\n")
})
