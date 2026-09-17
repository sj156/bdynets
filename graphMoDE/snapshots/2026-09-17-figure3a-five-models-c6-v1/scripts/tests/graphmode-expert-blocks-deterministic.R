# D-055: algebra, dense Gaussian references and fixed tapes; no real PG/MCMC.
if (!exists("graphmode_root", inherits = FALSE)) {
    entry <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
    graphmode_root <- dirname(dirname(dirname(normalizePath(entry, mustWork = TRUE))))
}
local({
    kernel <- new.env(parent = globalenv())
    before_kind <- RNGkind()
    before_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    files <- c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$")),
        "graphmode_dev.R", "graphmode_gate_refresh.R", "graphmode_expert_blocks.R")
    for (f in files) sys.source(file.path(graphmode_root, "R", f), kernel)
    test_environment <- environment()
    parent.env(test_environment) <- kernel
    checks <- 0L
    assert <- function(ok) if (!isTRUE(ok)) stop("D-055 assertion: ",
        paste(deparse(substitute(ok)), collapse = " "), call. = FALSE)
    close <- function(x, y, tol = 1e-10) {
        assert(identical(dim(x), dim(y)) && length(x) == length(y))
        assert(all(is.finite(c(x, y))) && max(abs(x-y), 0) <= tol * max(1, abs(y)))
    }
    fails <- function(code, pattern = NULL) {
        e <- tryCatch({force(code); NULL}, error = identity)
        assert(inherits(e, "error"))
        if (!is.null(pattern)) assert(grepl(pattern, conditionMessage(e)))
    }
    patched <- function(bindings, code) {
        old <- mget(names(bindings), kernel)
        on.exit(list2env(old, kernel), add = TRUE)
        list2env(bindings, kernel); force(code)
    }
    tape <- function(code) {
        used <- c(normal = 0L, uniform = 0L, pg = 0L)
        patched(list(graphmode_normal = function(n) {
            i <- used["normal"] + seq_len(n); used["normal"] <<- used["normal"] + n
            .15 * sin(i / 3)
        }, graphmode_uniform = function(n) {
            i <- used["uniform"] + seq_len(n); used["uniform"] <<- used["uniform"] + n
            .2 + .6 * (i %% 11) / 11
        }, graphmode_pg = function(b, z) {
            used["pg"] <<- used["pg"] + length(b); rep(.4, length(b))
        }), {answer <- force(code); list(value = answer, used = used)})
    }
    test <- function(name, code) {
        # A missed tape must fail, not silently make a scientific draw.
        patched(list(graphmode_normal = function(...) stop("Unexpected random normal"),
            graphmode_uniform = function(...) stop("Unexpected random uniform"),
            graphmode_pg = function(...) stop("Unexpected real PG"),
            graphmode_gamma = function(...) stop("Unexpected random gamma")), force(code))
        checks <<- checks + 1L; cat("OK", checks, name, "\n")
    }
    config <- graphmode_config(matrix(c(1,2,4,3,2,5,1,2,3,4,2,1,3,2,4),3,5),
        cbind(1, seq(-.5,.5,length.out=5)), c(.2,-.1), matrix(c(.6,.1,.1,.4),2),
        "graphMoDE-W", K=3L, G=matrix(c(.9,-.2,.1,.8),2),
        W=matrix(c(.12,.02,.02,.08),2), Phi=diag(3), guidance_proposal_sd=.4, rho=4L)
    initial <- graphmode_initial_state(config, c(1L,1L,2L),
        array(.08*cos(seq_len(30)),c(3,5,2)),
        x=.05*sin(seq_len(graphmode_gate_dimension(config))), v=c(-.2,.1,.4))
    policy <- graphmode_expert_blocks_policy(2L,"fixed-zero")
    full <- graphmode_expert_blocks_policy(5L,"fixed-zero")
    gp <- graphmode_gate_refresh_policy(4L,.4)

    # Independent joint precision for theta_1:T, integrating theta_0 once.
    # Only small well-conditioned fixtures use dense inverses in tests.
    dense <- function(F, precision, natural, m0, C0, G, W, path, begin, end) {
        TT <- nrow(F); p <- ncol(F); q <- TT*p
        J <- matrix(0,q,q); h <- numeric(q)
        index <- function(t) (t-1L)*p+seq_len(p)
        first <- solve(G %*% C0 %*% t(G) + W)
        J[index(1),index(1)] <- first; h[index(1)] <- first %*% G %*% m0
        V <- solve(W)
        if(TT>1) for(t in 2:TT) {
            a <- index(t-1); b <- index(t)
            J[a,a] <- J[a,a] + t(G)%*%V%*%G
            J[a,b] <- J[a,b] - t(G)%*%V; J[b,a] <- J[b,a] - V%*%G
            J[b,b] <- J[b,b] + V
        }
        for(t in 1:TT) {
            a <- index(t); J[a,a] <- J[a,a] + precision[t]*tcrossprod(F[t,])
            h[a] <- h[a] + natural[t]*F[t,]
        }
        B <- unlist(lapply(begin:end,index)); O <- setdiff(seq_len(q),B)
        covariance <- solve(J[B,B,drop=FALSE])
        rhs <- h[B]
        if(length(O)) rhs <- rhs - J[B,O,drop=FALSE]%*%as.vector(t(path))[O]
        list(mean=as.numeric(covariance%*%rhs),covariance=covariance)
    }
    # Reconstruct the complete Gaussian draw affine map using zero and basis
    # normal tapes; this is an exact finite calculation, not sampled moments.
    affine <- function(F, precision, natural, m0, C0, G, W, left, right, cache) {
        q <- nrow(F)*ncol(F)
        draw <- function(z) {
            pos <- 0L
            patched(list(graphmode_normal=function(n) {
                at <- pos+seq_len(n); pos <<- pos+n
                assert(pos<=length(z)); z[at]
            }), {
                x <- graphmode_expert_block_ffbs(precision,natural,F,m0,C0,G,W,left,right,cache)
                assert(pos==q); as.vector(t(x$theta))
            })
        }
        mean <- draw(numeric(q))
        root <- vapply(seq_len(q),function(j) draw(diag(q)[,j])-mean,numeric(q))
        dim(root) <- c(q,q)
        list(mean=mean,covariance=tcrossprod(root))
    }

    test("all offsets cover every time exactly once without circular transitions", {
        for(TT in 1:12) for(width in 1:TT) for(off in if(width==TT) 0L else 0:(width-1L)) {
            b <- graphmode_expert_blocks_layout(TT,width,off)
            assert(identical(unlist(Map(seq.int,b$start,b$end),use.names=FALSE),seq_len(TT)))
            assert(all(b$end-b$start+1<=width))
        }
    })
    test("malformed policies and unsupported time ranges fail before draws", {
        for(width in c(0,NA_real_,1.5,-1)) fails(graphmode_expert_blocks_policy(width,"uniform"))
        fails(graphmode_expert_blocks_policy(2,"until-accepted"))
        bad <- policy; bad$adaptation <- "online"
        fails(graphmode_expert_blocks_validate(config,bad))
        fails(graphmode_expert_blocks_validate(config,graphmode_expert_blocks_policy(6,"uniform")))
        fails(graphmode_expert_blocks_layout(5,2,2)); fails(graphmode_expert_blocks_layout(5,5,1))
    })
    test("full-path FFBS is exactly the existing cached/uncached calculation", {
        for(cache in c(FALSE,TRUE)) {
            f <- if(cache) graphmode_dev_ffbs else graphmode_ffbs
            old <- tape(f(rep(.3,5),seq(-.2,.2,length.out=5),config$Fmat,
                config$m0,config$C0,config$G,config$W))
            new <- tape(graphmode_expert_block_ffbs(rep(.3,5),seq(-.2,.2,length.out=5),
                config$Fmat,config$m0,config$C0,config$G,config$W,NULL,NULL,cache))
            assert(identical(old,new))
        }
    })
    test("all 15 contiguous Gaussian block means/covariances match independent joint conditioning", {
        path <- matrix(seq(-.4,.5,length.out=10),5,2)
        precision <- c(.2,0,.4,.1,.7); natural <- c(.3,0,-.2,.1,.6)
        for(begin in 1:5) for(end in begin:5) {
            idx <- begin:end
            reference <- dense(config$Fmat,precision,natural,config$m0,config$C0,
                config$G,config$W,path,begin,end)
            for(cache in c(FALSE,TRUE)) {
                actual <- affine(config$Fmat[idx,,drop=FALSE],precision[idx],natural[idx],
                    config$m0,config$C0,config$G,config$W,
                    if(begin>1) path[begin-1,] else NULL,if(end<5) path[end+1,] else NULL,cache)
                close(actual$mean,reference$mean);close(actual$covariance,reference$covariance)
            }
        }
    })
    test("scalar and one-time bridges retain both boundary terms", {
        F <- matrix(1,3,1); path <- matrix(c(-.4,.8,.2),3,1)
        reference <- dense(F,c(.2,.6,.8),c(.1,.3,-.2),.3,matrix(.5),matrix(.7),matrix(.2),path,2,2)
        actual <- affine(F[2,,drop=FALSE],.6,.3,.3,matrix(.5),matrix(.7),matrix(.2),-.4,.2,FALSE)
        close(actual$mean,reference$mean);close(actual$covariance,reference$covariance)
        single <- affine(matrix(1),0,0,.3,matrix(.5),matrix(.7),matrix(.2),NULL,NULL,FALSE)
        close(single$mean,.21);close(single$covariance,matrix(.7^2*.5+.2))
    })
    test("singular G is permitted; boundary implementation never inverts G", {
        G <- matrix(c(1,0,.2,0),2); path <- matrix(.1,5,2)
        r <- dense(config$Fmat,rep(.2,5),rep(.1,5),config$m0,config$C0,G,config$W,path,2,4)
        a <- affine(config$Fmat[2:4,,drop=FALSE],rep(.2,3),rep(.1,3),
            config$m0,config$C0,G,config$W,path[1,],path[5,],TRUE)
        close(a$mean,r$mean);close(a$covariance,r$covariance)
    })
    test("left boundary uses W once and right boundary is not omitted", {
        z <- graphmode_expert_block_filter(0,0,matrix(1),0,matrix(100),matrix(.8),matrix(.2),2,NULL)
        close(z$a[1,],1.6);close(tcrossprod(matrix(z$root[,,1])),matrix(.2))
        r <- graphmode_expert_block_filter(0,0,matrix(1),0,matrix(100),matrix(.8),matrix(.2),2,-2)
        assert(abs(r$m[1,]-z$m[1,])>.1)
        assert(is.finite(r$root_reciprocal_condition) && r$root_reciprocal_condition>0)
    })
    test("168-time three-dimensional bridge mean and numerical audits match a dense reference", {
        F <- cbind(1,sin(seq_len(170)/11),cos(seq_len(170)/17))
        G <- matrix(c(.95,.04,0,-.02,.9,.03,.01,0,.85),3)
        W <- matrix(c(.08,.01,0,.01,.06,.005,0,.005,.04),3)
        C0 <- diag(c(.4,.3,.2));m0 <- c(.2,-.1,.05)
        path <- matrix(.2*sin(seq_len(510)/7),170,3)
        precision <- .1+(seq_len(170)%%9)/20;natural <- .2*cos(seq_len(170)/13)
        reference <- dense(F,precision,natural,m0,C0,G,W,path,2,169)
        values <- lapply(c(FALSE,TRUE),function(cache) {
            patched(list(graphmode_normal=function(n) numeric(n)), {
                a <- graphmode_expert_block_ffbs(precision[2:169],natural[2:169],F[2:169,],
                    m0,C0,G,W,path[1,],path[170,],cache)
                close(as.vector(t(a$theta)),reference$mean)
                assert(is.finite(a$root_residual) && a$root_residual<1e-10)
                assert(is.finite(a$root_reciprocal_condition) && a$root_reciprocal_condition>1e-6)
                a
            })
        })
        assert(identical(values[[1]],values[[2]]))
    })
    test("boundary and zero-precision validation precede Gaussian draws", {
        fails(graphmode_expert_block_ffbs(1,0,matrix(1),0,matrix(1),matrix(1),matrix(.1),NA,NULL,FALSE),"boundary")
        fails(graphmode_expert_block_ffbs(0,1,matrix(1),0,matrix(1),matrix(1),matrix(.1),0,0,FALSE),"information")
        fails(graphmode_expert_block_ffbs(1,0,matrix(1),0,matrix(1),matrix(1),matrix(.1),0,0,NA),"switch")
    })
    test("right-boundary numerical failure is not bypassed or silently repaired", {
        patched(list(graphmode_backward_condition=function(...) stop("fixed boundary audit failure")), {
            fails(graphmode_expert_block_ffbs(rep(.2,2),rep(.1,2),matrix(1,2,1),0,
                matrix(1),matrix(1),matrix(.1),0,1,FALSE),"boundary audit")
        })
    })
    test("full-block outer state and draw order equal D-050 for m1/m4 and both caches", {
        for(m in c(1L,4L)) for(cache in c(FALSE,TRUE)) {
            g <- graphmode_gate_refresh_policy(m,.4)
            a <- tape(graphmode_gate_refresh_sweep(initial,config,g,cache))
            b <- tape(graphmode_expert_blocks_sweep(initial,config,g,full,cache))
            assert(identical(a$used,b$used));assert(identical(a$value$transition$state,b$value$transition$state))
            assert(identical(a$value$gate_refresh$counts,b$value$gate_refresh$counts))
        }
    })
    test("cached/uncached conditional blocks preserve full state and draw counts", {
        a <- tape(graphmode_expert_blocks_sweep(initial,config,gp,policy,FALSE))
        b <- tape(graphmode_expert_blocks_sweep(initial,config,gp,policy,TRUE))
        assert(identical(a$used,b$used));assert(identical(a$value$transition$state,b$value$transition$state))
        assert(a$value$completed_outer_steps==1L && a$value$expert_update_count==3L)
    })
    test("every occupied time gets one PG draw, empty expert one full prior draw", {
        calls <- 0L; original <- kernel$graphmode_prior_expert
        patched(list(graphmode_prior_expert=function(...) {calls <<- calls+1L;original(...)}), {
            b <- tape(graphmode_expert_blocks_sweep(initial,config,gp,policy,FALSE))
            assert(calls==1L && b$used["pg"]==10L)
            e <- b$value$transition$expert_updates
            assert(e[[1]]$counts$proposals==3L && e[[2]]$counts$proposals==3L)
            assert(e[[3]]$counts$proposals==0L && e[[3]]$counts$prior_refreshes==1L)
            assert(is.na(e[[3]]$counts$acceptance) && is.na(e[[3]]$root_reciprocal_condition))
        })
    })
    test("new block schema cannot report any-accept as a whole-path MH rate", {
        b <- tape(graphmode_expert_blocks_sweep(initial,config,gp,policy,TRUE))$value
        assert(b$schema==graphmode_expert_blocks_version && !b$run_registered && !b$formal_authorized)
        assert(!b$block_states_are_retained_draws && !b$convergence_certified)
        for(e in b$transition$expert_updates) {
            assert(is.null(e$accepted) && is.null(e$log_acceptance))
            assert(e$counts$accepted==sum(vapply(e$blocks,`[[`,logical(1),"accepted")))
        }
    })
    test("one final allocation uses all finished experts and final refreshed utilities", {
        calls <- 0L; weights <- NULL; original <- kernel$graphmode_categorical
        patched(list(graphmode_categorical=function(log_weights) {
            calls <<- calls+1L;weights <<- log_weights;original(log_weights)
        }), {
            b <- tape(graphmode_expert_blocks_sweep(initial,config,gp,policy,FALSE))$value
            assert(calls==1L && b$transition$state$iteration==1L)
            close(weights,graphmode_response_loglik(config$Y,b$transition$eta,"poisson")+b$transition$utilities)
            assert(b$gate_refresh$counts$total_proposals==12L)
        })
    })
    test("uniform offsets have equal-width intervals and are drawn once per outer sweep", {
        pol <- graphmode_expert_blocks_policy(3L,"uniform")
        for(j in 0:2) patched(list(graphmode_uniform=function(n) rep((j+.5)/3,n)), {
            assert(graphmode_expert_blocks_offset(5,pol)==j)
        })
        calls <- 0L; original <- kernel$graphmode_expert_blocks_offset
        patched(list(graphmode_expert_blocks_offset=function(TT,policy) {
            calls <<- calls+1L; original(TT,policy)
        }), {
            b <- tape(graphmode_expert_blocks_sweep(initial,config,gp,pol,FALSE))$value
            assert(calls==1L)
            assert(all(vapply(b$expert_observations,function(x) identical(x$observation$offset,b$block_offset),logical(1))))
        })
    })
    test("full-path and unit-width policies consume no offset random number", {
        assert(graphmode_expert_blocks_offset(5,graphmode_expert_blocks_policy(5,"uniform"))==0L)
        assert(graphmode_expert_blocks_offset(5,graphmode_expert_blocks_policy(1,"uniform"))==0L)
        patched(list(graphmode_uniform=function(n) rep(1,n)), {
            fails(graphmode_expert_blocks_offset(5,graphmode_expert_blocks_policy(2,"uniform")),"offset draw")
        })
    })
    test("invalid initial state fails before selecting an offset", {
        bad <- initial;bad$theta[1,1,1] <- NA_real_
        fails(graphmode_expert_blocks_sweep(bad,config,gp,graphmode_expert_blocks_policy(2,"uniform"),FALSE),"initial state")
    })
    test("sequential blocks use newly accepted left states and preserve rejected slices", {
        cfg <- graphmode_config(matrix(1,1,7),matrix(1,7,1),0,matrix(1),"MoDE",K=1L,
            G=matrix(1),W=matrix(.1),rho=4L,dirichlet_alpha=1)
        cur <- matrix(0,7,1); draws <- 0L;pg_calls <- 0L
        patched(list(graphmode_uniform=function(n) {draws <<- draws+1L;c(0,.999,0)[draws]},
            graphmode_pg=function(b,z) {pg_calls <<- pg_calls+1L;rep(.4,length(b))},
            graphmode_expert_block_ffbs=function(precision,natural,Fmat,m0,C0,G,W,left,right,cache_ffbs)
                list(theta=matrix(5,nrow(Fmat),1),factor_residual=0,root_residual=0,root_reciprocal_condition=1)), {
            b <- graphmode_expert_blocks_expert(cur,cfg$Y,cfg,graphmode_expert_blocks_policy(3,"fixed-zero"),0,FALSE)
            assert(identical(b$update$theta,matrix(c(5,5,5,0,0,0,5),7,1)))
            assert(is.null(b$update$blocks[[1]]$left))
            close(b$update$blocks[[2]]$left,5);close(b$update$blocks[[3]]$left,0)
            close(b$update$blocks[[1]]$right,0);assert(is.null(b$update$blocks[[3]]$right))
            assert(b$update$counts$accepted==2 && b$update$counts$proposals==3 && pg_calls==3)
            close(b$update$counts$acceptance,2/3)
        })
        assert(identical(cur,matrix(0,7,1)))
    })
    test("block-local MH corrections equal independent Poisson/NB log-density ratios", {
        S <- c(0,3,7);N <- 2L;r <- gmde_make_nb_r(S,4L)
        old <- c(-.2,.4,.9);new <- c(.1,.2,.7)
        correction <- gmde_poisson_nb_log_ratio(S,N*exp(new),r)-gmde_poisson_nb_log_ratio(S,N*exp(old),r)
        direct <- (dpois(S,N*exp(new),log=TRUE)-dnbinom(S,size=r,mu=N*exp(new),log=TRUE))-
                  (dpois(S,N*exp(old),log=TRUE)-dnbinom(S,size=r,mu=N*exp(old),log=TRUE))
        close(correction,direct)
        b <- tape(graphmode_expert_blocks_expert(matrix(initial$theta[1,,],5,2),
            config$Y[1:2,,drop=FALSE],config,policy,0,FALSE))$value
        for(row in b$update$blocks) {
            close(row$r,gmde_make_nb_r(colSums(config$Y[1:2,,drop=FALSE]),config$rho)[row$start:row$end])
            close(row$log_acceptance,min(0,sum(row$observation$correction_by_time)))
        }
    })
    test("all-rejected blocks still draw fresh auxiliaries once per block and return the old path", {
        cfg <- graphmode_config(matrix(1,1,7),matrix(1,7,1),0,matrix(1),"MoDE",K=1L,
            G=matrix(1),W=matrix(.1),rho=4L,dirichlet_alpha=1)
        cur <- matrix(0,7,1);pg_calls <- uniforms <- 0L
        patched(list(graphmode_uniform=function(n) {uniforms <<- uniforms+1L;rep(.999,n)},
            graphmode_pg=function(b,z) {pg_calls <<- pg_calls+1L;rep(.2+pg_calls/10,length(b))},
            graphmode_expert_block_ffbs=function(precision,natural,Fmat,m0,C0,G,W,left,right,cache_ffbs)
                list(theta=matrix(5,nrow(Fmat),1),factor_residual=0,root_residual=0,root_reciprocal_condition=1)), {
            a <- graphmode_expert_blocks_expert(cur,cfg$Y,cfg,graphmode_expert_blocks_policy(3,"fixed-zero"),0,FALSE)
            assert(identical(a$update$theta,cur) && a$update$movement==0)
            assert(a$update$counts$proposals==3 && a$update$counts$accepted==0)
            assert(pg_calls==3 && uniforms==3 && a$update$counts$acceptance==0)
            assert(all(vapply(a$update$blocks,function(b) b$observation$proposed_information_movement>0,logical(1))))
        })
    })
    test("zero-count occupied data are not treated as an empty expert", {
        a <- tape(graphmode_expert_blocks_expert(matrix(initial$theta[1,,],5,2),
            matrix(0,2,5),config,policy,0,TRUE))
        assert(a$used["pg"]==5L && !a$value$update$empty)
        assert(a$value$update$counts$prior_refreshes==0L && a$value$update$counts$proposals==3L)
        assert(all(a$value$update$r==gmde_make_nb_r(rep(0,5),config$rho)))
    })
    test("injected later block failure returns no partial expert and never retries", {
        calls <- 0L;old <- serialize(initial,NULL)
        patched(list(graphmode_expert_block_ffbs=function(...) {
            calls <<- calls+1L;if(calls==2L) stop("fixed block failure")
            list(theta=matrix(.1,2,2),factor_residual=0,root_residual=0,root_reciprocal_condition=1)
        }), {fails(tape(graphmode_expert_blocks_sweep(initial,config,gp,policy,FALSE)),"fixed block failure")})
        assert(calls==2L && identical(serialize(initial,NULL),old))
    })
    test("finite-state corrected conditional kernels preserve the same joint target for every layout", {
        # Enumerated reversible NB surrogate proposals, not a claim about a
        # replacement PG backend or empirical performance of the real sampler.
        states <- as.matrix(expand.grid(rep(list(c(-.4,.6)),4)))
        S <- c(0,2,4,1);r <- gmde_make_nb_r(S,4L)
        logP <- apply(states,1,function(z) dnorm(z[1],.1,.8,log=TRUE)+
            sum(dnorm(z[-1],.8*z[-4],.4,log=TRUE))+sum(dpois(S,2*exp(z),log=TRUE)))
        pi <- exp(logP-max(logP));pi <- pi/sum(pi)
        conditional <- function(idx) {
            P <- matrix(0,16,16);outside <- setdiff(1:4,idx)
            for(i in 1:16) {
                choices <- if(!length(outside)) 1:16 else which(apply(states[,outside,drop=FALSE],1,
                    function(z) all(z==states[i,outside])))
                correction <- vapply(choices,function(j) sum(gmde_poisson_nb_log_ratio(S[idx],2*exp(states[j,idx]),r[idx])),numeric(1))
                target <- logP[choices];surrogate <- target-correction
                q <- exp(surrogate-max(surrogate));q <- q/sum(q)
                for(j in seq_along(choices)) if(choices[j]!=i) {
                    ii <- which(choices==i)
                    P[i,choices[j]] <- q[j]*min(1,exp(correction[j]-correction[ii]))
                }
                P[i,i] <- 1-sum(P[i,])
            }
            close(pi*P,t(pi*P));close(as.numeric(pi%*%P),pi);P
        }
        for(width in 1:4) for(off in if(width==4) 0L else 0:(width-1L)) {
            b <- graphmode_expert_blocks_layout(4,width,off);P <- diag(16)
            for(j in seq_len(nrow(b))) P <- P%*%conditional(b$start[j]:b$end[j])
            close(rowSums(P),rep(1,16));close(as.numeric(pi%*%P),pi)
        }
    })
    test("all 68 frozen execution files and original bindings remain unchanged", {
        manifest <- read.table(file.path(graphmode_root,"docs/provenance/graphmode-refresh-freeze-2026-09-15.sha256"),stringsAsFactors=FALSE)
        assert(nrow(manifest)==68L)
        actual <- vapply(file.path(graphmode_root,manifest[[2]]),function(f) digest::digest(file=f,algo="sha256"),character(1))
        assert(identical(unname(actual),manifest[[1]]))
        for(name in c("graphmode_ffbs","graphmode_information_filter","graphmode_update_expert","graphmode_gate_refresh_sweep"))
            assert(identical(environment(get(name,kernel)),kernel))
    })
    test("RNG state/kind unchanged after all fixed-input checks", {
        after <- if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) get(".Random.seed",.GlobalEnv) else NULL
        assert(identical(RNGkind(),before_kind) && identical(after,before_seed))
    })
    cat("Passed",checks,"D-055 fixed-input groups. No real PG/MCMC or convergence claim.\n")
})
