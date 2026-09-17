# D-060 fixed inputs / independent utility algebra. No stochastic experiment.
if (!exists("graphmode_root", inherits = FALSE)) {
    entry <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
    graphmode_root <- dirname(dirname(dirname(normalizePath(entry, mustWork = TRUE))))
}
local({
    kernel <- new.env(parent = globalenv())
    before_kind <- RNGkind()
    before_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
    files <- c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        "graphmode_dev.R", "graphmode_gate_refresh.R", "graphmode_expert_blocks.R", "graphmode_gate_blocks.R")
    for (f in files) sys.source(file.path(graphmode_root, "R", f), kernel)
    test_environment <- environment()
    parent.env(test_environment) <- kernel
    checks <- 0L
    assert <- function(x) if (!isTRUE(x)) stop("D-060 assertion: ", paste(deparse(substitute(x)),collapse=" "),call.=FALSE)
    close <- function(a,b,tol=1e-10) assert(length(a)==length(b) && identical(dim(a),dim(b)) &&
        all(is.finite(c(a,b))) && max(abs(a-b),0) <= tol * max(1,abs(b)))
    fails <- function(code, pattern=NULL) {
        e <- tryCatch({force(code);NULL},error=identity); assert(inherits(e,"error"))
        if(!is.null(pattern)) assert(grepl(pattern,conditionMessage(e)))
    }
    patched <- function(bindings,code) {
        old <- mget(names(bindings),kernel);on.exit(list2env(old,kernel),add=TRUE)
        list2env(bindings,kernel);force(code)
    }
    tape <- function(code) {
        used <- c(normal=0L,uniform=0L,pg=0L)
        patched(list(graphmode_normal=function(n) {
            i <- used["normal"]+seq_len(n);used["normal"] <<- used["normal"]+n;.12*sin(i/5)
        },graphmode_uniform=function(n) {
            i <- used["uniform"]+seq_len(n);used["uniform"] <<- used["uniform"]+n;.2+.6*(i%%13)/13
        },graphmode_pg=function(b,z) {used["pg"] <<- used["pg"]+length(b);rep(.4,length(b))}),{
            value <- force(code);list(value=value,used=used)
        })
    }
    test <- function(name,code) {
        patched(list(graphmode_normal=function(...)stop("Unexpected random normal"),
            graphmode_uniform=function(...)stop("Unexpected random uniform"),
            graphmode_pg=function(...)stop("Unexpected real PG"),
            graphmode_gamma=function(...)stop("Unexpected random gamma")),force(code))
        checks <<- checks+1L;cat("OK",checks,name,"\n")
    }
    config <- graphmode_config(matrix(1:12,4,3),cbind(1,c(-1,0,1)),c(.2,-.1),diag(2),
        "graphMoDE-W",K=3L,G=diag(2),W=diag(.02,2),Phi=diag(4),guidance_proposal_sd=.4,rho=4L)
    state <- graphmode_initial_state(config,c(1L,2L,1L,3L),array(.1,c(3,3,2)),
        x=.2*sin(seq_len(graphmode_gate_dimension(config))),v=c(-.4,.1,.7))
    full <- graphmode_gate_blocks_policy(config,"full")
    block <- graphmode_gate_blocks_policy(config,"contrast")
    gp <- graphmode_gate_refresh_policy(4L,.4)
    ep <- graphmode_expert_blocks_policy(2L,"fixed-zero")
    # Dense utility map from independent Cholesky factors, not kernel QR/cache.
    dense <- function(x,v,cfg) {
        d <- cfg$K-1L;n <- cfg$n;q <- ncol(cfg$Phi);H <- gmde_helmert_contrast(cfg$K)
        a <- if(cfg$guidance=="shared")rep(plogis(v),cfg$K) else plogis(v)
        A <- crossprod(H,a*H);E <- crossprod(H,(1-a)*H)
        gamma <- matrix(x[d+seq_len(q*d)],q,d)
        noise <- matrix(x[d+q*d+seq_len(n*d)],n,d)
        (matrix(rep(cfg$s_b*x[seq_len(d)],each=n),n,d)+cfg$Phi%*%gamma%*%chol(A)+
            cfg$tau*noise%*%chol(E))%*%t(H)
    }
    test("fixed contrast layout covers each prior coordinate once",{
        assert(identical(block$blocks,list(as.integer(c(1,3:6,11:14)),as.integer(c(2,7:10,15:18)))))
        assert(identical(sort(unlist(block$blocks)),seq_len(length(state$x))))
    })
    test("K10 n121 uses nine fixed 243-coordinate blocks, not ten class groups",{
        c10 <- graphmode_config(matrix(1,121,3),config$Fmat,c(0,0),diag(2),"graphMoDE-W",
            K=10L,G=diag(2),W=diag(.02,2),Phi=diag(121),guidance_proposal_sd=.4,rho=4L)
        pol <- graphmode_gate_blocks_policy(c10,"contrast")
        assert(pol$dimension==2187 && identical(lengths(pol$blocks),rep(243L,9L)))
    })
    test("policy tampering rejected before draws",{
        for(nm in c("order","budget","adaptation","schema")) {
            bad <- block;bad[[nm]] <- "changed";fails(graphmode_gate_blocks_ess(state$x,state$v,state$Z,config,bad))
        }
        bad <- block;bad$blocks[[1]][1] <- 2L
        fails(graphmode_gate_blocks_ess(state$x,state$v,state$Z,config,bad))
        for(s in list(NA_character_,"auto",c("full","contrast"),NULL)) fails(graphmode_gate_blocks_policy(config,s))
    })
    test("invalid states and labels rejected before consuming draws",{
        fails(graphmode_gate_blocks_ess(state$x[-1],state$v,state$Z,config,block))
        fails(graphmode_gate_blocks_ess(state$x,state$v[-1],state$Z,config,block))
        fails(graphmode_gate_blocks_ess(state$x,state$v,c(0L,1L,1L,2L),config,block))
    })
    test("whole-vector option is identical to original ESS including draws",{
        a <- tape(graphmode_gate_ess(state$x,state$v,state$Z,config))
        b <- tape(graphmode_gate_blocks_ess(state$x,state$v,state$Z,config,full))
        assert(identical(a$used,b$used));assert(identical(a$value,b$value[names(a$value)]))
        assert(b$value$blocks[[1]]$evaluations==a$value$evaluations)
    })
    test("independent dense utility formula agrees for nonsymmetric white inputs",{
        close(dense(state$x,state$v,config),graphmode_gate_utilities(state$x,state$v,config))
        c2 <- config;c2$Phi <- matrix(c(1,.2,.1,0,0,.8,-.2,.4),4,2)
        xx <- seq_len(graphmode_gate_dimension(c2))/20
        close(dense(xx,state$v,c2),graphmode_gate_utilities(xx,state$v,c2))
    })
    test("conditional rotations keep fixed complement and immediately hand off accepted block",{
        seen <- list();normal_calls <- 0L
        patched(list(graphmode_gate_loglik=function(F,Z){seen[[length(seen)+1L]] <<- F;0},
            graphmode_uniform=function(n) rep(.25,n),graphmode_normal=function(n){normal_calls <<- normal_calls+1L;rep(.3*normal_calls,n)}),{
            out <- graphmode_gate_blocks_ess(state$x,state$v,state$Z,config,block)
            xx <- state$x
            close(seen[[1]],dense(xx,state$v,config))
            for(h in 1:2) {
                idx <- block$blocks[[h]];xx[idx] <- state$x[idx]*cos(pi/2)+.3*h*sin(pi/2)
                close(seen[[2*h]],dense(xx,state$v,config))
                if(h==1) close(seen[[3]],dense(xx,state$v,config))
            }
            close(out$x,xx);assert(out$evaluations==2L);assert(length(seen)==4L)
        })
    })
    test("zero likelihood yields independent prior rotations without rotating outside block",{
        angle <- .73
        for(idx in block$blocks) {
            xx <- state$x;nu <- .3*cos(seq_along(idx));xx[idx] <- xx[idx]*cos(angle)+nu*sin(angle)
            rotated_nu <- -state$x[idx]*sin(angle)+nu*cos(angle)
            close(sum(xx[idx]^2)+sum(rotated_nu^2),sum(state$x[idx]^2)+sum(nu^2))
            assert(identical(xx[-idx],state$x[-idx]))
        }
    })
    test("actual softmax likelihood agrees with direct fixed-complement target",{
        seen <- list();old_ll <- kernel$graphmode_gate_loglik
        patched(list(graphmode_gate_loglik=function(F,Z) {
            val <- sum(F[cbind(seq_along(Z),Z)]-apply(F,1,function(row){m<-max(row);m+log(sum(exp(row-m)))}))
            close(val,old_ll(F,Z));seen[[length(seen)+1L]] <<- val;old_ll(F,Z)
        }),{out <- tape(graphmode_gate_blocks_ess(state$x,state$v,state$Z,config,block))$value})
        assert(length(seen)>=4L);close(out$utilities,dense(out$x,state$v,config))
    })
    test("rejection shrinks the bracket and counts every attempted angle",{
        calls <- 0L
        patched(list(graphmode_gate_loglik=function(F,Z){calls <<- calls+1L;if(calls==2L)-1e9 else 0},
            graphmode_uniform=function(n)rep(.25,n),graphmode_normal=function(n)rep(.1,n)),{
            out <- graphmode_gate_blocks_ess(state$x,state$v,state$Z,config,block)
            assert(out$blocks[[1]]$evaluations==2L && out$blocks[[2]]$evaluations==1L && out$evaluations==3L)
        })
    })
    test("shared budget failure after a successful block returns no partial state",{
        c1 <- config;c1$max_ess_steps <- 1L;count <- 0L
        patched(list(graphmode_gate_loglik=function(...)0,graphmode_uniform=function(n)rep(.25,n),
            graphmode_normal=function(n){count <<- count+1L;rep(.1,n)}),{
            fails(graphmode_gate_blocks_ess(state$x,state$v,state$Z,c1,block),"shared bracket budget")
            assert(count==1L)
        })
    })
    test("all rejections stop at the shared budget without retry or extra direction",{
        c1 <- config;c1$max_ess_steps <- 3L;calls <- 0L;normal_calls <- 0L
        patched(list(graphmode_gate_loglik=function(...){calls <<- calls+1L;if(calls==1L)0 else -1e9},
            graphmode_uniform=function(n)rep(.25,n),graphmode_normal=function(n){normal_calls <<- normal_calls+1L;rep(.1,n)}),{
            fails(graphmode_gate_blocks_ess(state$x,state$v,state$Z,c1,block),"shared bracket budget")
            assert(normal_calls==1L && calls==4L)
        })
    })
    test("binary shared guidance degenerates exactly to the original full ESS",{
        c2 <- graphmode_config(config$Y,config$Fmat,config$m0,config$C0,"graphMoDE-W",K=2L,
            G=config$G,W=config$W,Phi=config$Phi,guidance="shared",guidance_proposal_sd=.4,rho=4L)
        x <- rep(.1,graphmode_gate_dimension(c2));Z <- c(1L,2L,1L,2L)
        a <- tape(graphmode_gate_ess(x,.2,Z,c2));bb <- tape(graphmode_gate_blocks_ess(x,.2,Z,c2,graphmode_gate_blocks_policy(c2,"contrast")))
        assert(identical(a$used,bb$used));assert(identical(a$value,bb$value[names(a$value)]))
    })
    test("shared guidance with three classes and rectangular Phi works",{
        cs <- graphmode_config(config$Y,config$Fmat,config$m0,config$C0,"graphMoDE-W",K=3L,
            G=config$G,W=config$W,Phi=sqrt(2)*config$Phi[,1:2],guidance="shared",guidance_proposal_sd=.4,rho=4L)
        x <- rep(.1,graphmode_gate_dimension(cs));po <- graphmode_gate_blocks_policy(cs,"contrast")
        z <- tape(graphmode_gate_blocks_ess(x,.2,state$Z,cs,po))$value
        close(z$utilities,dense(z$x,.2,cs));assert(length(z$x)==14L)
    })
    test("full-option m4 composition preserves original x v utilities and counts",{
        a <- tape(graphmode_gate_refresh(state$x,state$v,state$Z,config,gp))
        b <- tape(graphmode_gate_blocks_refresh(state$x,state$v,state$Z,config,gp,full))
        assert(identical(a$used,b$used))
        for(nm in c("x","v","utilities","counts")) assert(identical(a$value[[nm]],b$value[[nm]]))
    })
    test("blocked m4 equals manual sequential composition with one MH scan after each",{
        a <- tape({x<-state$x;v<-state$v
            for(i in 1:4){g<-graphmode_gate_blocks_ess(x,v,state$Z,config,block);u<-graphmode_guidance_update(g$x,v,state$Z,config);x<-g$x;v<-u$v}
            list(x=x,v=v,utilities=u$utilities)})
        b <- tape(graphmode_gate_blocks_refresh(state$x,state$v,state$Z,config,gp,block))
        assert(identical(a$used,b$used));assert(identical(a$value,b$value[c("x","v","utilities")]))
        assert(b$value$counts$total_proposals==12 && length(b$value$ess_scans)==4L)
        assert(!b$value$inner_states_are_retained_draws)
    })
    test("cached conditional ESS equals independent direct-state bracket replay",{
        # Evaluate every proposed COMPLETE white state through the dense map;
        # no Ffixed/Fblock cache and no call to the implementation under test.
        direct <- function(x,v,Z,cfg,pol) {
            total <- 0L
            for(idx in pol$blocks) {
                log_y <- graphmode_gate_loglik(dense(x,v,cfg),Z)+log(graphmode_uniform(1L))
                nu <- graphmode_normal(length(idx));angle <- 2*pi*graphmode_uniform(1L)
                lo <- angle-2*pi;hi <- angle
                repeat {
                    proposal <- x;proposal[idx] <- x[idx]*cos(angle)+nu*sin(angle)
                    total <- total+1L
                    if(graphmode_gate_loglik(dense(proposal,v,cfg),Z)>=log_y){x<-proposal;break}
                    if(angle<0)lo<-angle else hi<-angle
                    angle <- lo+(hi-lo)*graphmode_uniform(1L)
                }
            }
            list(x=x,utilities=dense(x,v,cfg),evaluations=total)
        }
        for(mult in c(-3,.5,4)) {
            x <- state$x*mult;v <- state$v*mult
            a <- tape(direct(x,v,state$Z,config,block))
            b <- tape(graphmode_gate_blocks_ess(x,v,state$Z,config,block))
            assert(identical(a$used,b$used));close(a$value$x,b$value$x)
            close(a$value$utilities,b$value$utilities);assert(a$value$evaluations==b$value$evaluations)
        }
        cc <- config
        ph <- outer(1:4,1:4,function(i,j).7^abs(i-j))
        cc$Phi <- ph*sqrt(4/sum(ph^2))
        a <- tape(direct(state$x,state$v,state$Z,cc,block))
        b <- tape(graphmode_gate_blocks_ess(state$x,state$v,state$Z,cc,block))
        assert(identical(a$used,b$used));close(a$value$x,b$value$x)
        close(a$value$utilities,b$value$utilities);assert(a$value$evaluations==b$value$evaluations)
    })
    test("full ESS in full outer step preserves old scientific state and PG count",{
        a <- tape(graphmode_expert_blocks_sweep(state,config,gp,ep,TRUE))
        b <- tape(graphmode_gate_blocks_sweep(state,config,gp,ep,full,TRUE))
        assert(identical(a$used,b$used));assert(identical(a$value$transition$state,b$value$transition$state))
    })
    test("blocked outer step calls expert kernel once per expert and allocation once",{
        calls <- 0L;old <- kernel$graphmode_categorical
        patched(list(graphmode_categorical=function(w){calls <<- calls+1L;old(w)}),{
            a <- tape(graphmode_gate_blocks_sweep(state,config,gp,ep,block,TRUE))
            assert(calls==1L);assert(a$value$completed_outer_steps==1L)
            assert(a$value$expert_update_count==config$K && a$value$transition$state$iteration==1L)
            assert(length(a$value$gate_refresh$ess_scans)==4L && a$value$gate_refresh$counts$total_proposals==12)
            assert(!a$value$run_registered && !a$value$formal_authorized)
            assert(is.null(a$value$transition$guidance_accept))
            reference <- tape(graphmode_expert_blocks_sweep(state,config,gp,ep,TRUE))
            assert(a$used["pg"]==reference$used["pg"])
        })
    })
    test("empty experts still refresh full prior once; empty guidance coordinates remain in MH",{
        st <- graphmode_initial_state(config,c(1L,1L,1L,1L),state$theta,x=state$x,v=state$v)
        z <- tape(graphmode_gate_blocks_sweep(st,config,gp,ep,block,TRUE))$value
        assert(sum(vapply(z$transition$expert_updates,function(e)e$counts$prior_refreshes,integer(1)))==2L)
        assert(identical(z$gate_refresh$counts$proposals_by_coordinate,rep(4,3)))
    })
    test("private composition does not replace frozen function definitions",{
        names <- c("graphmode_gate_ess","graphmode_gate_refresh","graphmode_gate_refresh_sweep","graphmode_expert_blocks_sweep")
        before <- lapply(mget(names,kernel),function(f)list(body=body(f),environment=environment(f)))
        tape(graphmode_gate_blocks_sweep(state,config,gp,ep,block,TRUE))
        after <- lapply(mget(names,kernel),function(f)list(body=body(f),environment=environment(f)))
        assert(identical(before,after))
    })
    test("disabled or nonadaptive guidance is not silently changed",{
        for(kind in c("none","forced")) {
            cc <- graphmode_config(config$Y,config$Fmat,config$m0,config$C0,"graphMoDE-W",K=3L,
                G=config$G,W=config$W,Phi=config$Phi,guidance=kind,rho=4L)
            fails(graphmode_gate_blocks_policy(cc,"contrast"),"free adaptive")
        }
    })
    test("old source glob excludes this optional module",{
        assert(!"graphmode_gate_blocks.R" %in% list.files(file.path(graphmode_root,"R"),"^graphmode-.*[.]R$"))
        assert(!"graphmode_gate_blocks.R" %in% list.files(file.path(graphmode_root,"R"),"^graphmode4-.*[.]R$"))
    })
    test("all 80 frozen entries remain byte-identical",{
        manifest <- read.table(file.path(graphmode_root,"docs/provenance/graphmode-block-freeze-2026-09-15.sha256"),stringsAsFactors=FALSE)
        assert(nrow(manifest)==80L)
        actual <- vapply(file.path(graphmode_root,manifest[[2]]),function(f)digest::digest(file=f,algo="sha256",serialize=FALSE),character(1))
        assert(identical(unname(actual),manifest[[1]]))
    })
    assert(identical(before_kind,RNGkind()))
    assert(identical(before_seed,if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) .Random.seed else NULL))
    cat("PASS",checks,"fixed-input groups; RNG unchanged; no simulation or empirical efficiency claim.\n")
})
