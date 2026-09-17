# Focused integration checks. No scientific data, real PG or random generator.
local({
    kind <- RNGkind(); had <- exists('.Random.seed', .GlobalEnv, inherits=FALSE)
    seed <- if(had)get('.Random.seed',.GlobalEnv) else NULL
    config <- graphmode_config(matrix(1:12,4,3),cbind(1,c(-1,0,1)),c(.2,-.1),diag(2),
        'graphMoDE-W',K=3L,G=diag(2),W=diag(.02,2),Phi=diag(4),guidance_proposal_sd=.4,rho=4L)
    state <- graphmode_initial_state(config,c(1L,1L,2L,2L)); state$iteration <- 9L
    checks <- 0L
    assert <- function(x) if(!isTRUE(x))stop('Joint executor assertion: ',paste(deparse(substitute(x)),collapse=' '))
    test <- function(name, code) {force(code);checks <<- checks+1L;cat('OK',checks,name,'\n')}
    stub <- function(state,config,policy) {
        previous <- state;state$iteration <- state$iteration+1L
        list(state=state,diagnostic=list(before_signature=graphmode_digest(previous),
            after_signature=graphmode_digest(state),expert=rep(list(NULL),config$K),gate=NULL,block_offset=0L))
    }
    step <- graphmode_dev_bind(graphmode_joint_run_step,list(graphmode_gate_cache_run_step=stub))
    record <- list(base_policy=NULL,thresholds=NULL,spec=list(joint_every=10L))
    check <- graphmode_dev_bind(graphmode_joint_run_check_step,list(
        graphmode_gate_cache_run_gate_check=function(...)invisible(TRUE),
        graphmode_blocks_run_expert_check=function(...)invisible(TRUE)))
    fixed_step <- function(input,enabled=TRUE,u=.5,z=0,real_policy=NULL) {
        # Rebind the dependency closure without modifying loaded production functions.
        env <- new.env(parent=environment(graphmode_joint_run_step))
        for(nm in ls(.GlobalEnv,pattern='^graphmode')) {
            f <- get(nm,.GlobalEnv)
            if(is.function(f)) {environment(f)<-env;assign(nm,f,env)}
        }
        if(is.null(real_policy))env$graphmode_gate_cache_run_step <- stub
        env$graphmode_uniform <- function(n)rep(u,n)
        env$graphmode_normal <- function(n)rep(z,n)
        env$graphmode_pg <- function(b,z)rep(.4,length(b))
        env$graphmode_gamma <- function(...)stop('Real gamma forbidden')
        env$graphmode_joint_run_step(input,config,real_policy,graphmode_joint_partition_policy(),enabled,10L)
    }
    test('registered plan has paired starts, eight unique worker seeds and unchanged thresholds',{
        s<-graphmode_joint_run_spec();assert(length(s$seeds)==24L && !anyDuplicated(s$seeds))
        assert(nrow(s$jobs)==8L && s$iterations==600L && s$warmup==300L && s$joint_every==10L)
        assert(identical(s$start_occupancy,c(1L,3L,7L,10L)))
        assert(s$science_seconds==7200 && s$diagnostic_seconds==600)
    })
    test('reference leaves the complete audited base result unchanged',{
        out<-fixed_step(state,FALSE);assert(is.null(out$joint) && is.null(out$candidate))
        assert(identical(out$state,stub(state,config,NULL)$state));check(state,out,config,record,FALSE)
    })
    test('joint update occurs after base allocation exactly on schedule',{
        out<-fixed_step(state,TRUE,0);assert(!is.null(out$joint))
        assert(out$state$iteration==10L && out$base_state$iteration==10L)
        check(state,out,config,record,TRUE)
    })
    test('off-schedule joint arm makes no additional scientific update',{
        st<-state;st$iteration<-8L;out<-fixed_step(st)
        assert(is.null(out$joint) && identical(out$state,out$base_state));check(st,out,config,record,TRUE)
    })
    test('rejected candidate remains separate and final state stays at base',{
        out<-fixed_step(state,TRUE,.99,8)
        assert(!out$joint$accepted && !is.null(out$candidate))
        assert(identical(out$state,out$base_state));check(state,out,config,record,TRUE)
    })
    test('accepted candidate supplies final events and derived values',{
        out<-fixed_step(state,TRUE,0,0)
        assert(out$joint$accepted && identical(out$state,out$candidate))
        assert(identical(out$events,graphmode_allocation_events(state$Z,out$state$Z,config$K)))
        assert(identical(out$joint$eta,gmde_eta(out$state$theta,config$Fmat)))
    })
    test('stale final values and iteration corruption fail closed',{
        out<-fixed_step(state,TRUE,0,0)
        for(field in c('eta','utilities','sizes')) {
            bad<-out;bad$joint[[field]][1]<-999
            assert(inherits(tryCatch({check(state,bad,config,record,TRUE);NULL},error=identity),'error'))
        }
        bad<-out;bad$state$iteration<-11L
        assert(inherits(tryCatch({check(state,bad,config,record,TRUE);NULL},error=identity),'error'))
    })
    test('registration seed audit detects historic collisions without drawing',{
        directory<-tempfile('joint-seed-test-');dir.create(directory)
        saveRDS(list(spec=list(seeds=c(a=12L,b=13L))),file.path(directory,'registration.rds'))
        assert(graphmode_joint_run_seeds(directory,c(20L,21L))$collision_count==0L)
        assert(inherits(tryCatch({graphmode_joint_run_seeds(directory,c(12L,20L));NULL},error=identity),'error'))
        unlink(directory,recursive=TRUE)
    })
    test('atomic progress and exclusive completion cannot overwrite existing result',{
        path<-tempfile(fileext='.json')
        graphmode_joint_run_json(list(status='running'),path,TRUE)
        graphmode_joint_run_json(list(status='completed',scalars=as.list(table(c('passed','failed')))),path)
        assert(jsonlite::read_json(path)$status=='completed')
        assert(inherits(tryCatch({graphmode_joint_run_json(list(),path,TRUE);NULL},error=identity),'error'))
        unlink(path)
    })
    test('complete cached base and joint update pass original numerical evidence checks with fixed PG inputs',{
        real_policy<-list(gate=graphmode_gate_refresh_policy(4L,.4),
            block=graphmode_expert_blocks_policy(3L,'uniform'),ess_scheme='contrast',factor_cache=TRUE)
        real_record<-list(base_policy=real_policy,spec=list(joint_every=10L),
            thresholds=graphmode4_pilot_spec()$thresholds)
        for(enabled in c(FALSE,TRUE)) {
            out<-fixed_step(state,enabled,.2,0,real_policy)
            graphmode_joint_run_check_step(state,out,config,real_record,enabled)
            assert(out$state$iteration==10L)
        }
    })
    assert(identical(kind,RNGkind()) && identical(had,exists('.Random.seed',.GlobalEnv,inherits=FALSE)))
    assert(identical(seed,if(had)get('.Random.seed',.GlobalEnv) else NULL))
    cat('PASS',checks,'focused integration groups; RNG unchanged; no simulation.\n')
})
