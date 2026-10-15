# Two-data supplementary execution. All sampling transitions are unchanged.
graphmode_extra_fit <- function(input,coordinates,method,beta=1) {
    stopifnot(method %in% c('EucMoDE','MoDE','PottsMoDE'))
    input <- do.call(graphmode_input,input)
    if(!is.matrix(coordinates) || nrow(coordinates)!=nrow(input$Y) || any(!is.finite(coordinates)))
        stop('Aligned coordinates[n,dimension] required.')
    if(ncol(input$Y)<42L || ncol(input$Y)>168L || ncol(input$Fmat)!=3L ||
       !isTRUE(all.equal(unname(input$Fmat),unname(cbind(1,sin(2*pi*seq_len(ncol(input$Y))/24),cos(2*pi*seq_len(ncol(input$Y))/24))))))
        stop('Historical comparison interface uses the registered three-column seasonal design.')
    reference <- graphmode_make_fit(input,'Exp-W')
    weight <- graphmode_h_weight(reference$bandwidth$geometry,'exp',0)
    graphmode_c9_config(list(ids=input$ids,coordinates=coordinates),input$Y,method,weight,beta)
}

graphmode_extra_register <- function(directory,configuration) {
    if(file.exists(directory)) stop('Require an absent output directory; no retry.')
    cfg <- jsonlite::fromJSON(configuration,simplifyVector=FALSE)
    directory <- file.path(normalizePath(dirname(directory)),basename(directory))
    if(startsWith(directory,paste0(source_root,'/'))) stop('Outputs must be outside source.')
    commit <- system2('git',c('-C',shQuote(source_root),'rev-parse','HEAD'),stdout=TRUE)
    dirty <- system2('git',c('-C',shQuote(source_root),'status','--porcelain','--untracked-files=all'),stdout=TRUE)
    if(length(commit)!=1L || length(dirty)) stop('Freeze a clean committed source first.')
    plan <- jsonlite::fromJSON(cfg$plan_file)
    plan$seeds <- setNames(as.integer(plan$seeds),names(plan$seeds))
    plan$base_policy <- graphmode_c9_base_policy();plan$thresholds <- graphmode_c9_spec()$thresholds
    plan$warmup <- 2000L;plan$chunk <- 100L;plan$initial_iterations <- plan$max_iterations <- 4000L
    core <- normalizePath(cfg$core_input,mustWork=TRUE)
    inputs <- unlist(lapply(sprintf('D%02d',1:2),function(d)
        file.path(core,d,c('blind.rds','design.rds','evaluation-only.rds'))))
    for(f in inputs) graphmode_read(f)
    lock <- jsonlite::fromJSON(file.path(source_root,'external-lock.json'),simplifyVector=FALSE)
    if(graphmode_c8_hash(cfg$external$pnar)!=lock$PNAR$sha256) stop('Wrong PNAR source.')
    for(f in names(lock$STGNN$files)) if(graphmode_c8_hash(file.path(cfg$external$stgnn,f))!=lock$STGNN$files[[f]]) stop('Changed STGNN source.')
    r <- list(plan=plan,source_sha256=graphmode_identity(source_root),source_commit=commit,
        core_input=core,input_sha256=setNames(vapply(inputs,graphmode_c8_hash,''),inputs),
        external=cfg$external,external_lock=lock,queue_after=cfg$queue_after,core_results=cfg$core_results,
        combined_outputs=cfg$combined_outputs,python_runtime=cfg$python_runtime,
        created=format(Sys.time(),'%Y-%m-%dT%H:%M:%S%z'),runtime=capture.output(sessionInfo()),
        packages=setNames(vapply(c('BayesLogit','posterior','digest','jsonlite','microbenchmark','Rfast','nloptr'),function(p) as.character(packageVersion(p)),''),
            c('BayesLogit','posterior','digest','jsonlite','microbenchmark','Rfast','nloptr')))
    r$signature <- graphmode_digest(r)
    dir.create(directory);dir.create(file.path(directory,'jobs'))
    graphmode_store(r,file.path(directory,'registration.rds'))
    transport <- r
    for(k in c('source_sha256','input_sha256','packages')) transport[[k]] <- as.list(r[[k]])
    transport$plan$seeds <- as.list(r$plan$seeds)
    graphmode_joint_run_json(transport,file.path(directory,'registration.json'),TRUE)
    graphmode_c8_accept(file.path(directory,'registration.json'))
    write.csv(plan$jobs,file.path(directory,'jobs.csv'),row.names=FALSE)
    write.csv(data.frame(role=names(plan$seeds),seed=unname(plan$seeds)),file.path(directory,'seeds.csv'),row.names=FALSE)
    for(d in sprintf('D%02d',1:2)) {
        old <- graphmode_read(file.path(core,d,'blind.rds'))
        design <- graphmode_read(file.path(core,d,'design.rds'))
        xy <- design$road$coordinates[match(old$input$ids,design$road$ids),,drop=FALSE]
        methods <- c('EucMoDE','MoDE','PottsMoDE')
        fits <- setNames(lapply(methods,function(m) graphmode_extra_fit(old$input,xy,m)),methods)
        starts <- lapply(fits,function(f) lapply(1:4,function(j)
            graphmode_c8_common_start(f,old$starts[['Exp-W']][[j]]$core,j)))
        weight <- graphmode_h_weight(old$fits[['Exp-W']]$bandwidth$geometry,'exp',0)
        b <- list(input=old$input,coordinates=xy,weight=weight,fits=fits,starts=starts)
        path <- file.path(directory,d);dir.create(path)
        graphmode_store(b,file.path(path,'blind.rds'))
        graphmode_store(graphmode_read(file.path(core,d,'evaluation-only.rds')),file.path(path,'evaluation-only.rds'))
        graphmode_joint_run_json(list(signature=r$signature,dataset=d,Y=unname(t(b$input$Y)),weight=unname(weight)),file.path(path,'training.json'),TRUE)
        graphmode_c8_accept(file.path(path,'training.json'))
    }
    graphmode_store(list(signature=r$signature),file.path(directory,'generation-complete.rds'))
    cat('REGISTERED',r$signature,'\n');invisible(r)
}

graphmode_extra_guard <- function(r) {
    graphmode_guard(r)
    if(graphmode_c8_hash(r$external$pnar)!=r$external_lock$PNAR$sha256) stop('PNAR changed.')
    for(f in names(r$external_lock$STGNN$files))
        if(graphmode_c8_hash(file.path(r$external$stgnn,f))!=r$external_lock$STGNN$files[[f]]) stop('STGNN changed.')
}

graphmode_extra_selected_fit <- function(directory,b,job) {
    fit <- b$fits[[job$method]]
    if(job$method=='PottsMoDE') {
        selection <- graphmode_read(file.path(directory,job$dataset,'selection-PottsMoDE.rds'))
        if(is.null(selection$selected)) stop('No complete valid Potts candidate; no fallback.')
        fit$core$potts_beta <- selection$selected$beta
    }
    fit
}

graphmode_extra_chain <- function(directory,r,job,b,path) {
    validation <- job$kind=='potts_validation'
    target <- if(validation) 600L else 4000L;warmup <- if(validation) 300L else 2000L
    if(validation) {
        x <- b$input;x$Y <- x$Y[,seq_len(job$origin),drop=FALSE];x$Fmat <- x$Fmat[seq_len(job$origin),,drop=FALSE]
        fit <- graphmode_extra_fit(x,b$coordinates,'PottsMoDE',as.numeric(sub('beta','',job$candidate)))
        state <- graphmode_c9_initial(fit,job$chain,r$plan$seeds,paste0(job$dataset,'-potts-',job$origin))
    } else {
        fit <- graphmode_extra_selected_fit(directory,b,job)
        state <- b$starts[[job$method]][[job$chain]]
    }
    states <- diagnostics <- list();started <- proc.time()[3L]
    set.seed(job$seed);graphmode_store(list(state=state,rng=.Random.seed,job=job,fit_hash=graphmode_digest(fit)),file.path(path,'initial.rds'))
    record <- list(base_policy=r$plan$base_policy,thresholds=r$plan$thresholds)
    tryCatch({
        for(i in seq_len(target)) {
            if(validation && proc.time()[3L]-started>1800) stop('Original Potts validation time cap exceeded; preserve failure.')
            before <- state;out <- graphmode_c8_step(before,fit)
            graphmode_c8_check_transition(before,out,fit,record);state <- out$state
            states[[length(states)+1L]] <- state
            diagnostics[[length(diagnostics)+1L]] <- list(base=out$base$base_diagnostic,events=out$base$events,q_update=out$q_update,scale_update=out$scale_update)
            if(i%%100L==0L || i==target) {
                graphmode_store(list(signature=r$signature,end_iteration=i,state=state,rng=.Random.seed,states=states,diagnostics=diagnostics),file.path(path,sprintf('chunk-%05d.rds',i)))
                states <- diagnostics <- list()
            }
            if(i%%25L==0L || i==target) {
                graphmode_joint_run_json(list(iteration=i,target=target,elapsed=unname(proc.time()[3L]-started)),file.path(path,'progress.json'))
                cat(job$id,i,'/',target,'elapsed',proc.time()[3L]-started,'\n');flush.console()
            }
        }
        chunks <- list.files(path,'^chunk-[0-9]+[.]rds$')
        graphmode_store(list(signature=r$signature,target=target,warmup=warmup,chunks=setNames(vapply(file.path(path,chunks),graphmode_c8_hash,''),chunks),seconds=unname(proc.time()[3L]-started),fit_hash=graphmode_digest(fit)),file.path(path,sprintf('receipt-%d.rds',target)))
        if(validation) {
            view <- r;view$plan$warmup <- warmup
            chain <- graphmode_chain(directory,job,view,target,fit)
            t <- job$origin+seq_len(48L)
            prediction <- graphmode_c8_predict(chain$draws,fit,cbind(1,sin(2*pi*t/24),cos(2*pi*t/24)))
            graphmode_store(list(valid=TRUE,prediction=prediction),file.path(path,'validation.rds'))
        }
    },error=function(e) {
        graphmode_store(list(error=conditionMessage(e),state=state,rng=.Random.seed,pending_states=states,diagnostics=diagnostics,iteration=state$core$iteration),file.path(path,'failure.rds'));stop(e)
    })
}

graphmode_extra_selection <- function(directory,r,job,b) {
    kinds <- c(PottsMoDE='potts_validation',PNAR='pnar_validation',STGNN='stgnn_validation')
    jobs <- r$plan$jobs[r$plan$jobs$dataset==job$dataset & r$plan$jobs$kind==kinds[[job$method]],]
    rows <- lapply(seq_len(nrow(jobs)),function(i) {
        j <- jobs[i,];path <- file.path(directory,'jobs',j$id)
        process <- jsonlite::fromJSON(file.path(path,'process.json'))
        good <- isTRUE(process$exit_code==0)
        if(job$method=='PottsMoDE') {
            x <- data.frame(beta=as.numeric(sub('beta','',j$candidate)),origin=j$origin,chain=j$chain,valid=good)
            x$prediction <- list(if(good) graphmode_read(file.path(path,'validation.rds'))$prediction else NULL);return(x)
        }
        epochs <- if(job$method=='PNAR') 0L else seq(25L,300L,25L)
        x <- data.frame(candidate=j$candidate,origin=j$origin,restart=j$restart,epoch=epochs,sse=NA_real_,n=5808L,valid=FALSE)
        if(good) {
            scores <- if(job$method=='PNAR') graphmode_read(file.path(path,'result.rds'))$scores else jsonlite::fromJSON(file.path(path,'result.json'))$scores
            stopifnot(identical(as.integer(scores$epoch),as.integer(epochs)))
            x$sse <- scores$sse;x$n <- scores$n;x$valid <- scores$valid
        }
        x
    })
    records <- do.call(rbind,rows)
    outcome <- tryCatch(if(job$method=='PottsMoDE') graphmode_c9_potts_select(records,b$input$Y) else graphmode_c8_select(records,job$method),error=function(e) {
        if(!grepl('No complete valid',conditionMessage(e),fixed=TRUE)) stop(e)
        list(selected=NULL,records=records,failure=conditionMessage(e))
    })
    graphmode_store(outcome,file.path(directory,job$dataset,paste0('selection-',job$method,'.rds')))
    graphmode_joint_run_json(list(signature=r$signature,selected=outcome$selected),file.path(directory,job$dataset,paste0('selection-',job$method,'.json')),TRUE)
    graphmode_c8_accept(file.path(directory,job$dataset,paste0('selection-',job$method,'.json')))
}

graphmode_extra_worker <- function(directory,index) {
    r <- graphmode_read(file.path(directory,'registration.rds'));graphmode_extra_guard(r)
    job <- r$plan$jobs[index,];stopifnot(nrow(job)==1L)
    b <- graphmode_read(file.path(directory,job$dataset,'blind.rds'))
    path <- file.path(directory,'jobs',job$id);dir.create(path,showWarnings=FALSE)
    if(job$kind=='selection') return(graphmode_extra_selection(directory,r,job,b))
    if(job$kind %in% c('bayes','potts_validation')) return(graphmode_extra_chain(directory,r,job,b,path))
    if(!job$kind %in% c('pnar_validation','pnar_final')) stop('Use the Python worker for STGNN.')
    Y <- b$input$Y
    if(job$kind=='pnar_validation') {p <- as.integer(sub('p','',job$candidate));train <- Y[,seq_len(job$origin),drop=FALSE]} else {
        selection <- graphmode_read(file.path(directory,job$dataset,'selection-PNAR.rds'))$selected
        if(is.null(selection)) stop('No valid PNAR candidate; no fallback.')
        p <- selection$p;train <- Y
    }
    author <- new.env(parent=globalenv());sys.source(r$external$pnar,author)
    setTimeLimit(elapsed=300,transient=TRUE)
    on.exit(setTimeLimit(cpu=Inf,elapsed=Inf,transient=FALSE),add=TRUE)
    tryCatch({
        fit <- graphmode_c8_pnar_fit(train,b$weight,p,author$lin_estimnarpq)
        scores <- if(job$kind=='pnar_validation') data.frame(epoch=0L,sse=sum((fit$prediction-Y[,job$origin+seq_len(48L)])^2),n=5808L,valid=TRUE) else NULL
        graphmode_store(list(fit=fit,scores=scores,signature=r$signature),file.path(path,'result.rds'))
    },error=function(e) {graphmode_store(list(error=conditionMessage(e),numeric_receipt=e$c8_failure),file.path(path,'failure.rds'));stop(e)})
}
