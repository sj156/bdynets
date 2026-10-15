# Durable execution plumbing only; transition functions remain unchanged.
graphmode_store <- function(x,path) {
    graphmode_c8_save(x,path);graphmode_c8_accept(path);invisible(path)
}
graphmode_read <- function(path) readRDS(graphmode_c8_artifact(dirname(path),basename(path)))
graphmode_identity <- function(root) {
    files <- sort(list.files(root,recursive=TRUE,all.files=TRUE,full.names=FALSE))
    files <- files[!grepl('(^|/)[.]git(/|$)|__pycache__|[.]pyc$',files)]
    files <- files[!file.info(file.path(root,files))$isdir]
    setNames(vapply(file.path(root,files),graphmode_c8_hash,''),files)
}
graphmode_guard <- function(r) {
    unsigned <- r;unsigned$signature <- NULL
    if(!identical(graphmode_digest(unsigned),r$signature) || !identical(graphmode_identity(source_root),r$source_sha256)) stop('Frozen source or registration changed.')
    if(!is.null(r$packages)) {
        current <- vapply(names(r$packages),function(p) as.character(packageVersion(p)),'')
        if(!identical(current,r$packages)) stop('Registered package versions changed.')
    }
    if(any(Sys.getenv(c('OMP_NUM_THREADS','OPENBLAS_NUM_THREADS','MKL_NUM_THREADS','VECLIB_MAXIMUM_THREADS'))!='1')) stop('Require single numeric threads.')
}
graphmode_register <- function(directory,smoke=FALSE) {
    if(file.exists(directory)) stop('Destination must not exist; never reuse old results.')
    directory <- file.path(normalizePath(dirname(directory),mustWork=TRUE),basename(directory))
    if(startsWith(directory,paste0(source_root,'/'))) stop('Keep output outside source tree.')
    plan <- graphmode_plan(smoke)
    commit <- suppressWarnings(system2('git',c('-C',shQuote(source_root),'rev-parse','HEAD'),stdout=TRUE,stderr=FALSE))
    if(!smoke) {
        dirty <- system2('git',c('-C',shQuote(source_root),'status','--porcelain','--untracked-files=all','--','.'),stdout=TRUE)
        if(length(commit)!=1L || !grepl('^[0-9a-f]{40}$',commit) || length(dirty)) stop('Formal core run requires a clean committed source tree.')
    }
    r <- list(plan=plan,source_sha256=graphmode_identity(source_root),
        source_commit=if(length(commit)==1L) commit else 'uncommitted-smoke-only',
        created=format(Sys.time(),'%Y-%m-%dT%H:%M:%S%z'),runtime=capture.output(sessionInfo()),
        packages=setNames(vapply(c('BayesLogit','posterior','digest','jsonlite','microbenchmark'),function(p) as.character(packageVersion(p)),''),
            c('BayesLogit','posterior','digest','jsonlite','microbenchmark')))
    r$signature <- graphmode_digest(r)
    dir.create(directory);dir.create(file.path(directory,'jobs'))
    graphmode_store(r,file.path(directory,'registration.rds'))
    graphmode_joint_run_json(r,file.path(directory,'registration.json'),TRUE)
    write.csv(plan$jobs,file.path(directory,'jobs.csv'),row.names=FALSE)
    write.csv(data.frame(role=names(plan$seeds),seed=unname(plan$seeds)),file.path(directory,'seeds.csv'),row.names=FALSE)
    graphmode_generate(plan,directory)
    graphmode_store(list(signature=r$signature),file.path(directory,'generation-complete.rds'))
    cat('REGISTERED',r$signature,'\n');r
}
graphmode_worker <- function(directory,index,target,step_override=NULL,check_override=NULL) {
    r <- graphmode_read(file.path(directory,'registration.rds'));graphmode_guard(r);p <- r$plan
    job <- p$jobs[index,];stopifnot(nrow(job)==1L,target %in% c(p$initial_iterations,p$max_iterations))
    b <- graphmode_read(file.path(directory,job$dataset,'blind.rds'));fit <- b$fits[[job$method]]
    path <- file.path(directory,'jobs',job$id);dir.create(path,showWarnings=FALSE)
    step <- if(is.null(fit$bandwidth)) graphmode_c8_step else graphmode_h_step
    check <- if(is.null(fit$bandwidth)) graphmode_c8_check_transition else graphmode_h_check_transition
    if(!is.null(step_override)) step <- step_override
    if(!is.null(check_override)) check <- check_override
    record <- list(base_policy=p$base_policy,thresholds=p$thresholds)
    states <- diagnostics <- list();started <- proc.time()[3L];last <- NULL
    if(target==p$initial_iterations) {
        if(file.exists(file.path(path,'initial.rds'))) stop('Initial run already exists; no retry.')
        state <- b$starts[[job$method]][[job$chain]]
        set.seed(job$seed);graphmode_store(list(state=state,rng=.Random.seed,job=job),file.path(path,'initial.rds'))
    } else {
        receipt <- graphmode_read(file.path(path,sprintf('receipt-%d.rds',p$initial_iterations)))
        stopifnot(receipt$signature==r$signature,receipt$target==p$initial_iterations)
        for(n in names(receipt$chunks)) stopifnot(graphmode_c8_hash(file.path(path,n))==receipt$chunks[[n]])
        last <- graphmode_read(file.path(path,sprintf('chunk-%05d.rds',p$initial_iterations)))
        state <- last$state;assign('.Random.seed',last$rng,.GlobalEnv)
    }
    start <- state$core$iteration
    tryCatch(withCallingHandlers({
        for(i in seq.int(start+1L,target)) {
            before <- state;out <- step(before,fit);check(before,out,fit,record);state <- out$state
            stopifnot(state$core$iteration==i)
            states[[length(states)+1L]] <- state
            diagnostics[[length(diagnostics)+1L]] <- list(base=out$base$base_diagnostic,events=out$base$events,
                q_update=out$q_update,scale_update=out$scale_update,h_update=out$h_update)
            if(i%%p$chunk==0L || i==target) {
                graphmode_store(list(signature=r$signature,end_iteration=i,state=state,rng=.Random.seed,
                    states=states,diagnostics=diagnostics),file.path(path,sprintf('chunk-%05d.rds',i)))
                states <- diagnostics <- list()
            }
            if(i%%25L==0L || i==target) {
                graphmode_joint_run_json(list(iteration=i,target=target,elapsed=unname(proc.time()[3L]-started)),file.path(path,'progress.json'))
                cat(job$id,i,'/',target,'elapsed',proc.time()[3L]-started,'\n');flush.console()
            }
        }
        chunks <- list.files(path,'^chunk-[0-9]+[.]rds$')
        graphmode_store(list(signature=r$signature,target=target,warmup=p$warmup,
            chunks=setNames(vapply(file.path(path,chunks),graphmode_c8_hash,''),chunks),
            seconds=unname(proc.time()[3L]-started),fit_hash=graphmode_digest(fit)),
            file.path(path,sprintf('receipt-%d.rds',target)))
    },warning=function(w) {cat('WARNING',conditionMessage(w),'\n');flush.console();invokeRestart('muffleWarning')}),error=function(e) {
        graphmode_store(list(error=conditionMessage(e),state=state,rng=.Random.seed,pending_states=states,
            diagnostics=diagnostics,iteration=state$core$iteration),file.path(path,sprintf('failure-%d.rds',target)))
        stop(e)
    })
}

graphmode_chain <- function(directory,job,r,target,fit) {
    path <- file.path(directory,'jobs',job$id);receipt <- graphmode_read(file.path(path,sprintf('receipt-%d.rds',target)))
    stopifnot(receipt$signature==r$signature,receipt$fit_hash==graphmode_digest(fit),receipt$target==target,receipt$warmup==r$plan$warmup)
    chunks <- names(receipt$chunks);expected <- seq(r$plan$chunk,target,by=r$plan$chunk)
    stopifnot(identical(chunks,sprintf('chunk-%05d.rds',expected)))
    draws <- list()
    for(n in chunks) {
        stopifnot(graphmode_c8_hash(file.path(path,n))==receipt$chunks[[n]])
        x <- graphmode_read(file.path(path,n));stopifnot(x$signature==r$signature)
        draws <- c(draws,Filter(function(s) s$core$iteration>r$plan$warmup,x$states))
    }
    stopifnot(identical(vapply(draws,function(s) s$core$iteration,1L),seq.int(r$plan$warmup+1L,target)))
    list(draws=draws)
}

graphmode_summary <- function(directory,target) {
    r <- graphmode_read(file.path(directory,'registration.rds'));graphmode_guard(r)
    path <- file.path(directory,paste0('summary-',target));if(!dir.create(path)) stop('Summary already exists.')
    rows <- list()
    for(d in sprintf('D%02d',1:2)) for(m in c('Exp-W','SE-W','C')) {
        cat('SUMMARIZING',d,m,'\n');flush.console()
        b <- graphmode_read(file.path(directory,d,'blind.rds'));fit <- b$fits[[m]]
        jobs <- r$plan$jobs[r$plan$jobs$dataset==d & r$plan$jobs$method==m,]
        chains <- lapply(1:4,function(j) graphmode_chain(directory,jobs[j,],r,target,fit))
        diag <- if(is.null(fit$bandwidth)) graphmode_c8_diagnostics(chains,fit,b$input$ids) else graphmode_h_diagnostics(chains,fit,b$input$ids)
        draws <- unlist(lapply(chains,`[[`,'draws'),recursive=FALSE)
        truth <- graphmode_read(file.path(directory,d,'evaluation-only.rds'))
        Z <- do.call(rbind,lapply(draws,function(x) x$core$Z));cl <- graphmode_c8_partition_summary(Z,truth$Z)
        predict <- if(is.null(fit$bandwidth)) graphmode_c8_predict else graphmode_h_predict
        prediction <- predict(draws,fit,b$input$Ffuture)
        ij <- which(upper.tri(b$input$adjacency)&b$input$adjacency==1,arr.ind=TRUE)
        cross <- truth$Z[ij[,1]]!=truth$Z[ij[,2]];cut <- cl$Z[ij[,1]]!=cl$Z[ij[,2]]
        report <- list(diagnostics=diag,classification=cl,prediction=prediction,
            boundary_errors=sum(!cut & cross),internal_cuts=sum(cut & !cross))
        graphmode_store(report,file.path(path,paste0(d,'-',m,'.rds')))
        write.csv(diag$scalars,file.path(path,paste0(d,'-',m,'-scalars.csv')),row.names=FALSE)
        rows[[length(rows)+1L]] <- data.frame(dataset=d,method=m,iterations=target,valid=diag$valid,
            failed_scalars=sum(!diag$scalars$status %in% c('passed','uninformative-constant-discrete')),
            max_psm_rms=max(diag$psm_rms$rms),ARI=cl$ARI,Khat=cl$Khat,Kocc_mean=mean(cl$Kocc),
            boundary_errors=report$boundary_errors,internal_cuts=report$internal_cuts,
            boundary_merge_probability=mean(cl$PSM[ij[cross,,drop=FALSE]]),
            internal_cut_probability=mean(1-cl$PSM[ij[!cross,,drop=FALSE]]),
            RMSE=unname(graphmode_c8_prediction_score(prediction,truth$Y)['RMSE']))
        rm(chains,draws,Z);invisible(gc())
    }
    table <- do.call(rbind,rows);write.csv(table,file.path(path,'fits.csv'),row.names=FALSE)
    pairs <- do.call(rbind,lapply(sprintf('D%02d',1:2),function(d) {
        x <- table[table$dataset==d,]
        contrasts <- list(c('Exp-W','C'),c('SE-W','C'),c('SE-W','Exp-W'))
        do.call(rbind,lapply(contrasts,function(pair) {
            w <- x[x$method==pair[1],];c <- x[x$method==pair[2],]
            rw <- graphmode_read(file.path(path,paste0(d,'-',pair[1],'.rds')))
            rc <- graphmode_read(file.path(path,paste0(d,'-',pair[2],'.rds')))
            data.frame(dataset=d,contrast=paste(pair[1],'minus',pair[2]),ARI=w$ARI-c$ARI,
                Khat=w$Khat-c$Khat,RMSE=w$RMSE-c$RMSE,boundary_errors=w$boundary_errors-c$boundary_errors,
                internal_cuts=w$internal_cuts-c$internal_cuts,
                PSM_RMS=graphmode4_psm_rms(rw$classification$PSM,rc$classification$PSM))
        }))
    }))
    write.csv(pairs,file.path(path,'paired.csv'),row.names=FALSE)
    graphmode_joint_run_json(list(target=target,all_passed=all(table$valid),
        decision=if(all(table$valid)) '4000 precision gate passed; no automatic extension' else '4000 precision gate failed; any extension needs an explicit decision'),
        file.path(path,'decision.json'),TRUE)
}
