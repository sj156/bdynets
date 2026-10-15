# Reporting only: evaluation labels and held-out counts enter here, after fitting.
graphmode_finished_core <- function(r,d,m) {
    out <- if(m=='C') r$core_results$C else r$core_results$W
    reg <- graphmode_read(file.path(out,'registration.rds'))
    b <- graphmode_read(file.path(if(m=='C') out else r$core_input,d,'blind.rds'))
    fit <- b$fits[[m]];jobs <- if(m=='C') reg$plan$jobs else reg$jobs
    jobs <- jobs[jobs$dataset==d & jobs$method==m,];jobs <- jobs[order(jobs$chain),]
    stopifnot(nrow(jobs)==4L)
    chains <- lapply(1:4,function(j) {
        if(m=='C') return(graphmode_chain(out,jobs[j,],reg,4000L,fit))
        receipt <- graphmode_read(file.path(out,'jobs',jobs$id[j],'receipt-4000.rds'))
        stopifnot(receipt$parent_signature==reg$parent_signature,receipt$continuation_signature==reg$signature,
            receipt$target==4000L,receipt$warmup==2000L,receipt$fit_hash==graphmode_digest(fit))
        paths <- names(receipt$chunks)
        stopifnot(identical(basename(paths),sprintf('chunk-%05d.rds',seq(100L,4000L,100L))))
        draws <- list()
        for(i in seq_along(paths)) {
            f <- paths[i];stopifnot(graphmode_c8_hash(f)==receipt$chunks[[f]])
            x <- graphmode_read(f)
            stopifnot(x$signature==reg$parent_signature,x$end_iteration==i*100L,
                identical(vapply(x$states,function(s) s$core$iteration,1L),seq.int(i*100L-99L,i*100L)))
            draws <- c(draws,Filter(function(s) s$core$iteration>2000L,x$states))
        }
        stopifnot(identical(vapply(draws,function(s) s$core$iteration,1L),2001:4000))
        list(draws=draws)
    })
    diagnostic <- graphmode_read(file.path(out,'summary-4000',paste0(d,'-',m,'-diagnostics.rds')))
    list(chains=chains,fit=fit,diagnostics=diagnostic)
}

graphmode_extra_summary <- function(directory) {
    r <- graphmode_read(file.path(directory,'registration.rds'));graphmode_extra_guard(r)
    path <- file.path(directory,'summary');dir.create(path,showWarnings=FALSE)
    rows <- list();all_jobs <- r$plan$jobs
    methods <- c('EucMoDE','MoDE','PottsMoDE','STGNN','PNAR')
    if(!is.null(r$core_results)) methods <- c('Exp-W','SE-W','C',methods)
    for(d in sprintf('D%02d',1:2)) for(m in methods) {
        cat('SUMMARIZING',d,m,'\n');flush.console()
        b <- graphmode_read(file.path(directory,d,'blind.rds'))
        truth <- graphmode_read(file.path(directory,d,'evaluation-only.rds'))
        jobs <- all_jobs[all_jobs$dataset==d & all_jobs$method==m & all_jobs$kind %in% c('bayes','stgnn_final','pnar_final'),]
        status <- vapply(jobs$id,function(id) isTRUE(jsonlite::fromJSON(file.path(directory,'jobs',id,'process.json'))$exit_code==0),TRUE)
        row <- data.frame(dataset=d,method=m,status='failed',valid=FALSE,failed_jobs=sum(!status),
            max_psm_rms=NA_real_,ARI=NA_real_,Khat=NA_integer_,Kocc_mean=NA_real_,boundary_errors=NA_integer_,internal_cuts=NA_integer_,
            boundary_merge_probability=NA_real_,internal_cut_probability=NA_real_,RMSE=NA_real_)
        is_core <- m %in% c('Exp-W','SE-W','C')
        core <- if(is_core) tryCatch(graphmode_finished_core(r,d,m),error=function(e) list(error=conditionMessage(e))) else NULL
        if(!all(status) || !is.null(core$error)) {
            row$failed_jobs <- if(is_core) NA_integer_ else sum(!status)
            if(!is.null(core$error)) graphmode_joint_run_json(core,file.path(path,paste0(d,'-',m,'-unavailable.json')),TRUE)
            rows[[length(rows)+1L]] <- row;next
        }
        report <- list()
        if(m %in% c('Exp-W','SE-W','C','EucMoDE','MoDE','PottsMoDE')) {
            fit <- if(is_core) core$fit else graphmode_extra_selected_fit(directory,b,jobs[1,])
            chains <- if(is_core) core$chains else lapply(1:4,function(j) graphmode_chain(directory,jobs[j,],r,4000L,fit))
            diag <- if(is_core) core$diagnostics else graphmode_c8_diagnostics(chains,fit,b$input$ids)
            draws <- unlist(lapply(chains,`[[`,'draws'),recursive=FALSE)
            Z <- do.call(rbind,lapply(draws,function(x) x$core$Z))
            prediction <- if(m %in% c('Exp-W','SE-W')) graphmode_h_predict(draws,fit,b$input$Ffuture) else graphmode_c8_predict(draws,fit,b$input$Ffuture)
            report$diagnostics <- diag;row$valid <- diag$valid;row$max_psm_rms <- max(diag$psm_rms$rms)
            write.csv(diag$scalars,file.path(path,paste0(d,'-',m,'-scalars.csv')),row.names=FALSE)
            rm(chains,draws,core);invisible(gc())
        } else if(m=='STGNN') {
            fits <- lapply(jobs$id,function(id) {
                f <- graphmode_c8_artifact(file.path(directory,'jobs',id),'result.json')
                jsonlite::fromJSON(f)
            })
            Z <- do.call(rbind,lapply(fits,`[[`,'labels'))
            prediction <- Reduce(`+`,lapply(fits,function(x) t(x$prediction)))/length(fits)
            report$restarts <- lapply(fits,function(f) f[setdiff(names(f),'prediction')])
            row$valid <- TRUE
        } else {
            result <- graphmode_read(file.path(directory,'jobs',jobs$id,'result.rds'))
            prediction <- result$fit$prediction;Z <- NULL;row$valid <- TRUE;report$fit <- result$fit
        }
        if(!is.null(Z)) {
            cl <- graphmode_c8_partition_summary(Z,truth$Z)
            ij <- which(upper.tri(b$input$adjacency)&b$input$adjacency==1,arr.ind=TRUE)
            cross <- truth$Z[ij[,1]]!=truth$Z[ij[,2]];cut <- cl$Z[ij[,1]]!=cl$Z[ij[,2]]
            report$classification <- cl
            row$ARI <- cl$ARI;row$Khat <- cl$Khat;row$Kocc_mean <- mean(cl$Kocc)
            row$boundary_errors <- sum(!cut & cross);row$internal_cuts <- sum(cut & !cross)
            row$boundary_merge_probability <- mean(cl$PSM[ij[cross,,drop=FALSE]])
            row$internal_cut_probability <- mean(1-cl$PSM[ij[!cross,,drop=FALSE]])
        }
        row$RMSE <- unname(graphmode_c8_prediction_score(prediction,truth$Y)['RMSE'])
        row$status <- if(m %in% c('STGNN','PNAR')) 'completed_no_MCMC_precision_claim' else if(row$valid) 'precision_passed' else 'precision_failed'
        report$prediction <- prediction;graphmode_store(report,file.path(path,paste0(d,'-',m,'.rds')))
        rows[[length(rows)+1L]] <- row
    }
    table <- do.call(rbind,rows);write.csv(table,file.path(path,'fits.csv'),row.names=FALSE)
    pairs <- list()
    for(d in sprintf('D%02d',1:2)) for(pair in list(c('Exp-W','C'),c('SE-W','C'),c('SE-W','Exp-W'))) {
        a <- table[table$dataset==d & table$method==pair[1],];b <- table[table$dataset==d & table$method==pair[2],]
        if(nrow(a)!=1L || nrow(b)!=1L || a$status=='failed' || b$status=='failed') next
        ra <- graphmode_read(file.path(path,paste0(d,'-',pair[1],'.rds')))
        rb <- graphmode_read(file.path(path,paste0(d,'-',pair[2],'.rds')))
        pairs[[length(pairs)+1L]] <- data.frame(dataset=d,contrast=paste(pair,collapse=' minus '),precision_passed=a$valid & b$valid,
            ARI=a$ARI-b$ARI,Khat=a$Khat-b$Khat,Kocc_mean=a$Kocc_mean-b$Kocc_mean,RMSE=a$RMSE-b$RMSE,
            boundary_errors=a$boundary_errors-b$boundary_errors,internal_cuts=a$internal_cuts-b$internal_cuts,
            PSM_RMS=graphmode4_psm_rms(ra$classification$PSM,rb$classification$PSM))
    }
    if(length(pairs)) write.csv(do.call(rbind,pairs),file.path(path,'paired-core.csv'),row.names=FALSE)
    graphmode_joint_run_json(list(target=4000,automatic_extension=FALSE,
        decision='All supplementary jobs terminated; keep failures and compare only with reported uncertainty. Any 8000 stage needs a new explicit decision.'),file.path(path,'decision.json'),TRUE)
}
