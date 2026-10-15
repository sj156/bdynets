# Exact selected definitions from R/graphmode_c8_reporting.R
graphmode_c8_diagnostics <- function(chains,fit,ids) {
    # Reuse the existing scalar definitions only; this temporary trace adapter
    # does not relabel C8 as a historical fixed-scale sampling run.
    record <- list(protocol=graphmode4_protocol,core=fit$core,ids=ids)
    record$signature <- graphmode_digest(record)
    traces <- lapply(chains,function(chain) {
        x <- graphmode4_trace(lapply(chain$draws,`[[`,"core"),record)
        x$scalars <- cbind(x$scalars,log_q=vapply(chain$draws,`[[`,0,"log_q"))
        if(!is.null(fit$kernel)) x$scalars <- cbind(x$scalars,
            log_kernel_scale=vapply(chain$draws,`[[`,0,"log_scale"))
        x
    })
    scalar <- do.call(rbind,lapply(colnames(traces[[1]]$scalars),function(name) {
        matrix <- do.call(cbind,lapply(traces,function(x) x$scalars[,name]))
        data.frame(scalar=name,as.data.frame(graphmode4_scalar_diagnostic(matrix,name %in% traces[[1]]$discrete)))
    }))
    pairs <- combn(1:4,2)
    psm <- data.frame(chain_a=pairs[1,],chain_b=pairs[2,],rms=apply(pairs,2,function(pair)
        graphmode4_psm_rms(traces[[pair[1]]]$similarity,traces[[pair[2]]]$similarity)))
    list(scalars=scalar,psm_rms=psm,traces=traces,
        valid=all(scalar$status %in% c("passed","uninformative-constant-discrete")) && all(psm$rms<=.05),
        convergence_certified=FALSE,version=graphmode_c8_execution_version)
}

graphmode_c8_prediction_score <- function(prediction,future) {
    if(!identical(dim(prediction),dim(future)) || !all(is.finite(prediction)) || any(prediction<0))
        stop("Invalid prediction shape/value.")
    error <- prediction-future
    c(RMSE=sqrt(mean(error^2)),MAE=mean(abs(error)),
        setNames(vapply(c(1,6,24,48),function(h) sqrt(mean(error[,h]^2)),0),paste0("RMSE_h",c(1,6,24,48))),
        setNames(vapply(c(1,6,24,48),function(h) mean(abs(error[,h])),0),paste0("MAE_h",c(1,6,24,48))))
}

graphmode_c8_partition_summary <- function(Z,truth) {
    similarity <- matrix(0,ncol(Z),ncol(Z))
    for(i in seq_len(nrow(Z))) similarity <- similarity + outer(Z[i,],Z[i,],"==")/nrow(Z)
    loss <- vapply(seq_len(nrow(Z)),function(i) sum((outer(Z[i,],Z[i,],"==")-similarity)^2),0)
    representative <- which.min(loss)
    exact <- vapply(seq_len(nrow(Z)),function(i) all(outer(Z[i,],Z[i,],"==")==outer(truth,truth,"==")),TRUE)
    list(Z=as.integer(Z[representative,]),representative_index=representative,PSM=similarity,
        ARI=gmde_adjusted_rand(truth,Z[representative,]),Khat=length(unique(Z[representative,])),
        exact=exact,exact_fraction=mean(exact),exact_denominator=nrow(Z),
        Kocc=apply(Z,1,function(z) length(unique(z))))
}
