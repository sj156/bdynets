# Exact historical training-only Potts selection and initialization.
graphmode_c9_potts_select <- function(records,training) {
    graphmode_matrix(training,"training prefix only")
    if(ncol(training)!=168L) stop("Use exactly 168 training times, never test observations.")
    spec <- graphmode_c9_spec()$potts
    expected <- expand.grid(beta=spec$beta,origin=spec$origins,chain=spec$chains)
    keys <- c("beta","origin","chain")
    key <- function(x) do.call(paste,c(x[keys],sep=":"))
    if(!is.data.frame(records) || !all(c(keys,"valid","prediction") %in% names(records)) ||
        anyDuplicated(key(records)) || !setequal(key(records),key(expected)))
        stop("Missing or duplicated Potts validation cells; failures must have explicit rows.")
    good <- records$valid %in% TRUE & vapply(records$prediction,function(p)
        is.matrix(p) && identical(dim(p),c(nrow(training),48L)) && all(is.finite(p) & p>=0),TRUE)
    scores <- list()
    for(beta in spec$beta) {
        keep <- records$beta==beta
        if(!all(good[keep])) next
        sse <- 0; count <- 0L
        for(origin in spec$origins) {
            ii <- which(keep & records$origin==origin)
            # Ensemble the retained-draw means, THEN score. Do not average
            # chain-specific SSEs, which would also penalize Monte Carlo noise.
            pred <- Reduce(`+`,records$prediction[ii])/length(ii)
            truth <- training[,origin+seq_len(48L),drop=FALSE]
            sse <- sse+sum((pred-truth)^2); count <- count+length(truth)
        }
        scores[[length(scores)+1L]] <- data.frame(beta=beta,sse=sse,n=count,RMSE=sqrt(sse/count))
    }
    if(!length(scores)) stop("No complete valid Potts candidate; no fallback or scientific retry.")
    scores <- do.call(rbind,scores); best <- min(scores$RMSE)
    ties <- scores[scores$RMSE<=best+1e-10*max(1,abs(best)),]
    list(selected=ties[which.min(ties$beta),,drop=FALSE],scores=scores,
        excluded=setdiff(spec$beta,scores$beta),records=records,
        rule="Two training folds; predictive means pooled across two complete chains; numerical ties favor smaller beta")
}

graphmode_c9_initial <- function(fit,chain,seeds,prefix) {
    # Potts validation starts depend only on the training prefix. The same start
    # tapes are reused across beta candidates and correct/noisy input graphs.
    core <- fit$core; n <- core$n; K <- core$K; occ <- graphmode_c9_spec()$bayes$start_occupancy[chain]
    Z <- graphmode_pilot_seeded(seeds[[paste0(prefix,"-Z_",sprintf("%02d",chain))]],{
        labels <- sample.int(K,occ,replace=FALSE);rep(labels,length.out=n)[sample.int(n)]
    })
    theta <- graphmode_pilot_seeded(seeds[[paste0(prefix,"-theta_",sprintf("%02d",chain))]],{
        result <- array(NA_real_,c(K,nrow(core$Fmat),ncol(core$Fmat)))
        for(k in seq_len(K)) result[k,,] <- graphmode_prior_expert(core$Fmat,core$m0,core$C0,core$G,core$W,core$dynamics)
        result
    })
    state <- graphmode_c8_initial(fit,Z,theta)
    state$log_q <- state$log_q+graphmode_c9_spec()$bayes$start_q_log_offsets[chain]
    graphmode_c8_validate_state(state,fit);state
}

