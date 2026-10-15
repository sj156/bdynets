# Exact selected definitions from R/graphmode_h_reporting.R
graphmode_h_diagnostics <- function(chains,fit,ids) {
    # Reuse the existing scalar definitions only; this temporary trace adapter
    # does not relabel C8 as a historical fixed-scale sampling run.
    record <- list(protocol=graphmode4_protocol,core=fit$core,ids=ids)
    record$signature <- graphmode_digest(record)
    traces <- lapply(chains,function(chain) {
        x <- graphmode4_trace(lapply(chain$draws,`[[`,"core"),record)
        x$scalars <- cbind(x$scalars,log_q=vapply(chain$draws,`[[`,0,"log_q"))
        if(!is.null(fit$kernel)) x$scalars <- cbind(x$scalars,
            log_kernel_scale=vapply(chain$draws,`[[`,0,"log_scale"))
        x$scalars <- cbind(x$scalars,log_bandwidth=vapply(chain$draws,`[[`,0,"log_eta")+log(fit$bandwidth$geometry$d_ref))
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
        convergence_certified=FALSE,version=graphmode_h_version)
}

graphmode_h_predict <- function(draws, fit, Ffuture) {
    c <- fit$core; stopifnot(identical(c$G, diag(ncol(c$Fmat))), length(draws) > 0L)
    graphmode_matrix(Ffuture, "future design")
    if (ncol(Ffuture) != ncol(c$Fmat)) stop("Future design dimension differs.")
    result <- matrix(0, c$n, nrow(Ffuture)); TT <- nrow(c$Fmat); p <- ncol(Ffuture)
    # Exponential-tailed Gamma q prior ensures these finite-horizon means exist.
    if (fit$prior$q_rate <= max((TT + seq_len(nrow(Ffuture))) * rowSums(Ffuture^2) / 2))
        stop("Innovation prior does not guarantee a finite predictive mean at this horizon.")
    for (d in draws) {
      graphmode_h_validate_state(d, fit)
      for (h in seq_len(nrow(Ffuture))) {
        core <- d$core; f <- Ffuture[h, ]; q <- exp(d$log_q)
        means <- exp(as.vector(matrix(core$theta[, TT, ], c$K, p) %*% f) + h*q*sum(f^2)/2)
        result[, h] <- result[, h] + means[core$Z] / length(draws)
      }
    }
    if (any(!is.finite(result) | result <= 0)) stop("Invalid predictive mean.")
    result
}
