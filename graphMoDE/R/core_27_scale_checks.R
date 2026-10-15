# Exact selected definitions from R/graphmode_c8_workers.R
graphmode_c8_check_transition <- function(before,out,fit,r) {
    config <- graphmode_c8_current_config(fit,before$log_scale,before$log_q)
    graphmode_compare_check(before$core,out$base,config,r)
    graphmode_c8_validate_state(out$state,fit)
    if(!identical(out$base$state,out$state$core)) stop("Hyperparameter update changed base scientific state.")
    for(name in c("q_update","scale_update")) if(!is.null(out[[name]])) {
        mh <- out[[name]]; old <- if(name=="q_update") before$log_q else before$log_scale
        actual <- if(name=="q_update") out$state$log_q else out$state$log_scale
        if(length(mh$accepted)!=1L || !is.logical(mh$accepted) || is.na(mh$accepted) ||
            any(!is.finite(c(mh$value,mh$proposed,mh$log_ratio))) ||
            !identical(actual,if(mh$accepted) mh$proposed else old)) stop("Invalid scale MH handoff.")
    }
    invisible(TRUE)
}
