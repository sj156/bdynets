# Authorized controlled binary64 approximation, not interval-certified MH.
# Logical edges and finite normalized log weights are retained. There is no
# user-selected weight cutoff. Every failed check raises an error, never MH rejection.
graphmode_h_numerical_policy <- function() list(
    version="binary64-underflow-audit-v1",underflow_loglik_max=1e-12,
    target_difference_max=1e-8,eigen_residual_coefficient=256,
    covariance_residual_coefficient=1024)

graphmode_h_numerical_sink <- NULL

graphmode_h_audit_emit <- function(record) {
    sink <- graphmode_h_numerical_sink
    if(is.function(sink)) sink(record) else {
        cat("NUMERICAL_AUDIT ",jsonlite::toJSON(record,auto_unbox=TRUE,digits=17,null="null"),"\n",sep="")
        flush.console()
    }
    invisible(record)
}

graphmode_h_underflow_record <- function(log_weights,values,geometry) {
    policy <- graphmode_h_numerical_policy();zero <- which(values==0)
    # Half the smallest positive subnormal is the round-to-zero boundary.
    log_boundary <- log(.Machine$double.xmin)+log(.Machine$double.eps)-log(2)
    if(any(!is.finite(log_weights)) || any(log_weights[zero]>log_boundary))
        stop("Numerical audit: zero weight is not genuine binary64 underflow.")
    small <- log_weights[zero];largest <- max(small)
    log_epsilon <- log(2)+largest+log(sum(exp(small-largest)))
    list(policy=policy,logical_edges=unname(geometry$edges),
      normalized_log_weights=unname(log_weights),zero_indices=zero,
      zero_edge_count=length(zero),physical_edge_count=nrow(geometry$edges),
      log_round_to_zero_boundary=log_boundary,log_laplacian_error_bound=log_epsilon,
      normalized_weight_mean=mean(values),
      approximation="Only unrepresentable weights rounded to zero in numerical matrix; logical support unchanged")
}

graphmode_h_spectrum_audit <- function(record,L,e) {
    n <- nrow(L);tolerance <- record$policy$eigen_residual_coefficient*n*.Machine$double.eps
    residual <- norm(L %*% e$vectors-sweep(e$vectors,2,e$values,"*"),"F")/max(1,norm(L,"F"))
    orthogonality <- norm(crossprod(e$vectors)-diag(n),"F")
    record$eigen_relative_residual <- residual
    record$eigenvector_orthogonality_residual <- orthogonality
    record$eigen_diagnostic_limit <- tolerance
    record$eigenvalue_range <- range(e$values)
    if(any(!is.finite(c(residual,orthogonality))) || max(residual,orthogonality)>tolerance) {
        record$stage <- "spectrum";record$status <- "failed";graphmode_h_audit_emit(record)
        stop("Numerical audit: eigensolver residual exceeds frozen diagnostic limit.")
    }
    record$L <- L;record
}

graphmode_h_root_audit <- function(context,Phi,log_scale) {
    record <- context$numerics;n <- nrow(Phi)
    if(context$nu!=1 || context$tau<=0) stop("Numerical audit bound requires nu=1 and positive tau.")
    a <- 2*context$nu/exp(log_scale)^2
    if(!is.finite(a) || a<=0) stop("Numerical audit: nonpositive or unrepresentable regularizer.")
    B <- diag(a,n)+record$L
    inverse <- chol2inv(chol(B))
    covariance <- n*context$tau^2*inverse/sum(diag(inverse))
    covariance <- (covariance+t(covariance))/2
    alternate <- eigen(covariance,symmetric=TRUE)
    if(any(!is.finite(alternate$values)) || min(alternate$values)<=0)
        stop("Numerical audit: alternate covariance is not numerically positive definite.")
    Phi_alternate <- sweep(alternate$vectors,2,sqrt(alternate$values),"*") %*% t(alternate$vectors)
    residual <- norm(tcrossprod(Phi)-covariance,"F")/max(1,norm(covariance,"F"))
    tolerance <- record$policy$covariance_residual_coefficient*n*.Machine$double.eps
    record$a_kernel <- a;record$nu <- context$nu;record$tau <- context$tau
    record$kappa <- exp(log_scale)
    record$regularized_condition_estimate <- (a+max(context$values))/a
    record$covariance_relative_residual <- residual;record$covariance_diagnostic_limit <- tolerance
    record$log_root_error_bound <- record$log_laplacian_error_bound-log(a)+
        .5*log(n*context$tau^2)-log(2)
    record$L <- NULL
    if(!is.finite(residual) || residual>tolerance) {
        record$stage <- "root";record$status <- "failed";graphmode_h_audit_emit(record)
        stop("Numerical audit: covariance residual exceeds frozen diagnostic limit.")
    }
    record$Phi_alternate <- Phi_alternate;record
}

graphmode_h_target_audit <- function(value,core_state,Phi,config) {
    record <- attr(Phi,"graphmode_numerics");alternate <- config
    alternate$Phi <- record$Phi_alternate;record$Phi_alternate <- NULL
    alternate_value <- graphmode_gate_loglik(
        graphmode_gate_utilities(core_state$x,core_state$v,alternate),core_state$Z)
    d <- config$K-1L
    Gamma <- matrix(core_state$x[d+seq_len(ncol(Phi)*d)],ncol(Phi),d)
    factors <- graphmode_guidance_factors(core_state$v,config$K,config$guidance)
    norm_guided <- norm(Gamma %*% t(factors$g),"F")
    log_bound <- record$log_root_error_bound+log(norm_guided)+.5*log(2*config$n)
    target_difference <- abs(value-alternate_value)
    record$iteration <- core_state$iteration;record$Gamma_guided_frobenius <- norm_guided
    record$log_gate_underflow_error_bound <- log_bound
    record$gate_target <- value;record$alternate_gate_target <- alternate_value
    record$gate_target_difference <- target_difference
    record$stage <- "target"
    record$strict_interval_certification <- FALSE
    ok <- !is.na(log_bound) && log_bound<=log(record$policy$underflow_loglik_max) &&
        is.finite(target_difference) && target_difference<=record$policy$target_difference_max
    record$status <- if(ok) "passed_diagnostics" else "failed"
    graphmode_h_audit_emit(record)
    if(!ok) stop("Numerical audit: target diagnostic exceeds frozen limit; stop, do not MH-reject.")
    invisible(record)
}
