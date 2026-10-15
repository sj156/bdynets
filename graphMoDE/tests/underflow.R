check('true double underflow preserves logical support and every finite log weight',{
    f <- fits[[2]];z <- -2.2
    W <- api$graphmode_h_weight(f$bandwidth$geometry,'se',z)
    audit <- attr(W,'graphmode_numerics')
    stopifnot(!is.null(audit),audit$physical_edge_count==120L,
      all(is.finite(audit$normalized_log_weights)),length(audit$normalized_log_weights)==120L,
      all(audit$normalized_log_weights[audit$zero_indices]<=audit$log_round_to_zero_boundary),
      identical(audit$logical_edges,unname(f$bandwidth$geometry$edges)))
    bad <- audit$normalized_log_weights;bad[audit$zero_indices[1]] <- -700
    fails(api$graphmode_h_underflow_record(bad,W[f$bandwidth$geometry$edges],f$bandwidth$geometry))
})
check('underflow covariance and target diagnostics are finite and stop on excess error',{
    f <- fits[[2]];raw <- api$graphmode_initial_state(f$core,rep(1L,121))
    raw$x <- .05*sin(seq_along(raw$x))
    s <- api$graphmode_h_common_start(f,raw,2L)
    spectrum <- api$graphmode_h_spectrum(f,-2.2)
    Phi <- api$graphmode_h_root(spectrum,log(2))
    value <- api$graphmode_h_gate_target(s$core,Phi,f$core)
    stopifnot(is.finite(value),!is.null(attr(Phi,'graphmode_numerics')))
    audit <- attr(Phi,'graphmode_numerics');audit$log_root_error_bound <- 100
    attr(Phi,'graphmode_numerics') <- audit
    fails(api$graphmode_h_gate_target(s$core,Phi,f$core))
})
cat('Controlled numerical approximation checks passed; not interval-MH certification.\n')
