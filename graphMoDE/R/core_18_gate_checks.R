# Exact selected definitions from R/graphmode_gate_blocks_run.R
graphmode_gate_blocks_run_gate_check <- function(g,config,policy) {
    bad <- function() stop("Invalid gate-block scan evidence/budget.",call.=FALSE)
    expected <- graphmode_gate_blocks_policy(config,policy$ess_scheme)
    m <- policy$gate$inner_steps; nb <- length(expected$blocks)
    if(!identical(g$schema,graphmode_gate_blocks_version) || !identical(g$ess_policy,expected) ||
        !is.list(g$ess_scans) || length(g$ess_scans)!=m || !is.list(g$records) || length(g$records)!=m) bad()
    for(j in seq_len(m)) {
        scan <- g$ess_scans[[j]]
        if(!is.list(scan) || length(scan)!=nb) bad()
        for(h in seq_len(nb)) {
            b <- scan[[h]]
            if(!is.list(b) || !identical(names(b),c("block","evaluations","jump_squared")) || !identical(b$block,h) ||
                !is.numeric(b$evaluations) || length(b$evaluations)!=1L || !is.finite(b$evaluations) ||
                b$evaluations<1 || b$evaluations!=floor(b$evaluations) ||
                !is.numeric(b$jump_squared) || length(b$jump_squared)!=1L || !is.finite(b$jump_squared) || b$jump_squared<0) bad()
        }
        ne <- sum(vapply(scan,`[[`,numeric(1),"evaluations"))
        jump <- sum(vapply(scan,`[[`,numeric(1),"jump_squared"))
        if(ne>config$max_ess_steps || !identical(ne,as.numeric(g$records[[j]]$ess_evaluations)) ||
            !isTRUE(all.equal(jump,g$records[[j]]$x_jump_squared,tolerance=1e-12))) bad()
    }
    invisible(TRUE)
}
