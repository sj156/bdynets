# Exact selected definitions from R/graphmode_c9_execution.R
graphmode_c9_base_policy <- function() {
    s <- graphmode_c9_spec()$bayes
    list(gate=graphmode_gate_refresh_policy(s$gate_scans,s$guidance_proposal_sd),
        block=graphmode_expert_blocks_policy(s$block,"uniform"),ess_scheme="contrast",factor_cache=TRUE)
}
