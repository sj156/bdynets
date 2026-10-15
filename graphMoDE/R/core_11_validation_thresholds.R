# Exact selected definitions from R/graphmode4-pilot.R
graphmode4_pilot_version <- "graphmode-r4-four-chain-smoke-20260912-v1"

graphmode4_pilot_spec <- function() {
    list(version = graphmode4_pilot_version, protocol = graphmode4_protocol,
        role = "development-pilot", geometry = "intertwined-spiral", truth = "road-voronoi",
        method = "graphMoDE-W", n = 121L, TT = 168L, Ktrue = 5L, Kfit = 10L,
        amplitude = .3, innovation_variance = .002,
        expert = list(m0 = c(log(25), 0, 0), C0 = diag(c(1, .5, .5)),
            family = "poisson", dynamics = "dynamic", G = diag(3), W = diag(.3^2 * .002, 3),
            rho = 2L, variance_prior = NULL),
        gate = list(guidance_prior = c(1, 1), guidance_proposal_sd = .25, max_ess_steps = 1000L),
        start_occupancy = c(1L, 3L, 7L, 10L), workers = 1L,
        iterations = 400L, warmup = 200L, thin = 1L, checkpoint_every = 50L,
        chain_budget_seconds = 600, phase_budget_seconds = 300, total_budget_seconds = 3000,
        seeds = c(innovations = 2026091201L, profile_permutation = 2026091202L,
            unit_permutation = 2026091203L, response = 2026091204L, pg = 2026091205L,
            start_01 = 2026091211L, start_02 = 2026091212L, start_03 = 2026091213L, start_04 = 2026091214L,
            chain_01 = 2026091221L, chain_02 = 2026091222L, chain_03 = 2026091223L, chain_04 = 2026091224L),
        pg_cases = data.frame(b = c(1, 20, 500, 500, 10000), z = c(0, 1, 5, -5, 5)),
        pg_draws_per_case = 2000L,
        thresholds = list(pg_mean_z_max = 6, pg_variance_relative_error_max = .2,
            factor_residual_max = 1e-10, root_residual_max = 1e-6, root_rcond_min = .Machine$double.eps,
            rank_rhat_max = 1.01, folded_rhat_max = 1.01, bulk_ess_min = 400,
            tail_ess_min = 400, pairwise_psm_rms_max = .05),
        rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"),
        calibration_id = "D-036 provisional engineering screen; rho2/signal/priors NOT calibrated or selected",
        scientific_role = "One fresh synthetic panel and four truth-blind starts; no ARI, model selection, SBC, paper use or convergence certification")
}
