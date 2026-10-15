# Exact selected definitions from R/graphmode-pilot.R
graphmode_pilot_version <- "graphmode-first-smoke-2026-09-11-v1"

graphmode_pilot_spec <- function() {
    list(version = graphmode_pilot_version, role = "development-pilot",
        geometry = "rings", n = 121L, TT = 168L, Ktrue = 5L, Kfit = 10L,
        method = "graphMoDE-W", q = 4L, nu = 1, kappa = sqrt(2), tau = 1, s_b = 1,
        amplitude = 0.3, innovation_variance = 0.002,
        m0 = c(log(25), 0, 0), C0 = diag(c(1, 0.5, 0.5)),
        guidance = "class-specific", guidance_prior = c(1, 1),
        guidance_proposal_sd = 0.25, rho = 2L, max_ess_steps = 1000L,
        iterations = 100L, warmup = 50L, thin = 1L, checkpoint_every = 25L,
        chain_budget_seconds = 600, total_budget_seconds = 1800,
        pg_budget_seconds = 300, workers = 1L,
        seeds = c(innovations = 2026091101L, profile_permutation = 2026091102L,
            unit_permutation = 2026091103L, response = 2026091104L,
            balanced_start = 2026091105L, pg = 2026091106L,
            chain_01 = 2026091111L, chain_02 = 2026091112L),
        starts = c("balanced-random-ten", "all-in-one-empty-nine"),
        pg_cases = data.frame(b = c(1, 20, 500, 500, 10000), z = c(0, 1, 5, -5, 5)),
        pg_draws_per_case = 2000L,
        thresholds = list(pg_mean_z_max = 6, pg_variance_relative_error_max = 0.2,
            root_residual_max = 1e-6, root_rcond_min = .Machine$double.eps,
            accepted_positive_moves_min = 1L, partition_moves_min = 1L,
            cross_start_psm_rms_flag = 0.2),
        calibration_decision = paste("D-032: provisional smoke values only; rho=2 is",
            "one uncalibrated candidate, not the legacy rho decision; no selection or tuning"),
        scientific_role = paste("Real PG moment screen and two short starts on one synthetic",
            "panel; no convergence certification, SBC, ranking, signal calibration or paper use"),
        rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"))
}

graphmode_pilot_seeded <- function(seed, code) {
    old_kind <- RNGkind()
    old_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    on.exit({
        do.call(RNGkind, as.list(old_kind))
        if (is.null(old_seed)) {
            if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
        } else assign(".Random.seed", old_seed, .GlobalEnv)
    }, add = TRUE)
    do.call(RNGkind, as.list(graphmode_pilot_spec()$rng_kind))
    set.seed(seed)
    force(code)
}
