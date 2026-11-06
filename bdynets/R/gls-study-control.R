.gls_study_control_impl <- function(mode = c("full", "quick", "smoke_check"),
                                   sensitivity = "none") {
  mode <- match.arg(mode)
  cfg <- list(
    experiment = "GLS directed-grid simulation",
    seed = 20261011L,
    mode = mode,
    sensitivity = sensitivity,
    grid_side = if (mode == "smoke_check") 2L else 3L,
    signal_grid = if (mode == "smoke_check") 0.10 else
      if (mode == "quick") c(0, 0.20) else c(0, 0.10, 0.20),
    n_total = if (mode == "smoke_check") 42L else
      if (mode == "quick") 300L else 1800L,
    train_fraction = 2 / 3,
    validation_fraction = 1 / 6,
    target_mean_min = 12,
    target_mean_max = 20,
    maximum_rate = 1e5,
    x_scale = 0.50,
    calibration_iterations = if (mode == "smoke_check") 1L else 3L,
    calibration_damping = 0.55,
    dgp_amplitude_calibration_steps = if (mode == "smoke_check") 0L else 2L,
    static_energy_fraction = 0.40,
    active_one_hop = if (mode == "smoke_check") 1L else 8L,
    active_two_hop = if (mode == "smoke_check") 1L else 8L,
    active_far = if (mode == "smoke_check") 1L else 4L,
    self_multiplier = 0.45,
    one_hop_multiplier = 0.70,
    two_hop_multiplier = 1.00,
    far_multiplier = 0.85,
    true_discount = 0.95,
    factor_dim = 1L,
    season_period = if (mode == "smoke_check") 21L else 300L,
    true_fourier_order = 2L,
    fitted_fourier_order = 2L,
    season_amplitude_1 = log(1.45),
    season_amplitude_2 = 0.14,
    alpha_prior_sd = 3,
    season_prior_sd = 1,
    c_ell = 1 / 4,
    shrinkage_spec = "learned_dictionary",
    fixed_tau = 1,
    tau_prior_scale = 1,
    graph_laplace_rho0 = 0.90,
    chi_shape = 1,
    chi_rate = 1,
    chi_proposal_sd = 0.08,
    chi_adapt_rate = 0.10,
    chi_target_acceptance = 0.30,
    affinity_feature_tolerance = 1e-9,
    discount_grid = c(0.90, 0.95, 0.98, 0.995),
    discount_prior = rep(0.25, 4L),
    reference_newton_tolerance = 1e-8,
    reference_newton_max_iterations = 30L,
    reference_eta_limit = 30,
    n_iter = if (mode == "smoke_check") 14L else
      if (mode == "quick") 300L else 5000L,
    burn_mcmc = if (mode == "smoke_check") 7L else
      if (mode == "quick") 150L else 4000L,
    thin = 1L,
    n_chains = if (mode == "smoke_check") 1L else 3L,
    parallel_chains = mode == "full",
    # Gaussian proposals are exact for the augmented kernel excluding the
    # covariance-discount evolution density. Small target blocks keep the
    # Appendix B.3 correction from degenerating in high dimension.
    static_target_block_size = if (mode == "smoke_check") 1L else 4L,
    # A pCN move is reversible with respect to the Gaussian augmented-data
    # conditional, so the Appendix B.3 MH ratio still contains only B(d; v).
    # Unlike the former independence proposal, it can make local moves when
    # the reference-covariance correction is sharp.
    static_pcn_initial_step = if (mode == "smoke_check") 0.20 else 0.80,
    static_pcn_target_acceptance = 0.25,
    static_pcn_adapt_rate = 0.20,
    static_pcn_min_step = 0.002,
    static_pcn_max_step = 0.99,
    initial_loading_sd = 0.01,
    startup_check_iteration = if (mode == "smoke_check") 5L else
      if (mode == "quick") 40L else 100L,
    startup_min_static_acceptance = 0.005,
    startup_min_static_accepts = 1L,
    startup_beta_abs_limit = 100,
    shrinkage_substeps = if (mode == "smoke_check") 1L else 3L,
    # The product-preserving tau/local-scale ridge is sampled with a bounded
    # one-dimensional slice update.  The width controls stepping out rather
    # than a random-walk proposal, so the update can cross the whole ridge
    # without relying on a sequence of small accepted moves.
    scale_move_method = "slice",
    scale_move_steps = if (mode == "smoke_check") 1L else 2L,
    scale_move_initial_sd = 0.20,
    scale_move_target_acceptance = 0.35,
    scale_move_adapt_rate = 0.15,
    scale_move_min_sd = 5e-4,
    scale_move_max_sd = 0.30,
    scale_move_max_stepout = 20L,
    scale_move_max_shrink = 100L,
    # ASIS-style loading/factor rescaling leaves L beta unchanged and is
    # corrected by the loading prior, the identified state law, and the
    # transformation Jacobian.  It targets the remaining loading-state ridge
    # without introducing a free factor-innovation scale.
    interweave_steps = 1L,
    interweave_initial_sd = 0.05,
    interweave_target_acceptance = 0.30,
    interweave_adapt_rate = 0.10,
    interweave_min_sd = 5e-4,
    interweave_max_sd = 0.20,
    progress_every = if (mode == "smoke_check") 5L else 250L,
    r_nb_epsilon = 0.05,
    r_nb_quantile = 0.99,
    r_nb_min = 25,
    r_nb_max = 5000,
    r_nb_round_to = 25,
    r_nb_smoothing = 0.90,
    predictive_draws = if (mode == "smoke_check") 4L else
      if (mode == "quick") 80L else 600L,
    delta_A = 0.01,
    c_A = 0.50,
    dss_weight = "intensity",
    dss_n_lambda = if (mode == "smoke_check") 8L else 100L,
    dss_lambda_min_ratio = 1e-4,
    dss_zero_tolerance = 1e-8,
    dss_max_draws = if (mode == "smoke_check") 4L else 600L,
    dss_validation_draws = if (mode == "smoke_check") 3L else 150L,
    # Evaluate every distinct DSS support and report the smallest support whose
    # validation loss is within 1% of the continuous fit.  The dense path is
    # important near the weak-signal selection boundary, where a coarse stride
    # can otherwise skip the smallest admissible support.
    dss_predictive_tolerance = 0.01,
    dss_path_eval_stride = 1L,
    mcmc_rhat_threshold = 1.01,
    mcmc_ess_threshold = 400,
    intensity_lower = -15,
    intensity_upper = 12,
    output_dir = file.path("output", mode)
  )
  cfg <- .gls_study_apply_sensitivity(cfg, sensitivity)
  .gls_study_validate_control(cfg)
  cfg
}

.gls_study_sensitivity_presets_impl <- function() {
  list(
    none = list(),
    static_energy_20 = list(static_energy_fraction = 0.20),
    static_energy_60 = list(static_energy_fraction = 0.60),
    c_ell_1_8 = list(c_ell = 1 / 8),
    c_ell_1_2 = list(c_ell = 1 / 2),
    true_discount_098 = list(true_discount = 0.98),
    graph_laplace_tau1 = list(
      shrinkage_spec = "graph_laplace_fixed_tau", fixed_tau = 1
    ),
    graph_laplace_tau0025 = list(
      shrinkage_spec = "graph_laplace_fixed_tau", fixed_tau = 0.025,
      n_iter = 4000L, burn_mcmc = 2500L
    ),
    graph_laplace_tau0001 = list(
      shrinkage_spec = "graph_laplace_fixed_tau", fixed_tau = 0.001,
      n_iter = 4000L, burn_mcmc = 2500L
    ),
    tau_scale_05 = list(tau_prior_scale = 0.5),
    tau_scale_2 = list(tau_prior_scale = 2),
    chi_rate_05 = list(chi_rate = 0.5),
    chi_rate_2 = list(chi_rate = 2),
    amplitude_0025 = list(delta_A = 0.025),
    amplitude_005 = list(delta_A = 0.05),
    evidence_08 = list(c_A = 0.80),
    evidence_09 = list(c_A = 0.90),
    dss_uniform = list(dss_weight = "uniform"),
    dss_tolerance_0025 = list(dss_predictive_tolerance = 0.0025),
    dss_tolerance_01 = list(dss_predictive_tolerance = 0.01)
  )
}

.gls_study_apply_sensitivity <- function(cfg, sensitivity = "none") {
  presets <- .gls_study_sensitivity_presets_impl()
  if (!sensitivity %in% names(presets))
    stop("Unknown sensitivity preset: ", sensitivity,
         ". Available: ", paste(names(presets), collapse = ", "))
  for (nm in names(presets[[sensitivity]]))
    cfg[[nm]] <- presets[[sensitivity]][[nm]]
  cfg$sensitivity <- sensitivity
  cfg
}

.gls_study_validate_control <- function(cfg) {
  # Saved controls produced before fixed_tau became configurable used the
  # Graph-Laplace tau-one specification, for which the implied value is one.
  if (is.null(cfg$fixed_tau)) cfg$fixed_tau <- 1
  stopifnot(cfg$grid_side >= 2L, cfg$factor_dim >= 1L,
            cfg$n_iter > cfg$burn_mcmc, cfg$thin >= 1L,
            cfg$n_chains >= 1L, cfg$c_ell > 0,
            cfg$tau_prior_scale > 0, cfg$fixed_tau > 0, cfg$chi_rate > 0,
            cfg$static_pcn_initial_step > 0,
            cfg$static_pcn_initial_step <= 1,
            cfg$static_pcn_min_step > 0,
            cfg$static_pcn_max_step <= 1,
            cfg$static_pcn_min_step <= cfg$static_pcn_initial_step,
            cfg$static_pcn_initial_step <= cfg$static_pcn_max_step,
            cfg$initial_loading_sd > 0,
            cfg$startup_check_iteration >= 1L,
            cfg$scale_move_steps >= 1L,
            cfg$scale_move_initial_sd > 0,
            cfg$scale_move_max_stepout >= 1L,
            cfg$scale_move_max_shrink >= 1L,
            cfg$interweave_steps >= 0L,
            cfg$interweave_initial_sd > 0,
            cfg$interweave_min_sd > 0,
            cfg$interweave_max_sd >= cfg$interweave_initial_sd,
            cfg$static_energy_fraction >= 0,
            cfg$static_energy_fraction <= 1,
            cfg$delta_A > 0, cfg$c_A > 0, cfg$c_A < 1,
            cfg$dss_predictive_tolerance >= 0,
            cfg$dss_path_eval_stride >= 1L,
            cfg$dss_path_eval_stride == as.integer(cfg$dss_path_eval_stride))
  if (length(cfg$discount_grid) != length(cfg$discount_prior) ||
      any(cfg$discount_grid <= 0 | cfg$discount_grid >= 1) ||
      any(cfg$discount_prior <= 0))
    stop("Discount grid/prior is invalid.")
  if (!cfg$dss_weight %in% c("intensity", "uniform"))
    stop("dss_weight must be 'intensity' or 'uniform'.")
  if (length(cfg$shrinkage_spec) != 1L ||
      !cfg$shrinkage_spec %in% c("learned_dictionary", "graph_laplace_fixed_tau",
                                 "graph_laplace_tau1"))
    stop(paste0("shrinkage_spec must be 'learned_dictionary' or ",
                "'graph_laplace_fixed_tau'."))
  if (length(cfg$graph_laplace_rho0) != 1L ||
      !is.finite(cfg$graph_laplace_rho0) || cfg$graph_laplace_rho0 < 0 ||
      cfg$graph_laplace_rho0 > 1)
    stop("graph_laplace_rho0 must be one finite number in [0, 1].")
  if (!cfg$scale_move_method %in% c("slice", "random_walk"))
    stop("scale_move_method must be 'slice' or 'random_walk'.")
  cfg$discount_prior <- cfg$discount_prior / sum(cfg$discount_prior)
  structure(cfg, class = c("gls_study_control", "list"))
}
