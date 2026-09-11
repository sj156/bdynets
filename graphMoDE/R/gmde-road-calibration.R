# Blinded rho calibration and short two-chain stability pilot for the
# approved central-Beijing road design.

countdlm_road_calibration_api_version <-
    "countdlm-road-calibration-2026-09-02-v2b"

countdlm_road_calibration_variants <- function(potts_beta_grid) {
    beta_slug <- gsub(
        "[.]", "p",
        format(potts_beta_grid, trim = TRUE, scientific = FALSE, nsmall = 2L)
    )
    rbind(
        data.frame(
            variant_id = countdlm_road_internal_methods[
                countdlm_road_internal_methods != "Potts-MDE"
            ],
            method = countdlm_road_internal_methods[
                countdlm_road_internal_methods != "Potts-MDE"
            ],
            potts_beta = NA_real_,
            stringsAsFactors = FALSE
        ),
        data.frame(
            variant_id = paste0("Potts-MDE-beta-", beta_slug),
            method = "Potts-MDE",
            potts_beta = as.numeric(potts_beta_grid),
            stringsAsFactors = FALSE
        )
    )
}

countdlm_road_calibration_tasks <- function(variants, chains) {
    rows <- lapply(seq_len(chains), function(chain_id) {
        value <- variants
        value$chain_id <- as.integer(chain_id)
        value$task_id <- paste0(value$variant_id, "-chain-", chain_id)
        value[, c(
            "task_id", "variant_id", "method", "potts_beta", "chain_id"
        )]
    })
    do.call(rbind, rows)
}

countdlm_road_rho_branch_tasks <- function(
    variants, chains, rho_grid, seed_base
) {
    seed_base <- gmde_scalar_integer(
        seed_base, "seed_base", 1L, .Machine$integer.max - 100000L
    )
    rows <- list()
    index <- 0L
    pair_index <- 0L
    for (variant_index in seq_len(nrow(variants))) {
        for (chain_id in seq_len(chains)) {
            pair_index <- pair_index + 1L
            paired_seed <- seed_base + 101L * pair_index
            for (rho in rho_grid) {
                index <- index + 1L
                variant <- variants[variant_index, , drop = FALSE]
                pair_id <- paste0(
                    variant$variant_id[[1L]], "-chain-", chain_id
                )
                rows[[index]] <- data.frame(
                    task_id = paste0(
                        "rho-", pair_id, "-rho-",
                        format(rho, trim = TRUE, scientific = FALSE)
                    ),
                    pair_id = pair_id,
                    variant_id = variant$variant_id[[1L]],
                    method = variant$method[[1L]],
                    potts_beta = variant$potts_beta[[1L]],
                    chain_id = as.integer(chain_id),
                    candidate_rho = as.numeric(rho),
                    paired_seed = as.integer(paired_seed),
                    stringsAsFactors = FALSE
                )
            }
        }
    }
    do.call(rbind, rows)
}

countdlm_road_validate_calibration_config <- function(config) {
    if (!inherits(config, "countdlm_road_calibration_config") ||
        !identical(config$api_version,
                   countdlm_road_calibration_api_version) ||
        !is.character(config$config_signature) ||
        length(config$config_signature) != 1L ||
        !identical(
            config$config_signature,
            countdlm_road_config_signature(config)
        )) {
        stop(
            "The road calibration configuration is invalid or was modified ",
            "after construction.", call. = FALSE
        )
    }
    expected_variants <- countdlm_road_calibration_variants(
        config$potts_beta_grid
    )
    expected_tasks <- countdlm_road_calibration_tasks(
        expected_variants, config$pilot_chains
    )
    expected_rho_tasks <- countdlm_road_rho_branch_tasks(
        expected_variants, config$rho_calibration_chains,
        config$rho_grid, config$rho_method_seed_base
    )
    if (!identical(
        config$sampler_version, countdlm_gmde_sampler_version
    ) || !identical(config$n, 100L) || !identical(config$TT, 168L) ||
        !identical(config$K_true, 5L) || !identical(config$K_fit, 10L) ||
        !identical(config$basis_m, 40L) ||
        !identical(config$pg_backend, "devroye-exact") ||
        !isTRUE(config$algorithm_exact) ||
        !isTRUE(config$classification_only) ||
        !identical(config$context_sha256, countdlm_road_context_sha256) ||
        !identical(config$methods, countdlm_road_internal_methods) ||
        !identical(config$workers, 6L) ||
        !identical(config$reserved_reported_cores, 4L) ||
        !identical(config$worker_thread_limit, 1L) ||
        !identical(config$progress_poll_seconds, 0.5) ||
        !identical(config$rho_grid, c(1, 2, 4)) ||
        !identical(config$rho_calibration_chains, 2L) ||
        !identical(config$rho_calibration_iterations, 120L) ||
        !identical(config$rho_calibration_settle, 60L) ||
        !identical(config$rho_calibration_score, 60L) ||
        !identical(config$rho_calibration_burn, 60L) ||
        !identical(
            config$rho_design, "independent-fixed-branches-v1"
        ) ||
        !identical(config$rho_tie_break, "smallest") ||
        !identical(config$rho_near_optimal_tolerance, 0.05) ||
        !identical(
            config$rho_selection_rule,
            "smallest-rho-within-five-percent-of-best-efficiency"
        ) ||
        !identical(
            config$rho_resolution_rule,
            paste(
                "pooled-choice-must-match-both-starts-and-score-halves",
                "with-branch-movement-and-family-regret-audits"
            )
        ) ||
        !identical(config$rho_max_iteration_movement_share, 0.20) ||
        !identical(config$rho_min_family_relative_efficiency, 0.80) ||
        !identical(
            config$rho_aggregation,
            "family-balanced-independent-branches-v2"
        ) ||
        !identical(
            config$rho_timing_scope,
            "nonempty-expert-pg-ffbs-mh-wall-seconds"
        ) ||
        !identical(
            config$rho_movement_scope,
            "accepted-poisson-information-state-movement"
        ) ||
        !identical(config$potts_variant_weight, 1 / 3) ||
        !identical(config$potts_beta_status, "screened-not-selected") ||
        !identical(config$pilot_iterations, 300L) ||
        !identical(config$pilot_burn, 150L) ||
        !identical(config$pilot_chains, 2L) ||
        !identical(config$short_log_rhat_limit, 1.10) ||
        !identical(config$short_count_rhat_limit, 1.20) ||
        !identical(config$short_psm_rms_limit, 0.10) ||
        !identical(config$short_psm_mean_abs_limit, 0.05) ||
        !identical(config$potts_beta_grid, c(0.25, 0.5, 1)) ||
        !identical(config$target_full_iterations, 1000L) ||
        !identical(config$target_full_chains, 2L) ||
        !identical(config$full_budget_hours, 12) ||
        !identical(config$budget_fraction, 0.80) ||
        !identical(config$projection_multiplier, 2) ||
        !identical(config$max_projected_replicates, 40L) ||
        !identical(config$data_seed, 2026091101L) ||
        !identical(config$rho_initialization_seed_base, 2026091200L) ||
        !identical(config$rho_method_seed_base, 2026092300L) ||
        !identical(config$pilot_initialization_seed_base, 2026091400L) ||
        !identical(config$pilot_method_seed_base, 2026092400L) ||
        !identical(config$state_G, diag(2)) ||
        !identical(config$state_W, diag(c(1e-6, 5e-7))) ||
        !identical(config$state_C0, diag(c(2, 1))) ||
        !identical(config$substantive_min, 5L) ||
        !identical(config$truth_metrics_computed, FALSE) ||
        !identical(config$formal_results_authorized, FALSE) ||
        !identical(config$variants, expected_variants) ||
        !identical(config$pilot_tasks, expected_tasks) ||
        !identical(config$rho_tasks, expected_rho_tasks)) {
        stop("The short calibration pilot's fixed contract is invalid.",
             call. = FALSE)
    }
    invisible(config)
}

#' Construct the approved-road short calibration-pilot configuration
#'
#' This fixed pilot leaves four of the ten reported CPU cores unused.  Six
#' independent workers each use one numerical thread.  The first phase runs
#' independent fixed-rho branches for every variant, start, and candidate.
#' Each branch discards 60 transitions and scores the next 60.  The second
#' phase reruns the same 14 variant-by-start tasks at the shared rho.
#'
#' @param context_file Approved external road-context RDS.
#' @param output_dir Brand-new external output directory.
#' @return A signed, fixed calibration-pilot configuration.
#' @export
countdlm_road_calibration_config <- function(context_file, output_dir) {
    rho_grid <- c(1, 2, 4)
    potts_beta_grid <- c(0.25, 0.5, 1)
    variants <- countdlm_road_calibration_variants(potts_beta_grid)
    rho_method_seed_base <- 2026092300L
    config <- list(
        api_version = countdlm_road_calibration_api_version,
        sampler_version = countdlm_gmde_sampler_version,
        scientific_role = paste(
            "truth-blinded rho calibration and short two-chain stability",
            "pilot; not an inferential or formal simulation"
        ),
        context_file = normalizePath(
            context_file, winslash = "/", mustWork = TRUE
        ),
        context_sha256 = countdlm_road_context_sha256,
        output_dir = normalizePath(
            output_dir, winslash = "/", mustWork = FALSE
        ),
        n = 100L,
        TT = 168L,
        K_true = 5L,
        K_fit = 10L,
        basis_m = 40L,
        methods = countdlm_road_internal_methods,
        variants = variants,
        pilot_tasks = countdlm_road_calibration_tasks(variants, 2L),
        rho_tasks = countdlm_road_rho_branch_tasks(
            variants, 2L, rho_grid, rho_method_seed_base
        ),
        workers = 6L,
        reserved_reported_cores = 4L,
        worker_thread_limit = 1L,
        progress_poll_seconds = 0.5,
        rho_grid = rho_grid,
        rho_calibration_chains = 2L,
        rho_calibration_iterations = 120L,
        rho_calibration_settle = 60L,
        rho_calibration_score = 60L,
        rho_calibration_burn = 60L,
        rho_design = "independent-fixed-branches-v1",
        rho_tie_break = "smallest",
        rho_near_optimal_tolerance = 0.05,
        rho_selection_rule =
            "smallest-rho-within-five-percent-of-best-efficiency",
        rho_resolution_rule =
            paste(
                "pooled-choice-must-match-both-starts-and-score-halves",
                "with-branch-movement-and-family-regret-audits"
            ),
        rho_max_iteration_movement_share = 0.20,
        rho_min_family_relative_efficiency = 0.80,
        rho_aggregation = "family-balanced-independent-branches-v2",
        rho_timing_scope =
            "nonempty-expert-pg-ffbs-mh-wall-seconds",
        rho_movement_scope =
            "accepted-poisson-information-state-movement",
        potts_variant_weight = 1 / 3,
        potts_beta_status = "screened-not-selected",
        pilot_iterations = 300L,
        pilot_burn = 150L,
        pilot_chains = 2L,
        short_log_rhat_limit = 1.10,
        short_count_rhat_limit = 1.20,
        short_psm_rms_limit = 0.10,
        short_psm_mean_abs_limit = 0.05,
        potts_beta_grid = potts_beta_grid,
        target_full_iterations = 1000L,
        target_full_chains = 2L,
        full_budget_hours = 12,
        budget_fraction = 0.80,
        projection_multiplier = 2,
        max_projected_replicates = 40L,
        data_seed = 2026091101L,
        rho_initialization_seed_base = 2026091200L,
        rho_method_seed_base = rho_method_seed_base,
        pilot_initialization_seed_base = 2026091400L,
        pilot_method_seed_base = 2026092400L,
        state_G = diag(2),
        state_W = diag(c(1e-6, 5e-7)),
        state_C0 = diag(c(2, 1)),
        substantive_min = 5L,
        pg_backend = "devroye-exact",
        algorithm_exact = TRUE,
        classification_only = TRUE,
        truth_metrics_computed = FALSE,
        formal_results_authorized = FALSE,
        unavailable_external_methods = countdlm_road_external_methods,
        projected_internal_scope = paste(
            "four non-Potts methods and all three fixed Potts beta variants;",
            "two formal chains per replicate; six workers"
        )
    )
    config$config_signature <- countdlm_road_config_signature(config)
    config <- structure(
        config, class = "countdlm_road_calibration_config"
    )
    countdlm_road_validate_calibration_config(config)
    config
}

countdlm_road_pool_rho_calibration <- function(
    calibrations, rho_grid, weights, tie_break = "smallest",
    near_optimal_tolerance = 0
) {
    if (!is.list(calibrations) || !length(calibrations)) {
        stop("At least one rho-calibration record is required.",
             call. = FALSE)
    }
    if (!is.numeric(weights) || length(weights) != length(calibrations) ||
        any(!is.finite(weights)) || any(weights <= 0)) {
        stop("weights must contain one finite positive value per record.",
             call. = FALSE)
    }
    if (length(near_optimal_tolerance) != 1L ||
        !is.numeric(near_optimal_tolerance) ||
        !is.finite(near_optimal_tolerance) ||
        near_optimal_tolerance < 0 || near_optimal_tolerance >= 1) {
        stop("near_optimal_tolerance must be in [0, 1).", call. = FALSE)
    }
    required <- c(
        "rho", "accepted_movement", "elapsed_seconds", "proposals", "accepts"
    )
    checked <- lapply(calibrations, function(value) {
        value <- as.data.frame(value)
        if (!all(required %in% names(value)) ||
            nrow(value) != length(rho_grid) ||
            !identical(as.numeric(value$rho), as.numeric(rho_grid)) ||
            any(!is.finite(value$accepted_movement)) ||
            any(value$accepted_movement < 0) ||
            any(!is.finite(value$elapsed_seconds)) ||
            any(value$elapsed_seconds <= 0) ||
            any(!is.finite(value$proposals)) || any(value$proposals <= 0) ||
            any(!is.finite(value$accepts)) || any(value$accepts < 0) ||
            any(value$accepts > value$proposals)) {
            stop("A rho-calibration record failed validation.", call. = FALSE)
        }
        value
    })
    weighted_sum <- function(field) {
        Reduce(`+`, Map(
            function(value, weight) value[[field]] * weight,
            checked, as.numeric(weights)
        ))
    }
    pooled <- data.frame(
        rho = as.numeric(rho_grid),
        weighted_accepted_movement = weighted_sum("accepted_movement"),
        weighted_elapsed_seconds = weighted_sum("elapsed_seconds"),
        weighted_proposals = weighted_sum("proposals"),
        weighted_accepts = weighted_sum("accepts"),
        stringsAsFactors = FALSE
    )
    pooled$weighted_acceptance_rate <-
        pooled$weighted_accepts / pooled$weighted_proposals
    pooled$efficiency <-
        pooled$weighted_accepted_movement /
        pooled$weighted_elapsed_seconds
    tie_break <- match.arg(tie_break, c("smallest", "largest", "first"))
    selection_available <- sum(pooled$weighted_accepts) > 0 &&
        max(pooled$weighted_accepted_movement) > 0
    if (selection_available) {
        best_efficiency <- max(pooled$efficiency)
        pooled$raw_best <- pooled$efficiency == best_efficiency
        pooled$near_optimal <- pooled$efficiency >=
            (1 - near_optimal_tolerance) * best_efficiency
        eligible <- which(pooled$near_optimal)
        selected <- switch(
            tie_break,
            smallest = min(pooled$rho[eligible]),
            largest = max(pooled$rho[eligible]),
            first = pooled$rho[eligible[[1L]]]
        )
        raw_best <- pooled$rho[pooled$raw_best][[1L]]
        selected_relative_efficiency <- pooled$efficiency[
            pooled$rho == selected
        ][[1L]] / best_efficiency
    } else {
        pooled$raw_best <- FALSE
        pooled$near_optimal <- FALSE
        selected <- NA_real_
        raw_best <- NA_real_
        selected_relative_efficiency <- NA_real_
    }
    pooled$selected <- !is.na(selected) & pooled$rho == selected
    list(
        table = pooled,
        selected_rho = as.numeric(selected),
        raw_best_rho = as.numeric(raw_best),
        selected_relative_efficiency = selected_relative_efficiency,
        selection_available = selection_available,
        selection_status = if (selection_available) {
            "candidate-selected-by-registered-efficiency-rule"
        } else "insufficient-accepted-movement",
        records_pooled = length(checked),
        total_record_weight = sum(weights),
        weights = as.numeric(weights),
        tie_break = tie_break,
        near_optimal_tolerance = near_optimal_tolerance,
        aggregation = "family-balanced raw-numerator/raw-denominator pooling",
        score = paste(
            "weighted accepted information-state movement / weighted",
            "state-update seconds; per-record efficiencies are never averaged"
        )
    )
}

countdlm_road_rho_resolution_decision <- function(
    selection_available, strata_match, all_branches_have_movement,
    movement_share_ok, family_efficiency_ok
) {
    gates <- c(
        selection_available = isTRUE(selection_available),
        strata_match = isTRUE(strata_match),
        all_branches_have_movement = isTRUE(all_branches_have_movement),
        movement_share_ok = isTRUE(movement_share_ok),
        family_efficiency_ok = isTRUE(family_efficiency_ok)
    )
    resolved <- all(gates)
    status <- if (resolved) {
        "resolved-all-rho-audits-passed"
    } else paste(
        c(
            if (!gates[["selection_available"]]) {
                "insufficient-pooled-accepted-movement"
            },
            if (!gates[["strata_match"]]) {
                "start-or-score-half-disagreement"
            },
            if (!gates[["all_branches_have_movement"]]) {
                "one-or-more-branches-have-no-accepted-movement"
            },
            if (!gates[["movement_share_ok"]]) {
                "single-iteration-movement-dominance"
            },
            if (!gates[["family_efficiency_ok"]]) {
                "method-family-efficiency-regret"
            }
        ),
        collapse = ";"
    )
    list(resolved = resolved, status = status, gates = gates)
}

countdlm_road_finalize_rho_pool <- function(
    rho_pooled, provisional_selected_rho, resolution
) {
    if (!is.list(rho_pooled) || !is.data.frame(rho_pooled$table) ||
        !"selected" %in% names(rho_pooled$table) ||
        !is.logical(rho_pooled$table$selected) ||
        !is.logical(rho_pooled$selection_available) ||
        length(rho_pooled$selection_available) != 1L ||
        !is.character(rho_pooled$selection_status) ||
        length(rho_pooled$selection_status) != 1L ||
        !is.numeric(rho_pooled$selected_relative_efficiency) ||
        length(rho_pooled$selected_relative_efficiency) != 1L ||
        !is.numeric(provisional_selected_rho) ||
        length(provisional_selected_rho) != 1L ||
        !is.list(resolution) || !is.logical(resolution$resolved) ||
        length(resolution$resolved) != 1L ||
        !is.character(resolution$status) || length(resolution$status) != 1L) {
        stop("The rho-resolution finalization contract is invalid.",
             call. = FALSE)
    }
    resolved <- isTRUE(resolution$resolved)
    rho_pooled$provisional_selected_rho <- provisional_selected_rho
    rho_pooled$provisional_selection_available <-
        rho_pooled$selection_available
    rho_pooled$provisional_selection_status <- rho_pooled$selection_status
    rho_pooled$provisional_selected_relative_efficiency <-
        rho_pooled$selected_relative_efficiency
    rho_pooled$table$provisional_selected <- rho_pooled$table$selected
    rho_pooled$table$rho_resolved <- rep(resolved, nrow(rho_pooled$table))
    rho_pooled$table$final_selected <-
        rho_pooled$table$selected & resolved
    rho_pooled$table$selected <- rho_pooled$table$final_selected
    rho_pooled$table$resolution_status <- rep(
        resolution$status, nrow(rho_pooled$table)
    )
    rho_pooled$selected_rho <- if (resolved) {
        provisional_selected_rho
    } else NA_real_
    rho_pooled$selected_relative_efficiency <- if (resolved) {
        rho_pooled$provisional_selected_relative_efficiency
    } else NA_real_
    rho_pooled$selection_available <- resolved
    rho_pooled$selection_status <- resolution$status
    rho_pooled$resolved <- resolved
    rho_pooled$resolution_status <- resolution$status
    rho_pooled
}

countdlm_road_fixed_rho_calibration <- function(fit, settle, score) {
    settle <- gmde_scalar_integer(settle, "settle", lower = 1L)
    score <- gmde_scalar_integer(score, "score", lower = 2L)
    if (score %% 2L != 0L) {
        stop("score must be even for the registered half-window audit.",
             call. = FALSE)
    }
    iterations <- settle + score
    required <- c(
        "Z", "state_rho", "state_accepted", "state_movement",
        "state_update_seconds", "algorithm_exact", "settings",
        "initialization"
    )
    if (!is.list(fit) || !all(required %in% names(fit)) ||
        !is.list(fit$settings) || !is.list(fit$initialization)) {
        stop("A fixed-rho branch failed the calibration trace contract.",
             call. = FALSE)
    }
    settings_required <- c(
        "n", "TT", "p", "K", "m", "n_iter", "burn", "rho",
        "rho_control", "pg_backend", "algorithm_exact", "sampler_version"
    )
    settings <- fit$settings
    integer_setting <- function(name, lower = 1L) {
        value <- settings[[name]]
        is.numeric(value) && length(value) == 1L && is.finite(value) &&
            value == round(value) && value >= lower
    }
    if (!all(settings_required %in% names(settings)) ||
        !integer_setting("n") || !integer_setting("TT") ||
        !integer_setting("p") || !integer_setting("K") ||
        !integer_setting("m", lower = 0L) ||
        !identical(settings$n_iter, iterations) ||
        !identical(settings$burn, settle) ||
        !identical(settings$pg_backend, "devroye-exact") ||
        !isTRUE(settings$algorithm_exact) || !isTRUE(fit$algorithm_exact) ||
        !identical(
            settings$sampler_version, countdlm_gmde_sampler_version
        ) || !is.list(settings$rho_control) ||
        !identical(settings$rho_control$mode, "fixed") ||
        length(settings$rho) != 1L || !is.numeric(settings$rho) ||
        !is.finite(settings$rho) ||
        length(settings$rho_control$selected) != 1L ||
        !is.numeric(settings$rho_control$selected) ||
        !is.finite(settings$rho_control$selected)) {
        stop("A fixed-rho branch failed the calibration settings contract.",
             call. = FALSE)
    }
    n <- as.integer(settings$n)
    TT <- as.integer(settings$TT)
    p <- as.integer(settings$p)
    K <- as.integer(settings$K)
    expected_state_dimension <- c(iterations, K)
    if (!is.matrix(fit$Z) ||
        !identical(dim(fit$Z), c(iterations, n)) ||
        !is.numeric(fit$Z) || any(!is.finite(fit$Z)) ||
        any(fit$Z != round(fit$Z)) || any(fit$Z < 1L) ||
        any(fit$Z > K) || !is.numeric(fit$state_rho) ||
        length(fit$state_rho) != iterations ||
        any(!is.finite(fit$state_rho)) ||
        !is.matrix(fit$state_accepted) ||
        !is.logical(fit$state_accepted) ||
        !identical(dim(fit$state_accepted), expected_state_dimension) ||
        !is.matrix(fit$state_movement) ||
        !identical(dim(fit$state_movement), expected_state_dimension) ||
        !is.matrix(fit$state_update_seconds) ||
        !identical(
            dim(fit$state_update_seconds), expected_state_dimension
        ) || any(!is.finite(fit$state_movement)) ||
        any(fit$state_movement < 0) ||
        any(!is.finite(fit$state_update_seconds)) ||
        any(fit$state_update_seconds <= 0)) {
        stop("A fixed-rho branch failed the calibration trace contract.",
             call. = FALSE)
    }
    initialization <- fit$initialization
    initialization_required <- c("model", "Z", "theta", "classifier")
    if (!all(initialization_required %in% names(initialization)) ||
        !is.character(initialization$model) ||
        length(initialization$model) != 1L ||
        !(initialization$model %in% c("gmde", "nograph", "potts")) ||
        !is.numeric(initialization$Z) || length(initialization$Z) != n ||
        any(!is.finite(initialization$Z)) ||
        any(initialization$Z != round(initialization$Z)) ||
        any(initialization$Z < 1L) || any(initialization$Z > K) ||
        !is.array(initialization$theta) ||
        !identical(dim(initialization$theta), c(K, TT, p)) ||
        !is.numeric(initialization$theta) ||
        any(!is.finite(initialization$theta))) {
        stop(
            "A fixed-rho branch failed the initial-state contract.",
            call. = FALSE
        )
    }
    classifier_ok <- switch(
        initialization$model,
        gmde = settings$m >= 1L &&
            is.matrix(initialization$classifier) &&
            is.numeric(initialization$classifier) &&
            identical(
                dim(initialization$classifier), c(settings$m, K - 1L)
            ) && all(is.finite(initialization$classifier)),
        nograph = settings$m == 0L &&
            is.numeric(initialization$classifier) &&
            length(initialization$classifier) == K &&
            all(is.finite(initialization$classifier)) &&
            all(initialization$classifier > 0) &&
            abs(sum(initialization$classifier) - 1) <= 1e-10,
        potts = settings$m == 0L && is.null(initialization$classifier)
    )
    if (!isTRUE(classifier_ok) || length(unique(fit$state_rho)) != 1L ||
        fit$state_rho[[1L]] <= 0 ||
        abs(settings$rho - fit$state_rho[[1L]]) > 1e-12 ||
        abs(
            settings$rho_control$selected - fit$state_rho[[1L]]
        ) > 1e-12) {
        stop("A fixed-rho branch failed the fixed-rho contract.",
             call. = FALSE)
    }
    accepted_indicator <- !is.na(fit$state_accepted) & fit$state_accepted
    if (any(fit$state_movement[!accepted_indicator] != 0)) {
        stop(
            "Rejected or inapplicable state proposals must have zero movement.",
            call. = FALSE
        )
    }
    score_index <- seq.int(settle + 1L, iterations)
    half <- score %/% 2L
    first_index <- score_index[seq_len(half)]
    second_index <- score_index[half + seq_len(half)]
    summarize <- function(index) {
        accepted <- fit$state_accepted[index, , drop = FALSE]
        valid <- !is.na(accepted)
        accepted_indicator <- valid & accepted
        movement <- fit$state_movement[index, , drop = FALSE]
        seconds <- fit$state_update_seconds[index, , drop = FALSE]
        c(
            accepted_movement = sum(movement[accepted_indicator]),
            elapsed_seconds = sum(seconds[valid]),
            proposals = sum(valid),
            accepts = sum(accepted[valid])
        )
    }
    full <- summarize(score_index)
    first <- summarize(first_index)
    second <- summarize(second_index)
    score_accepted <- fit$state_accepted[
        score_index, , drop = FALSE
    ]
    score_accepted_indicator <- !is.na(score_accepted) & score_accepted
    score_movement <- fit$state_movement[
        score_index, , drop = FALSE
    ]
    score_movement[!score_accepted_indicator] <- 0
    movement_by_iteration <- rowSums(score_movement)
    if (full[["elapsed_seconds"]] <= 0 || full[["proposals"]] <= 0) {
        stop("A fixed-rho branch contains no timed scored proposals.",
             call. = FALSE)
    }
    movement_sufficient <- full[["accepts"]] > 0 &&
        full[["accepted_movement"]] > 0
    data.frame(
        rho = as.numeric(fit$state_rho[[1L]]),
        accepted_movement = full[["accepted_movement"]],
        elapsed_seconds = full[["elapsed_seconds"]],
        proposals = full[["proposals"]],
        accepts = full[["accepts"]],
        efficiency = full[["accepted_movement"]] /
            full[["elapsed_seconds"]],
        maximum_iteration_accepted_movement = max(movement_by_iteration),
        maximum_iteration_movement_share = if (movement_sufficient) {
            max(movement_by_iteration) / full[["accepted_movement"]]
        } else NA_real_,
        accepted_movement_sufficient = movement_sufficient,
        first_half_accepted_movement = first[["accepted_movement"]],
        first_half_elapsed_seconds = first[["elapsed_seconds"]],
        first_half_proposals = first[["proposals"]],
        first_half_accepts = first[["accepts"]],
        second_half_accepted_movement = second[["accepted_movement"]],
        second_half_elapsed_seconds = second[["elapsed_seconds"]],
        second_half_proposals = second[["proposals"]],
        second_half_accepts = second[["accepts"]],
        settle_transitions = settle,
        scored_transitions = score,
        score_first_iteration = settle + 1L,
        score_last_iteration = iterations,
        stringsAsFactors = FALSE
    )
}

countdlm_road_rho_half_calibration <- function(calibration, half) {
    half <- match.arg(half, c("first", "second"))
    prefix <- paste0(half, "_half_")
    data.frame(
        rho = calibration$rho,
        accepted_movement = calibration[[paste0(
            prefix, "accepted_movement"
        )]],
        elapsed_seconds = calibration[[paste0(prefix, "elapsed_seconds")]],
        proposals = calibration[[paste0(prefix, "proposals")]],
        accepts = calibration[[paste0(prefix, "accepts")]],
        stringsAsFactors = FALSE
    )
}

countdlm_road_rho_long_table <- function(
    pair_tasks, calibrations, weights, selected_rho,
    branch_metadata = NULL
) {
    if (nrow(pair_tasks) != length(calibrations) ||
        length(weights) != length(calibrations) ||
        length(selected_rho) != length(calibrations)) {
        stop("The rho long-table inputs have incompatible lengths.",
             call. = FALSE)
    }
    values <- lapply(seq_along(calibrations), function(i) {
        calibration <- calibrations[[i]]
        nr <- nrow(calibration)
        pair_metadata <- pair_tasks[
            rep(i, nr),
            c("variant_id", "method", "potts_beta", "chain_id"),
            drop = FALSE
        ]
        rownames(pair_metadata) <- NULL
        pair_id <- rep(pair_tasks$task_id[[i]], nr)
        branch_task_id <- paste0(
            "rho-", pair_id, "-rho-",
            format(calibration$rho, trim = TRUE, scientific = FALSE)
        )
        paired_seed <- rep(NA_integer_, nr)
        initialization_sha256 <- rep(NA_character_, nr)
        if (!is.null(branch_metadata)) {
            required <- c(
                "task_id", "pair_id", "candidate_rho", "paired_seed"
            )
            if (!is.data.frame(branch_metadata) ||
                !all(required %in% names(branch_metadata))) {
                stop("branch_metadata has an invalid schema.", call. = FALSE)
            }
            key <- paste(pair_id, calibration$rho, sep = "\r")
            branch_key <- paste(
                branch_metadata$pair_id,
                branch_metadata$candidate_rho,
                sep = "\r"
            )
            matched <- match(key, branch_key)
            if (anyNA(matched) || anyDuplicated(branch_key)) {
                stop(
                    "branch_metadata does not uniquely match every rho row.",
                    call. = FALSE
                )
            }
            branch_task_id <- branch_metadata$task_id[matched]
            paired_seed <- as.integer(branch_metadata$paired_seed[matched])
            if ("initialization_sha256" %in% names(branch_metadata)) {
                initialization_sha256 <-
                    branch_metadata$initialization_sha256[matched]
            }
        }
        data.frame(
            branch_task_id = branch_task_id,
            pair_id = pair_id,
            pair_metadata,
            paired_seed = paired_seed,
            initialization_sha256 = initialization_sha256,
            aggregation_weight = rep(weights[[i]], nr),
            selected_rho_within_pair = rep(selected_rho[[i]], nr),
            calibration,
            check.names = FALSE,
            stringsAsFactors = FALSE
        )
    })
    value <- do.call(rbind, values)
    rownames(value) <- NULL
    value
}

countdlm_road_posterior_similarity <- function(Z) {
    Z <- as.matrix(Z)
    if (!nrow(Z) || !ncol(Z) || any(!is.finite(Z)) ||
        any(Z != round(Z))) {
        stop("Z must be a finite integer-label draw matrix.", call. = FALSE)
    }
    similarity <- matrix(0, ncol(Z), ncol(Z))
    for (draw in seq_len(nrow(Z))) {
        similarity <- similarity + outer(Z[draw, ], Z[draw, ], "==")
    }
    similarity / nrow(Z)
}

countdlm_road_pilot_rhat <- function(chains) {
    pooled <- unlist(chains, use.names = FALSE)
    if (length(unique(pooled)) == 1L) return(NA_real_)
    countdlm_current_rank_split_rhat(chains)
}

countdlm_road_truth_blinding_ok <- function(fit, n_iter) {
    is.list(fit) &&
        all(c("ari", "acc", "settings") %in% names(fit)) &&
        length(fit$ari) == n_iter && length(fit$acc) == n_iter &&
        all(is.na(fit$ari)) && all(is.na(fit$acc)) &&
        is.list(fit$settings) &&
        length(fit$settings$K_true) == 1L &&
        is.na(fit$settings$K_true)
}

countdlm_road_pair_diagnostics <- function(
    runtime_summary, tasks, output_dir, burn, iterations,
    log_rhat_limit = 1.10, count_rhat_limit = 1.20,
    psm_rms_limit = 0.10, psm_mean_abs_limit = 0.05
) {
    values <- lapply(unique(tasks$variant_id), function(variant_id) {
        task <- tasks[tasks$variant_id == variant_id, , drop = FALSE]
        rows <- runtime_summary[
            match(task$task_id, runtime_summary$task_id), , drop = FALSE
        ]
        base <- data.frame(
            variant_id = variant_id,
            method = task$method[[1L]],
            potts_beta = task$potts_beta[[1L]],
            density_diagnostic = if (task$method[[1L]] == "Potts-MDE") {
                "unnormalized complete density; within fixed beta only"
            } else "observed-data log likelihood",
            stringsAsFactors = FALSE
        )
        required <- c(
            "status", "fit_file", "warning_count", "algorithm_exact",
            "sampler_version", "fixed_rho_contract_passed",
            "completed_fit_retained", "rho_timed",
            "iterations", "burn", "ari_computed", "acc_computed"
        )
        row_contract_ok <- all(required %in% names(rows)) &&
            nrow(rows) == 2L && all(rows$status == "ok") &&
            all(rows$warning_count == 0L) &&
            all(rows$algorithm_exact %in% TRUE) &&
            all(rows$sampler_version == countdlm_gmde_sampler_version) &&
            all(rows$fixed_rho_contract_passed %in% TRUE) &&
            all(rows$completed_fit_retained %in% TRUE) &&
            all(is.finite(rows$rho_timed)) &&
            length(unique(rows$rho_timed)) == 1L &&
            all(rows$iterations == iterations) && all(rows$burn == burn) &&
            all(rows$ari_computed %in% FALSE) &&
            all(rows$acc_computed %in% FALSE)
        row_contract_ok <- isTRUE(row_contract_ok)
        if (!row_contract_ok ||
            any(is.na(rows$fit_file)) ||
            any(!nzchar(rows$fit_file)) ||
            any(!file.exists(file.path(output_dir, rows$fit_file)))) {
            return(cbind(
                base,
                data.frame(
                    log_density_rank_split_rhat = NA_real_,
                    Kocc_rank_split_rhat = NA_real_,
                    Ksub_rank_split_rhat = NA_real_,
                    log_density_constant_across_draws = NA,
                    Kocc_constant_across_draws = NA,
                    Ksub_constant_across_draws = NA,
                    Kocc_reference_status = NA_character_,
                    Ksub_reference_status = NA_character_,
                    psm_rms_between_chains = NA_real_,
                    psm_mean_abs_between_chains = NA_real_,
                    short_screen_passed = FALSE,
                    short_pilot_reference = "unavailable-task-failure",
                    stringsAsFactors = FALSE
                )
            ))
        }
        wrappers <- lapply(
            file.path(output_dir, rows$fit_file), readRDS
        )
        fits <- lapply(wrappers, `[[`, "fit")
        if (!all(vapply(
            fits, countdlm_road_truth_blinding_ok, logical(1),
            n_iter = iterations
        ))) {
            return(cbind(
                base,
                data.frame(
                    log_density_rank_split_rhat = NA_real_,
                    Kocc_rank_split_rhat = NA_real_,
                    Ksub_rank_split_rhat = NA_real_,
                    log_density_constant_across_draws = NA,
                    Kocc_constant_across_draws = NA,
                    Ksub_constant_across_draws = NA,
                    Kocc_reference_status = NA_character_,
                    Ksub_reference_status = NA_character_,
                    psm_rms_between_chains = NA_real_,
                    psm_mean_abs_between_chains = NA_real_,
                    short_screen_passed = FALSE,
                    short_pilot_reference = "unavailable-blinding-contract",
                    stringsAsFactors = FALSE
                )
            ))
        }
        post <- seq.int(burn + 1L, iterations)
        density <- lapply(fits, function(fit) {
            if (task$method[[1L]] == "Potts-MDE") {
                fit$loglik[post]
            } else fit$observed_loglik[post]
        })
        Kocc <- lapply(fits, function(fit) fit$occupied_experts[post])
        Ksub <- lapply(fits, function(fit) fit$substantive_experts[post])
        similarity <- lapply(fits, function(fit) {
            countdlm_road_posterior_similarity(fit$Z[post, , drop = FALSE])
        })
        index <- upper.tri(similarity[[1L]])
        difference <- similarity[[1L]][index] - similarity[[2L]][index]
        log_rhat <- countdlm_road_pilot_rhat(density)
        Kocc_rhat <- countdlm_road_pilot_rhat(Kocc)
        Ksub_rhat <- countdlm_road_pilot_rhat(Ksub)
        log_constant <- length(unique(unlist(
            density, use.names = FALSE
        ))) == 1L
        Kocc_constant <- length(unique(unlist(
            Kocc, use.names = FALSE
        ))) == 1L
        Ksub_constant <- length(unique(unlist(
            Ksub, use.names = FALSE
        ))) == 1L
        Kocc_ok <- Kocc_constant ||
            (is.finite(Kocc_rhat) && Kocc_rhat <= count_rhat_limit)
        Ksub_ok <- Ksub_constant ||
            (is.finite(Ksub_rhat) && Ksub_rhat <= count_rhat_limit)
        psm_rms <- sqrt(mean(difference^2))
        psm_mean_abs <- mean(abs(difference))
        screen_passed <- is.finite(log_rhat) &&
            log_rhat <= log_rhat_limit && Kocc_ok && Ksub_ok &&
            psm_rms <= psm_rms_limit &&
            psm_mean_abs <= psm_mean_abs_limit
        reference <- if (screen_passed) {
            if (Kocc_constant || Ksub_constant) {
                "short-screen-pass-constant-count-is-uninformative"
            } else "short-screen-pass-not-formal-convergence"
        } else "short-screen-flag-review-required"
        cbind(
            base,
            data.frame(
                log_density_rank_split_rhat = log_rhat,
                Kocc_rank_split_rhat = Kocc_rhat,
                Ksub_rank_split_rhat = Ksub_rhat,
                log_density_constant_across_draws = log_constant,
                Kocc_constant_across_draws = Kocc_constant,
                Ksub_constant_across_draws = Ksub_constant,
                Kocc_reference_status = if (Kocc_constant) {
                    "constant-agreement-uninformative"
                } else if (Kocc_ok) "rhat-within-short-screen" else
                    "rhat-flag",
                Ksub_reference_status = if (Ksub_constant) {
                    "constant-agreement-uninformative"
                } else if (Ksub_ok) "rhat-within-short-screen" else
                    "rhat-flag",
                psm_rms_between_chains = psm_rms,
                psm_mean_abs_between_chains = psm_mean_abs,
                short_screen_passed = screen_passed,
                short_pilot_reference = reference,
                stringsAsFactors = FALSE
            )
        )
    })
    do.call(rbind, values)
}

countdlm_road_calibration_projection <- function(
    runtime_summary, config, selected_rho, actual_workers
) {
    blocked <- function(reason) list(
        variant_timing = data.frame(),
        table = data.frame(),
        recommended_replicates = 0L,
        gate_hours = config$full_budget_hours * config$budget_fraction,
        internal_budget_gate_passed = FALSE,
        reason = reason,
        covered_scope = config$projected_internal_scope,
        excluded_scope = paste(
            "PNARM, AR(1) latent class, convergence extensions, reruns,",
            "formal checkpoint overhead, and any unregistered beta values"
        )
    )
    actual_workers <- gmde_scalar_integer(
        actual_workers, "actual_workers", 1L, nrow(config$pilot_tasks)
    )
    required <- c(
        "task_id", "variant_id", "method", "potts_beta_timed", "chain_id",
        "status", "elapsed_seconds", "iterations", "burn", "rho_timed",
        "algorithm_exact", "sampler_version", "fixed_rho_contract_passed",
        "completed_fit_retained",
        "warning_count", "ari_computed", "acc_computed"
    )
    if (!is.data.frame(runtime_summary) ||
        !all(required %in% names(runtime_summary)) ||
        nrow(runtime_summary) != nrow(config$pilot_tasks) ||
        !identical(runtime_summary$task_id, config$pilot_tasks$task_id) ||
        !identical(runtime_summary$variant_id,
                   config$pilot_tasks$variant_id) ||
        !identical(runtime_summary$method, config$pilot_tasks$method) ||
        !identical(as.integer(runtime_summary$chain_id),
                   as.integer(config$pilot_tasks$chain_id))) {
        return(blocked("runtime summary does not match the registered task grid"))
    }
    expected_beta <- config$pilot_tasks$potts_beta
    beta_ok <- (is.na(expected_beta) &
        is.na(runtime_summary$potts_beta_timed)) |
        (!is.na(expected_beta) &
         !is.na(runtime_summary$potts_beta_timed) &
         abs(expected_beta - runtime_summary$potts_beta_timed) < 1e-12)
    ok <- runtime_summary$status == "ok" &
        is.finite(runtime_summary$elapsed_seconds) &
        runtime_summary$elapsed_seconds > 0 &
        runtime_summary$iterations == config$pilot_iterations &
        runtime_summary$burn == config$pilot_burn &
        abs(runtime_summary$rho_timed - selected_rho) < 1e-12 &
        runtime_summary$algorithm_exact %in% TRUE &
        runtime_summary$sampler_version == config$sampler_version &
        runtime_summary$fixed_rho_contract_passed %in% TRUE &
        runtime_summary$completed_fit_retained %in% TRUE &
        runtime_summary$warning_count == 0L &
        runtime_summary$ari_computed %in% FALSE &
        runtime_summary$acc_computed %in% FALSE & beta_ok
    ok[is.na(ok)] <- FALSE
    if (!all(ok)) {
        return(blocked("one or more registered pilot tasks failed validation"))
    }
    variant_timing <- do.call(rbind, lapply(
        config$variants$variant_id,
        function(variant_id) {
            rows <- runtime_summary[
                runtime_summary$variant_id == variant_id, , drop = FALSE
            ]
            data.frame(
                variant_id = variant_id,
                method = rows$method[[1L]],
                potts_beta = rows$potts_beta_timed[[1L]],
                measured_chains = nrow(rows),
                minimum_seconds = min(rows$elapsed_seconds),
                maximum_seconds = max(rows$elapsed_seconds),
                conservative_seconds_per_formal_chain =
                    max(rows$elapsed_seconds) *
                    config$target_full_iterations / config$pilot_iterations *
                    config$projection_multiplier,
                stringsAsFactors = FALSE
            )
        }
    ))
    duration <- variant_timing$conservative_seconds_per_formal_chain
    list_schedule_upper_bound <- function(durations, workers) {
        sum(durations) / workers +
            (1 - 1 / workers) * max(durations)
    }
    projection <- do.call(rbind, lapply(
        seq_len(config$max_projected_replicates),
        function(replicates) {
            jobs <- rep(
                duration,
                times = config$target_full_chains * replicates
            )
            data.frame(
                replicates = replicates,
                chains = config$target_full_chains,
                iterations = config$target_full_iterations,
                workers = actual_workers,
                internal_method_families = length(config$methods),
                potts_beta_values = length(config$potts_beta_grid),
                chain_jobs = length(jobs),
                raw_work_hours = sum(jobs) /
                    config$projection_multiplier / 3600,
                conservative_wall_hours = list_schedule_upper_bound(
                    jobs, actual_workers
                ) / 3600,
                stringsAsFactors = FALSE
            )
        }
    ))
    gate <- config$full_budget_hours * config$budget_fraction
    allowed <- projection$replicates[
        projection$conservative_wall_hours <= gate
    ]
    recommended <- if (length(allowed)) max(allowed) else 0L
    list(
        variant_timing = variant_timing,
        table = projection,
        recommended_replicates = as.integer(recommended),
        gate_hours = gate,
        internal_budget_gate_passed = recommended >= 1L,
        reason = "all registered internal pilot tasks validated",
        covered_scope = config$projected_internal_scope,
        excluded_scope = paste(
            "PNARM, AR(1) latent class, convergence extensions, reruns,",
            "formal checkpoint overhead, and any unregistered beta values"
        )
    )
}

countdlm_road_calibration_limit_threads <- function() {
    Sys.setenv(
        OMP_NUM_THREADS = "1",
        OPENBLAS_NUM_THREADS = "1",
        MKL_NUM_THREADS = "1",
        VECLIB_MAXIMUM_THREADS = "1",
        RCPP_PARALLEL_NUM_THREADS = "1"
    )
    invisible(NULL)
}

countdlm_road_calibration_initializations <- function(Y, K, seed_base) {
    Y <- as.matrix(Y)
    K <- gmde_scalar_integer(K, "K", 2L, nrow(Y))
    seed_base <- gmde_scalar_integer(
        seed_base, "seed_base", 1L, .Machine$integer.max - 2L
    )
    set.seed(seed_base + 1L)
    data_informed <- gmde_initialize_allocations(Y, K)
    set.seed(seed_base + 2L)
    balanced_random <- sample(rep(seq_len(K), length.out = nrow(Y)))
    same_partition <- identical(
        outer(data_informed, data_informed, "=="),
        outer(balanced_random, balanced_random, "==")
    )
    if (same_partition || any(tabulate(data_informed, K) == 0L) ||
        any(tabulate(balanced_random, K) == 0L)) {
        stop(
            "The two registered initial partitions must be non-equivalent ",
            "and occupy all fitted experts.", call. = FALSE
        )
    }
    value <- list(data_informed, as.integer(balanced_random))
    attr(value, "strategies") <- c(
        "data-informed-kmeans", "balanced-random-permutation"
    )
    attr(value, "seeds") <- seed_base + seq_len(2L)
    attr(value, "adjusted_rand_between_starts") <- gmde_adjusted_rand(
        data_informed, balanced_random
    )
    value
}

countdlm_road_calibration_output_path <- function(output_dir, git_root) {
    parent <- normalizePath(
        dirname(output_dir), winslash = "/", mustWork = TRUE
    )
    path <- file.path(parent, basename(output_dir))
    if (!identical(path, output_dir)) {
        stop("The output path changed after parent-path canonicalization.",
             call. = FALSE)
    }
    root_prefix <- paste0(git_root, "/")
    if (identical(path, git_root) ||
        startsWith(paste0(path, "/"), root_prefix)) {
        stop("Calibration output must be outside the Git repository.",
             call. = FALSE)
    }
    if (file.exists(path)) {
        stop("output_dir must not already exist: ", path, call. = FALSE)
    }
    path
}

countdlm_road_try_retain_failure <- function(object, path) {
    tryCatch({
        countdlm_road_atomic_save_rds(object, path)
        list(saved = TRUE, error = NA_character_)
    }, warning = function(condition) {
        list(
            saved = FALSE,
            error = paste0("warning while retaining failure: ",
                           conditionMessage(condition))
        )
    }, error = function(condition) {
        list(
            saved = FALSE,
            error = paste0("error while retaining failure: ",
                           conditionMessage(condition))
        )
    })
}

countdlm_road_stop_if_failure_unretained <- function(results, phase) {
    for (result in results) {
        if (!is.list(result) || !isFALSE(result$failure_record_saved)) {
            next
        }
        original_error <- if (is.data.frame(result$summary) &&
                              nrow(result$summary) >= 1L &&
                              "error" %in% names(result$summary)) {
            as.character(result$summary$error[[1L]])
        } else {
            "worker error unavailable"
        }
        stop(
            "A ", phase, " worker failed and its diagnostic could not be ",
            "retained. Original worker error: ", original_error,
            "; failure-record error: ", result$failure_record_error,
            ". The registered output tree will not be recreated; this run ",
            "is invalid.", call. = FALSE
        )
    }
    invisible(TRUE)
}

countdlm_road_assert_calibration_output <- function(
    output_dir, directories, config_signature, stage
) {
    required_directories <- unique(c(output_dir, directories))
    required_files <- file.path(
        output_dir,
        c("road-calibration-registration.rds", "RUN-STARTED.rds")
    )
    missing <- c(
        required_directories[!dir.exists(required_directories)],
        required_files[!file.exists(required_files)]
    )
    if (length(missing)) {
        stop(
            "The registered calibration output tree is unavailable during ",
            stage, ". Missing: ", paste(missing, collapse = ", "),
            ". It may have been moved or deleted. The directory will not ",
            "be recreated, and this run is invalid.", call. = FALSE
        )
    }
    registration_problem <- NULL
    registration <- tryCatch(
        readRDS(required_files[[1L]]),
        error = function(condition) {
            registration_problem <<- conditionMessage(condition)
            NULL
        }
    )
    observed_signature <- if (is.list(registration) &&
                              is.list(registration$config)) {
        registration$config$config_signature
    } else NULL
    if (!is.null(registration_problem) ||
        !identical(observed_signature, config_signature)) {
        detail <- if (!is.null(registration_problem)) {
            registration_problem
        } else {
            "registered configuration signature does not match"
        }
        stop(
            "The calibration output identity could not be verified during ",
            stage, ": ", detail,
            ". The directory will not be recreated, and this run is invalid.",
            call. = FALSE
        )
    }
    invisible(TRUE)
}

#' Run the approved-road blinded short calibration pilot
#'
#' This function first calibrates rho in independent fixed-rho branches for
#' every method variant, start, and candidate.  It discards each branch's
#' settling window, pools raw movement and full state-update time with equal
#' method-family weight, applies pre-registered stability audits, and only then
#' fixes the shared rho for two 300-step chains of the same variants.  It uses
#' six single-thread workers, records warnings and failures, provides dynamic
#' Console progress, and never starts a formal simulation.
#'
#' @param config Output of `countdlm_road_calibration_config()`.
#' @param repository_root Any path inside the Git repository containing the
#'   executed `R/*.R` sources.
#' @return Calibration summaries, short-chain diagnostics, runtime projection,
#'   and immutable output paths.
#' @export
countdlm_road_calibration_pilot <- function(config, repository_root) {
    started_at <- Sys.time()
    previous_rng_kind <- RNGkind()
    had_global_random_seed <- exists(
        ".Random.seed", envir = .GlobalEnv, inherits = FALSE
    )
    previous_random_seed <- if (had_global_random_seed) {
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else NULL
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    on.exit({
        do.call(RNGkind, as.list(previous_rng_kind))
        if (had_global_random_seed) {
            assign(
                ".Random.seed", previous_random_seed,
                envir = .GlobalEnv
            )
        } else if (exists(
            ".Random.seed", envir = .GlobalEnv, inherits = FALSE
        )) {
            rm(".Random.seed", envir = .GlobalEnv)
        }
    }, add = TRUE)
    thread_variables <- c(
        "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
        "VECLIB_MAXIMUM_THREADS", "RCPP_PARALLEL_NUM_THREADS"
    )
    previous_thread_values <- Sys.getenv(
        thread_variables, unset = NA_character_
    )
    countdlm_road_calibration_limit_threads()
    on.exit({
        previously_set <- !is.na(previous_thread_values)
        if (any(previously_set)) {
            do.call(Sys.setenv, as.list(stats::setNames(
                previous_thread_values[previously_set],
                thread_variables[previously_set]
            )))
        }
        if (any(!previously_set)) {
            Sys.unsetenv(thread_variables[!previously_set])
        }
    }, add = TRUE)
    countdlm_road_validate_calibration_config(config)
    if (!requireNamespace("BayesLogit", quietly = TRUE) ||
        !requireNamespace("digest", quietly = TRUE)) {
        stop("The exact calibration pilot requires BayesLogit and digest.",
             call. = FALSE)
    }
    git <- countdlm_road_git_state(repository_root)
    output_dir <- countdlm_road_calibration_output_path(
        config$output_dir, git$root
    )
    reported_physical_cores <- parallel::detectCores(logical = FALSE)
    actual_workers <- if (.Platform$OS.type == "unix") {
        config$workers
    } else 1L
    if (!is.numeric(reported_physical_cores) ||
        length(reported_physical_cores) != 1L ||
        is.na(reported_physical_cores) ||
        !is.finite(reported_physical_cores) ||
        reported_physical_cores < 1L) {
        stop(
            "The number of physical cores could not be verified; the safe ",
            "six-worker calibration was not started.", call. = FALSE
        )
    }
    if (actual_workers > max(
            1L,
            reported_physical_cores - config$reserved_reported_cores
        )) {
        stop(
            "The calibration worker count must leave the registered number ",
            "of reported physical cores unused.", call. = FALSE
        )
    }
    if (!dir.create(output_dir, recursive = FALSE, showWarnings = FALSE)) {
        stop("Could not create the registered calibration directory.",
             call. = FALSE)
    }
    run_complete <- FALSE
    run_stage <- "registration"
    on.exit({
        if (!run_complete) {
            incomplete <- file.path(output_dir, "RUN-INCOMPLETE.rds")
            if (!dir.exists(output_dir)) {
                message(
                    "The registered calibration output directory is ",
                    "unavailable; RUN-INCOMPLETE could not be written: ",
                    output_dir
                )
            } else if (!file.exists(incomplete)) {
                incomplete_retention <- countdlm_road_try_retain_failure(
                    list(
                        api_version = config$api_version,
                        status = "failed-or-interrupted",
                        last_stage = run_stage,
                        recorded_at = format(
                            Sys.time(), tz = "UTC", usetz = TRUE
                        ),
                        formal_simulation_launched = FALSE,
                        temporary_files_retained = list.files(
                            output_dir, pattern = "[.]tmp-", recursive = TRUE,
                            full.names = FALSE
                        )
                    ),
                    incomplete
                )
                if (!isTRUE(incomplete_retention$saved)) {
                    message(
                        "RUN-INCOMPLETE could not be written: ",
                        incomplete_retention$error
                    )
                }
            }
        }
    }, add = TRUE)

    source_files <- sort(list.files(
        file.path(git$root, "R"), pattern = "[.]R$", full.names = TRUE
    ))
    registration <- list(
        api_version = config$api_version,
        created_at = format(started_at, tz = "UTC", usetz = TRUE),
        scientific_role = config$scientific_role,
        canonical = FALSE,
        formal_results_authorized = FALSE,
        git_head = git$head,
        git_clean = git$clean,
        git_status = git$status,
        execution_environment = list(
            system = as.list(Sys.info()),
            R_version = R.version.string,
            platform = R.version$platform,
            RNGkind = RNGkind(),
            BayesLogit_version = as.character(
                utils::packageVersion("BayesLogit")
            ),
            digest_version = as.character(utils::packageVersion("digest")),
            physical_cores_reported = reported_physical_cores,
            logical_cores_reported = parallel::detectCores(logical = TRUE),
            requested_workers = config$workers,
            actual_workers = actual_workers,
            reported_cores_left_unused = if (
                is.finite(reported_physical_cores)
            ) reported_physical_cores - actual_workers else NA_integer_,
            worker_thread_limit = 1L,
            BLAS = unname(extSoftVersion()[["BLAS"]])
        ),
        source_sha256 = stats::setNames(
            vapply(source_files, countdlm_road_sha256, character(1)),
            basename(source_files)
        ),
        context_sha256 = config$context_sha256,
        context_approval = "D-017",
        config = unclass(config),
        note = paste(
            "Truth-blinded calibration only; no truth-based ARI or other",
            "recovery metric is computed and no formal simulation is launched."
        )
    )
    countdlm_road_atomic_save_rds(
        registration,
        file.path(output_dir, "road-calibration-registration.rds")
    )
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            status = "running",
            started_at = format(started_at, tz = "UTC", usetz = TRUE),
            formal_simulation_launched = FALSE
        ),
        file.path(output_dir, "RUN-STARTED.rds")
    )

    run_stage <- "approved-context-and-blinded-data"
    context <- countdlm_road_load_approved_context(config$context_file)
    observed_context_sha256 <- context$sha256
    method_inputs <- countdlm_road_method_inputs(context)
    generated <- countdlm_road_generate_moderate(
        context, config$data_seed
    )
    blinded_data <- list(Y = generated$Y, Fmat = generated$Fmat)
    dgp_id <- generated$dgp
    if (!identical(names(blinded_data), c("Y", "Fmat"))) {
        stop("The calibration data object must contain only Y and Fmat.",
             call. = FALSE)
    }
    rho_initializations <- countdlm_road_calibration_initializations(
        blinded_data$Y, config$K_fit,
        config$rho_initialization_seed_base
    )
    pilot_initializations <- countdlm_road_calibration_initializations(
        blinded_data$Y, config$K_fit,
        config$pilot_initialization_seed_base
    )
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            data_seed = config$data_seed,
            dgp = dgp_id,
            Y = blinded_data$Y,
            Fmat = blinded_data$Fmat,
            Y_Fmat_sha256 = digest::digest(
                blinded_data, algo = "sha256", serialize = TRUE
            ),
            truth_fields_stored = FALSE,
            truth_metrics_computed = FALSE,
            rho_initialization_seeds = attr(
                rho_initializations, "seeds"
            ),
            rho_initialization_strategies = attr(
                rho_initializations, "strategies"
            ),
            rho_adjusted_rand_between_starts = attr(
                rho_initializations, "adjusted_rand_between_starts"
            ),
            pilot_initialization_seeds = attr(
                pilot_initializations, "seeds"
            ),
            pilot_initialization_strategies = attr(
                pilot_initializations, "strategies"
            ),
            pilot_adjusted_rand_between_starts = attr(
                pilot_initializations, "adjusted_rand_between_starts"
            ),
            rho_Z_init = rho_initializations,
            pilot_Z_init = pilot_initializations
        ),
        file.path(output_dir, "road-calibration-blinded-data.rds")
    )
    rm(generated, context)

    rho_chain_dir <- file.path(output_dir, "rho-calibration", "chains")
    rho_diagnostic_dir <- file.path(
        output_dir, "rho-calibration", "diagnostics"
    )
    pilot_chain_dir <- file.path(output_dir, "pilot", "chains")
    pilot_diagnostic_dir <- file.path(output_dir, "pilot", "diagnostics")
    failure_dir <- file.path(output_dir, "failures")
    directories <- c(
        rho_chain_dir, rho_diagnostic_dir, pilot_chain_dir,
        pilot_diagnostic_dir, failure_dir
    )
    for (directory in directories) {
        if (!dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
            stop("Could not create calibration output directories.",
                 call. = FALSE)
        }
    }
    countdlm_road_assert_calibration_output(
        output_dir = output_dir,
        directories = directories,
        config_signature = config$config_signature,
        stage = "pre-Phase-1 output verification"
    )

    rho_tasks <- config$rho_tasks
    rho_task_ids <- rho_tasks$task_id
    rho_worker <- function(index) {
        countdlm_road_calibration_limit_threads()
        task <- rho_tasks[index, , drop = FALSE]
        task_id <- task$task_id[[1L]]
        variant_id <- task$variant_id[[1L]]
        method <- task$method[[1L]]
        potts_beta <- task$potts_beta[[1L]]
        chain_id <- task$chain_id[[1L]]
        candidate_rho <- task$candidate_rho[[1L]]
        paired_seed <- task$paired_seed[[1L]]
        pair_id <- task$pair_id[[1L]]
        slug <- paste0(sprintf("%02d", index), "-", gsub(
            "[^A-Za-z0-9]+", "-", task_id
        ))
        fit_path <- file.path(
            rho_chain_dir, paste0(slug, "-fit.rds")
        )
        fit_relative_path <- file.path(
            "rho-calibration", "chains", basename(fit_path)
        )
        diagnostic_path <- file.path(
            rho_diagnostic_dir, paste0(slug, "-calibration.rds")
        )
        failure_path <- file.path(
            failure_dir, paste0(slug, "-failure.rds")
        )
        task_started <- Sys.time()
        worker_stage <- "rho-calibration-fit"
        warning_messages <- character()
        withCallingHandlers(
            tryCatch({
                fit <- countdlm_road_fit_method(
                    method = method,
                    data = blinded_data,
                    method_inputs = method_inputs,
                    config = config,
                    seed = paired_seed,
                    Z_init = rho_initializations[[chain_id]],
                    potts_beta = if (method == "Potts-MDE") {
                        potts_beta
                    } else NULL,
                    n_iter = config$rho_calibration_iterations,
                    burn = config$rho_calibration_burn,
                    rho = candidate_rho
                )
            elapsed <- as.numeric(difftime(
                Sys.time(), task_started, units = "secs"
            ))
            worker_stage <- "rho-save-completed-fit"
            countdlm_road_atomic_save_rds(
                list(
                    api_version = config$api_version,
                    task_id = task_id,
                    pair_id = pair_id,
                    variant_id = variant_id,
                    method = method,
                    potts_beta = potts_beta,
                    chain_id = chain_id,
                    candidate_rho = candidate_rho,
                    paired_seed = paired_seed,
                    warnings_before_fit_retention = warning_messages,
                    truth_metrics_computed = FALSE,
                    fit = fit
                ),
                fit_path
            )
            worker_stage <- "rho-post-fit-contract"
            if (!countdlm_road_truth_blinding_ok(
                fit, config$rho_calibration_iterations
            )) {
                stop(
                    "The fit failed the strict truth-blinding contract.",
                    call. = FALSE
                )
            }
            calibration <- countdlm_road_fixed_rho_calibration(
                fit, config$rho_calibration_settle,
                config$rho_calibration_score
            )
            initialization_sha256 <- digest::digest(
                fit$initialization, algo = "sha256", serialize = TRUE
            )
            worker_stage <- "rho-save-diagnostic"
            countdlm_road_atomic_save_rds(
                list(
                    api_version = config$api_version,
                    task_id = task_id,
                    pair_id = pair_id,
                    variant_id = variant_id,
                    method = method,
                    potts_beta = potts_beta,
                    chain_id = chain_id,
                    candidate_rho = candidate_rho,
                    paired_seed = paired_seed,
                    initialization_sha256 = initialization_sha256,
                    calibration = calibration,
                    warnings = warning_messages,
                    truth_metrics_computed = FALSE
                ),
                diagnostic_path
            )
            list(
                summary = data.frame(
                    task_id = task_id,
                    pair_id = pair_id,
                    variant_id = variant_id,
                    method = method,
                    potts_beta_timed = potts_beta,
                    chain_id = chain_id,
                    candidate_rho = candidate_rho,
                    paired_seed = paired_seed,
                    status = "ok",
                    elapsed_seconds = elapsed,
                    iterations = config$rho_calibration_iterations,
                    burn = config$rho_calibration_burn,
                    settle_transitions = config$rho_calibration_settle,
                    scored_transitions = config$rho_calibration_score,
                    score_first_iteration =
                        config$rho_calibration_settle + 1L,
                    score_last_iteration =
                        config$rho_calibration_iterations,
                    initialization_sha256 = initialization_sha256,
                    algorithm_exact = isTRUE(fit$algorithm_exact),
                    sampler_version = fit$settings$sampler_version,
                    fixed_rho_contract_passed = TRUE,
                    warning_count = length(warning_messages),
                    warnings = if (length(warning_messages)) {
                        paste(warning_messages, collapse = " | ")
                    } else NA_character_,
                    failure_stage = NA_character_,
                    error = NA_character_,
                    fit_file = fit_relative_path,
                    completed_fit_retained = TRUE,
                    stringsAsFactors = FALSE
                ),
                calibration = calibration
            )
        }, error = function(error) {
            elapsed <- as.numeric(difftime(
                Sys.time(), task_started, units = "secs"
            ))
            summary <- data.frame(
                task_id = task_id,
                pair_id = pair_id,
                variant_id = variant_id,
                method = method,
                potts_beta_timed = potts_beta,
                chain_id = chain_id,
                candidate_rho = candidate_rho,
                paired_seed = paired_seed,
                status = "error",
                elapsed_seconds = elapsed,
                iterations = config$rho_calibration_iterations,
                burn = config$rho_calibration_burn,
                settle_transitions = config$rho_calibration_settle,
                scored_transitions = config$rho_calibration_score,
                score_first_iteration =
                    config$rho_calibration_settle + 1L,
                score_last_iteration = config$rho_calibration_iterations,
                initialization_sha256 = NA_character_,
                algorithm_exact = NA,
                sampler_version = NA_character_,
                fixed_rho_contract_passed = FALSE,
                warning_count = length(warning_messages),
                warnings = if (length(warning_messages)) {
                    paste(warning_messages, collapse = " | ")
                } else NA_character_,
                failure_stage = worker_stage,
                error = conditionMessage(error),
                fit_file = if (file.exists(fit_path)) {
                    fit_relative_path
                } else NA_character_,
                completed_fit_retained = file.exists(fit_path),
                stringsAsFactors = FALSE
            )
            failure_retention <- countdlm_road_try_retain_failure(
                list(
                    api_version = config$api_version,
                    phase = "rho-calibration",
                    task_id = task_id,
                    pair_id = pair_id,
                    variant_id = variant_id,
                    method = method,
                    potts_beta = potts_beta,
                    chain_id = chain_id,
                    candidate_rho = candidate_rho,
                    paired_seed = paired_seed,
                    status = "error",
                    elapsed_seconds = elapsed,
                    stage = worker_stage,
                    error = conditionMessage(error),
                    call = paste(deparse(conditionCall(error)), collapse = " "),
                    warnings = warning_messages,
                    retained = TRUE,
                    completed_fit_retained = file.exists(fit_path),
                    retained_fit_file = if (file.exists(fit_path)) {
                        fit_relative_path
                    } else NA_character_,
                    summary = summary
                ),
                failure_path
            )
            list(
                summary = summary,
                calibration = NULL,
                failure_record_saved = failure_retention$saved,
                failure_record_error = failure_retention$error
            )
            }),
            warning = function(warning) {
                warning_messages <<- unique(c(
                    warning_messages, conditionMessage(warning)
                ))
                invokeRestart("muffleWarning")
            }
        )
    }

    cat(
        "Road calibration pilot, phase 1/2: 7 variants x 2 starts x 3 independent fixed-rho branches\n",
        "rho grid = 1,2,4; 120 transitions per branch; first 60 unscored, last 60 scored; ",
        actual_workers, " worker(s)\n", sep = ""
    )
    run_stage <- "rho-calibration"
    rho_started <- Sys.time()
    rho_results <- vector("list", nrow(rho_tasks))
    for (variant_index in seq_len(nrow(config$variants))) {
        variant_id <- config$variants$variant_id[[variant_index]]
        indices <- which(rho_tasks$variant_id == variant_id)
        countdlm_road_assert_calibration_output(
            output_dir = output_dir,
            directories = directories,
            config_signature = config$config_signature,
            stage = paste0("before Phase-1 batch ", variant_index)
        )
        cat(
            "[RHO BATCH ", variant_index, "/", nrow(config$variants),
            "] ", variant_id, ": 2 starts x 3 candidates\n", sep = ""
        )
        batch <- countdlm_road_run_batches(
            rho_task_ids[indices],
            function(local_index) rho_worker(indices[[local_index]]),
            actual_workers,
            poll_seconds = config$progress_poll_seconds
        )
        rho_results[indices] <- batch
        countdlm_road_stop_if_failure_unretained(
            batch, paste0("Phase-1 batch ", variant_index)
        )
        countdlm_road_assert_calibration_output(
            output_dir = output_dir,
            directories = directories,
            config_signature = config$config_signature,
            stage = paste0("after Phase-1 batch ", variant_index)
        )
    }
    rho_wall_seconds <- as.numeric(difftime(
        Sys.time(), rho_started, units = "secs"
    ))
    for (index in seq_along(rho_results)) {
        if (!inherits(
            rho_results[[index]], "countdlm_road_scheduler_error"
        )) next
        error <- rho_results[[index]]
        task <- rho_tasks[index, , drop = FALSE]
        task_id <- task$task_id[[1L]]
        summary <- data.frame(
            task_id = task_id,
            pair_id = task$pair_id[[1L]],
            variant_id = task$variant_id[[1L]],
            method = task$method[[1L]],
            potts_beta_timed = task$potts_beta[[1L]],
            chain_id = task$chain_id[[1L]],
            candidate_rho = task$candidate_rho[[1L]],
            paired_seed = task$paired_seed[[1L]],
            status = "error",
            elapsed_seconds = error$elapsed_seconds,
            iterations = config$rho_calibration_iterations,
            burn = config$rho_calibration_burn,
            settle_transitions = config$rho_calibration_settle,
            scored_transitions = config$rho_calibration_score,
            score_first_iteration = config$rho_calibration_settle + 1L,
            score_last_iteration = config$rho_calibration_iterations,
            initialization_sha256 = NA_character_,
            algorithm_exact = NA,
            sampler_version = NA_character_,
            fixed_rho_contract_passed = FALSE,
            warning_count = 0L,
            warnings = NA_character_,
            failure_stage = if (identical(
                error$origin, "scheduler-collect"
            )) "parallel-scheduler-collect" else "parallel-worker-uncaught",
            error = error$message,
            fit_file = NA_character_,
            completed_fit_retained = FALSE,
            stringsAsFactors = FALSE
        )
        failure_retention <- countdlm_road_try_retain_failure(
            list(
                api_version = config$api_version,
                phase = "rho-calibration",
                status = "error",
                retained = TRUE,
                error_origin = error$origin,
                original_error = error$message,
                summary = summary
            ),
            file.path(
                failure_dir,
                paste0("rho-", sprintf("%02d", index),
                       "-scheduler-failure.rds")
            )
        )
        rho_results[[index]] <- list(
            summary = summary,
            calibration = NULL,
            failure_record_saved = failure_retention$saved,
            failure_record_error = failure_retention$error
        )
    }
    countdlm_road_stop_if_failure_unretained(
        rho_results, "Phase-1 scheduler reconciliation"
    )
    countdlm_road_assert_calibration_output(
        output_dir = output_dir,
        directories = directories,
        config_signature = config$config_signature,
        stage = "Phase-1 scheduler reconciliation"
    )
    rho_runtime <- do.call(rbind, lapply(rho_results, `[[`, "summary"))
    rownames(rho_runtime) <- NULL
    countdlm_road_atomic_write_csv(
        rho_runtime,
        file.path(output_dir, "rho-calibration-runtime.csv")
    )
    rho_expected_beta <- rho_tasks$potts_beta
    rho_beta_ok <- (is.na(rho_expected_beta) &
        is.na(rho_runtime$potts_beta_timed)) |
        (!is.na(rho_expected_beta) &
         !is.na(rho_runtime$potts_beta_timed) &
         abs(rho_expected_beta - rho_runtime$potts_beta_timed) < 1e-12)
    rho_contract_ok <- nrow(rho_runtime) == nrow(rho_tasks) &&
        identical(rho_runtime$task_id, rho_tasks$task_id) &&
        identical(rho_runtime$pair_id, rho_tasks$pair_id) &&
        identical(rho_runtime$variant_id, rho_tasks$variant_id) &&
        identical(rho_runtime$method, rho_tasks$method) &&
        identical(as.integer(rho_runtime$chain_id),
                  as.integer(rho_tasks$chain_id)) &&
        identical(
            as.numeric(rho_runtime$candidate_rho),
            as.numeric(rho_tasks$candidate_rho)
        ) && identical(
            as.integer(rho_runtime$paired_seed),
            as.integer(rho_tasks$paired_seed)
        )
    rho_valid <- rho_contract_ok & rho_runtime$status == "ok" &
        rho_runtime$warning_count == 0L &
        rho_runtime$algorithm_exact %in% TRUE &
        rho_runtime$sampler_version == config$sampler_version &
        rho_runtime$fixed_rho_contract_passed %in% TRUE &
        rho_runtime$completed_fit_retained %in% TRUE &
        !is.na(rho_runtime$fit_file) & nzchar(rho_runtime$fit_file) &
        file.exists(file.path(output_dir, rho_runtime$fit_file)) &
        rho_runtime$iterations == config$rho_calibration_iterations &
        rho_runtime$burn == config$rho_calibration_burn &
        rho_runtime$settle_transitions == config$rho_calibration_settle &
        rho_runtime$scored_transitions == config$rho_calibration_score &
        rho_runtime$score_first_iteration ==
            config$rho_calibration_settle + 1L &
        rho_runtime$score_last_iteration ==
            config$rho_calibration_iterations &
        !is.na(rho_runtime$initialization_sha256) &
        grepl(
            "^[0-9a-f]{64}$", rho_runtime$initialization_sha256
        ) & rho_beta_ok
    rho_valid[is.na(rho_valid)] <- FALSE
    if (!all(rho_valid)) {
        stop(
            "The rho phase did not complete warning-free in all 42 branches; ",
            "the fixed-rho pilot was not started.", call. = FALSE
        )
    }

    pair_tasks <- config$pilot_tasks
    pair_ids <- pair_tasks$task_id
    rho_calibrations <- vector("list", length(pair_ids))
    rho_pair_audit <- vector("list", length(pair_ids))
    for (pair_index in seq_along(pair_ids)) {
        indices <- which(rho_tasks$pair_id == pair_ids[[pair_index]])
        indices <- indices[order(rho_tasks$candidate_rho[indices])]
        records <- lapply(rho_results[indices], `[[`, "calibration")
        initialization_hashes <- rho_runtime$initialization_sha256[indices]
        paired_seeds <- rho_runtime$paired_seed[indices]
        calibration <- do.call(rbind, records)
        rownames(calibration) <- NULL
        init_match <- length(unique(initialization_hashes)) == 1L
        seed_match <- length(unique(paired_seeds)) == 1L
        candidate_match <- identical(
            as.numeric(calibration$rho), as.numeric(config$rho_grid)
        )
        if (length(indices) != length(config$rho_grid) || !init_match ||
            !seed_match || !candidate_match) {
            stop(
                "A paired fixed-rho branch group failed its initialization ",
                "or candidate-grid audit; phase 2 was not started.",
                call. = FALSE
            )
        }
        selection <- countdlm_road_pool_rho_calibration(
            list(calibration), config$rho_grid, 1,
            config$rho_tie_break, config$rho_near_optimal_tolerance
        )
        finite_movement_shares <- calibration$maximum_iteration_movement_share[
            is.finite(calibration$maximum_iteration_movement_share)
        ]
        maximum_movement_share <- if (length(finite_movement_shares)) {
            max(finite_movement_shares)
        } else NA_real_
        selected_movement_share <- if (selection$selection_available) {
            calibration[
                calibration$rho == selection$selected_rho,
                "maximum_iteration_movement_share"
            ][[1L]]
        } else NA_real_
        all_branches_sufficient <- all(
            calibration$accepted_movement_sufficient %in% TRUE
        )
        all_branches_below_limit <- all_branches_sufficient && all(
            is.finite(calibration$maximum_iteration_movement_share) &
                calibration$maximum_iteration_movement_share <=
                    config$rho_max_iteration_movement_share
        )
        rho_calibrations[[pair_index]] <- calibration
        rho_pair_audit[[pair_index]] <- data.frame(
            pair_id = pair_ids[[pair_index]],
            variant_id = pair_tasks$variant_id[[pair_index]],
            method = pair_tasks$method[[pair_index]],
            potts_beta = pair_tasks$potts_beta[[pair_index]],
            chain_id = pair_tasks$chain_id[[pair_index]],
            paired_seed = paired_seeds[[1L]],
            initialization_sha256 = initialization_hashes[[1L]],
            initialization_match = init_match,
            seed_match = seed_match,
            candidate_grid_match = candidate_match,
            selected_rho_within_pair = selection$selected_rho,
            raw_best_rho_within_pair = selection$raw_best_rho,
            rho_selection_available = selection$selection_available,
            rho_selection_status = selection$selection_status,
            selected_relative_efficiency =
                selection$selected_relative_efficiency,
            all_branches_have_accepted_movement = all_branches_sufficient,
            maximum_branch_iteration_movement_share =
                maximum_movement_share,
            selected_branch_iteration_movement_share =
                selected_movement_share,
            all_branches_below_movement_share_limit =
                all_branches_below_limit,
            stringsAsFactors = FALSE
        )
    }
    rho_pair_audit <- do.call(rbind, rho_pair_audit)
    rownames(rho_pair_audit) <- NULL
    rho_weights <- ifelse(
        pair_tasks$method == "Potts-MDE", config$potts_variant_weight, 1
    )
    rho_pooled <- countdlm_road_pool_rho_calibration(
        rho_calibrations, config$rho_grid, rho_weights,
        config$rho_tie_break, config$rho_near_optimal_tolerance
    )
    rho_pooled$weight_rule <- paste(
        "Each non-Potts method-chain record has weight 1; each Potts",
        "beta-chain record has weight 1/3, so the Potts family has the",
        "same aggregate weight as each other method family."
    )
    selected_rho <- rho_pooled$selected_rho
    rho_long <- countdlm_road_rho_long_table(
        pair_tasks, rho_calibrations, rho_weights,
        rho_pair_audit$selected_rho_within_pair,
        branch_metadata = rho_runtime
    )
    rho_sensitivity <- function(
        calibrations, weights, group_values, group_name
    ) {
        values <- lapply(unique(group_values), function(group_value) {
            indices <- which(group_values == group_value)
            pooled <- countdlm_road_pool_rho_calibration(
                calibrations[indices], config$rho_grid,
                weights[indices], config$rho_tie_break,
                config$rho_near_optimal_tolerance
            )
            data.frame(
                grouping = rep(group_name, nrow(pooled$table)),
                group = rep(as.character(group_value), nrow(pooled$table)),
                records = rep(length(indices), nrow(pooled$table)),
                pooled$table,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )
        })
        value <- do.call(rbind, values)
        rownames(value) <- NULL
        value
    }
    rho_variant_sensitivity <- rho_sensitivity(
        rho_calibrations, rho_weights, pair_tasks$variant_id, "variant"
    )
    rho_family_sensitivity <- rho_sensitivity(
        rho_calibrations, rho_weights, pair_tasks$method, "method-family"
    )
    rho_family_regret_audit <- do.call(rbind, lapply(
        unique(rho_family_sensitivity$group),
        function(method_family) {
            rows <- rho_family_sensitivity[
                rho_family_sensitivity$group == method_family,
                , drop = FALSE
            ]
            best_efficiency <- max(rows$efficiency)
            family_selection_available <- best_efficiency > 0
            shared_efficiency <- if (
                isTRUE(rho_pooled$selection_available) &&
                is.finite(selected_rho)
            ) {
                rows$efficiency[rows$rho == selected_rho][[1L]]
            } else NA_real_
            relative_efficiency <- if (
                family_selection_available && is.finite(shared_efficiency)
            ) shared_efficiency / best_efficiency else NA_real_
            data.frame(
                method_family = method_family,
                shared_rho = selected_rho,
                family_best_rho = if (family_selection_available) rows$rho[
                    rows$efficiency == best_efficiency
                ][[1L]] else NA_real_,
                family_selection_available = family_selection_available,
                shared_efficiency = shared_efficiency,
                family_best_efficiency = best_efficiency,
                shared_relative_efficiency = relative_efficiency,
                minimum_required_relative_efficiency =
                    config$rho_min_family_relative_efficiency,
                family_efficiency_gate_passed =
                    is.finite(relative_efficiency) &&
                    relative_efficiency >=
                        config$rho_min_family_relative_efficiency,
                stringsAsFactors = FALSE
            )
        }
    ))
    rownames(rho_family_regret_audit) <- NULL
    rho_start_sensitivity <- rho_sensitivity(
        rho_calibrations, rho_weights,
        c("data-informed", "balanced-random")[pair_tasks$chain_id],
        "initialization"
    )
    first_half_calibrations <- lapply(
        rho_calibrations, countdlm_road_rho_half_calibration,
        half = "first"
    )
    second_half_calibrations <- lapply(
        rho_calibrations, countdlm_road_rho_half_calibration,
        half = "second"
    )
    rho_half_sensitivity <- rbind(
        rho_sensitivity(
            first_half_calibrations, rho_weights,
            rep("first", length(rho_calibrations)), "score-half"
        ),
        rho_sensitivity(
            second_half_calibrations, rho_weights,
            rep("second", length(rho_calibrations)), "score-half"
        )
    )
    rownames(rho_half_sensitivity) <- NULL
    stratified_selected <- c(
        rho_start_sensitivity$rho[rho_start_sensitivity$selected],
        rho_half_sensitivity$rho[rho_half_sensitivity$selected]
    )
    if (length(stratified_selected) == 4L) {
        names(stratified_selected) <- c(
            "start-data-informed", "start-balanced-random",
            "score-half-first", "score-half-second"
        )
    }
    rho_selection_available <- isTRUE(rho_pooled$selection_available) &&
        is.finite(selected_rho)
    rho_strata_match <- rho_selection_available &&
        length(stratified_selected) == 4L &&
        all(is.finite(stratified_selected)) &&
        all(stratified_selected == selected_rho)
    rho_all_branches_have_movement <- all(
        rho_long$accepted_movement_sufficient %in% TRUE
    )
    rho_movement_share_ok <- all(
        !rho_long$accepted_movement_sufficient |
            (
        is.finite(rho_long$maximum_iteration_movement_share) &
            rho_long$maximum_iteration_movement_share <=
                config$rho_max_iteration_movement_share
            )
    )
    rho_branch_movement_ok <- rho_all_branches_have_movement &&
        rho_movement_share_ok
    rho_family_efficiency_ok <- all(
        rho_family_regret_audit$family_efficiency_gate_passed
    )
    rho_resolution <- countdlm_road_rho_resolution_decision(
        selection_available = rho_selection_available,
        strata_match = rho_strata_match,
        all_branches_have_movement = rho_all_branches_have_movement,
        movement_share_ok = rho_movement_share_ok,
        family_efficiency_ok = rho_family_efficiency_ok
    )
    rho_resolved <- rho_resolution$resolved
    rho_pooled$resolution_rule <- config$rho_resolution_rule
    rho_pooled$stratified_selected_rho <- stratified_selected
    rho_pooled$strata_match <- rho_strata_match
    finite_movement_shares <- rho_long$maximum_iteration_movement_share[
        is.finite(rho_long$maximum_iteration_movement_share)
    ]
    rho_pooled$maximum_observed_iteration_movement_share <- if (
        length(finite_movement_shares)
    ) max(finite_movement_shares) else NA_real_
    rho_pooled$maximum_allowed_iteration_movement_share <-
        config$rho_max_iteration_movement_share
    rho_pooled$all_branches_have_accepted_movement <-
        rho_all_branches_have_movement
    rho_pooled$movement_share_gate_passed <- rho_movement_share_ok
    rho_pooled$branch_movement_gate_passed <- rho_branch_movement_ok
    finite_family_efficiencies <-
        rho_family_regret_audit$shared_relative_efficiency[
            is.finite(rho_family_regret_audit$shared_relative_efficiency)
        ]
    rho_pooled$minimum_family_relative_efficiency <- if (
        length(finite_family_efficiencies)
    ) min(finite_family_efficiencies) else NA_real_
    rho_pooled$minimum_required_family_relative_efficiency <-
        config$rho_min_family_relative_efficiency
    rho_pooled$family_efficiency_gate_passed <- rho_family_efficiency_ok
    rho_pooled <- countdlm_road_finalize_rho_pool(
        rho_pooled, selected_rho, rho_resolution
    )
    rho_family_regret_audit$provisional_shared_rho <-
        rho_family_regret_audit$shared_rho
    rho_family_regret_audit$final_shared_rho <- if (rho_resolved) {
        rho_family_regret_audit$shared_rho
    } else rep(NA_real_, nrow(rho_family_regret_audit))
    rho_family_regret_audit$shared_rho <-
        rho_family_regret_audit$final_shared_rho
    rho_family_regret_audit$rho_resolved <- rep(
        rho_resolved, nrow(rho_family_regret_audit)
    )
    rho_family_regret_audit$resolution_status <- rep(
        rho_pooled$resolution_status, nrow(rho_family_regret_audit)
    )
    countdlm_road_atomic_write_csv(
        rho_pooled$table,
        file.path(output_dir, "rho-calibration-pooled.csv")
    )
    countdlm_road_atomic_write_csv(
        rho_long,
        file.path(output_dir, "rho-calibration-by-task.csv")
    )
    countdlm_road_atomic_write_csv(
        rho_pair_audit,
        file.path(output_dir, "rho-calibration-pair-audit.csv")
    )
    countdlm_road_atomic_write_csv(
        rho_variant_sensitivity,
        file.path(output_dir, "rho-calibration-by-variant.csv")
    )
    countdlm_road_atomic_write_csv(
        rho_family_sensitivity,
        file.path(output_dir, "rho-calibration-by-family.csv")
    )
    countdlm_road_atomic_write_csv(
        rho_family_regret_audit,
        file.path(output_dir, "rho-calibration-family-regret-audit.csv")
    )
    countdlm_road_atomic_write_csv(
        rho_start_sensitivity,
        file.path(output_dir, "rho-calibration-by-initialization.csv")
    )
    countdlm_road_atomic_write_csv(
        rho_half_sensitivity,
        file.path(output_dir, "rho-calibration-by-score-half.csv")
    )
    countdlm_road_atomic_save_rds(
        rho_pooled,
        file.path(output_dir, "rho-calibration-pooled.rds")
    )
    if (!rho_resolved) {
        total_wall_seconds <- as.numeric(difftime(
            Sys.time(), started_at, units = "secs"
        ))
        result <- list(
            api_version = config$api_version,
            config = config,
            registration = registration,
            selected_rho = NA_real_,
            provisional_selected_rho = selected_rho,
            rho_runtime = rho_runtime,
            rho_pooled = rho_pooled,
            rho_by_task = rho_long,
            rho_pair_audit = rho_pair_audit,
            rho_variant_sensitivity = rho_variant_sensitivity,
            rho_family_sensitivity = rho_family_sensitivity,
            rho_family_regret_audit = rho_family_regret_audit,
            rho_start_sensitivity = rho_start_sensitivity,
            rho_half_sensitivity = rho_half_sensitivity,
            rho_wall_seconds = rho_wall_seconds,
            rho_resolved = FALSE,
            phase2_started = FALSE,
            truth_metrics_computed = FALSE,
            eligible_for_formal_freeze = FALSE,
            formal_simulation_launched = FALSE,
            total_wall_seconds_through_derived_tables = total_wall_seconds
        )
        result_path <- file.path(output_dir, "road-calibration-result.rds")
        countdlm_road_atomic_save_rds(result, result_path)
        report <- c(
            "countDLM approved-road rho calibration report",
            paste("API:", config$api_version),
            paste("Output:", output_dir),
            paste("Git HEAD:", git$head),
            paste("Git clean:", git$clean),
            paste("Context SHA-256:", observed_context_sha256),
            paste("Rho design:", config$rho_design),
            paste("Rho branches:", nrow(config$rho_tasks)),
            paste(
                "Transitions / settle / score per branch:",
                config$rho_calibration_iterations, "/",
                config$rho_calibration_settle, "/",
                config$rho_calibration_score
            ),
            paste("Provisional pooled rho:", selected_rho),
            paste("Resolution status:", rho_pooled$resolution_status),
            paste("Movement scope:", config$rho_movement_scope),
            paste("Timing scope:", config$rho_timing_scope),
            paste(
                "Rho phase wall time:",
                countdlm_road_format_duration(rho_wall_seconds)
            ),
            "Phase 2 was intentionally not started because a pre-registered rho audit did not pass.",
            "This is a completed diagnostic outcome, not a runtime failure and not a formal result.",
            "No truth labels were stored or passed to a worker; no truth-based recovery metric was computed.",
            "Return the complete retained directory for review before extending calibration.",
            "",
            "Pooled rho calibration:",
            utils::capture.output(print(rho_pooled$table, row.names = FALSE)),
            "",
            "Method-family regret audit:",
            utils::capture.output(print(
                rho_family_regret_audit, row.names = FALSE
            ))
        )
        report_path <- file.path(output_dir, "road-calibration-report.txt")
        countdlm_road_atomic_write_lines(report, report_path)
        cat("\n", paste(report, collapse = "\n"), "\n", sep = "")
        run_stage <- "checksums-and-rho-unresolved-marker"
        checksum_path <- countdlm_road_write_checksums(output_dir)
        completion_path <- file.path(output_dir, "RUN-COMPLETE.rds")
        countdlm_road_atomic_save_rds(
            list(
                api_version = config$api_version,
                status = "rho-calibration-unresolved-phase2-not-started",
                completed_at = format(
                    Sys.time(), tz = "UTC", usetz = TRUE
                ),
                selected_rho = NA_real_,
                provisional_selected_rho = selected_rho,
                rho_resolution_status = rho_pooled$resolution_status,
                rho_resolved = FALSE,
                phase2_started = FALSE,
                truth_metrics_computed = FALSE,
                formal_simulation_launched = FALSE,
                checksums_cover_payload_before_this_marker = basename(
                    checksum_path
                )
            ),
            completion_path
        )
        run_complete <- TRUE
        return(invisible(c(
            result,
            list(
                result_file = result_path,
                report_file = report_path,
                checksum_file = checksum_path,
                completion_file = completion_path
            )
        )))
    }
    cat("Pooled rho selected without truth metrics: ", selected_rho, "\n",
        sep = "")

    pilot_worker <- function(index) {
        countdlm_road_calibration_limit_threads()
        task <- config$pilot_tasks[index, , drop = FALSE]
        task_id <- task$task_id[[1L]]
        variant_id <- task$variant_id[[1L]]
        method <- task$method[[1L]]
        potts_beta <- task$potts_beta[[1L]]
        chain_id <- task$chain_id[[1L]]
        slug <- paste0(sprintf("%02d", index), "-", gsub(
            "[^A-Za-z0-9]+", "-", task_id
        ))
        fit_path <- file.path(pilot_chain_dir, paste0(slug, "-fit.rds"))
        fit_relative_path <- file.path(
            "pilot", "chains", basename(fit_path)
        )
        diagnostic_path <- file.path(
            pilot_diagnostic_dir, paste0(slug, "-diagnostic.rds")
        )
        failure_path <- file.path(
            failure_dir, paste0("pilot-", slug, "-failure.rds")
        )
        task_started <- Sys.time()
        worker_stage <- "pilot-fit"
        warning_messages <- character()
        withCallingHandlers(
            tryCatch({
                fit <- countdlm_road_fit_method(
                    method = method,
                    data = blinded_data,
                    method_inputs = method_inputs,
                    config = config,
                    seed = config$pilot_method_seed_base + 101L * index,
                    Z_init = pilot_initializations[[chain_id]],
                    potts_beta = if (method == "Potts-MDE") {
                        potts_beta
                    } else NULL,
                    n_iter = config$pilot_iterations,
                    burn = config$pilot_burn,
                    rho = selected_rho
                )
            elapsed <- as.numeric(difftime(
                Sys.time(), task_started, units = "secs"
            ))
            worker_stage <- "pilot-save-completed-fit"
            countdlm_road_atomic_save_rds(
                list(
                    api_version = config$api_version,
                    task_id = task_id,
                    variant_id = variant_id,
                    method = method,
                    potts_beta = potts_beta,
                    chain_id = chain_id,
                    seed = config$pilot_method_seed_base + 101L * index,
                    selected_rho = selected_rho,
                    warnings_before_fit_retention = warning_messages,
                    truth_metrics_computed = FALSE,
                    fit = fit
                ),
                fit_path
            )
            worker_stage <- "pilot-post-fit-contract"
            if (!countdlm_road_truth_blinding_ok(
                fit, config$pilot_iterations
            )) {
                stop(
                    "The fit failed the strict truth-blinding contract.",
                    call. = FALSE
                )
            }
            worker_stage <- "pilot-fixed-rho-contract"
            pilot_fixed_rho_audit <- countdlm_road_fixed_rho_calibration(
                fit = fit,
                settle = config$pilot_burn,
                score = config$pilot_iterations - config$pilot_burn
            )
            worker_stage <- "pilot-summarize"
            summarized <- countdlm_road_summarize_fit(
                fit = fit, task_id = task_id, method = method,
                potts_beta = potts_beta, burn = config$pilot_burn,
                elapsed = elapsed, warnings = warning_messages
            )
            summarized$summary$variant_id <- variant_id
            summarized$summary$chain_id <- chain_id
            summarized$summary$sampler_version <-
                fit$settings$sampler_version
            summarized$summary$fixed_rho_contract_passed <- TRUE
            summarized$summary$ari_computed <- any(!is.na(fit$ari))
            summarized$summary$acc_computed <- any(!is.na(fit$acc))
            summarized$summary$fit_file <- fit_relative_path
            summarized$summary$completed_fit_retained <- TRUE
            summarized$compact$variant_id <- variant_id
            summarized$compact$chain_id <- chain_id
            summarized$compact$selected_rho <- selected_rho
            summarized$compact$summary_schema_version <-
                countdlm_road_benchmark_api_version
            summarized$compact$api_version <- config$api_version
            summarized$compact$fixed_rho_contract <- pilot_fixed_rho_audit
            worker_stage <- "pilot-save-diagnostic"
            countdlm_road_atomic_save_rds(
                summarized$compact, diagnostic_path
            )
            summarized$summary$warning_count <- length(warning_messages)
            summarized$summary$warnings <- if (length(warning_messages)) {
                paste(warning_messages, collapse = " | ")
            } else NA_character_
            list(summary = summarized$summary)
        }, error = function(error) {
            elapsed <- as.numeric(difftime(
                Sys.time(), task_started, units = "secs"
            ))
            summary <- countdlm_road_failure_summary(
                task_id = task_id, method = method,
                potts_beta = potts_beta, elapsed = elapsed,
                config = list(
                    quick_iterations = config$pilot_iterations,
                    quick_burn = config$pilot_burn,
                    rho_timing = selected_rho
                ),
                error = conditionMessage(error), stage = worker_stage,
                warnings = warning_messages
            )
            summary$variant_id <- variant_id
            summary$chain_id <- chain_id
            summary$sampler_version <- NA_character_
            summary$fixed_rho_contract_passed <- FALSE
            summary$ari_computed <- FALSE
            summary$acc_computed <- FALSE
            summary$fit_file <- if (file.exists(fit_path)) {
                fit_relative_path
            } else NA_character_
            summary$completed_fit_retained <- file.exists(fit_path)
            failure_retention <- countdlm_road_try_retain_failure(
                list(
                    api_version = config$api_version,
                    phase = "fixed-rho-two-chain-pilot",
                    task_id = task_id,
                    variant_id = variant_id,
                    method = method,
                    potts_beta = potts_beta,
                    chain_id = chain_id,
                    status = "error",
                    elapsed_seconds = elapsed,
                    stage = worker_stage,
                    error = conditionMessage(error),
                    call = paste(deparse(conditionCall(error)), collapse = " "),
                    warnings = warning_messages,
                    retained = TRUE,
                    completed_fit_retained = file.exists(fit_path),
                    retained_fit_file = if (file.exists(fit_path)) {
                        fit_relative_path
                    } else NA_character_,
                    summary = summary
                ),
                failure_path
            )
            list(
                summary = summary,
                failure_record_saved = failure_retention$saved,
                failure_record_error = failure_retention$error
            )
            }),
            warning = function(warning) {
                warning_messages <<- unique(c(
                    warning_messages, conditionMessage(warning)
                ))
                invokeRestart("muffleWarning")
            }
        )
    }

    cat(
        "Road calibration pilot, phase 2/2: seven variants x two chains\n",
        "300 transitions / 150 burn; fixed pooled rho = ", selected_rho,
        "; ", actual_workers, " worker(s), leaving approximately ",
        config$reserved_reported_cores, " reported cores unused\n", sep = ""
    )
    run_stage <- "fixed-rho-two-chain-pilot"
    countdlm_road_assert_calibration_output(
        output_dir = output_dir,
        directories = directories,
        config_signature = config$config_signature,
        stage = "before Phase-2 dispatch"
    )
    pilot_started <- Sys.time()
    pilot_results <- countdlm_road_run_batches(
        config$pilot_tasks$task_id, pilot_worker, actual_workers,
        poll_seconds = config$progress_poll_seconds
    )
    countdlm_road_stop_if_failure_unretained(
        pilot_results, "Phase-2 dispatch"
    )
    countdlm_road_assert_calibration_output(
        output_dir = output_dir,
        directories = directories,
        config_signature = config$config_signature,
        stage = "after Phase-2 dispatch"
    )
    pilot_wall_seconds <- as.numeric(difftime(
        Sys.time(), pilot_started, units = "secs"
    ))
    for (index in seq_along(pilot_results)) {
        if (!inherits(
            pilot_results[[index]], "countdlm_road_scheduler_error"
        )) next
        error <- pilot_results[[index]]
        task <- config$pilot_tasks[index, , drop = FALSE]
        summary <- countdlm_road_failure_summary(
            task_id = task$task_id[[1L]], method = task$method[[1L]],
            potts_beta = task$potts_beta[[1L]],
            elapsed = error$elapsed_seconds,
            config = list(
                quick_iterations = config$pilot_iterations,
                quick_burn = config$pilot_burn,
                rho_timing = selected_rho
            ),
            error = error$message,
            stage = if (identical(error$origin, "scheduler-collect")) {
                "parallel-scheduler-collect"
            } else "parallel-worker-uncaught"
        )
        summary$variant_id <- task$variant_id[[1L]]
        summary$chain_id <- task$chain_id[[1L]]
        summary$sampler_version <- NA_character_
        summary$fixed_rho_contract_passed <- FALSE
        summary$ari_computed <- FALSE
        summary$acc_computed <- FALSE
        summary$fit_file <- NA_character_
        summary$completed_fit_retained <- FALSE
        failure_retention <- countdlm_road_try_retain_failure(
            list(
                api_version = config$api_version,
                phase = "fixed-rho-two-chain-pilot",
                status = "error",
                retained = TRUE,
                error_origin = error$origin,
                original_error = error$message,
                summary = summary
            ),
            file.path(
                failure_dir,
                paste0("pilot-", sprintf("%02d", index),
                       "-scheduler-failure.rds")
            )
        )
        pilot_results[[index]] <- list(
            summary = summary,
            failure_record_saved = failure_retention$saved,
            failure_record_error = failure_retention$error
        )
    }
    countdlm_road_stop_if_failure_unretained(
        pilot_results, "Phase-2 scheduler reconciliation"
    )
    countdlm_road_assert_calibration_output(
        output_dir = output_dir,
        directories = directories,
        config_signature = config$config_signature,
        stage = "Phase-2 scheduler reconciliation"
    )
    pilot_runtime <- do.call(rbind, lapply(
        pilot_results, `[[`, "summary"
    ))
    pilot_runtime <- pilot_runtime[
        match(config$pilot_tasks$task_id, pilot_runtime$task_id), , drop = FALSE
    ]
    rownames(pilot_runtime) <- NULL
    run_stage <- "pilot-derived-diagnostics"
    pair_diagnostics <- countdlm_road_pair_diagnostics(
        pilot_runtime, config$pilot_tasks, output_dir,
        config$pilot_burn, config$pilot_iterations,
        log_rhat_limit = config$short_log_rhat_limit,
        count_rhat_limit = config$short_count_rhat_limit,
        psm_rms_limit = config$short_psm_rms_limit,
        psm_mean_abs_limit = config$short_psm_mean_abs_limit
    )
    projection <- countdlm_road_calibration_projection(
        pilot_runtime, config, selected_rho, actual_workers
    )
    countdlm_road_atomic_write_csv(
        pilot_runtime, file.path(output_dir, "pilot-runtime.csv")
    )
    countdlm_road_atomic_write_csv(
        pair_diagnostics,
        file.path(output_dir, "pilot-chain-diagnostics.csv")
    )
    countdlm_road_atomic_write_csv(
        projection$variant_timing,
        file.path(output_dir, "pilot-variant-timing.csv")
    )
    countdlm_road_atomic_write_csv(
        projection$table,
        file.path(output_dir, "formal-runtime-projection.csv")
    )
    failed_tasks <- sum(pilot_runtime$status != "ok")
    warning_tasks <- sum(pilot_runtime$warning_count > 0L)
    all_short_screens_pass <- nrow(pair_diagnostics) ==
        nrow(config$variants) &&
        all(pair_diagnostics$short_screen_passed %in% TRUE)
    total_wall_seconds <- as.numeric(difftime(
        Sys.time(), started_at, units = "secs"
    ))
    result <- list(
        api_version = config$api_version,
        config = config,
        registration = registration,
        selected_rho = selected_rho,
        rho_runtime = rho_runtime,
        rho_pooled = rho_pooled,
        rho_by_task = rho_long,
        rho_pair_audit = rho_pair_audit,
        rho_variant_sensitivity = rho_variant_sensitivity,
        rho_family_sensitivity = rho_family_sensitivity,
        rho_family_regret_audit = rho_family_regret_audit,
        rho_start_sensitivity = rho_start_sensitivity,
        rho_half_sensitivity = rho_half_sensitivity,
        rho_wall_seconds = rho_wall_seconds,
        rho_resolved = TRUE,
        phase2_started = TRUE,
        pilot_runtime = pilot_runtime,
        pair_diagnostics = pair_diagnostics,
        projection = projection,
        pilot_wall_seconds = pilot_wall_seconds,
        total_wall_seconds_through_derived_tables = total_wall_seconds,
        truth_metrics_computed = FALSE,
        all_tasks_ok = failed_tasks == 0L,
        failed_tasks = failed_tasks,
        warning_tasks = warning_tasks,
        all_short_screens_pass = all_short_screens_pass,
        ready_for_longer_stability_pilot = failed_tasks == 0L &&
            warning_tasks == 0L && all_short_screens_pass,
        clean_commit_calibration_evidence = isTRUE(git$clean),
        eligible_for_formal_freeze = FALSE,
        formal_simulation_launched = FALSE
    )
    result_path <- file.path(output_dir, "road-calibration-result.rds")
    countdlm_road_atomic_save_rds(result, result_path)
    report <- c(
        "countDLM approved-road short calibration-pilot report",
        paste("API:", config$api_version),
        paste("Output:", output_dir),
        paste("Git HEAD:", git$head),
        paste("Git clean:", git$clean),
        paste("Context SHA-256:", observed_context_sha256),
        paste("Requested / actual workers:", config$workers, "/",
              actual_workers),
        paste("Reported physical cores:", reported_physical_cores),
        paste("Reported cores left unused:",
              reported_physical_cores - actual_workers),
        paste("Rho grid:", paste(config$rho_grid, collapse = ", ")),
        paste("Rho design:", config$rho_design),
        paste("Rho calibration branches:", nrow(config$rho_tasks)),
        paste("Rho calibration variants x starts x candidates:",
              nrow(config$variants), "x", config$rho_calibration_chains,
              "x", length(config$rho_grid)),
        paste("Rho transitions / settle / score per branch:",
              config$rho_calibration_iterations, "/",
              config$rho_calibration_settle, "/",
              config$rho_calibration_score),
        paste("Rho selection rule:", config$rho_selection_rule),
        paste("Rho resolution:", rho_pooled$resolution_status),
        paste("Rho phase wall time:",
              countdlm_road_format_duration(rho_wall_seconds)),
        paste("Rho aggregation:", config$rho_aggregation),
        paste("Rho movement scope:", config$rho_movement_scope),
        paste("Rho timing scope:", config$rho_timing_scope),
        paste("Potts beta status:", config$potts_beta_status),
        paste("Pooled truth-blinded selected rho:", selected_rho),
        paste("Fixed-rho pilot variants x chains:",
              nrow(config$variants), "x", config$pilot_chains),
        paste("Pilot transitions / burn:", config$pilot_iterations, "/",
              config$pilot_burn),
        paste("Pilot method-fit wall time:",
              countdlm_road_format_duration(pilot_wall_seconds)),
        paste("Total wall time through derived tables:",
              countdlm_road_format_duration(total_wall_seconds)),
        paste("Pilot task failures / warning tasks:", failed_tasks, "/",
              warning_tasks),
        paste("All short stability screens passed:",
              all_short_screens_pass),
        paste("12-hour computation gate:", projection$gate_hours, "hours"),
        paste("Largest projected internal-method replicate count:",
              projection$recommended_replicates),
        paste("Internal budget gate passed:",
              projection$internal_budget_gate_passed),
        "Short-chain R-hat and posterior-similarity values are diagnostic references, not formal convergence declarations.",
        "Passing this short screen permits only a longer stability pilot; it never authorizes formal simulation.",
        "No truth labels were stored or passed to a worker; no truth-based ARI or recovery metric was computed.",
        "Potts complete-density diagnostics may be compared between chains at one fixed beta, never across beta values.",
        paste("Excluded from projection:", projection$excluded_scope),
        "Formal simulation was not launched and still requires human review, checkpointing, and separate authorization.",
        "",
        "Pooled rho calibration:",
        utils::capture.output(print(rho_pooled$table, row.names = FALSE)),
        "",
        "Method-family rho regret audit:",
        utils::capture.output(print(
            rho_family_regret_audit, row.names = FALSE
        )),
        "",
        "Short two-chain diagnostic references:",
        utils::capture.output(print(pair_diagnostics, row.names = FALSE))
    )
    report_path <- file.path(output_dir, "road-calibration-report.txt")
    countdlm_road_atomic_write_lines(report, report_path)
    cat("\n", paste(report, collapse = "\n"), "\n", sep = "")
    run_stage <- "checksums-and-completion-marker"
    checksum_path <- countdlm_road_write_checksums(output_dir)
    completion_path <- file.path(output_dir, "RUN-COMPLETE.rds")
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            status = if (failed_tasks == 0L && warning_tasks == 0L &&
                all_short_screens_pass) {
                "complete"
            } else "complete-with-short-screen-flags",
            completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
            selected_rho = selected_rho,
            failed_tasks = failed_tasks,
            warning_tasks = warning_tasks,
            rho_resolved = TRUE,
            phase2_started = TRUE,
            all_short_screens_pass = all_short_screens_pass,
            ready_for_longer_stability_pilot = failed_tasks == 0L &&
                warning_tasks == 0L && all_short_screens_pass,
            truth_metrics_computed = FALSE,
            formal_simulation_launched = FALSE,
            checksums_cover_payload_before_this_marker = basename(
                checksum_path
            )
        ),
        completion_path
    )
    run_complete <- TRUE
    invisible(c(
        result,
        list(
            result_file = result_path,
            report_file = report_path,
            checksum_file = checksum_path,
            completion_file = completion_path
        )
    ))
}
