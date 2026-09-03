# Targeted truth-blinded follow-up for the three graph-gating variants that
# remained unstable in the 2026-09-03 longer stability pilot.  This file is
# intentionally additive: every source used by the parent run remains byte-for-
# byte unchanged and can therefore be checked against the returned archive.

countdlm_road_targeted_stability_api_version <-
    "countdlm-road-targeted-stability-2026-09-03-v1"
countdlm_road_targeted_parent_api_version <-
    "countdlm-road-longer-stability-2026-09-03-v1"
countdlm_road_targeted_parent_zip_sha256 <-
    "5142cb9dc9ede3f5e4606bd410e81ef5ad34004b300c669c9cdddd29da398f7b"
countdlm_road_targeted_parent_manifest_sha256 <-
    "bcf0a26a3a7577e22d0d6021abac163ea40841ae47e66192d4e80a7f0a0a0a7c"
countdlm_road_targeted_parent_result_sha256 <-
    "5d7c02ea03f94182af725cf25c7ef398b83f74777beb21600dfede666dcad40d"
countdlm_road_targeted_parent_completion_sha256 <-
    "e5aeb23dff565b4a1ac553cf7aeccd78b08d5b8df21845a391fa1a2b20c6ef9a"
countdlm_road_targeted_parent_registration_sha256 <-
    "5f3e582b8f54708cd95627d6b3202ff1adbd8d60c031ecd5d8675df01b005d55"
countdlm_road_targeted_parent_config_signature <-
    "094be797c50d9bdfae641d75243c5f1d395b546add1c21588013f36388aa98fc"
countdlm_road_targeted_methods <- c("GMDE-W", "GMDE-C", "Euc-MDE")
countdlm_road_targeted_parent_prefix_iterations <- 2000L
countdlm_road_targeted_task_seeds <- c(
    "GMDE-W-chain-1" = 2026092501L,
    "GMDE-C-chain-1" = 2026092602L,
    "Euc-MDE-chain-1" = 2026092703L,
    "GMDE-W-chain-2" = 2026093208L,
    "GMDE-C-chain-2" = 2026093309L,
    "Euc-MDE-chain-2" = 2026093410L
)
countdlm_road_targeted_parent_prefix_sha256 <- c(
    "GMDE-W-chain-1" =
        "d4d55d580e9683e674fbb39cad279a748b102db4e1fc636c44287f087174c4d7",
    "GMDE-C-chain-1" =
        "f231c988423128359ee0b8632e1d6cbb64b922323a00bd8c0d1e7dcca32db0c9",
    "Euc-MDE-chain-1" =
        "bf6ecd493c1c6af1371e38175b43de3302d81622f336a66b751acfbb76116fbc",
    "GMDE-W-chain-2" =
        "59b82504b19b326ccfe499a7fde5a2a6475d6e7ec3e8ed39cdc0706cd89b5a0c",
    "GMDE-C-chain-2" =
        "e1c8b50b491e9183f774ce357ba2072101764e959c623c1e4c156d6fef01aea9",
    "Euc-MDE-chain-2" =
        "ef683b4da62d185bd2691d46b96b64060f1dbaf7927d7e47c6461e1086f21727"
)

countdlm_road_targeted_variants <- function() {
    variants <- countdlm_road_calibration_variants(c(0.25, 0.5, 1))
    variants[match(
        countdlm_road_targeted_methods, variants$variant_id
    ), , drop = FALSE]
}

countdlm_road_targeted_tasks <- function() {
    tasks <- countdlm_road_calibration_tasks(
        countdlm_road_targeted_variants(), 2L
    )
    tasks$seed <- unname(countdlm_road_targeted_task_seeds[tasks$task_id])
    tasks$parent_prefix_sha256 <- unname(
        countdlm_road_targeted_parent_prefix_sha256[tasks$task_id]
    )
    tasks
}

countdlm_road_validate_targeted_parent <- function(
    result, completion, registration
) {
    expected_variants <- countdlm_road_calibration_variants(c(0.25, 0.5, 1))
    expected_tasks <- countdlm_road_calibration_tasks(expected_variants, 2L)
    required_result <- c(
        "api_version", "config", "registration", "selected_rho", "runtime",
        "pair_diagnostics", "truth_metrics_computed", "all_tasks_ok",
        "failed_tasks", "warning_tasks", "all_stability_screens_pass",
        "ready_for_formal_design_review", "eligible_for_formal_freeze",
        "formal_simulation_launched"
    )
    if (!is.list(result) || !all(required_result %in% names(result)) ||
        !identical(result$api_version,
                   countdlm_road_targeted_parent_api_version) ||
        !is.list(result$config) ||
        !identical(result$config$config_signature,
                   countdlm_road_targeted_parent_config_signature) ||
        !identical(result$config$sampler_version,
                   countdlm_gmde_sampler_version) ||
        !identical(result$config$context_sha256,
                   countdlm_road_context_sha256) ||
        !identical(result$config$data_sha256,
                   countdlm_road_stability_data_sha256) ||
        !identical(result$config$initialization_sha256,
                   countdlm_road_stability_initialization_sha256) ||
        !identical(result$config$pilot_iterations, 2000L) ||
        !identical(result$config$pilot_burn, 1000L) ||
        !identical(result$config$workers, 6L) ||
        !identical(as.numeric(result$selected_rho), 1) ||
        !isTRUE(result$all_tasks_ok) ||
        !identical(as.integer(result$failed_tasks), 0L) ||
        !identical(as.integer(result$warning_tasks), 0L) ||
        !identical(result$all_stability_screens_pass, FALSE) ||
        !identical(result$ready_for_formal_design_review, FALSE) ||
        !identical(result$eligible_for_formal_freeze, FALSE) ||
        !identical(result$truth_metrics_computed, FALSE) ||
        !identical(result$formal_simulation_launched, FALSE) ||
        !is.data.frame(result$runtime) || nrow(result$runtime) != 14L ||
        !identical(result$runtime$task_id, expected_tasks$task_id) ||
        any(result$runtime$status != "ok") ||
        any(result$runtime$warning_count != 0L) ||
        any(result$runtime$algorithm_exact != TRUE) ||
        any(result$runtime$fixed_rho_contract_passed != TRUE) ||
        any(result$runtime$ari_computed != FALSE) ||
        any(result$runtime$acc_computed != FALSE) ||
        !is.data.frame(result$pair_diagnostics) ||
        !identical(result$pair_diagnostics$variant_id,
                   expected_variants$variant_id)) {
        stop(
            "The returned longer-stability result does not match the reviewed ",
            "parent evidence.", call. = FALSE
        )
    }
    failed <- result$pair_diagnostics$variant_id[
        !result$pair_diagnostics$stability_screen_passed
    ]
    passed <- result$pair_diagnostics$variant_id[
        result$pair_diagnostics$stability_screen_passed
    ]
    if (!identical(failed, countdlm_road_targeted_methods) ||
        !identical(
            passed,
            c(
                "MoDE", "Potts-MDE-beta-0p25",
                "Potts-MDE-beta-0p50", "Potts-MDE-beta-1p00"
            )
        )) {
        stop(
            "The parent stability flags do not identify exactly the three ",
            "registered targeted variants.", call. = FALSE
        )
    }
    required_completion <- c(
        "api_version", "status", "selected_rho", "failed_tasks",
        "warning_tasks", "all_stability_screens_pass",
        "ready_for_formal_design_review", "truth_metrics_computed",
        "eligible_for_formal_freeze", "formal_simulation_launched",
        "checksums_cover_payload_before_this_marker"
    )
    if (!is.list(completion) ||
        !all(required_completion %in% names(completion)) ||
        !identical(completion$api_version,
                   countdlm_road_targeted_parent_api_version) ||
        !identical(completion$status,
                   "complete-with-stability-flags") ||
        !identical(as.numeric(completion$selected_rho), 1) ||
        !identical(as.integer(completion$failed_tasks), 0L) ||
        !identical(as.integer(completion$warning_tasks), 0L) ||
        !identical(completion$all_stability_screens_pass, FALSE) ||
        !identical(completion$ready_for_formal_design_review, FALSE) ||
        !identical(completion$truth_metrics_computed, FALSE) ||
        !identical(completion$eligible_for_formal_freeze, FALSE) ||
        !identical(completion$formal_simulation_launched, FALSE) ||
        !identical(completion$checksums_cover_payload_before_this_marker,
                   "CHECKSUMS.sha256")) {
        stop(
            "The returned longer-stability completion marker is invalid.",
            call. = FALSE
        )
    }
    if (!is.list(registration) ||
        !identical(registration, result$registration) ||
        !identical(registration$config$config_signature,
                   countdlm_road_targeted_parent_config_signature) ||
        !identical(registration$data_sha256,
                   countdlm_road_stability_data_sha256) ||
        !identical(unname(registration$initialization_sha256),
                   unname(countdlm_road_stability_initialization_sha256)) ||
        !is.character(registration$source_sha256) ||
        !length(registration$source_sha256) ||
        is.null(names(registration$source_sha256)) ||
        anyDuplicated(names(registration$source_sha256)) ||
        any(!grepl("^[[:xdigit:]]{64}$", registration$source_sha256))) {
        stop(
            "The returned longer-stability registration is invalid.",
            call. = FALSE
        )
    }
    required_environment <- c(
        "system", "R_version", "platform", "RNGkind",
        "BayesLogit_version", "digest_version", "actual_workers",
        "worker_thread_limit", "BLAS"
    )
    environment <- registration$execution_environment
    if (!is.list(environment) ||
        !all(required_environment %in% names(environment)) ||
        !is.list(environment$system) ||
        !all(c("sysname", "release", "machine") %in%
             names(environment$system)) ||
        !identical(environment$RNGkind, c(
            "Mersenne-Twister", "Inversion", "Rejection"
        )) ||
        !identical(as.integer(environment$actual_workers), 6L) ||
        !identical(as.integer(environment$worker_thread_limit), 1L) ||
        anyNA(unlist(environment[c(
            "R_version", "platform", "BayesLogit_version",
            "digest_version", "BLAS"
        )], use.names = FALSE))) {
        stop(
            "The returned longer-stability execution environment is invalid.",
            call. = FALSE
        )
    }
    invisible(TRUE)
}

countdlm_road_targeted_parent_evidence <- function(zip_file) {
    if (length(zip_file) != 1L || !is.character(zip_file) ||
        is.na(zip_file) || !nzchar(zip_file)) {
        stop("zip_file must be one explicit path.", call. = FALSE)
    }
    zip_file <- normalizePath(zip_file, winslash = "/", mustWork = TRUE)
    zip_sha256 <- countdlm_road_sha256(zip_file)
    if (!identical(zip_sha256,
                   countdlm_road_targeted_parent_zip_sha256)) {
        stop(
            "The longer-stability ZIP SHA-256 does not match the reviewed ",
            "archive.", call. = FALSE
        )
    }
    members <- c(
        manifest = "CHECKSUMS.sha256",
        result = "road-stability-result.rds",
        completion = "RUN-COMPLETE.rds",
        registration = "road-stability-registration.rds"
    )
    raw <- lapply(members, function(member) {
        countdlm_road_stability_zip_member_raw(zip_file, member)
    })
    observed <- vapply(
        raw, digest::digest, character(1),
        algo = "sha256", serialize = FALSE
    )
    expected <- c(
        manifest = countdlm_road_targeted_parent_manifest_sha256,
        result = countdlm_road_targeted_parent_result_sha256,
        completion = countdlm_road_targeted_parent_completion_sha256,
        registration = countdlm_road_targeted_parent_registration_sha256
    )
    if (!identical(observed, expected)) {
        stop(
            "One or more reviewed longer-stability members failed their ",
            "registered SHA-256 check.", call. = FALSE
        )
    }
    manifest_lines <- strsplit(
        rawToChar(raw$manifest), "\n", fixed = TRUE
    )[[1L]]
    manifest_expected <- c(
        paste0(expected[["result"]], "  road-stability-result.rds"),
        paste0(expected[["registration"]],
               "  road-stability-registration.rds")
    )
    if (any(vapply(
        manifest_expected,
        function(line) sum(manifest_lines == line) != 1L,
        logical(1)
    ))) {
        stop(
            "The reviewed parent members are not uniquely recorded in its ",
            "checksum manifest.", call. = FALSE
        )
    }
    result <- countdlm_road_stability_read_rds_raw(
        raw$result, members[["result"]]
    )
    completion <- countdlm_road_stability_read_rds_raw(
        raw$completion, members[["completion"]]
    )
    registration <- countdlm_road_stability_read_rds_raw(
        raw$registration, members[["registration"]]
    )
    countdlm_road_validate_targeted_parent(
        result, completion, registration
    )
    list(
        zip_file = zip_file,
        zip_sha256 = zip_sha256,
        manifest_sha256 = observed[["manifest"]],
        result_sha256 = observed[["result"]],
        completion_sha256 = observed[["completion"]],
        registration_sha256 = observed[["registration"]],
        config_signature = result$config$config_signature,
        source_sha256 = registration$source_sha256,
        execution_environment = registration$execution_environment,
        selected_rho = as.numeric(result$selected_rho),
        failed_variants = countdlm_road_targeted_methods,
        result = result,
        completion = completion,
        registration = registration
    )
}

countdlm_road_validate_targeted_stability_config <- function(config) {
    expected_variants <- countdlm_road_targeted_variants()
    expected_tasks <- countdlm_road_targeted_tasks()
    valid_signature <- inherits(
        config, "countdlm_road_targeted_stability_config"
    ) && identical(
        config$api_version, countdlm_road_targeted_stability_api_version
    ) && is.character(config$config_signature) &&
        length(config$config_signature) == 1L && identical(
            config$config_signature, countdlm_road_config_signature(config)
        )
    if (!isTRUE(valid_signature) ||
        !identical(config$sampler_version,
                   countdlm_gmde_sampler_version) ||
        !identical(config$parent_api_version,
                   countdlm_road_targeted_parent_api_version) ||
        !identical(config$parent_zip_sha256,
                   countdlm_road_targeted_parent_zip_sha256) ||
        !identical(config$parent_manifest_sha256,
                   countdlm_road_targeted_parent_manifest_sha256) ||
        !identical(config$parent_result_sha256,
                   countdlm_road_targeted_parent_result_sha256) ||
        !identical(config$parent_completion_sha256,
                   countdlm_road_targeted_parent_completion_sha256) ||
        !identical(config$parent_registration_sha256,
                   countdlm_road_targeted_parent_registration_sha256) ||
        !identical(config$parent_config_signature,
                   countdlm_road_targeted_parent_config_signature) ||
        !identical(config$context_sha256,
                   countdlm_road_context_sha256) ||
        !identical(config$data_sha256,
                   countdlm_road_stability_data_sha256) ||
        !identical(unname(config$initialization_sha256),
                   unname(countdlm_road_stability_initialization_sha256)) ||
        !identical(config$n, 100L) || !identical(config$TT, 168L) ||
        !identical(config$K_true, 5L) || !identical(config$K_fit, 10L) ||
        !identical(config$basis_m, 40L) ||
        !identical(config$methods, countdlm_road_targeted_methods) ||
        !identical(config$variants, expected_variants) ||
        !identical(config$tasks, expected_tasks) ||
        !identical(config$workers, 6L) ||
        !identical(config$reserved_reported_cores, 4L) ||
        !identical(config$worker_thread_limit, 1L) ||
        !identical(config$progress_poll_seconds, 0.5) ||
        !identical(config$selected_rho, 1) ||
        !identical(config$iterations, 8000L) ||
        !identical(config$burn, 4000L) ||
        !identical(config$chains, 2L) ||
        !identical(config$full_window, 4001:8000) ||
        !identical(config$late_window, 6001:8000) ||
        !identical(config$reference_window, 7001:8000) ||
        !identical(config$drift_first_window, 4001:6000) ||
        !identical(config$drift_second_window, 6001:8000) ||
        !identical(config$log_rhat_limit, 1.10) ||
        !identical(config$count_rhat_limit, 1.20) ||
        !identical(config$psm_rms_limit, 0.10) ||
        !identical(config$psm_mean_abs_limit, 0.05) ||
        !identical(config$parent_prefix_iterations,
                   countdlm_road_targeted_parent_prefix_iterations) ||
        !identical(config$parent_prefix_sha256,
                   countdlm_road_targeted_parent_prefix_sha256) ||
        !identical(config$state_G, diag(2)) ||
        !identical(config$state_W, diag(c(1e-6, 5e-7))) ||
        !identical(config$state_C0, diag(c(2, 1))) ||
        !identical(config$substantive_min, 5L) ||
        !identical(config$pg_backend, "devroye-exact") ||
        !isTRUE(config$algorithm_exact) ||
        !isTRUE(config$classification_only) ||
        !identical(config$truth_metrics_computed, FALSE) ||
        !identical(config$formal_results_authorized, FALSE) ||
        !is.character(config$execution_source_dir) ||
        !dir.exists(config$execution_source_dir) ||
        !is.character(config$source_sha256) ||
        !length(config$source_sha256) ||
        is.null(names(config$source_sha256)) ||
        anyDuplicated(names(config$source_sha256)) ||
        anyNA(config$source_sha256) ||
        any(!grepl("^[[:xdigit:]]{64}$", config$source_sha256))) {
        stop(
            "The targeted stability configuration is invalid or was modified ",
            "after construction.", call. = FALSE
        )
    }
    invisible(config)
}

#' Construct the approved-road targeted stability configuration
#'
#' This follow-up reruns only GMDE-W, GMDE-C, and Euc-MDE for 8,000
#' transitions with burn-in 4,000.  It uses the same blinded data, starts,
#' method seeds, fixed rho, and diagnostic limits as the reviewed parent.
#'
#' @param context_file Approved external D-017 road-context RDS.
#' @param parent_stability_zip Exact returned longer-stability ZIP.
#' @param output_dir Brand-new external output directory.
#' @param execution_source_dir Directory containing frozen `R/*.R` sources.
#' @return A signed, fixed targeted-stability configuration.
#' @export
countdlm_road_targeted_stability_config <- function(
    context_file, parent_stability_zip, output_dir,
    execution_source_dir = file.path(getwd(), "R")
) {
    execution_source_dir <- normalizePath(
        execution_source_dir, winslash = "/", mustWork = TRUE
    )
    source_files <- sort(list.files(
        execution_source_dir, pattern = "[.]R$", full.names = TRUE
    ))
    if (!length(source_files)) {
        stop("execution_source_dir contains no R source files.",
             call. = FALSE)
    }
    source_sha256 <- stats::setNames(
        vapply(source_files, countdlm_road_sha256, character(1)),
        basename(source_files)
    )
    config <- list(
        api_version = countdlm_road_targeted_stability_api_version,
        sampler_version = countdlm_gmde_sampler_version,
        scientific_role = paste(
            "truth-blinded targeted fixed-rho stability follow-up;",
            "not an inferential or formal simulation"
        ),
        context_file = normalizePath(
            context_file, winslash = "/", mustWork = TRUE
        ),
        context_sha256 = countdlm_road_context_sha256,
        parent_stability_zip = normalizePath(
            parent_stability_zip, winslash = "/", mustWork = TRUE
        ),
        parent_api_version = countdlm_road_targeted_parent_api_version,
        parent_zip_sha256 = countdlm_road_targeted_parent_zip_sha256,
        parent_manifest_sha256 =
            countdlm_road_targeted_parent_manifest_sha256,
        parent_result_sha256 = countdlm_road_targeted_parent_result_sha256,
        parent_completion_sha256 =
            countdlm_road_targeted_parent_completion_sha256,
        parent_registration_sha256 =
            countdlm_road_targeted_parent_registration_sha256,
        parent_config_signature =
            countdlm_road_targeted_parent_config_signature,
        output_dir = normalizePath(
            output_dir, winslash = "/", mustWork = FALSE
        ),
        execution_source_dir = execution_source_dir,
        source_sha256 = source_sha256,
        n = 100L,
        TT = 168L,
        K_true = 5L,
        K_fit = 10L,
        basis_m = 40L,
        methods = countdlm_road_targeted_methods,
        variants = countdlm_road_targeted_variants(),
        tasks = countdlm_road_targeted_tasks(),
        workers = 6L,
        reserved_reported_cores = 4L,
        worker_thread_limit = 1L,
        progress_poll_seconds = 0.5,
        selected_rho = 1,
        iterations = 8000L,
        burn = 4000L,
        chains = 2L,
        full_window = 4001:8000,
        late_window = 6001:8000,
        reference_window = 7001:8000,
        drift_first_window = 4001:6000,
        drift_second_window = 6001:8000,
        log_rhat_limit = 1.10,
        count_rhat_limit = 1.20,
        psm_rms_limit = 0.10,
        psm_mean_abs_limit = 0.05,
        parent_prefix_iterations =
            countdlm_road_targeted_parent_prefix_iterations,
        parent_prefix_sha256 =
            countdlm_road_targeted_parent_prefix_sha256,
        data_seed = 2026091101L,
        data_sha256 = countdlm_road_stability_data_sha256,
        initialization_seed_base = 2026091400L,
        initialization_sha256 =
            countdlm_road_stability_initialization_sha256,
        state_G = diag(2),
        state_W = diag(c(1e-6, 5e-7)),
        state_C0 = diag(c(2, 1)),
        substantive_min = 5L,
        pg_backend = "devroye-exact",
        algorithm_exact = TRUE,
        classification_only = TRUE,
        truth_metrics_computed = FALSE,
        formal_results_authorized = FALSE
    )
    config$config_signature <- countdlm_road_config_signature(config)
    config <- structure(
        config, class = "countdlm_road_targeted_stability_config"
    )
    countdlm_road_validate_targeted_stability_config(config)
    config
}

countdlm_road_targeted_prefix_fingerprint <- function(
    fit, iterations = countdlm_road_targeted_parent_prefix_iterations
) {
    iterations <- gmde_scalar_integer(
        iterations, "iterations", lower = 1L
    )
    required <- c(
        "Z", "size", "mean_lambda", "lambda", "loglik",
        "observed_loglik", "occupied_experts", "substantive_experts",
        "classifier_trace", "mean_assignment_probability", "state_accepted",
        "state_log_acceptance", "state_movement", "state_pg_shape_sum",
        "state_pg_shape_max", "state_rho", "ess_bracket_evaluations",
        "ess_likelihood_evaluations"
    )
    if (!is.list(fit) || !all(required %in% names(fit)) ||
        nrow(fit$Z) < iterations || nrow(fit$size) < iterations ||
        nrow(fit$mean_lambda) < iterations ||
        dim(fit$lambda)[[1L]] < iterations ||
        length(fit$loglik) < iterations ||
        length(fit$observed_loglik) < iterations ||
        length(fit$occupied_experts) < iterations ||
        length(fit$substantive_experts) < iterations ||
        nrow(fit$classifier_trace) < iterations ||
        nrow(fit$mean_assignment_probability) < iterations ||
        nrow(fit$state_accepted) < iterations ||
        nrow(fit$state_log_acceptance) < iterations ||
        nrow(fit$state_movement) < iterations ||
        nrow(fit$state_pg_shape_sum) < iterations ||
        nrow(fit$state_pg_shape_max) < iterations ||
        length(fit$state_rho) < iterations ||
        length(fit$ess_bracket_evaluations) < iterations ||
        length(fit$ess_likelihood_evaluations) < iterations) {
        stop("The fit cannot supply the registered parent-prefix payload.",
             call. = FALSE)
    }
    index <- seq_len(iterations)
    digest::digest(
        list(
            Z = fit$Z[index, , drop = FALSE],
            size = fit$size[index, , drop = FALSE],
            mean_lambda = fit$mean_lambda[index, , drop = FALSE],
            lambda = fit$lambda[index, , , drop = FALSE],
            loglik = fit$loglik[index],
            observed_loglik = fit$observed_loglik[index],
            occupied_experts = fit$occupied_experts[index],
            substantive_experts = fit$substantive_experts[index],
            classifier_trace =
                fit$classifier_trace[index, , drop = FALSE],
            mean_assignment_probability =
                fit$mean_assignment_probability[index, , drop = FALSE],
            state_accepted = fit$state_accepted[index, , drop = FALSE],
            state_log_acceptance =
                fit$state_log_acceptance[index, , drop = FALSE],
            state_movement = fit$state_movement[index, , drop = FALSE],
            state_pg_shape_sum =
                fit$state_pg_shape_sum[index, , drop = FALSE],
            state_pg_shape_max =
                fit$state_pg_shape_max[index, , drop = FALSE],
            state_rho = fit$state_rho[index],
            ess_bracket_evaluations =
                fit$ess_bracket_evaluations[index],
            ess_likelihood_evaluations =
                fit$ess_likelihood_evaluations[index]
        ),
        algo = "sha256", serialize = TRUE
    )
}

countdlm_road_assert_targeted_output <- function(
    output_dir, directories, config_signature, stage
) {
    required_directories <- unique(c(output_dir, directories))
    required_files <- file.path(
        output_dir,
        c("road-targeted-stability-registration.rds", "RUN-STARTED.rds")
    )
    missing <- c(
        required_directories[!dir.exists(required_directories)],
        required_files[!file.exists(required_files)]
    )
    if (length(missing)) {
        stop(
            "The registered targeted-stability output tree is unavailable ",
            "during ", stage, ". Missing: ", paste(missing, collapse = ", "),
            ". It will not be recreated, and this run is invalid.",
            call. = FALSE
        )
    }
    problem <- NULL
    registration <- tryCatch(
        readRDS(required_files[[1L]]),
        error = function(condition) {
            problem <<- conditionMessage(condition)
            NULL
        }
    )
    observed <- if (is.list(registration) &&
                    is.list(registration$config)) {
        registration$config$config_signature
    } else NULL
    if (!is.null(problem) || !identical(observed, config_signature)) {
        detail <- if (!is.null(problem)) problem else
            "registered configuration signature does not match"
        stop(
            "The targeted-stability output identity could not be verified ",
            "during ", stage, ": ", detail,
            ". It will not be recreated, and this run is invalid.",
            call. = FALSE
        )
    }
    invisible(TRUE)
}

countdlm_road_targeted_window_row <- function(
    fits, variant_id, method, window_name, index,
    log_rhat_limit, count_rhat_limit, psm_rms_limit,
    psm_mean_abs_limit, gate_window
) {
    density <- lapply(fits, function(fit) fit$observed_loglik[index])
    Kocc <- lapply(fits, function(fit) fit$occupied_experts[index])
    Ksub <- lapply(fits, function(fit) fit$substantive_experts[index])
    similarity <- lapply(fits, function(fit) {
        countdlm_road_posterior_similarity(
            fit$Z[index, , drop = FALSE]
        )
    })
    upper <- upper.tri(similarity[[1L]])
    difference <- similarity[[1L]][upper] - similarity[[2L]][upper]
    density_rhat <- countdlm_road_pilot_rhat(density)
    Kocc_rhat <- countdlm_road_pilot_rhat(Kocc)
    Ksub_rhat <- countdlm_road_pilot_rhat(Ksub)
    density_constant <- length(unique(unlist(
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
    passed <- is.finite(density_rhat) &&
        density_rhat <= log_rhat_limit && Kocc_ok && Ksub_ok &&
        psm_rms <= psm_rms_limit &&
        psm_mean_abs <= psm_mean_abs_limit
    data.frame(
        variant_id = variant_id,
        method = method,
        window = window_name,
        first_iteration = min(index),
        last_iteration = max(index),
        draws_per_chain = length(index),
        gate_window = isTRUE(gate_window),
        density_diagnostic = "observed-data log likelihood",
        log_density_rank_split_rhat = density_rhat,
        Kocc_rank_split_rhat = Kocc_rhat,
        Ksub_rank_split_rhat = Ksub_rhat,
        log_density_constant_across_draws = density_constant,
        Kocc_constant_across_draws = Kocc_constant,
        Ksub_constant_across_draws = Ksub_constant,
        Kocc_reference_status = if (Kocc_constant) {
            "constant-agreement-uninformative"
        } else if (Kocc_ok) "rhat-within-targeted-limit" else "rhat-flag",
        Ksub_reference_status = if (Ksub_constant) {
            "constant-agreement-uninformative"
        } else if (Ksub_ok) "rhat-within-targeted-limit" else "rhat-flag",
        psm_rms_between_chains = psm_rms,
        psm_mean_abs_between_chains = psm_mean_abs,
        window_screen_passed = isTRUE(passed),
        reference = if (isTRUE(passed)) {
            if (Kocc_constant || Ksub_constant) {
                "targeted-window-pass-constant-count-is-uninformative"
            } else "targeted-window-pass-not-formal-convergence"
        } else "targeted-window-flag-review-required",
        stringsAsFactors = FALSE
    )
}

countdlm_road_targeted_diagnostics <- function(
    runtime, tasks, output_dir, config
) {
    window_rows <- list()
    drift_rows <- list()
    prefix_rows <- list()
    variants <- unique(tasks$variant_id)
    for (variant_index in seq_along(variants)) {
        variant_id <- variants[[variant_index]]
        selected_tasks <- tasks[
            tasks$variant_id == variant_id, , drop = FALSE
        ]
        selected_runtime <- runtime[
            match(selected_tasks$task_id, runtime$task_id), , drop = FALSE
        ]
        if (nrow(selected_runtime) != 2L ||
            any(selected_runtime$status != "ok") ||
            any(is.na(selected_runtime$fit_file)) ||
            any(!file.exists(file.path(
                output_dir, selected_runtime$fit_file
            )))) {
            stop(
                "Targeted diagnostics require two retained successful fits ",
                "for ", variant_id, ".", call. = FALSE
            )
        }
        wrappers <- lapply(
            file.path(output_dir, selected_runtime$fit_file), readRDS
        )
        fits <- lapply(wrappers, `[[`, "fit")
        if (!all(vapply(
            fits, countdlm_road_truth_blinding_ok, logical(1),
            n_iter = config$iterations
        ))) {
            stop("A retained targeted fit failed truth blinding.",
                 call. = FALSE)
        }
        window_specification <- list(
            full_postburn = config$full_window,
            late_postburn = config$late_window,
            last_1000_reference = config$reference_window
        )
        window_gate <- c(TRUE, TRUE, FALSE)
        for (window_index in seq_along(window_specification)) {
            window_rows[[length(window_rows) + 1L]] <-
                countdlm_road_targeted_window_row(
                    fits = fits,
                    variant_id = variant_id,
                    method = selected_tasks$method[[1L]],
                    window_name = names(window_specification)[[window_index]],
                    index = window_specification[[window_index]],
                    log_rhat_limit = config$log_rhat_limit,
                    count_rhat_limit = config$count_rhat_limit,
                    psm_rms_limit = config$psm_rms_limit,
                    psm_mean_abs_limit = config$psm_mean_abs_limit,
                    gate_window = window_gate[[window_index]]
                )
        }
        for (chain_index in seq_len(2L)) {
            first_psm <- countdlm_road_posterior_similarity(
                fits[[chain_index]]$Z[
                    config$drift_first_window, , drop = FALSE
                ]
            )
            second_psm <- countdlm_road_posterior_similarity(
                fits[[chain_index]]$Z[
                    config$drift_second_window, , drop = FALSE
                ]
            )
            upper <- upper.tri(first_psm)
            difference <- first_psm[upper] - second_psm[upper]
            rms <- sqrt(mean(difference^2))
            mean_abs <- mean(abs(difference))
            drift_rows[[length(drift_rows) + 1L]] <- data.frame(
                variant_id = variant_id,
                method = selected_tasks$method[[1L]],
                chain_id = selected_tasks$chain_id[[chain_index]],
                first_window = "4001-6000",
                second_window = "6001-8000",
                psm_rms_within_chain = rms,
                psm_mean_abs_within_chain = mean_abs,
                drift_screen_passed =
                    rms <= config$psm_rms_limit &&
                    mean_abs <= config$psm_mean_abs_limit,
                reference = paste(
                    "within-chain posterior-similarity drift;",
                    "same fixed PSM limits as between-chain screen"
                ),
                stringsAsFactors = FALSE
            )
        }
        for (chain_index in seq_len(2L)) {
            wrapper <- wrappers[[chain_index]]
            observed <- countdlm_road_targeted_prefix_fingerprint(
                wrapper$fit, config$parent_prefix_iterations
            )
            expected <- selected_tasks$parent_prefix_sha256[[chain_index]]
            prefix_rows[[length(prefix_rows) + 1L]] <- data.frame(
                task_id = selected_tasks$task_id[[chain_index]],
                variant_id = variant_id,
                chain_id = selected_tasks$chain_id[[chain_index]],
                seed = selected_tasks$seed[[chain_index]],
                prefix_iterations = config$parent_prefix_iterations,
                expected_sha256 = expected,
                observed_sha256 = observed,
                prefix_reproduced = identical(observed, expected),
                stringsAsFactors = FALSE
            )
        }
        rm(wrappers, fits)
        invisible(gc(verbose = FALSE))
    }
    windows <- do.call(rbind, window_rows)
    drift <- do.call(rbind, drift_rows)
    prefix <- do.call(rbind, prefix_rows)
    rownames(windows) <- rownames(drift) <- rownames(prefix) <- NULL
    gate_windows <- windows[windows$gate_window, , drop = FALSE]
    gate <- do.call(rbind, lapply(variants, function(variant_id) {
        window_values <- gate_windows$window_screen_passed[
            gate_windows$variant_id == variant_id
        ]
        drift_values <- drift$drift_screen_passed[
            drift$variant_id == variant_id
        ]
        prefix_values <- prefix$prefix_reproduced[
            prefix$variant_id == variant_id
        ]
        data.frame(
            variant_id = variant_id,
            full_postburn_passed = isTRUE(window_values[[1L]]),
            late_postburn_passed = isTRUE(window_values[[2L]]),
            both_within_chain_drift_screens_passed =
                length(drift_values) == 2L && all(drift_values %in% TRUE),
            both_parent_prefixes_reproduced =
                length(prefix_values) == 2L && all(prefix_values %in% TRUE),
            targeted_gate_passed =
                length(window_values) == 2L && all(window_values %in% TRUE) &&
                length(drift_values) == 2L && all(drift_values %in% TRUE) &&
                length(prefix_values) == 2L && all(prefix_values %in% TRUE),
            stringsAsFactors = FALSE
        )
    }))
    rownames(gate) <- NULL
    list(windows = windows, drift = drift, prefix = prefix, gate = gate)
}

#' Run the approved-road targeted stability follow-up
#'
#' The runner verifies the exact returned longer-stability archive, confirms
#' byte identity for every source used by that parent run, regenerates the same
#' truth-blinded data and two starts, and reruns only GMDE-W, GMDE-C, and
#' Euc-MDE. It never computes truth-based metrics or launches a formal
#' simulation.
#'
#' @param config Output of `countdlm_road_targeted_stability_config()`.
#' @param repository_root Any path inside the Git repository whose provenance
#'   is recorded for this run.
#' @return Targeted diagnostics, timing, and immutable output paths.
#' @export
countdlm_road_targeted_stability_pilot <- function(
    config, repository_root
) {
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

    countdlm_road_validate_targeted_stability_config(config)
    if (!requireNamespace("BayesLogit", quietly = TRUE) ||
        !requireNamespace("digest", quietly = TRUE)) {
        stop(
            "The targeted stability follow-up requires BayesLogit and digest.",
            call. = FALSE
        )
    }
    parent <- countdlm_road_targeted_parent_evidence(
        config$parent_stability_zip
    )
    current_environment <- list(
        system = as.list(Sys.info()[c("sysname", "release", "machine")]),
        R_version = R.version.string,
        platform = R.version$platform,
        RNGkind = RNGkind(),
        BayesLogit_version = as.character(
            utils::packageVersion("BayesLogit")
        ),
        digest_version = as.character(utils::packageVersion("digest")),
        BLAS = unname(extSoftVersion()[["BLAS"]])
    )
    parent_environment <- parent$execution_environment
    parent_environment$system <- parent_environment$system[
        c("sysname", "release", "machine")
    ]
    environment_fields <- names(current_environment)
    environment_match <- vapply(
        environment_fields,
        function(field) identical(
            current_environment[[field]],
            parent_environment[[field]]
        ),
        logical(1)
    )
    thread_values <- Sys.getenv(
        c(
            "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
            "VECLIB_MAXIMUM_THREADS", "RCPP_PARALLEL_NUM_THREADS"
        ),
        unset = NA_character_
    )
    if (!all(environment_match) ||
        any(is.na(thread_values)) || any(thread_values != "1")) {
        mismatch <- environment_fields[!environment_match]
        if (any(is.na(thread_values)) || any(thread_values != "1")) {
            mismatch <- c(mismatch, "single-thread worker environment")
        }
        stop(
            "The current R execution environment differs from the reviewed ",
            "parent for: ", paste(mismatch, collapse = ", "),
            ". The targeted follow-up was not started; use the same RStudio ",
            "environment as the parent run.", call. = FALSE
        )
    }
    git <- countdlm_road_git_state(repository_root)
    source_files <- sort(list.files(
        config$execution_source_dir,
        pattern = "[.]R$", full.names = TRUE
    ))
    source_sha256 <- stats::setNames(
        vapply(source_files, countdlm_road_sha256, character(1)),
        basename(source_files)
    )
    if (!identical(source_sha256, config$source_sha256)) {
        stop(
            "The frozen execution sources changed after configuration; ",
            "the targeted follow-up was not started.", call. = FALSE
        )
    }
    parent_source_names <- names(parent$source_sha256)
    if (!length(parent_source_names) ||
        any(!parent_source_names %in% names(source_sha256)) ||
        !identical(
            unname(source_sha256[parent_source_names]),
            unname(parent$source_sha256)
        )) {
        stop(
            "One or more sources used by the reviewed parent run changed; ",
            "the targeted follow-up was not started.", call. = FALSE
        )
    }

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
            "Physical cores could not be verified; the six-worker targeted ",
            "follow-up was not started.", call. = FALSE
        )
    }
    if (actual_workers > max(
            1L,
            reported_physical_cores - config$reserved_reported_cores
        )) {
        stop(
            "The worker count must leave the registered number of reported ",
            "physical cores unused.", call. = FALSE
        )
    }

    context <- countdlm_road_load_approved_context(config$context_file)
    observed_context_sha256 <- context$sha256
    method_inputs <- countdlm_road_method_inputs(context)
    generated <- countdlm_road_generate_moderate(
        context, config$data_seed
    )
    blinded_data <- list(Y = generated$Y, Fmat = generated$Fmat)
    observed_data_sha256 <- digest::digest(
        blinded_data, algo = "sha256", serialize = TRUE
    )
    if (!identical(observed_data_sha256, config$data_sha256)) {
        stop(
            "The regenerated truth-blinded data do not match the reviewed ",
            "parent run.", call. = FALSE
        )
    }
    initializations <- countdlm_road_calibration_initializations(
        blinded_data$Y, config$K_fit,
        config$initialization_seed_base
    )
    observed_initialization_sha256 <- vapply(
        initializations, digest::digest, character(1),
        algo = "sha256", serialize = TRUE
    )
    if (!identical(
        unname(observed_initialization_sha256),
        unname(config$initialization_sha256)
    )) {
        stop(
            "The regenerated starting partitions do not match the reviewed ",
            "parent run.", call. = FALSE
        )
    }
    rm(generated, context)

    if (!dir.create(output_dir, recursive = FALSE, showWarnings = FALSE)) {
        stop(
            "Could not create the registered targeted-stability directory.",
            call. = FALSE
        )
    }
    run_complete <- FALSE
    run_stage <- "registration"
    on.exit({
        if (!run_complete) {
            incomplete <- file.path(output_dir, "RUN-INCOMPLETE.rds")
            if (!dir.exists(output_dir)) {
                message(
                    "The targeted-stability output directory is unavailable; ",
                    "RUN-INCOMPLETE could not be written: ", output_dir
                )
            } else if (!file.exists(incomplete)) {
                retention <- countdlm_road_try_retain_failure(
                    list(
                        api_version = config$api_version,
                        status = "failed-or-interrupted",
                        last_stage = run_stage,
                        recorded_at = format(
                            Sys.time(), tz = "UTC", usetz = TRUE
                        ),
                        formal_simulation_launched = FALSE,
                        temporary_files_retained = list.files(
                            output_dir, pattern = "[.]tmp-[0-9]+$",
                            recursive = TRUE, full.names = FALSE
                        )
                    ),
                    incomplete
                )
                if (!isTRUE(retention$saved)) {
                    message(
                        "RUN-INCOMPLETE could not be written: ",
                        retention$error
                    )
                }
            }
        }
    }, add = TRUE)

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
            digest_version = as.character(
                utils::packageVersion("digest")
            ),
            physical_cores_reported = reported_physical_cores,
            logical_cores_reported = parallel::detectCores(logical = TRUE),
            requested_workers = config$workers,
            actual_workers = actual_workers,
            reported_cores_left_unused =
                reported_physical_cores - actual_workers,
            worker_thread_limit = config$worker_thread_limit,
            BLAS = unname(extSoftVersion()[["BLAS"]])
        ),
        source_sha256 = source_sha256,
        parent_stability = list(
            api_version = config$parent_api_version,
            zip_file = parent$zip_file,
            zip_sha256 = parent$zip_sha256,
            manifest_sha256 = parent$manifest_sha256,
            result_sha256 = parent$result_sha256,
            completion_sha256 = parent$completion_sha256,
            registration_sha256 = parent$registration_sha256,
            config_signature = parent$config_signature,
            selected_rho = parent$selected_rho,
            failed_variants = parent$failed_variants
        ),
        context_sha256 = config$context_sha256,
        context_approval = "D-017",
        data_sha256 = observed_data_sha256,
        initialization_sha256 = observed_initialization_sha256,
        task_seeds = stats::setNames(
            config$tasks$seed, config$tasks$task_id
        ),
        parent_prefix_sha256 = config$parent_prefix_sha256,
        config = unclass(config),
        note = paste(
            "Truth-blinded targeted stability follow-up only; no truth-based",
            "recovery metric is computed and no formal simulation is launched."
        )
    )
    countdlm_road_atomic_save_rds(
        registration,
        file.path(
            output_dir, "road-targeted-stability-registration.rds"
        )
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
    chain_dir <- file.path(output_dir, "chains")
    diagnostic_dir <- file.path(output_dir, "diagnostics")
    failure_dir <- file.path(output_dir, "failures")
    directories <- c(chain_dir, diagnostic_dir, failure_dir)
    for (directory in directories) {
        if (!dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
            stop(
                "Could not create targeted-stability output directories.",
                call. = FALSE
            )
        }
    }
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            data_seed = config$data_seed,
            Y = blinded_data$Y,
            Fmat = blinded_data$Fmat,
            Y_Fmat_sha256 = observed_data_sha256,
            truth_fields_stored = FALSE,
            truth_metrics_computed = FALSE,
            initialization_seeds = attr(initializations, "seeds"),
            initialization_strategies = attr(initializations, "strategies"),
            adjusted_rand_between_starts = attr(
                initializations, "adjusted_rand_between_starts"
            ),
            initialization_sha256 = observed_initialization_sha256,
            Z_init = initializations
        ),
        file.path(
            output_dir, "road-targeted-stability-blinded-data.rds"
        )
    )
    countdlm_road_assert_targeted_output(
        output_dir, directories, config$config_signature,
        "pre-dispatch output verification"
    )
    parent$result <- parent$completion <- parent$registration <- NULL

    targeted_worker <- function(index) {
        countdlm_road_calibration_limit_threads()
        task <- config$tasks[index, , drop = FALSE]
        task_id <- task$task_id[[1L]]
        variant_id <- task$variant_id[[1L]]
        method <- task$method[[1L]]
        chain_id <- task$chain_id[[1L]]
        seed <- task$seed[[1L]]
        expected_prefix <- task$parent_prefix_sha256[[1L]]
        slug <- paste0(
            sprintf("%02d", index), "-",
            gsub("[^A-Za-z0-9]+", "-", task_id)
        )
        fit_path <- file.path(chain_dir, paste0(slug, "-fit.rds"))
        fit_relative_path <- file.path("chains", basename(fit_path))
        diagnostic_path <- file.path(
            diagnostic_dir, paste0(slug, "-diagnostic.rds")
        )
        failure_path <- file.path(
            failure_dir, paste0(slug, "-failure.rds")
        )
        task_started <- Sys.time()
        worker_stage <- "targeted-fit"
        warning_messages <- character()
        observed_prefix <- NA_character_
        withCallingHandlers(
            tryCatch({
                fit <- countdlm_road_fit_method(
                    method = method,
                    data = blinded_data,
                    method_inputs = method_inputs,
                    config = config,
                    seed = seed,
                    Z_init = initializations[[chain_id]],
                    potts_beta = NULL,
                    n_iter = config$iterations,
                    burn = config$burn,
                    rho = config$selected_rho
                )
                elapsed <- as.numeric(difftime(
                    Sys.time(), task_started, units = "secs"
                ))
                worker_stage <- "targeted-save-completed-fit"
                countdlm_road_atomic_save_rds(
                    list(
                        api_version = config$api_version,
                        parent_stability_result_sha256 =
                            config$parent_result_sha256,
                        task_id = task_id,
                        variant_id = variant_id,
                        method = method,
                        potts_beta = NA_real_,
                        chain_id = chain_id,
                        seed = seed,
                        selected_rho = config$selected_rho,
                        expected_parent_prefix_sha256 = expected_prefix,
                        warnings_before_fit_retention = warning_messages,
                        truth_metrics_computed = FALSE,
                        fit = fit
                    ),
                    fit_path
                )
                worker_stage <- "targeted-post-fit-contract"
                if (!countdlm_road_truth_blinding_ok(
                    fit, config$iterations
                )) {
                    stop(
                        "The fit failed the strict truth-blinding contract.",
                        call. = FALSE
                    )
                }
                worker_stage <- "targeted-fixed-rho-contract"
                fixed_rho_audit <- countdlm_road_fixed_rho_calibration(
                    fit = fit, settle = config$burn,
                    score = config$iterations - config$burn
                )
                worker_stage <- "targeted-parent-prefix-contract"
                observed_prefix <-
                    countdlm_road_targeted_prefix_fingerprint(
                        fit, config$parent_prefix_iterations
                    )
                if (!identical(observed_prefix, expected_prefix)) {
                    stop(
                        "The first 2,000 scientific transitions do not ",
                        "reproduce the reviewed parent chain.", call. = FALSE
                    )
                }
                worker_stage <- "targeted-summarize"
                summarized <- countdlm_road_summarize_fit(
                    fit = fit, task_id = task_id, method = method,
                    potts_beta = NA_real_, burn = config$burn,
                    elapsed = elapsed, warnings = warning_messages
                )
                summarized$summary$variant_id <- variant_id
                summarized$summary$chain_id <- chain_id
                summarized$summary$seed <- seed
                summarized$summary$sampler_version <-
                    fit$settings$sampler_version
                summarized$summary$fixed_rho_contract_passed <- TRUE
                summarized$summary$parent_prefix_reproduced <- TRUE
                summarized$summary$parent_prefix_sha256 <- observed_prefix
                summarized$summary$ari_computed <- any(!is.na(fit$ari))
                summarized$summary$acc_computed <- any(!is.na(fit$acc))
                summarized$summary$fit_file <- fit_relative_path
                summarized$summary$completed_fit_retained <- TRUE
                summarized$compact$variant_id <- variant_id
                summarized$compact$chain_id <- chain_id
                summarized$compact$seed <- seed
                summarized$compact$selected_rho <- config$selected_rho
                summarized$compact$api_version <- config$api_version
                summarized$compact$fixed_rho_contract <- fixed_rho_audit
                summarized$compact$parent_prefix_sha256 <- observed_prefix
                worker_stage <- "targeted-save-diagnostic"
                countdlm_road_atomic_save_rds(
                    summarized$compact, diagnostic_path
                )
                summarized$summary$warning_count <-
                    length(warning_messages)
                summarized$summary$warnings <- if (
                    length(warning_messages)
                ) paste(warning_messages, collapse = " | ") else NA_character_
                list(summary = summarized$summary)
            }, error = function(error) {
                elapsed <- as.numeric(difftime(
                    Sys.time(), task_started, units = "secs"
                ))
                summary <- countdlm_road_failure_summary(
                    task_id = task_id,
                    method = method,
                    potts_beta = NA_real_,
                    elapsed = elapsed,
                    config = list(
                        quick_iterations = config$iterations,
                        quick_burn = config$burn,
                        rho_timing = config$selected_rho
                    ),
                    error = conditionMessage(error),
                    stage = worker_stage,
                    warnings = warning_messages
                )
                summary$variant_id <- variant_id
                summary$chain_id <- chain_id
                summary$seed <- seed
                summary$sampler_version <- NA_character_
                summary$fixed_rho_contract_passed <- FALSE
                summary$parent_prefix_reproduced <- FALSE
                summary$parent_prefix_sha256 <- observed_prefix
                summary$ari_computed <- FALSE
                summary$acc_computed <- FALSE
                summary$fit_file <- if (file.exists(fit_path)) {
                    fit_relative_path
                } else NA_character_
                summary$completed_fit_retained <- file.exists(fit_path)
                retention <- countdlm_road_try_retain_failure(
                    list(
                        api_version = config$api_version,
                        phase = "fixed-rho-targeted-stability-follow-up",
                        task_id = task_id,
                        variant_id = variant_id,
                        method = method,
                        chain_id = chain_id,
                        seed = seed,
                        status = "error",
                        elapsed_seconds = elapsed,
                        stage = worker_stage,
                        error = conditionMessage(error),
                        call = paste(
                            deparse(conditionCall(error)), collapse = " "
                        ),
                        warnings = warning_messages,
                        expected_parent_prefix_sha256 = expected_prefix,
                        observed_parent_prefix_sha256 = observed_prefix,
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
                    failure_record_saved = retention$saved,
                    failure_record_error = retention$error
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
        "Targeted road stability follow-up: three variants x two starts\n",
        config$iterations, " transitions / ", config$burn,
        " burn; fixed reviewed rho = ", config$selected_rho, "; ",
        actual_workers, " single-thread worker(s), leaving approximately ",
        config$reserved_reported_cores, " reported cores unused\n",
        "The progress bar remains at 0/6 until the first complete chain ",
        "returns; this is expected.\n",
        "No truth metrics and no formal simulation.\n", sep = ""
    )
    run_stage <- "fixed-rho-targeted-stability-follow-up"
    countdlm_road_assert_targeted_output(
        output_dir, directories, config$config_signature,
        "before targeted dispatch"
    )
    pilot_started <- Sys.time()
    targeted_results <- countdlm_road_run_batches(
        config$tasks$task_id,
        targeted_worker,
        cores = actual_workers,
        poll_seconds = config$progress_poll_seconds
    )
    countdlm_road_stop_if_failure_unretained(
        targeted_results, "targeted stability"
    )
    for (index in seq_along(targeted_results)) {
        if (!inherits(
            targeted_results[[index]],
            "countdlm_road_scheduler_error"
        )) {
            next
        }
        error <- targeted_results[[index]]
        task <- config$tasks[index, , drop = FALSE]
        summary <- countdlm_road_failure_summary(
            task_id = task$task_id[[1L]],
            method = task$method[[1L]],
            potts_beta = NA_real_,
            elapsed = error$elapsed_seconds,
            config = list(
                quick_iterations = config$iterations,
                quick_burn = config$burn,
                rho_timing = config$selected_rho
            ),
            error = error$message,
            stage = if (identical(error$origin, "scheduler-collect")) {
                "parallel-scheduler-collect"
            } else "parallel-worker-uncaught"
        )
        summary$variant_id <- task$variant_id[[1L]]
        summary$chain_id <- task$chain_id[[1L]]
        summary$seed <- task$seed[[1L]]
        summary$sampler_version <- NA_character_
        summary$fixed_rho_contract_passed <- FALSE
        summary$parent_prefix_reproduced <- FALSE
        summary$parent_prefix_sha256 <- NA_character_
        summary$ari_computed <- FALSE
        summary$acc_computed <- FALSE
        summary$fit_file <- NA_character_
        summary$completed_fit_retained <- FALSE
        retention <- countdlm_road_try_retain_failure(
            list(
                api_version = config$api_version,
                phase = "fixed-rho-targeted-stability-follow-up",
                status = "error",
                retained = TRUE,
                error_origin = error$origin,
                original_error = error$message,
                summary = summary
            ),
            file.path(
                failure_dir,
                paste0(sprintf("%02d", index), "-scheduler-failure.rds")
            )
        )
        targeted_results[[index]] <- list(
            summary = summary,
            failure_record_saved = retention$saved,
            failure_record_error = retention$error
        )
    }
    countdlm_road_stop_if_failure_unretained(
        targeted_results, "targeted stability reconciliation"
    )
    countdlm_road_assert_targeted_output(
        output_dir, directories, config$config_signature,
        "after targeted dispatch"
    )
    runtime <- do.call(rbind, lapply(
        targeted_results, `[[`, "summary"
    ))
    runtime <- runtime[
        match(config$tasks$task_id, runtime$task_id), , drop = FALSE
    ]
    rownames(runtime) <- NULL
    if (any(runtime$status != "ok") ||
        any(runtime$warning_count > 0L)) {
        stop(
            "A targeted-stability task failed or warned. Retained fits and ",
            "failure diagnostics must be reviewed.", call. = FALSE
        )
    }
    pilot_wall_seconds <- as.numeric(difftime(
        Sys.time(), pilot_started, units = "secs"
    ))

    run_stage <- "targeted-derived-diagnostics"
    diagnostics <- countdlm_road_targeted_diagnostics(
        runtime = runtime,
        tasks = config$tasks,
        output_dir = output_dir,
        config = config
    )
    variant_timing <- do.call(rbind, lapply(
        config$methods, function(method) {
            values <- runtime$elapsed_seconds[runtime$method == method]
            data.frame(
                method = method,
                chains = length(values),
                minimum_seconds = min(values),
                median_seconds = stats::median(values),
                maximum_seconds = max(values),
                stringsAsFactors = FALSE
            )
        }
    ))
    rownames(variant_timing) <- NULL
    countdlm_road_atomic_write_csv(
        runtime,
        file.path(output_dir, "targeted-stability-runtime.csv")
    )
    countdlm_road_atomic_write_csv(
        diagnostics$windows,
        file.path(
            output_dir, "targeted-stability-window-diagnostics.csv"
        )
    )
    countdlm_road_atomic_write_csv(
        diagnostics$drift,
        file.path(output_dir, "targeted-stability-within-chain-drift.csv")
    )
    countdlm_road_atomic_write_csv(
        diagnostics$prefix,
        file.path(output_dir, "targeted-stability-prefix-audit.csv")
    )
    countdlm_road_atomic_write_csv(
        diagnostics$gate,
        file.path(output_dir, "targeted-stability-gate.csv")
    )
    countdlm_road_atomic_write_csv(
        variant_timing,
        file.path(output_dir, "targeted-stability-variant-timing.csv")
    )
    failed_tasks <- sum(runtime$status != "ok")
    warning_tasks <- sum(runtime$warning_count > 0L)
    all_prefixes_reproduced <- nrow(diagnostics$prefix) ==
        nrow(config$tasks) &&
        all(diagnostics$prefix$prefix_reproduced %in% TRUE)
    all_targeted_gates_pass <- nrow(diagnostics$gate) ==
        nrow(config$variants) &&
        all(diagnostics$gate$targeted_gate_passed %in% TRUE)
    total_wall_seconds <- as.numeric(difftime(
        Sys.time(), started_at, units = "secs"
    ))
    ready_for_formal_design_review <- failed_tasks == 0L &&
        warning_tasks == 0L && all_prefixes_reproduced &&
        all_targeted_gates_pass
    result <- list(
        api_version = config$api_version,
        config = config,
        registration = registration,
        parent_stability = registration$parent_stability,
        selected_rho = config$selected_rho,
        runtime = runtime,
        window_diagnostics = diagnostics$windows,
        within_chain_drift = diagnostics$drift,
        prefix_audit = diagnostics$prefix,
        targeted_gate = diagnostics$gate,
        variant_timing = variant_timing,
        pilot_wall_seconds = pilot_wall_seconds,
        total_wall_seconds_through_derived_tables = total_wall_seconds,
        truth_metrics_computed = FALSE,
        all_tasks_ok = failed_tasks == 0L,
        failed_tasks = failed_tasks,
        warning_tasks = warning_tasks,
        all_parent_prefixes_reproduced = all_prefixes_reproduced,
        all_targeted_gates_pass = all_targeted_gates_pass,
        ready_for_formal_design_review =
            ready_for_formal_design_review,
        clean_commit_stability_evidence = isTRUE(git$clean),
        eligible_for_formal_freeze = FALSE,
        formal_simulation_launched = FALSE,
        requires_human_review = TRUE
    )
    result_path <- file.path(
        output_dir, "road-targeted-stability-result.rds"
    )
    countdlm_road_atomic_save_rds(result, result_path)
    report <- c(
        "countDLM approved-road targeted stability report",
        paste("API:", config$api_version),
        paste("Output:", output_dir),
        paste("Git HEAD:", git$head),
        paste("Git clean:", git$clean),
        paste("Context SHA-256:", observed_context_sha256),
        paste("Parent stability ZIP SHA-256:", parent$zip_sha256),
        paste("Parent stability result SHA-256:", parent$result_sha256),
        paste("Reviewed fixed rho:", config$selected_rho),
        paste(
            "Requested / actual workers:", config$workers, "/",
            actual_workers
        ),
        paste("Reported physical cores:", reported_physical_cores),
        paste(
            "Reported cores left unused:",
            reported_physical_cores - actual_workers
        ),
        paste("Variants x chains:", nrow(config$variants), "x", 2L),
        paste("Transitions / burn:", config$iterations, "/", config$burn),
        paste(
            "Method-fit wall time:",
            countdlm_road_format_duration(pilot_wall_seconds)
        ),
        paste(
            "Total wall time through derived tables:",
            countdlm_road_format_duration(total_wall_seconds)
        ),
        paste(
            "Task failures / warning tasks:", failed_tasks, "/",
            warning_tasks
        ),
        paste("All parent prefixes reproduced:",
              all_prefixes_reproduced),
        paste("All targeted gates passed:", all_targeted_gates_pass),
        paste(
            "Ready only for formal-design review:",
            ready_for_formal_design_review
        ),
        "This result cannot authorize or launch a formal simulation.",
        "Any pass combines the four parent variants assessed at 2,000 transitions with three targeted variants assessed at 8,000; it is not a uniform-length seven-variant convergence proof.",
        "No truth labels were stored or passed to a worker; no truth-based recovery metric was computed.",
        "R-hat and posterior-similarity screens are diagnostics, not proof of convergence.",
        "The first 2,000 scientific transitions must exactly reproduce the reviewed parent chains.",
        "Formal simulation still requires human review and separate authorization.",
        "",
        "Targeted decision table:",
        utils::capture.output(print(diagnostics$gate, row.names = FALSE))
    )
    report_path <- file.path(
        output_dir, "road-targeted-stability-report.txt"
    )
    countdlm_road_atomic_write_lines(report, report_path)
    cat("\n", paste(report, collapse = "\n"), "\n", sep = "")
    run_stage <- "checksums-and-completion-marker"
    checksum_path <- countdlm_road_write_checksums(output_dir)
    completion_path <- file.path(output_dir, "RUN-COMPLETE.rds")
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            status = if (ready_for_formal_design_review) {
                "complete"
            } else "complete-with-targeted-stability-flags",
            completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
            selected_rho = config$selected_rho,
            failed_tasks = failed_tasks,
            warning_tasks = warning_tasks,
            all_parent_prefixes_reproduced = all_prefixes_reproduced,
            all_targeted_gates_pass = all_targeted_gates_pass,
            ready_for_formal_design_review =
                ready_for_formal_design_review,
            truth_metrics_computed = FALSE,
            eligible_for_formal_freeze = FALSE,
            formal_simulation_launched = FALSE,
            requires_human_review = TRUE,
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
