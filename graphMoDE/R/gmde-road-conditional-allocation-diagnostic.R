# Passive conditional-allocation mechanism diagnostic for the reviewed K = 10
# partition-mode experiment.  This truth-blinded development experiment
# reconstructs the twelve reviewed chains with the same data, bases, starts,
# seeds, rho, and run length.  It records deterministic summaries of the
# categorical allocation probabilities immediately before each draw.  The
# summaries must not consume random numbers or alter any scientific trace.

countdlm_road_conditional_allocation_api_version <-
    "countdlm-road-k10-conditional-allocation-diagnostic-2026-09-03-v1"
countdlm_road_conditional_allocation_parent_api_version <-
    "countdlm-road-partition-diagnostic-2026-09-03-v1"
countdlm_road_conditional_allocation_k6_api_version <-
    "countdlm-road-k6-diagnostic-2026-09-03-v1"
countdlm_road_conditional_allocation_k6_zip_sha256 <-
    "64948bff4758cc6915af35ddb984f81a3b0bbde3244c27f5160c350793f8e775"
countdlm_road_conditional_allocation_k6_manifest_sha256 <-
    "7da2476f7af945a4bd2470bd8e3a35e2dc1630f31806599e54b37ce55fc6b53c"
countdlm_road_conditional_allocation_k6_result_sha256 <-
    "c227cd25f0c80d35ed567f90e80d10cfa4e1202dd1f9789b0db8b6c7972bc4b3"
countdlm_road_conditional_allocation_k6_completion_sha256 <-
    "6cd611b893836ab089f90875a007c3689f0dbb67cad61ac26d9c4803ae16ea67"
countdlm_road_conditional_allocation_k6_registration_sha256 <-
    "a5ce589b577817621c66102823f7704d74d75e57779813a272b8345b950ca24a"
countdlm_road_conditional_allocation_k6_blinded_sha256 <-
    "7c786cf1634f0a26a6f54e711703940c103d0a5354cc993ed074b5909c5ad605"
countdlm_road_conditional_allocation_k6_config_signature <-
    "2a9c21807d3cc70e80462e780eed552c32d80b9dee792c87b6b77f595d8c6a70"

countdlm_road_conditional_allocation_scientific_fields <- c(
    "Z", "size", "mean_lambda", "lambda", "loglik", "observed_loglik",
    "occupied_experts", "substantive_experts", "classifier_trace",
    "mean_assignment_probability", "ari", "acc", "initialization",
    "state_accepted", "state_log_acceptance", "state_movement",
    "state_pg_shape_sum", "state_pg_shape_max", "state_rho",
    "ess_bracket_evaluations", "ess_likelihood_evaluations",
    "algorithm_exact"
)

countdlm_road_conditional_allocation_parent_trace_sha256 <- c(
    "GMDE-W-mode-A-seed-1" =
        "4faa4b050f7455ac78ecc3223708b1543e9efb5c520870af28904e3d88ac7c18",
    "GMDE-W-mode-B-seed-1" =
        "dd20f899af8ef2a33f3ad14cec16bdd30b6fc9fbc23da2054f84d0a5284c6a4d",
    "GMDE-C-mode-A-seed-1" =
        "25cce38fa6d14b66334655f276021d6e2b8bcdc0f2955421e3e67c2ed3536cec",
    "GMDE-C-mode-B-seed-1" =
        "b8545af756b3e5962397671e6bbc45752a0d509cb54dd20ff49b8b62297aa338",
    "Euc-MDE-mode-A-seed-1" =
        "bed1af21783595753ce7cca261a0e2b17c6947c9179fd7f448cb6796c96923f9",
    "Euc-MDE-mode-B-seed-1" =
        "37fc30f7d2549acfb8da728df2ea626d5b499297faed4719dab624c03928948d",
    "GMDE-W-mode-A-seed-2" =
        "4146dc66cf553170155dacd7f6660ecb16492426b2fd25674434aea17c7f1ced",
    "GMDE-W-mode-B-seed-2" =
        "460f8ddb7d74714c47da44a53f08643a2203dd054a8ff3d85f3d86894e092763",
    "GMDE-C-mode-A-seed-2" =
        "9898d29f163051b081854aec0dc785000499091d5f7de15ac7eda232ce314ce9",
    "GMDE-C-mode-B-seed-2" =
        "b7dad21ebab594e668606ac967fd30e6a9071878e7923be0100126587b725702",
    "Euc-MDE-mode-A-seed-2" =
        "4975b4ba1ce0a8dfc8fdb3968a12dac3ae8676d6758b2d05fbc02589704f4f5a",
    "Euc-MDE-mode-B-seed-2" =
        "f21a499efa90f27c19b1eec5cdf5eef00e3a41d2f2b66cb6ae81a33045f925a0"
)

countdlm_road_conditional_allocation_trace_hash <- function(fit) {
    fields <- countdlm_road_conditional_allocation_scientific_fields
    if (!is.list(fit) || !all(fields %in% names(fit))) {
        stop("The fit lacks a field in the frozen scientific trace.",
             call. = FALSE)
    }
    digest::digest(
        fit[fields], algo = "sha256", serialize = TRUE
    )
}

countdlm_road_conditional_allocation_k6_evidence <- function(zip_file) {
    zip_file <- normalizePath(zip_file, winslash = "/", mustWork = TRUE)
    if (!identical(
        countdlm_road_sha256(zip_file),
        countdlm_road_conditional_allocation_k6_zip_sha256
    )) {
        stop("The K = 6 motivation ZIP is not the reviewed archive.",
             call. = FALSE)
    }
    members <- c(
        manifest = "CHECKSUMS.sha256",
        result = "road-k6-diagnostic-result.rds",
        completion = "RUN-COMPLETE.rds",
        registration = "road-k6-diagnostic-registration.rds",
        blinded = "road-k6-diagnostic-blinded-input.rds"
    )
    raw_members <- lapply(members, function(member) {
        countdlm_road_stability_zip_member_raw(zip_file, member)
    })
    observed <- vapply(
        raw_members, digest::digest, character(1),
        algo = "sha256", serialize = FALSE
    )
    expected <- c(
        manifest = countdlm_road_conditional_allocation_k6_manifest_sha256,
        result = countdlm_road_conditional_allocation_k6_result_sha256,
        completion =
            countdlm_road_conditional_allocation_k6_completion_sha256,
        registration =
            countdlm_road_conditional_allocation_k6_registration_sha256,
        blinded = countdlm_road_conditional_allocation_k6_blinded_sha256
    )
    if (!identical(observed, expected)) {
        stop("A K = 6 motivation member failed its SHA-256 check.",
             call. = FALSE)
    }
    result <- countdlm_road_stability_read_rds_raw(
        raw_members$result, members[["result"]]
    )
    completion <- countdlm_road_stability_read_rds_raw(
        raw_members$completion, members[["completion"]]
    )
    registration <- countdlm_road_stability_read_rds_raw(
        raw_members$registration, members[["registration"]]
    )
    blinded <- countdlm_road_stability_read_rds_raw(
        raw_members$blinded, members[["blinded"]]
    )
    valid <- is.list(result) &&
        identical(result$api_version,
                  countdlm_road_conditional_allocation_k6_api_version) &&
        is.list(result$config) &&
        identical(result$config$config_signature,
                  countdlm_road_conditional_allocation_k6_config_signature) &&
        identical(result$config$K_fit, 6L) &&
        identical(result$config$parent_K_fit, 10L) &&
        identical(result$config$basis_m, 40L) &&
        identical(result$config$selected_rho, 1) &&
        identical(result$config$iterations, 3000L) &&
        identical(result$config$burn, 1000L) &&
        identical(result$config$methods, countdlm_road_partition_methods) &&
        isTRUE(result$all_tasks_ok) &&
        identical(as.integer(result$failed_tasks), 0L) &&
        identical(as.integer(result$warning_tasks), 0L) &&
        isTRUE(result$all_initializations_paired) &&
        isTRUE(result$all_terminal_states_ready) &&
        identical(result$all_methods_resolved_under_control, FALSE) &&
        identical(result$ready_for_formal_design_review, FALSE) &&
        identical(result$truth_metrics_computed, FALSE) &&
        identical(result$formal_simulation_launched, FALSE) &&
        is.data.frame(result$method_decision) &&
        identical(result$method_decision$method,
                  countdlm_road_partition_methods) &&
        all(result$method_decision$resolved_under_control == FALSE) &&
        all(result$method_decision$formal_run_authorized == FALSE) &&
        is.list(completion) &&
        identical(completion$api_version,
                  countdlm_road_conditional_allocation_k6_api_version) &&
        identical(completion$status,
                  "complete-with-k6-target-sensitivity-findings") &&
        identical(as.integer(completion$failed_tasks), 0L) &&
        identical(as.integer(completion$warning_tasks), 0L) &&
        identical(completion$all_methods_resolved_under_control, FALSE) &&
        identical(completion$ready_for_formal_design_review, FALSE) &&
        identical(completion$truth_metrics_computed, FALSE) &&
        identical(completion$formal_simulation_launched, FALSE) &&
        identical(completion$checksums_cover_payload_before_this_marker,
                  "CHECKSUMS.sha256") &&
        is.list(registration) && identical(registration, result$registration) &&
        identical(registration$config$config_signature,
                  countdlm_road_conditional_allocation_k6_config_signature) &&
        is.list(blinded) &&
        identical(blinded$api_version,
                  countdlm_road_conditional_allocation_k6_api_version) &&
        identical(blinded$Y_Fmat_sha256,
                  countdlm_road_stability_data_sha256) &&
        identical(blinded$K_fit, 6L) &&
        identical(blinded$parent_K_fit, 10L) &&
        identical(blinded$truth_fields_stored, FALSE) &&
        identical(blinded$truth_metrics_computed, FALSE)
    if (!isTRUE(valid)) {
        stop("The K = 6 motivation evidence failed its reviewed contract.",
             call. = FALSE)
    }
    if (!identical(
        result$parent_partition$zip_sha256,
        countdlm_road_k6_parent_zip_sha256
    )) {
        stop("The K = 6 result is not bound to the reviewed K = 10 parent.",
             call. = FALSE)
    }
    list(
        zip_file = zip_file,
        zip_sha256 =
            countdlm_road_conditional_allocation_k6_zip_sha256,
        manifest_sha256 = observed[["manifest"]],
        result_sha256 = observed[["result"]],
        completion_sha256 = observed[["completion"]],
        registration_sha256 = observed[["registration"]],
        blinded_sha256 = observed[["blinded"]],
        config_signature = result$config$config_signature,
        method_decision = result$method_decision
    )
}

countdlm_road_conditional_allocation_parent_trace_audit <- function(parent) {
    tasks <- countdlm_road_partition_diagnostic_tasks()
    expected <- countdlm_road_conditional_allocation_parent_trace_sha256[
        tasks$task_id
    ]
    if (anyNA(expected) || !identical(names(expected), tasks$task_id)) {
        stop("The frozen parent scientific fingerprints are incomplete.",
             call. = FALSE)
    }
    rows <- vector("list", nrow(tasks))
    runtime_index <- match(tasks$task_id, parent$result$runtime$task_id)
    if (anyNA(runtime_index)) {
        stop("The K = 10 parent runtime is missing a diagnostic task.",
             call. = FALSE)
    }
    for (index in seq_len(nrow(tasks))) {
        parent_row <- parent$result$runtime[
            runtime_index[[index]], , drop = FALSE
        ]
        member <- basename(parent_row$fit_file[[1L]])
        wrapper <- countdlm_road_stability_read_rds_raw(
            countdlm_road_stability_zip_member_raw(parent$zip_file, member),
            member
        )
        if (!is.list(wrapper) ||
            !identical(wrapper$api_version,
                       countdlm_road_conditional_allocation_parent_api_version) ||
            !identical(wrapper$task_id, tasks$task_id[[index]]) ||
            !countdlm_road_truth_blinding_ok(wrapper$fit, 3000L)) {
            stop("A K = 10 parent fit failed its wrapper contract.",
                 call. = FALSE)
        }
        observed <- countdlm_road_conditional_allocation_trace_hash(
            wrapper$fit
        )
        rows[[index]] <- data.frame(
            task_id = tasks$task_id[[index]],
            parent_fit_member = member,
            expected_scientific_trace_sha256 = unname(expected[[index]]),
            observed_scientific_trace_sha256 = observed,
            parent_trace_verified = identical(
                observed, unname(expected[[index]])
            ),
            stringsAsFactors = FALSE
        )
        rm(wrapper)
        invisible(gc(verbose = FALSE))
    }
    value <- do.call(rbind, rows)
    rownames(value) <- NULL
    if (!all(value$parent_trace_verified)) {
        stop("A frozen K = 10 parent scientific fingerprint failed.",
             call. = FALSE)
    }
    value
}

#' Construct the passive K = 10 conditional-allocation diagnostic
#'
#' The configuration reconstructs the reviewed twelve-chain K = 10
#' partition-mode experiment and adds only deterministic, pre-draw summaries
#' of its allocation probabilities.  The reviewed K = 6 result is accepted
#' only as motivation and never supplies the target, data, or chain state.
#'
#' @param context_file Approved external D-017 road-context RDS.
#' @param parent_partition_zip Exact returned K = 10 partition diagnostic ZIP.
#' @param k6_diagnostic_zip Exact returned K = 6 diagnostic ZIP used only as
#'   motivation for this mechanism check.
#' @param output_dir Brand-new external output directory.
#' @param execution_source_dir Directory containing frozen `R/*.R` sources.
#' @return A signed, fixed diagnostic configuration.
#' @export
countdlm_road_conditional_allocation_config <- function(
    context_file, parent_partition_zip, k6_diagnostic_zip, output_dir,
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
    tasks <- countdlm_road_partition_diagnostic_tasks()
    expected_trace <-
        countdlm_road_conditional_allocation_parent_trace_sha256[
            tasks$task_id
        ]
    config <- list(
        api_version = countdlm_road_conditional_allocation_api_version,
        sampler_version = countdlm_gmde_sampler_version,
        scientific_role = paste(
            "truth-blinded passive K=10 conditional-allocation mechanism",
            "diagnostic; not tuning, inference, or formal simulation"
        ),
        context_file = normalizePath(
            context_file, winslash = "/", mustWork = TRUE
        ),
        context_sha256 = countdlm_road_context_sha256,
        parent_partition_zip = normalizePath(
            parent_partition_zip, winslash = "/", mustWork = TRUE
        ),
        parent_api_version =
            countdlm_road_conditional_allocation_parent_api_version,
        parent_zip_sha256 = countdlm_road_k6_parent_zip_sha256,
        parent_manifest_sha256 = countdlm_road_k6_parent_manifest_sha256,
        parent_result_sha256 = countdlm_road_k6_parent_result_sha256,
        parent_completion_sha256 =
            countdlm_road_k6_parent_completion_sha256,
        parent_registration_sha256 =
            countdlm_road_k6_parent_registration_sha256,
        parent_blinded_sha256 = countdlm_road_k6_parent_blinded_sha256,
        parent_config_signature =
            countdlm_road_k6_parent_config_signature,
        k6_diagnostic_zip = normalizePath(
            k6_diagnostic_zip, winslash = "/", mustWork = TRUE
        ),
        k6_api_version =
            countdlm_road_conditional_allocation_k6_api_version,
        k6_zip_sha256 =
            countdlm_road_conditional_allocation_k6_zip_sha256,
        k6_manifest_sha256 =
            countdlm_road_conditional_allocation_k6_manifest_sha256,
        k6_result_sha256 =
            countdlm_road_conditional_allocation_k6_result_sha256,
        k6_completion_sha256 =
            countdlm_road_conditional_allocation_k6_completion_sha256,
        k6_registration_sha256 =
            countdlm_road_conditional_allocation_k6_registration_sha256,
        k6_blinded_sha256 =
            countdlm_road_conditional_allocation_k6_blinded_sha256,
        k6_config_signature =
            countdlm_road_conditional_allocation_k6_config_signature,
        output_dir = normalizePath(
            output_dir, winslash = "/", mustWork = FALSE
        ),
        execution_source_dir = execution_source_dir,
        source_sha256 = source_sha256,
        n = 100L,
        TT = 168L,
        K_fit = 10L,
        basis_m = 40L,
        methods = countdlm_road_partition_methods,
        mode_specification = countdlm_road_partition_mode_specification(),
        tasks = tasks,
        workers = 6L,
        reserved_reported_cores = 4L,
        worker_thread_limit = 1L,
        progress_poll_seconds = 0.5,
        selected_rho = 1,
        iterations = 3000L,
        burn = 1000L,
        blocks = list(
            settling = 1:1000,
            middle = 1001:2000,
            late = 2001:3000
        ),
        analysis_window = 1001:3000,
        data_sha256 = countdlm_road_stability_data_sha256,
        basis_sha256 = countdlm_road_partition_basis_sha256,
        neutral_theta_sha256 =
            countdlm_road_partition_neutral_theta_sha256,
        neutral_gamma_sha256 =
            countdlm_road_partition_neutral_gamma_sha256,
        neutral_joint_sha256 =
            countdlm_road_partition_neutral_joint_sha256,
        expected_parent_scientific_trace_sha256 = expected_trace,
        scientific_trace_fields =
            countdlm_road_conditional_allocation_scientific_fields,
        state_G = diag(2),
        state_W = diag(c(1e-6, 5e-7)),
        state_C0 = diag(c(2, 1)),
        substantive_min = 5L,
        pg_backend = "devroye-exact",
        algorithm_exact = TRUE,
        classification_only = TRUE,
        store_allocation_conditionals = TRUE,
        target_changed_from_parent = FALSE,
        calibration_z_limit = 4,
        observed_expected_ratio_limits = c(0.5, 2),
        expected_event_minimum = 10,
        conditioning_range_limit = 0.5,
        conditioning_persistence_ratio = 0.75,
        occupancy_activity_breaks_per_1000 = c(1, 10, 50),
        substantive_churn_limit_per_1000 = 50,
        method_occupancy_consensus_min = 3L,
        method_substantive_churn_min = 2L,
        truth_metrics_computed = FALSE,
        formal_results_authorized = FALSE
    )
    config$config_signature <- countdlm_road_config_signature(config)
    config <- structure(
        config,
        class = "countdlm_road_conditional_allocation_config"
    )
    countdlm_road_validate_conditional_allocation_config(config)
    config
}

countdlm_road_validate_conditional_allocation_config <- function(config) {
    tasks <- countdlm_road_partition_diagnostic_tasks()
    expected_trace <-
        countdlm_road_conditional_allocation_parent_trace_sha256[
            tasks$task_id
        ]
    valid_signature <- inherits(
        config, "countdlm_road_conditional_allocation_config"
    ) && identical(
        config$api_version,
        countdlm_road_conditional_allocation_api_version
    ) && is.character(config$config_signature) &&
        length(config$config_signature) == 1L && identical(
            config$config_signature,
            countdlm_road_config_signature(config)
        )
    if (!isTRUE(valid_signature) ||
        !identical(config$sampler_version, countdlm_gmde_sampler_version) ||
        !identical(config$parent_api_version,
                   countdlm_road_conditional_allocation_parent_api_version) ||
        !identical(config$parent_zip_sha256,
                   countdlm_road_k6_parent_zip_sha256) ||
        !identical(config$parent_manifest_sha256,
                   countdlm_road_k6_parent_manifest_sha256) ||
        !identical(config$parent_result_sha256,
                   countdlm_road_k6_parent_result_sha256) ||
        !identical(config$parent_completion_sha256,
                   countdlm_road_k6_parent_completion_sha256) ||
        !identical(config$parent_registration_sha256,
                   countdlm_road_k6_parent_registration_sha256) ||
        !identical(config$parent_blinded_sha256,
                   countdlm_road_k6_parent_blinded_sha256) ||
        !identical(config$parent_config_signature,
                   countdlm_road_k6_parent_config_signature) ||
        !identical(config$k6_api_version,
                   countdlm_road_conditional_allocation_k6_api_version) ||
        !identical(config$k6_zip_sha256,
                   countdlm_road_conditional_allocation_k6_zip_sha256) ||
        !identical(config$k6_manifest_sha256,
                   countdlm_road_conditional_allocation_k6_manifest_sha256) ||
        !identical(config$k6_result_sha256,
                   countdlm_road_conditional_allocation_k6_result_sha256) ||
        !identical(config$k6_completion_sha256,
                   countdlm_road_conditional_allocation_k6_completion_sha256) ||
        !identical(config$k6_registration_sha256,
                   countdlm_road_conditional_allocation_k6_registration_sha256) ||
        !identical(config$k6_blinded_sha256,
                   countdlm_road_conditional_allocation_k6_blinded_sha256) ||
        !identical(config$k6_config_signature,
                   countdlm_road_conditional_allocation_k6_config_signature) ||
        !identical(config$context_sha256, countdlm_road_context_sha256) ||
        !identical(config$data_sha256,
                   countdlm_road_stability_data_sha256) ||
        !identical(config$n, 100L) || !identical(config$TT, 168L) ||
        !identical(config$K_fit, 10L) ||
        !identical(config$basis_m, 40L) ||
        !identical(config$methods, countdlm_road_partition_methods) ||
        !identical(config$mode_specification,
                   countdlm_road_partition_mode_specification()) ||
        !identical(config$tasks, tasks) ||
        !identical(config$workers, 6L) ||
        !identical(config$reserved_reported_cores, 4L) ||
        !identical(config$worker_thread_limit, 1L) ||
        !identical(config$progress_poll_seconds, 0.5) ||
        !identical(config$selected_rho, 1) ||
        !identical(config$iterations, 3000L) ||
        !identical(config$burn, 1000L) ||
        !identical(config$blocks, list(
            settling = 1:1000,
            middle = 1001:2000,
            late = 2001:3000
        )) ||
        !identical(config$analysis_window, 1001:3000) ||
        !identical(config$basis_sha256,
                   countdlm_road_partition_basis_sha256) ||
        !identical(config$neutral_theta_sha256,
                   countdlm_road_partition_neutral_theta_sha256) ||
        !identical(config$neutral_gamma_sha256,
                   countdlm_road_partition_neutral_gamma_sha256) ||
        !identical(config$neutral_joint_sha256,
                   countdlm_road_partition_neutral_joint_sha256) ||
        !identical(config$expected_parent_scientific_trace_sha256,
                   expected_trace) ||
        !identical(config$scientific_trace_fields,
                   countdlm_road_conditional_allocation_scientific_fields) ||
        !identical(config$state_G, diag(2)) ||
        !identical(config$state_W, diag(c(1e-6, 5e-7))) ||
        !identical(config$state_C0, diag(c(2, 1))) ||
        !identical(config$substantive_min, 5L) ||
        !identical(config$pg_backend, "devroye-exact") ||
        !isTRUE(config$algorithm_exact) ||
        !isTRUE(config$classification_only) ||
        !isTRUE(config$store_allocation_conditionals) ||
        !identical(config$target_changed_from_parent, FALSE) ||
        !identical(config$calibration_z_limit, 4) ||
        !identical(config$observed_expected_ratio_limits, c(0.5, 2)) ||
        !identical(config$expected_event_minimum, 10) ||
        !identical(config$conditioning_range_limit, 0.5) ||
        !identical(config$conditioning_persistence_ratio, 0.75) ||
        !identical(config$occupancy_activity_breaks_per_1000,
                   c(1, 10, 50)) ||
        !identical(config$substantive_churn_limit_per_1000, 50) ||
        !identical(config$method_occupancy_consensus_min, 3L) ||
        !identical(config$method_substantive_churn_min, 2L) ||
        !identical(config$truth_metrics_computed, FALSE) ||
        !identical(config$formal_results_authorized, FALSE) ||
        !is.character(config$execution_source_dir) ||
        !dir.exists(config$execution_source_dir) ||
        !is.character(config$source_sha256) ||
        !length(config$source_sha256) || is.null(names(config$source_sha256)) ||
        anyDuplicated(names(config$source_sha256)) ||
        anyNA(config$source_sha256) ||
        any(!grepl("^[[:xdigit:]]{64}$", config$source_sha256))) {
        stop(
            paste(
                "The passive conditional-allocation diagnostic",
                "configuration is invalid or modified."
            ),
            call. = FALSE
        )
    }
    invisible(config)
}

countdlm_road_conditional_allocation_activity_label <- function(
    events_per_1000, breaks
) {
    if (!is.finite(events_per_1000) || events_per_1000 < 0 ||
        !identical(as.numeric(breaks), c(1, 10, 50))) {
        stop("Conditional-activity classification inputs are invalid.",
             call. = FALSE)
    }
    if (events_per_1000 < breaks[[1L]]) {
        "near-deterministic"
    } else if (events_per_1000 < breaks[[2L]]) {
        "sparse"
    } else if (events_per_1000 < breaks[[3L]]) {
        "active"
    } else {
        "high"
    }
}

countdlm_road_conditional_allocation_z <- function(
    observed, expected, variance
) {
    observed <- as.numeric(observed)
    expected <- as.numeric(expected)
    variance <- as.numeric(variance)
    if (!identical(length(observed), length(expected)) ||
        !identical(length(observed), length(variance)) ||
        any(!is.finite(c(observed, expected, variance))) ||
        any(variance < 0)) {
        stop("Conditional calibration inputs are invalid.", call. = FALSE)
    }
    variance_sum <- sum(variance)
    residual_sum <- sum(observed - expected)
    if (variance_sum == 0) {
        if (residual_sum == 0) return(NA_real_)
        return(sign(residual_sum) * Inf)
    }
    residual_sum / sqrt(variance_sum)
}

countdlm_road_validate_conditional_allocation_fit <- function(
    fit, starting_partition, config, task_id, method
) {
    required_fit <- c(
        "occupied_births", "occupied_deaths",
        "substantive_upcrossings", "substantive_downcrossings",
        "allocation_conditionals", "sampler_terminal_state"
    )
    if (!is.list(fit) || !all(required_fit %in% names(fit)) ||
        !countdlm_road_truth_blinding_ok(fit, config$iterations) ||
        !is.list(fit$settings) ||
        !isTRUE(fit$settings$allocation_conditionals_stored) ||
        !isTRUE(fit$settings$sampler_terminal_state_stored)) {
        stop("A reconstructed fit lacks the passive diagnostic contract.",
             call. = FALSE)
    }
    conditional <- fit$allocation_conditionals
    matrix_fields <- c(
        "size_before", "size_after", "expected_size_after",
        "log_probability_empty_after", "probability_empty_after",
        "probability_substantive_after",
        "probability_nonsubstantive_after", "birth_probability",
        "death_probability", "upcross_probability",
        "downcross_probability"
    )
    vector_fields <- c(
        "expected_Kocc_after", "expected_Ksub_after",
        "expected_births", "expected_deaths",
        "expected_upcrossings", "expected_downcrossings",
        "expected_node_switches", "observed_node_switches",
        "Kocc_after_variance", "birth_variance", "death_variance",
        "node_switch_variance", "maximum_row_sum_error",
        "maximum_dp_mass_error", "maximum_zero_probability_error",
        "rng_unchanged"
    )
    if (!is.list(conditional) ||
        !all(c(matrix_fields, vector_fields) %in% names(conditional)) ||
        any(vapply(matrix_fields, function(field) {
            !is.matrix(conditional[[field]]) ||
                !identical(dim(conditional[[field]]),
                           c(config$iterations, config$K_fit))
        }, logical(1))) ||
        any(vapply(vector_fields, function(field) {
            length(conditional[[field]]) != config$iterations
        }, logical(1)))) {
        stop("Conditional-allocation storage has an invalid schema.",
             call. = FALSE)
    }
    finite_matrix_fields <- setdiff(
        matrix_fields, "log_probability_empty_after"
    )
    if (any(vapply(finite_matrix_fields, function(field) {
        any(!is.finite(conditional[[field]]))
    }, logical(1))) ||
        any(is.na(conditional$log_probability_empty_after)) ||
        any(conditional$log_probability_empty_after > 0) ||
        any(vapply(setdiff(vector_fields, "rng_unchanged"), function(field) {
            any(!is.finite(conditional[[field]]))
        }, logical(1))) ||
        !is.logical(conditional$rng_unchanged) ||
        !all(conditional$rng_unchanged)) {
        stop("Conditional-allocation diagnostics contain invalid values.",
             call. = FALSE)
    }
    probability_fields <- c(
        "probability_empty_after", "probability_substantive_after",
        "probability_nonsubstantive_after", "birth_probability",
        "death_probability", "upcross_probability",
        "downcross_probability"
    )
    if (any(vapply(probability_fields, function(field) {
        any(conditional[[field]] < -1e-12 |
            conditional[[field]] > 1 + 1e-12)
    }, logical(1))) ||
        any(conditional$size_before < 0 |
            conditional$size_before != round(conditional$size_before)) ||
        any(conditional$size_after < 0 |
            conditional$size_after != round(conditional$size_after)) ||
        any(abs(rowSums(conditional$size_before) - config$n) > 0) ||
        any(abs(rowSums(conditional$size_after) - config$n) > 0) ||
        any(abs(rowSums(conditional$expected_size_after) - config$n) >
            1e-10)) {
        stop("Conditional component values failed bounds or mass checks.",
             call. = FALSE)
    }
    tolerance <- 1e-10
    total_identity <- c(
        Kocc = max(abs(
            conditional$expected_Kocc_after -
                rowSums(1 - conditional$probability_empty_after)
        )),
        Ksub = max(abs(
            conditional$expected_Ksub_after -
                rowSums(conditional$probability_substantive_after)
        )),
        births = max(abs(
            conditional$expected_births -
                rowSums(conditional$birth_probability)
        )),
        deaths = max(abs(
            conditional$expected_deaths -
                rowSums(conditional$death_probability)
        )),
        upcrossings = max(abs(
            conditional$expected_upcrossings -
                rowSums(conditional$upcross_probability)
        )),
        downcrossings = max(abs(
            conditional$expected_downcrossings -
                rowSums(conditional$downcross_probability)
        ))
    )
    size_before <- conditional$size_before
    size_after <- conditional$size_after
    observed_Kocc <- rowSums(size_after > 0L)
    observed_Ksub <- rowSums(size_after >= config$substantive_min)
    observed_births <- rowSums(size_before == 0L & size_after > 0L)
    observed_deaths <- rowSums(size_before > 0L & size_after == 0L)
    observed_upcrossings <- rowSums(
        size_before < config$substantive_min &
            size_after >= config$substantive_min
    )
    observed_downcrossings <- rowSums(
        size_before >= config$substantive_min &
            size_after < config$substantive_min
    )
    gross_identity <- identical(
        as.integer(observed_births), as.integer(fit$occupied_births)
    ) && identical(
        as.integer(observed_deaths), as.integer(fit$occupied_deaths)
    ) && identical(
        as.integer(observed_upcrossings),
        as.integer(fit$substantive_upcrossings)
    ) && identical(
        as.integer(observed_downcrossings),
        as.integer(fit$substantive_downcrossings)
    )
    count_identity <- identical(
        as.integer(observed_Kocc), as.integer(fit$occupied_experts)
    ) && identical(
        as.integer(observed_Ksub), as.integer(fit$substantive_experts)
    )
    initial_size_identity <- identical(
        as.integer(size_before[1L, ]),
        as.integer(tabulate(starting_partition, nbins = config$K_fit))
    )
    adjacent_multiset_identity <- all(vapply(
        2:config$iterations, function(iteration) identical(
            sort(as.integer(size_before[iteration, ])),
            sort(as.integer(size_after[iteration - 1L, ]))
        ), logical(1)
    ))
    variance_fields <- c(
        "Kocc_after_variance", "birth_variance", "death_variance",
        "node_switch_variance"
    )
    variance_nonnegative <- all(vapply(variance_fields, function(field) {
        all(conditional[[field]] >= -1e-12)
    }, logical(1)))
    numerical_max <- c(
        row_sum = max(conditional$maximum_row_sum_error),
        dp_mass = max(conditional$maximum_dp_mass_error),
        zero_probability =
            max(conditional$maximum_zero_probability_error),
        total_identity = max(total_identity)
    )
    state <- fit$sampler_terminal_state
    terminal_valid <- is.list(state) &&
        all(c(
            "model", "completed_iterations", "Z", "theta", "classifier",
            "rng_state", "rng_kind", "n", "TT", "p", "K", "m", "rho",
            "sampler_version"
        ) %in% names(state)) &&
        identical(state$model, "gmde") &&
        identical(state$completed_iterations, config$iterations) &&
        identical(state$n, config$n) && identical(state$TT, config$TT) &&
        identical(state$p, 2L) && identical(state$K, config$K_fit) &&
        identical(state$m, config$basis_m) &&
        identical(state$rho, config$selected_rho) &&
        identical(state$sampler_version, config$sampler_version) &&
        identical(dim(state$theta), c(10L, 168L, 2L)) &&
        identical(dim(state$classifier), c(40L, 9L)) &&
        length(state$Z) == config$n &&
        all(state$Z >= 1L & state$Z <= config$K_fit) &&
        is.numeric(state$rng_state) && length(state$rng_state) >= 2L &&
        all(is.finite(state$rng_state))
    observed_trace <- countdlm_road_conditional_allocation_trace_hash(fit)
    expected_trace <- unname(
        config$expected_parent_scientific_trace_sha256[[task_id]]
    )
    trace_reproduced <- identical(observed_trace, expected_trace)
    contract <- max(numerical_max) <= tolerance && gross_identity &&
        count_identity && initial_size_identity &&
        adjacent_multiset_identity && variance_nonnegative &&
        terminal_valid && trace_reproduced
    audit <- data.frame(
        task_id = task_id,
        method = method,
        expected_scientific_trace_sha256 = expected_trace,
        observed_scientific_trace_sha256 = observed_trace,
        scientific_trace_reproduced = trace_reproduced,
        conditional_rng_unchanged = all(conditional$rng_unchanged),
        maximum_row_sum_error = numerical_max[["row_sum"]],
        maximum_dp_mass_error = numerical_max[["dp_mass"]],
        maximum_zero_probability_error =
            numerical_max[["zero_probability"]],
        maximum_total_identity_error =
            numerical_max[["total_identity"]],
        gross_event_identity_passed = gross_identity,
        count_identity_passed = count_identity,
        initial_size_identity_passed = initial_size_identity,
        adjacent_size_multiset_identity_passed =
            adjacent_multiset_identity,
        variance_nonnegative = variance_nonnegative,
        terminal_state_contract_passed = terminal_valid,
        conditional_contract_passed = contract,
        stringsAsFactors = FALSE
    )
    if (!isTRUE(contract)) {
        stop(
            paste(
                "A reconstructed K = 10 chain failed its passive",
                "diagnostic, trajectory-reproduction, or terminal-state",
                "contract."
            ),
            call. = FALSE
        )
    }
    list(state = state, audit = audit)
}

countdlm_road_conditional_allocation_block_rows <- function(
    fit, task, config
) {
    conditional <- fit$allocation_conditionals
    rows <- vector("list", length(config$blocks))
    for (block_number in seq_along(config$blocks)) {
        block_name <- names(config$blocks)[[block_number]]
        index <- config$blocks[[block_number]]
        observed_Kocc <- rowSums(
            conditional$size_after[index, , drop = FALSE] > 0L
        )
        observed_Ksub <- rowSums(
            conditional$size_after[index, , drop = FALSE] >=
                config$substantive_min
        )
        observed <- list(
            Kocc = observed_Kocc,
            births = fit$occupied_births[index],
            deaths = fit$occupied_deaths[index],
            node_switches = conditional$observed_node_switches[index]
        )
        expected <- list(
            Kocc = conditional$expected_Kocc_after[index],
            births = conditional$expected_births[index],
            deaths = conditional$expected_deaths[index],
            node_switches = conditional$expected_node_switches[index]
        )
        variance <- list(
            Kocc = conditional$Kocc_after_variance[index],
            births = conditional$birth_variance[index],
            deaths = conditional$death_variance[index],
            node_switches = conditional$node_switch_variance[index]
        )
        z <- vapply(names(observed), function(metric) {
            countdlm_road_conditional_allocation_z(
                observed[[metric]], expected[[metric]], variance[[metric]]
            )
        }, numeric(1))
        ratio <- vapply(names(observed), function(metric) {
            denominator <- sum(expected[[metric]])
            if (denominator < config$expected_event_minimum) {
                NA_real_
            } else sum(observed[[metric]]) / denominator
        }, numeric(1))
        occupancy_crossings_expected <- sum(
            expected$births + expected$deaths
        )
        substantive_crossings_expected <- sum(
            conditional$expected_upcrossings[index] +
                conditional$expected_downcrossings[index]
        )
        per_1000 <- 1000 / length(index)
        activity <- countdlm_road_conditional_allocation_activity_label(
            occupancy_crossings_expected * per_1000,
            config$occupancy_activity_breaks_per_1000
        )
        ratio_alert <- function(value) {
            is.finite(value) && (
                value < config$observed_expected_ratio_limits[[1L]] ||
                value > config$observed_expected_ratio_limits[[2L]]
            )
        }
        rows[[block_number]] <- data.frame(
            task_id = task$task_id,
            method = task$method,
            mode_id = task$mode_id,
            seed_id = task$seed_id,
            block = block_name,
            first_iteration = min(index),
            last_iteration = max(index),
            draws = length(index),
            mean_expected_Kocc_after = mean(expected$Kocc),
            mean_observed_Kocc_after = mean(observed_Kocc),
            mean_expected_Ksub_after = mean(
                conditional$expected_Ksub_after[index]
            ),
            mean_observed_Ksub_after = mean(observed_Ksub),
            expected_births = sum(expected$births),
            observed_births = sum(observed$births),
            expected_deaths = sum(expected$deaths),
            observed_deaths = sum(observed$deaths),
            expected_upcrossings = sum(
                conditional$expected_upcrossings[index]
            ),
            observed_upcrossings = sum(
                fit$substantive_upcrossings[index]
            ),
            expected_downcrossings = sum(
                conditional$expected_downcrossings[index]
            ),
            observed_downcrossings = sum(
                fit$substantive_downcrossings[index]
            ),
            expected_node_switches = sum(expected$node_switches),
            observed_node_switches = sum(observed$node_switches),
            expected_occupancy_crossings_per_1000 =
                occupancy_crossings_expected * per_1000,
            observed_occupancy_crossings_per_1000 =
                sum(observed$births + observed$deaths) * per_1000,
            expected_substantive_crossings_per_1000 =
                substantive_crossings_expected * per_1000,
            observed_substantive_crossings_per_1000 = sum(
                fit$substantive_upcrossings[index] +
                    fit$substantive_downcrossings[index]
            ) * per_1000,
            occupancy_activity = activity,
            substantive_boundary_churn =
                occupancy_crossings_expected * per_1000 < 10 &&
                substantive_crossings_expected * per_1000 >=
                    config$substantive_churn_limit_per_1000,
            calibration_z_Kocc = z[["Kocc"]],
            calibration_z_births = z[["births"]],
            calibration_z_deaths = z[["deaths"]],
            calibration_z_node_switches = z[["node_switches"]],
            calibration_alert = any(abs(z) > config$calibration_z_limit,
                                    na.rm = TRUE),
            observed_expected_ratio_Kocc = ratio[["Kocc"]],
            observed_expected_ratio_births = ratio[["births"]],
            observed_expected_ratio_deaths = ratio[["deaths"]],
            observed_expected_ratio_node_switches =
                ratio[["node_switches"]],
            descriptive_ratio_alert = any(vapply(
                ratio, ratio_alert, logical(1)
            )),
            maximum_numerical_error = max(c(
                conditional$maximum_row_sum_error[index],
                conditional$maximum_dp_mass_error[index],
                conditional$maximum_zero_probability_error[index]
            )),
            conditional_rng_unchanged = all(
                conditional$rng_unchanged[index]
            ),
            stringsAsFactors = FALSE
        )
    }
    value <- do.call(rbind, rows)
    rownames(value) <- NULL
    value
}

countdlm_road_conditional_allocation_component_rows <- function(
    fit, task, config
) {
    conditional <- fit$allocation_conditionals
    rows <- list()
    for (block_name in names(config$blocks)) {
        index <- config$blocks[[block_name]]
        before <- conditional$size_before[index, , drop = FALSE]
        after <- conditional$size_after[index, , drop = FALSE]
        probabilities <- list(
            birth = conditional$birth_probability[index, , drop = FALSE],
            death = conditional$death_probability[index, , drop = FALSE],
            upcross =
                conditional$upcross_probability[index, , drop = FALSE],
            downcross =
                conditional$downcross_probability[index, , drop = FALSE]
        )
        observed <- list(
            birth = before == 0L & after > 0L,
            death = before > 0L & after == 0L,
            upcross = before < config$substantive_min &
                after >= config$substantive_min,
            downcross = before >= config$substantive_min &
                after < config$substantive_min
        )
        for (component in seq_len(config$K_fit)) {
            component_z <- vapply(names(probabilities), function(event) {
                probability <- probabilities[[event]][, component]
                countdlm_road_conditional_allocation_z(
                    observed[[event]][, component], probability,
                    probability * (1 - probability)
                )
            }, numeric(1))
            rows[[length(rows) + 1L]] <- data.frame(
                task_id = task$task_id,
                method = task$method,
                mode_id = task$mode_id,
                seed_id = task$seed_id,
                block = block_name,
                component = component,
                component_identity = paste(
                    "iteration-local ordered label; not a persistent identity"
                ),
                mean_size_before = mean(before[, component]),
                mean_size_after = mean(after[, component]),
                mean_expected_size_after = mean(
                    conditional$expected_size_after[index, component]
                ),
                observed_occupied_after_probability = mean(
                    after[, component] > 0L
                ),
                expected_occupied_after_probability = mean(
                    1 - conditional$probability_empty_after[
                        index, component
                    ]
                ),
                observed_substantive_after_probability = mean(
                    after[, component] >= config$substantive_min
                ),
                expected_substantive_after_probability = mean(
                    conditional$probability_substantive_after[
                        index, component
                    ]
                ),
                expected_births = sum(probabilities$birth[, component]),
                observed_births = sum(observed$birth[, component]),
                expected_deaths = sum(probabilities$death[, component]),
                observed_deaths = sum(observed$death[, component]),
                expected_upcrossings = sum(
                    probabilities$upcross[, component]
                ),
                observed_upcrossings = sum(
                    observed$upcross[, component]
                ),
                expected_downcrossings = sum(
                    probabilities$downcross[, component]
                ),
                observed_downcrossings = sum(
                    observed$downcross[, component]
                ),
                calibration_z_births = component_z[["birth"]],
                calibration_z_deaths = component_z[["death"]],
                calibration_z_upcrossings = component_z[["upcross"]],
                calibration_z_downcrossings =
                    component_z[["downcross"]],
                calibration_alert = any(
                    abs(component_z) > config$calibration_z_limit,
                    na.rm = TRUE
                ),
                stringsAsFactors = FALSE
            )
        }
    }
    value <- do.call(rbind, rows)
    rownames(value) <- NULL
    value
}

countdlm_road_conditional_allocation_calibration_rows <- function(
    block_summary, config
) {
    metrics <- c("Kocc", "births", "deaths", "node_switches")
    rows <- list()
    for (row in seq_len(nrow(block_summary))) {
        for (metric in metrics) {
            rows[[length(rows) + 1L]] <- data.frame(
                task_id = block_summary$task_id[[row]],
                method = block_summary$method[[row]],
                mode_id = block_summary$mode_id[[row]],
                seed_id = block_summary$seed_id[[row]],
                block = block_summary$block[[row]],
                metric = metric,
                calibration_z = block_summary[[paste0(
                    "calibration_z_", metric
                )]][[row]],
                absolute_z_limit = config$calibration_z_limit,
                calibration_alert = abs(block_summary[[paste0(
                    "calibration_z_", metric
                )]][[row]]) > config$calibration_z_limit,
                interpretation = paste(
                    "descriptive martingale-residual calibration screen;",
                    "not an iid test or convergence proof"
                ),
                stringsAsFactors = FALSE
            )
        }
    }
    value <- do.call(rbind, rows)
    value$calibration_alert[is.na(value$calibration_alert)] <- FALSE
    rownames(value) <- NULL
    value
}

countdlm_road_conditional_allocation_method_rows <- function(
    block_summary, calibration, parent_decision, k6_decision, config
) {
    range_width <- function(value) diff(range(value))
    persistence_ratio <- function(late, middle) {
        if (middle == 0) {
            if (late == 0) 0 else Inf
        } else late / middle
    }
    rows <- vector("list", length(config$methods))
    for (method_index in seq_along(config$methods)) {
        method <- config$methods[[method_index]]
        middle <- block_summary[
            block_summary$method == method &
                block_summary$block == "middle", , drop = FALSE
        ]
        late <- block_summary[
            block_summary$method == method &
                block_summary$block == "late", , drop = FALSE
        ]
        if (nrow(middle) != 4L || nrow(late) != 4L) {
            stop("Method-level conditioning summaries are incomplete.",
                 call. = FALSE)
        }
        Kocc_middle_range <- range_width(middle$mean_expected_Kocc_after)
        Kocc_late_range <- range_width(late$mean_expected_Kocc_after)
        Ksub_middle_range <- range_width(middle$mean_expected_Ksub_after)
        Ksub_late_range <- range_width(late$mean_expected_Ksub_after)
        Kocc_persistence <- persistence_ratio(
            Kocc_late_range, Kocc_middle_range
        )
        Ksub_persistence <- persistence_ratio(
            Ksub_late_range, Ksub_middle_range
        )
        persistent_conditioning_separation <-
            (Kocc_late_range >= config$conditioning_range_limit &&
             Kocc_persistence >= config$conditioning_persistence_ratio) ||
            (Ksub_late_range >= config$conditioning_range_limit &&
             Ksub_persistence >= config$conditioning_persistence_ratio)

        method_calibration <- calibration[
            calibration$method == method &
                calibration$block %in% c("middle", "late"), , drop = FALSE
        ]
        middle_tasks <- as.character(middle$task_id)
        late_tasks <- as.character(late$task_id)
        if (anyDuplicated(middle_tasks) || anyDuplicated(late_tasks) ||
            !identical(sort(middle_tasks), sort(late_tasks))) {
            stop(
                "Method-level middle and late task identities do not match.",
                call. = FALSE
            )
        }
        expected_metrics <- c(
            "Kocc", "births", "deaths", "node_switches"
        )
        expected_calibration <- expand.grid(
            task_id = sort(middle_tasks),
            metric = expected_metrics,
            block = c("middle", "late"),
            KEEP.OUT.ATTRS = FALSE,
            stringsAsFactors = FALSE
        )
        observed_calibration_key <- paste(
            method_calibration$task_id,
            method_calibration$metric,
            method_calibration$block,
            sep = "|"
        )
        expected_calibration_key <- paste(
            expected_calibration$task_id,
            expected_calibration$metric,
            expected_calibration$block,
            sep = "|"
        )
        if (!identical(
            sort(observed_calibration_key),
            sort(expected_calibration_key)
        )) {
            stop(
                "Method-level calibration rows are incomplete or duplicated.",
                call. = FALSE
            )
        }
        calibration_keys <- unique(method_calibration[, c(
            "task_id", "metric"
        )])
        persistent_calibration <- logical(nrow(calibration_keys))
        for (key_index in seq_len(nrow(calibration_keys))) {
            selected <- method_calibration$task_id ==
                calibration_keys$task_id[[key_index]] &
                method_calibration$metric ==
                    calibration_keys$metric[[key_index]]
            pair <- method_calibration[selected, , drop = FALSE]
            expected_blocks <- c("middle", "late")
            if (!identical(
                sort(as.character(pair$block)), sort(expected_blocks)
            )) {
                stop(
                    "A calibration key lacks one middle and one late row.",
                    call. = FALSE
                )
            }
            pair <- pair[
                match(expected_blocks, pair$block), , drop = FALSE
            ]
            z_pair <- pair$calibration_z
            persistent_calibration[[key_index]] <- isTRUE(
                !anyNA(z_pair) &&
                    all(abs(z_pair) > config$calibration_z_limit) &&
                    identical(sign(z_pair[[1L]]), sign(z_pair[[2L]]))
            )
        }
        persistent_calibration_count <- sum(persistent_calibration)
        occupancy_low_by_chain <-
            late$expected_occupancy_crossings_per_1000 <
                config$occupancy_activity_breaks_per_1000[[2L]]
        occupancy_mobile_by_chain <-
            late$expected_occupancy_crossings_per_1000 >=
                config$occupancy_activity_breaks_per_1000[[2L]]
        occupancy_low_count <- sum(occupancy_low_by_chain)
        occupancy_mobile_count <- sum(occupancy_mobile_by_chain)
        substantive_churn_count <- sum(late$substantive_boundary_churn)
        occupancy_low <- occupancy_low_count >=
            config$method_occupancy_consensus_min
        occupancy_mobile <- occupancy_mobile_count >=
            config$method_occupancy_consensus_min
        substantive_churn <- substantive_churn_count >=
            config$method_substantive_churn_min
        k10_row <- parent_decision[parent_decision$method == method,
                                   , drop = FALSE]
        k6_row <- k6_decision[k6_decision$method == method,
                             , drop = FALSE]
        if (nrow(k10_row) != 1L || nrow(k6_row) != 1L) {
            stop("A parent method decision is unavailable.", call. = FALSE)
        }
        flags <- c(
            if (persistent_calibration_count > 0L) {
                "implementation-review-required"
            },
            if (persistent_conditioning_separation && occupancy_low) {
                "conditioning-state-bottleneck"
            },
            if (occupancy_mobile &&
                !isTRUE(k10_row$resolved_under_control[[1L]])) {
                "allocation-conditionally-mobile-but-joint-unresolved"
            },
            if (substantive_churn) "substantive-boundary-churn"
        )
        if (!length(flags)) flags <- "mixed-inconclusive"
        rows[[method_index]] <- data.frame(
            method = method,
            K10_parent_resolved_under_control =
                k10_row$resolved_under_control[[1L]],
            K6_resolved_under_same_control =
                k6_row$resolved_under_control[[1L]],
            middle_expected_Kocc_range = Kocc_middle_range,
            late_expected_Kocc_range = Kocc_late_range,
            Kocc_late_middle_range_ratio = Kocc_persistence,
            middle_expected_Ksub_range = Ksub_middle_range,
            late_expected_Ksub_range = Ksub_late_range,
            Ksub_late_middle_range_ratio = Ksub_persistence,
            persistent_conditioning_separation =
                persistent_conditioning_separation,
            minimum_late_expected_occupancy_crossings_per_1000 = min(
                late$expected_occupancy_crossings_per_1000
            ),
            median_late_expected_occupancy_crossings_per_1000 =
                stats::median(
                    late$expected_occupancy_crossings_per_1000
                ),
            maximum_late_expected_occupancy_crossings_per_1000 = max(
                late$expected_occupancy_crossings_per_1000
            ),
            minimum_late_expected_substantive_crossings_per_1000 = min(
                late$expected_substantive_crossings_per_1000
            ),
            median_late_expected_substantive_crossings_per_1000 =
                stats::median(
                    late$expected_substantive_crossings_per_1000
                ),
            maximum_late_expected_substantive_crossings_per_1000 = max(
                late$expected_substantive_crossings_per_1000
            ),
            late_occupancy_low_chain_count = occupancy_low_count,
            late_occupancy_mobile_chain_count = occupancy_mobile_count,
            method_occupancy_consensus_min =
                config$method_occupancy_consensus_min,
            method_substantive_churn_min =
                config$method_substantive_churn_min,
            persistent_calibration_alert_count =
                persistent_calibration_count,
            late_descriptive_ratio_alert_count = sum(
                late$descriptive_ratio_alert
            ),
            late_substantive_boundary_churn_chain_count =
                substantive_churn_count,
            diagnostic_label = paste(flags, collapse = " | "),
            formal_run_authorized = FALSE,
            requires_human_review = TRUE,
            stringsAsFactors = FALSE
        )
    }
    value <- do.call(rbind, rows)
    rownames(value) <- NULL
    value
}

countdlm_road_conditional_allocation_rule_specification <- function(config) {
    data.frame(
        rule_id = c(
            "passive-pre-draw", "scientific-trace-identity",
            "component-label-scope", "occupancy-activity",
            "substantive-boundary-churn", "conditioning-separation",
            "calibration-z", "observed-expected-ratio",
            "interpretation-limit"
        ),
        fixed_definition = c(
            paste(
                "summaries are computed from p(Z_i=k | theta,gamma,Y)",
                "immediately before the existing categorical draw and",
                "must leave .Random.seed unchanged"
            ),
            paste(
                "SHA-256 of the frozen scientific field list must equal",
                "the reviewed K10 fingerprint separately for all 12 tasks"
            ),
            paste(
                "component columns refer to the allocation step's",
                "pre-relabel, iteration-local labels; they must not be",
                "aligned componentwise with relabeled fit$size or fit$Z"
            ),
            paste0(
                "expected occupancy births+deaths per 1000: <",
                config$occupancy_activity_breaks_per_1000[[1L]],
                " near-deterministic; <",
                config$occupancy_activity_breaks_per_1000[[2L]],
                " sparse; <",
                config$occupancy_activity_breaks_per_1000[[3L]],
                " active; otherwise high; method-level low or mobile",
                " requires at least ",
                config$method_occupancy_consensus_min,
                " of four late chains"
            ),
            paste0(
                "flag when expected occupancy crossings per 1000 <10 and",
                " expected substantive threshold crossings per 1000 >=",
                config$substantive_churn_limit_per_1000,
                " in at least ", config$method_substantive_churn_min,
                " of four late chains"
            ),
            paste0(
                "flag when the four-chain late range of conditional mean",
                " Kocc or Ksub is >=", config$conditioning_range_limit,
                " and its late/middle range ratio is >=",
                config$conditioning_persistence_ratio
            ),
            paste0(
                "martingale-residual descriptive alert when |z| >",
                config$calibration_z_limit,
                "; persistent when middle and late have the same sign and",
                " both exceed the limit"
            ),
            paste0(
                "descriptive alert only when conditional expected total >=",
                config$expected_event_minimum,
                " and observed/expected lies outside [",
                config$observed_expected_ratio_limits[[1L]], ",",
                config$observed_expected_ratio_limits[[2L]], "]"
            ),
            paste(
                "diagnoses one allocation update conditional on the current",
                "theta and gamma; it is not a convergence proof, does not",
                "change the K10 target, and cannot authorize a formal run"
            )
        ),
        interpretation_limit = c(
            "the diagnostic is deterministic and consumes no RNG",
            "any mismatch invalidates the reconstructed task",
            paste(
                "only label-invariant totals and event totals may be",
                "compared after relabeling"
            ),
            "activity is conditional, not realized joint-chain mixing",
            "a threshold-boundary mechanism flag, not a target revision",
            "a conditioning-state flag, not proof of a posterior mode",
            "descriptive sequential calibration screen, not an iid test",
            "descriptive discrepancy only; thresholds are not retuned",
            "all method labels require human review"
        ),
        stringsAsFactors = FALSE
    )
}

countdlm_road_conditional_allocation_output_files <- function() {
    c(
        rule_specification =
            "conditional-allocation-rule-specification.csv",
        runtime = "conditional-allocation-runtime.csv",
        initialization = "conditional-allocation-initialization-audit.csv",
        parent_trace = "parent-scientific-trace-audit.csv",
        reconstructed_trace =
            "reconstructed-scientific-trace-audit.csv",
        block_summary = "conditional-allocation-block-summary.csv",
        component_summary =
            "conditional-allocation-component-block-summary.csv",
        calibration = "conditional-allocation-calibration-z.csv",
        method_diagnosis = "conditional-allocation-method-diagnosis.csv",
        terminal_audit = "conditional-allocation-terminal-state-audit.csv"
    )
}

countdlm_road_conditional_allocation_expected_payload <- function(
    config, source_names
) {
    slugs <- vapply(seq_len(nrow(config$tasks)), function(index) {
        countdlm_road_partition_task_slug(
            index, config$tasks$task_id[[index]]
        )
    }, character(1))
    sort(c(
        "FROZEN_SOURCE_SHA256.csv",
        "road-conditional-allocation-registration.rds",
        "RUN-STARTED.rds",
        "road-conditional-allocation-blinded-input.rds",
        "conditional-allocation-mode-specification.csv",
        unname(countdlm_road_conditional_allocation_output_files()),
        "road-conditional-allocation-result.rds",
        "road-conditional-allocation-report.txt",
        file.path("chains", paste0(slugs, "-fit.rds")),
        file.path("diagnostics", paste0(slugs, "-diagnostic.rds")),
        file.path(
            "terminal-states", paste0(slugs, "-terminal-state.rds")
        ),
        file.path("frozen-source", "R", source_names)
    ))
}

countdlm_road_assert_conditional_allocation_payload <- function(
    output_dir, config, source_names
) {
    expected <- countdlm_road_conditional_allocation_expected_payload(
        config, source_names
    )
    observed <- sort(list.files(
        output_dir, recursive = TRUE, full.names = FALSE,
        include.dirs = FALSE, all.files = TRUE, no.. = TRUE
    ))
    missing <- setdiff(expected, observed)
    unexpected <- setdiff(observed, expected)
    size <- file.info(file.path(output_dir, expected))$size
    if (length(missing) || length(unexpected) || anyNA(size) ||
        any(size <= 0)) {
        stop(
            paste(
                "The conditional-allocation payload failed its exact",
                "inventory audit."
            ),
            " Missing: ", paste(missing, collapse = ", "),
            "; unexpected: ", paste(unexpected, collapse = ", "), ".",
            call. = FALSE
        )
    }
    invisible(expected)
}

countdlm_road_assert_conditional_allocation_output <- function(
    output_dir, directories, config_signature, stage
) {
    required <- unique(c(output_dir, directories))
    registration_path <- file.path(
        output_dir, "road-conditional-allocation-registration.rds"
    )
    started_path <- file.path(output_dir, "RUN-STARTED.rds")
    files <- c(registration_path, started_path)
    missing <- c(
        required[!dir.exists(required)], files[!file.exists(files)]
    )
    if (length(missing)) {
        stop(
            "The registered conditional-allocation output tree is unavailable ",
            "during ", stage, ". Missing: ",
            paste(missing, collapse = ", "),
            ". It will not be recreated.", call. = FALSE
        )
    }
    registration <- tryCatch(
        readRDS(registration_path), error = function(error) NULL
    )
    observed <- if (is.list(registration) &&
        is.list(registration$config)) {
        registration$config$config_signature
    } else NULL
    if (!identical(observed, config_signature)) {
        stop(
            "The conditional-allocation output identity failed during ",
            stage, ".", call. = FALSE
        )
    }
    invisible(TRUE)
}

#' Run the passive K = 10 conditional-allocation diagnostic
#'
#' The runner reconstructs the twelve reviewed K = 10 partition-diagnostic
#' chains and records deterministic summaries of the allocation probabilities
#' immediately before the existing categorical draws.  Every reconstructed
#' scientific trace must match its reviewed parent SHA-256 exactly.  This is a
#' truth-blinded development diagnostic and cannot authorize a formal run.
#'
#' @param config Output of
#'   `countdlm_road_conditional_allocation_config()`.
#' @param repository_root Any path inside the Git repository recorded as
#'   provenance.
#' @return Diagnostic summaries and immutable output paths.
#' @export
countdlm_road_conditional_allocation_pilot <- function(
    config, repository_root
) {
    started_at <- Sys.time()
    previous_rng_kind <- RNGkind()
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    previous_seed <- if (had_seed) {
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else NULL
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    on.exit({
        do.call(RNGkind, as.list(previous_rng_kind))
        if (had_seed) {
            assign(".Random.seed", previous_seed, envir = .GlobalEnv)
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
        set <- !is.na(previous_thread_values)
        if (any(set)) {
            do.call(Sys.setenv, as.list(stats::setNames(
                previous_thread_values[set], thread_variables[set]
            )))
        }
        if (any(!set)) Sys.unsetenv(thread_variables[!set])
    }, add = TRUE)

    countdlm_road_validate_conditional_allocation_config(config)
    if (!requireNamespace("BayesLogit", quietly = TRUE) ||
        !requireNamespace("digest", quietly = TRUE)) {
        stop(
            "The conditional-allocation diagnostic requires BayesLogit and digest.",
            call. = FALSE
        )
    }
    parent <- countdlm_road_k6_parent_evidence(
        config$parent_partition_zip
    )
    k6 <- countdlm_road_conditional_allocation_k6_evidence(
        config$k6_diagnostic_zip
    )
    parent_trace_audit <-
        countdlm_road_conditional_allocation_parent_trace_audit(parent)

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
    environment_match <- vapply(
        names(current_environment), function(field) identical(
            current_environment[[field]], parent_environment[[field]]
        ), logical(1)
    )
    thread_values <- Sys.getenv(thread_variables, unset = NA_character_)
    if (!all(environment_match) || anyNA(thread_values) ||
        any(thread_values != "1")) {
        mismatch <- names(current_environment)[!environment_match]
        if (anyNA(thread_values) || any(thread_values != "1")) {
            mismatch <- c(mismatch, "single-thread worker environment")
        }
        stop(
            "The current R environment differs from the reviewed K = 10 parent for: ",
            paste(mismatch, collapse = ", "), ". The run was not started.",
            call. = FALSE
        )
    }

    git <- countdlm_road_git_state(repository_root)
    source_files <- sort(list.files(
        config$execution_source_dir, pattern = "[.]R$", full.names = TRUE
    ))
    source_sha256 <- stats::setNames(
        vapply(source_files, countdlm_road_sha256, character(1)),
        basename(source_files)
    )
    if (!identical(source_sha256, config$source_sha256)) {
        stop("Frozen execution sources changed after configuration.",
             call. = FALSE)
    }

    context <- countdlm_road_load_approved_context(config$context_file)
    observed_context_sha256 <- countdlm_road_sha256(config$context_file)
    if (!identical(observed_context_sha256, config$context_sha256)) {
        stop("The approved D-017 context hash does not match.",
             call. = FALSE)
    }
    method_inputs <- countdlm_road_method_inputs(context)
    observed_basis_sha256 <- stats::setNames(vapply(
        config$methods, function(method) digest::digest(
            method_inputs[[method]]$Phi,
            algo = "sha256", serialize = TRUE
        ), character(1)
    ), config$methods)
    if (!identical(observed_basis_sha256, config$basis_sha256) ||
        any(vapply(config$methods, function(method) {
            !identical(dim(method_inputs[[method]]$Phi), c(100L, 40L))
        }, logical(1)))) {
        stop("A registered graph-gating basis changed.", call. = FALSE)
    }
    blinded_data <- list(Y = parent$blinded$Y, Fmat = parent$blinded$Fmat)
    observed_data_sha256 <- digest::digest(
        blinded_data, algo = "sha256", serialize = TRUE
    )
    if (!identical(observed_data_sha256, config$data_sha256)) {
        stop("The K = 10 parent blinded-data hash does not match.",
             call. = FALSE)
    }
    neutral <- countdlm_road_partition_neutral_initialization(
        blinded_data$Y, blinded_data$Fmat
    )
    modes <- parent$modes
    expected_mode_hash <- stats::setNames(
        config$mode_specification$partition_sha256,
        paste(
            config$mode_specification$method,
            config$mode_specification$mode_id, sep = "|"
        )
    )
    if (!identical(
        vapply(modes, countdlm_road_partition_hash, character(1)),
        expected_mode_hash
    )) {
        stop("The reviewed K = 10 starts failed their hash audit.",
             call. = FALSE)
    }

    reported_physical_cores <- parallel::detectCores(logical = FALSE)
    actual_workers <- min(
        config$workers,
        reported_physical_cores - config$reserved_reported_cores
    )
    if (!is.finite(reported_physical_cores) ||
        reported_physical_cores < 10L ||
        !identical(as.integer(actual_workers), config$workers)) {
        stop(
            "The six-worker/four-reported-core-reserve contract cannot be met.",
            call. = FALSE
        )
    }
    output_dir <- countdlm_road_calibration_output_path(
        config$output_dir, git$root
    )
    rm(context)
    invisible(gc(verbose = FALSE))

    if (!dir.create(output_dir, recursive = FALSE, showWarnings = FALSE)) {
        stop(
            "Could not create the registered conditional-allocation output directory.",
            call. = FALSE
        )
    }
    run_complete <- FALSE
    run_stage <- "registration"
    on.exit({
        if (!run_complete && dir.exists(output_dir)) {
            incomplete <- file.path(output_dir, "RUN-INCOMPLETE.rds")
            if (!file.exists(incomplete)) {
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
                    ), incomplete
                )
                if (!isTRUE(retention$saved)) {
                    message("RUN-INCOMPLETE could not be written: ",
                            retention$error)
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
        target_changed_from_parent = FALSE,
        git_head = git$head,
        git_clean = git$clean,
        git_status = git$status,
        execution_environment = c(current_environment, list(
            physical_cores_reported = reported_physical_cores,
            logical_cores_reported = parallel::detectCores(logical = TRUE),
            requested_workers = config$workers,
            actual_workers = actual_workers,
            reported_cores_left_unused =
                reported_physical_cores - actual_workers,
            worker_thread_limit = config$worker_thread_limit
        )),
        source_sha256 = source_sha256,
        parent_partition = list(
            api_version = config$parent_api_version,
            zip_file = parent$zip_file,
            zip_sha256 = parent$zip_sha256,
            manifest_sha256 = parent$manifest_sha256,
            result_sha256 = parent$result_sha256,
            completion_sha256 = parent$completion_sha256,
            registration_sha256 = parent$registration_sha256,
            blinded_sha256 = parent$blinded_sha256,
            config_signature = config$parent_config_signature,
            fitted_K = config$K_fit
        ),
        k6_motivation = list(
            api_version = config$k6_api_version,
            zip_file = k6$zip_file,
            zip_sha256 = k6$zip_sha256,
            manifest_sha256 = k6$manifest_sha256,
            result_sha256 = k6$result_sha256,
            completion_sha256 = k6$completion_sha256,
            registration_sha256 = k6$registration_sha256,
            blinded_sha256 = k6$blinded_sha256,
            config_signature = k6$config_signature,
            supplies_target_or_chain_state = FALSE
        ),
        context_sha256 = observed_context_sha256,
        context_approval = "D-017",
        data_sha256 = observed_data_sha256,
        basis_sha256 = observed_basis_sha256,
        neutral_initialization_sha256 = neutral$sha256,
        mode_specification = config$mode_specification,
        task_seeds = stats::setNames(
            config$tasks$seed, config$tasks$task_id
        ),
        expected_parent_scientific_trace_sha256 =
            config$expected_parent_scientific_trace_sha256,
        config = unclass(config),
        note = paste(
            "Passive pre-draw conditional-allocation diagnostic only;",
            "the K10 target and scientific RNG trajectory must remain",
            "bitwise identical to the reviewed parent; no formal run is",
            "authorized."
        )
    )
    countdlm_road_atomic_save_rds(
        registration,
        file.path(
            output_dir,
            "road-conditional-allocation-registration.rds"
        )
    )
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            status = "running",
            started_at = format(started_at, tz = "UTC", usetz = TRUE),
            formal_simulation_launched = FALSE
        ), file.path(output_dir, "RUN-STARTED.rds")
    )

    chain_dir <- file.path(output_dir, "chains")
    diagnostic_dir <- file.path(output_dir, "diagnostics")
    terminal_dir <- file.path(output_dir, "terminal-states")
    failure_dir <- file.path(output_dir, "failures")
    frozen_source_dir <- file.path(output_dir, "frozen-source", "R")
    directories <- c(
        chain_dir, diagnostic_dir, terminal_dir, failure_dir,
        frozen_source_dir
    )
    for (directory in directories) {
        if (!dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
            stop(
                "Could not create conditional-allocation subdirectories.",
                call. = FALSE
            )
        }
    }
    frozen_source_files <- file.path(
        frozen_source_dir, basename(source_files)
    )
    copied <- file.copy(
        source_files, frozen_source_files,
        overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE
    )
    frozen_source_sha256 <- stats::setNames(
        vapply(frozen_source_files, countdlm_road_sha256, character(1)),
        basename(frozen_source_files)
    )
    if (!all(copied) || !identical(frozen_source_sha256, source_sha256)) {
        stop("The frozen execution-source copy failed verification.",
             call. = FALSE)
    }
    countdlm_road_atomic_write_csv(
        data.frame(
            source_file = names(frozen_source_sha256),
            sha256 = unname(frozen_source_sha256),
            stringsAsFactors = FALSE
        ), file.path(output_dir, "FROZEN_SOURCE_SHA256.csv")
    )
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            Y = blinded_data$Y,
            Fmat = blinded_data$Fmat,
            Y_Fmat_sha256 = observed_data_sha256,
            truth_fields_stored = FALSE,
            truth_metrics_computed = FALSE,
            K_fit = config$K_fit,
            modes = modes,
            mode_specification = config$mode_specification,
            neutral_theta = neutral$theta,
            neutral_gamma = neutral$gamma,
            neutral_initialization_sha256 = neutral$sha256
        ), file.path(
            output_dir,
            "road-conditional-allocation-blinded-input.rds"
        )
    )
    countdlm_road_atomic_write_csv(
        config$mode_specification,
        file.path(output_dir, "conditional-allocation-mode-specification.csv")
    )
    countdlm_road_atomic_write_csv(
        countdlm_road_conditional_allocation_rule_specification(config),
        file.path(
            output_dir,
            "conditional-allocation-rule-specification.csv"
        )
    )
    countdlm_road_atomic_write_csv(
        parent_trace_audit,
        file.path(output_dir, "parent-scientific-trace-audit.csv")
    )
    countdlm_road_assert_conditional_allocation_output(
        output_dir, directories, config$config_signature,
        "pre-dispatch verification"
    )

    diagnostic_worker <- function(index) {
        countdlm_road_calibration_limit_threads()
        task <- config$tasks[index, , drop = FALSE]
        task_id <- task$task_id[[1L]]
        method <- task$method[[1L]]
        mode_id <- task$mode_id[[1L]]
        seed_id <- task$seed_id[[1L]]
        seed <- task$seed[[1L]]
        starting_partition <- modes[[paste(method, mode_id, sep = "|")]]
        expected_partition_hash <- task$parent_partition_sha256[[1L]]
        slug <- countdlm_road_partition_task_slug(index, task_id)
        fit_path <- file.path(chain_dir, paste0(slug, "-fit.rds"))
        fit_relative <- file.path("chains", basename(fit_path))
        diagnostic_path <- file.path(
            diagnostic_dir, paste0(slug, "-diagnostic.rds")
        )
        terminal_path <- file.path(
            terminal_dir, paste0(slug, "-terminal-state.rds")
        )
        terminal_relative <- file.path(
            "terminal-states", basename(terminal_path)
        )
        failure_path <- file.path(
            failure_dir, paste0(slug, "-failure.rds")
        )
        task_started <- Sys.time()
        worker_stage <- "conditional-allocation-fit"
        warning_messages <- character()

        withCallingHandlers(tryCatch({
            fit <- countdlm_road_fit_method(
                method = method,
                data = blinded_data,
                method_inputs = method_inputs,
                config = config,
                seed = seed,
                Z_init = starting_partition,
                theta_init = neutral$theta,
                gamma_init = neutral$gamma,
                potts_beta = NULL,
                n_iter = config$iterations,
                burn = config$burn,
                rho = config$selected_rho,
                store_prediction_state = FALSE,
                store_sampler_terminal_state = TRUE,
                store_allocation_conditionals = TRUE
            )
            elapsed <- as.numeric(difftime(
                Sys.time(), task_started, units = "secs"
            ))
            worker_stage <- "conditional-allocation-save-fit"
            countdlm_road_atomic_save_rds(
                list(
                    api_version = config$api_version,
                    parent_partition_result_sha256 =
                        config$parent_result_sha256,
                    k6_motivation_result_sha256 = config$k6_result_sha256,
                    task_id = task_id,
                    method = method,
                    mode_id = mode_id,
                    seed_id = seed_id,
                    seed = seed,
                    fitted_K = config$K_fit,
                    selected_rho = config$selected_rho,
                    starting_partition = starting_partition,
                    expected_partition_sha256 = expected_partition_hash,
                    truth_metrics_computed = FALSE,
                    target_changed_from_parent = FALSE,
                    warnings_before_fit_retention = warning_messages,
                    fit = fit
                ), fit_path
            )

            worker_stage <- "conditional-allocation-contract"
            initial_partition_hash <- countdlm_road_partition_hash(
                fit$initialization$Z
            )
            initial_theta_hash <- digest::digest(
                fit$initialization$theta,
                algo = "sha256", serialize = TRUE
            )
            initial_gamma_hash <- digest::digest(
                fit$initialization$classifier,
                algo = "sha256", serialize = TRUE
            )
            initial_joint_hash <- digest::digest(
                list(
                    theta = fit$initialization$theta,
                    gamma = fit$initialization$classifier
                ), algo = "sha256", serialize = TRUE
            )
            if (!identical(initial_partition_hash,
                           expected_partition_hash) ||
                !identical(initial_theta_hash,
                           config$neutral_theta_sha256) ||
                !identical(initial_gamma_hash,
                           config$neutral_gamma_sha256) ||
                !identical(initial_joint_hash,
                           config$neutral_joint_sha256)) {
                stop(
                    "The reconstructed K = 10 initialization contract failed.",
                    call. = FALSE
                )
            }
            validated <-
                countdlm_road_validate_conditional_allocation_fit(
                    fit = fit,
                    starting_partition = starting_partition,
                    config = config,
                    task_id = task_id,
                    method = method
                )
            validated$audit$mode_id <- mode_id
            validated$audit$seed_id <- seed_id
            validated$audit$seed <- seed
            fixed_rho_audit <- countdlm_road_fixed_rho_calibration(
                fit = fit,
                settle = config$burn,
                score = config$iterations - config$burn
            )

            worker_stage <- "conditional-allocation-save-terminal"
            countdlm_road_atomic_save_rds(
                list(
                    api_version = config$api_version,
                    task_id = task_id,
                    method = method,
                    mode_id = mode_id,
                    seed_id = seed_id,
                    data_sha256 = config$data_sha256,
                    basis_sha256 = unname(config$basis_sha256[[method]]),
                    config_signature = config$config_signature,
                    exact_resume_contract = paste(
                        "fixed-rho GMDE; pass sampler_state as resume_state;",
                        "keep seed and initialization arguments NULL"
                    ),
                    sampler_state = validated$state
                ), terminal_path
            )
            terminal_audit <- data.frame(
                task_id = task_id,
                method = method,
                mode_id = mode_id,
                seed_id = seed_id,
                seed = seed,
                completed_iterations =
                    validated$state$completed_iterations,
                Z_sha256 = digest::digest(
                    validated$state$Z,
                    algo = "sha256", serialize = TRUE
                ),
                theta_sha256 = digest::digest(
                    validated$state$theta,
                    algo = "sha256", serialize = TRUE
                ),
                classifier_sha256 = digest::digest(
                    validated$state$classifier,
                    algo = "sha256", serialize = TRUE
                ),
                rng_state_sha256 = digest::digest(
                    validated$state$rng_state,
                    algo = "sha256", serialize = TRUE
                ),
                joint_state_sha256 = digest::digest(
                    validated$state,
                    algo = "sha256", serialize = TRUE
                ),
                exact_resume_interface_available = TRUE,
                terminal_state_file = terminal_relative,
                terminal_state_file_sha256 =
                    countdlm_road_sha256(terminal_path),
                stringsAsFactors = FALSE
            )

            worker_stage <- "conditional-allocation-summarize"
            block_summary <-
                countdlm_road_conditional_allocation_block_rows(
                    fit, task, config
                )
            component_summary <-
                countdlm_road_conditional_allocation_component_rows(
                    fit, task, config
                )
            summarized <- countdlm_road_summarize_fit(
                fit = fit,
                task_id = task_id,
                method = method,
                potts_beta = NA_real_,
                burn = config$burn,
                elapsed = elapsed,
                warnings = warning_messages
            )
            summarized$summary$variant_id <- method
            summarized$summary$mode_id <- mode_id
            summarized$summary$seed_id <- seed_id
            summarized$summary$seed <- seed
            summarized$summary$sampler_version <-
                fit$settings$sampler_version
            summarized$summary$fixed_rho_contract_passed <- TRUE
            summarized$summary$initial_partition_sha256 <-
                initial_partition_hash
            summarized$summary$initial_theta_sha256 <- initial_theta_hash
            summarized$summary$initial_gamma_sha256 <- initial_gamma_hash
            summarized$summary$initial_joint_sha256 <- initial_joint_hash
            summarized$summary$paired_initialization_passed <- TRUE
            summarized$summary$scientific_trace_reproduced <-
                validated$audit$scientific_trace_reproduced[[1L]]
            summarized$summary$conditional_rng_unchanged <-
                validated$audit$conditional_rng_unchanged[[1L]]
            summarized$summary$conditional_contract_passed <-
                validated$audit$conditional_contract_passed[[1L]]
            summarized$summary$terminal_state_contract_passed <-
                validated$audit$terminal_state_contract_passed[[1L]]
            summarized$summary$ari_computed <- any(!is.na(fit$ari))
            summarized$summary$acc_computed <- any(!is.na(fit$acc))
            summarized$summary$fit_file <- fit_relative
            summarized$summary$terminal_state_file <- terminal_relative
            summarized$summary$completed_fit_retained <- TRUE

            summarized$compact$api_version <- config$api_version
            summarized$compact$method <- method
            summarized$compact$mode_id <- mode_id
            summarized$compact$seed_id <- seed_id
            summarized$compact$seed <- seed
            summarized$compact$fixed_rho_contract <- fixed_rho_audit
            summarized$compact$initial_partition_sha256 <-
                initial_partition_hash
            summarized$compact$initial_joint_sha256 <- initial_joint_hash
            summarized$compact$scientific_trace_audit <- validated$audit
            summarized$compact$block_summary <- block_summary
            summarized$compact$component_summary <- component_summary
            summarized$compact$terminal_audit <- terminal_audit
            countdlm_road_atomic_save_rds(
                summarized$compact, diagnostic_path
            )
            summarized$summary$warning_count <- length(warning_messages)
            summarized$summary$warnings <- if (length(warning_messages)) {
                paste(warning_messages, collapse = " | ")
            } else NA_character_
            list(
                summary = summarized$summary,
                trace_audit = validated$audit,
                block_summary = block_summary,
                component_summary = component_summary,
                terminal_audit = terminal_audit
            )
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
            summary$variant_id <- method
            summary$mode_id <- mode_id
            summary$seed_id <- seed_id
            summary$seed <- seed
            summary$sampler_version <- NA_character_
            summary$fixed_rho_contract_passed <- FALSE
            summary$initial_partition_sha256 <- NA_character_
            summary$initial_theta_sha256 <- NA_character_
            summary$initial_gamma_sha256 <- NA_character_
            summary$initial_joint_sha256 <- NA_character_
            summary$paired_initialization_passed <- FALSE
            summary$scientific_trace_reproduced <- FALSE
            summary$conditional_rng_unchanged <- FALSE
            summary$conditional_contract_passed <- FALSE
            summary$terminal_state_contract_passed <- FALSE
            summary$ari_computed <- FALSE
            summary$acc_computed <- FALSE
            summary$fit_file <- if (file.exists(fit_path)) {
                fit_relative
            } else NA_character_
            summary$terminal_state_file <- if (file.exists(terminal_path)) {
                terminal_relative
            } else NA_character_
            summary$completed_fit_retained <- file.exists(fit_path)
            retention <- countdlm_road_try_retain_failure(
                list(
                    api_version = config$api_version,
                    phase = "K10-conditional-allocation-diagnostic",
                    task_id = task_id,
                    method = method,
                    mode_id = mode_id,
                    seed_id = seed_id,
                    seed = seed,
                    status = "error",
                    elapsed_seconds = elapsed,
                    stage = worker_stage,
                    error = conditionMessage(error),
                    call = paste(deparse(conditionCall(error)),
                                 collapse = " "),
                    warnings = warning_messages,
                    completed_fit_retained = file.exists(fit_path),
                    terminal_state_retained = file.exists(terminal_path),
                    summary = summary
                ), failure_path
            )
            list(
                summary = summary,
                trace_audit = NULL,
                block_summary = NULL,
                component_summary = NULL,
                terminal_audit = NULL,
                failure_record_saved = retention$saved,
                failure_record_error = retention$error
            )
        }), warning = function(warning) {
            warning_messages <<- unique(c(
                warning_messages, conditionMessage(warning)
            ))
            invokeRestart("muffleWarning")
        })
    }

    cat(
        "K=10 passive conditional-allocation diagnostic: ",
        "3 methods x 2 starts x 2 seeds\n",
        config$iterations, " transitions / ", config$burn,
        " settling; m=", config$basis_m,
        "; fixed rho=", config$selected_rho, "\n",
        "Two guarded batches of six single-thread workers; about ",
        config$reserved_reported_cores,
        " reported cores remain unused.\n",
        "The allocation summaries are pre-draw and passive; every scientific ",
        "trace must reproduce its reviewed K10 SHA-256.\n",
        "No truth metrics, no target change, and no formal simulation.\n",
        sep = ""
    )
    run_stage <- "conditional-allocation-chain-batches"
    pilot_started <- Sys.time()
    task_results <- vector("list", nrow(config$tasks))
    batches <- sort(unique(config$tasks$batch_id))
    for (batch_id in batches) {
        indices <- which(config$tasks$batch_id == batch_id)
        countdlm_road_assert_conditional_allocation_output(
            output_dir, directories, config$config_signature,
            paste0("before batch ", batch_id)
        )
        cat(
            "\n[BATCH] ", batch_id, "/", length(batches),
            "; 6 tasks\n", sep = ""
        )
        batch_results <- countdlm_road_run_batches(
            config$tasks$task_id[indices],
            function(local_index) diagnostic_worker(
                indices[[local_index]]
            ),
            cores = actual_workers,
            poll_seconds = config$progress_poll_seconds
        )
        for (local_index in seq_along(batch_results)) {
            if (!inherits(
                batch_results[[local_index]],
                "countdlm_road_scheduler_error"
            )) next
            error <- batch_results[[local_index]]
            index <- indices[[local_index]]
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
                stage = paste0("parallel-", error$origin)
            )
            summary$variant_id <- task$method[[1L]]
            summary$mode_id <- task$mode_id[[1L]]
            summary$seed_id <- task$seed_id[[1L]]
            summary$seed <- task$seed[[1L]]
            summary$sampler_version <- NA_character_
            summary$fixed_rho_contract_passed <- FALSE
            summary$initial_partition_sha256 <- NA_character_
            summary$initial_theta_sha256 <- NA_character_
            summary$initial_gamma_sha256 <- NA_character_
            summary$initial_joint_sha256 <- NA_character_
            summary$paired_initialization_passed <- FALSE
            summary$scientific_trace_reproduced <- FALSE
            summary$conditional_rng_unchanged <- FALSE
            summary$conditional_contract_passed <- FALSE
            summary$terminal_state_contract_passed <- FALSE
            summary$ari_computed <- FALSE
            summary$acc_computed <- FALSE
            summary$fit_file <- NA_character_
            summary$terminal_state_file <- NA_character_
            summary$completed_fit_retained <- FALSE
            retention <- countdlm_road_try_retain_failure(
                list(
                    api_version = config$api_version,
                    phase = "K10-conditional-allocation-diagnostic",
                    status = "error",
                    retained = TRUE,
                    error_origin = error$origin,
                    original_error = error$message,
                    summary = summary
                ), file.path(
                    failure_dir,
                    paste0(sprintf("%02d", index),
                           "-scheduler-failure.rds")
                )
            )
            batch_results[[local_index]] <- list(
                summary = summary,
                trace_audit = NULL,
                block_summary = NULL,
                component_summary = NULL,
                terminal_audit = NULL,
                failure_record_saved = retention$saved,
                failure_record_error = retention$error
            )
        }
        countdlm_road_stop_if_failure_unretained(
            batch_results,
            paste0("conditional-allocation diagnostic batch ", batch_id)
        )
        for (local_index in seq_along(indices)) {
            task_results[[indices[[local_index]]]] <-
                batch_results[[local_index]]
        }
        countdlm_road_assert_conditional_allocation_output(
            output_dir, directories, config$config_signature,
            paste0("after batch ", batch_id)
        )
        batch_runtime <- do.call(rbind, lapply(
            batch_results, `[[`, "summary"
        ))
        if (any(batch_runtime$status != "ok") ||
            any(batch_runtime$warning_count > 0L)) {
            stop(
                "A conditional-allocation task failed or warned in batch ",
                batch_id, ". Retained evidence must be reviewed.",
                call. = FALSE
            )
        }
    }

    pilot_wall_seconds <- as.numeric(difftime(
        Sys.time(), pilot_started, units = "secs"
    ))
    runtime <- do.call(rbind, lapply(task_results, `[[`, "summary"))
    runtime <- runtime[
        match(config$tasks$task_id, runtime$task_id), , drop = FALSE
    ]
    rownames(runtime) <- NULL
    reconstructed_trace_audit <- do.call(rbind, lapply(
        task_results, `[[`, "trace_audit"
    ))
    reconstructed_trace_audit <- reconstructed_trace_audit[
        match(
            config$tasks$task_id,
            reconstructed_trace_audit$task_id
        ), , drop = FALSE
    ]
    rownames(reconstructed_trace_audit) <- NULL
    block_summary <- do.call(rbind, lapply(
        task_results, `[[`, "block_summary"
    ))
    component_summary <- do.call(rbind, lapply(
        task_results, `[[`, "component_summary"
    ))
    terminal_audit <- do.call(rbind, lapply(
        task_results, `[[`, "terminal_audit"
    ))
    terminal_audit <- terminal_audit[
        match(config$tasks$task_id, terminal_audit$task_id), , drop = FALSE
    ]
    rownames(block_summary) <- NULL
    rownames(component_summary) <- NULL
    rownames(terminal_audit) <- NULL
    if (any(runtime$status != "ok") ||
        any(runtime$warning_count > 0L) ||
        any(runtime$paired_initialization_passed != TRUE) ||
        any(runtime$scientific_trace_reproduced != TRUE) ||
        any(runtime$conditional_rng_unchanged != TRUE) ||
        any(runtime$conditional_contract_passed != TRUE) ||
        any(runtime$terminal_state_contract_passed != TRUE) ||
        nrow(reconstructed_trace_audit) != nrow(config$tasks) ||
        any(reconstructed_trace_audit$conditional_contract_passed != TRUE) ||
        nrow(terminal_audit) != nrow(config$tasks) ||
        any(terminal_audit$exact_resume_interface_available != TRUE)) {
        stop(
            "Completed conditional-allocation tasks failed the final contract.",
            call. = FALSE
        )
    }
    for (method in config$methods) {
        for (seed_id in 1:2) {
            paired <- runtime$method == method &
                runtime$seed_id == seed_id
            if (sum(paired) != 2L ||
                length(unique(runtime$initial_theta_sha256[paired])) != 1L ||
                length(unique(runtime$initial_gamma_sha256[paired])) != 1L ||
                length(unique(runtime$initial_joint_sha256[paired])) != 1L ||
                length(unique(
                    runtime$initial_partition_sha256[paired]
                )) != 2L) {
                stop(
                    "A same-seed K = 10 pair failed initialization pairing.",
                    call. = FALSE
                )
            }
        }
    }

    run_stage <- "conditional-allocation-derived-diagnostics"
    calibration <- countdlm_road_conditional_allocation_calibration_rows(
        block_summary, config
    )
    method_diagnosis <- countdlm_road_conditional_allocation_method_rows(
        block_summary = block_summary,
        calibration = calibration,
        parent_decision = parent$result$method_decision,
        k6_decision = k6$method_decision,
        config = config
    )
    initialization_audit <- runtime[, c(
        "task_id", "method", "mode_id", "seed_id", "seed",
        "initial_partition_sha256", "initial_theta_sha256",
        "initial_gamma_sha256", "initial_joint_sha256",
        "paired_initialization_passed"
    )]

    countdlm_road_atomic_write_csv(
        runtime,
        file.path(output_dir, "conditional-allocation-runtime.csv")
    )
    countdlm_road_atomic_write_csv(
        initialization_audit,
        file.path(
            output_dir,
            "conditional-allocation-initialization-audit.csv"
        )
    )
    countdlm_road_atomic_write_csv(
        reconstructed_trace_audit,
        file.path(output_dir, "reconstructed-scientific-trace-audit.csv")
    )
    countdlm_road_atomic_write_csv(
        block_summary,
        file.path(output_dir, "conditional-allocation-block-summary.csv")
    )
    countdlm_road_atomic_write_csv(
        component_summary,
        file.path(
            output_dir,
            "conditional-allocation-component-block-summary.csv"
        )
    )
    countdlm_road_atomic_write_csv(
        calibration,
        file.path(output_dir, "conditional-allocation-calibration-z.csv")
    )
    countdlm_road_atomic_write_csv(
        method_diagnosis,
        file.path(output_dir, "conditional-allocation-method-diagnosis.csv")
    )
    countdlm_road_atomic_write_csv(
        terminal_audit,
        file.path(
            output_dir,
            "conditional-allocation-terminal-state-audit.csv"
        )
    )

    failed_tasks <- sum(runtime$status != "ok")
    warning_tasks <- sum(runtime$warning_count > 0L)
    all_initializations_paired <- all(
        runtime$paired_initialization_passed %in% TRUE
    )
    all_scientific_traces_reproduced <- all(
        reconstructed_trace_audit$scientific_trace_reproduced %in% TRUE
    )
    all_conditional_rng_unchanged <- all(
        reconstructed_trace_audit$conditional_rng_unchanged %in% TRUE
    )
    all_conditional_contracts_passed <- all(
        reconstructed_trace_audit$conditional_contract_passed %in% TRUE
    )
    all_terminal_states_ready <- all(
        runtime$terminal_state_contract_passed %in% TRUE
    )
    total_wall_seconds <- as.numeric(difftime(
        Sys.time(), started_at, units = "secs"
    ))
    result <- list(
        api_version = config$api_version,
        config = config,
        registration = registration,
        parent_partition = registration$parent_partition,
        k6_motivation = registration$k6_motivation,
        runtime = runtime,
        initialization_audit = initialization_audit,
        parent_trace_audit = parent_trace_audit,
        reconstructed_trace_audit = reconstructed_trace_audit,
        block_summary = block_summary,
        component_summary = component_summary,
        calibration = calibration,
        method_diagnosis = method_diagnosis,
        terminal_state_audit = terminal_audit,
        rule_specification =
            countdlm_road_conditional_allocation_rule_specification(config),
        pilot_wall_seconds = pilot_wall_seconds,
        total_wall_seconds_through_derived_tables = total_wall_seconds,
        truth_metrics_computed = FALSE,
        target_changed_from_parent = FALSE,
        all_tasks_ok = failed_tasks == 0L,
        failed_tasks = failed_tasks,
        warning_tasks = warning_tasks,
        all_initializations_paired = all_initializations_paired,
        all_parent_scientific_traces_verified = all(
            parent_trace_audit$parent_trace_verified
        ),
        all_scientific_traces_reproduced =
            all_scientific_traces_reproduced,
        all_conditional_rng_unchanged = all_conditional_rng_unchanged,
        all_conditional_contracts_passed =
            all_conditional_contracts_passed,
        all_terminal_states_ready = all_terminal_states_ready,
        ready_for_formal_design_review = FALSE,
        eligible_for_formal_freeze = FALSE,
        formal_simulation_launched = FALSE,
        requires_human_review = TRUE
    )
    result_path <- file.path(
        output_dir, "road-conditional-allocation-result.rds"
    )
    countdlm_road_atomic_save_rds(result, result_path)
    report <- c(
        "countDLM passive K=10 conditional-allocation diagnostic report",
        paste("API:", config$api_version),
        paste("Output:", output_dir),
        paste("Git HEAD:", git$head),
        paste("Git clean:", git$clean),
        paste("Context SHA-256:", observed_context_sha256),
        paste("Reviewed K10 parent ZIP SHA-256:", parent$zip_sha256),
        paste("Reviewed K6 motivation ZIP SHA-256:", k6$zip_sha256),
        paste("Fixed target K / basis rank / rho:",
              config$K_fit, "/", config$basis_m, "/",
              config$selected_rho),
        paste("Requested / actual workers:",
              config$workers, "/", actual_workers),
        paste("Reported physical cores:", reported_physical_cores),
        paste("Transitions / settling:",
              config$iterations, "/", config$burn),
        paste("Tasks / failures / warning tasks:",
              nrow(config$tasks), "/", failed_tasks, "/", warning_tasks),
        paste("All initializations paired exactly:",
              all_initializations_paired),
        paste("All reviewed parent traces verified:",
              result$all_parent_scientific_traces_verified),
        paste("All reconstructed scientific traces reproduced:",
              all_scientific_traces_reproduced),
        paste("All passive diagnostics left RNG unchanged:",
              all_conditional_rng_unchanged),
        paste("All conditional numerical contracts passed:",
              all_conditional_contracts_passed),
        paste("All terminal states continuation-ready:",
              all_terminal_states_ready),
        paste("Method-fit wall time:",
              countdlm_road_format_duration(pilot_wall_seconds)),
        paste("Total wall time through derived tables:",
              countdlm_road_format_duration(total_wall_seconds)),
        paste(
            "The diagnostic observes allocation probabilities immediately",
            "before the existing categorical draws and consumes no RNG."
        ),
        paste(
            "The K10 target, data, starts, seeds, rho, iteration count,",
            "and scientific trajectory are unchanged."
        ),
        paste(
            "The K6 archive supplies motivation only and never supplies",
            "the target, data, start, or chain state."
        ),
        paste(
            "Calibration screens are descriptive martingale-residual",
            "summaries, not iid tests or convergence proofs."
        ),
        "No outcome can authorize a formal simulation; all labels require human review.",
        "",
        "Method diagnostic labels:",
        utils::capture.output(print(method_diagnosis, row.names = FALSE))
    )
    report_path <- file.path(
        output_dir, "road-conditional-allocation-report.txt"
    )
    countdlm_road_atomic_write_lines(report, report_path)
    cat("\n", paste(report, collapse = "\n"), "\n", sep = "")

    run_stage <- "checksums-and-completion-marker"
    countdlm_road_assert_conditional_allocation_payload(
        output_dir, config, basename(source_files)
    )
    checksum_path <- countdlm_road_write_checksums(output_dir)
    completion_path <- file.path(output_dir, "RUN-COMPLETE.rds")
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            status = "complete-with-conditional-allocation-findings",
            completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
            failed_tasks = failed_tasks,
            warning_tasks = warning_tasks,
            all_initializations_paired = all_initializations_paired,
            all_parent_scientific_traces_verified =
                result$all_parent_scientific_traces_verified,
            all_scientific_traces_reproduced =
                all_scientific_traces_reproduced,
            all_conditional_rng_unchanged = all_conditional_rng_unchanged,
            all_conditional_contracts_passed =
                all_conditional_contracts_passed,
            all_terminal_states_ready = all_terminal_states_ready,
            target_changed_from_parent = FALSE,
            ready_for_formal_design_review = FALSE,
            truth_metrics_computed = FALSE,
            eligible_for_formal_freeze = FALSE,
            formal_simulation_launched = FALSE,
            requires_human_review = TRUE,
            checksums_cover_payload_before_this_marker = basename(
                checksum_path
            )
        ), completion_path
    )
    run_complete <- TRUE
    invisible(c(result, list(
        result_file = result_path,
        report_file = report_path,
        checksum_file = checksum_path,
        completion_file = completion_path
    )))
}
