# K = 6 target-sensitivity diagnostic following the reviewed K = 10
# partition-mode experiment.  This is a truth-blinded development experiment,
# not a formal simulation and not evidence for the registered K = 10 target.

countdlm_road_k6_diagnostic_api_version <-
    "countdlm-road-k6-diagnostic-2026-09-03-v1"
countdlm_road_k6_parent_api_version <-
    "countdlm-road-partition-diagnostic-2026-09-03-v1"
countdlm_road_k6_parent_zip_sha256 <-
    "4fc43b7ddb4a894e05d0cbd06516dd3431e178c7d9fc66dbfd322746e884029d"
countdlm_road_k6_parent_manifest_sha256 <-
    "ca4891c535e3378fbc86190f6131986c132f25904d16fdc2f6695f26f30df584"
countdlm_road_k6_parent_result_sha256 <-
    "ae09e78cabffc2db07caad6e2a09407f70c3c30684b4ece46e50ad59ed600674"
countdlm_road_k6_parent_completion_sha256 <-
    "8d5f48651637d65a52128f8bd2b8cab0c0366ce7933ca619fac3956a8e754e32"
countdlm_road_k6_parent_registration_sha256 <-
    "d6006a796c6f584ad4ff3e1f4c8d56456f53d73537795d4013e07a8b334d4305"
countdlm_road_k6_parent_blinded_sha256 <-
    "429a45df3780469e5dc03a092035a2ce10e1d5879fb3eabe26a5326397b6015e"
countdlm_road_k6_parent_config_signature <-
    "4abf00ec21be8eda45a9793a4759d766df307e47028f773f1384057d4122317f"
countdlm_road_k6_neutral_theta_sha256 <-
    "dea7bf3d22404559b20847d7fc4e0cf9d5e10e857c7f6f0fdb6b53d13ebbfb01"
countdlm_road_k6_neutral_gamma_sha256 <-
    "5b3710efaf4a6765b3c0c6d7bdf83ff4ab0a434b064da48527a2ff17cc9c3ea9"
countdlm_road_k6_neutral_joint_sha256 <-
    "bddc48b604598a56af579ec7a0f2ad82a2092f14e64fb2e77022eb6581601d5f"
countdlm_road_k6_methods <- c("GMDE-W", "GMDE-C", "Euc-MDE")

countdlm_road_k6_mode_specification <- function() {
    source <- countdlm_road_partition_mode_specification()
    data.frame(
        method = source$method,
        mode_id = source$mode_id,
        k10_source_task_id = paste0(
            source$method, "-mode-", source$mode_id, "-seed-1"
        ),
        occupied_at_start = source$occupied_experts,
        substantive_at_start = source$substantive_experts,
        partition_sha256 = source$partition_sha256,
        admissible_under_K6 = source$occupied_experts <= 6L,
        stringsAsFactors = FALSE
    )
}

countdlm_road_k6_tasks <- function() {
    rows <- list()
    for (seed_id in 1:2) {
        for (method in countdlm_road_k6_methods) {
            for (mode_id in c("A", "B")) {
                seed_name <- paste(method, "seed", seed_id, sep = "-")
                rows[[length(rows) + 1L]] <- data.frame(
                    task_id = paste(
                        method, "K6", paste0("mode-", mode_id),
                        paste0("seed-", seed_id), sep = "-"
                    ),
                    method = method,
                    mode_id = mode_id,
                    seed_id = seed_id,
                    seed = unname(
                        countdlm_road_partition_task_seeds[[seed_name]]
                    ),
                    batch_id = seed_id,
                    stringsAsFactors = FALSE
                )
            }
        }
    }
    value <- do.call(rbind, rows)
    specification <- countdlm_road_k6_mode_specification()
    key <- paste(value$method, value$mode_id, sep = "|")
    specification_key <- paste(
        specification$method, specification$mode_id, sep = "|"
    )
    value$parent_partition_sha256 <- specification$partition_sha256[
        match(key, specification_key)
    ]
    rownames(value) <- NULL
    value
}

countdlm_road_k6_validate_parent <- function(
    result, completion, registration, blinded
) {
    if (!is.list(result) ||
        !identical(result$api_version,
                   countdlm_road_k6_parent_api_version) ||
        !is.list(result$config) ||
        !identical(result$config$config_signature,
                   countdlm_road_k6_parent_config_signature) ||
        !identical(result$config$K_fit, 10L) ||
        !identical(result$config$basis_m, 40L) ||
        !identical(result$config$selected_rho, 1) ||
        !identical(result$config$iterations, 3000L) ||
        !identical(result$config$burn, 1000L) ||
        !identical(result$config$methods, countdlm_road_k6_methods) ||
        !is.data.frame(result$runtime) || nrow(result$runtime) != 12L ||
        any(result$runtime$status != "ok") ||
        any(result$runtime$warning_count != 0L) ||
        !isTRUE(result$all_tasks_ok) ||
        !identical(as.integer(result$failed_tasks), 0L) ||
        !identical(as.integer(result$warning_tasks), 0L) ||
        !isTRUE(result$all_initializations_paired) ||
        !identical(result$all_methods_resolved_under_control, FALSE) ||
        !identical(result$ready_for_formal_design_review, FALSE) ||
        !identical(result$truth_metrics_computed, FALSE) ||
        !identical(result$eligible_for_formal_freeze, FALSE) ||
        !identical(result$formal_simulation_launched, FALSE) ||
        !is.data.frame(result$method_decision) ||
        !identical(result$method_decision$method,
                   countdlm_road_k6_methods) ||
        any(result$method_decision$resolved_under_control != FALSE) ||
        any(result$method_decision$formal_run_authorized != FALSE)) {
        stop("The K = 10 parent result is not the reviewed diagnostic.",
             call. = FALSE)
    }
    if (!is.list(completion) ||
        !identical(completion$api_version,
                   countdlm_road_k6_parent_api_version) ||
        !identical(completion$status,
                   "complete-with-diagnostic-findings") ||
        !identical(as.integer(completion$failed_tasks), 0L) ||
        !identical(as.integer(completion$warning_tasks), 0L) ||
        !isTRUE(completion$all_initializations_paired) ||
        !identical(completion$all_methods_resolved_under_control, FALSE) ||
        !identical(completion$ready_for_formal_design_review, FALSE) ||
        !identical(completion$truth_metrics_computed, FALSE) ||
        !identical(completion$eligible_for_formal_freeze, FALSE) ||
        !identical(completion$formal_simulation_launched, FALSE) ||
        !identical(completion$checksums_cover_payload_before_this_marker,
                   "CHECKSUMS.sha256")) {
        stop("The K = 10 parent completion marker is invalid.",
             call. = FALSE)
    }
    if (!is.list(registration) ||
        !identical(registration, result$registration) ||
        !identical(registration$config$config_signature,
                   countdlm_road_k6_parent_config_signature) ||
        !is.character(registration$source_sha256) ||
        !length(registration$source_sha256)) {
        stop("The K = 10 parent registration is invalid.", call. = FALSE)
    }
    expected_names <- paste(
        rep(countdlm_road_k6_methods, each = 2L),
        rep(c("A", "B"), times = 3L), sep = "|"
    )
    if (!is.list(blinded) ||
        !identical(blinded$api_version,
                   countdlm_road_k6_parent_api_version) ||
        !identical(blinded$Y_Fmat_sha256,
                   countdlm_road_stability_data_sha256) ||
        !identical(blinded$truth_fields_stored, FALSE) ||
        !identical(blinded$truth_metrics_computed, FALSE) ||
        !is.matrix(blinded$Y) ||
        !identical(dim(blinded$Y), c(100L, 168L)) ||
        !is.matrix(blinded$Fmat) ||
        !identical(dim(blinded$Fmat), c(168L, 2L)) ||
        !is.list(blinded$modes) ||
        !identical(names(blinded$modes), expected_names)) {
        stop("The K = 10 parent blinded payload is invalid.",
             call. = FALSE)
    }
    specification <- countdlm_road_k6_mode_specification()
    observed_hash <- vapply(
        blinded$modes, countdlm_road_partition_hash, character(1)
    )
    expected_hash <- stats::setNames(
        specification$partition_sha256, expected_names
    )
    admissible <- vapply(blinded$modes, function(partition) {
        identical(
            as.integer(partition),
            countdlm_road_canonical_partition(partition)
        ) && length(partition) == 100L && min(partition) == 1L &&
            max(partition) <= 6L
    }, logical(1))
    if (!identical(observed_hash, expected_hash) || !all(admissible) ||
        !all(specification$admissible_under_K6)) {
        stop("A reviewed K = 10 start is not admissible under K = 6.",
             call. = FALSE)
    }
    invisible(TRUE)
}

countdlm_road_k6_parent_evidence <- function(zip_file) {
    zip_file <- normalizePath(zip_file, winslash = "/", mustWork = TRUE)
    if (!identical(countdlm_road_sha256(zip_file),
                   countdlm_road_k6_parent_zip_sha256)) {
        stop("The K = 10 parent ZIP does not match the reviewed archive.",
             call. = FALSE)
    }
    members <- c(
        manifest = "CHECKSUMS.sha256",
        result = "road-partition-diagnostic-result.rds",
        completion = "RUN-COMPLETE.rds",
        registration = "road-partition-diagnostic-registration.rds",
        blinded = "road-partition-diagnostic-blinded-input.rds"
    )
    raw_members <- lapply(members, function(member) {
        countdlm_road_stability_zip_member_raw(zip_file, member)
    })
    observed <- vapply(
        raw_members, digest::digest, character(1),
        algo = "sha256", serialize = FALSE
    )
    expected <- c(
        manifest = countdlm_road_k6_parent_manifest_sha256,
        result = countdlm_road_k6_parent_result_sha256,
        completion = countdlm_road_k6_parent_completion_sha256,
        registration = countdlm_road_k6_parent_registration_sha256,
        blinded = countdlm_road_k6_parent_blinded_sha256
    )
    if (!identical(observed, expected)) {
        stop("A K = 10 parent member failed its SHA-256 check.",
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
    countdlm_road_k6_validate_parent(
        result, completion, registration, blinded
    )
    list(
        zip_file = zip_file,
        zip_sha256 = countdlm_road_k6_parent_zip_sha256,
        manifest_sha256 = observed[["manifest"]],
        result_sha256 = observed[["result"]],
        completion_sha256 = observed[["completion"]],
        registration_sha256 = observed[["registration"]],
        blinded_sha256 = observed[["blinded"]],
        execution_environment = registration$execution_environment,
        source_sha256 = registration$source_sha256,
        result = result,
        completion = completion,
        blinded = blinded,
        modes = blinded$modes
    )
}

countdlm_road_k6_neutral_initialization <- function(Y, Fmat) {
    Y <- as.matrix(Y)
    Fmat <- as.matrix(Fmat)
    if (!identical(dim(Y), c(100L, 168L)) ||
        !identical(dim(Fmat), c(168L, 2L)) ||
        any(!is.finite(Y)) || any(Y < 0) || any(Y != round(Y)) ||
        any(!is.finite(Fmat))) {
        stop("The K = 6 neutral start requires registered blinded data.",
             call. = FALSE)
    }
    theta <- array(0, c(6L, 168L, 2L))
    theta[, , 1L] <- log(mean(Y) + 0.1)
    gamma <- matrix(0, 40L, 5L)
    observed <- c(
        theta = digest::digest(theta, algo = "sha256", serialize = TRUE),
        gamma = digest::digest(gamma, algo = "sha256", serialize = TRUE),
        joint = digest::digest(
            list(theta = theta, gamma = gamma),
            algo = "sha256", serialize = TRUE
        )
    )
    expected <- c(
        theta = countdlm_road_k6_neutral_theta_sha256,
        gamma = countdlm_road_k6_neutral_gamma_sha256,
        joint = countdlm_road_k6_neutral_joint_sha256
    )
    if (!identical(observed, expected)) {
        stop("The registered K = 6 neutral-start hash does not match.",
             call. = FALSE)
    }
    list(theta = theta, gamma = gamma, sha256 = observed)
}

#' Construct the K = 6 target-sensitivity diagnostic configuration
#'
#' This experiment preserves the reviewed K = 10 diagnostic's data, graph
#' bases, starts, seeds, rho, run length, and thresholds while changing only
#' the fitted upper bound from K = 10 to K = 6.  Its results cannot validate
#' or replace the K = 10 scientific target.
#'
#' @param context_file Approved external D-017 road-context RDS.
#' @param parent_partition_zip Exact returned K = 10 partition-diagnostic ZIP.
#' @param output_dir Brand-new external output directory.
#' @param execution_source_dir Directory containing frozen `R/*.R` sources.
#' @return A signed, fixed diagnostic configuration.
#' @export
countdlm_road_k6_diagnostic_config <- function(
    context_file, parent_partition_zip, output_dir,
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
        api_version = countdlm_road_k6_diagnostic_api_version,
        sampler_version = countdlm_gmde_sampler_version,
        scientific_role = paste(
            "truth-blinded K=6 target-sensitivity mechanism diagnostic;",
            "not an inferential, tuning-selection, or formal simulation"
        ),
        context_file = normalizePath(
            context_file, winslash = "/", mustWork = TRUE
        ),
        context_sha256 = countdlm_road_context_sha256,
        parent_partition_zip = normalizePath(
            parent_partition_zip, winslash = "/", mustWork = TRUE
        ),
        parent_api_version = countdlm_road_k6_parent_api_version,
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
        output_dir = normalizePath(
            output_dir, winslash = "/", mustWork = FALSE
        ),
        execution_source_dir = execution_source_dir,
        source_sha256 = source_sha256,
        n = 100L,
        TT = 168L,
        K_fit = 6L,
        parent_K_fit = 10L,
        basis_m = 40L,
        methods = countdlm_road_k6_methods,
        mode_specification = countdlm_road_k6_mode_specification(),
        tasks = countdlm_road_k6_tasks(),
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
        log_rhat_limit = 1.10,
        count_rhat_limit = 1.20,
        psm_rms_limit = 0.10,
        psm_mean_abs_limit = 0.05,
        start_distance_margin = 0.05,
        persistence_ratio = 0.75,
        macro_tv_limit = 0.50,
        macro_dominant_mass = 0.75,
        macro_minimum_run = 250L,
        macro_loglik_separation = 1.0,
        literal_freeze_fraction = 0.98,
        data_sha256 = countdlm_road_stability_data_sha256,
        basis_sha256 = countdlm_road_partition_basis_sha256,
        neutral_theta_sha256 = countdlm_road_k6_neutral_theta_sha256,
        neutral_gamma_sha256 = countdlm_road_k6_neutral_gamma_sha256,
        neutral_joint_sha256 = countdlm_road_k6_neutral_joint_sha256,
        state_G = diag(2),
        state_W = diag(c(1e-6, 5e-7)),
        state_C0 = diag(c(2, 1)),
        substantive_min = 5L,
        pg_backend = "devroye-exact",
        algorithm_exact = TRUE,
        classification_only = TRUE,
        target_changed_from_parent = TRUE,
        truth_metrics_computed = FALSE,
        formal_results_authorized = FALSE
    )
    config$config_signature <- countdlm_road_config_signature(config)
    config <- structure(
        config, class = "countdlm_road_k6_diagnostic_config"
    )
    countdlm_road_validate_k6_diagnostic_config(config)
    config
}

countdlm_road_validate_k6_diagnostic_config <- function(config) {
    valid_signature <- inherits(
        config, "countdlm_road_k6_diagnostic_config"
    ) && identical(
        config$api_version, countdlm_road_k6_diagnostic_api_version
    ) && is.character(config$config_signature) &&
        length(config$config_signature) == 1L && identical(
            config$config_signature, countdlm_road_config_signature(config)
        )
    if (!isTRUE(valid_signature) ||
        !identical(config$sampler_version,
                   countdlm_gmde_sampler_version) ||
        !identical(config$parent_api_version,
                   countdlm_road_k6_parent_api_version) ||
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
        !identical(config$context_sha256, countdlm_road_context_sha256) ||
        !identical(config$data_sha256,
                   countdlm_road_stability_data_sha256) ||
        !identical(config$n, 100L) || !identical(config$TT, 168L) ||
        !identical(config$K_fit, 6L) ||
        !identical(config$parent_K_fit, 10L) ||
        !identical(config$basis_m, 40L) ||
        !identical(config$methods, countdlm_road_k6_methods) ||
        !identical(config$mode_specification,
                   countdlm_road_k6_mode_specification()) ||
        !identical(config$tasks, countdlm_road_k6_tasks()) ||
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
        !identical(config$log_rhat_limit, 1.10) ||
        !identical(config$count_rhat_limit, 1.20) ||
        !identical(config$psm_rms_limit, 0.10) ||
        !identical(config$psm_mean_abs_limit, 0.05) ||
        !identical(config$start_distance_margin, 0.05) ||
        !identical(config$persistence_ratio, 0.75) ||
        !identical(config$macro_tv_limit, 0.50) ||
        !identical(config$macro_dominant_mass, 0.75) ||
        !identical(config$macro_minimum_run, 250L) ||
        !identical(config$macro_loglik_separation, 1.0) ||
        !identical(config$literal_freeze_fraction, 0.98) ||
        !identical(config$basis_sha256,
                   countdlm_road_partition_basis_sha256) ||
        !identical(config$neutral_theta_sha256,
                   countdlm_road_k6_neutral_theta_sha256) ||
        !identical(config$neutral_gamma_sha256,
                   countdlm_road_k6_neutral_gamma_sha256) ||
        !identical(config$neutral_joint_sha256,
                   countdlm_road_k6_neutral_joint_sha256) ||
        !identical(config$state_G, diag(2)) ||
        !identical(config$state_W, diag(c(1e-6, 5e-7))) ||
        !identical(config$state_C0, diag(c(2, 1))) ||
        !identical(config$substantive_min, 5L) ||
        !identical(config$pg_backend, "devroye-exact") ||
        !isTRUE(config$algorithm_exact) ||
        !isTRUE(config$classification_only) ||
        !isTRUE(config$target_changed_from_parent) ||
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
            "The K = 6 diagnostic configuration is invalid or modified.",
            call. = FALSE
        )
    }
    invisible(config)
}

countdlm_road_k6_output_tables <- function() {
    c(
        rule_specification = "k6-diagnostic-rule-spec.csv",
        block_summary = "k6-partition-block-summary.csv",
        pairwise_psm = "k6-partition-pairwise-psm.csv",
        pair_persistence = "k6-partition-pair-persistence.csv",
        within_chain_drift = "k6-partition-within-chain-drift.csv",
        start_memory = "k6-partition-start-memory.csv",
        scalar_diagnostics = "k6-partition-scalar-diagnostics.csv",
        cluster_count_frequency = "k6-partition-count-frequency.csv",
        late_representative_partition =
            "k6-partition-late-representative.csv",
        node_psm_difference = "k6-partition-node-psm-difference.csv",
        macro_pairs = "k6-partition-macro-pairs.csv",
        method_decision = "k6-partition-method-decision.csv"
    )
}

countdlm_road_k6_expected_payload <- function(config, source_names) {
    slugs <- vapply(seq_len(nrow(config$tasks)), function(index) {
        countdlm_road_partition_task_slug(
            index, config$tasks$task_id[[index]]
        )
    }, character(1))
    sort(c(
        "FROZEN_SOURCE_SHA256.csv",
        "road-k6-diagnostic-registration.rds",
        "RUN-STARTED.rds",
        "road-k6-diagnostic-blinded-input.rds",
        "k6-mode-specification.csv",
        "k6-diagnostic-runtime.csv",
        "k6-paired-initialization-audit.csv",
        unname(countdlm_road_k6_output_tables()),
        "k6-count-mechanics-summary.csv",
        "k6-count-transition-matrix.csv",
        "k6-count-dwell-runs.csv",
        "k6-vs-k10-count-mechanics.csv",
        "k6-vs-k10-method-screen.csv",
        "k6-terminal-state-audit.csv",
        "road-k6-diagnostic-result.rds",
        "road-k6-diagnostic-report.txt",
        file.path("chains", paste0(slugs, "-fit.rds")),
        file.path("diagnostics", paste0(slugs, "-diagnostic.rds")),
        file.path(
            "terminal-states", paste0(slugs, "-terminal-state.rds")
        ),
        file.path("frozen-source", "R", source_names)
    ))
}

countdlm_road_assert_k6_payload <- function(
    output_dir, config, source_names
) {
    expected <- countdlm_road_k6_expected_payload(config, source_names)
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
            "The K = 6 payload failed its exact inventory audit. Missing: ",
            paste(missing, collapse = ", "), "; unexpected: ",
            paste(unexpected, collapse = ", "), ".", call. = FALSE
        )
    }
    invisible(expected)
}

countdlm_road_assert_k6_output <- function(
    output_dir, directories, config_signature, stage
) {
    required <- unique(c(output_dir, directories))
    registration_path <- file.path(
        output_dir, "road-k6-diagnostic-registration.rds"
    )
    started_path <- file.path(output_dir, "RUN-STARTED.rds")
    missing <- c(
        required[!dir.exists(required)],
        c(registration_path, started_path)[
            !file.exists(c(registration_path, started_path))
        ]
    )
    if (length(missing)) {
        stop(
            "The registered K = 6 output tree is unavailable during ",
            stage, ". Missing: ", paste(missing, collapse = ", "),
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
            "The K = 6 output identity failed during ", stage, ".",
            call. = FALSE
        )
    }
    invisible(TRUE)
}

countdlm_road_k6_validate_terminal_state <- function(
    fit, starting_partition, config, method
) {
    required_traces <- c(
        "occupied_births", "occupied_deaths",
        "substantive_upcrossings", "substantive_downcrossings"
    )
    state <- fit$sampler_terminal_state
    if (!is.list(state) ||
        !all(c(
            "model", "completed_iterations", "Z", "theta", "classifier",
            "rng_state", "rng_kind", "n", "TT", "p", "K", "m", "rho",
            "sampler_version"
        ) %in% names(state)) ||
        !all(required_traces %in% names(fit)) ||
        !identical(state$model, "gmde") ||
        !identical(state$completed_iterations, config$iterations) ||
        !identical(state$n, config$n) ||
        !identical(state$TT, config$TT) ||
        !identical(state$p, 2L) ||
        !identical(state$K, config$K_fit) ||
        !identical(state$m, config$basis_m) ||
        !identical(state$rho, config$selected_rho) ||
        !identical(state$sampler_version, config$sampler_version) ||
        !identical(dim(state$theta), c(6L, 168L, 2L)) ||
        !identical(dim(state$classifier), c(40L, 5L)) ||
        length(state$Z) != 100L || any(state$Z < 1L | state$Z > 6L) ||
        !is.numeric(state$rng_state) || length(state$rng_state) < 2L ||
        any(!is.finite(state$rng_state)) ||
        !isTRUE(fit$settings$sampler_terminal_state_stored)) {
        stop("A retained K = 6 terminal state is not continuation-ready.",
             call. = FALSE)
    }
    initial_Kocc <- length(unique(starting_partition))
    initial_Ksub <- sum(tabulate(
        starting_partition, nbins = config$K_fit
    ) >= config$substantive_min)
    Kocc_delta <- diff(c(initial_Kocc, fit$occupied_experts))
    Ksub_delta <- diff(c(initial_Ksub, fit$substantive_experts))
    occupied_identity <- identical(
        as.integer(Kocc_delta),
        as.integer(fit$occupied_births - fit$occupied_deaths)
    )
    substantive_identity <- identical(
        as.integer(Ksub_delta),
        as.integer(
            fit$substantive_upcrossings - fit$substantive_downcrossings
        )
    )
    if (!occupied_identity || !substantive_identity) {
        stop("The K = 6 gross birth/death identity failed.", call. = FALSE)
    }
    list(
        state = state,
        audit = data.frame(
            method = method,
            completed_iterations = state$completed_iterations,
            Z_sha256 = digest::digest(
                state$Z, algo = "sha256", serialize = TRUE
            ),
            theta_sha256 = digest::digest(
                state$theta, algo = "sha256", serialize = TRUE
            ),
            classifier_sha256 = digest::digest(
                state$classifier, algo = "sha256", serialize = TRUE
            ),
            rng_state_sha256 = digest::digest(
                state$rng_state, algo = "sha256", serialize = TRUE
            ),
            joint_state_sha256 = digest::digest(
                state, algo = "sha256", serialize = TRUE
            ),
            occupied_identity_passed = occupied_identity,
            substantive_identity_passed = substantive_identity,
            exact_resume_interface_available = TRUE,
            stringsAsFactors = FALSE
        )
    )
}

countdlm_road_k6_count_rows <- function(
    fit, task, config, fitted_K, source_label
) {
    summary_rows <- list()
    transition_rows <- list()
    dwell_rows <- list()
    for (block_name in names(config$blocks)) {
        index <- config$blocks[[block_name]]
        for (count_type in c("Kocc", "Ksub")) {
            full_trace <- if (count_type == "Kocc") {
                fit$occupied_experts
            } else fit$substantive_experts
            trace <- as.integer(full_trace[index])
            difference <- diff(trace)
            pair <- data.frame(
                from_count = trace[-length(trace)],
                to_count = trace[-1L]
            )
            transition <- as.data.frame(table(
                pair$from_count, pair$to_count,
                dnn = c("from_count", "to_count")
            ), stringsAsFactors = FALSE)
            transition <- transition[transition$Freq > 0L, , drop = FALSE]
            transition$from_count <- as.integer(
                as.character(transition$from_count)
            )
            transition$to_count <- as.integer(
                as.character(transition$to_count)
            )
            row_total <- rowsum(
                transition$Freq, transition$from_count, reorder = FALSE
            )
            transition$row_probability <- transition$Freq /
                row_total[match(
                    transition$from_count,
                    as.integer(rownames(row_total))
                ), 1L]
            transition_rows[[length(transition_rows) + 1L]] <- data.frame(
                source = source_label,
                fitted_K = fitted_K,
                task_id = task$task_id,
                method = task$method,
                mode_id = task$mode_id,
                seed_id = task$seed_id,
                block = block_name,
                count_type = count_type,
                from_count = transition$from_count,
                to_count = transition$to_count,
                transition_count = transition$Freq,
                row_probability = transition$row_probability,
                stringsAsFactors = FALSE
            )
            runs <- rle(trace)
            run_end_local <- cumsum(runs$lengths)
            run_start_local <- run_end_local - runs$lengths + 1L
            run_start <- index[run_start_local]
            run_end <- index[run_end_local]
            left_continues <- run_start > 1L &
                full_trace[pmax(1L, run_start - 1L)] == runs$values
            right_continues <- run_end < length(full_trace) &
                full_trace[pmin(length(full_trace), run_end + 1L)] ==
                    runs$values
            dwell_rows[[length(dwell_rows) + 1L]] <- data.frame(
                source = source_label,
                fitted_K = fitted_K,
                task_id = task$task_id,
                method = task$method,
                mode_id = task$mode_id,
                seed_id = task$seed_id,
                block = block_name,
                count_type = count_type,
                run_id = seq_along(runs$lengths),
                count_value = runs$values,
                first_iteration = run_start,
                last_iteration = run_end,
                dwell_length = runs$lengths,
                left_continues_outside_block = left_continues,
                right_continues_outside_block = right_continues,
                stringsAsFactors = FALSE
            )
            gross_up <- gross_down <- rep(NA_integer_, length(index))
            if (fitted_K == 6L) {
                if (count_type == "Kocc") {
                    gross_up <- fit$occupied_births[index]
                    gross_down <- fit$occupied_deaths[index]
                } else {
                    gross_up <- fit$substantive_upcrossings[index]
                    gross_down <- fit$substantive_downcrossings[index]
                }
            }
            summary_rows[[length(summary_rows) + 1L]] <- data.frame(
                source = source_label,
                fitted_K = fitted_K,
                task_id = task$task_id,
                method = task$method,
                mode_id = task$mode_id,
                seed_id = task$seed_id,
                block = block_name,
                count_type = count_type,
                draws = length(trace),
                distinct_count_values = length(unique(trace)),
                minimum_count = min(trace),
                maximum_count = max(trace),
                net_change = trace[[length(trace)]] - trace[[1L]],
                adjacent_change_count = sum(difference != 0L),
                adjacent_change_fraction = mean(difference != 0L),
                upward_net_transitions = sum(difference > 0L),
                downward_net_transitions = sum(difference < 0L),
                mean_absolute_net_jump = mean(abs(difference)),
                maximum_dwell = max(runs$lengths),
                gross_up_events = if (all(is.na(gross_up))) {
                    NA_integer_
                } else sum(gross_up),
                gross_down_events = if (all(is.na(gross_down))) {
                    NA_integer_
                } else sum(gross_down),
                gross_event_source = if (fitted_K == 6L) {
                    "allocation-sweep pre/post expert occupancy"
                } else "unavailable in frozen K10 parent",
                stringsAsFactors = FALSE
            )
        }
    }
    list(
        summary = do.call(rbind, summary_rows),
        transitions = do.call(rbind, transition_rows),
        dwell = do.call(rbind, dwell_rows)
    )
}

countdlm_road_k6_parent_count_mechanics <- function(parent, config) {
    rows <- list()
    for (index in seq_len(nrow(parent$result$runtime))) {
        parent_task <- parent$result$config$tasks[index, , drop = FALSE]
        member <- basename(parent$result$runtime$fit_file[[index]])
        wrapper <- countdlm_road_stability_read_rds_raw(
            countdlm_road_stability_zip_member_raw(
                parent$zip_file, member
            ), member
        )
        if (!is.list(wrapper) ||
            !identical(wrapper$api_version,
                       countdlm_road_k6_parent_api_version) ||
            !countdlm_road_truth_blinding_ok(wrapper$fit, 3000L)) {
            stop("A K = 10 parent chain failed comparison validation.",
                 call. = FALSE)
        }
        comparison_task <- data.frame(
            task_id = paste0(
                parent_task$method, "-K10-mode-", parent_task$mode_id,
                "-seed-", parent_task$seed_id
            ),
            method = parent_task$method,
            mode_id = parent_task$mode_id,
            seed_id = parent_task$seed_id,
            stringsAsFactors = FALSE
        )
        rows[[index]] <- countdlm_road_k6_count_rows(
            wrapper$fit, comparison_task, config, 10L,
            "reviewed-K10-parent"
        )$summary
        rm(wrapper)
        invisible(gc(verbose = FALSE))
    }
    value <- do.call(rbind, rows)
    rownames(value) <- NULL
    value
}

#' Run the K = 6 target-sensitivity diagnostic
#'
#' The runner verifies the exact returned K = 10 parent archive and approved
#' D-017 context before creating a new external output.  It runs twelve
#' truth-blinded chains in two guarded batches.  It cannot authorize a formal
#' simulation regardless of its findings.
#'
#' @param config Output of `countdlm_road_k6_diagnostic_config()`.
#' @param repository_root Any path inside the Git repository recorded as
#'   provenance.
#' @return Diagnostic summaries and immutable output paths.
#' @export
countdlm_road_k6_diagnostic_pilot <- function(config, repository_root) {
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

    countdlm_road_validate_k6_diagnostic_config(config)
    if (!requireNamespace("BayesLogit", quietly = TRUE) ||
        !requireNamespace("digest", quietly = TRUE) ||
        !requireNamespace("posterior", quietly = TRUE)) {
        stop(
            "The K = 6 diagnostic requires BayesLogit, digest, and posterior.",
            call. = FALSE
        )
    }
    parent <- countdlm_road_k6_parent_evidence(
        config$parent_partition_zip
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
            "The current R environment differs from the K = 10 parent for: ",
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
    neutral <- countdlm_road_k6_neutral_initialization(
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
        stop("The K = 6 starts failed their registered hash audit.",
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
        stop("Could not create the registered K = 6 output directory.",
             call. = FALSE)
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
        target_changed_from_parent = TRUE,
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
        diagnostic_software = list(
            posterior_version = as.character(
                utils::packageVersion("posterior")
            )
        ),
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
            fitted_K = config$parent_K_fit
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
        config = unclass(config),
        note = paste(
            "Only K_fit changes from 10 to 6. This changes the posterior",
            "target, cannot validate K=10, uses no truth, and cannot",
            "authorize the formal simulation."
        )
    )
    countdlm_road_atomic_save_rds(
        registration,
        file.path(output_dir, "road-k6-diagnostic-registration.rds")
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
            stop("Could not create K = 6 diagnostic subdirectories.",
                 call. = FALSE)
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
            parent_K_fit = config$parent_K_fit,
            modes = modes,
            mode_specification = config$mode_specification,
            neutral_theta = neutral$theta,
            neutral_gamma = neutral$gamma,
            neutral_initialization_sha256 = neutral$sha256
        ), file.path(output_dir, "road-k6-diagnostic-blinded-input.rds")
    )
    countdlm_road_atomic_write_csv(
        config$mode_specification,
        file.path(output_dir, "k6-mode-specification.csv")
    )
    countdlm_road_assert_k6_output(
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
        worker_stage <- "k6-fit"
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
                store_sampler_terminal_state = TRUE
            )
            elapsed <- as.numeric(difftime(
                Sys.time(), task_started, units = "secs"
            ))
            worker_stage <- "k6-contract"
            if (!countdlm_road_truth_blinding_ok(
                fit, config$iterations
            )) {
                stop("The K = 6 fit failed strict truth blinding.",
                     call. = FALSE)
            }
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
                stop("The K = 6 paired-initialization contract failed.",
                     call. = FALSE)
            }
            terminal <- countdlm_road_k6_validate_terminal_state(
                fit, starting_partition, config, method
            )
            fixed_rho_audit <- countdlm_road_fixed_rho_calibration(
                fit = fit, settle = config$burn,
                score = config$iterations - config$burn
            )
            worker_stage <- "k6-save-fit"
            countdlm_road_atomic_save_rds(
                list(
                    api_version = config$api_version,
                    parent_partition_result_sha256 =
                        config$parent_result_sha256,
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
                    warnings_before_fit_retention = warning_messages,
                    fit = fit
                ), fit_path
            )
            worker_stage <- "k6-save-terminal"
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
                    sampler_state = terminal$state
                ), terminal_path
            )
            terminal$audit$task_id <- task_id
            terminal$audit$mode_id <- mode_id
            terminal$audit$seed_id <- seed_id
            terminal$audit$seed <- seed
            terminal$audit$terminal_state_file <- terminal_relative
            terminal$audit$terminal_state_file_sha256 <-
                countdlm_road_sha256(terminal_path)
            worker_stage <- "k6-summarize"
            summarized <- countdlm_road_summarize_fit(
                fit = fit, task_id = task_id, method = method,
                potts_beta = NA_real_, burn = config$burn,
                elapsed = elapsed, warnings = warning_messages
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
            summarized$summary$terminal_state_contract_passed <- TRUE
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
            summarized$compact$terminal_state_audit <- terminal$audit
            countdlm_road_atomic_save_rds(
                summarized$compact, diagnostic_path
            )
            summarized$summary$warning_count <- length(warning_messages)
            summarized$summary$warnings <- if (length(warning_messages)) {
                paste(warning_messages, collapse = " | ")
            } else NA_character_
            list(summary = summarized$summary,
                 terminal_audit = terminal$audit)
        }, error = function(error) {
            elapsed <- as.numeric(difftime(
                Sys.time(), task_started, units = "secs"
            ))
            summary <- countdlm_road_failure_summary(
                task_id = task_id, method = method,
                potts_beta = NA_real_, elapsed = elapsed,
                config = list(
                    quick_iterations = config$iterations,
                    quick_burn = config$burn,
                    rho_timing = config$selected_rho
                ), error = conditionMessage(error), stage = worker_stage,
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
                    phase = "k6-target-sensitivity-diagnostic",
                    task_id = task_id, method = method,
                    mode_id = mode_id, seed_id = seed_id, seed = seed,
                    status = "error", elapsed_seconds = elapsed,
                    stage = worker_stage,
                    error = conditionMessage(error),
                    call = paste(deparse(conditionCall(error)), collapse = " "),
                    warnings = warning_messages,
                    completed_fit_retained = file.exists(fit_path),
                    terminal_state_retained = file.exists(terminal_path),
                    summary = summary
                ), failure_path
            )
            list(
                summary = summary,
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
        "K=6 target-sensitivity diagnostic: 3 methods x 2 starts x 2 seeds\n",
        config$iterations, " transitions / ", config$burn,
        " burn; m=", config$basis_m, "; fixed rho=",
        config$selected_rho, "\n",
        "Two guarded batches of six single-thread workers; about ",
        config$reserved_reported_cores, " reported cores remain unused.\n",
        "Only K changes from the reviewed K=10 parent; no truth and no formal run.\n",
        sep = ""
    )
    run_stage <- "k6-chain-batches"
    pilot_started <- Sys.time()
    task_results <- vector("list", nrow(config$tasks))
    batches <- sort(unique(config$tasks$batch_id))
    for (batch_id in batches) {
        indices <- which(config$tasks$batch_id == batch_id)
        countdlm_road_assert_k6_output(
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
            ), cores = actual_workers,
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
                method = task$method[[1L]], potts_beta = NA_real_,
                elapsed = error$elapsed_seconds,
                config = list(
                    quick_iterations = config$iterations,
                    quick_burn = config$burn,
                    rho_timing = config$selected_rho
                ), error = error$message,
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
            summary$terminal_state_contract_passed <- FALSE
            summary$ari_computed <- FALSE
            summary$acc_computed <- FALSE
            summary$fit_file <- NA_character_
            summary$terminal_state_file <- NA_character_
            summary$completed_fit_retained <- FALSE
            retention <- countdlm_road_try_retain_failure(
                list(
                    api_version = config$api_version,
                    phase = "k6-target-sensitivity-diagnostic",
                    status = "error", retained = TRUE,
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
                summary = summary, terminal_audit = NULL,
                failure_record_saved = retention$saved,
                failure_record_error = retention$error
            )
        }
        countdlm_road_stop_if_failure_unretained(
            batch_results, paste0("K = 6 diagnostic batch ", batch_id)
        )
        for (local_index in seq_along(indices)) {
            task_results[[indices[[local_index]]]] <-
                batch_results[[local_index]]
        }
        countdlm_road_assert_k6_output(
            output_dir, directories, config$config_signature,
            paste0("after batch ", batch_id)
        )
        batch_runtime <- do.call(rbind, lapply(
            batch_results, `[[`, "summary"
        ))
        if (any(batch_runtime$status != "ok") ||
            any(batch_runtime$warning_count > 0L)) {
            stop(
                "A K = 6 task failed or warned in batch ", batch_id,
                ". Retained evidence must be reviewed.", call. = FALSE
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
    terminal_audit <- do.call(rbind, lapply(
        task_results, `[[`, "terminal_audit"
    ))
    terminal_audit <- terminal_audit[
        match(config$tasks$task_id, terminal_audit$task_id), , drop = FALSE
    ]
    rownames(terminal_audit) <- NULL
    if (any(runtime$status != "ok") ||
        any(runtime$warning_count > 0L) ||
        any(runtime$paired_initialization_passed != TRUE) ||
        any(runtime$terminal_state_contract_passed != TRUE) ||
        nrow(terminal_audit) != nrow(config$tasks) ||
        any(terminal_audit$exact_resume_interface_available != TRUE)) {
        stop("Completed K = 6 tasks failed the final contract.",
             call. = FALSE)
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
                stop("A same-seed K = 6 pair failed initialization pairing.",
                     call. = FALSE)
            }
        }
    }

    run_stage <- "k6-derived-diagnostics"
    diagnostics <- countdlm_road_partition_diagnostics(
        runtime, config$tasks, output_dir, config
    )
    target_limit <- data.frame(
        rule_id = "K6-target-sensitivity-limit",
        fixed_definition = paste(
            "only K_fit changes from 10 to 6; all other registered",
            "controls and thresholds remain fixed"
        ),
        interpretation_limit = paste(
            "K6 changes the posterior target and cannot validate, replace,",
            "or authorize the formal K10 design"
        ),
        stringsAsFactors = FALSE
    )
    diagnostics$rule_specification <- rbind(
        target_limit, diagnostics$rule_specification
    )
    initialization_audit <- runtime[, c(
        "task_id", "method", "mode_id", "seed_id", "seed",
        "initial_partition_sha256", "initial_theta_sha256",
        "initial_gamma_sha256", "initial_joint_sha256",
        "paired_initialization_passed", "terminal_state_contract_passed"
    )]
    k6_mechanic_parts <- vector("list", nrow(config$tasks))
    for (index in seq_len(nrow(config$tasks))) {
        wrapper <- readRDS(file.path(
            output_dir, runtime$fit_file[[index]]
        ))
        k6_mechanic_parts[[index]] <- countdlm_road_k6_count_rows(
            wrapper$fit, config$tasks[index, , drop = FALSE],
            config, 6L, "K6-diagnostic"
        )
        rm(wrapper)
        invisible(gc(verbose = FALSE))
    }
    k6_mechanics <- list(
        summary = do.call(rbind, lapply(k6_mechanic_parts, `[[`, "summary")),
        transitions = do.call(rbind, lapply(
            k6_mechanic_parts, `[[`, "transitions"
        )),
        dwell = do.call(rbind, lapply(k6_mechanic_parts, `[[`, "dwell"))
    )
    k10_mechanics <- countdlm_road_k6_parent_count_mechanics(
        parent, config
    )
    combined_mechanics <- rbind(k10_mechanics, k6_mechanics$summary)
    parent_decision <- parent$result$method_decision
    k6_decision <- diagnostics$method_decision
    method_screen <- data.frame(
        method = config$methods,
        K10_D_late = parent_decision$D_late[
            match(config$methods, parent_decision$method)
        ],
        K6_D_late = k6_decision$D_late[
            match(config$methods, k6_decision$method)
        ],
        K10_failed_drift_screens = parent_decision$failed_drift_screens[
            match(config$methods, parent_decision$method)
        ],
        K6_failed_drift_screens = k6_decision$failed_drift_screens[
            match(config$methods, k6_decision$method)
        ],
        K10_failed_scalar_screens = parent_decision$failed_scalar_screens[
            match(config$methods, parent_decision$method)
        ],
        K6_failed_scalar_screens = k6_decision$failed_scalar_screens[
            match(config$methods, k6_decision$method)
        ],
        K10_resolved_under_control =
            parent_decision$resolved_under_control[
                match(config$methods, parent_decision$method)
            ],
        K6_resolved_under_same_screen =
            k6_decision$resolved_under_control[
                match(config$methods, k6_decision$method)
            ],
        target_sensitivity_candidate =
            !parent_decision$resolved_under_control[
                match(config$methods, parent_decision$method)
            ] & k6_decision$resolved_under_control[
                match(config$methods, k6_decision$method)
            ],
        remains_unresolved_at_K6 =
            !k6_decision$resolved_under_control[
                match(config$methods, k6_decision$method)
            ],
        formal_inference_permitted = FALSE,
        stringsAsFactors = FALSE
    )
    rownames(k6_mechanics$summary) <- NULL
    rownames(k6_mechanics$transitions) <- NULL
    rownames(k6_mechanics$dwell) <- NULL
    rownames(combined_mechanics) <- NULL

    countdlm_road_atomic_write_csv(
        runtime, file.path(output_dir, "k6-diagnostic-runtime.csv")
    )
    countdlm_road_atomic_write_csv(
        initialization_audit,
        file.path(output_dir, "k6-paired-initialization-audit.csv")
    )
    output_tables <- countdlm_road_k6_output_tables()
    for (name in names(output_tables)) {
        countdlm_road_atomic_write_csv(
            diagnostics[[name]], file.path(output_dir, output_tables[[name]])
        )
    }
    countdlm_road_atomic_write_csv(
        k6_mechanics$summary,
        file.path(output_dir, "k6-count-mechanics-summary.csv")
    )
    countdlm_road_atomic_write_csv(
        k6_mechanics$transitions,
        file.path(output_dir, "k6-count-transition-matrix.csv")
    )
    countdlm_road_atomic_write_csv(
        k6_mechanics$dwell,
        file.path(output_dir, "k6-count-dwell-runs.csv")
    )
    countdlm_road_atomic_write_csv(
        combined_mechanics,
        file.path(output_dir, "k6-vs-k10-count-mechanics.csv")
    )
    countdlm_road_atomic_write_csv(
        method_screen,
        file.path(output_dir, "k6-vs-k10-method-screen.csv")
    )
    countdlm_road_atomic_write_csv(
        terminal_audit,
        file.path(output_dir, "k6-terminal-state-audit.csv")
    )

    failed_tasks <- sum(runtime$status != "ok")
    warning_tasks <- sum(runtime$warning_count > 0L)
    all_initializations_paired <- all(
        runtime$paired_initialization_passed %in% TRUE
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
        runtime = runtime,
        initialization_audit = initialization_audit,
        terminal_state_audit = terminal_audit,
        mode_specification = config$mode_specification,
        rule_specification = diagnostics$rule_specification,
        block_summary = diagnostics$block_summary,
        pairwise_psm = diagnostics$pairwise_psm,
        pair_persistence = diagnostics$pair_persistence,
        within_chain_drift = diagnostics$within_chain_drift,
        start_memory = diagnostics$start_memory,
        scalar_diagnostics = diagnostics$scalar_diagnostics,
        cluster_count_frequency = diagnostics$cluster_count_frequency,
        late_representative_partition =
            diagnostics$late_representative_partition,
        node_psm_difference = diagnostics$node_psm_difference,
        macro_pairs = diagnostics$macro_pairs,
        method_decision = diagnostics$method_decision,
        count_mechanics_summary = k6_mechanics$summary,
        count_transition_matrix = k6_mechanics$transitions,
        count_dwell_runs = k6_mechanics$dwell,
        K6_vs_K10_count_mechanics = combined_mechanics,
        K6_vs_K10_method_screen = method_screen,
        pilot_wall_seconds = pilot_wall_seconds,
        total_wall_seconds_through_derived_tables = total_wall_seconds,
        truth_metrics_computed = FALSE,
        target_changed_from_parent = TRUE,
        all_tasks_ok = failed_tasks == 0L,
        failed_tasks = failed_tasks,
        warning_tasks = warning_tasks,
        all_initializations_paired = all_initializations_paired,
        all_terminal_states_ready = all_terminal_states_ready,
        all_methods_resolved_under_control = all(
            diagnostics$method_decision$resolved_under_control
        ),
        ready_for_formal_design_review = FALSE,
        eligible_for_formal_freeze = FALSE,
        formal_simulation_launched = FALSE,
        requires_human_review = TRUE
    )
    result_path <- file.path(output_dir, "road-k6-diagnostic-result.rds")
    countdlm_road_atomic_save_rds(result, result_path)
    report <- c(
        "countDLM K=6 target-sensitivity diagnostic report",
        paste("API:", config$api_version),
        paste("Output:", output_dir),
        paste("Git HEAD:", git$head),
        paste("Git clean:", git$clean),
        paste("Context SHA-256:", observed_context_sha256),
        paste("Reviewed K10 parent ZIP SHA-256:", parent$zip_sha256),
        paste("Parent K / diagnostic K:",
              config$parent_K_fit, "/", config$K_fit),
        paste("Fixed basis rank / rho:",
              config$basis_m, "/", config$selected_rho),
        paste("Requested / actual workers:",
              config$workers, "/", actual_workers),
        paste("Reported physical cores:", reported_physical_cores),
        paste("Transitions / burn:", config$iterations, "/", config$burn),
        paste("Tasks / failures / warning tasks:",
              nrow(config$tasks), "/", failed_tasks, "/", warning_tasks),
        paste("All initializations paired exactly:",
              all_initializations_paired),
        paste("All terminal states continuation-ready:",
              all_terminal_states_ready),
        paste("Method-fit wall time:",
              countdlm_road_format_duration(pilot_wall_seconds)),
        paste("Total wall time through derived tables:",
              countdlm_road_format_duration(total_wall_seconds)),
        "Only K_fit changes from 10 to 6; this changes the posterior target.",
        "K6 findings cannot validate, replace, tune, or authorize the formal K10 study.",
        "Gross births/deaths are measured before and after each allocation sweep; net count transitions and dwell runs are reported separately.",
        "Saved terminal states support the tested fixed-rho GMDE resume interface.",
        "The first 1000 iterations are a settling block, not posterior evidence.",
        "No truth metrics are computed and no outcome can authorize a formal simulation.",
        "All findings require human review.",
        "",
        "K6 versus reviewed K10 screen:",
        utils::capture.output(print(method_screen, row.names = FALSE))
    )
    report_path <- file.path(output_dir, "road-k6-diagnostic-report.txt")
    countdlm_road_atomic_write_lines(report, report_path)
    cat("\n", paste(report, collapse = "\n"), "\n", sep = "")
    run_stage <- "checksums-and-completion-marker"
    countdlm_road_assert_k6_payload(
        output_dir, config, basename(source_files)
    )
    checksum_path <- countdlm_road_write_checksums(output_dir)
    completion_path <- file.path(output_dir, "RUN-COMPLETE.rds")
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            status = "complete-with-k6-target-sensitivity-findings",
            completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
            failed_tasks = failed_tasks,
            warning_tasks = warning_tasks,
            all_initializations_paired = all_initializations_paired,
            all_terminal_states_ready = all_terminal_states_ready,
            all_methods_resolved_under_control =
                result$all_methods_resolved_under_control,
            target_changed_from_parent = TRUE,
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
