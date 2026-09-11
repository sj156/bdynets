# Longer truth-blinded stability pilot for the approved central-Beijing road
# design.  This stage consumes, but never changes, the reviewed calibration-v2b
# evidence that fixed rho at one.

countdlm_road_stability_api_version <-
    "countdlm-road-longer-stability-2026-09-03-v1"
countdlm_road_stability_parent_api_version <-
    "countdlm-road-calibration-2026-09-02-v2b"
countdlm_road_stability_parent_zip_sha256 <-
    "45b7350ebaaf302a8ef8dacf610f6dcaa1c0c34fcfcb4bfbb6fe54d28ce1f8e2"
countdlm_road_stability_parent_manifest_sha256 <-
    "d96ed74ae9d23572165618ba075e4e61b761e8b5ba82ed1b684dad59d92a1a0a"
countdlm_road_stability_parent_result_sha256 <-
    "977511fadd1c15b7f4a5e6f5ace5f0d37987f916160453cbfbabcfce321b608e"
countdlm_road_stability_parent_completion_sha256 <-
    "656872d2c51d98f004958df09f9b9849357923d1f1eed528b35d989f1a213cf3"
countdlm_road_stability_parent_config_signature <-
    "5a97954122a4418cb0631f416a899764e63906c3c7de4ef9cd7dbef06c6681ad"
countdlm_road_stability_data_sha256 <-
    "1e5d62f5c8d2978b62c862dbb3d30e84b217af22b87208563f41fc08742ce845"
countdlm_road_stability_initialization_sha256 <- c(
    "6ac5c4ed47f70633e46c7ecd836e24d717e2bd475bb9218c066b7adf3cabc555",
    "6d07f4b9f53990cfe916ef7779326bd0ba685c0586459342366c5b419b88b1d1"
)

countdlm_road_stability_zip_member_raw <- function(zip_file, member) {
    listing <- utils::unzip(zip_file, list = TRUE)
    matched <- which(basename(listing$Name) == member &
        !endsWith(listing$Name, "/"))
    if (length(matched) != 1L) {
        stop(
            "The reviewed calibration ZIP must contain exactly one ",
            member, ".", call. = FALSE
        )
    }
    expected_length <- as.numeric(listing$Length[[matched]])
    if (!is.finite(expected_length) || expected_length <= 0 ||
        expected_length > .Machine$integer.max) {
        stop("The calibration ZIP member has an invalid length: ", member,
             call. = FALSE)
    }
    connection <- unz(
        zip_file, listing$Name[[matched]], open = "rb"
    )
    on.exit(close(connection), add = TRUE)
    value <- readBin(
        connection, what = "raw", n = as.integer(expected_length)
    )
    if (length(value) != expected_length) {
        stop("The calibration ZIP member could not be read completely: ",
             member, call. = FALSE)
    }
    value
}

countdlm_road_stability_read_rds_raw <- function(value, name) {
    raw_connection <- rawConnection(value, open = "rb")
    is_gzip <- length(value) >= 2L &&
        identical(as.integer(value[seq_len(2L)]), c(31L, 139L))
    connection <- if (is_gzip) gzcon(raw_connection) else raw_connection
    on.exit(close(connection), add = TRUE)
    tryCatch(
        readRDS(connection),
        error = function(condition) {
            stop(
                "The reviewed calibration member could not be read as RDS: ",
                name, ". ", conditionMessage(condition), call. = FALSE
            )
        }
    )
}

countdlm_road_validate_stability_parent <- function(result, completion) {
    expected_variants <- countdlm_road_calibration_variants(c(0.25, 0.5, 1))
    expected_tasks <- countdlm_road_calibration_tasks(expected_variants, 2L)
    required_result <- c(
        "api_version", "config", "registration", "selected_rho",
        "rho_runtime", "rho_resolved", "phase2_started", "pilot_runtime",
        "pair_diagnostics", "all_tasks_ok", "failed_tasks", "warning_tasks",
        "all_short_screens_pass", "ready_for_longer_stability_pilot",
        "truth_metrics_computed", "formal_simulation_launched"
    )
    if (!is.list(result) || !all(required_result %in% names(result)) ||
        !identical(result$api_version,
                   countdlm_road_stability_parent_api_version) ||
        !is.list(result$config) || !is.list(result$registration) ||
        !identical(
            result$config$config_signature,
            countdlm_road_stability_parent_config_signature
        ) ||
        !identical(
            result$registration$config$config_signature,
            countdlm_road_stability_parent_config_signature
        ) ||
        !identical(result$config$context_sha256,
                   countdlm_road_context_sha256) ||
        !identical(result$config$sampler_version,
                   countdlm_gmde_sampler_version) ||
        !isTRUE(result$config$algorithm_exact) ||
        !identical(as.numeric(result$selected_rho), 1) ||
        !isTRUE(result$rho_resolved) || !isTRUE(result$phase2_started) ||
        !isTRUE(result$all_tasks_ok) ||
        !identical(as.integer(result$failed_tasks), 0L) ||
        !identical(as.integer(result$warning_tasks), 0L) ||
        !identical(result$all_short_screens_pass, FALSE) ||
        !identical(result$ready_for_longer_stability_pilot, FALSE) ||
        !identical(result$truth_metrics_computed, FALSE) ||
        !identical(result$formal_simulation_launched, FALSE) ||
        !is.data.frame(result$rho_runtime) ||
        nrow(result$rho_runtime) != 42L ||
        any(result$rho_runtime$status != "ok") ||
        any(result$rho_runtime$warning_count != 0L) ||
        !is.data.frame(result$pilot_runtime) ||
        nrow(result$pilot_runtime) != 14L ||
        any(result$pilot_runtime$status != "ok") ||
        any(result$pilot_runtime$warning_count != 0L) ||
        !identical(result$pilot_runtime$task_id, expected_tasks$task_id) ||
        !is.data.frame(result$pair_diagnostics) ||
        nrow(result$pair_diagnostics) != nrow(expected_variants) ||
        !identical(result$pair_diagnostics$variant_id,
                   expected_variants$variant_id) ||
        sum(result$pair_diagnostics$short_screen_passed %in% TRUE) != 2L) {
        stop(
            "The parent calibration result does not match the reviewed v2b ",
            "rho evidence and short-screen outcome.", call. = FALSE
        )
    }
    required_completion <- c(
        "api_version", "status", "selected_rho", "failed_tasks",
        "warning_tasks", "rho_resolved", "phase2_started",
        "all_short_screens_pass", "ready_for_longer_stability_pilot",
        "truth_metrics_computed", "formal_simulation_launched",
        "checksums_cover_payload_before_this_marker"
    )
    if (!is.list(completion) ||
        !all(required_completion %in% names(completion)) ||
        !identical(completion$api_version,
                   countdlm_road_stability_parent_api_version) ||
        !identical(completion$status,
                   "complete-with-short-screen-flags") ||
        !identical(as.numeric(completion$selected_rho), 1) ||
        !identical(as.integer(completion$failed_tasks), 0L) ||
        !identical(as.integer(completion$warning_tasks), 0L) ||
        !isTRUE(completion$rho_resolved) ||
        !isTRUE(completion$phase2_started) ||
        !identical(completion$all_short_screens_pass, FALSE) ||
        !identical(completion$ready_for_longer_stability_pilot, FALSE) ||
        !identical(completion$truth_metrics_computed, FALSE) ||
        !identical(completion$formal_simulation_launched, FALSE) ||
        !identical(
            completion$checksums_cover_payload_before_this_marker,
            "CHECKSUMS.sha256"
        )) {
        stop(
            "The parent calibration completion marker does not match the ",
            "reviewed v2b run.", call. = FALSE
        )
    }
    invisible(TRUE)
}

countdlm_road_stability_parent_evidence <- function(zip_file) {
    if (length(zip_file) != 1L || !is.character(zip_file) ||
        is.na(zip_file) || !nzchar(zip_file)) {
        stop("zip_file must be one explicit path.", call. = FALSE)
    }
    zip_file <- normalizePath(zip_file, winslash = "/", mustWork = TRUE)
    zip_sha256 <- countdlm_road_sha256(zip_file)
    if (!identical(
        zip_sha256, countdlm_road_stability_parent_zip_sha256
    )) {
        stop(
            "The calibration ZIP SHA-256 does not match the reviewed v2b ",
            "archive.", call. = FALSE
        )
    }
    manifest_raw <- countdlm_road_stability_zip_member_raw(
        zip_file, "CHECKSUMS.sha256"
    )
    result_raw <- countdlm_road_stability_zip_member_raw(
        zip_file, "road-calibration-result.rds"
    )
    completion_raw <- countdlm_road_stability_zip_member_raw(
        zip_file, "RUN-COMPLETE.rds"
    )
    observed_hashes <- c(
        manifest = digest::digest(
            manifest_raw, algo = "sha256", serialize = FALSE
        ),
        result = digest::digest(
            result_raw, algo = "sha256", serialize = FALSE
        ),
        completion = digest::digest(
            completion_raw, algo = "sha256", serialize = FALSE
        )
    )
    expected_hashes <- c(
        manifest = countdlm_road_stability_parent_manifest_sha256,
        result = countdlm_road_stability_parent_result_sha256,
        completion = countdlm_road_stability_parent_completion_sha256
    )
    if (!identical(observed_hashes, expected_hashes)) {
        stop(
            "One or more reviewed calibration evidence members failed their ",
            "registered SHA-256 check.", call. = FALSE
        )
    }
    manifest_lines <- strsplit(
        rawToChar(manifest_raw), "\n", fixed = TRUE
    )[[1L]]
    result_manifest_line <- paste0(
        countdlm_road_stability_parent_result_sha256,
        "  road-calibration-result.rds"
    )
    if (sum(manifest_lines == result_manifest_line) != 1L) {
        stop(
            "The reviewed result hash is not uniquely recorded in the parent ",
            "checksum manifest.", call. = FALSE
        )
    }
    result <- countdlm_road_stability_read_rds_raw(
        result_raw, "road-calibration-result.rds"
    )
    completion <- countdlm_road_stability_read_rds_raw(
        completion_raw, "RUN-COMPLETE.rds"
    )
    countdlm_road_validate_stability_parent(result, completion)
    list(
        zip_file = zip_file,
        zip_sha256 = zip_sha256,
        manifest_sha256 = observed_hashes[["manifest"]],
        result_sha256 = observed_hashes[["result"]],
        completion_sha256 = observed_hashes[["completion"]],
        config_signature = result$config$config_signature,
        selected_rho = as.numeric(result$selected_rho),
        failed_short_variants = result$pair_diagnostics$variant_id[
            !result$pair_diagnostics$short_screen_passed
        ],
        source_sha256 = result$registration$source_sha256,
        result = result,
        completion = completion
    )
}

countdlm_road_validate_stability_config <- function(config) {
    if (!inherits(config, "countdlm_road_stability_config") ||
        !identical(config$api_version,
                   countdlm_road_stability_api_version) ||
        !is.character(config$config_signature) ||
        length(config$config_signature) != 1L ||
        !identical(
            config$config_signature,
            countdlm_road_config_signature(config)
        )) {
        stop(
            "The road stability configuration is invalid or was modified ",
            "after construction.", call. = FALSE
        )
    }
    expected_variants <- countdlm_road_calibration_variants(
        c(0.25, 0.5, 1)
    )
    expected_tasks <- countdlm_road_calibration_tasks(
        expected_variants, 2L
    )
    if (!identical(config$sampler_version,
                   countdlm_gmde_sampler_version) ||
        !identical(config$parent_calibration_api_version,
                   countdlm_road_stability_parent_api_version) ||
        !identical(config$parent_calibration_zip_sha256,
                   countdlm_road_stability_parent_zip_sha256) ||
        !identical(config$parent_calibration_manifest_sha256,
                   countdlm_road_stability_parent_manifest_sha256) ||
        !identical(config$parent_calibration_result_sha256,
                   countdlm_road_stability_parent_result_sha256) ||
        !identical(config$parent_calibration_completion_sha256,
                   countdlm_road_stability_parent_completion_sha256) ||
        !identical(config$parent_calibration_config_signature,
                   countdlm_road_stability_parent_config_signature) ||
        !identical(config$context_sha256,
                   countdlm_road_context_sha256) ||
        !is.character(config$execution_source_dir) ||
        length(config$execution_source_dir) != 1L ||
        !dir.exists(config$execution_source_dir) ||
        !is.character(config$source_sha256) ||
        is.null(names(config$source_sha256)) ||
        anyDuplicated(names(config$source_sha256)) ||
        !length(config$source_sha256) ||
        anyNA(config$source_sha256) ||
        any(!grepl("^[[:xdigit:]]{64}$", config$source_sha256)) ||
        !identical(config$data_sha256,
                   countdlm_road_stability_data_sha256) ||
        !identical(config$initialization_sha256,
                   countdlm_road_stability_initialization_sha256) ||
        !identical(config$n, 100L) || !identical(config$TT, 168L) ||
        !identical(config$K_true, 5L) ||
        !identical(config$K_fit, 10L) ||
        !identical(config$basis_m, 40L) ||
        !identical(config$methods, countdlm_road_internal_methods) ||
        !identical(config$variants, expected_variants) ||
        !identical(config$pilot_tasks, expected_tasks) ||
        !identical(config$workers, 6L) ||
        !identical(config$reserved_reported_cores, 4L) ||
        !identical(config$worker_thread_limit, 1L) ||
        !identical(config$progress_poll_seconds, 0.5) ||
        !identical(config$selected_rho, 1) ||
        !identical(config$pilot_iterations, 2000L) ||
        !identical(config$pilot_burn, 1000L) ||
        !identical(config$pilot_chains, 2L) ||
        !identical(config$short_log_rhat_limit, 1.10) ||
        !identical(config$short_count_rhat_limit, 1.20) ||
        !identical(config$short_psm_rms_limit, 0.10) ||
        !identical(config$short_psm_mean_abs_limit, 0.05) ||
        !identical(config$potts_beta_grid, c(0.25, 0.5, 1)) ||
        !identical(config$potts_beta_status, "screened-not-selected") ||
        !identical(config$target_full_iterations, 2000L) ||
        !identical(config$target_full_chains, 2L) ||
        !identical(config$full_budget_hours, 12) ||
        !identical(config$budget_fraction, 0.80) ||
        !identical(config$projection_multiplier, 2) ||
        !identical(config$max_projected_replicates, 40L) ||
        !identical(config$data_seed, 2026091101L) ||
        !identical(config$pilot_initialization_seed_base, 2026091400L) ||
        !identical(config$pilot_method_seed_base, 2026092400L) ||
        !identical(config$state_G, diag(2)) ||
        !identical(config$state_W, diag(c(1e-6, 5e-7))) ||
        !identical(config$state_C0, diag(c(2, 1))) ||
        !identical(config$substantive_min, 5L) ||
        !identical(config$pg_backend, "devroye-exact") ||
        !isTRUE(config$algorithm_exact) ||
        !isTRUE(config$classification_only) ||
        !identical(config$truth_metrics_computed, FALSE) ||
        !identical(config$formal_results_authorized, FALSE)) {
        stop("The longer stability pilot's fixed contract is invalid.",
             call. = FALSE)
    }
    invisible(config)
}

#' Construct the approved-road longer stability-pilot configuration
#'
#' The configuration is fixed to the reviewed calibration-v2b decision:
#' `rho = 1`, seven internal variants, two independent starts, 2,000
#' transitions, and a 1,000-transition burn-in.  It records SHA-256 hashes for
#' the frozen execution sources, remains truth-blinded, and cannot authorize
#' or launch a formal simulation.
#'
#' @param context_file Approved external D-017 road-context RDS.
#' @param parent_calibration_zip Exact reviewed calibration-v2b ZIP archive.
#' @param output_dir Brand-new external output directory.
#' @param execution_source_dir Directory containing the frozen `R/*.R` files
#'   that were sourced for this run.
#' @return A signed, fixed longer-stability configuration.
#' @export
countdlm_road_stability_config <- function(
    context_file, parent_calibration_zip, output_dir,
    execution_source_dir = file.path(getwd(), "R")
) {
    potts_beta_grid <- c(0.25, 0.5, 1)
    variants <- countdlm_road_calibration_variants(potts_beta_grid)
    execution_source_dir <- normalizePath(
        execution_source_dir, winslash = "/", mustWork = TRUE
    )
    execution_source_files <- sort(list.files(
        execution_source_dir, pattern = "[.]R$", full.names = TRUE
    ))
    if (!length(execution_source_files)) {
        stop("execution_source_dir contains no R source files.",
             call. = FALSE)
    }
    source_sha256 <- stats::setNames(
        vapply(
            execution_source_files, countdlm_road_sha256, character(1)
        ),
        basename(execution_source_files)
    )
    config <- list(
        api_version = countdlm_road_stability_api_version,
        sampler_version = countdlm_gmde_sampler_version,
        scientific_role = paste(
            "truth-blinded fixed-rho longer two-chain stability pilot;",
            "not an inferential or formal simulation"
        ),
        context_file = normalizePath(
            context_file, winslash = "/", mustWork = TRUE
        ),
        context_sha256 = countdlm_road_context_sha256,
        parent_calibration_zip = normalizePath(
            parent_calibration_zip, winslash = "/", mustWork = TRUE
        ),
        parent_calibration_api_version =
            countdlm_road_stability_parent_api_version,
        parent_calibration_zip_sha256 =
            countdlm_road_stability_parent_zip_sha256,
        parent_calibration_manifest_sha256 =
            countdlm_road_stability_parent_manifest_sha256,
        parent_calibration_result_sha256 =
            countdlm_road_stability_parent_result_sha256,
        parent_calibration_completion_sha256 =
            countdlm_road_stability_parent_completion_sha256,
        parent_calibration_config_signature =
            countdlm_road_stability_parent_config_signature,
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
        methods = countdlm_road_internal_methods,
        variants = variants,
        pilot_tasks = countdlm_road_calibration_tasks(variants, 2L),
        workers = 6L,
        reserved_reported_cores = 4L,
        worker_thread_limit = 1L,
        progress_poll_seconds = 0.5,
        selected_rho = 1,
        pilot_iterations = 2000L,
        pilot_burn = 1000L,
        pilot_chains = 2L,
        short_log_rhat_limit = 1.10,
        short_count_rhat_limit = 1.20,
        short_psm_rms_limit = 0.10,
        short_psm_mean_abs_limit = 0.05,
        potts_beta_grid = potts_beta_grid,
        potts_beta_status = "screened-not-selected",
        target_full_iterations = 2000L,
        target_full_chains = 2L,
        full_budget_hours = 12,
        budget_fraction = 0.80,
        projection_multiplier = 2,
        max_projected_replicates = 40L,
        data_seed = 2026091101L,
        data_sha256 = countdlm_road_stability_data_sha256,
        pilot_initialization_seed_base = 2026091400L,
        initialization_sha256 =
            countdlm_road_stability_initialization_sha256,
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
            "two 2,000-transition chains per replicate; six workers"
        )
    )
    config$config_signature <- countdlm_road_config_signature(config)
    config <- structure(config, class = "countdlm_road_stability_config")
    countdlm_road_validate_stability_config(config)
    config
}

countdlm_road_assert_stability_output <- function(
    output_dir, directories, config_signature, stage
) {
    required_directories <- unique(c(output_dir, directories))
    required_files <- file.path(
        output_dir,
        c("road-stability-registration.rds", "RUN-STARTED.rds")
    )
    missing <- c(
        required_directories[!dir.exists(required_directories)],
        required_files[!file.exists(required_files)]
    )
    if (length(missing)) {
        stop(
            "The registered stability output tree is unavailable during ",
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
            "The stability output identity could not be verified during ",
            stage, ": ", detail,
            ". The directory will not be recreated, and this run is invalid.",
            call. = FALSE
        )
    }
    invisible(TRUE)
}

countdlm_road_stability_pair_diagnostics <- function(
    runtime_summary, tasks, output_dir, burn, iterations,
    log_rhat_limit, count_rhat_limit, psm_rms_limit, psm_mean_abs_limit
) {
    value <- countdlm_road_pair_diagnostics(
        runtime_summary = runtime_summary,
        tasks = tasks,
        output_dir = output_dir,
        burn = burn,
        iterations = iterations,
        log_rhat_limit = log_rhat_limit,
        count_rhat_limit = count_rhat_limit,
        psm_rms_limit = psm_rms_limit,
        psm_mean_abs_limit = psm_mean_abs_limit
    )
    names(value)[names(value) == "short_screen_passed"] <-
        "stability_screen_passed"
    names(value)[names(value) == "short_pilot_reference"] <-
        "stability_reference"
    value$stability_reference <- sub(
        "^short-screen", "longer-stability-screen",
        value$stability_reference
    )
    value
}

#' Run the approved-road longer fixed-rho stability pilot
#'
#' The runner verifies the exact reviewed calibration-v2b ZIP, confirms that
#' all previously executed sampler sources are byte-identical, regenerates the
#' same truth-blinded data and two starts, and runs 14 fresh fixed-`rho` chains
#' in guarded batches.  It records diagnostics and timing but never launches a
#' formal simulation.
#'
#' @param config Output of `countdlm_road_stability_config()`.
#' @param repository_root Any path inside the Git repository used to record
#'   Git metadata for the run.
#' @return Longer-stability summaries, diagnostics, timing projection, and
#'   immutable output paths.
#' @export
countdlm_road_stability_pilot <- function(config, repository_root) {
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
    countdlm_road_validate_stability_config(config)
    if (!requireNamespace("BayesLogit", quietly = TRUE) ||
        !requireNamespace("digest", quietly = TRUE)) {
        stop("The longer stability pilot requires BayesLogit and digest.",
             call. = FALSE)
    }
    parent <- countdlm_road_stability_parent_evidence(
        config$parent_calibration_zip
    )
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
            "the longer stability pilot was not started.", call. = FALSE
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
            "One or more sampler sources differ from the reviewed v2b run; ",
            "the longer stability pilot was not started.", call. = FALSE
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
            "The number of physical cores could not be verified; the safe ",
            "six-worker stability pilot was not started.", call. = FALSE
        )
    }
    if (actual_workers > max(
            1L,
            reported_physical_cores - config$reserved_reported_cores
        )) {
        stop(
            "The stability worker count must leave the registered number of ",
            "reported physical cores unused.", call. = FALSE
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
            "calibration data.", call. = FALSE
        )
    }
    initializations <- countdlm_road_calibration_initializations(
        blinded_data$Y, config$K_fit,
        config$pilot_initialization_seed_base
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
            "The two regenerated starting partitions do not match the ",
            "reviewed calibration starts.", call. = FALSE
        )
    }
    rm(generated, context)

    if (!dir.create(output_dir, recursive = FALSE, showWarnings = FALSE)) {
        stop("Could not create the registered stability directory.",
             call. = FALSE)
    }
    run_complete <- FALSE
    run_stage <- "registration"
    on.exit({
        if (!run_complete) {
            incomplete <- file.path(output_dir, "RUN-INCOMPLETE.rds")
            if (!dir.exists(output_dir)) {
                message(
                    "The registered stability output directory is ",
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
            reported_cores_left_unused =
                reported_physical_cores - actual_workers,
            worker_thread_limit = config$worker_thread_limit,
            BLAS = unname(extSoftVersion()[["BLAS"]])
        ),
        source_sha256 = source_sha256,
        parent_calibration = list(
            api_version = config$parent_calibration_api_version,
            zip_file = parent$zip_file,
            zip_sha256 = parent$zip_sha256,
            manifest_sha256 = parent$manifest_sha256,
            result_sha256 = parent$result_sha256,
            completion_sha256 = parent$completion_sha256,
            config_signature = parent$config_signature,
            selected_rho = parent$selected_rho,
            failed_short_variants = parent$failed_short_variants
        ),
        context_sha256 = config$context_sha256,
        context_approval = "D-017",
        data_sha256 = observed_data_sha256,
        initialization_sha256 = observed_initialization_sha256,
        config = unclass(config),
        note = paste(
            "Truth-blinded longer stability pilot only; no truth-based",
            "recovery metric is computed and no formal simulation is launched."
        )
    )
    countdlm_road_atomic_save_rds(
        registration,
        file.path(output_dir, "road-stability-registration.rds")
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
            stop("Could not create stability output directories.",
                 call. = FALSE)
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
        file.path(output_dir, "road-stability-blinded-data.rds")
    )
    countdlm_road_assert_stability_output(
        output_dir, directories, config$config_signature,
        "pre-dispatch output verification"
    )
    parent$result <- NULL
    parent$completion <- NULL

    stability_worker <- function(index) {
        countdlm_road_calibration_limit_threads()
        task <- config$pilot_tasks[index, , drop = FALSE]
        task_id <- task$task_id[[1L]]
        variant_id <- task$variant_id[[1L]]
        method <- task$method[[1L]]
        potts_beta <- task$potts_beta[[1L]]
        chain_id <- task$chain_id[[1L]]
        seed <- config$pilot_method_seed_base + 101L * index
        slug <- paste0(sprintf("%02d", index), "-", gsub(
            "[^A-Za-z0-9]+", "-", task_id
        ))
        fit_path <- file.path(chain_dir, paste0(slug, "-fit.rds"))
        fit_relative_path <- file.path("chains", basename(fit_path))
        diagnostic_path <- file.path(
            diagnostic_dir, paste0(slug, "-diagnostic.rds")
        )
        failure_path <- file.path(
            failure_dir, paste0(slug, "-failure.rds")
        )
        task_started <- Sys.time()
        worker_stage <- "stability-fit"
        warning_messages <- character()
        withCallingHandlers(
            tryCatch({
                fit <- countdlm_road_fit_method(
                    method = method,
                    data = blinded_data,
                    method_inputs = method_inputs,
                    config = config,
                    seed = seed,
                    Z_init = initializations[[chain_id]],
                    potts_beta = if (method == "Potts-MDE") {
                        potts_beta
                    } else NULL,
                    n_iter = config$pilot_iterations,
                    burn = config$pilot_burn,
                    rho = config$selected_rho
                )
                elapsed <- as.numeric(difftime(
                    Sys.time(), task_started, units = "secs"
                ))
                worker_stage <- "stability-save-completed-fit"
                countdlm_road_atomic_save_rds(
                    list(
                        api_version = config$api_version,
                        parent_calibration_result_sha256 =
                            config$parent_calibration_result_sha256,
                        task_id = task_id,
                        variant_id = variant_id,
                        method = method,
                        potts_beta = potts_beta,
                        chain_id = chain_id,
                        seed = seed,
                        selected_rho = config$selected_rho,
                        warnings_before_fit_retention = warning_messages,
                        truth_metrics_computed = FALSE,
                        fit = fit
                    ),
                    fit_path
                )
                worker_stage <- "stability-post-fit-contract"
                if (!countdlm_road_truth_blinding_ok(
                    fit, config$pilot_iterations
                )) {
                    stop(
                        "The fit failed the strict truth-blinding contract.",
                        call. = FALSE
                    )
                }
                worker_stage <- "stability-fixed-rho-contract"
                fixed_rho_audit <- countdlm_road_fixed_rho_calibration(
                    fit = fit,
                    settle = config$pilot_burn,
                    score = config$pilot_iterations - config$pilot_burn
                )
                worker_stage <- "stability-summarize"
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
                summarized$compact$selected_rho <- config$selected_rho
                summarized$compact$summary_schema_version <-
                    countdlm_road_benchmark_api_version
                summarized$compact$api_version <- config$api_version
                summarized$compact$fixed_rho_contract <- fixed_rho_audit
                worker_stage <- "stability-save-diagnostic"
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
                    potts_beta = potts_beta,
                    elapsed = elapsed,
                    config = list(
                        quick_iterations = config$pilot_iterations,
                        quick_burn = config$pilot_burn,
                        rho_timing = config$selected_rho
                    ),
                    error = conditionMessage(error),
                    stage = worker_stage,
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
                        phase = "fixed-rho-longer-stability-pilot",
                        task_id = task_id,
                        variant_id = variant_id,
                        method = method,
                        potts_beta = potts_beta,
                        chain_id = chain_id,
                        status = "error",
                        elapsed_seconds = elapsed,
                        stage = worker_stage,
                        error = conditionMessage(error),
                        call = paste(
                            deparse(conditionCall(error)), collapse = " "
                        ),
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
        "Longer road stability pilot: seven variants x two fresh starts\n",
        config$pilot_iterations, " transitions / ", config$pilot_burn,
        " burn; fixed reviewed rho = ", config$selected_rho, "; ",
        actual_workers, " single-thread worker(s), leaving approximately ",
        config$reserved_reported_cores, " reported cores unused\n",
        "No truth metrics and no formal simulation.\n", sep = ""
    )
    run_stage <- "fixed-rho-longer-stability-pilot"
    task_batches <- split(
        seq_len(nrow(config$pilot_tasks)),
        ceiling(seq_len(nrow(config$pilot_tasks)) / actual_workers)
    )
    stability_results <- vector("list", nrow(config$pilot_tasks))
    pilot_started <- Sys.time()
    for (batch_index in seq_along(task_batches)) {
        indices <- task_batches[[batch_index]]
        countdlm_road_assert_stability_output(
            output_dir, directories, config$config_signature,
            paste0("before stability batch ", batch_index)
        )
        cat(
            "\nStability batch ", batch_index, "/",
            length(task_batches), "\n", sep = ""
        )
        batch_results <- countdlm_road_run_batches(
            config$pilot_tasks$task_id[indices],
            function(local_index) stability_worker(indices[[local_index]]),
            cores = min(actual_workers, length(indices)),
            poll_seconds = config$progress_poll_seconds
        )
        countdlm_road_stop_if_failure_unretained(
            batch_results,
            paste0("longer-stability batch ", batch_index)
        )
        for (local_index in seq_along(indices)) {
            if (inherits(
                batch_results[[local_index]],
                "countdlm_road_scheduler_error"
            )) {
                error <- batch_results[[local_index]]
                index <- indices[[local_index]]
                task <- config$pilot_tasks[index, , drop = FALSE]
                summary <- countdlm_road_failure_summary(
                    task_id = task$task_id[[1L]],
                    method = task$method[[1L]],
                    potts_beta = task$potts_beta[[1L]],
                    elapsed = error$elapsed_seconds,
                    config = list(
                        quick_iterations = config$pilot_iterations,
                        quick_burn = config$pilot_burn,
                        rho_timing = config$selected_rho
                    ),
                    error = error$message,
                    stage = if (identical(
                        error$origin, "scheduler-collect"
                    )) "parallel-scheduler-collect" else
                        "parallel-worker-uncaught"
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
                        phase = "fixed-rho-longer-stability-pilot",
                        status = "error",
                        retained = TRUE,
                        error_origin = error$origin,
                        original_error = error$message,
                        summary = summary
                    ),
                    file.path(
                        failure_dir,
                        paste0(sprintf("%02d", index),
                               "-scheduler-failure.rds")
                    )
                )
                batch_results[[local_index]] <- list(
                    summary = summary,
                    failure_record_saved = failure_retention$saved,
                    failure_record_error = failure_retention$error
                )
            }
            stability_results[[indices[[local_index]]]] <-
                batch_results[[local_index]]
        }
        countdlm_road_stop_if_failure_unretained(
            batch_results,
            paste0("longer-stability batch ", batch_index,
                   " reconciliation")
        )
        countdlm_road_assert_stability_output(
            output_dir, directories, config$config_signature,
            paste0("after stability batch ", batch_index)
        )
        batch_summary <- do.call(rbind, lapply(
            batch_results, `[[`, "summary"
        ))
        if (any(batch_summary$status != "ok") ||
            any(batch_summary$warning_count > 0L)) {
            stop(
                "A longer-stability task failed or warned in batch ",
                batch_index,
                "; no later batch was started. Retained diagnostics must be ",
                "reviewed.", call. = FALSE
            )
        }
    }
    pilot_wall_seconds <- as.numeric(difftime(
        Sys.time(), pilot_started, units = "secs"
    ))
    runtime <- do.call(rbind, lapply(
        stability_results, `[[`, "summary"
    ))
    runtime <- runtime[
        match(config$pilot_tasks$task_id, runtime$task_id), , drop = FALSE
    ]
    rownames(runtime) <- NULL
    run_stage <- "longer-stability-derived-diagnostics"
    pair_diagnostics <- countdlm_road_stability_pair_diagnostics(
        runtime_summary = runtime,
        tasks = config$pilot_tasks,
        output_dir = output_dir,
        burn = config$pilot_burn,
        iterations = config$pilot_iterations,
        log_rhat_limit = config$short_log_rhat_limit,
        count_rhat_limit = config$short_count_rhat_limit,
        psm_rms_limit = config$short_psm_rms_limit,
        psm_mean_abs_limit = config$short_psm_mean_abs_limit
    )
    projection <- countdlm_road_calibration_projection(
        runtime, config, config$selected_rho, actual_workers
    )
    countdlm_road_atomic_write_csv(
        runtime, file.path(output_dir, "stability-runtime.csv")
    )
    countdlm_road_atomic_write_csv(
        pair_diagnostics,
        file.path(output_dir, "stability-chain-diagnostics.csv")
    )
    countdlm_road_atomic_write_csv(
        projection$variant_timing,
        file.path(output_dir, "stability-variant-timing.csv")
    )
    countdlm_road_atomic_write_csv(
        projection$table,
        file.path(output_dir, "formal-runtime-projection-2000.csv")
    )
    failed_tasks <- sum(runtime$status != "ok")
    warning_tasks <- sum(runtime$warning_count > 0L)
    all_stability_screens_pass <- nrow(pair_diagnostics) ==
        nrow(config$variants) &&
        all(pair_diagnostics$stability_screen_passed %in% TRUE)
    total_wall_seconds <- as.numeric(difftime(
        Sys.time(), started_at, units = "secs"
    ))
    result <- list(
        api_version = config$api_version,
        config = config,
        registration = registration,
        parent_calibration = registration$parent_calibration,
        selected_rho = config$selected_rho,
        runtime = runtime,
        pair_diagnostics = pair_diagnostics,
        projection = projection,
        pilot_wall_seconds = pilot_wall_seconds,
        total_wall_seconds_through_derived_tables = total_wall_seconds,
        truth_metrics_computed = FALSE,
        all_tasks_ok = failed_tasks == 0L,
        failed_tasks = failed_tasks,
        warning_tasks = warning_tasks,
        all_stability_screens_pass = all_stability_screens_pass,
        ready_for_formal_design_review = failed_tasks == 0L &&
            warning_tasks == 0L && all_stability_screens_pass,
        clean_commit_stability_evidence = isTRUE(git$clean),
        eligible_for_formal_freeze = FALSE,
        formal_simulation_launched = FALSE,
        requires_human_review = TRUE
    )
    result_path <- file.path(output_dir, "road-stability-result.rds")
    countdlm_road_atomic_save_rds(result, result_path)
    report <- c(
        "countDLM approved-road longer stability-pilot report",
        paste("API:", config$api_version),
        paste("Output:", output_dir),
        paste("Git HEAD:", git$head),
        paste("Git clean:", git$clean),
        paste("Context SHA-256:", observed_context_sha256),
        paste("Parent calibration ZIP SHA-256:", parent$zip_sha256),
        paste("Parent calibration result SHA-256:", parent$result_sha256),
        paste("Reviewed fixed rho:", config$selected_rho),
        paste("Requested / actual workers:", config$workers, "/",
              actual_workers),
        paste("Reported physical cores:", reported_physical_cores),
        paste("Reported cores left unused:",
              reported_physical_cores - actual_workers),
        paste("Variants x chains:", nrow(config$variants), "x",
              config$pilot_chains),
        paste("Transitions / burn:", config$pilot_iterations, "/",
              config$pilot_burn),
        paste("Method-fit wall time:",
              countdlm_road_format_duration(pilot_wall_seconds)),
        paste("Total wall time through derived tables:",
              countdlm_road_format_duration(total_wall_seconds)),
        paste("Task failures / warning tasks:", failed_tasks, "/",
              warning_tasks),
        paste("All longer stability screens passed:",
              all_stability_screens_pass),
        paste("Ready only for formal-design review:",
              result$ready_for_formal_design_review),
        paste("12-hour internal computation gate:",
              projection$gate_hours, "hours"),
        paste("Largest projected 2,000-step internal replicate count:",
              projection$recommended_replicates),
        "R-hat and posterior-similarity screens are diagnostics, not automatic formal-run authorization.",
        "No truth labels were stored or passed to a worker; no truth-based recovery metric was computed.",
        "Potts complete densities may be compared between chains at one fixed beta, never across beta values.",
        paste("Excluded from projection:", projection$excluded_scope),
        "Formal simulation was not launched and still requires human review, checkpointing, a clean frozen source, and separate authorization.",
        "",
        "Longer two-chain diagnostic references:",
        utils::capture.output(print(pair_diagnostics, row.names = FALSE))
    )
    report_path <- file.path(output_dir, "road-stability-report.txt")
    countdlm_road_atomic_write_lines(report, report_path)
    cat("\n", paste(report, collapse = "\n"), "\n", sep = "")
    run_stage <- "checksums-and-completion-marker"
    checksum_path <- countdlm_road_write_checksums(output_dir)
    completion_path <- file.path(output_dir, "RUN-COMPLETE.rds")
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            status = if (failed_tasks == 0L && warning_tasks == 0L &&
                all_stability_screens_pass) {
                "complete"
            } else "complete-with-stability-flags",
            completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
            selected_rho = config$selected_rho,
            failed_tasks = failed_tasks,
            warning_tasks = warning_tasks,
            all_stability_screens_pass = all_stability_screens_pass,
            ready_for_formal_design_review =
                result$ready_for_formal_design_review,
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
