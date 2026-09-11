# Paired partition-mode restart diagnostic for the three graph-gating methods.
# This is a truth-blinded development experiment.  It keeps K, graph rank,
# rho, data, and the exact sampler fixed, and changes only the registered
# starting partition within each paired-seed comparison.  The paired chains
# begin from the same RNG seed, but their later random-number consumption may
# diverge when their allocation-dependent execution paths differ.

countdlm_road_partition_diagnostic_api_version <-
    "countdlm-road-partition-diagnostic-2026-09-03-v1"
countdlm_road_partition_parent_api_version <-
    "countdlm-road-targeted-stability-2026-09-03-v1"
countdlm_road_partition_parent_zip_sha256 <-
    "daa90cfa866101bf09366e8c9c7f5e47ed665f6e6395fe98d452eeaa3b5198d7"
countdlm_road_partition_parent_manifest_sha256 <-
    "1a9679326639321e067d488e4b76c5397143fdf745785565bac0e40e4c5108a3"
countdlm_road_partition_parent_result_sha256 <-
    "91997362bb1f0c66ab3ba65efa0e128aa0bf7bc171e97dcfb1ea82ca0411ce44"
countdlm_road_partition_parent_completion_sha256 <-
    "9dd81cdc40c7c6c670e3a307985f350908d48b19d8236efd517f3b60fa89dd73"
countdlm_road_partition_parent_registration_sha256 <-
    "3a11d69bd64362cd441c1c48fbc5025b7504729bf4f3857aefae8708510695c0"
countdlm_road_partition_parent_blinded_sha256 <-
    "6fd5e8ee257a8ab99e13a8d45b57a878d5aab05a5f8567d5ef772aaa5194053f"
countdlm_road_partition_parent_config_signature <-
    "25a4e9e5098769cafbd57e459494c4353b02e82c0b7d2624aaa514db80de0ff3"
countdlm_road_partition_neutral_theta_sha256 <-
    "f64af3dd151911d9e176188acf7404ddaae963f4611b53737c6975f312ffec60"
countdlm_road_partition_neutral_gamma_sha256 <-
    "5337ae0b2ba7653f5539a1b7571dc220c86d04407a9302879e354476fbe6945a"
countdlm_road_partition_neutral_joint_sha256 <-
    "e8de3be529e0d3b536adc946c598ef0d05b265d7833a386d018f8c083ebc4790"
countdlm_road_partition_methods <- c("GMDE-W", "GMDE-C", "Euc-MDE")
countdlm_road_partition_basis_sha256 <- c(
    "GMDE-W" =
        "111b28887bfc8d12389f8f0f211276754671ed0d9520cb231e990949c04d62b0",
    "GMDE-C" =
        "fa0980e4be680969db8d7821b0262dcd66767fc8fb89680121dba698cf25ba06",
    "Euc-MDE" =
        "6cba1a02dbc2698eec4def54535e118156e77098750ef7a50ce881a87b2697c7"
)

countdlm_road_partition_mode_specification <- function() {
    data.frame(
        method = rep(countdlm_road_partition_methods, each = 2L),
        mode_id = rep(c("A", "B"), times = 3L),
        parent_chain_id = rep(1:2, times = 3L),
        parent_fit_member = c(
            "01-GMDE-W-chain-1-fit.rds",
            "04-GMDE-W-chain-2-fit.rds",
            "02-GMDE-C-chain-1-fit.rds",
            "05-GMDE-C-chain-2-fit.rds",
            "03-Euc-MDE-chain-1-fit.rds",
            "06-Euc-MDE-chain-2-fit.rds"
        ),
        representative_iteration = c(
            7646L, 7598L, 7397L, 7264L, 7481L, 7362L
        ),
        occupied_experts = c(6L, 5L, 3L, 5L, 6L, 5L),
        substantive_experts = c(5L, 4L, 3L, 5L, 6L, 5L),
        partition_sha256 = c(
            "63ff60b740f1dfcca7cda950b626dc934748ca5c5c339409092fd6130530a2d2",
            "f1c7144c308e49e4d9a3193520ea1db291c0a1f0c11f7e163693dffe2160a5b6",
            "93261c1fc09550b82ecfcfcfdf509bd0cba7977db9c25e20b6c6f90c499714ad",
            "149e0563ab9a87ed12a0ff1e2441734ff655149915edbadd1c81f844e75de41c",
            "b4c0983afbaeda9558ee59d9facb0ced7928ec5c3dab071cd6ba74988fe01b8e",
            "c867d3a6f6d142502cbffe6a5e991089f02990b62f2f29805e48466d0e4888dd"
        ),
        stringsAsFactors = FALSE
    )
}

countdlm_road_partition_task_seeds <- c(
    "GMDE-W-seed-1" = 2026094101L,
    "GMDE-W-seed-2" = 2026094102L,
    "GMDE-C-seed-1" = 2026094201L,
    "GMDE-C-seed-2" = 2026094202L,
    "Euc-MDE-seed-1" = 2026094301L,
    "Euc-MDE-seed-2" = 2026094302L
)

countdlm_road_partition_diagnostic_tasks <- function() {
    rows <- list()
    for (seed_id in 1:2) {
        for (method in countdlm_road_partition_methods) {
            for (mode_id in c("A", "B")) {
                seed_name <- paste(method, "seed", seed_id, sep = "-")
                rows[[length(rows) + 1L]] <- data.frame(
                    task_id = paste(
                        method, paste0("mode-", mode_id),
                        paste0("seed-", seed_id), sep = "-"
                    ),
                    method = method,
                    mode_id = mode_id,
                    seed_id = seed_id,
                    seed = unname(countdlm_road_partition_task_seeds[[
                        seed_name
                    ]]),
                    batch_id = seed_id,
                    stringsAsFactors = FALSE
                )
            }
        }
    }
    value <- do.call(rbind, rows)
    mode_spec <- countdlm_road_partition_mode_specification()
    key <- paste(value$method, value$mode_id, sep = "|")
    mode_key <- paste(mode_spec$method, mode_spec$mode_id, sep = "|")
    value$parent_partition_sha256 <- mode_spec$partition_sha256[
        match(key, mode_key)
    ]
    rownames(value) <- NULL
    value
}

countdlm_road_canonical_partition <- function(Z) {
    if (!is.numeric(Z) || !length(Z) || any(!is.finite(Z)) ||
        any(Z != round(Z))) {
        stop("Z must be a finite whole-number partition.", call. = FALSE)
    }
    as.integer(match(as.integer(Z), unique(as.integer(Z))))
}

countdlm_road_partition_hash <- function(Z) {
    digest::digest(
        countdlm_road_canonical_partition(Z),
        algo = "sha256", serialize = TRUE
    )
}

countdlm_road_dahl_partition <- function(Z, global_index = seq_len(nrow(Z))) {
    Z <- as.matrix(Z)
    if (!is.numeric(Z) || nrow(Z) < 2L || ncol(Z) < 2L ||
        any(!is.finite(Z)) || any(Z != round(Z)) ||
        length(global_index) != nrow(Z)) {
        stop("Dahl selection requires a finite allocation matrix and index.",
             call. = FALSE)
    }
    similarity <- countdlm_road_posterior_similarity(Z)
    upper <- upper.tri(similarity)
    target <- similarity[upper]
    loss <- vapply(seq_len(nrow(Z)), function(index) {
        membership <- outer(Z[index, ], Z[index, ], "==")
        sum((membership[upper] - target)^2)
    }, numeric(1))
    selected <- which.min(loss)
    partition <- countdlm_road_canonical_partition(Z[selected, ])
    list(
        partition = partition,
        local_iteration = selected,
        global_iteration = as.integer(global_index[[selected]]),
        loss = loss[[selected]],
        psm = similarity,
        partition_sha256 = countdlm_road_partition_hash(partition)
    )
}

countdlm_road_partition_neutral_initialization <- function(Y, Fmat) {
    Y <- as.matrix(Y)
    Fmat <- as.matrix(Fmat)
    if (!identical(dim(Y), c(100L, 168L)) ||
        !identical(dim(Fmat), c(168L, 2L)) ||
        any(!is.finite(Y)) || any(Y < 0) ||
        any(Y != round(Y)) || any(!is.finite(Fmat))) {
        stop("The neutral initialization requires the registered blinded data.",
             call. = FALSE)
    }
    theta <- array(0, c(10L, 168L, 2L))
    theta[, , 1L] <- log(mean(Y) + 0.1)
    gamma <- matrix(0, 40L, 9L)
    observed <- c(
        theta = digest::digest(theta, algo = "sha256", serialize = TRUE),
        gamma = digest::digest(gamma, algo = "sha256", serialize = TRUE),
        joint = digest::digest(
            list(theta = theta, gamma = gamma),
            algo = "sha256", serialize = TRUE
        )
    )
    expected <- c(
        theta = countdlm_road_partition_neutral_theta_sha256,
        gamma = countdlm_road_partition_neutral_gamma_sha256,
        joint = countdlm_road_partition_neutral_joint_sha256
    )
    if (!identical(observed, expected)) {
        stop("The registered neutral initialization hash does not match.",
             call. = FALSE)
    }
    list(theta = theta, gamma = gamma, sha256 = observed)
}

countdlm_road_validate_partition_parent <- function(
    result, completion, registration, blinded
) {
    required_result <- c(
        "api_version", "config", "registration", "runtime",
        "targeted_gate", "truth_metrics_computed", "all_tasks_ok",
        "failed_tasks", "warning_tasks", "all_parent_prefixes_reproduced",
        "all_targeted_gates_pass", "ready_for_formal_design_review",
        "eligible_for_formal_freeze", "formal_simulation_launched"
    )
    if (!is.list(result) || !all(required_result %in% names(result)) ||
        !identical(result$api_version,
                   countdlm_road_partition_parent_api_version) ||
        !is.list(result$config) ||
        !identical(result$config$config_signature,
                   countdlm_road_partition_parent_config_signature) ||
        !identical(result$config$K_fit, 10L) ||
        !identical(result$config$basis_m, 40L) ||
        !identical(result$config$selected_rho, 1) ||
        !identical(result$config$iterations, 8000L) ||
        !identical(result$config$burn, 4000L) ||
        !identical(result$config$methods,
                   countdlm_road_partition_methods) ||
        !isTRUE(result$all_tasks_ok) ||
        !identical(as.integer(result$failed_tasks), 0L) ||
        !identical(as.integer(result$warning_tasks), 0L) ||
        !isTRUE(result$all_parent_prefixes_reproduced) ||
        !identical(result$all_targeted_gates_pass, FALSE) ||
        !identical(result$ready_for_formal_design_review, FALSE) ||
        !identical(result$truth_metrics_computed, FALSE) ||
        !identical(result$eligible_for_formal_freeze, FALSE) ||
        !identical(result$formal_simulation_launched, FALSE) ||
        !is.data.frame(result$runtime) || nrow(result$runtime) != 6L ||
        any(result$runtime$status != "ok") ||
        any(result$runtime$warning_count != 0L) ||
        !is.data.frame(result$targeted_gate) ||
        !identical(result$targeted_gate$variant_id,
                   countdlm_road_partition_methods) ||
        any(result$targeted_gate$targeted_gate_passed != FALSE)) {
        stop("The targeted parent result is not the reviewed failed screen.",
             call. = FALSE)
    }
    if (!is.list(completion) ||
        !identical(completion$api_version,
                   countdlm_road_partition_parent_api_version) ||
        !identical(completion$status,
                   "complete-with-targeted-stability-flags") ||
        !identical(completion$all_targeted_gates_pass, FALSE) ||
        !identical(completion$ready_for_formal_design_review, FALSE) ||
        !identical(completion$truth_metrics_computed, FALSE) ||
        !identical(completion$formal_simulation_launched, FALSE) ||
        !identical(completion$checksums_cover_payload_before_this_marker,
                   "CHECKSUMS.sha256")) {
        stop("The targeted parent completion marker is invalid.",
             call. = FALSE)
    }
    if (!is.list(registration) ||
        !identical(registration, result$registration) ||
        !identical(registration$config$config_signature,
                   countdlm_road_partition_parent_config_signature) ||
        !is.character(registration$source_sha256) ||
        !length(registration$source_sha256)) {
        stop("The targeted parent registration is invalid.", call. = FALSE)
    }
    required_blinded <- c(
        "api_version", "Y", "Fmat", "Y_Fmat_sha256",
        "truth_fields_stored", "truth_metrics_computed",
        "initialization_sha256", "Z_init"
    )
    if (!is.list(blinded) ||
        !all(required_blinded %in% names(blinded)) ||
        !identical(blinded$api_version,
                   countdlm_road_partition_parent_api_version) ||
        !identical(blinded$Y_Fmat_sha256,
                   countdlm_road_stability_data_sha256) ||
        !identical(blinded$truth_fields_stored, FALSE) ||
        !identical(blinded$truth_metrics_computed, FALSE) ||
        !identical(unname(blinded$initialization_sha256),
                   unname(countdlm_road_stability_initialization_sha256)) ||
        !is.matrix(blinded$Y) || !identical(dim(blinded$Y), c(100L, 168L)) ||
        !is.matrix(blinded$Fmat) ||
        !identical(dim(blinded$Fmat), c(168L, 2L))) {
        stop("The targeted parent blinded-data payload is invalid.",
             call. = FALSE)
    }
    invisible(TRUE)
}

countdlm_road_partition_parent_evidence <- function(zip_file) {
    zip_file <- normalizePath(zip_file, winslash = "/", mustWork = TRUE)
    if (!identical(countdlm_road_sha256(zip_file),
                   countdlm_road_partition_parent_zip_sha256)) {
        stop("The targeted parent ZIP does not match the reviewed archive.",
             call. = FALSE)
    }
    members <- c(
        manifest = "CHECKSUMS.sha256",
        result = "road-targeted-stability-result.rds",
        completion = "RUN-COMPLETE.rds",
        registration = "road-targeted-stability-registration.rds",
        blinded = "road-targeted-stability-blinded-data.rds"
    )
    raw <- lapply(members, function(member) {
        countdlm_road_stability_zip_member_raw(zip_file, member)
    })
    observed <- vapply(
        raw, digest::digest, character(1),
        algo = "sha256", serialize = FALSE
    )
    expected <- c(
        manifest = countdlm_road_partition_parent_manifest_sha256,
        result = countdlm_road_partition_parent_result_sha256,
        completion = countdlm_road_partition_parent_completion_sha256,
        registration = countdlm_road_partition_parent_registration_sha256,
        blinded = countdlm_road_partition_parent_blinded_sha256
    )
    if (!identical(observed, expected)) {
        stop("A targeted parent member failed its SHA-256 check.",
             call. = FALSE)
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
    blinded <- countdlm_road_stability_read_rds_raw(
        raw$blinded, members[["blinded"]]
    )
    countdlm_road_validate_partition_parent(
        result, completion, registration, blinded
    )
    mode_spec <- countdlm_road_partition_mode_specification()
    modes <- vector("list", nrow(mode_spec))
    names(modes) <- paste(mode_spec$method, mode_spec$mode_id, sep = "|")
    for (index in seq_len(nrow(mode_spec))) {
        member <- mode_spec$parent_fit_member[[index]]
        wrapper <- countdlm_road_stability_read_rds_raw(
            countdlm_road_stability_zip_member_raw(zip_file, member),
            member
        )
        if (!is.list(wrapper) ||
            !identical(wrapper$api_version,
                       countdlm_road_partition_parent_api_version) ||
            !identical(wrapper$variant_id, mode_spec$method[[index]]) ||
            !identical(wrapper$chain_id,
                       mode_spec$parent_chain_id[[index]]) ||
            !countdlm_road_truth_blinding_ok(wrapper$fit, 8000L)) {
            stop("A targeted parent fit failed its wrapper contract.",
                 call. = FALSE)
        }
        selected <- countdlm_road_dahl_partition(
            wrapper$fit$Z[7001:8000, , drop = FALSE],
            global_index = 7001:8000
        )
        size <- tabulate(selected$partition)
        if (!identical(selected$global_iteration,
                       mode_spec$representative_iteration[[index]]) ||
            !identical(length(size),
                       mode_spec$occupied_experts[[index]]) ||
            !identical(sum(size >= 5L),
                       mode_spec$substantive_experts[[index]]) ||
            !identical(selected$partition_sha256,
                       mode_spec$partition_sha256[[index]])) {
            stop("A registered Dahl partition could not be reproduced.",
                 call. = FALSE)
        }
        modes[[index]] <- selected$partition
        rm(wrapper, selected)
        invisible(gc(verbose = FALSE))
    }
    list(
        zip_file = zip_file,
        zip_sha256 = countdlm_road_partition_parent_zip_sha256,
        manifest_sha256 = observed[["manifest"]],
        result_sha256 = observed[["result"]],
        completion_sha256 = observed[["completion"]],
        registration_sha256 = observed[["registration"]],
        blinded_sha256 = observed[["blinded"]],
        execution_environment = registration$execution_environment,
        source_sha256 = registration$source_sha256,
        blinded = blinded,
        modes = modes,
        mode_specification = mode_spec,
        parent_result = result,
        parent_completion = completion
    )
}

countdlm_road_validate_partition_diagnostic_config <- function(config) {
    expected_tasks <- countdlm_road_partition_diagnostic_tasks()
    expected_modes <- countdlm_road_partition_mode_specification()
    valid_signature <- inherits(
        config, "countdlm_road_partition_diagnostic_config"
    ) && identical(
        config$api_version, countdlm_road_partition_diagnostic_api_version
    ) && is.character(config$config_signature) &&
        length(config$config_signature) == 1L && identical(
            config$config_signature, countdlm_road_config_signature(config)
        )
    if (!isTRUE(valid_signature) ||
        !identical(config$sampler_version,
                   countdlm_gmde_sampler_version) ||
        !identical(config$parent_api_version,
                   countdlm_road_partition_parent_api_version) ||
        !identical(config$parent_zip_sha256,
                   countdlm_road_partition_parent_zip_sha256) ||
        !identical(config$parent_manifest_sha256,
                   countdlm_road_partition_parent_manifest_sha256) ||
        !identical(config$parent_result_sha256,
                   countdlm_road_partition_parent_result_sha256) ||
        !identical(config$parent_completion_sha256,
                   countdlm_road_partition_parent_completion_sha256) ||
        !identical(config$parent_registration_sha256,
                   countdlm_road_partition_parent_registration_sha256) ||
        !identical(config$parent_blinded_sha256,
                   countdlm_road_partition_parent_blinded_sha256) ||
        !identical(config$parent_config_signature,
                   countdlm_road_partition_parent_config_signature) ||
        !identical(config$context_sha256,
                   countdlm_road_context_sha256) ||
        !identical(config$data_sha256,
                   countdlm_road_stability_data_sha256) ||
        !identical(config$n, 100L) || !identical(config$TT, 168L) ||
        !identical(config$K_fit, 10L) ||
        !identical(config$basis_m, 40L) ||
        !identical(config$methods, countdlm_road_partition_methods) ||
        !identical(config$mode_specification, expected_modes) ||
        !identical(config$tasks, expected_tasks) ||
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
                   countdlm_road_partition_neutral_theta_sha256) ||
        !identical(config$neutral_gamma_sha256,
                   countdlm_road_partition_neutral_gamma_sha256) ||
        !identical(config$neutral_joint_sha256,
                   countdlm_road_partition_neutral_joint_sha256) ||
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
            "The partition diagnostic configuration is invalid or was ",
            "modified after construction.", call. = FALSE
        )
    }
    invisible(config)
}

#' Construct the paired partition-mode diagnostic configuration
#'
#' The experiment keeps the reviewed `K = 10`, graph-basis rank 40, fixed
#' `rho = 1`, exact sampler, blinded data, and scientific thresholds.  For
#' each graph-gating method it crosses two registered Dahl partitions with two
#' paired seeds while holding the continuous initialization exactly fixed.
#'
#' @param context_file Approved external D-017 road-context RDS.
#' @param parent_targeted_zip Exact returned targeted-stability ZIP.
#' @param output_dir Brand-new external output directory.
#' @param execution_source_dir Directory containing frozen `R/*.R` sources.
#' @return A signed, fixed diagnostic configuration.
#' @export
countdlm_road_partition_diagnostic_config <- function(
    context_file, parent_targeted_zip, output_dir,
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
        api_version = countdlm_road_partition_diagnostic_api_version,
        sampler_version = countdlm_gmde_sampler_version,
        scientific_role = paste(
            "truth-blinded paired partition-mode restart diagnostic;",
            "not an inferential, tuning-selection, or formal simulation"
        ),
        context_file = normalizePath(
            context_file, winslash = "/", mustWork = TRUE
        ),
        context_sha256 = countdlm_road_context_sha256,
        parent_targeted_zip = normalizePath(
            parent_targeted_zip, winslash = "/", mustWork = TRUE
        ),
        parent_api_version = countdlm_road_partition_parent_api_version,
        parent_zip_sha256 = countdlm_road_partition_parent_zip_sha256,
        parent_manifest_sha256 =
            countdlm_road_partition_parent_manifest_sha256,
        parent_result_sha256 = countdlm_road_partition_parent_result_sha256,
        parent_completion_sha256 =
            countdlm_road_partition_parent_completion_sha256,
        parent_registration_sha256 =
            countdlm_road_partition_parent_registration_sha256,
        parent_blinded_sha256 =
            countdlm_road_partition_parent_blinded_sha256,
        parent_config_signature =
            countdlm_road_partition_parent_config_signature,
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
        tasks = countdlm_road_partition_diagnostic_tasks(),
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
        neutral_theta_sha256 =
            countdlm_road_partition_neutral_theta_sha256,
        neutral_gamma_sha256 =
            countdlm_road_partition_neutral_gamma_sha256,
        neutral_joint_sha256 =
            countdlm_road_partition_neutral_joint_sha256,
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
        config, class = "countdlm_road_partition_diagnostic_config"
    )
    countdlm_road_validate_partition_diagnostic_config(config)
    config
}

countdlm_road_assert_partition_output <- function(
    output_dir, directories, config_signature, stage
) {
    required_directories <- unique(c(output_dir, directories))
    required_files <- file.path(
        output_dir,
        c("road-partition-diagnostic-registration.rds", "RUN-STARTED.rds")
    )
    missing <- c(
        required_directories[!dir.exists(required_directories)],
        required_files[!file.exists(required_files)]
    )
    if (length(missing)) {
        stop(
            "The registered partition-diagnostic output tree is unavailable ",
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
            "The partition-diagnostic output identity could not be verified ",
            "during ", stage, ": ", detail,
            ". It will not be recreated, and this run is invalid.",
            call. = FALSE
        )
    }
    invisible(TRUE)
}

countdlm_road_psm_distance <- function(first, second, config) {
    first <- as.matrix(first)
    second <- as.matrix(second)
    if (!identical(dim(first), dim(second)) || nrow(first) < 2L ||
        nrow(first) != ncol(first) || any(!is.finite(c(first, second)))) {
        stop("PSM distance inputs are incompatible.", call. = FALSE)
    }
    upper <- upper.tri(first)
    difference <- first[upper] - second[upper]
    rms <- sqrt(mean(difference^2))
    mean_abs <- mean(abs(difference))
    Q <- max(
        rms / config$psm_rms_limit,
        mean_abs / config$psm_mean_abs_limit
    )
    c(
        psm_rms = rms,
        psm_mean_abs = mean_abs,
        standardized_Q = Q,
        within_limits = Q <= 1
    )
}

countdlm_road_partition_dominant_state <- function(x, K = 10L) {
    x <- as.integer(x)
    if (!length(x) || any(!is.finite(x)) || any(x < 1L | x > K)) {
        stop("A finite nonempty cluster-count trace is required.",
             call. = FALSE)
    }
    counts <- tabulate(x, nbins = K)
    value <- which.max(counts)
    runs <- rle(x)
    value_runs <- runs$lengths[runs$values == value]
    list(
        value = as.integer(value),
        mass = counts[[value]] / length(x),
        maximum_run = if (length(value_runs)) {
            as.integer(max(value_runs))
        } else 0L
    )
}

countdlm_road_partition_tv <- function(first, second, K = 10L) {
    first_probability <- tabulate(as.integer(first), nbins = K) / length(first)
    second_probability <-
        tabulate(as.integer(second), nbins = K) / length(second)
    0.5 * sum(abs(first_probability - second_probability))
}

countdlm_road_partition_loglik_separation <- function(first, second) {
    pooled_sd <- sqrt((stats::var(first) + stats::var(second)) / 2)
    difference <- abs(mean(first) - mean(second))
    if (!is.finite(pooled_sd) || pooled_sd <= 0) {
        if (difference == 0) 0 else Inf
    } else difference / pooled_sd
}

countdlm_road_partition_movement <- function(Z) {
    Z <- as.matrix(Z)
    if (nrow(Z) < 2L || any(!is.finite(Z)) || any(Z != round(Z))) {
        stop("At least two finite partition draws are required.",
             call. = FALSE)
    }
    signatures <- apply(Z, 1L, function(partition) {
        paste(countdlm_road_canonical_partition(partition), collapse = ":")
    })
    adjacent_ari <- vapply(seq.int(2L, nrow(Z)), function(index) {
        gmde_adjusted_rand(Z[index - 1L, ], Z[index, ])
    }, numeric(1))
    list(
        median_adjacent_ari = stats::median(adjacent_ari),
        fraction_exactly_unchanged = mean(
            signatures[-1L] == signatures[-length(signatures)]
        ),
        unique_partitions = length(unique(signatures)),
        maximum_identical_partition_run = as.integer(
            max(rle(signatures)$lengths)
        )
    )
}

countdlm_road_partition_mcmc_precision <- function(chains) {
    values <- do.call(cbind, lapply(chains, as.numeric))
    if (length(unique(as.numeric(values))) == 1L) {
        return(c(bulk_ess = NA_real_, tail_ess = NA_real_,
                 mcse_mean = NA_real_))
    }
    c(
        bulk_ess = posterior::ess_bulk(values),
        tail_ess = posterior::ess_tail(values),
        mcse_mean = posterior::mcse_mean(values)
    )
}

countdlm_road_partition_scalar_row <- function(
    fits, method, mode_id, window_name, index, config
) {
    density <- lapply(fits, function(fit) fit$observed_loglik[index])
    Kocc <- lapply(fits, function(fit) fit$occupied_experts[index])
    Ksub <- lapply(fits, function(fit) fit$substantive_experts[index])
    density_rhat <- countdlm_road_pilot_rhat(density)
    Kocc_rhat <- countdlm_road_pilot_rhat(Kocc)
    Ksub_rhat <- countdlm_road_pilot_rhat(Ksub)
    density_precision <- countdlm_road_partition_mcmc_precision(density)
    Kocc_precision <- countdlm_road_partition_mcmc_precision(Kocc)
    Ksub_precision <- countdlm_road_partition_mcmc_precision(Ksub)
    density_constant <- length(unique(unlist(density))) == 1L
    Kocc_constant <- length(unique(unlist(Kocc))) == 1L
    Ksub_constant <- length(unique(unlist(Ksub))) == 1L
    Kocc_ok <- Kocc_constant ||
        (is.finite(Kocc_rhat) && Kocc_rhat <= config$count_rhat_limit)
    Ksub_ok <- Ksub_constant ||
        (is.finite(Ksub_rhat) && Ksub_rhat <= config$count_rhat_limit)
    passed <- is.finite(density_rhat) &&
        density_rhat <= config$log_rhat_limit && Kocc_ok && Ksub_ok
    data.frame(
        method = method,
        mode_id = mode_id,
        window = window_name,
        first_iteration = min(index),
        last_iteration = max(index),
        draws_per_seed = length(index),
        observed_loglik_rank_split_rhat = density_rhat,
        Kocc_rank_split_rhat = Kocc_rhat,
        Ksub_rank_split_rhat = Ksub_rhat,
        observed_loglik_bulk_ess = density_precision[["bulk_ess"]],
        observed_loglik_tail_ess = density_precision[["tail_ess"]],
        observed_loglik_mcse_mean = density_precision[["mcse_mean"]],
        Kocc_bulk_ess = Kocc_precision[["bulk_ess"]],
        Kocc_tail_ess = Kocc_precision[["tail_ess"]],
        Kocc_mcse_mean = Kocc_precision[["mcse_mean"]],
        Ksub_bulk_ess = Ksub_precision[["bulk_ess"]],
        Ksub_tail_ess = Ksub_precision[["tail_ess"]],
        Ksub_mcse_mean = Ksub_precision[["mcse_mean"]],
        observed_loglik_constant = density_constant,
        Kocc_constant = Kocc_constant,
        Ksub_constant = Ksub_constant,
        Kocc_informative = !Kocc_constant,
        Ksub_informative = !Ksub_constant,
        scalar_screen_passed = passed,
        stringsAsFactors = FALSE
    )
}

countdlm_road_partition_pair_type <- function(first, second) {
    if (first$seed_id == second$seed_id &&
        first$mode_id != second$mode_id) {
        "same-seed-different-mode"
    } else if (first$seed_id != second$seed_id &&
               first$mode_id == second$mode_id) {
        "same-mode-different-seed"
    } else {
        "different-seed-different-mode"
    }
}

countdlm_road_partition_task_slug <- function(index, task_id) {
    paste0(
        sprintf("%02d", as.integer(index)), "-",
        gsub("[^A-Za-z0-9]+", "-", task_id)
    )
}

countdlm_road_partition_output_tables <- function() {
    c(
        rule_specification = "partition-diagnostic-rule-spec.csv",
        block_summary = "partition-block-summary.csv",
        pairwise_psm = "partition-pairwise-psm.csv",
        pair_persistence = "partition-pair-persistence.csv",
        within_chain_drift = "partition-within-chain-drift.csv",
        start_memory = "partition-start-memory.csv",
        scalar_diagnostics = "partition-scalar-diagnostics.csv",
        cluster_count_frequency = "partition-count-frequency.csv",
        late_representative_partition =
            "partition-late-representative-partition.csv",
        node_psm_difference = "partition-node-psm-difference.csv",
        macro_pairs = "partition-macro-pairs.csv",
        method_decision = "partition-method-decision.csv"
    )
}

countdlm_road_partition_expected_payload <- function(config, source_names) {
    task_slugs <- vapply(
        seq_len(nrow(config$tasks)),
        function(index) countdlm_road_partition_task_slug(
            index, config$tasks$task_id[[index]]
        ),
        character(1)
    )
    sort(c(
        "FROZEN_SOURCE_SHA256.csv",
        "road-partition-diagnostic-registration.rds",
        "RUN-STARTED.rds",
        "road-partition-diagnostic-blinded-input.rds",
        "partition-mode-specification.csv",
        "partition-diagnostic-runtime.csv",
        "paired-initialization-audit.csv",
        unname(countdlm_road_partition_output_tables()),
        "road-partition-diagnostic-result.rds",
        "road-partition-diagnostic-report.txt",
        file.path("chains", paste0(task_slugs, "-fit.rds")),
        file.path(
            "diagnostics", paste0(task_slugs, "-diagnostic.rds")
        ),
        file.path("frozen-source", "R", source_names)
    ))
}

countdlm_road_assert_partition_payload <- function(
    output_dir, config, source_names
) {
    expected <- countdlm_road_partition_expected_payload(
        config, source_names
    )
    observed <- sort(list.files(
        output_dir, recursive = TRUE, full.names = FALSE,
        include.dirs = FALSE, all.files = TRUE, no.. = TRUE
    ))
    missing <- setdiff(expected, observed)
    unexpected <- setdiff(observed, expected)
    size <- file.info(file.path(output_dir, expected))$size
    if (length(missing) || length(unexpected) ||
        anyNA(size) || any(size <= 0)) {
        stop(
            "The completed diagnostic payload failed its exact inventory ",
            "audit. Missing: ", paste(missing, collapse = ", "),
            "; unexpected: ", paste(unexpected, collapse = ", "),
            ".", call. = FALSE
        )
    }
    invisible(expected)
}

countdlm_road_partition_rule_specification <- function(config) {
    data.frame(
        rule_id = c(
            "standardized-Q", "resolved-under-control",
            "partition-start-sensitive", "stochastic-or-multimode",
            "macro-allocation-mode", "distance-envelope-decline-candidate",
            "literal-local-freeze", "local-moves-but-poor-macro-mixing",
            "formal-authority"
        ),
        fixed_definition = c(
            sprintf(
                "max(PSM_RMS/%.2f, PSM_MAE/%.2f); pass iff Q <= 1",
                config$psm_rms_limit, config$psm_mean_abs_limit
            ),
            paste(
                "late maximum pairwise Q <= 1; all middle-vs-late drift",
                "Q <= 1; analysis and late scalar screens pass both within",
                "each start and jointly across all four chains; no literal",
                "local-freeze flag"
            ),
            sprintf(
                paste(
                    "both same-seed/different-start late Q > 1; minimum",
                    "start Q exceeds maximum same-start/different-seed Q by",
                    "at least %.2f; both late/middle ratios >= %.2f"
                ), config$start_distance_margin, config$persistence_ratio
            ),
            sprintf(
                paste(
                    "at least one same-start/different-seed late Q > 1 with",
                    "late/middle ratio >= %.2f"
                ), config$persistence_ratio
            ),
            sprintf(
                paste(
                    "a late pair has Q > 1, Ksub TV >= %.2f, each dominant",
                    "Ksub mass >= %.2f, each dominant Ksub run >= %d, and",
                    "loglik mean gap <= %.2f pooled SD"
                ), config$macro_tv_limit, config$macro_dominant_mass,
                config$macro_minimum_run, config$macro_loglik_separation
            ),
            sprintf(
                paste(
                    "maximum pairwise-Q envelope decreases in all three",
                    "blocks, remains > 1 late, and late/settling <= %.2f;",
                    "start, stochastic, macro, and literal-freeze flags absent"
                ), config$persistence_ratio
            ),
            sprintf(
                paste(
                    "at least one late chain has fraction of exactly",
                    "unchanged adjacent partitions >= %.2f"
                ), config$literal_freeze_fraction
            ),
            "macro-allocation-mode is present and no late chain is literally frozen",
            "all diagnostic outcomes require human review and formal_run_authorized is always FALSE"
        ),
        interpretation_limit = c(
            "PSM elements are descriptive and not independent observations",
            "screen resolution is not a convergence proof or formal release",
            "paired seeds share only their RNG start; later consumption may diverge",
            "does not distinguish random divergence from additional posterior modes",
            "a mechanism flag, not proof of a unique posterior mode",
            "different pairs may define the envelope; this does not prove that more iterations suffice",
            "exact partition repetition, not merely high adjacent ARI",
            "local allocation changes coexist with a macro-mode warning",
            "this experiment can never launch or authorize the formal study"
        ),
        stringsAsFactors = FALSE
    )
}

countdlm_road_partition_diagnostics <- function(
    runtime, tasks, output_dir, config
) {
    block_rows <- list()
    pair_rows <- list()
    drift_rows <- list()
    memory_rows <- list()
    scalar_rows <- list()
    macro_rows <- list()
    count_frequency_rows <- list()
    late_partition_rows <- list()
    node_difference_rows <- list()
    decision_rows <- list()

    for (method in config$methods) {
        method_tasks <- tasks[tasks$method == method, , drop = FALSE]
        method_runtime <- runtime[
            match(method_tasks$task_id, runtime$task_id), , drop = FALSE
        ]
        if (nrow(method_runtime) != 4L ||
            any(method_runtime$status != "ok") ||
            any(is.na(method_runtime$fit_file))) {
            stop("Four successful retained fits are required for ", method,
                 ".", call. = FALSE)
        }
        wrappers <- lapply(
            file.path(output_dir, method_runtime$fit_file), readRDS
        )
        fits <- lapply(wrappers, `[[`, "fit")
        if (!all(vapply(
            fits, countdlm_road_truth_blinding_ok, logical(1),
            n_iter = config$iterations
        ))) {
            stop("A retained partition-diagnostic fit failed truth blinding.",
                 call. = FALSE)
        }
        names(fits) <- method_tasks$task_id

        block_psm <- vector("list", length(config$blocks))
        names(block_psm) <- names(config$blocks)
        for (block_name in names(config$blocks)) {
            index <- config$blocks[[block_name]]
            block_psm[[block_name]] <- lapply(fits, function(fit) {
                countdlm_road_posterior_similarity(
                    fit$Z[index, , drop = FALSE]
                )
            })
            for (task_index in seq_len(nrow(method_tasks))) {
                fit <- fits[[task_index]]
                Kocc <- fit$occupied_experts[index]
                Ksub <- fit$substantive_experts[index]
                movement <- countdlm_road_partition_movement(
                    fit$Z[index, , drop = FALSE]
                )
                dominant_Ksub <- countdlm_road_partition_dominant_state(
                    Ksub, config$K_fit
                )
                block_rows[[length(block_rows) + 1L]] <- data.frame(
                    task_id = method_tasks$task_id[[task_index]],
                    method = method,
                    mode_id = method_tasks$mode_id[[task_index]],
                    seed_id = method_tasks$seed_id[[task_index]],
                    block = block_name,
                    first_iteration = min(index),
                    last_iteration = max(index),
                    mean_observed_loglik = mean(
                        fit$observed_loglik[index]
                    ),
                    sd_observed_loglik = stats::sd(
                        fit$observed_loglik[index]
                    ),
                    mean_Kocc = mean(Kocc),
                    median_Kocc = stats::median(Kocc),
                    mean_Ksub = mean(Ksub),
                    median_Ksub = stats::median(Ksub),
                    dominant_Ksub_value = dominant_Ksub$value,
                    dominant_Ksub_mass = dominant_Ksub$mass,
                    dominant_Ksub_maximum_run =
                        dominant_Ksub$maximum_run,
                    median_adjacent_partition_ari =
                        movement$median_adjacent_ari,
                    fraction_exactly_unchanged_partitions =
                        movement$fraction_exactly_unchanged,
                    unique_partitions = movement$unique_partitions,
                    maximum_identical_partition_run =
                        movement$maximum_identical_partition_run,
                    state_acceptance = mean(
                        fit$state_accepted[index, , drop = FALSE],
                        na.rm = TRUE
                    ),
                    mean_ess_bracket_evaluations = mean(
                        fit$ess_bracket_evaluations[index]
                    ),
                    stringsAsFactors = FALSE
                )
                for (count_type in c("Kocc", "Ksub")) {
                    trace <- if (count_type == "Kocc") Kocc else Ksub
                    frequency <- tabulate(trace, nbins = config$K_fit)
                    retained <- which(frequency > 0L)
                    count_frequency_rows[[
                        length(count_frequency_rows) + 1L
                    ]] <- data.frame(
                        task_id = method_tasks$task_id[[task_index]],
                        method = method,
                        mode_id = method_tasks$mode_id[[task_index]],
                        seed_id = method_tasks$seed_id[[task_index]],
                        block = block_name,
                        count_type = count_type,
                        count_value = retained,
                        frequency = frequency[retained],
                        proportion = frequency[retained] / length(trace),
                        stringsAsFactors = FALSE
                    )
                }
                mode_key <- paste(
                    method, method_tasks$mode_id[[task_index]], sep = "|"
                )
                start_psm <- outer(
                    wrappers[[task_index]]$starting_partition,
                    wrappers[[task_index]]$starting_partition, "=="
                )
                memory_distance <- countdlm_road_psm_distance(
                    block_psm[[block_name]][[task_index]],
                    start_psm, config
                )
                memory_rows[[length(memory_rows) + 1L]] <- data.frame(
                    task_id = method_tasks$task_id[[task_index]],
                    method = method,
                    mode_id = method_tasks$mode_id[[task_index]],
                    seed_id = method_tasks$seed_id[[task_index]],
                    start_mode_key = mode_key,
                    block = block_name,
                    psm_rms_from_start = memory_distance[["psm_rms"]],
                    psm_mean_abs_from_start =
                        memory_distance[["psm_mean_abs"]],
                    standardized_Q_from_start =
                        memory_distance[["standardized_Q"]],
                    stringsAsFactors = FALSE
                )
            }

            combinations <- utils::combn(seq_len(nrow(method_tasks)), 2L)
            for (pair_index in seq_len(ncol(combinations))) {
                first_index <- combinations[1L, pair_index]
                second_index <- combinations[2L, pair_index]
                first_task <- method_tasks[first_index, , drop = FALSE]
                second_task <- method_tasks[second_index, , drop = FALSE]
                distance <- countdlm_road_psm_distance(
                    block_psm[[block_name]][[first_index]],
                    block_psm[[block_name]][[second_index]], config
                )
                pair_rows[[length(pair_rows) + 1L]] <- data.frame(
                    method = method,
                    block = block_name,
                    first_task_id = first_task$task_id,
                    second_task_id = second_task$task_id,
                    first_mode_id = first_task$mode_id,
                    second_mode_id = second_task$mode_id,
                    first_seed_id = first_task$seed_id,
                    second_seed_id = second_task$seed_id,
                    pair_type = countdlm_road_partition_pair_type(
                        first_task, second_task
                    ),
                    psm_rms = distance[["psm_rms"]],
                    psm_mean_abs = distance[["psm_mean_abs"]],
                    standardized_Q = distance[["standardized_Q"]],
                    within_limits = as.logical(
                        distance[["within_limits"]]
                    ),
                    stringsAsFactors = FALSE
                )
                if (block_name == "late") {
                    difference <- abs(
                        block_psm[[block_name]][[first_index]] -
                            block_psm[[block_name]][[second_index]]
                    )
                    diag(difference) <- NA_real_
                    node_difference_rows[[
                        length(node_difference_rows) + 1L
                    ]] <- data.frame(
                        method = method,
                        first_task_id = first_task$task_id,
                        second_task_id = second_task$task_id,
                        pair_type = countdlm_road_partition_pair_type(
                            first_task, second_task
                        ),
                        node_id = seq_len(nrow(difference)),
                        node_psm_mean_abs_difference = rowMeans(
                            difference, na.rm = TRUE
                        ),
                        node_psm_rms_difference = sqrt(rowMeans(
                            difference^2, na.rm = TRUE
                        )),
                        stringsAsFactors = FALSE
                    )
                }
            }
        }

        for (task_index in seq_len(nrow(method_tasks))) {
            representative <- countdlm_road_dahl_partition(
                fits[[task_index]]$Z[
                    config$blocks$late, , drop = FALSE
                ],
                global_index = config$blocks$late
            )
            size <- tabulate(
                representative$partition, nbins = config$K_fit
            )
            late_partition_rows[[length(late_partition_rows) + 1L]] <-
                data.frame(
                    task_id = method_tasks$task_id[[task_index]],
                    method = method,
                    mode_id = method_tasks$mode_id[[task_index]],
                    seed_id = method_tasks$seed_id[[task_index]],
                    representative_iteration =
                        representative$global_iteration,
                    partition_sha256 =
                        representative$partition_sha256,
                    node_id = seq_along(representative$partition),
                    cluster_label = representative$partition,
                    cluster_size = size[representative$partition],
                    stringsAsFactors = FALSE
                )
        }

        for (task_index in seq_len(nrow(method_tasks))) {
            distance <- countdlm_road_psm_distance(
                block_psm$middle[[task_index]],
                block_psm$late[[task_index]], config
            )
            drift_rows[[length(drift_rows) + 1L]] <- data.frame(
                task_id = method_tasks$task_id[[task_index]],
                method = method,
                mode_id = method_tasks$mode_id[[task_index]],
                seed_id = method_tasks$seed_id[[task_index]],
                psm_rms_middle_vs_late = distance[["psm_rms"]],
                psm_mean_abs_middle_vs_late =
                    distance[["psm_mean_abs"]],
                standardized_Q_middle_vs_late =
                    distance[["standardized_Q"]],
                drift_screen_passed = as.logical(
                    distance[["within_limits"]]
                ),
                stringsAsFactors = FALSE
            )
        }

        for (mode_id in c("A", "B")) {
            selected <- which(method_tasks$mode_id == mode_id)
            scalar_rows[[length(scalar_rows) + 1L]] <-
                countdlm_road_partition_scalar_row(
                    fits[selected], method, mode_id,
                    "analysis-1001-3000", config$analysis_window, config
                )
            scalar_rows[[length(scalar_rows) + 1L]] <-
                countdlm_road_partition_scalar_row(
                    fits[selected], method, mode_id,
                    "late-2001-3000", config$blocks$late, config
                )
        }
        scalar_rows[[length(scalar_rows) + 1L]] <-
            countdlm_road_partition_scalar_row(
                fits, method, "ALL", "analysis-1001-3000",
                config$analysis_window, config
            )
        scalar_rows[[length(scalar_rows) + 1L]] <-
            countdlm_road_partition_scalar_row(
                fits, method, "ALL", "late-2001-3000",
                config$blocks$late, config
            )

        method_pairs <- do.call(rbind, pair_rows)[
            do.call(rbind, pair_rows)$method == method &
                do.call(rbind, pair_rows)$block == "late", , drop = FALSE
        ]
        combinations <- utils::combn(seq_len(nrow(method_tasks)), 2L)
        for (pair_index in seq_len(ncol(combinations))) {
            first_index <- combinations[1L, pair_index]
            second_index <- combinations[2L, pair_index]
            first_fit <- fits[[first_index]]
            second_fit <- fits[[second_index]]
            index <- config$blocks$late
            first_Ksub <- first_fit$substantive_experts[index]
            second_Ksub <- second_fit$substantive_experts[index]
            row <- method_pairs[
                method_pairs$first_task_id ==
                    method_tasks$task_id[[first_index]] &
                method_pairs$second_task_id ==
                    method_tasks$task_id[[second_index]], , drop = FALSE
            ]
            tv <- countdlm_road_partition_tv(first_Ksub, second_Ksub)
            first_dominant <- countdlm_road_partition_dominant_state(
                first_Ksub, config$K_fit
            )
            second_dominant <- countdlm_road_partition_dominant_state(
                second_Ksub, config$K_fit
            )
            loglik_separation <- countdlm_road_partition_loglik_separation(
                first_fit$observed_loglik[index],
                second_fit$observed_loglik[index]
            )
            macro <- row$standardized_Q[[1L]] > 1 &&
                tv >= config$macro_tv_limit &&
                min(first_dominant$mass, second_dominant$mass) >=
                    config$macro_dominant_mass &&
                min(first_dominant$maximum_run,
                    second_dominant$maximum_run) >=
                    config$macro_minimum_run &&
                loglik_separation <= config$macro_loglik_separation
            macro_rows[[length(macro_rows) + 1L]] <- data.frame(
                method = method,
                first_task_id = method_tasks$task_id[[first_index]],
                second_task_id = method_tasks$task_id[[second_index]],
                pair_type = row$pair_type[[1L]],
                standardized_Q = row$standardized_Q[[1L]],
                Ksub_total_variation = tv,
                first_dominant_Ksub_value = first_dominant$value,
                second_dominant_Ksub_value = second_dominant$value,
                first_dominant_Ksub_mass = first_dominant$mass,
                second_dominant_Ksub_mass = second_dominant$mass,
                first_dominant_Ksub_maximum_run =
                    first_dominant$maximum_run,
                second_dominant_Ksub_maximum_run =
                    second_dominant$maximum_run,
                observed_loglik_separation_pooled_sd = loglik_separation,
                macro_allocation_mode_flag = macro,
                stringsAsFactors = FALSE
            )
        }

        all_pairs <- do.call(rbind, pair_rows)
        all_pairs <- all_pairs[all_pairs$method == method, , drop = FALSE]
        D <- vapply(names(config$blocks), function(block_name) {
            max(all_pairs$standardized_Q[all_pairs$block == block_name])
        }, numeric(1))
        late_pairs <- all_pairs[all_pairs$block == "late", , drop = FALSE]
        middle_pairs <- all_pairs[
            all_pairs$block == "middle", , drop = FALSE
        ]
        B_late <- late_pairs[
            late_pairs$pair_type == "same-seed-different-mode",
            , drop = FALSE
        ]
        W_late <- late_pairs[
            late_pairs$pair_type == "same-mode-different-seed",
            , drop = FALSE
        ]
        B_middle <- middle_pairs[
            middle_pairs$pair_type == "same-seed-different-mode",
            , drop = FALSE
        ]
        W_middle <- middle_pairs[
            middle_pairs$pair_type == "same-mode-different-seed",
            , drop = FALSE
        ]
        B_ratio <- B_late$standardized_Q / B_middle$standardized_Q
        W_ratio <- W_late$standardized_Q / W_middle$standardized_Q
        method_drift <- do.call(rbind, drift_rows)
        method_drift <- method_drift[
            method_drift$method == method, , drop = FALSE
        ]
        method_scalar <- do.call(rbind, scalar_rows)
        method_scalar <- method_scalar[
            method_scalar$method == method, , drop = FALSE
        ]
        method_macro <- do.call(rbind, macro_rows)
        method_macro <- method_macro[
            method_macro$method == method, , drop = FALSE
        ]
        resolved <- D[["late"]] <= 1 &&
            all(method_drift$drift_screen_passed) &&
            all(method_scalar$scalar_screen_passed)
        start_sensitive <- nrow(B_late) == 2L &&
            all(B_late$standardized_Q > 1) &&
            min(B_late$standardized_Q) >
                max(W_late$standardized_Q) +
                    config$start_distance_margin &&
            all(B_ratio >= config$persistence_ratio)
        stochastic <- any(
            W_late$standardized_Q > 1 &
                W_ratio >= config$persistence_ratio
        )
        macro <- any(method_macro$macro_allocation_mode_flag)
        envelope_decline <- D[["settling"]] > D[["middle"]] &&
            D[["middle"]] > D[["late"]] && D[["late"]] > 1 &&
            D[["late"]] / D[["settling"]] <=
                config$persistence_ratio &&
            !start_sensitive && !stochastic && !macro
        late_block <- do.call(rbind, block_rows)
        late_block <- late_block[
            late_block$method == method & late_block$block == "late",
            , drop = FALSE
        ]
        literal_freeze_count <- sum(
            late_block$fraction_exactly_unchanged_partitions >=
                config$literal_freeze_fraction,
            na.rm = TRUE
        )
        literal_freeze <- literal_freeze_count > 0L
        local_moves_macro <- macro && literal_freeze_count == 0L
        resolved <- resolved && !literal_freeze
        envelope_decline <- envelope_decline && !literal_freeze
        B_late <- B_late[order(B_late$first_seed_id), , drop = FALSE]
        B_middle <- B_middle[order(B_middle$first_seed_id), , drop = FALSE]
        W_late <- W_late[order(W_late$first_mode_id), , drop = FALSE]
        W_middle <- W_middle[order(W_middle$first_mode_id), , drop = FALSE]
        B_ratio <- B_late$standardized_Q / B_middle$standardized_Q
        W_ratio <- W_late$standardized_Q / W_middle$standardized_Q
        late_argmax <- late_pairs[which.max(
            late_pairs$standardized_Q
        ), , drop = FALSE]
        flags <- c(
            if (resolved) "resolved-under-control",
            if (start_sensitive) "partition-start-sensitive",
            if (stochastic) "stochastic-or-multimode",
            if (macro) "macro-allocation-mode",
            if (envelope_decline) "distance-envelope-decline-candidate",
            if (literal_freeze) "literal-local-freeze",
            if (local_moves_macro) "local-moves-but-poor-macro-mixing"
        )
        if (!length(flags)) flags <- "mixed-or-inconclusive"
        decision_rows[[length(decision_rows) + 1L]] <- data.frame(
            method = method,
            D_settling = D[["settling"]],
            D_middle = D[["middle"]],
            D_late = D[["late"]],
            D_late_first_task_id = late_argmax$first_task_id,
            D_late_second_task_id = late_argmax$second_task_id,
            B_seed_1_late = B_late$standardized_Q[[1L]],
            B_seed_2_late = B_late$standardized_Q[[2L]],
            B_seed_1_late_middle_ratio = B_ratio[[1L]],
            B_seed_2_late_middle_ratio = B_ratio[[2L]],
            W_mode_A_late = W_late$standardized_Q[[1L]],
            W_mode_B_late = W_late$standardized_Q[[2L]],
            W_mode_A_late_middle_ratio = W_ratio[[1L]],
            W_mode_B_late_middle_ratio = W_ratio[[2L]],
            failed_drift_screens = sum(
                !method_drift$drift_screen_passed
            ),
            failed_scalar_screens = sum(
                !method_scalar$scalar_screen_passed
            ),
            resolved_under_control = resolved,
            partition_start_sensitive = start_sensitive,
            stochastic_or_multimode = stochastic,
            macro_allocation_mode = macro,
            distance_envelope_decline_candidate = envelope_decline,
            literal_local_freeze_chain_count = literal_freeze_count,
            literal_local_freeze = literal_freeze,
            local_moves_but_poor_macro_mixing = local_moves_macro,
            diagnostic_flags = paste(flags, collapse = " | "),
            formal_run_authorized = FALSE,
            stringsAsFactors = FALSE
        )
        rm(wrappers, fits, block_psm)
        invisible(gc(verbose = FALSE))
    }

    all_pair_rows <- do.call(rbind, pair_rows)
    pair_key <- function(value) paste(
        value$method, value$first_task_id, value$second_task_id, sep = "|"
    )
    pair_persistence <- all_pair_rows[
        all_pair_rows$block == "settling",
        c(
            "method", "first_task_id", "second_task_id", "first_mode_id",
            "second_mode_id", "first_seed_id", "second_seed_id",
            "pair_type"
        ),
        drop = FALSE
    ]
    base_key <- pair_key(pair_persistence)
    for (block_name in names(config$blocks)) {
        source <- all_pair_rows[
            all_pair_rows$block == block_name, , drop = FALSE
        ]
        matched <- match(base_key, pair_key(source))
        if (anyNA(matched)) {
            stop("A pairwise persistence block is incomplete.", call. = FALSE)
        }
        pair_persistence[[paste0("Q_", block_name)]] <-
            source$standardized_Q[matched]
    }
    pair_persistence$late_middle_ratio <-
        pair_persistence$Q_late / pair_persistence$Q_middle
    pair_persistence$late_over_limit <- pair_persistence$Q_late > 1
    pair_persistence$persistent_late_difference <-
        pair_persistence$late_over_limit &
            pair_persistence$late_middle_ratio >= config$persistence_ratio

    values <- list(
        rule_specification =
            countdlm_road_partition_rule_specification(config),
        block_summary = do.call(rbind, block_rows),
        pairwise_psm = all_pair_rows,
        pair_persistence = pair_persistence,
        within_chain_drift = do.call(rbind, drift_rows),
        start_memory = do.call(rbind, memory_rows),
        scalar_diagnostics = do.call(rbind, scalar_rows),
        cluster_count_frequency = do.call(rbind, count_frequency_rows),
        late_representative_partition = do.call(
            rbind, late_partition_rows
        ),
        node_psm_difference = do.call(rbind, node_difference_rows),
        macro_pairs = do.call(rbind, macro_rows),
        method_decision = do.call(rbind, decision_rows)
    )
    values <- lapply(values, function(value) {
        rownames(value) <- NULL
        value
    })
    values
}

#' Run the paired partition-mode restart diagnostic
#'
#' The runner verifies the returned targeted-stability archive and D-017
#' context before creating a new external output.  It starts twelve short,
#' truth-blinded chains in two guarded six-worker batches and never computes a
#' recovery metric or launches a formal simulation.
#'
#' @param config Output of `countdlm_road_partition_diagnostic_config()`.
#' @param repository_root Any path inside the Git repository whose provenance
#'   is recorded for this run.
#' @return Diagnostic summaries and immutable output paths.
#' @export
countdlm_road_partition_diagnostic_pilot <- function(
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
            assign(".Random.seed", previous_random_seed,
                   envir = .GlobalEnv)
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

    countdlm_road_validate_partition_diagnostic_config(config)
    if (!requireNamespace("BayesLogit", quietly = TRUE) ||
        !requireNamespace("digest", quietly = TRUE) ||
        !requireNamespace("posterior", quietly = TRUE)) {
        stop("The partition diagnostic requires BayesLogit, digest, and posterior.",
             call. = FALSE)
    }
    parent <- countdlm_road_partition_parent_evidence(
        config$parent_targeted_zip
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
        names(current_environment),
        function(field) identical(
            current_environment[[field]], parent_environment[[field]]
        ),
        logical(1)
    )
    thread_values <- Sys.getenv(thread_variables, unset = NA_character_)
    if (!all(environment_match) ||
        any(is.na(thread_values)) || any(thread_values != "1")) {
        mismatch <- names(current_environment)[!environment_match]
        if (any(is.na(thread_values)) || any(thread_values != "1")) {
            mismatch <- c(mismatch, "single-thread worker environment")
        }
        stop(
            "The current R environment differs from the reviewed parent for: ",
            paste(mismatch, collapse = ", "),
            ". The diagnostic was not started.", call. = FALSE
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
        stop("The frozen execution sources changed after configuration.",
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
        config$methods,
        function(method) digest::digest(
            method_inputs[[method]]$Phi,
            algo = "sha256", serialize = TRUE
        ),
        character(1)
    ), config$methods)
    if (!identical(observed_basis_sha256, config$basis_sha256) ||
        any(vapply(
            config$methods,
            function(method) !identical(
                dim(method_inputs[[method]]$Phi), c(100L, 40L)
            ), logical(1)
        ))) {
        stop("A registered graph-gating basis changed.", call. = FALSE)
    }
    blinded_data <- list(Y = parent$blinded$Y, Fmat = parent$blinded$Fmat)
    observed_data_sha256 <- digest::digest(
        blinded_data, algo = "sha256", serialize = TRUE
    )
    if (!identical(observed_data_sha256, config$data_sha256)) {
        stop("The parent blinded data hash does not match.", call. = FALSE)
    }
    neutral <- countdlm_road_partition_neutral_initialization(
        blinded_data$Y, blinded_data$Fmat
    )
    modes <- parent$modes
    if (!identical(
        vapply(modes, countdlm_road_partition_hash, character(1)),
        stats::setNames(
            config$mode_specification$partition_sha256,
            paste(
                config$mode_specification$method,
                config$mode_specification$mode_id, sep = "|"
            )
        )
    )) {
        stop("The derived parent modes failed their registered hash audit.",
             call. = FALSE)
    }

    reported_physical_cores <- parallel::detectCores(logical = FALSE)
    actual_workers <- min(config$workers, reported_physical_cores -
        config$reserved_reported_cores)
    if (!is.finite(reported_physical_cores) ||
        reported_physical_cores < 10L ||
        !identical(as.integer(actual_workers), config$workers)) {
        stop(
            "The registered six-worker/four-core-reserve contract cannot ",
            "be satisfied on this machine.", call. = FALSE
        )
    }
    output_dir <- countdlm_road_calibration_output_path(
        config$output_dir, git$root
    )
    rm(context)
    invisible(gc(verbose = FALSE))

    if (!dir.create(output_dir, recursive = FALSE, showWarnings = FALSE)) {
        stop("Could not create the registered partition-diagnostic directory.",
             call. = FALSE)
    }
    run_complete <- FALSE
    run_stage <- "registration"
    on.exit({
        if (!run_complete) {
            incomplete <- file.path(output_dir, "RUN-INCOMPLETE.rds")
            if (!dir.exists(output_dir)) {
                message(
                    "The partition-diagnostic output directory is unavailable; ",
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
        parent_targeted = list(
            api_version = config$parent_api_version,
            zip_file = parent$zip_file,
            zip_sha256 = parent$zip_sha256,
            manifest_sha256 = parent$manifest_sha256,
            result_sha256 = parent$result_sha256,
            completion_sha256 = parent$completion_sha256,
            registration_sha256 = parent$registration_sha256,
            blinded_sha256 = parent$blinded_sha256,
            config_signature = config$parent_config_signature
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
            "Truth-blinded partition-mode diagnostic only; modes are derived",
            "without truth, continuous initial values are paired exactly,",
            "and no formal simulation is authorized."
        )
    )
    countdlm_road_atomic_save_rds(
        registration,
        file.path(output_dir, "road-partition-diagnostic-registration.rds")
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
    frozen_source_dir <- file.path(output_dir, "frozen-source", "R")
    directories <- c(
        chain_dir, diagnostic_dir, failure_dir, frozen_source_dir
    )
    for (directory in directories) {
        if (!dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
            stop("Could not create partition-diagnostic subdirectories.",
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
        ),
        file.path(output_dir, "FROZEN_SOURCE_SHA256.csv")
    )
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            Y = blinded_data$Y,
            Fmat = blinded_data$Fmat,
            Y_Fmat_sha256 = observed_data_sha256,
            truth_fields_stored = FALSE,
            truth_metrics_computed = FALSE,
            modes = modes,
            mode_specification = config$mode_specification,
            neutral_theta = neutral$theta,
            neutral_gamma = neutral$gamma,
            neutral_initialization_sha256 = neutral$sha256
        ),
        file.path(output_dir, "road-partition-diagnostic-blinded-input.rds")
    )
    countdlm_road_atomic_write_csv(
        config$mode_specification,
        file.path(output_dir, "partition-mode-specification.csv")
    )
    countdlm_road_assert_partition_output(
        output_dir, directories, config$config_signature,
        "pre-dispatch output verification"
    )
    parent$parent_result <- parent$parent_completion <- NULL
    parent$blinded <- NULL

    diagnostic_worker <- function(index) {
        countdlm_road_calibration_limit_threads()
        task <- config$tasks[index, , drop = FALSE]
        task_id <- task$task_id[[1L]]
        method <- task$method[[1L]]
        mode_id <- task$mode_id[[1L]]
        seed_id <- task$seed_id[[1L]]
        seed <- task$seed[[1L]]
        mode_key <- paste(method, mode_id, sep = "|")
        starting_partition <- modes[[mode_key]]
        expected_partition_hash <- task$parent_partition_sha256[[1L]]
        slug <- countdlm_road_partition_task_slug(index, task_id)
        fit_path <- file.path(chain_dir, paste0(slug, "-fit.rds"))
        fit_relative_path <- file.path("chains", basename(fit_path))
        diagnostic_path <- file.path(
            diagnostic_dir, paste0(slug, "-diagnostic.rds")
        )
        failure_path <- file.path(
            failure_dir, paste0(slug, "-failure.rds")
        )
        task_started <- Sys.time()
        worker_stage <- "partition-diagnostic-fit"
        warning_messages <- character()
        withCallingHandlers(
            tryCatch({
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
                    rho = config$selected_rho
                )
                elapsed <- as.numeric(difftime(
                    Sys.time(), task_started, units = "secs"
                ))
                worker_stage <- "partition-diagnostic-save-fit"
                countdlm_road_atomic_save_rds(
                    list(
                        api_version = config$api_version,
                        parent_targeted_result_sha256 =
                            config$parent_result_sha256,
                        task_id = task_id,
                        method = method,
                        mode_id = mode_id,
                        seed_id = seed_id,
                        seed = seed,
                        selected_rho = config$selected_rho,
                        starting_partition = starting_partition,
                        expected_partition_sha256 =
                            expected_partition_hash,
                        truth_metrics_computed = FALSE,
                        warnings_before_fit_retention = warning_messages,
                        fit = fit
                    ), fit_path
                )
                worker_stage <- "partition-diagnostic-contract"
                if (!countdlm_road_truth_blinding_ok(
                    fit, config$iterations
                )) {
                    stop("The fit failed the strict truth-blinding contract.",
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
                    stop("The paired initialization contract failed.",
                         call. = FALSE)
                }
                fixed_rho_audit <- countdlm_road_fixed_rho_calibration(
                    fit = fit, settle = config$burn,
                    score = config$iterations - config$burn
                )
                worker_stage <- "partition-diagnostic-summarize"
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
                summarized$summary$initial_theta_sha256 <-
                    initial_theta_hash
                summarized$summary$initial_gamma_sha256 <-
                    initial_gamma_hash
                summarized$summary$initial_joint_sha256 <-
                    initial_joint_hash
                summarized$summary$paired_initialization_passed <- TRUE
                summarized$summary$ari_computed <- any(!is.na(fit$ari))
                summarized$summary$acc_computed <- any(!is.na(fit$acc))
                summarized$summary$fit_file <- fit_relative_path
                summarized$summary$completed_fit_retained <- TRUE
                summarized$compact$api_version <- config$api_version
                summarized$compact$method <- method
                summarized$compact$mode_id <- mode_id
                summarized$compact$seed_id <- seed_id
                summarized$compact$seed <- seed
                summarized$compact$fixed_rho_contract <- fixed_rho_audit
                summarized$compact$initial_partition_sha256 <-
                    initial_partition_hash
                summarized$compact$initial_joint_sha256 <-
                    initial_joint_hash
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
                summary$ari_computed <- FALSE
                summary$acc_computed <- FALSE
                summary$fit_file <- if (file.exists(fit_path)) {
                    fit_relative_path
                } else NA_character_
                summary$completed_fit_retained <- file.exists(fit_path)
                retention <- countdlm_road_try_retain_failure(
                    list(
                        api_version = config$api_version,
                        phase = "paired-partition-mode-diagnostic",
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
                        retained = TRUE,
                        completed_fit_retained = file.exists(fit_path),
                        retained_fit_file = if (file.exists(fit_path)) {
                            fit_relative_path
                        } else NA_character_,
                        summary = summary
                    ), failure_path
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
        "Paired partition-mode diagnostic: 3 methods x 2 modes x 2 seeds\n",
        config$iterations, " transitions / ", config$burn,
        " burn; K=", config$K_fit, "; m=", config$basis_m,
        "; fixed rho=", config$selected_rho, "\n",
        "Two guarded batches of six single-thread workers; about ",
        config$reserved_reported_cores, " reported cores remain unused.\n",
        "No truth metrics, no target change, and no formal simulation.\n",
        sep = ""
    )
    run_stage <- "paired-partition-mode-diagnostic"
    pilot_started <- Sys.time()
    task_results <- vector("list", nrow(config$tasks))
    for (batch_id in sort(unique(config$tasks$batch_id))) {
        indices <- which(config$tasks$batch_id == batch_id)
        countdlm_road_assert_partition_output(
            output_dir, directories, config$config_signature,
            paste0("before diagnostic batch ", batch_id)
        )
        cat("\n[BATCH] ", batch_id, "/2; 6 paired tasks\n", sep = "")
        batch_results <- countdlm_road_run_batches(
            config$tasks$task_id[indices],
            function(local_index) diagnostic_worker(indices[[local_index]]),
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
                stage = if (identical(error$origin, "scheduler-collect")) {
                    "parallel-scheduler-collect"
                } else "parallel-worker-uncaught"
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
            summary$ari_computed <- FALSE
            summary$acc_computed <- FALSE
            summary$fit_file <- NA_character_
            summary$completed_fit_retained <- FALSE
            retention <- countdlm_road_try_retain_failure(
                list(
                    api_version = config$api_version,
                    phase = "paired-partition-mode-diagnostic",
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
                failure_record_saved = retention$saved,
                failure_record_error = retention$error
            )
        }
        countdlm_road_stop_if_failure_unretained(
            batch_results, paste0("partition diagnostic batch ", batch_id)
        )
        for (local_index in seq_along(indices)) {
            task_results[[indices[[local_index]]]] <-
                batch_results[[local_index]]
        }
        countdlm_road_assert_partition_output(
            output_dir, directories, config$config_signature,
            paste0("after diagnostic batch ", batch_id)
        )
        batch_runtime <- do.call(rbind, lapply(
            batch_results, `[[`, "summary"
        ))
        if (any(batch_runtime$status != "ok") ||
            any(batch_runtime$warning_count > 0L)) {
            stop(
                "A partition-diagnostic task failed or warned in batch ",
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
    if (any(runtime$status != "ok") ||
        any(runtime$warning_count > 0L) ||
        any(runtime$paired_initialization_passed != TRUE)) {
        stop("The completed diagnostic tasks failed the final contract.",
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
                length(unique(runtime$initial_partition_sha256[paired])) !=
                    2L) {
                stop("A same-seed mode pair failed initialization pairing.",
                     call. = FALSE)
            }
        }
    }

    run_stage <- "partition-derived-diagnostics"
    diagnostics <- countdlm_road_partition_diagnostics(
        runtime, config$tasks, output_dir, config
    )
    initialization_audit <- runtime[, c(
        "task_id", "method", "mode_id", "seed_id", "seed",
        "initial_partition_sha256", "initial_theta_sha256",
        "initial_gamma_sha256", "initial_joint_sha256",
        "paired_initialization_passed"
    )]
    countdlm_road_atomic_write_csv(
        runtime, file.path(output_dir, "partition-diagnostic-runtime.csv")
    )
    countdlm_road_atomic_write_csv(
        initialization_audit,
        file.path(output_dir, "paired-initialization-audit.csv")
    )
    output_tables <- countdlm_road_partition_output_tables()
    for (name in names(output_tables)) {
        countdlm_road_atomic_write_csv(
            diagnostics[[name]], file.path(output_dir, output_tables[[name]])
        )
    }
    failed_tasks <- sum(runtime$status != "ok")
    warning_tasks <- sum(runtime$warning_count > 0L)
    all_initializations_paired <-
        all(runtime$paired_initialization_passed %in% TRUE)
    total_wall_seconds <- as.numeric(difftime(
        Sys.time(), started_at, units = "secs"
    ))
    result <- list(
        api_version = config$api_version,
        config = config,
        registration = registration,
        parent_targeted = registration$parent_targeted,
        runtime = runtime,
        initialization_audit = initialization_audit,
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
        pilot_wall_seconds = pilot_wall_seconds,
        total_wall_seconds_through_derived_tables = total_wall_seconds,
        truth_metrics_computed = FALSE,
        all_tasks_ok = failed_tasks == 0L,
        failed_tasks = failed_tasks,
        warning_tasks = warning_tasks,
        all_initializations_paired = all_initializations_paired,
        all_methods_resolved_under_control = all(
            diagnostics$method_decision$resolved_under_control
        ),
        ready_for_formal_design_review = FALSE,
        eligible_for_formal_freeze = FALSE,
        formal_simulation_launched = FALSE,
        requires_human_review = TRUE
    )
    result_path <- file.path(
        output_dir, "road-partition-diagnostic-result.rds"
    )
    countdlm_road_atomic_save_rds(result, result_path)
    report <- c(
        "countDLM paired partition-mode diagnostic report",
        paste("API:", config$api_version),
        paste("Output:", output_dir),
        paste("Git HEAD:", git$head),
        paste("Git clean:", git$clean),
        paste("Context SHA-256:", observed_context_sha256),
        paste("Parent targeted ZIP SHA-256:", parent$zip_sha256),
        paste("Fixed target K / basis rank / rho:",
              config$K_fit, "/", config$basis_m, "/", config$selected_rho),
        paste("Requested / actual workers:",
              config$workers, "/", actual_workers),
        paste("Reported physical cores:", reported_physical_cores),
        paste("Transitions / burn:", config$iterations, "/", config$burn),
        paste("Tasks / failures / warning tasks:",
              nrow(config$tasks), "/", failed_tasks, "/", warning_tasks),
        paste("All continuous initializations paired exactly:",
              all_initializations_paired),
        paste("Method-fit wall time:",
              countdlm_road_format_duration(pilot_wall_seconds)),
        paste("Total wall time through derived tables:",
              countdlm_road_format_duration(total_wall_seconds)),
        "This diagnostic does not change K, graph rank, rho, data, or thresholds.",
        "Dahl starts were selected from iterations 7001--8000 without truth.",
        "The first 1000 iterations are a settling block, not posterior evidence.",
        "Flags are nonexclusive diagnostics; they are not convergence proofs.",
        "The exact fixed definitions and interpretation limits are retained in partition-diagnostic-rule-spec.csv.",
        "The reported ESS values are posterior bulk/tail effective sample sizes; mean_ess_bracket_evaluations is only the elliptical-slice computational count.",
        "No result from this run can authorize a formal simulation.",
        "A distance-envelope decline is not an exact continuation or proof that more iterations suffice; different pairs can define the block maxima.",
        "Any K or basis-rank sensitivity experiment requires a separately registered second stage.",
        "",
        "Method diagnostic flags:",
        utils::capture.output(print(
            diagnostics$method_decision, row.names = FALSE
        ))
    )
    report_path <- file.path(
        output_dir, "road-partition-diagnostic-report.txt"
    )
    countdlm_road_atomic_write_lines(report, report_path)
    cat("\n", paste(report, collapse = "\n"), "\n", sep = "")
    run_stage <- "checksums-and-completion-marker"
    countdlm_road_assert_partition_payload(
        output_dir, config, basename(source_files)
    )
    checksum_path <- countdlm_road_write_checksums(output_dir)
    completion_path <- file.path(output_dir, "RUN-COMPLETE.rds")
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            status = "complete-with-diagnostic-findings",
            completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
            failed_tasks = failed_tasks,
            warning_tasks = warning_tasks,
            all_initializations_paired = all_initializations_paired,
            all_methods_resolved_under_control =
                result$all_methods_resolved_under_control,
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
