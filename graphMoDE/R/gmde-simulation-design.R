# Scientific design for the current-algorithm Stage-I benchmark.

countdlm_current_benchmark_api_version <-
    "countdlm-current-exact-target-2026-08-31-v4"
countdlm_current_checkpoint_schema <-
    "countdlm-current-exact-target-checkpoint-2026-08-31-v4"
countdlm_current_prediction_api_version <-
    "prediction-not-migrated-2026-08-31-v1"
countdlm_current_context_sha256 <-
    "4d0fac8739063f69e99eaf58f3196b5eda122ea0588e0a9a28d6718b19858357"
countdlm_current_context_workflow <-
    "countdlm-central-beijing-n100-2026-08-22-final-v1"
countdlm_current_parent_context_sha256 <-
    "8c07fc505c3a14e0ffec4642ce33338d7be47ad158d7f7520b6ac88a405ad577"

#' Build a current-algorithm Stage-I benchmark configuration
#'
#' The design retains the user-owned n=100 benchmark settings.  It deliberately
#' has no default rho calibration: the fixed value or full grid policy must be
#' supplied explicitly before a run can be registered.
#'
#' @param profile One of `"smoke"`, `"exact-timing"`, `"pilot"`, or
#'   `"full"`.
#' @param output_dir New external output directory.
#' @param rho Fixed positive multiplier, or `NULL` when calibrating.
#' @param rho_grid Explicit increasing geometric grid.
#' @param rho_transitions_per_candidate Whole-number warm-up transitions per
#'   grid value.
#' @param rho_tie_break Explicit calibration tie rule.
#' @param rho_schedule Explicit candidate ordering, `"cyclic"` or `"blocked"`.
#' @param context_file External frozen n=100 graph-context RDS for pilot/full.
#' @param context_sha256 Expected SHA-256 of `context_file`.
#' @param code_commit Registered code commit; required by exact-timing,
#'   pilot, and full preflight.
#' @param exact_timing_reviewed Whether an exact tiny timing result has been
#'   reviewed before constructing the n=100 pilot.
#' @param exact_timing_output_dir Registered output directory containing the
#'   reviewed exact-timing evidence; required by the pilot.
#' @param pilot_session_launch_budget_hours Positive reviewed per-session
#'   wall-clock boundary for launching new pilot chains.
#' @param scientific_role Registered role such as `"development"` or
#'   `"formal replacement candidate"`.
#' @return A validated configuration list; no files are created.
#' @export
countdlm_current_config <- function(
    profile = c("smoke", "exact-timing", "pilot", "full"),
    output_dir,
    rho = NULL,
    rho_grid = NULL,
    rho_transitions_per_candidate = NULL,
    rho_tie_break = NULL,
    rho_schedule = NULL,
    context_file = NULL,
    context_sha256 = countdlm_current_context_sha256,
    code_commit = NULL,
    exact_timing_reviewed = FALSE,
    exact_timing_output_dir = NULL,
    pilot_session_launch_budget_hours = NULL,
    scientific_role = "development"
) {
    profile <- match.arg(profile)
    if (missing(output_dir) || length(output_dir) != 1L ||
        !is.character(output_dir) || !nzchar(output_dir)) {
        stop("output_dir must be one explicit external path.", call. = FALSE)
    }
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    if (length(scientific_role) != 1L || !is.character(scientific_role) ||
        is.na(scientific_role) || !nzchar(trimws(scientific_role))) {
        stop("scientific_role must be one nonempty string.", call. = FALSE)
    }

    common <- list(
        profile = profile,
        stage = "current-exact-target",
        dgp = c("S", "D"),
        blocked_dgp = "P pending audited PNARM provenance",
        label_regime = c("radial", "balanced_permuted"),
        track = c("fixed", "discovery"),
        methods = c("GMDE-weighted", "MoDE"),
        blocked_method =
            "PNARM pending exact revision, license, and current adapter",
        K_true = 5L,
        K_fixed = 5L,
        K_discovery = 8L,
        permutations = 15L,
        permutation_seed = 2026082101L,
        data_seed = 2026082200L,
        seed_family_version = "stage1-2026-08-21-offset-map-v1",
        process_burn = 200L,
        nu = 1.5,
        length_scale = sqrt(2 * 1.5) / 0.5,
        target_rms_sd = 1.5,
        fit_G = diag(2),
        fit_W = diag(c(1e-6, 5e-7)),
        fit_C0 = diag(c(2, 1)),
        m0_offset = 0.1,
        signal = list(
            mu_star = 25,
            log_level_step = 0.035,
            seasonal = 0.20
        ),
        center = c(longitude = 116.3975, latitude = 39.9087),
        bbox = c(xmin = 116.23, ymin = 39.79, xmax = 116.62, ymax = 40.03),
        output_dir = output_dir,
        context_file = context_file,
        context_sha256 = context_sha256,
        code_commit = code_commit,
        exact_timing_reviewed = isTRUE(exact_timing_reviewed),
        exact_timing_output_dir = exact_timing_output_dir,
        pilot_session_launch_budget_hours =
            pilot_session_launch_budget_hours,
        scientific_role = scientific_role,
        sampler_version = countdlm_gmde_sampler_version,
        benchmark_api_version = countdlm_current_benchmark_api_version,
        checkpoint_schema = countdlm_current_checkpoint_schema,
        prediction_api_version = countdlm_current_prediction_api_version,
        r_rule = "rho * pmax(S_k,t, 1)",
        full_path_state_proposal = TRUE,
        temporal_blocking = FALSE,
        rolling_prediction_status =
            "blocked pending a validated Poisson predictive-filter algorithm",
        convergence_rhat_max = 1.05,
        convergence_K_rhat_reference = 1.10
    )
    profile_settings <- switch(
        profile,
        smoke = list(
            n = 25L, TT = 12L, train_T = 10L,
            fixed_replicates = 1L, discovery_replicates = 1L,
            chains = 2L, basis_m = 8L,
            n_iter = 3L, burn = 1L, substantive_min = 2L,
            pg_backend = "truncated", pg_trunc = 20L,
            inferential = FALSE
        ),
        `exact-timing` = list(
            n = 25L, TT = 6L, train_T = 4L,
            fixed_replicates = 1L, discovery_replicates = 1L,
            chains = 2L, basis_m = 8L,
            n_iter = 4L, burn = 2L, substantive_min = 2L,
            pg_backend = "devroye-exact", pg_trunc = NA_integer_,
            inferential = FALSE
        ),
        pilot = list(
            n = 100L, TT = 168L, train_T = 144L,
            fixed_replicates = 1L, discovery_replicates = 1L,
            chains = 2L, basis_m = 40L,
            n_iter = 200L, burn = 100L, substantive_min = 5L,
            pg_backend = "devroye-exact", pg_trunc = NA_integer_,
            inferential = FALSE
        ),
        full = list(
            n = 100L, TT = 168L, train_T = 144L,
            fixed_replicates = 15L, discovery_replicates = 5L,
            chains = 2L, basis_m = 40L,
            n_iter = 3000L, burn = 1500L, substantive_min = 5L,
            pg_backend = "devroye-exact", pg_trunc = NA_integer_,
            inferential = TRUE
        )
    )
    cfg <- utils::modifyList(common, profile_settings)

    if (identical(profile, "pilot")) {
        if (!isTRUE(exact_timing_reviewed) ||
            length(exact_timing_output_dir) != 1L ||
            !is.character(exact_timing_output_dir) ||
            is.na(exact_timing_output_dir) ||
            !nzchar(exact_timing_output_dir) ||
            length(pilot_session_launch_budget_hours) != 1L ||
            !is.numeric(pilot_session_launch_budget_hours) ||
            !is.finite(pilot_session_launch_budget_hours) ||
            pilot_session_launch_budget_hours <= 0) {
            stop(
                "Pilot construction requires reviewed exact-timing evidence ",
                "with exact_timing_output_dir and one positive ",
                "pilot_session_launch_budget_hours value.",
                call. = FALSE
            )
        }
        cfg$exact_timing_output_dir <- normalizePath(
            exact_timing_output_dir, winslash = "/", mustWork = FALSE
        )
    } else if (isTRUE(exact_timing_reviewed) ||
        !is.null(exact_timing_output_dir) ||
        !is.null(pilot_session_launch_budget_hours)) {
        stop(
            "exact_timing_reviewed, exact_timing_output_dir, and ",
            "pilot_session_launch_budget_hours apply only to pilot.",
            call. = FALSE
        )
    }

    if (!is.null(rho)) {
        if (!is.numeric(rho) || length(rho) != 1L || !is.finite(rho) ||
            rho <= 0 || !is.null(rho_grid) ||
            !is.null(rho_transitions_per_candidate) ||
            !is.null(rho_tie_break) || !is.null(rho_schedule)) {
            stop(
                "Supply one positive fixed rho, or the complete grid policy, ",
                "but not both.", call. = FALSE
            )
        }
        cfg$rho <- rho
        cfg$rho_grid <- NULL
        cfg$rho_warmup <- NULL
        cfg$rho_tie_break <- NULL
        cfg$rho_schedule <- NULL
    } else {
        if (is.null(rho_grid) || is.null(rho_transitions_per_candidate) ||
            is.null(rho_tie_break) || is.null(rho_schedule)) {
            stop(
                "No rho policy is assumed. Supply rho, or rho_grid plus ",
                "rho_transitions_per_candidate, rho_tie_break, and ",
                "rho_schedule.",
                call. = FALSE
            )
        }
        transitions <- gmde_scalar_integer(
            rho_transitions_per_candidate,
            "rho_transitions_per_candidate",
            lower = 1L
        )
        cfg$rho <- NULL
        cfg$rho_grid <- as.numeric(rho_grid)
        cfg$rho_warmup <- transitions * length(cfg$rho_grid)
        cfg$rho_tie_break <- rho_tie_break
        cfg$rho_schedule <- rho_schedule
    }
    state_control <- gmde_validate_rho_control(
        rho = cfg$rho,
        rho_grid = cfg$rho_grid,
        rho_warmup = cfg$rho_warmup,
        rho_tie_break = cfg$rho_tie_break,
        rho_schedule = cfg$rho_schedule,
        burn = cfg$burn
    )
    if (identical(cfg$pg_backend, "devroye-exact")) {
        rho_values <- state_control$grid
        if (any(rho_values != round(rho_values)) ||
            any(rho_values > .Machine$integer.max)) {
            stop(
                "Pilot/full exact profiles require positive integer rho values.",
                call. = FALSE
            )
        }
    }
    cfg$rho_control <- state_control
    structure(cfg, class = c("countdlm_current_config", "list"))
}

countdlm_current_rebuild_config <- function(cfg, output_dir = cfg$output_dir) {
    if (!is.list(cfg) || length(cfg$profile) != 1L) {
        stop("The current benchmark configuration is not reconstructable.",
             call. = FALSE)
    }
    grid_mode <- is.null(cfg$rho)
    transitions <- if (grid_mode && length(cfg$rho_grid)) {
        cfg$rho_warmup / length(cfg$rho_grid)
    } else NULL
    countdlm_current_config(
        profile = cfg$profile,
        output_dir = output_dir,
        rho = if (grid_mode) NULL else cfg$rho,
        rho_grid = if (grid_mode) cfg$rho_grid else NULL,
        rho_transitions_per_candidate = transitions,
        rho_tie_break = if (grid_mode) cfg$rho_tie_break else NULL,
        rho_schedule = if (grid_mode) cfg$rho_schedule else NULL,
        context_file = cfg$context_file,
        context_sha256 = cfg$context_sha256,
        code_commit = cfg$code_commit,
        exact_timing_reviewed = cfg$exact_timing_reviewed,
        exact_timing_output_dir = cfg$exact_timing_output_dir,
        pilot_session_launch_budget_hours =
            cfg$pilot_session_launch_budget_hours,
        scientific_role = cfg$scientific_role
    )
}

countdlm_current_validate_config <- function(cfg) {
    if (!inherits(cfg, "countdlm_current_config") ||
        anyDuplicated(names(cfg))) {
        stop(
            "Use countdlm_current_config() to construct the benchmark config.",
            call. = FALSE
        )
    }
    expected <- tryCatch(
        countdlm_current_rebuild_config(cfg),
        error = function(e) {
            stop(
                "The current benchmark configuration failed reconstruction: ",
                conditionMessage(e), call. = FALSE
            )
        }
    )
    actual_names <- sort(names(cfg))
    expected_names <- sort(names(expected))
    if (!identical(actual_names, expected_names) ||
        !identical(unclass(cfg[actual_names]),
                   unclass(expected[expected_names]))) {
        stop(
            "The current benchmark configuration was modified after ",
            "construction or is not canonical.", call. = FALSE
        )
    }
    invisible(TRUE)
}

countdlm_current_seed <- function(
    cfg,
    dgp,
    label,
    track = "discovery",
    replicate = 1L,
    chain = 0L,
    method = "data"
) {
    method_levels <- c(
        "data", "initial", "GMDE-weighted", "GMDE-binary",
        "GMDE-permuted", "MoDE", "PNARM", "prediction",
        "graph-permutation"
    )
    values <- c(
        cfg$data_seed,
        100000L * match(dgp, c("S", "D", "P")),
        10000L * match(label, c("radial", "balanced_permuted")),
        1000L * match(track, c("discovery", "fixed")),
        20L * gmde_scalar_integer(
            replicate, "replicate", lower = 1L, upper = 999L
        ),
        2L * gmde_scalar_integer(chain, "chain", lower = 0L, upper = 99L),
        match(method, method_levels)
    )
    if (anyNA(values)) {
        stop("Unknown benchmark seed component.", call. = FALSE)
    }
    as.integer(sum(values) %% (.Machine$integer.max - 1L) + 1L)
}

gmde_graph_basis <- function(W, m, nu, length_scale, target_rms_sd) {
    W <- as.matrix(W)
    if (nrow(W) < 2L || nrow(W) != ncol(W) || any(!is.finite(W))) {
        stop("W must be a finite square graph-weight matrix.", call. = FALSE)
    }
    scale <- max(1, abs(W))
    tolerance <- 1e-10 * scale
    if (max(abs(W - t(W))) > tolerance || any(W < -tolerance) ||
        any(abs(diag(W)) > tolerance)) {
        stop("W must be symmetric, nonnegative, and zero-diagonal.",
             call. = FALSE)
    }
    m <- gmde_scalar_integer(m, "m", lower = 1L, upper = nrow(W))
    if (length(nu) != 1L || !is.finite(nu) || nu <= 0 ||
        length(length_scale) != 1L || !is.finite(length_scale) ||
        length_scale <= 0 || length(target_rms_sd) != 1L ||
        !is.finite(target_rms_sd) || target_rms_sd <= 0) {
        stop("Graph-basis scale parameters must be finite and positive.",
             call. = FALSE)
    }
    W[W < 0] <- 0
    diag(W) <- 0
    laplacian <- diag(rowSums(W)) - W
    decomposition <- eigen(laplacian, symmetric = TRUE)
    order_index <- order(decomposition$values)
    eigenvalue <- decomposition$values[order_index]
    eigen_tolerance <- 1e-10 * max(1, abs(eigenvalue))
    if (min(eigenvalue) < -eigen_tolerance) {
        stop("The graph Laplacian has a materially negative eigenvalue.",
             call. = FALSE)
    }
    eigenvalue[eigenvalue < 0] <- 0
    eigenvector <- decomposition$vectors[, order_index, drop = FALSE]
    variance <- (
        2 * nu / length_scale^2 + eigenvalue
    )^(-nu)
    raw <- eigenvector[, seq_len(m), drop = FALSE] %*%
        diag(sqrt(variance[seq_len(m)]), m, m)
    raw * target_rms_sd / sqrt(mean(rowSums(raw^2)))
}

countdlm_current_smoke_context <- function(cfg) {
    side <- 5L
    grid <- expand.grid(x = seq_len(side), y = seq_len(side))
    n <- nrow(grid)
    W <- matrix(0, n, n)
    for (i in seq_len(n)) {
        for (j in seq_len(n)) {
            dx <- abs(grid$x[i] - grid$x[j])
            dy <- abs(grid$y[i] - grid$y[j])
            if (dx + dy == 1L) W[i, j] <- if (dx == 1L) 1 else 0.7
        }
    }
    W <- (W + t(W)) / 2
    locations <- data.frame(
        location_id = seq_len(n),
        longitude = cfg$center[["longitude"]] + (grid$x - 3) * 0.01,
        latitude = cfg$center[["latitude"]] + (grid$y - 3) * 0.01
    )
    list(
        locations = locations,
        W = W,
        A = 1L * (W > 0),
        Phi = gmde_graph_basis(
            W, cfg$basis_m, cfg$nu, cfg$length_scale, cfg$target_rms_sd
        ),
        source = "deterministic-5-by-5-smoke-lattice",
        sha256 = "deterministic-5-by-5-smoke-lattice-v1"
    )
}

countdlm_current_load_context <- function(cfg) {
    if (cfg$profile %in% c("smoke", "exact-timing")) {
        return(countdlm_current_smoke_context(cfg))
    }
    if (is.null(cfg$context_file) || is.null(cfg$context_sha256) ||
        length(cfg$context_file) != 1L ||
        length(cfg$context_sha256) != 1L ||
        !grepl("^[0-9a-f]{64}$", tolower(cfg$context_sha256))) {
        stop(
            "Pilot/full require an external frozen context_file and SHA-256.",
            call. = FALSE
        )
    }
    if (!identical(
        tolower(cfg$context_sha256), countdlm_current_context_sha256
    )) {
        stop("Pilot/full require the canonical frozen n=100 context hash.",
             call. = FALSE)
    }
    path <- normalizePath(cfg$context_file, winslash = "/", mustWork = TRUE)
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("Package 'digest' is required to verify the graph context.",
             call. = FALSE)
    }
    actual <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
    if (!identical(tolower(actual), tolower(cfg$context_sha256))) {
        stop("Frozen graph-context SHA-256 mismatch.", call. = FALSE)
    }
    context <- readRDS(path)
    if (!is.list(context) ||
        !all(c("locations", "W", "Phi", "metadata") %in% names(context))) {
        stop("The frozen context must contain locations, W, Phi, and metadata.",
             call. = FALSE)
    }
    metadata <- context$metadata
    if (!is.list(metadata) ||
        !identical(metadata$workflow_version, countdlm_current_context_workflow) ||
        !identical(
            unname(metadata$source_sha256[["context"]]),
            countdlm_current_parent_context_sha256
        ) || !identical(
            as.integer(metadata$selected_source_ids),
            as.integer(context$locations$source_location_id)
        )) {
        stop("The frozen context metadata/parent lineage is incompatible.",
             call. = FALSE)
    }
    context$locations <- as.data.frame(context$locations)
    context$W <- as.matrix(context$W)
    context$Phi <- as.matrix(context$Phi)
    if (nrow(context$locations) != cfg$n ||
        !identical(dim(context$W), c(cfg$n, cfg$n)) ||
        !identical(dim(context$Phi), c(cfg$n, cfg$basis_m)) ||
        any(!is.finite(c(context$W, context$Phi))) ||
        !all(c(
            "location_id", "source_location_id", "longitude", "latitude",
            "ring_label"
        ) %in% names(context$locations)) ||
        anyDuplicated(context$locations$location_id) ||
        anyDuplicated(context$locations$source_location_id) ||
        any(!is.finite(context$locations$source_location_id)) ||
        any(context$locations$source_location_id !=
                round(context$locations$source_location_id)) ||
        any(!is.finite(context$locations$longitude)) ||
        any(!is.finite(context$locations$latitude))) {
        stop("The frozen n=100 graph context failed dimension/value checks.",
             call. = FALSE)
    }
    if (any(context$locations$longitude < cfg$bbox[["xmin"]]) ||
        any(context$locations$longitude > cfg$bbox[["xmax"]]) ||
        any(context$locations$latitude < cfg$bbox[["ymin"]]) ||
        any(context$locations$latitude > cfg$bbox[["ymax"]])) {
        stop("A frozen context location is outside the registered bbox.",
             call. = FALSE)
    }
    expected_basis <- gmde_graph_basis(
        context$W, cfg$basis_m, cfg$nu,
        cfg$length_scale, cfg$target_rms_sd
    )
    covariance_scale <- max(1, abs(tcrossprod(expected_basis)))
    if (max(abs(
        tcrossprod(context$Phi) - tcrossprod(expected_basis)
    )) > 1e-8 * covariance_scale) {
        stop(
            "The stored Phi is not the registered variance-calibrated basis.",
            call. = FALSE
        )
    }
    context$A <- 1L * (context$W > 0)
    degree <- rowSums(context$A)
    if (any(degree == 0L)) {
        stop("The frozen graph contains an isolated location.",
             call. = FALSE)
    }
    visited <- rep(FALSE, cfg$n)
    queue <- 1L
    visited[1L] <- TRUE
    while (length(queue)) {
        vertex <- queue[1L]
        queue <- queue[-1L]
        neighbor <- which(context$A[vertex, ] > 0 & !visited)
        if (length(neighbor)) {
            visited[neighbor] <- TRUE
            queue <- c(queue, neighbor)
        }
    }
    if (!all(visited)) stop("The frozen graph must be connected.",
                            call. = FALSE)
    selected_id_sha256 <- if (!is.null(context$metadata)) {
        context$metadata$selected_id_sha256
    } else NULL
    if (length(selected_id_sha256) != 1L ||
        !grepl("^[0-9a-f]{64}$", tolower(selected_id_sha256))) {
        stop("The context lacks its immutable selected-location digest.",
             call. = FALSE)
    }
    selected_id_string <- paste(
        as.integer(context$locations$source_location_id), collapse = ","
    )
    recomputed_id_sha256 <- digest::digest(
        selected_id_string, algo = "sha256", serialize = FALSE
    )
    if (!identical(
        tolower(selected_id_sha256), tolower(recomputed_id_sha256)
    )) {
        stop("The selected source-location digest does not match the context.",
             call. = FALSE)
    }
    recomputed_ring <- countdlm_current_radial_labels(
        context$locations, cfg
    )
    if (!identical(
        as.integer(context$locations$ring_label), recomputed_ring
    )) {
        stop("The frozen radial labels do not match source-ID tie-breaking.",
             call. = FALSE)
    }
    context$source <- path
    context$sha256 <- actual
    context$selected_id_sha256 <- tolower(selected_id_sha256)
    context
}

countdlm_current_radial_labels <- function(locations, cfg) {
    longitude <- as.numeric(locations$longitude) * pi / 180
    latitude <- as.numeric(locations$latitude) * pi / 180
    longitude0 <- cfg$center[["longitude"]] * pi / 180
    latitude0 <- cfg$center[["latitude"]] * pi / 180
    haversine <- sin((latitude - latitude0) / 2)^2 +
        cos(latitude0) * cos(latitude) *
        sin((longitude - longitude0) / 2)^2
    distance <- 2 * 6371.0088 * asin(pmin(1, sqrt(haversine)))
    tie_id <- if ("source_location_id" %in% names(locations)) {
        locations$source_location_id
    } else locations$location_id
    order_index <- order(distance, tie_id)
    if (length(order_index) %% cfg$K_true != 0L) {
        stop("Equal radial groups require n divisible by K_true.",
             call. = FALSE)
    }
    labels <- integer(length(order_index))
    labels[order_index] <- rep(
        seq_len(cfg$K_true),
        each = length(order_index) / cfg$K_true
    )
    labels
}

countdlm_current_label_bank <- function(context, cfg) {
    radial <- countdlm_current_radial_labels(context$locations, cfg)
    set.seed(cfg$permutation_seed)
    list(
        radial = radial,
        balanced_permuted = replicate(
            cfg$permutations, sample(radial), simplify = FALSE
        )
    )
}

countdlm_current_generate <- function(dgp, Z, context, cfg, seed) {
    dgp <- match.arg(dgp, cfg$dgp)
    Z <- gmde_validate_labels(Z, cfg$n, cfg$K_true, "Z")
    set.seed(as.integer(seed))
    K <- cfg$K_true
    TT <- cfg$TT
    time <- seq_len(TT)
    Fmat <- cbind(intercept = 1, daily_sine = sin(2 * pi * time / 24))
    level <- cfg$signal$mu_star * exp(
        cfg$signal$log_level_step * (seq_len(K) - (K + 1) / 2)
    )
    if (identical(dgp, "S")) {
        lambda_class <- matrix(level, nrow = K, ncol = TT)
        theta <- NULL
    } else {
        theta <- array(NA_real_, c(K, TT, 2L))
        for (k in seq_len(K)) {
            theta[k, 1L, ] <- c(log(level[k]), cfg$signal$seasonal)
            if (TT > 1L) {
                for (tt in 2:TT) {
                    theta[k, tt, ] <- theta[k, tt - 1L, ] +
                        stats::rnorm(2L, sd = sqrt(diag(cfg$fit_W)))
                }
            }
        }
        lambda_class <- matrix(NA_real_, K, TT)
        for (k in seq_len(K)) {
            lambda_class[k, ] <- exp(rowSums(theta[k, , ] * Fmat))
        }
    }
    lambda <- lambda_class[Z, , drop = FALSE]
    Y <- matrix(
        stats::rpois(cfg$n * TT, lambda),
        nrow = cfg$n,
        ncol = TT
    )
    list(
        dgp = dgp,
        Y = Y,
        lambda = lambda,
        Fmat = Fmat,
        Z = Z,
        truth = list(lambda_class = lambda_class, theta = theta),
        seed = as.integer(seed),
        train_T = cfg$train_T
    )
}

countdlm_current_representative_partition <- function(Z_draws) {
    Z_draws <- as.matrix(Z_draws)
    if (nrow(Z_draws) < 1L || ncol(Z_draws) < 1L ||
        any(!is.finite(Z_draws)) || any(Z_draws != round(Z_draws))) {
        stop("Z_draws must be a nonempty integer-label matrix.",
             call. = FALSE)
    }
    n <- ncol(Z_draws)
    similarity <- matrix(0, n, n)
    for (draw in seq_len(nrow(Z_draws))) {
        similarity <- similarity + outer(
            Z_draws[draw, ], Z_draws[draw, ], "=="
        )
    }
    similarity <- similarity / nrow(Z_draws)
    loss <- vapply(seq_len(nrow(Z_draws)), function(draw) {
        membership <- outer(Z_draws[draw, ], Z_draws[draw, ], "==")
        sum((membership - similarity)^2)
    }, numeric(1))
    list(
        label = as.integer(Z_draws[which.min(loss), ]),
        draw = which.min(loss),
        loss = min(loss),
        similarity = similarity
    )
}

#' Construct the supported current-algorithm task grid
#'
#' @param cfg Current benchmark configuration.
#' @param replicate_ids,dgps,labels,tracks,methods Optional execution filters.
#' @return A stable data frame of supported GMDE/MoDE tasks.
#' @export
countdlm_current_task_grid <- function(
    cfg,
    replicate_ids = NULL,
    dgps = NULL,
    labels = NULL,
    tracks = NULL,
    methods = NULL
) {
    rows <- list()
    index <- 0L
    replicate_count <- c(
        fixed = cfg$fixed_replicates,
        discovery = cfg$discovery_replicates
    )
    for (track in cfg$track) {
        for (replicate in seq_len(replicate_count[[track]])) {
            for (dgp in cfg$dgp) {
                for (label in cfg$label_regime) {
                    for (method in cfg$methods) {
                        index <- index + 1L
                        rows[[index]] <- data.frame(
                            task_id = index,
                            task_key = paste(
                                track, sprintf("rep-%03d", replicate),
                                paste0("DGP-", dgp), label, method,
                                sep = "|"
                            ),
                            track = track,
                            replicate = replicate,
                            dgp = dgp,
                            label_regime = label,
                            method = method,
                            stringsAsFactors = FALSE
                        )
                    }
                }
            }
        }
    }
    grid <- do.call(rbind, rows)
    select <- function(value, allowed, name) {
        if (is.null(value)) return(allowed)
        value <- unique(as.character(value))
        invalid <- setdiff(value, allowed)
        if (length(invalid)) {
            stop("Unknown ", name, ": ", paste(invalid, collapse = ", "),
                 call. = FALSE)
        }
        value
    }
    chosen_dgp <- select(dgps, cfg$dgp, "DGP")
    chosen_label <- select(labels, cfg$label_regime, "label regime")
    chosen_track <- select(tracks, cfg$track, "track")
    chosen_method <- select(methods, cfg$methods, "method")
    keep <- grid$dgp %in% chosen_dgp &
        grid$label_regime %in% chosen_label &
        grid$track %in% chosen_track &
        grid$method %in% chosen_method
    if (!is.null(replicate_ids)) {
        if (!is.numeric(replicate_ids) || any(!is.finite(replicate_ids)) ||
            any(replicate_ids != round(replicate_ids)) ||
            any(replicate_ids < 1)) {
            stop("replicate_ids must contain positive whole numbers.",
                 call. = FALSE)
        }
        keep <- keep & grid$replicate %in% as.integer(replicate_ids)
    }
    grid <- grid[keep, , drop = FALSE]
    if (!nrow(grid)) stop("The execution filters select no tasks.",
                          call. = FALSE)
    rownames(grid) <- NULL
    grid
}
