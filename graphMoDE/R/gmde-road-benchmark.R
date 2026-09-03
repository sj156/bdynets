# Approved-road-design simulation and runtime calibration for the current paper.

countdlm_road_benchmark_api_version <-
    "countdlm-road-benchmark-2026-09-02-v1"
countdlm_road_context_sha256 <-
    "992e41c34dff68f876a04e4ed63814f48c57592a6c600f4f9140070318521bc0"
countdlm_road_context_workflow <-
    "countdlm-central-beijing-road-candidate-2026-09-02-v2"
countdlm_road_selected_id_sha256 <-
    "3e76c75c869c35774f167f08280e6e44f8104c8cd0f6fa3a31d7f63dc27e8dbf"
countdlm_road_internal_methods <-
    c("GMDE-W", "GMDE-C", "Euc-MDE", "MoDE", "Potts-MDE")
countdlm_road_external_methods <- c("PNARM", "AR1-latent-class")

countdlm_road_config_signature <- function(config) {
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("Package 'digest' is required for configuration signatures.",
             call. = FALSE)
    }
    payload <- unclass(config)
    payload$config_signature <- NULL
    digest::digest(payload, algo = "sha256", serialize = TRUE)
}

countdlm_road_validate_quick_config <- function(config) {
    if (!inherits(config, "countdlm_road_quick_config") ||
        !identical(config$api_version, countdlm_road_benchmark_api_version) ||
        !is.character(config$config_signature) ||
        length(config$config_signature) != 1L ||
        !identical(
            config$config_signature,
            countdlm_road_config_signature(config)
        )) {
        stop(
            "The road quick-test configuration is invalid or was modified ",
            "after construction.", call. = FALSE
        )
    }
    if (!identical(config$n, 100L) || !identical(config$TT, 168L) ||
        !identical(config$K_true, 5L) || !identical(config$K_fit, 10L) ||
        !identical(config$basis_m, 40L) ||
        !identical(config$pg_backend, "devroye-exact") ||
        !isTRUE(config$algorithm_exact) ||
        !isTRUE(config$classification_only) ||
        !identical(config$methods, countdlm_road_internal_methods) ||
        !identical(config$context_sha256, countdlm_road_context_sha256) ||
        !is.data.frame(config$quick_tasks) ||
        anyDuplicated(config$quick_tasks$task_id) ||
        !identical(
            sort(unique(config$quick_tasks$method)),
            sort(countdlm_road_internal_methods)
        )) {
        stop("The road quick-test fixed scientific contract is invalid.",
             call. = FALSE)
    }
    invisible(config)
}

countdlm_road_sha256 <- function(path) {
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("Package 'digest' is required for SHA-256 verification.",
             call. = FALSE)
    }
    digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

countdlm_road_validate_weight_matrix <- function(W, n, name) {
    W <- as.matrix(W)
    tolerance <- 1e-10 * max(1, abs(W))
    if (!identical(dim(W), c(n, n)) || any(!is.finite(W)) ||
        any(W < -tolerance) || max(abs(W - t(W))) > tolerance ||
        any(abs(diag(W)) > tolerance)) {
        stop(
            name, " must be a finite symmetric nonnegative ", n,
            " by ", n, " matrix with zero diagonal.", call. = FALSE
        )
    }
    W[W < 0] <- 0
    diag(W) <- 0
    W
}

countdlm_road_component_count <- function(A) {
    A <- as.matrix(A) > 0
    n <- nrow(A)
    unseen <- rep(TRUE, n)
    components <- 0L
    while (any(unseen)) {
        components <- components + 1L
        frontier <- which(unseen)[[1L]]
        unseen[frontier] <- FALSE
        while (length(frontier)) {
            neighbors <- which(colSums(A[frontier, , drop = FALSE]) > 0 &
                unseen)
            unseen[neighbors] <- FALSE
            frontier <- neighbors
        }
    }
    components
}

#' Load and validate the approved 2026-09-02 road context
#'
#' The external RDS retains its original candidate metadata.  Approval is
#' represented by durable repository decision D-017 and exact file/selected-ID
#' hashes; the evidence RDS is never rewritten in place.
#'
#' @param context_file Path to the approved external road-context RDS.
#' @return A validated context with source and approval metadata attached.
#' @export
countdlm_road_load_approved_context <- function(context_file) {
    if (length(context_file) != 1L || !is.character(context_file) ||
        is.na(context_file) || !nzchar(context_file)) {
        stop("context_file must be one explicit path.", call. = FALSE)
    }
    path <- normalizePath(context_file, winslash = "/", mustWork = TRUE)
    observed_sha256 <- countdlm_road_sha256(path)
    if (!identical(observed_sha256, countdlm_road_context_sha256)) {
        stop(
            "The road-context SHA-256 does not match the design approved in ",
            "D-017.", call. = FALSE
        )
    }
    context <- readRDS(path)
    required <- c(
        "locations", "D_km", "W", "A", "L", "Phi", "q", "h",
        "group_boundaries", "metadata"
    )
    if (!is.list(context) || !all(required %in% names(context))) {
        stop("The approved road context lacks required objects.",
             call. = FALSE)
    }
    context$locations <- as.data.frame(context$locations)
    n <- nrow(context$locations)
    if (n != 100L || !all(c(
        "location_id", "road_vertex_id", "longitude", "latitude",
        "cell_id", "center_road_distance_km", "ring_label"
    ) %in% names(context$locations)) ||
        anyDuplicated(context$locations$location_id) ||
        anyDuplicated(context$locations$road_vertex_id) ||
        any(!is.finite(context$locations$longitude)) ||
        any(!is.finite(context$locations$latitude))) {
        stop("The approved location table failed identity/value checks.",
             call. = FALSE)
    }
    context$D_km <- as.matrix(context$D_km)
    if (!identical(dim(context$D_km), c(n, n)) ||
        any(!is.finite(context$D_km)) || any(context$D_km < 0) ||
        max(abs(context$D_km - t(context$D_km))) > 1e-8 ||
        any(abs(diag(context$D_km)) > 1e-8)) {
        stop("The approved symmetric road-distance matrix is invalid.",
             call. = FALSE)
    }
    context$W <- countdlm_road_validate_weight_matrix(context$W, n, "W")
    context$A <- 1L * (as.matrix(context$A) > 0)
    diag(context$A) <- 0L
    if (!identical(context$A, 1L * (context$W > 0))) {
        stop("A does not match the positive support of W.", call. = FALSE)
    }
    labels <- gmde_validate_labels(
        context$locations$ring_label, n, 5L, "road-distance labels"
    )
    if (!identical(as.integer(tabulate(labels, nbins = 5L)), rep(20L, 5L))) {
        stop("The approved road-distance groups are not five groups of 20.",
             call. = FALSE)
    }
    selected_digest <- digest::digest(
        paste(context$locations$road_vertex_id, collapse = ","),
        algo = "sha256", serialize = FALSE
    )
    metadata <- context$metadata
    if (!is.list(metadata) ||
        !identical(metadata$workflow_version, countdlm_road_context_workflow) ||
        !identical(metadata$selected_id_sha256,
                   countdlm_road_selected_id_sha256) ||
        !identical(selected_digest, countdlm_road_selected_id_sha256) ||
        !identical(as.integer(metadata$selection_seed), 2026090201L) ||
        !identical(as.integer(metadata$grid_nx), 5L) ||
        !identical(as.integer(metadata$grid_ny), 5L) ||
        !identical(as.integer(metadata$sampled_per_cell), 4L) ||
        !identical(as.integer(metadata$n), 100L) ||
        !identical(as.integer(metadata$K_true), 5L) ||
        !identical(as.integer(context$q), 4L) ||
        !identical(as.integer(metadata$q_fixed), 4L) ||
        !isTRUE(all.equal(
            as.numeric(context$h), as.numeric(metadata$h_nn_km),
            tolerance = 1e-12
        ))) {
        stop("The road-context metadata do not match D-017.", call. = FALSE)
    }
    cell_counts <- tabulate(
        as.integer(context$locations$cell_id), nbins = 25L
    )
    if (!identical(cell_counts, rep(4L, 25L))) {
        stop("The approved 5 by 5 sampling grid is not four-per-cell.",
             call. = FALSE)
    }
    graph_diagnostics <- metadata$graph_diagnostics
    if (!is.list(graph_diagnostics) ||
        !all(c("components", "isolates") %in% names(graph_diagnostics))) {
        stop("The approved graph diagnostics are missing.", call. = FALSE)
    }
    if (sum(context$A[upper.tri(context$A)]) != 242L ||
        any(rowSums(context$A) == 0L) ||
        countdlm_road_component_count(context$A) != 1L ||
        !identical(as.integer(graph_diagnostics$components), 1L) ||
        !identical(as.integer(graph_diagnostics$isolates), 0L)) {
        stop("The approved q=4 graph edge/isolate audit failed.",
             call. = FALSE)
    }
    context$L <- as.matrix(context$L)
    expected_L <- diag(rowSums(context$W)) - context$W
    if (!identical(dim(context$L), c(n, n)) ||
        any(!is.finite(context$L)) ||
        max(abs(context$L - expected_L)) > 1e-10 * max(1, abs(expected_L))) {
        stop("The stored weighted graph Laplacian is inconsistent with W.",
             call. = FALSE)
    }
    context$Phi <- as.matrix(context$Phi)
    if (!identical(dim(context$Phi), c(n, 40L)) ||
        any(!is.finite(context$Phi))) {
        stop("The approved weighted graph basis is not 100 by 40 finite.",
             call. = FALSE)
    }
    expected_phi <- gmde_graph_basis(
        context$W,
        m = as.integer(metadata$basis_m),
        nu = metadata$basis_nu,
        length_scale = metadata$basis_length_scale,
        target_rms_sd = metadata$basis_target_rms_sd
    )
    covariance_scale <- max(1, abs(tcrossprod(expected_phi)))
    if (max(abs(
        tcrossprod(context$Phi) - tcrossprod(expected_phi)
    )) > 1e-8 * covariance_scale) {
        stop("The stored weighted graph basis failed reconstruction.",
             call. = FALSE)
    }
    context$locations$ring_label <- labels
    context$source <- path
    context$sha256 <- observed_sha256
    context$approval <- list(
        decision = "D-017",
        approved_by_user = TRUE,
        evidence_metadata_retained = TRUE
    )
    context
}

countdlm_road_project_coordinates_km <- function(locations) {
    if (!is.data.frame(locations) ||
        !all(c("longitude", "latitude") %in% names(locations))) {
        stop("locations must contain longitude and latitude columns.",
             call. = FALSE)
    }
    longitude <- as.numeric(locations$longitude)
    latitude <- as.numeric(locations$latitude)
    if (length(longitude) < 2L || length(latitude) != length(longitude) ||
        any(!is.finite(longitude)) || any(!is.finite(latitude))) {
        stop("Location coordinates must be finite and nonempty.",
             call. = FALSE)
    }
    longitude0 <- mean(longitude)
    latitude0 <- mean(latitude)
    km_per_degree <- 111.1950802
    cbind(
        x_km = (longitude - longitude0) *
            cos(latitude0 * pi / 180) * km_per_degree,
        y_km = (latitude - latitude0) * km_per_degree
    )
}

countdlm_road_euclidean_basis <- function(
    locations,
    m = 40L,
    q = 4L,
    target_rms_sd = 1.5
) {
    if (length(target_rms_sd) != 1L || !is.numeric(target_rms_sd) ||
        !is.finite(target_rms_sd) || target_rms_sd <= 0) {
        stop("target_rms_sd must be one finite positive number.",
             call. = FALSE)
    }
    coordinates <- countdlm_road_project_coordinates_km(locations)
    m <- gmde_scalar_integer(m, "m", 1L, nrow(coordinates))
    q <- gmde_scalar_integer(q, "q", 1L, nrow(coordinates) - 1L)
    distance <- as.matrix(stats::dist(coordinates))
    ordered <- t(apply(distance, 1L, sort))
    range_km <- stats::median(ordered[, q + 1L])
    if (!is.finite(range_km) || range_km <= 0) {
        stop("The Euclidean Matern range rule failed.", call. = FALSE)
    }
    scaled <- sqrt(3) * distance / range_km
    covariance <- (1 + scaled) * exp(-scaled)
    decomposition <- eigen(covariance, symmetric = TRUE)
    tolerance <- 1e-10 * max(1, abs(decomposition$values))
    if (min(decomposition$values) < -tolerance) {
        stop("The Euclidean Matern covariance is not positive semidefinite.",
             call. = FALSE)
    }
    value <- pmax(decomposition$values, 0)
    raw <- decomposition$vectors[, seq_len(m), drop = FALSE] %*%
        diag(sqrt(value[seq_len(m)]), m, m)
    basis <- raw * target_rms_sd / sqrt(mean(rowSums(raw^2)))
    attr(basis, "range_km") <- range_km
    attr(basis, "kernel") <- "Matern nu=3/2"
    basis
}

countdlm_road_method_inputs <- function(context) {
    metadata <- context$metadata
    phi_binary <- gmde_graph_basis(
        context$A,
        m = as.integer(metadata$basis_m),
        nu = metadata$basis_nu,
        length_scale = metadata$basis_length_scale,
        target_rms_sd = metadata$basis_target_rms_sd
    )
    phi_euclidean <- countdlm_road_euclidean_basis(
        context$locations,
        m = as.integer(metadata$basis_m),
        q = as.integer(context$q),
        target_rms_sd = metadata$basis_target_rms_sd
    )
    list(
        `GMDE-W` = list(
            Phi = context$Phi,
            graph_meta = list(
                allocation = "weighted road qNN",
                q = context$q,
                h_km = context$h
            )
        ),
        `GMDE-C` = list(
            Phi = phi_binary,
            graph_meta = list(
                allocation = "binary support of weighted road qNN",
                q = context$q,
                edges = sum(context$A[upper.tri(context$A)])
            )
        ),
        `Euc-MDE` = list(
            Phi = phi_euclidean,
            graph_meta = list(
                allocation = "Euclidean Matern",
                nu = 1.5,
                range_rule = "median fourth-nearest Euclidean distance",
                range_km = attr(phi_euclidean, "range_km")
            )
        ),
        `MoDE` = list(
            alpha_pi = rep(1 / 10, 10L),
            graph_meta = list(allocation = "global Dirichlet")
        ),
        `Potts-MDE` = list(
            potts_W = context$W,
            graph_meta = list(
                allocation = "weighted road Potts field",
                q = context$q,
                h_km = context$h
            )
        )
    )
}

#' Generate the paper's single moderate dynamic-count mechanism
#'
#' @param context Validated approved road context.
#' @param seed Fixed calibration or evaluation data seed.
#' @return Counts, design matrix, truth paths, and road-distance labels.
#' @export
countdlm_road_generate_moderate <- function(context, seed) {
    seed <- gmde_scalar_integer(seed, "seed", 1L, .Machine$integer.max - 1L)
    set.seed(seed)
    n <- nrow(context$locations)
    TT <- 168L
    K <- 5L
    time <- seq_len(TT)
    Fmat <- cbind(intercept = 1, daily_sine = sin(2 * pi * time / 24))
    state_W <- diag(c(1e-6, 5e-7))
    theta <- array(NA_real_, c(K, TT, 2L))
    mu <- 25 * exp(0.035 * (seq_len(K) - 3))
    for (k in seq_len(K)) {
        theta[k, 1L, ] <- c(log(mu[k]), 0.20)
        if (TT > 1L) {
            for (tt in 2:TT) {
                theta[k, tt, ] <- theta[k, tt - 1L, ] +
                    stats::rnorm(2L, sd = sqrt(diag(state_W)))
            }
        }
    }
    eta <- matrix(NA_real_, K, TT)
    for (k in seq_len(K)) {
        eta[k, ] <- rowSums(Fmat * theta[k, , ])
    }
    Z <- context$locations$ring_label
    lambda <- exp(eta[Z, , drop = FALSE])
    Y <- matrix(stats::rpois(n * TT, lambda), nrow = n, ncol = TT)
    list(
        Y = Y,
        Fmat = Fmat,
        Z = Z,
        lambda = lambda,
        theta = theta,
        state_W = state_W,
        seed = seed,
        dgp = "single-moderate-Poisson-DLM-road-groups-v1"
    )
}

#' Construct a full-size quick-test configuration
#'
#' The quick test keeps n=100, T=168, Kmax=10 and the exact PG backend.  It
#' shortens only the number of MCMC transitions, so its measured cost can be
#' used for a conservative full-run projection.
#'
#' @param context_file Approved external road-context RDS.
#' @param output_dir Brand-new external output directory.
#' @param cores Number of independent method workers.
#' @param quick_iterations,quick_burn Short exact-chain lengths.  The defaults
#'   are long enough to reduce fixed-startup distortion while remaining a
#'   noninferential timing run.
#' @param rho_grid Provisional positive-integer geometric grid.  The quick test
#'   times its largest value; it does not freeze the final shared rho.
#' @param potts_beta_grid Provisional short sensitivity grid.  The quick test
#'   times every value; it does not select the final beta.
#' @param target_full_iterations,target_full_chains Projection targets.
#' @param full_budget_hours Total desired wall-clock limit.
#' @param budget_fraction Fraction reserved for projected computation.
#' @param projection_multiplier Conservative timing multiplier.
#' @param max_projected_replicates Largest replication count to display.
#' @return A validated quick-test configuration.
#' @export
countdlm_road_quick_config <- function(
    context_file,
    output_dir,
    cores = 4L,
    quick_iterations = 30L,
    quick_burn = 10L,
    rho_grid = c(1L, 2L, 4L),
    potts_beta_grid = c(0.25, 0.5, 1),
    target_full_iterations = 1000L,
    target_full_chains = 2L,
    full_budget_hours = 12,
    budget_fraction = 0.80,
    projection_multiplier = 2.00,
    max_projected_replicates = 30L
) {
    cores <- gmde_scalar_integer(cores, "cores", 1L, 16L)
    quick_iterations <- gmde_scalar_integer(
        quick_iterations, "quick_iterations", 4L, 100L
    )
    quick_burn <- gmde_scalar_integer(
        quick_burn, "quick_burn", 1L, quick_iterations - 1L
    )
    target_full_iterations <- gmde_scalar_integer(
        target_full_iterations, "target_full_iterations", 100L, 10000L
    )
    target_full_chains <- gmde_scalar_integer(
        target_full_chains, "target_full_chains", 2L, 8L
    )
    max_projected_replicates <- gmde_scalar_integer(
        max_projected_replicates, "max_projected_replicates", 1L, 500L
    )
    rho_grid <- as.numeric(rho_grid)
    if (length(rho_grid) < 2L || any(!is.finite(rho_grid)) ||
        any(rho_grid <= 0) || any(rho_grid != round(rho_grid)) ||
        anyDuplicated(rho_grid) || any(diff(rho_grid) <= 0)) {
        stop("rho_grid must be an increasing set of distinct positive integers.",
             call. = FALSE)
    }
    ratios <- rho_grid[-1L] / rho_grid[-length(rho_grid)]
    if (max(abs(log(ratios) - mean(log(ratios)))) > 1e-8) {
        stop("rho_grid must be geometric.", call. = FALSE)
    }
    potts_beta_grid <- as.numeric(potts_beta_grid)
    if (length(potts_beta_grid) < 2L || length(potts_beta_grid) > 7L ||
        any(!is.finite(potts_beta_grid)) || any(potts_beta_grid < 0) ||
        anyDuplicated(potts_beta_grid) || any(diff(potts_beta_grid) <= 0)) {
        stop(paste(
            "potts_beta_grid must contain 2--7 increasing distinct",
            "nonnegative values."
        ),
             call. = FALSE)
    }
    beta_slug <- gsub(
        "[.]", "p",
        format(potts_beta_grid, trim = TRUE, scientific = FALSE)
    )
    quick_tasks <- rbind(
        data.frame(
            task_id = countdlm_road_internal_methods[
                countdlm_road_internal_methods != "Potts-MDE"
            ],
            method = countdlm_road_internal_methods[
                countdlm_road_internal_methods != "Potts-MDE"
            ],
            potts_beta = NA_real_,
            stringsAsFactors = FALSE
        ),
        data.frame(
            task_id = paste0("Potts-MDE-beta-", beta_slug),
            method = "Potts-MDE",
            potts_beta = potts_beta_grid,
            stringsAsFactors = FALSE
        )
    )
    scalar_probability <- function(x, name, include_one = FALSE) {
        upper_ok <- if (include_one) x <= 1 else x < 1
        if (length(x) != 1L || !is.numeric(x) || !is.finite(x) ||
            x <= 0 || !upper_ok) {
            interval <- if (include_one) "(0, 1]" else "(0, 1)"
            stop(name, " must be in ", interval, ".", call. = FALSE)
        }
        as.numeric(x)
    }
    budget_fraction <- scalar_probability(
        budget_fraction, "budget_fraction", include_one = TRUE
    )
    if (length(full_budget_hours) != 1L ||
        !is.numeric(full_budget_hours) || !is.finite(full_budget_hours) ||
        full_budget_hours <= 0 || length(projection_multiplier) != 1L ||
        !is.numeric(projection_multiplier) ||
        !is.finite(projection_multiplier) || projection_multiplier < 1) {
        stop("The budget must be positive and projection_multiplier >= 1.",
             call. = FALSE)
    }
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    config <- list(
        api_version = countdlm_road_benchmark_api_version,
        scientific_role = "noninferential full-size exact runtime quick test",
        context_file = normalizePath(
            context_file, winslash = "/", mustWork = TRUE
        ),
        context_sha256 = countdlm_road_context_sha256,
        output_dir = output_dir,
        methods = countdlm_road_internal_methods,
        quick_tasks = quick_tasks,
        unavailable_external_methods = countdlm_road_external_methods,
        n = 100L,
        TT = 168L,
        K_true = 5L,
        K_fit = 10L,
        basis_m = 40L,
        quick_iterations = quick_iterations,
        quick_burn = quick_burn,
        quick_chains = 1L,
        cores = min(cores, nrow(quick_tasks)),
        progress_poll_seconds = 0.5,
        rho_grid_provisional = rho_grid,
        rho_timing = max(rho_grid),
        potts_beta_grid_provisional = potts_beta_grid,
        target_full_iterations = target_full_iterations,
        target_full_chains = target_full_chains,
        full_budget_hours = as.numeric(full_budget_hours),
        budget_fraction = budget_fraction,
        projection_multiplier = as.numeric(projection_multiplier),
        max_projected_replicates = max_projected_replicates,
        data_seed = 2026090211L,
        initialization_seed = 2026090212L,
        method_seed_base = 2026090300L,
        state_G = diag(2),
        state_W = diag(c(1e-6, 5e-7)),
        state_C0 = diag(c(2, 1)),
        substantive_min = 5L,
        pg_backend = "devroye-exact",
        algorithm_exact = TRUE,
        classification_only = TRUE,
        projected_internal_scope = paste(
            "four non-Potts methods plus the complete provisional Potts beta",
            "grid for every projected chain; every beta is timed separately",
            "and rho is timed at its largest value"
        ),
        external_status = paste(
            "PNARM and AR(1) latent class are not timed: exact upstream",
            "revisions/adapters are unavailable and placeholder fits are forbidden."
        )
    )
    config$config_signature <- countdlm_road_config_signature(config)
    structure(config, class = "countdlm_road_quick_config")
}

countdlm_road_git_state <- function(repository_root) {
    candidate <- normalizePath(
        repository_root, winslash = "/", mustWork = TRUE
    )
    if (!dir.exists(candidate)) {
        stop("repository_root must identify a directory.", call. = FALSE)
    }
    root_query <- suppressWarnings(system2(
        "git", c("-C", candidate, "rev-parse", "--show-toplevel"),
        stdout = TRUE, stderr = TRUE
    ))
    if (!length(root_query) || !is.null(attr(root_query, "status")) &&
        attr(root_query, "status") != 0L) {
        stop("Could not resolve the containing Git root.", call. = FALSE)
    }
    root <- normalizePath(
        trimws(root_query[[1L]]), winslash = "/", mustWork = TRUE
    )
    run_git <- function(args) {
        value <- suppressWarnings(system2(
            "git", c("-C", root, args), stdout = TRUE, stderr = TRUE
        ))
        if (!is.null(attr(value, "status")) && attr(value, "status") != 0L) {
            stop("Git provenance query failed.", call. = FALSE)
        }
        value
    }
    list(
        root = root,
        head = trimws(run_git(c("rev-parse", "HEAD"))[[1L]]),
        status = run_git(c("status", "--porcelain")),
        clean = length(run_git(c("status", "--porcelain"))) == 0L
    )
}

countdlm_road_require_atomic_parent <- function(path) {
    parent <- dirname(path)
    if (!dir.exists(parent)) {
        stop(
            "Registered output parent is unavailable: ", parent,
            ". The run directory may have been moved or deleted; it will ",
            "not be recreated.", call. = FALSE
        )
    }
    if (file.access(parent, mode = 2L) != 0L) {
        stop(
            "Registered output parent is not writable: ", parent,
            call. = FALSE
        )
    }
    invisible(parent)
}

countdlm_road_atomic_save_rds <- function(object, path) {
    countdlm_road_require_atomic_parent(path)
    if (file.exists(path)) {
        stop("Refusing to overwrite immutable output: ", path,
             call. = FALSE)
    }
    temporary <- paste0(path, ".tmp-", Sys.getpid())
    saveRDS(object, temporary, version = 3)
    if (!file.rename(temporary, path)) {
        stop("Could not atomically publish output: ", path, call. = FALSE)
    }
    invisible(path)
}

countdlm_road_atomic_write_csv <- function(object, path) {
    countdlm_road_require_atomic_parent(path)
    if (file.exists(path)) {
        stop("Refusing to overwrite immutable output: ", path,
             call. = FALSE)
    }
    temporary <- paste0(path, ".tmp-", Sys.getpid())
    utils::write.csv(object, temporary, row.names = FALSE, na = "")
    if (!file.rename(temporary, path)) {
        stop("Could not atomically publish output: ", path, call. = FALSE)
    }
    invisible(path)
}

countdlm_road_atomic_write_lines <- function(text, path) {
    countdlm_road_require_atomic_parent(path)
    if (file.exists(path)) {
        stop("Refusing to overwrite immutable output: ", path,
             call. = FALSE)
    }
    temporary <- paste0(path, ".tmp-", Sys.getpid())
    writeLines(text, temporary, useBytes = TRUE)
    if (!file.rename(temporary, path)) {
        stop("Could not atomically publish output: ", path, call. = FALSE)
    }
    invisible(path)
}

countdlm_road_write_checksums <- function(output_dir) {
    checksum_path <- file.path(output_dir, "CHECKSUMS.sha256")
    if (file.exists(checksum_path)) {
        stop("Refusing to overwrite an output checksum manifest.",
             call. = FALSE)
    }
    files <- sort(list.files(
        output_dir, recursive = TRUE, full.names = TRUE,
        include.dirs = FALSE, all.files = TRUE, no.. = TRUE
    ))
    files <- files[basename(files) != basename(checksum_path) &
        !grepl("[.]tmp-[0-9]+$", files)]
    relative <- substring(files, nchar(output_dir) + 2L)
    hashes <- vapply(files, countdlm_road_sha256, character(1))
    countdlm_road_atomic_write_lines(
        paste0(hashes, "  ", relative), checksum_path
    )
    checksum_path
}

countdlm_road_nmi_vi <- function(truth, estimate) {
    contingency <- table(as.integer(truth), as.integer(estimate))
    probability <- contingency / sum(contingency)
    row_probability <- rowSums(probability)
    column_probability <- colSums(probability)
    nonzero <- probability > 0
    mutual_information <- sum(probability[nonzero] * log(
        probability[nonzero] /
            outer(row_probability, column_probability)[nonzero]
    ))
    entropy <- function(value) {
        value <- value[value > 0]
        -sum(value * log(value))
    }
    entropy_truth <- entropy(row_probability)
    entropy_estimate <- entropy(column_probability)
    denominator <- sqrt(entropy_truth * entropy_estimate)
    list(
        nmi = if (denominator > 0) mutual_information / denominator else 1,
        vi = entropy_truth + entropy_estimate - 2 * mutual_information
    )
}

countdlm_road_pairwise_error <- function(truth, similarity) {
    truth_matrix <- outer(truth, truth, "==")
    index <- upper.tri(truth_matrix)
    mean((similarity[index] - truth_matrix[index])^2)
}

countdlm_road_summarize_fit <- function(
    fit, task_id, method, potts_beta, burn, elapsed,
    warnings = character()
) {
    warnings <- unique(as.character(warnings))
    post_index <- seq.int(burn + 1L, nrow(fit$Z))
    summary <- data.frame(
        task_id = task_id,
        method = method,
        status = "ok",
        elapsed_seconds = as.numeric(elapsed),
        seconds_per_iteration = as.numeric(elapsed) / nrow(fit$Z),
        iterations = nrow(fit$Z),
        burn = burn,
        rho_timed = fit$settings$rho,
        potts_beta_timed = potts_beta,
        median_occupied = stats::median(fit$occupied_experts[post_index]),
        median_substantive = stats::median(
            fit$substantive_experts[post_index]
        ),
        state_acceptance = fit$postburn_state_acceptance_rate,
        mean_observed_loglik = if (all(is.na(
            fit$observed_loglik[post_index]
        ))) NA_real_ else mean(
            fit$observed_loglik[post_index], na.rm = TRUE
        ),
        mean_complete_diagnostic = mean(fit$loglik[post_index]),
        algorithm_exact = isTRUE(fit$algorithm_exact),
        warning_count = length(warnings),
        warnings = if (length(warnings)) {
            paste(warnings, collapse = " | ")
        } else NA_character_,
        failure_stage = NA_character_,
        error = NA_character_,
        stringsAsFactors = FALSE
    )
    compact <- list(
        api_version = countdlm_road_benchmark_api_version,
        task_id = task_id,
        method = method,
        scientific_role = "runtime-only; truth metrics intentionally blinded",
        warnings = warnings,
        summary = summary,
        truth_metrics_computed = FALSE,
        observed_loglik_postburn = fit$observed_loglik[post_index],
        complete_diagnostic_postburn = fit$loglik[post_index],
        occupied_postburn = fit$occupied_experts[post_index],
        substantive_postburn = fit$substantive_experts[post_index],
        state_accepted_postburn = fit$state_accepted[
            post_index, , drop = FALSE
        ],
        state_update_seconds = fit$state_update_seconds,
        state_pg_seconds = fit$state_pg_seconds,
        state_pg_shape_sum = fit$state_pg_shape_sum,
        state_pg_shape_max = fit$state_pg_shape_max,
        ess_bracket_evaluations = fit$ess_bracket_evaluations,
        ess_likelihood_evaluations = fit$ess_likelihood_evaluations,
        settings = fit$settings
    )
    list(summary = summary, compact = compact)
}

countdlm_road_failure_summary <- function(
    task_id, method, potts_beta, elapsed, config, error, stage,
    warnings = character()
) {
    warnings <- unique(as.character(warnings))
    data.frame(
        task_id = task_id,
        method = method,
        status = "error",
        elapsed_seconds = as.numeric(elapsed),
        seconds_per_iteration = NA_real_,
        iterations = config$quick_iterations,
        burn = config$quick_burn,
        rho_timed = config$rho_timing,
        potts_beta_timed = potts_beta,
        median_occupied = NA_real_,
        median_substantive = NA_real_,
        state_acceptance = NA_real_,
        mean_observed_loglik = NA_real_,
        mean_complete_diagnostic = NA_real_,
        algorithm_exact = NA,
        warning_count = length(warnings),
        warnings = if (length(warnings)) {
            paste(warnings, collapse = " | ")
        } else NA_character_,
        failure_stage = stage,
        error = as.character(error),
        stringsAsFactors = FALSE
    )
}

#' Fit one explicitly named internal road-benchmark method
#'
#' Unknown or unavailable method names fail closed; they are never routed to a
#' fallback sampler.
#'
#' @param method One of the five internally matched methods.
#' @param data Moderate-DGP output.
#' @param method_inputs Output of the internal method-input constructor.
#' @param config Quick configuration.
#' @param seed Method-specific MCMC seed.
#' @param Z_init Common initialization labels.
#' @param theta_init Optional fixed initial state-path array.
#' @param gamma_init Optional fixed graph-classifier contrast matrix.
#' @param store_prediction_state Whether to retain post-burn prediction state.
#' @param store_sampler_terminal_state Whether to retain the complete terminal
#'   Markov state and R RNG state for exact fixed-rho GMDE continuation.
#' @param store_allocation_conditionals Whether to retain passive, pre-draw
#'   conditional allocation-mechanism diagnostics.  This is supported only by
#'   the three GMDE variants and does not consume random numbers.
#' @param rng_state_init Optional complete R RNG state for controlled starts.
#' @param resume_state Optional terminal state from a prior fixed-rho GMDE run;
#'   it cannot be combined with a seed or explicit initialization arguments.
#' @param potts_beta Explicit fixed beta for Potts-MDE; `NULL` otherwise.
#' @param n_iter,burn MCMC length overrides.  Defaults come from the supplied
#'   quick-test configuration.
#' @param rho Fixed structured multiplier.  Set it to `NULL` only when all four
#'   explicit rho-calibration controls are supplied.
#' @param rho_grid,rho_warmup,rho_tie_break,rho_schedule Optional explicit
#'   warm-up calibration controls passed to the shared sampler.
#' @return A fitted sampler object.
#' @export
countdlm_road_fit_method <- function(
    method, data, method_inputs, config, seed = NULL, Z_init = NULL,
    potts_beta = NULL,
    n_iter = config$quick_iterations,
    burn = config$quick_burn,
    rho = config$rho_timing,
    rho_grid = NULL,
    rho_warmup = NULL,
    rho_tie_break = NULL,
    rho_schedule = NULL,
    theta_init = NULL,
    gamma_init = NULL,
    store_prediction_state = FALSE,
    store_sampler_terminal_state = FALSE,
    store_allocation_conditionals = FALSE,
    rng_state_init = NULL,
    resume_state = NULL
) {
    if (length(method) != 1L || !is.character(method) || is.na(method) ||
        !method %in% countdlm_road_internal_methods) {
        stop(
            "method must exactly match one of: ",
            paste(countdlm_road_internal_methods, collapse = ", "),
            ".", call. = FALSE
        )
    }
    if (method == "Potts-MDE") {
        if (length(potts_beta) != 1L || !is.numeric(potts_beta) ||
            !is.finite(potts_beta) || potts_beta < 0) {
            stop("Potts-MDE requires one explicit nonnegative potts_beta.",
                 call. = FALSE)
        }
    } else if (!is.null(potts_beta)) {
        stop("potts_beta applies only to Potts-MDE.", call. = FALSE)
    }
    common <- list(
        Y = data$Y,
        Fmat = data$Fmat,
        K = config$K_fit,
        n_iter = n_iter,
        burn = burn,
        m0 = c(log(mean(data$Y) + 0.1), 0),
        C0 = config$state_C0,
        G = config$state_G,
        W = config$state_W,
        rho = rho,
        rho_grid = rho_grid,
        rho_warmup = rho_warmup,
        rho_tie_break = rho_tie_break,
        rho_schedule = rho_schedule,
        Z_init = Z_init,
        theta_init = theta_init,
        gamma_init = gamma_init,
        substantive_min = config$substantive_min,
        pg_backend = config$pg_backend,
        store_prediction_state = store_prediction_state,
        store_sampler_terminal_state = store_sampler_terminal_state,
        store_allocation_conditionals = store_allocation_conditionals,
        rng_state_init = rng_state_init,
        resume_state = resume_state,
        print_freq = 0L,
        seed = seed
    )
    switch(
        method,
        `GMDE-W` = do.call(
            run_gmde_mcmc,
            c(common, method_inputs[[method]][c("Phi", "graph_meta")])
        ),
        `GMDE-C` = do.call(
            run_gmde_mcmc,
            c(common, method_inputs[[method]][c("Phi", "graph_meta")])
        ),
        `Euc-MDE` = do.call(
            run_gmde_mcmc,
            c(common, method_inputs[[method]][c("Phi", "graph_meta")])
        ),
        MoDE = do.call(
            run_nograph_mcmc,
            c(common, method_inputs[[method]][c("alpha_pi", "graph_meta")])
        ),
        `Potts-MDE` = do.call(
            run_potts_mcmc,
            c(
                common,
                method_inputs[[method]][c("potts_W", "graph_meta")],
                list(potts_beta = potts_beta)
            )
        ),
        stop("Unreachable method dispatch.", call. = FALSE)
    )
}

countdlm_road_project_runtime <- function(
    summary, config, actual_workers = config$cores
) {
    countdlm_road_validate_quick_config(config)
    actual_workers <- gmde_scalar_integer(
        actual_workers, "actual_workers", 1L, nrow(config$quick_tasks)
    )
    required <- c(
        "task_id", "method", "status", "elapsed_seconds", "iterations",
        "burn", "rho_timed", "potts_beta_timed", "algorithm_exact"
    )
    blocked <- function() list(
        table = data.frame(),
        recommended_replicates = 0L,
        gate_hours = config$full_budget_hours * config$budget_fraction,
        internal_budget_gate_passed = FALSE,
        covered_scope = config$projected_internal_scope,
        excluded_scope = paste(
            "PNARM and AR(1) latent class; convergence-triggered extension",
            "or reruns; any future beta values outside the registered grid"
        )
    )
    if (!is.data.frame(summary) || !all(required %in% names(summary)) ||
        nrow(summary) != nrow(config$quick_tasks)) {
        return(blocked())
    }
    expected_beta <- config$quick_tasks$potts_beta
    observed_beta <- summary$potts_beta_timed
    beta_ok <- (is.na(expected_beta) & is.na(observed_beta)) |
        (!is.na(expected_beta) & !is.na(observed_beta) &
         abs(expected_beta - observed_beta) < 1e-12)
    ok <- summary$status == "ok" & is.finite(summary$elapsed_seconds) &
        summary$elapsed_seconds > 0 &
        summary$iterations == config$quick_iterations &
        summary$burn == config$quick_burn &
        abs(summary$rho_timed - config$rho_timing) < 1e-12 &
        summary$algorithm_exact %in% TRUE & beta_ok
    ok[is.na(ok)] <- FALSE
    if (!all(ok) ||
        !identical(summary$task_id, config$quick_tasks$task_id) ||
        !identical(summary$method, config$quick_tasks$method)) {
        return(blocked())
    }
    seconds_per_full_chain <- summary$elapsed_seconds *
        config$target_full_iterations / config$quick_iterations *
        config$projection_multiplier
    names(seconds_per_full_chain) <- summary$task_id
    list_schedule_upper_bound <- function(durations, workers) {
        ## Valid for any work-conserving list schedule, including the FIFO
        ## dynamic scheduler intended for the later formal runner.
        sum(durations) / workers +
            (1 - 1 / workers) * max(durations)
    }
    projection <- do.call(rbind, lapply(
        seq_len(config$max_projected_replicates),
        function(replicates) {
            ## Every registered Potts beta is timed and projected separately.
            ## The largest rho is used for all timing tasks because exact PG
            ## cost increases with the integer shape contribution from rho.
            one_chain <- seconds_per_full_chain
            tasks <- rep(
                one_chain,
                times = config$target_full_chains * replicates
            )
            data.frame(
                replicates = replicates,
                chains = config$target_full_chains,
                iterations = config$target_full_iterations,
                workers = actual_workers,
                internal_method_families = length(config$methods),
                potts_beta_values = length(
                    config$potts_beta_grid_provisional
                ),
                chain_jobs = length(tasks),
                raw_work_hours = sum(tasks) /
                    config$projection_multiplier / 3600,
                conservative_wall_hours = list_schedule_upper_bound(
                    tasks, actual_workers
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
        table = projection,
        method_seconds_per_full_chain = seconds_per_full_chain,
        recommended_replicates = as.integer(recommended),
        gate_hours = gate,
        internal_budget_gate_passed = recommended >= 1L,
        covered_scope = config$projected_internal_scope,
        excluded_scope = paste(
            "PNARM and AR(1) latent class; convergence-triggered extension",
            "or reruns; any future beta values outside the registered grid"
        )
    )
}

countdlm_road_format_duration <- function(seconds) {
    if (!is.finite(seconds) || seconds < 0) return("unknown")
    hours <- floor(seconds / 3600)
    minutes <- floor((seconds %% 3600) / 60)
    remaining <- round(seconds %% 60)
    sprintf("%02d:%02d:%02d", hours, minutes, remaining)
}

countdlm_road_scheduler_error <- function(
    index, message, elapsed_seconds,
    origin = c("worker-uncaught", "scheduler-collect")
) {
    origin <- match.arg(origin)
    structure(list(
        index = as.integer(index),
        message = paste(as.character(message), collapse = " "),
        elapsed_seconds = as.numeric(elapsed_seconds),
        origin = origin
    ), class = "countdlm_road_scheduler_error")
}

countdlm_road_run_batches <- function(
    methods, worker, cores, poll_seconds = 0.5
) {
    total <- length(methods)
    if (!total || anyDuplicated(methods)) {
        stop("methods must be a nonempty unique vector.", call. = FALSE)
    }
    cores <- gmde_scalar_integer(cores, "cores", 1L, total)
    if (length(poll_seconds) != 1L || !is.numeric(poll_seconds) ||
        !is.finite(poll_seconds) || poll_seconds < 0.1 ||
        poll_seconds > 5) {
        stop("poll_seconds must be between 0.1 and 5 seconds.",
             call. = FALSE)
    }
    completed <- 0L
    started <- Sys.time()
    result <- vector("list", total)
    names(result) <- methods
    task_seconds <- rep(NA_real_, total)
    last_width <- 0L
    clear_progress <- function() {
        if (last_width > 0L) {
            cat("\r", paste(rep(" ", last_width), collapse = ""), "\r",
                sep = "")
            utils::flush.console()
        }
    }
    show_progress <- function(active = integer(0), pending = integer(0)) {
        clear_progress()
        width <- 28L
        fraction <- completed / total
        filled <- min(width, floor(width * fraction))
        bar <- paste0(
            "[", paste(rep("=", filled), collapse = ""),
            if (filled < width) ">" else "",
            paste(rep(" ", max(0L, width - filled - (filled < width))),
                  collapse = ""), "]"
        )
        elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
        observed <- task_seconds[is.finite(task_seconds) & task_seconds > 0]
        eta <- if (!length(observed) || completed == total) {
            if (completed == total) 0 else NA_real_
        } else {
            remaining_work <- stats::median(observed) *
                (length(active) + length(pending))
            remaining_work / max(1L, min(cores, total - completed))
        }
        active_text <- if (length(active)) {
            paste0(" | running: ", paste(methods[active], collapse = ","))
        } else ""
        line <- paste0(
            bar, " ", completed, "/", total, " (",
            sprintf("%5.1f", 100 * fraction), "%) | elapsed ",
            countdlm_road_format_duration(elapsed), " | ETA ",
            countdlm_road_format_duration(eta), active_text
        )
        cat("\r", line, sep = "")
        utils::flush.console()
        last_width <<- max(last_width, nchar(line))
        invisible(NULL)
    }
    run_one <- function(index) {
        task_started <- Sys.time()
        value <- tryCatch(
            worker(index),
            error = function(error) countdlm_road_scheduler_error(
                index, conditionMessage(error),
                as.numeric(difftime(Sys.time(), task_started, units = "secs")),
                origin = "worker-uncaught"
            )
        )
        list(
            index = index,
            value = value,
            elapsed_seconds = as.numeric(difftime(
                Sys.time(), task_started, units = "secs"
            ))
        )
    }
    note_result <- function(record) {
        index <- as.integer(record$index)
        result[[index]] <<- record$value
        task_seconds[[index]] <<- as.numeric(record$elapsed_seconds)
        completed <<- completed + 1L
        clear_progress()
        status_suffix <- if (inherits(
            record$value, "countdlm_road_scheduler_error"
        )) {
            if (identical(record$value$origin, "scheduler-collect")) {
                " | scheduler collection error retained"
            } else {
                " | uncaught worker error retained"
            }
        } else if (is.list(record$value) &&
                   is.data.frame(record$value$summary) &&
                   nrow(record$value$summary) == 1L &&
                   identical(record$value$summary$status[[1L]], "error")) {
            if (isFALSE(record$value$failure_record_saved)) {
                " | worker error; failure record unavailable"
            } else {
                " | worker error retained"
            }
        } else ""
        cat(
            "[DONE] ", methods[[index]], " | ", completed, "/", total,
            " | ", countdlm_road_format_duration(task_seconds[[index]]),
            status_suffix,
            "\n", sep = ""
        )
    }

    if (cores == 1L || .Platform$OS.type != "unix") {
        if (cores > 1L) {
            warning("Fork scheduling is unavailable; using one worker.",
                    call. = FALSE)
        }
        pending <- seq_len(total)
        show_progress(pending = pending)
        for (index in pending) {
            clear_progress()
            cat("[START] ", methods[[index]], "\n", sep = "")
            record <- run_one(index)
            note_result(record)
            show_progress(pending = pending[pending > index])
        }
        clear_progress()
        return(result)
    }

    pending <- seq_len(total)
    active <- list()
    cleanup_active <- TRUE
    on.exit({
        clear_progress()
        if (cleanup_active && length(active)) {
            for (item in active) {
                try(tools::pskill(item$job$pid, tools::SIGTERM), silent = TRUE)
            }
            try(parallel::mccollect(
                lapply(active, `[[`, "job"), wait = FALSE
            ), silent = TRUE)
        }
    }, add = TRUE)
    launch_available <- function() {
        while (length(pending) && length(active) < cores) {
            index <- pending[[1L]]
            pending <<- pending[-1L]
            job <- parallel::mcparallel(
                run_one(index), mc.set.seed = FALSE, silent = TRUE
            )
            active[[as.character(job$pid)]] <<- list(
                job = job, index = index, started = Sys.time()
            )
            clear_progress()
            cat("[START] ", methods[[index]], "\n", sep = "")
        }
    }
    launch_available()
    show_progress(
        active = vapply(active, `[[`, integer(1), "index"),
        pending = pending
    )
    while (length(active)) {
        jobs <- lapply(active, `[[`, "job")
        names(jobs) <- names(active)
        collected <- suppressWarnings(parallel::mccollect(
            jobs, wait = FALSE
        ))
        if (!is.null(collected) && length(collected)) {
            for (pid in names(collected)) {
                item <- active[[pid]]
                record <- collected[[pid]]
                if (inherits(record, "try-error") || !is.list(record) ||
                    is.null(record$index) || is.null(record$value)) {
                    elapsed <- as.numeric(difftime(
                        Sys.time(), item$started, units = "secs"
                    ))
                    record <- list(
                        index = item$index,
                        value = countdlm_road_scheduler_error(
                            item$index, record, elapsed,
                            origin = "scheduler-collect"
                        ),
                        elapsed_seconds = elapsed
                    )
                }
                note_result(record)
                active[[pid]] <- NULL
            }
            launch_available()
        }
        active_index <- if (length(active)) {
            vapply(active, `[[`, integer(1), "index")
        } else integer(0)
        show_progress(active = active_index, pending = pending)
        if (length(active)) Sys.sleep(poll_seconds)
    }
    cleanup_active <- FALSE
    clear_progress()
    if (completed != total || any(vapply(result, is.null, logical(1)))) {
        stop(
            "The dynamic method scheduler ended with missing results.",
            call. = FALSE
        )
    }
    result
}

#' Run the full-size exact quick test and project a 12-hour internal benchmark
#'
#' This is a noninferential timing run.  It creates a brand-new external
#' directory, shows a Console progress bar, retains failures, and never launches
#' a formal simulation automatically.
#'
#' @param config Output of `countdlm_road_quick_config()`.
#' @param repository_root Git root containing the source used for the quick run.
#' @return Quick summaries, a conservative projection, and output paths.
#' @export
countdlm_road_quick_test <- function(config, repository_root) {
    quick_started <- Sys.time()
    countdlm_road_validate_quick_config(config)
    if (!requireNamespace("BayesLogit", quietly = TRUE)) {
        stop("The exact quick test requires the installed BayesLogit package.",
             call. = FALSE)
    }
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("The quick test requires the installed digest package.",
             call. = FALSE)
    }
    git <- countdlm_road_git_state(repository_root)
    actual_workers <- if (.Platform$OS.type == "unix") {
        config$cores
    } else 1L
    output_parent <- normalizePath(
        dirname(config$output_dir), winslash = "/", mustWork = TRUE
    )
    output_dir <- file.path(output_parent, basename(config$output_dir))
    if (!identical(output_dir, config$output_dir)) {
        stop("The output path changed after parent-path canonicalization.",
             call. = FALSE)
    }
    root_prefix <- paste0(git$root, "/")
    if (identical(output_dir, git$root) ||
        startsWith(paste0(output_dir, "/"), root_prefix)) {
        stop("Quick-test output must be outside the Git repository.",
             call. = FALSE)
    }
    if (file.exists(output_dir)) {
        stop("output_dir must not already exist: ", output_dir,
             call. = FALSE)
    }
    if (!dir.create(output_dir, recursive = FALSE, showWarnings = FALSE)) {
        stop("Could not create the registered output directory.",
             call. = FALSE)
    }
    run_complete <- FALSE
    run_stage <- "registration"
    on.exit({
        if (!run_complete) {
            incomplete_path <- file.path(output_dir, "RUN-INCOMPLETE.rds")
            if (!file.exists(incomplete_path)) {
                try(countdlm_road_atomic_save_rds(
                    list(
                        api_version = config$api_version,
                        status = "failed-or-interrupted",
                        last_stage = run_stage,
                        recorded_at = format(
                            Sys.time(), tz = "UTC", usetz = TRUE
                        ),
                        temporary_files_retained = list.files(
                            output_dir, pattern = "[.]tmp-", recursive = TRUE,
                            full.names = FALSE
                        )
                    ),
                    incomplete_path
                ), silent = TRUE)
            }
        }
    }, add = TRUE)
    source_files <- sort(list.files(
        file.path(git$root, "R"), pattern = "[.]R$", full.names = TRUE
    ))
    registration <- list(
        api_version = config$api_version,
        created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
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
            BayesLogit_version = as.character(
                utils::packageVersion("BayesLogit")
            ),
            digest_version = as.character(utils::packageVersion("digest")),
            physical_cores_reported = parallel::detectCores(logical = FALSE),
            logical_cores_reported = parallel::detectCores(logical = TRUE),
            requested_workers = config$cores,
            actual_workers = actual_workers,
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
            "Full-size exact timing only; rho/beta are provisional and",
            "PNARM/AR1 are unavailable, so this is not a formal comparison."
        )
    )
    countdlm_road_atomic_save_rds(
        registration, file.path(output_dir, "road-quick-registration.rds")
    )
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            status = "running",
            started_at = format(quick_started, tz = "UTC", usetz = TRUE)
        ),
        file.path(output_dir, "RUN-STARTED.rds")
    )
    run_stage <- "approved-context-and-data"
    context <- countdlm_road_load_approved_context(config$context_file)
    method_inputs <- countdlm_road_method_inputs(context)
    data <- countdlm_road_generate_moderate(context, config$data_seed)
    set.seed(config$initialization_seed)
    Z_init <- gmde_initialize_allocations(data$Y, config$K_fit)
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            dgp = data,
            Z_init = Z_init,
            context_sha256 = context$sha256
        ),
        file.path(output_dir, "road-quick-data.rds")
    )
    chain_dir <- file.path(output_dir, "chains")
    if (!dir.create(chain_dir, showWarnings = FALSE)) {
        stop("Could not create the quick-test chain directory.",
             call. = FALSE)
    }
    diagnostic_dir <- file.path(output_dir, "method-diagnostics")
    failure_dir <- file.path(output_dir, "failures")
    if (!dir.create(diagnostic_dir, showWarnings = FALSE) ||
        !dir.create(failure_dir, showWarnings = FALSE)) {
        stop("Could not create quick-test diagnostic directories.",
             call. = FALSE)
    }
    task_seed <- stats::setNames(
        config$method_seed_base + 101L * seq_len(nrow(config$quick_tasks)),
        config$quick_tasks$task_id
    )
    worker <- function(index) {
        Sys.setenv(
            OMP_NUM_THREADS = "1",
            OPENBLAS_NUM_THREADS = "1",
            MKL_NUM_THREADS = "1",
            VECLIB_MAXIMUM_THREADS = "1",
            RCPP_PARALLEL_NUM_THREADS = "1"
        )
        task <- config$quick_tasks[index, , drop = FALSE]
        task_id <- task$task_id[[1L]]
        method <- task$method[[1L]]
        potts_beta <- task$potts_beta[[1L]]
        slug <- paste0(
            sprintf("%02d", index), "-",
            gsub("[^A-Za-z0-9]+", "-", task_id)
        )
        fit_path <- file.path(chain_dir, paste0(slug, "-fit.rds"))
        diagnostic_path <- file.path(
            diagnostic_dir, paste0(slug, "-diagnostic.rds")
        )
        failure_path <- file.path(
            failure_dir, paste0(slug, "-failure.rds")
        )
        started <- Sys.time()
        worker_stage <- "fit"
        warning_messages <- character()
        value <- tryCatch({
            fit <- withCallingHandlers(
                countdlm_road_fit_method(
                    method = method,
                    data = data,
                    method_inputs = method_inputs,
                    config = config,
                    seed = task_seed[[task_id]],
                    Z_init = Z_init,
                    potts_beta = if (method == "Potts-MDE") {
                        potts_beta
                    } else NULL
                ),
                warning = function(warning) {
                    warning_messages <<- unique(c(
                        warning_messages, conditionMessage(warning)
                    ))
                    invokeRestart("muffleWarning")
                }
            )
            elapsed <- as.numeric(difftime(
                Sys.time(), started, units = "secs"
            ))
            worker_stage <- "save-fit"
            countdlm_road_atomic_save_rds(
                list(
                    api_version = config$api_version,
                    scientific_role = config$scientific_role,
                    task_id = task_id,
                    method = method,
                    potts_beta = potts_beta,
                    seed = task_seed[[task_id]],
                    warnings = warning_messages,
                    truth_metrics_computed = FALSE,
                    fit = fit
                ),
                fit_path
            )
            worker_stage <- "summarize-diagnostics"
            summarized <- countdlm_road_summarize_fit(
                fit = fit, task_id = task_id, method = method,
                potts_beta = potts_beta,
                burn = config$quick_burn, elapsed = elapsed,
                warnings = warning_messages
            )
            worker_stage <- "save-diagnostics"
            countdlm_road_atomic_save_rds(
                summarized$compact, diagnostic_path
            )
            summarized$summary
        }, error = function(error) {
            elapsed <- as.numeric(difftime(
                Sys.time(), started, units = "secs"
            ))
            failure_summary <- countdlm_road_failure_summary(
                task_id = task_id, method = method,
                potts_beta = potts_beta,
                elapsed = elapsed, config = config,
                error = conditionMessage(error), stage = worker_stage,
                warnings = warning_messages
            )
            failure <- list(
                api_version = config$api_version,
                task_id = task_id,
                method = method,
                potts_beta = potts_beta,
                status = "error",
                elapsed_seconds = elapsed,
                error = conditionMessage(error),
                warnings = warning_messages,
                call = paste(deparse(conditionCall(error)), collapse = " "),
                seed = task_seed[[task_id]],
                stage = worker_stage,
                completed_fit_retained = file.exists(fit_path),
                partial_in_memory_chain_unavailable = !file.exists(fit_path),
                retained = TRUE,
                summary = failure_summary
            )
            countdlm_road_atomic_save_rds(failure, failure_path)
            failure_summary
        })
        value
    }
    cat(
        "Road quick test: n=100, T=168, Kmax=10, exact PG\n",
        "Five internal method families / ", nrow(config$quick_tasks),
        " timed tasks; ", actual_workers,
        " parallel worker(s); rho timing value = ", config$rho_timing,
        "\n", sep = ""
    )
    run_stage <- "parallel-method-fits"
    fit_started <- Sys.time()
    result <- countdlm_road_run_batches(
        config$quick_tasks$task_id, worker, actual_workers,
        poll_seconds = config$progress_poll_seconds
    )
    fit_elapsed <- as.numeric(difftime(
        Sys.time(), fit_started, units = "secs"
    ))
    for (index in seq_along(result)) {
        if (!inherits(result[[index]], "countdlm_road_scheduler_error")) next
        task <- config$quick_tasks[index, , drop = FALSE]
        task_id <- task$task_id[[1L]]
        method <- task$method[[1L]]
        potts_beta <- task$potts_beta[[1L]]
        scheduler_error <- result[[index]]
        failure_summary <- countdlm_road_failure_summary(
            task_id = task_id, method = method, potts_beta = potts_beta,
            elapsed = scheduler_error$elapsed_seconds,
            config = config,
            error = scheduler_error$message,
            stage = "parallel-scheduler"
        )
        failure_path <- file.path(
            failure_dir,
            paste0(
                sprintf("%02d", index), "-",
                gsub("[^A-Za-z0-9]+", "-", task_id),
                "-scheduler-failure.rds"
            )
        )
        countdlm_road_atomic_save_rds(
            list(
                api_version = config$api_version,
                task_id = task_id,
                method = method,
                potts_beta = potts_beta,
                status = "error",
                stage = "parallel-scheduler",
                error = scheduler_error$message,
                elapsed_seconds = scheduler_error$elapsed_seconds,
                retained = TRUE,
                summary = failure_summary
            ),
            failure_path
        )
        result[[index]] <- failure_summary
    }
    summary <- do.call(rbind, result)
    summary <- summary[
        match(config$quick_tasks$task_id, summary$task_id), , drop = FALSE
    ]
    rownames(summary) <- NULL
    failed_tasks <- sum(summary$status != "ok")
    all_tasks_ok <- failed_tasks == 0L
    projection <- countdlm_road_project_runtime(
        summary, config, actual_workers = actual_workers
    )
    summary_path <- file.path(output_dir, "road-quick-runtime.csv")
    projection_path <- file.path(output_dir, "road-full-runtime-projection.csv")
    run_stage <- "derived-runtime-outputs"
    countdlm_road_atomic_write_csv(summary, summary_path)
    countdlm_road_atomic_write_csv(projection$table, projection_path)
    total_elapsed <- as.numeric(difftime(
        Sys.time(), quick_started, units = "secs"
    ))
    result_object <- list(
        api_version = config$api_version,
        config = config,
        registration = registration,
        summary = summary,
        projection = projection,
        fit_wall_seconds = fit_elapsed,
        total_wall_seconds_through_derived_tables = total_elapsed,
        truth_metrics_computed = FALSE,
        all_tasks_ok = all_tasks_ok,
        failed_tasks = failed_tasks,
        clean_commit_timing_evidence = isTRUE(git$clean),
        eligible_for_human_timing_review = all_tasks_ok && isTRUE(git$clean),
        formal_scale_freeze_ready = FALSE,
        external_methods = data.frame(
            method = countdlm_road_external_methods,
            status = "unavailable-not-timed",
            reason = c(
                "exact upstream revision/license and current adapter unresolved",
                "inarmix implementation/adapter unavailable in current environment"
            ),
            stringsAsFactors = FALSE
        )
    )
    result_path <- file.path(output_dir, "road-quick-result.rds")
    countdlm_road_atomic_save_rds(result_object, result_path)
    report <- c(
        "countDLM approved-road-design quick-test report",
        paste("API:", config$api_version),
        paste("Output:", output_dir),
        paste("Git HEAD:", git$head),
        paste("Git clean:", git$clean),
        paste("Context SHA-256:", context$sha256),
        "Design: n=100; T=168; Ktrue=5; Kmax=10; one moderate Poisson DLM",
        paste("Exact quick iterations/burn:", config$quick_iterations,
              "/", config$quick_burn),
        paste("Requested / actual workers:", config$cores, "/", actual_workers),
        paste("Method-fit wall time:",
              countdlm_road_format_duration(fit_elapsed)),
        paste("Quick wall time through derived tables:",
              countdlm_road_format_duration(total_elapsed)),
        paste("Provisional rho grid:",
              paste(config$rho_grid_provisional, collapse = ", ")),
        paste("Conservative timing rho:", config$rho_timing),
        paste("Provisional Potts beta grid:",
              paste(config$potts_beta_grid_provisional, collapse = ", ")),
        paste("Conservative timing multiplier:",
              config$projection_multiplier),
        paste("Projected internal scope:", projection$covered_scope),
        paste("Excluded from projection:", projection$excluded_scope),
        paste("12-hour planning gate:",
              format(projection$gate_hours), "hours"),
        paste("Largest projected internal-method replicate count:",
              projection$recommended_replicates),
        paste("Completed task failures:", failed_tasks),
        paste("Internal budget gate passed:",
              projection$internal_budget_gate_passed),
        paste("Clean-commit timing evidence:", isTRUE(git$clean)),
        "Formal scale freeze ready: FALSE (human review is still required).",
        "Truth-based recovery metrics were intentionally not computed in this timing run.",
        "Potts observed-data likelihood was not replaced by a conditional pseudolikelihood.",
        "Do not compare the unnormalized Potts complete-data diagnostic across beta values.",
        "External methods: PNARM and AR(1) were not timed or replaced by placeholders.",
        "Formal simulation was not launched. Review this report before freezing rho, beta, iterations, chains, and repeats."
    )
    report_path <- file.path(output_dir, "road-quick-report.txt")
    countdlm_road_atomic_write_lines(report, report_path)
    cat("\nPer-method timing summary:\n")
    print(summary[, c(
        "task_id", "method", "potts_beta_timed", "status",
        "elapsed_seconds", "seconds_per_iteration", "state_acceptance",
        "warning_count", "warnings", "failure_stage", "error"
    )], row.names = FALSE)
    cat("\n", paste(report, collapse = "\n"), "\n", sep = "")
    run_stage <- "checksums-and-completion-marker"
    checksum_path <- countdlm_road_write_checksums(output_dir)
    complete_path <- file.path(output_dir, "RUN-COMPLETE.rds")
    countdlm_road_atomic_save_rds(
        list(
            api_version = config$api_version,
            status = if (all_tasks_ok) {
                "complete"
            } else "complete-with-method-failures",
            completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
            all_tasks_ok = all_tasks_ok,
            failed_tasks = failed_tasks,
            truth_metrics_computed = FALSE,
            formal_simulation_launched = FALSE,
            checksums_cover_payload_before_this_marker = basename(checksum_path)
        ),
        complete_path
    )
    run_complete <- TRUE
    invisible(c(
        result_object,
        list(
            summary_file = summary_path,
            projection_file = projection_path,
            result_file = result_path,
            report_file = report_path,
            checksum_file = checksum_path,
            completion_file = complete_path
        )
    ))
}
