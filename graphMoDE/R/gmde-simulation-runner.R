# Safe orchestration and checkpoint contracts for current-algorithm simulations.

countdlm_current_scientific_config <- function(cfg) {
    cfg[setdiff(names(cfg), c("output_dir"))]
}

countdlm_current_pg_package_version <- function(cfg) {
    if (identical(cfg$pg_backend, "devroye-exact") &&
        requireNamespace("BayesLogit", quietly = TRUE)) {
        as.character(utils::packageVersion("BayesLogit"))
    } else {
        NA_character_
    }
}

countdlm_current_runtime_metadata <- function() {
    session <- utils::sessionInfo()
    thread_names <- c(
        "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
        "VECLIB_MAXIMUM_THREADS", "BLIS_NUM_THREADS"
    )
    thread_values <- Sys.getenv(thread_names, unset = NA_character_)
    names(thread_values) <- thread_names
    system <- Sys.info()[c("sysname", "release", "machine")]
    list(
        R_version = R.version.string,
        platform = R.version$platform,
        system = unname(system),
        system_fields = names(system),
        BLAS = session$BLAS,
        LAPACK = session$LAPACK,
        thread_environment = thread_values
    )
}

countdlm_current_signature <- function(
    cfg,
    context,
    timing_evidence = NULL,
    runtime = countdlm_current_runtime_metadata()
) {
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("Package 'digest' is required for scientific signatures.",
             call. = FALSE)
    }
    digest::digest(
        list(
            scientific_config = countdlm_current_scientific_config(cfg),
            context_sha256 = context$sha256,
            context_basis_sha256 = digest::digest(
                context$Phi, algo = "sha256", serialize = TRUE
            ),
            selected_id_sha256 = if (
                length(context$selected_id_sha256) == 1L
            ) context$selected_id_sha256 else NA_character_,
            sampler_version = countdlm_gmde_sampler_version,
            benchmark_api_version = countdlm_current_benchmark_api_version,
            checkpoint_schema = countdlm_current_checkpoint_schema,
            pg_package_version = countdlm_current_pg_package_version(cfg),
            runtime = runtime,
            exact_timing_evidence = timing_evidence
        ),
        algo = "sha256",
        serialize = TRUE
    )
}

countdlm_current_git_state <- function(repository_root) {
    repository_root <- normalizePath(
        repository_root, winslash = "/", mustWork = TRUE
    )
    if (!file.exists(file.path(repository_root, ".git"))) {
        stop("repository_root must be the bdynets Git root.", call. = FALSE)
    }
    required_marker <- c(
        "DESCRIPTION", "docs/PROJECT_INVENTORY.md", "R/gmde-helpers.R"
    )
    if (!all(file.exists(file.path(repository_root, required_marker)))) {
        stop("repository_root does not contain the bdynets source markers.",
             call. = FALSE)
    }
    description <- tryCatch(
        read.dcf(file.path(repository_root, "DESCRIPTION")),
        error = function(e) NULL
    )
    if (is.null(description) ||
        !identical(unname(description[1L, "Package"]), "bdynets")) {
        stop("repository_root is not the bdynets package root.", call. = FALSE)
    }
    top <- system2(
        "git", c("-C", repository_root, "rev-parse", "--show-toplevel"),
        stdout = TRUE, stderr = TRUE
    )
    if (length(top) != 1L || !identical(
        normalizePath(top, winslash = "/", mustWork = TRUE), repository_root
    )) {
        stop("repository_root is not the resolved Git top level.",
             call. = FALSE)
    }
    head <- system2(
        "git",
        c("-C", repository_root, "rev-parse", "HEAD"),
        stdout = TRUE,
        stderr = TRUE
    )
    status <- system2(
        "git",
        c("-C", repository_root, "status", "--porcelain"),
        stdout = TRUE,
        stderr = TRUE
    )
    if (length(head) != 1L || !grepl("^[0-9a-f]{40}$", head)) {
        stop("Could not resolve the repository HEAD.", call. = FALSE)
    }
    list(root = repository_root, head = head, clean = !length(status))
}

countdlm_current_registration_path <- function(output_dir) {
    file.path(output_dir, "current-run-registration.rds")
}

countdlm_current_progress_path <- function(output_dir) {
    file.path(output_dir, "current-run-progress.rds")
}

countdlm_current_atomic_replace_rds <- function(object, path) {
    directory <- dirname(path)
    if (!dir.exists(directory)) {
        stop("Checkpoint parent directory does not exist: ", directory,
             call. = FALSE)
    }
    temporary <- tempfile(
        paste0(".", basename(path), ".tmp-"), tmpdir = directory
    )
    on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
    saveRDS(object, temporary, version = 3)
    if (!file.rename(temporary, path)) {
        stop("Could not atomically publish: ", path, call. = FALSE)
    }
    invisible(path)
}

countdlm_current_acquire_lock <- function(output_dir) {
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
    path <- file.path(output_dir, ".countdlm-current-run.lock")
    if (!dir.create(path, showWarnings = FALSE)) {
        owner_path <- file.path(path, "owner.rds")
        owner <- if (file.exists(owner_path)) {
            tryCatch(readRDS(owner_path), error = function(e) NULL)
        } else NULL
        detail <- if (is.list(owner)) {
            paste0(
                " Existing owner: pid=", owner$pid,
                ", host=", owner$host,
                ", started_at=", owner$started_at, "."
            )
        } else " Existing owner metadata is unavailable."
        stop(
            "Another writer lock exists for this output directory.", detail,
            " Never remove it until the writer is confirmed stopped.",
            call. = FALSE
        )
    }
    token <- paste(
        Sys.getpid(),
        format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"),
        basename(tempfile("lock-token-")),
        sep = "-"
    )
    owner <- list(
        token = token,
        pid = Sys.getpid(),
        host = unname(Sys.info()[["nodename"]]),
        started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        output_dir = output_dir
    )
    owner_path <- file.path(path, "owner.rds")
    tryCatch(
        saveRDS(owner, owner_path, version = 3),
        error = function(e) {
            unlink(path, recursive = TRUE)
            stop("Could not publish the output writer lock: ",
                 conditionMessage(e), call. = FALSE)
        }
    )
    list(path = path, token = token, owner = owner)
}

countdlm_current_release_lock <- function(lock) {
    if (!is.list(lock) || !dir.exists(lock$path)) return(invisible(FALSE))
    owner_path <- file.path(lock$path, "owner.rds")
    owner <- if (file.exists(owner_path)) {
        tryCatch(readRDS(owner_path), error = function(e) NULL)
    } else NULL
    if (!is.list(owner) || !identical(owner$token, lock$token)) {
        warning(
            "The output writer lock changed ownership and was not removed.",
            call. = FALSE
        )
        return(invisible(FALSE))
    }
    invisible(unlink(lock$path, recursive = TRUE) == 0L)
}

countdlm_current_save_immutable_rds <- function(object, path, validator) {
    if (file.exists(path)) {
        existing <- tryCatch(readRDS(path), error = function(e) NULL)
        valid_existing <- tryCatch(
            isTRUE(validator(existing)), error = function(e) FALSE
        )
        if (!valid_existing) {
            stop(
                "An incompatible file already occupies immutable path: ",
                path, call. = FALSE
            )
        }
        return(existing)
    }
    directory <- dirname(path)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile(
        paste0(".", basename(path), ".tmp-"), tmpdir = directory
    )
    on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
    saveRDS(object, temporary, version = 3)
    candidate <- tryCatch(readRDS(temporary), error = function(e) NULL)
    valid_candidate <- tryCatch(
        isTRUE(validator(candidate)), error = function(e) FALSE
    )
    if (!valid_candidate) {
        stop("The new immutable checkpoint failed validation.",
             call. = FALSE)
    }
    if (!file.rename(temporary, path)) {
        stop("Could not publish immutable checkpoint: ", path,
             call. = FALSE)
    }
    object
}

countdlm_current_validate_timing_evidence <- function(
    cfg, runtime = countdlm_current_runtime_metadata()
) {
    timing_dir <- normalizePath(
        cfg$exact_timing_output_dir, winslash = "/", mustWork = TRUE
    )
    if (identical(timing_dir, normalizePath(
        cfg$output_dir, winslash = "/", mustWork = TRUE
    ))) {
        stop("Pilot output and exact-timing evidence must use different paths.",
             call. = FALSE)
    }
    timing <- countdlm_current_status(timing_dir)
    registration <- timing$registration
    progress <- timing$progress
    summary <- timing$summary
    timing_cfg <- registration$scientific_config
    required_timing_rho <- max(cfg$rho_control$grid)
    final_session <- progress$session_history[[
        length(progress$session_history)
    ]]
    if (!is.list(timing_cfg) ||
        !identical(timing_cfg$profile, "exact-timing") ||
        !identical(timing_cfg$rho_control$mode, "fixed") ||
        !isTRUE(all.equal(
            timing_cfg$rho_control$selected,
            required_timing_rho,
            tolerance = 0,
            check.attributes = TRUE
        )) ||
        !identical(registration$code_commit, cfg$code_commit) ||
        !isTRUE(registration$git_clean) ||
        !identical(registration$runtime, runtime) ||
        !identical(
            registration$pg_package_version,
            countdlm_current_pg_package_version(cfg)
        ) || length(registration$authorized_task_keys) != 1L ||
        !identical(
            as.character(progress$completed_task_keys),
            as.character(registration$authorized_task_keys)
        ) || dir.exists(file.path(
            timing_dir, ".countdlm-current-run.lock"
        )) || !identical(final_session$status, "completed") ||
        is.null(summary) || nrow(summary) != 1L ||
        !identical(as.character(summary$task_key),
                   as.character(registration$authorized_task_keys)) ||
        !isTRUE(summary$algorithm_exact[[1L]]) ||
        !identical(summary$status[[1L]],
                   "exact_timing_only_noninferential") ||
        !isTRUE(summary$selected_rho_agreement[[1L]]) ||
        !isTRUE(all.equal(
            summary$selected_rho_min[[1L]], required_timing_rho,
            tolerance = 0, check.attributes = TRUE
        )) || !isTRUE(all.equal(
            summary$selected_rho_max[[1L]], required_timing_rho,
            tolerance = 0, check.attributes = TRUE
        )) ||
        !is.finite(summary$pg_seconds[[1L]]) ||
        summary$pg_seconds[[1L]] <= 0 ||
        !is.finite(summary$max_pg_shape[[1L]]) ||
        summary$max_pg_shape[[1L]] < 1) {
        stop(
            "The reviewed exact-timing directory is incomplete, incompatible, ",
            "or not an exact one-task timing result.", call. = FALSE
        )
    }
    list(
        output_dir = timing_dir,
        signature = registration$signature,
        code_commit = registration$code_commit,
        task_key = registration$authorized_task_keys,
        pg_package_version = registration$pg_package_version,
        runtime = registration$runtime,
        timed_rho = required_timing_rho,
        summary_sha256 = progress$summary_sha256,
        selected_rho_values = summary$selected_rho_values[[1L]],
        pg_seconds = summary$pg_seconds[[1L]],
        max_pg_shape = summary$max_pg_shape[[1L]]
    )
}

#' Preflight a current-algorithm benchmark without running it
#'
#' @param cfg Current benchmark configuration.
#' @param repository_root Explicit bdynets Git root.
#' @param resume Whether a matching registered output directory may be resumed.
#' @return Verified context, signature, Git state, and output metadata.
#' @export
countdlm_current_preflight <- function(cfg, repository_root, resume = FALSE) {
    countdlm_current_validate_config(cfg)
    if (identical(cfg$profile, "full")) {
        stop(
            "Full simulation is blocked until the Poisson predictive filter ",
            "and formal rho policy are validated and explicitly approved.",
            call. = FALSE
        )
    }
    git <- countdlm_current_git_state(repository_root)
    if (cfg$profile %in% c("exact-timing", "pilot", "full") &&
        (!identical(cfg$pg_backend, "devroye-exact") ||
         !requireNamespace("BayesLogit", quietly = TRUE))) {
        stop(
            "Exact timing/pilot/full require BayesLogit and ",
            "pg_backend='devroye-exact'.", call. = FALSE
        )
    }
    if (cfg$profile %in% c("exact-timing", "pilot", "full")) {
        if (!is.character(cfg$code_commit) || length(cfg$code_commit) != 1L ||
            !identical(cfg$code_commit, git$head) || !isTRUE(git$clean)) {
            stop(
                "Exact timing/pilot/full require the registered commit to ",
                "equal a clean HEAD.",
                call. = FALSE
            )
        }
    }

    output_dir <- normalizePath(
        cfg$output_dir, winslash = "/", mustWork = TRUE
    )
    repository_root <- git$root
    if (identical(output_dir, repository_root) ||
        startsWith(output_dir, paste0(repository_root, "/"))) {
        stop("Simulation output must remain outside the Git repository.",
             call. = FALSE)
    }
    registration_path <- countdlm_current_registration_path(output_dir)
    entries <- list.files(output_dir, all.files = TRUE, no.. = TRUE)
    context <- countdlm_current_load_context(cfg)
    runtime <- countdlm_current_runtime_metadata()
    timing_evidence <- if (identical(cfg$profile, "pilot")) {
        countdlm_current_validate_timing_evidence(cfg, runtime = runtime)
    } else NULL
    signature <- countdlm_current_signature(
        cfg, context, timing_evidence = timing_evidence, runtime = runtime
    )
    pg_package_version <- countdlm_current_pg_package_version(cfg)
    if (!isTRUE(resume)) {
        if (length(entries)) {
            stop("A new current-algorithm run requires an empty directory.",
                 call. = FALSE)
        }
    } else {
        if (!file.exists(registration_path)) {
            stop("Resume requires an existing current-run registration.",
                 call. = FALSE)
        }
        registration <- tryCatch(
            readRDS(registration_path), error = function(e) NULL
        )
        if (!is.list(registration) ||
            !identical(registration$schema, cfg$checkpoint_schema) ||
            !identical(registration$signature, signature) ||
            !identical(registration$code_commit, cfg$code_commit) ||
            !identical(
                registration$pg_package_version, pg_package_version
            ) || !identical(registration$runtime, runtime) ||
            !identical(
                registration$exact_timing_evidence, timing_evidence
            )) {
            stop("The existing output registration is incompatible.",
                 call. = FALSE)
        }
    }
    list(
        context = context,
        signature = signature,
        git = git,
        output_dir = output_dir,
        registration_path = registration_path,
        pg_package_version = pg_package_version,
        runtime = runtime,
        exact_timing_evidence = timing_evidence,
        note = if (isTRUE(cfg$inferential)) {
            "formal profile (currently blocked before this point)"
        } else if (identical(cfg$profile, "exact-timing")) {
            "tiny exact-kernel timing only; non-inferential"
        } else if (identical(cfg$pg_backend, "devroye-exact")) {
            "exact training-kernel pilot; prediction remains blocked"
        } else {
            "approximate interface smoke; non-inferential"
        }
    )
}

countdlm_current_task_path <- function(cfg, task, chain) {
    output_root <- normalizePath(
        cfg$output_dir, winslash = "/", mustWork = TRUE
    )
    path <- normalizePath(file.path(
        cfg$output_dir,
        cfg$profile,
        cfg$stage,
        paste0("DGP-", task$dgp),
        task$label_regime,
        task$track,
        sprintf("replicate-%03d", as.integer(task$replicate)),
        task$method,
        sprintf("chain-%03d.rds", as.integer(chain))
    ), winslash = "/", mustWork = FALSE)
    if (!startsWith(path, paste0(output_root, "/"))) {
        stop("Resolved chain checkpoint escapes output_dir.", call. = FALSE)
    }
    path
}

countdlm_current_prepare_task <- function(task, context, bank, cfg) {
    permutation_index <- (
        as.integer(task$replicate) - 1L
    ) %% cfg$permutations + 1L
    Z <- if (identical(task$label_regime, "radial")) {
        bank$radial
    } else bank$balanced_permuted[[permutation_index]]
    data_seed <- countdlm_current_seed(
        cfg, task$dgp, task$label_regime,
        track = "discovery", replicate = task$replicate,
        chain = 0L, method = "data"
    )
    data <- countdlm_current_generate(
        task$dgp, Z, context, cfg, data_seed
    )
    list(data = data, data_seed = data_seed)
}

#' Fit one registered current-algorithm chain
#'
#' @param method Supported method name.
#' @param data Generated full data object; this function slices training times.
#' @param context Verified graph context.
#' @param cfg Current benchmark configuration.
#' @param track `"fixed"` or `"discovery"`.
#' @param chain_seed,init_seed Independent registered seeds.
#' @return A current GMDE or MoDE fit.
#' @export
countdlm_fit_current_chain <- function(
    method,
    data,
    context,
    cfg,
    track,
    chain_seed,
    init_seed
) {
    method <- match.arg(method, cfg$methods)
    track <- match.arg(track, cfg$track)
    if (!is.list(data) || !identical(dim(data$Y), c(cfg$n, cfg$TT)) ||
        !identical(dim(data$Fmat), c(cfg$TT, 2L))) {
        stop("Generated data do not match the registered n/T design.",
             call. = FALSE)
    }
    training_index <- seq_len(cfg$train_T)
    Y_train <- data$Y[, training_index, drop = FALSE]
    F_train <- data$Fmat[training_index, , drop = FALSE]
    if (!identical(dim(Y_train), c(cfg$n, cfg$train_T)) ||
        !identical(nrow(F_train), cfg$train_T)) {
        stop("Training extraction failed; holdout must not enter MCMC.",
             call. = FALSE)
    }
    K <- if (identical(track, "fixed")) cfg$K_fixed else cfg$K_discovery
    set.seed(as.integer(init_seed))
    Z_init <- gmde_initialize_allocations(Y_train, K)
    common <- list(
        Y = Y_train,
        Fmat = F_train,
        K = K,
        n_iter = cfg$n_iter,
        burn = cfg$burn,
        m0 = c(log(mean(Y_train) + cfg$m0_offset), 0),
        C0 = cfg$fit_C0,
        G = cfg$fit_G,
        W = cfg$fit_W,
        rho = cfg$rho,
        rho_grid = cfg$rho_grid,
        rho_warmup = cfg$rho_warmup,
        rho_tie_break = cfg$rho_tie_break,
        rho_schedule = cfg$rho_schedule,
        Z_true = data$Z,
        Z_init = Z_init,
        substantive_min = cfg$substantive_min,
        pg_backend = cfg$pg_backend,
        pg_trunc = if (is.na(cfg$pg_trunc)) 80L else cfg$pg_trunc,
        print_freq = 0L,
        seed = chain_seed
    )
    fit <- if (identical(method, "GMDE-weighted")) {
        do.call(
            run_gmde_mcmc,
            c(
                common,
                list(
                    Phi = context$Phi,
                    graph_meta = list(
                        source = context$source,
                        sha256 = context$sha256,
                        n = cfg$n,
                        m = cfg$basis_m,
                        graph = "weighted"
                    )
                )
            )
        )
    } else {
        alpha <- if (identical(track, "fixed")) {
            rep(1, K)
        } else rep(1 / K, K)
        do.call(run_nograph_mcmc, c(common, list(alpha_pi = alpha)))
    }
    fit$settings$training_index <- training_index
    fit$settings$holdout_index <- seq.int(cfg$train_T + 1L, cfg$TT)
    fit$settings$init_seed <- as.integer(init_seed)
    fit$settings$chain_seed <- as.integer(chain_seed)
    fit$settings$prediction_status <- cfg$rolling_prediction_status
    fit
}

#' Validate the current fit/checkpoint contract
#'
#' @param fit Candidate fit.
#' @param cfg Current benchmark configuration.
#' @param method Method name.
#' @param K Registered fitted expert count.
#' @param expected Optional immutable chain metadata used during checkpoint
#'   validation.
#' @return `TRUE` invisibly, otherwise an error.
#' @export
countdlm_current_fit_contract <- function(
    fit, cfg, method, K, expected = NULL
) {
    method <- match.arg(method, cfg$methods)
    K <- gmde_scalar_integer(K, "K", lower = 2L, upper = cfg$n)
    required <- c(
        "Z", "theta_terminal", "observed_loglik", "state_accepted",
        "state_log_acceptance", "state_movement", "state_update_seconds",
        "state_pg_seconds",
        "state_pg_shape_sum", "state_pg_shape_max", "state_rho",
        "occupied_experts", "substantive_experts",
        "postburn_state_acceptance_rate", "ess_bracket_evaluations",
        "algorithm_exact", "settings"
    )
    missing <- setdiff(required, names(fit))
    if (length(missing)) {
        stop("Current fit is missing: ", paste(missing, collapse = ", "),
             call. = FALSE)
    }
    if (!is.list(fit$settings) ||
        !identical(dim(fit$Z), c(cfg$n_iter, cfg$n)) ||
        !is.numeric(fit$Z) || any(!is.finite(fit$Z)) ||
        any(fit$Z != round(fit$Z)) || any(fit$Z < 1L) || any(fit$Z > K) ||
        !identical(length(fit$observed_loglik), cfg$n_iter) ||
        any(!is.finite(fit$observed_loglik)) ||
        !identical(length(fit$state_rho), cfg$n_iter) ||
        any(!is.finite(fit$state_rho)) || any(fit$state_rho <= 0) ||
        !identical(
            dim(fit$theta_terminal),
            c(cfg$n_iter - cfg$burn, K, 2L)
        ) || any(!is.finite(fit$theta_terminal))) {
        stop("Current fit trace dimensions are incompatible.",
             call. = FALSE)
    }
    if (length(fit$occupied_experts) != cfg$n_iter ||
        !is.numeric(fit$occupied_experts) ||
        any(!is.finite(fit$occupied_experts)) ||
        any(fit$occupied_experts != round(fit$occupied_experts)) ||
        any(fit$occupied_experts < 1L) || any(fit$occupied_experts > K) ||
        length(fit$substantive_experts) != cfg$n_iter ||
        !is.numeric(fit$substantive_experts) ||
        any(!is.finite(fit$substantive_experts)) ||
        any(fit$substantive_experts != round(fit$substantive_experts)) ||
        any(fit$substantive_experts < 0L) ||
        any(fit$substantive_experts > K) ||
        length(fit$postburn_state_acceptance_rate) != 1L ||
        !is.numeric(fit$postburn_state_acceptance_rate) ||
        !is.finite(fit$postburn_state_acceptance_rate) ||
        fit$postburn_state_acceptance_rate < 0 ||
        fit$postburn_state_acceptance_rate > 1) {
        stop("Current fit expert-count or acceptance summaries are invalid.",
             call. = FALSE)
    }
    diagnostic_names <- c(
        "state_accepted", "state_log_acceptance", "state_movement",
        "state_update_seconds", "state_pg_seconds", "state_pg_shape_sum",
        "state_pg_shape_max"
    )
    for (name in diagnostic_names) {
        value <- fit[[name]]
        valid_value <- if (identical(name, "state_log_acceptance")) {
            (!is.na(value) & value != Inf) |
                (is.na(value) & !is.nan(value))
        } else {
            is.finite(value) | is.na(value)
        }
        if (!identical(dim(value), c(cfg$n_iter, K)) ||
            !all(valid_value)) {
            stop("Current fit has invalid ", name, " dimensions.",
                 call. = FALSE)
        }
    }
    if (any(fit$state_movement < 0, na.rm = TRUE) ||
        any(fit$state_update_seconds <= 0, na.rm = TRUE) ||
        any(fit$state_pg_seconds < 0, na.rm = TRUE) ||
        any(fit$state_pg_shape_sum <= 0, na.rm = TRUE) ||
        any(fit$state_pg_shape_max <= 0, na.rm = TRUE) ||
        !is.logical(fit$state_accepted) ||
        (identical(cfg$pg_backend, "devroye-exact") &&
         (any(
             fit$state_pg_shape_sum != round(fit$state_pg_shape_sum),
             na.rm = TRUE
         ) || any(
             fit$state_pg_shape_max != round(fit$state_pg_shape_max),
             na.rm = TRUE
         )))) {
        stop("Current state diagnostics contain invalid values.",
             call. = FALSE)
    }
    occupied_from_Z <- apply(fit$Z, 1L, function(z) {
        sum(tabulate(as.integer(z), nbins = K) > 0L)
    })
    substantive_from_Z <- apply(fit$Z, 1L, function(z) {
        sum(tabulate(as.integer(z), nbins = K) >= cfg$substantive_min)
    })
    postburn_state <- fit$state_accepted[
        seq.int(cfg$burn + 1L, cfg$n_iter), , drop = FALSE
    ]
    valid_postburn_state <- !is.na(postburn_state)
    acceptance_from_trace <- if (any(valid_postburn_state)) {
        mean(postburn_state[valid_postburn_state])
    } else NA_real_
    if (!identical(
        as.integer(fit$occupied_experts), as.integer(occupied_from_Z)
    ) || !identical(
        as.integer(fit$substantive_experts),
        as.integer(substantive_from_Z)
    ) || !isTRUE(all.equal(
        fit$postburn_state_acceptance_rate,
        acceptance_from_trace,
        tolerance = 0,
        check.attributes = TRUE
    ))) {
        stop("Current fit summaries do not match their stored traces.",
             call. = FALSE)
    }
    same <- function(x, y) isTRUE(all.equal(
        x, y, tolerance = 0, check.attributes = TRUE
    ))
    if (!identical(
        fit$settings$sampler_version, cfg$sampler_version
    ) || !identical(fit$settings$n, cfg$n) ||
        !identical(fit$settings$TT, cfg$train_T) ||
        !identical(fit$settings$K, K) ||
        !identical(fit$settings$n_iter, cfg$n_iter) ||
        !identical(fit$settings$burn, cfg$burn) ||
        !identical(
            fit$settings$substantive_cluster_size,
            cfg$substantive_min
        ) ||
        !identical(fit$settings$pg_backend, cfg$pg_backend) ||
        !same(fit$settings$C0, cfg$fit_C0) ||
        !same(fit$settings$G, cfg$fit_G) ||
        !same(fit$settings$W, cfg$fit_W) ||
        !identical(
            fit$settings$pg_package_version,
            countdlm_current_pg_package_version(cfg)
        ) ||
        !identical(
            isTRUE(fit$algorithm_exact),
            identical(cfg$pg_backend, "devroye-exact")
        )) {
        stop("Current fit settings do not match the registered configuration.",
             call. = FALSE)
    }
    rho_control <- fit$settings$rho_control
    rho_fields <- c(
        "mode", "grid", "warmup", "tie_break", "schedule_rule", "schedule"
    )
    if (!is.list(rho_control) ||
        !all(rho_fields %in% names(rho_control)) ||
        any(!vapply(rho_fields, function(name) {
            same(rho_control[[name]], cfg$rho_control[[name]])
        }, logical(1))) ||
        length(rho_control$selected) != 1L ||
        !is.finite(rho_control$selected) || rho_control$selected <= 0 ||
        !same(fit$settings$rho, rho_control$selected)) {
        stop("Current fit rho calibration does not match the registration.",
             call. = FALSE)
    }
    expected_rho_trace <- if (rho_control$warmup > 0L) {
        c(
            rho_control$schedule,
            rep(rho_control$selected, cfg$n_iter - rho_control$warmup)
        )
    } else rep(rho_control$selected, cfg$n_iter)
    if (!same(fit$state_rho, expected_rho_trace)) {
        stop("Current fit rho trace does not match its frozen control.",
             call. = FALSE)
    }
    calibration <- fit$settings$rho_calibration
    calibration_fields <- c(
        "rho", "accepted_movement", "elapsed_seconds", "proposals",
        "accepts", "efficiency"
    )
    if (!is.data.frame(calibration) ||
        !all(calibration_fields %in% names(calibration)) ||
        nrow(calibration) != length(cfg$rho_control$grid) ||
        !same(as.numeric(calibration$rho), cfg$rho_control$grid)) {
        stop("Current fit rho calibration table is incompatible.",
             call. = FALSE)
    }
    if (identical(cfg$rho_control$mode, "fixed")) {
        if (!same(rho_control$selected, cfg$rho_control$selected) ||
            !all(is.na(unlist(
            calibration[setdiff(calibration_fields, "rho")],
            use.names = FALSE
        )))) {
            stop("Fixed-rho calibration diagnostics must be unavailable.",
                 call. = FALSE)
        }
    } else {
        expected_efficiency <- calibration$accepted_movement /
            calibration$elapsed_seconds
        selected_from_table <- tryCatch(
            gmde_select_calibrated_rho(
                cfg$rho_control$grid,
                expected_efficiency,
                cfg$rho_control$tie_break
            ),
            error = function(e) NA_real_
        )
        if (any(!is.finite(calibration$accepted_movement)) ||
            any(calibration$accepted_movement < 0) ||
            any(!is.finite(calibration$elapsed_seconds)) ||
            any(calibration$elapsed_seconds <= 0) ||
            any(!is.finite(calibration$proposals)) ||
            any(calibration$proposals <= 0) ||
            any(calibration$proposals != round(calibration$proposals)) ||
            any(!is.finite(calibration$accepts)) ||
            any(calibration$accepts < 0) ||
            any(calibration$accepts != round(calibration$accepts)) ||
            any(calibration$accepts > calibration$proposals) ||
            any(!is.finite(calibration$efficiency)) ||
            !same(calibration$efficiency, expected_efficiency) ||
            !is.finite(selected_from_table) ||
            !same(rho_control$selected, selected_from_table)) {
            stop("Current fit rho calibration diagnostics are invalid.",
                 call. = FALSE)
        }
    }
    if (!identical(
        as.integer(fit$settings$training_index), seq_len(cfg$train_T)
    ) || !identical(
        as.integer(fit$settings$holdout_index),
        seq.int(cfg$train_T + 1L, cfg$TT)
    ) || !identical(
        fit$settings$prediction_status, cfg$rolling_prediction_status
    )) {
        stop("Current fit training/holdout metadata are incompatible.",
             call. = FALSE)
    }
    if (!is.null(expected)) {
        if (!same(fit$settings$m0, expected$m0) ||
            !identical(
                as.integer(fit$settings$init_seed), expected$init_seed
            ) || !identical(
                as.integer(fit$settings$chain_seed), expected$chain_seed
            )) {
            stop("Current fit seed or initial-state metadata are incompatible.",
                 call. = FALSE)
        }
    }
    if (identical(method, "GMDE-weighted")) {
        if (length(fit$ess_bracket_evaluations) != cfg$n_iter ||
            !is.numeric(fit$ess_bracket_evaluations) ||
            any(!is.finite(fit$ess_bracket_evaluations)) ||
            any(fit$ess_bracket_evaluations < 1L) ||
            any(fit$ess_bracket_evaluations !=
                    round(fit$ess_bracket_evaluations)) ||
            is.null(fit$settings$graph_meta) ||
            !identical(fit$settings$graph_meta$m, cfg$basis_m) ||
            (!is.null(expected) && !identical(
                fit$settings$graph_meta$sha256, expected$context_sha256
            ))) {
            stop("GMDE fit lacks ESS or weighted-graph provenance.",
                 call. = FALSE)
        }
    } else if (!is.null(expected)) {
        expected_alpha <- if (identical(expected$track, "fixed")) {
            rep(1, K)
        } else rep(1 / K, K)
        if (!same(fit$settings$alpha_pi, expected_alpha)) {
            stop("MoDE alpha_pi does not match its registered track.",
                 call. = FALSE)
        }
    }
    if (cfg$profile %in% c("pilot", "full") &&
        !isTRUE(fit$algorithm_exact)) {
        stop("A pilot/full fit was not produced by the exact PG backend.",
             call. = FALSE)
    }
    invisible(TRUE)
}

countdlm_current_chain_record_valid <- function(record, expected, cfg, K) {
    if (!is.list(record) ||
        !all(c(names(expected), "status", "fit", "error", "created_at",
               "wall_seconds") %in% names(record)) ||
        !all(vapply(names(expected), function(name) {
            identical(record[[name]], expected[[name]])
        }, logical(1))) ||
        !is.character(record$status) || length(record$status) != 1L ||
        !record$status %in% c("ok", "error") ||
        !is.character(record$created_at) || length(record$created_at) != 1L ||
        !is.finite(record$wall_seconds) || record$wall_seconds < 0) {
        return(FALSE)
    }
    if (identical(record$status, "error")) {
        return(is.null(record$fit) && is.character(record$error) &&
                   length(record$error) == 1L && nzchar(record$error))
    }
    if (!is.null(record$error) || is.null(record$fit)) return(FALSE)
    tryCatch({
        countdlm_current_fit_contract(
            record$fit, cfg, expected$method, K, expected = expected
        )
        TRUE
    }, error = function(e) FALSE)
}

countdlm_current_chain_expected <- function(
    task, chain, prepared, context, cfg, signature
) {
    K <- if (identical(task$track, "fixed")) {
        cfg$K_fixed
    } else cfg$K_discovery
    init_seed <- countdlm_current_seed(
        cfg, task$dgp, task$label_regime, task$track,
        task$replicate, chain, "initial"
    )
    chain_seed <- countdlm_current_seed(
        cfg, task$dgp, task$label_regime, task$track,
        task$replicate, chain, task$method
    )
    list(
        K = K,
        init_seed = init_seed,
        chain_seed = chain_seed,
        expected = list(
            schema = cfg$checkpoint_schema,
            signature = signature,
            sampler_version = cfg$sampler_version,
            benchmark_api_version = cfg$benchmark_api_version,
            task_key = task$task_key,
            method = task$method,
            track = task$track,
            chain = as.integer(chain),
            K = as.integer(K),
            n = as.integer(cfg$n),
            train_T = as.integer(cfg$train_T),
            n_iter = as.integer(cfg$n_iter),
            burn = as.integer(cfg$burn),
            pg_backend = cfg$pg_backend,
            pg_package_version = countdlm_current_pg_package_version(cfg),
            context_sha256 = context$sha256,
            context_basis_sha256 = digest::digest(
                context$Phi, algo = "sha256", serialize = TRUE
            ),
            selected_id_sha256 = if (
                length(context$selected_id_sha256) == 1L
            ) context$selected_id_sha256 else NA_character_,
            data_seed = as.integer(prepared$data_seed),
            init_seed = as.integer(init_seed),
            chain_seed = as.integer(chain_seed),
            m0 = c(
                log(mean(
                    prepared$data$Y[
                        , seq_len(cfg$train_T), drop = FALSE
                    ]
                ) + cfg$m0_offset),
                0
            )
        )
    )
}

countdlm_current_run_chain <- function(
    task,
    chain,
    prepared,
    context,
    cfg,
    signature,
    resume
) {
    contract <- countdlm_current_chain_expected(
        task, chain, prepared, context, cfg, signature
    )
    K <- contract$K
    init_seed <- contract$init_seed
    chain_seed <- contract$chain_seed
    expected <- contract$expected
    path <- countdlm_current_task_path(cfg, task, chain)
    if (file.exists(path)) {
        existing <- tryCatch(readRDS(path), error = function(e) NULL)
        if (!isTRUE(resume) ||
            !countdlm_current_chain_record_valid(existing, expected, cfg, K)) {
            stop("Existing chain checkpoint is incompatible: ", path,
                 call. = FALSE)
        }
        return(existing)
    }

    started <- proc.time()[[3L]]
    error_message <- NULL
    fit <- tryCatch({
        candidate <- countdlm_fit_current_chain(
            task$method, prepared$data, context, cfg, task$track,
            chain_seed, init_seed
        )
        countdlm_current_fit_contract(
            candidate, cfg, task$method, K, expected = expected
        )
        candidate
    },
        error = function(e) {
            error_message <<- conditionMessage(e)
            NULL
        }
    )
    record <- c(
        expected,
        list(
            status = if (is.null(fit)) "error" else "ok",
            fit = fit,
            error = error_message,
            created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
            wall_seconds = max(
                proc.time()[[3L]] - started, .Machine$double.eps
            )
        )
    )
    countdlm_current_save_immutable_rds(
        record, path,
        function(x) countdlm_current_chain_record_valid(
            x, expected, cfg, K
        )
    )
}

countdlm_current_basic_rhat <- function(draws) {
    draws <- as.matrix(draws)
    n <- nrow(draws)
    if (n < 2L || ncol(draws) < 2L || any(!is.finite(draws))) {
        return(NA_real_)
    }
    within <- mean(apply(draws, 2L, stats::var))
    if (!is.finite(within) || within == 0) {
        return(NA_real_)
    }
    between <- n * stats::var(colMeans(draws))
    variance_plus <- (n - 1) / n * within + between / n
    sqrt(variance_plus / within)
}

countdlm_current_rank_split_rhat <- function(chains) {
    if (!is.list(chains) || length(chains) < 2L) return(NA_real_)
    chains <- lapply(chains, as.numeric)
    lengths <- vapply(chains, length, integer(1))
    if (length(unique(lengths)) != 1L || lengths[1L] < 4L ||
        any(!is.finite(unlist(chains, use.names = FALSE)))) {
        return(NA_real_)
    }
    half <- floor(lengths[1L] / 2L)
    split <- do.call(cbind, lapply(chains, function(x) {
        cbind(x[seq_len(half)], x[length(x) - half + seq_len(half)])
    }))
    rank_normalize <- function(value) {
        pooled_rank <- rank(as.numeric(value), ties.method = "average")
        probability <- (pooled_rank - 3 / 8) /
            (length(pooled_rank) + 1 / 4)
        matrix(
            stats::qnorm(probability),
            nrow = nrow(value), ncol = ncol(value)
        )
    }
    normalized <- rank_normalize(split)
    folded <- rank_normalize(abs(split - stats::median(split)))
    max(
        countdlm_current_basic_rhat(normalized),
        countdlm_current_basic_rhat(folded)
    )
}

countdlm_current_integer_mode <- function(x) {
    x <- as.integer(x)
    values <- sort(unique(x))
    values[which.max(tabulate(match(x, values), nbins = length(values)))]
}

countdlm_current_summarize_task <- function(
    task,
    records,
    prepared,
    cfg
) {
    errors <- vapply(records, function(x) identical(x$status, "error"),
                     logical(1))
    if (any(errors)) {
        return(data.frame(
            task_key = task$task_key,
            method = task$method,
            track = task$track,
            dgp = task$dgp,
            label_regime = task$label_regime,
            replicate = task$replicate,
            status = "error",
            algorithm_exact = NA,
            ARI = NA_real_, accuracy = NA_real_,
            loglik_rank_split_rhat = NA_real_,
            Kocc_rank_split_rhat = NA_real_,
            Ksub_rank_split_rhat = NA_real_,
            Kocc_mode_agreement = NA,
            Ksub_mode_agreement = NA,
            loglik_gate_pass = NA,
            K_reference_pass = NA,
            selected_rho_values = NA_character_,
            selected_rho_agreement = NA,
            selected_rho_min = NA_real_,
            selected_rho_max = NA_real_,
            rho_calibration_efficiency = NA_character_,
            mean_state_acceptance = NA_real_,
            max_pg_shape = NA_real_, pg_seconds = NA_real_,
            mean_ess_bracket = NA_real_,
            mean_observed_loglik = NA_real_,
            prediction_status = cfg$rolling_prediction_status,
            error = paste(
                vapply(records[errors], `[[`, character(1), "error"),
                collapse = " | "
            ),
            stringsAsFactors = FALSE
        ))
    }
    fits <- lapply(records, `[[`, "fit")
    postburn <- seq.int(cfg$burn + 1L, cfg$n_iter)
    loglik_chains <- lapply(fits, function(fit) {
        fit$observed_loglik[postburn]
    })
    Kocc_chains <- lapply(fits, function(fit) {
        fit$occupied_experts[postburn]
    })
    Ksub_chains <- lapply(fits, function(fit) {
        fit$substantive_experts[postburn]
    })
    loglik_rhat <- countdlm_current_rank_split_rhat(loglik_chains)
    Kocc_rhat <- countdlm_current_rank_split_rhat(Kocc_chains)
    Ksub_rhat <- countdlm_current_rank_split_rhat(Ksub_chains)
    Kocc_modes <- vapply(
        Kocc_chains, countdlm_current_integer_mode, integer(1)
    )
    Ksub_modes <- vapply(
        Ksub_chains, countdlm_current_integer_mode, integer(1)
    )
    selected_rho <- vapply(
        fits, function(fit) as.numeric(fit$settings$rho), numeric(1)
    )
    format_number <- function(x) {
        ifelse(
            is.na(x), "NA",
            format(x, digits = 17L, scientific = TRUE, trim = TRUE)
        )
    }
    calibration_text <- paste(vapply(seq_along(fits), function(chain) {
        table <- fits[[chain]]$settings$rho_calibration
        entries <- paste0(
            format_number(table$rho), "=", format_number(table$efficiency)
        )
        paste0(
            sprintf("chain-%03d", chain), ":[", paste(entries, collapse = ";"),
            "]"
        )
    }, character(1)), collapse = " | ")
    loglik_gate <- is.finite(loglik_rhat) &&
        loglik_rhat <= cfg$convergence_rhat_max
    K_reference <- is.finite(Kocc_rhat) && is.finite(Ksub_rhat) &&
        Kocc_rhat <= cfg$convergence_K_rhat_reference &&
        Ksub_rhat <= cfg$convergence_K_rhat_reference
    exact_run <- isTRUE(fits[[1L]]$algorithm_exact)
    diagnostic_required <- cfg$profile %in% c("pilot", "full")
    pool_allowed <- !diagnostic_required || loglik_gate
    representative <- if (pool_allowed) {
        draws <- do.call(rbind, lapply(fits, function(fit) {
            fit$Z[postburn, , drop = FALSE]
        }))
        countdlm_current_representative_partition(draws)
    } else NULL
    K <- if (identical(task$track, "fixed")) {
        cfg$K_fixed
    } else cfg$K_discovery
    data.frame(
        task_key = task$task_key,
        method = task$method,
        track = task$track,
        dgp = task$dgp,
        label_regime = task$label_regime,
        replicate = task$replicate,
        status = if (identical(cfg$profile, "exact-timing")) {
            "exact_timing_only_noninferential"
        } else if (diagnostic_required && !loglik_gate) {
            "diagnostic_failed_retained"
        } else if (exact_run) {
            "training_only_ok_prediction_blocked"
        } else "interface_only_approximate",
        algorithm_exact = exact_run,
        ARI = if (pool_allowed) {
            gmde_adjusted_rand(prepared$data$Z, representative$label)
        } else NA_real_,
        accuracy = if (pool_allowed) {
            gmde_best_label_accuracy(
                prepared$data$Z, representative$label, K
            )
        } else NA_real_,
        loglik_rank_split_rhat = loglik_rhat,
        Kocc_rank_split_rhat = Kocc_rhat,
        Ksub_rank_split_rhat = Ksub_rhat,
        Kocc_mode_agreement = length(unique(Kocc_modes)) == 1L,
        Ksub_mode_agreement = length(unique(Ksub_modes)) == 1L,
        loglik_gate_pass = if (
            diagnostic_required && is.finite(loglik_rhat)
        ) loglik_gate else NA,
        K_reference_pass = if (
            is.finite(Kocc_rhat) && is.finite(Ksub_rhat)
        ) K_reference else NA,
        selected_rho_values = paste(
            format_number(selected_rho), collapse = ";"
        ),
        selected_rho_agreement = length(unique(selected_rho)) == 1L,
        selected_rho_min = min(selected_rho),
        selected_rho_max = max(selected_rho),
        rho_calibration_efficiency = calibration_text,
        mean_state_acceptance = mean(vapply(
            fits, `[[`, numeric(1), "postburn_state_acceptance_rate"
        )),
        max_pg_shape = max(vapply(
            fits,
            function(fit) max(fit$state_pg_shape_max, na.rm = TRUE),
            numeric(1)
        )),
        pg_seconds = sum(vapply(
            fits, function(fit) sum(fit$state_pg_seconds), numeric(1)
        )),
        mean_ess_bracket = if (identical(task$method, "GMDE-weighted")) {
            mean(unlist(lapply(fits, `[[`, "ess_bracket_evaluations")))
        } else NA_real_,
        mean_observed_loglik = mean(unlist(lapply(fits, function(fit) {
            fit$observed_loglik[postburn]
        }))),
        prediction_status = cfg$rolling_prediction_status,
        error = NA_character_,
        stringsAsFactors = FALSE
    )
}

countdlm_current_write_summary <- function(summary, path) {
    directory <- dirname(path)
    temporary <- tempfile(
        paste0(".", basename(path), ".tmp-"), tmpdir = directory
    )
    on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
    utils::write.csv(summary, temporary, row.names = FALSE)
    if (!file.rename(temporary, path)) {
        stop("Could not atomically publish summary: ", path,
             call. = FALSE)
    }
    invisible(path)
}

countdlm_current_file_sha256 <- function(path) {
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("Package 'digest' is required for checkpoint hashes.",
             call. = FALSE)
    }
    digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

countdlm_current_summary_sha256 <- function(summary) {
    if (is.null(summary)) return(NA_character_)
    path <- tempfile("current-summary-hash-", fileext = ".csv")
    on.exit(if (file.exists(path)) unlink(path), add = TRUE)
    utils::write.csv(summary, path, row.names = FALSE)
    countdlm_current_file_sha256(path)
}

countdlm_current_empty_chain_manifest <- function() {
    data.frame(
        task_key = character(0),
        chain = integer(0),
        relative_path = character(0),
        sha256 = character(0),
        bytes = numeric(0),
        stringsAsFactors = FALSE
    )
}

countdlm_current_session_history_valid <- function(
    session_history, authorized_task_keys, allow_empty = FALSE
) {
    if (!is.list(session_history) ||
        (!isTRUE(allow_empty) && !length(session_history))) {
        return(FALSE)
    }
    if (!length(session_history)) return(TRUE)
    valid_status <- c(
        "running", "interrupted_before_resume", "launch_budget_reached",
        "completed"
    )
    valid <- vapply(session_history, function(session) {
        is.list(session) &&
            is.character(session$session_id) &&
            length(session$session_id) == 1L &&
            !is.na(session$session_id) && nzchar(session$session_id) &&
            is.character(session$started_at) &&
            length(session$started_at) == 1L &&
            is.character(session$last_update) &&
            length(session$last_update) == 1L &&
            is.character(session$ended_at) &&
            length(session$ended_at) == 1L &&
            is.character(session$status) && length(session$status) == 1L &&
            session$status %in% valid_status &&
            is.character(session$stopped_reason) &&
            length(session$stopped_reason) == 1L &&
            is.numeric(session$launch_budget_hours) &&
            length(session$launch_budget_hours) == 1L &&
            (is.na(session$launch_budget_hours) ||
             (is.finite(session$launch_budget_hours) &&
              session$launch_budget_hours > 0)) &&
            is.numeric(session$elapsed_seconds) &&
            length(session$elapsed_seconds) == 1L &&
            is.finite(session$elapsed_seconds) &&
            session$elapsed_seconds >= 0 &&
            is.character(session$requested_task_keys) &&
            !anyDuplicated(session$requested_task_keys) &&
            all(session$requested_task_keys %in% authorized_task_keys) &&
            if (identical(session$status, "running")) {
                is.na(session$ended_at)
            } else !is.na(session$ended_at)
    }, logical(1))
    statuses <- vapply(session_history, `[[`, character(1), "status")
    all(valid) && !anyDuplicated(vapply(
        session_history, `[[`, character(1), "session_id"
    )) && sum(statuses == "running") <= 1L &&
        (!any(statuses == "running") ||
         identical(utils::tail(statuses, 1L), "running"))
}

countdlm_current_rebuild_completed <- function(
    task_keys, full_grid, context, bank, cfg, signature
) {
    task_keys <- unique(as.character(task_keys))
    if (!length(task_keys)) {
        return(list(
            summary = NULL,
            chain_manifest = countdlm_current_empty_chain_manifest()
        ))
    }
    order_index <- match(task_keys, full_grid$task_key)
    if (anyNA(order_index)) {
        stop("A completed task key is outside the registered full grid.",
             call. = FALSE)
    }
    task_keys <- full_grid$task_key[sort(order_index)]
    summaries <- vector("list", length(task_keys))
    manifests <- vector("list", length(task_keys) * cfg$chains)
    manifest_index <- 0L
    output_root <- normalizePath(
        cfg$output_dir, winslash = "/", mustWork = TRUE
    )
    for (task_index in seq_along(task_keys)) {
        row <- full_grid[match(task_keys[[task_index]], full_grid$task_key),
                         , drop = FALSE]
        task <- lapply(as.list(row), function(x) x[[1L]])
        prepared <- countdlm_current_prepare_task(task, context, bank, cfg)
        records <- vector("list", cfg$chains)
        for (chain in seq_len(cfg$chains)) {
            path <- countdlm_current_task_path(cfg, task, chain)
            if (!file.exists(path)) {
                stop(
                    "Completed task is missing chain checkpoint: ", path,
                    call. = FALSE
                )
            }
            record <- tryCatch(readRDS(path), error = function(e) NULL)
            contract <- countdlm_current_chain_expected(
                task, chain, prepared, context, cfg, signature
            )
            if (!countdlm_current_chain_record_valid(
                record, contract$expected, cfg, contract$K
            )) {
                stop("Completed chain checkpoint is incompatible: ", path,
                     call. = FALSE)
            }
            records[[chain]] <- record
            manifest_index <- manifest_index + 1L
            info <- file.info(path)
            manifests[[manifest_index]] <- data.frame(
                task_key = task$task_key,
                chain = as.integer(chain),
                relative_path = substring(
                    path, nchar(output_root) + 2L
                ),
                sha256 = countdlm_current_file_sha256(path),
                bytes = unname(as.numeric(info$size)),
                stringsAsFactors = FALSE
            )
        }
        summaries[[task_index]] <- countdlm_current_summarize_task(
            task, records, prepared, cfg
        )
    }
    summary <- do.call(rbind, summaries)
    rownames(summary) <- NULL
    manifest <- do.call(rbind, manifests[seq_len(manifest_index)])
    rownames(manifest) <- NULL
    list(summary = summary, chain_manifest = manifest)
}

countdlm_current_rebuild_partial <- function(
    completed_keys, full_grid, context, bank, cfg, signature
) {
    completed_keys <- as.character(completed_keys)
    manifests <- list()
    partial_keys <- character(0)
    manifest_index <- 0L
    output_root <- normalizePath(
        cfg$output_dir, winslash = "/", mustWork = TRUE
    )
    for (task_index in seq_len(nrow(full_grid))) {
        task <- lapply(
            as.list(full_grid[task_index, , drop = FALSE]),
            function(x) x[[1L]]
        )
        if (task$task_key %in% completed_keys) next
        paths <- vapply(seq_len(cfg$chains), function(chain) {
            countdlm_current_task_path(cfg, task, chain)
        }, character(1))
        present <- file.exists(paths)
        if (!any(present) || all(present)) next
        partial_keys <- c(partial_keys, task$task_key)
        prepared <- countdlm_current_prepare_task(task, context, bank, cfg)
        for (chain in which(present)) {
            record <- tryCatch(readRDS(paths[[chain]]), error = function(e) NULL)
            contract <- countdlm_current_chain_expected(
                task, chain, prepared, context, cfg, signature
            )
            if (!countdlm_current_chain_record_valid(
                record, contract$expected, cfg, contract$K
            )) {
                stop("Partial chain checkpoint is incompatible: ",
                     paths[[chain]], call. = FALSE)
            }
            manifest_index <- manifest_index + 1L
            info <- file.info(paths[[chain]])
            manifests[[manifest_index]] <- data.frame(
                task_key = task$task_key,
                chain = as.integer(chain),
                relative_path = substring(
                    paths[[chain]], nchar(output_root) + 2L
                ),
                sha256 = countdlm_current_file_sha256(paths[[chain]]),
                bytes = unname(as.numeric(info$size)),
                stringsAsFactors = FALSE
            )
        }
    }
    manifest <- if (manifest_index) {
        value <- do.call(rbind, manifests)
        rownames(value) <- NULL
        value
    } else countdlm_current_empty_chain_manifest()
    list(
        partial_task_keys = unique(partial_keys),
        chain_manifest = manifest
    )
}

countdlm_current_complete_task_keys_on_disk <- function(cfg, full_grid) {
    full_grid$task_key[vapply(seq_len(nrow(full_grid)), function(index) {
        task <- lapply(
            as.list(full_grid[index, , drop = FALSE]),
            function(x) x[[1L]]
        )
        all(vapply(seq_len(cfg$chains), function(chain) {
            file.exists(countdlm_current_task_path(cfg, task, chain))
        }, logical(1)))
    }, logical(1))]
}

countdlm_current_merge_summary <- function(previous, current, full_grid) {
    if (is.null(previous) || !nrow(previous)) {
        combined <- current
    } else {
        if (!identical(names(previous), names(current)) ||
            anyDuplicated(previous$task_key)) {
            stop("The existing summary is incompatible with this schema.",
                 call. = FALSE)
        }
        combined <- rbind(
            previous[!previous$task_key %in% current$task_key, , drop = FALSE],
            current
        )
    }
    order_index <- match(combined$task_key, full_grid$task_key)
    if (anyNA(order_index) || anyDuplicated(combined$task_key)) {
        stop("Summary task keys do not match the registered full grid.",
             call. = FALSE)
    }
    combined[order(order_index), , drop = FALSE]
}

#' Run supported current-algorithm simulation tasks
#'
#' This sequential driver never enables PNARM, DGP-P, or rolling prediction.
#' It writes only to a pre-existing external directory registered by the
#' current schema.  Scientific computation begins only when this function is
#' called explicitly.
#'
#' @param cfg Current benchmark configuration.
#' @param repository_root Explicit bdynets Git root.
#' @param resume Resume only a matching registered current run.
#' @param replicate_ids,dgps,labels,tracks,methods Optional task filters.
#' @return Summary, registration, progress, and paths invisibly.
#' @export
countdlm_current_run <- function(
    cfg,
    repository_root,
    resume = FALSE,
    replicate_ids = NULL,
    dgps = NULL,
    labels = NULL,
    tracks = NULL,
    methods = NULL
) {
    preflight <- countdlm_current_preflight(
        cfg, repository_root, resume = resume
    )
    writer_lock <- countdlm_current_acquire_lock(preflight$output_dir)
    on.exit(countdlm_current_release_lock(writer_lock), add = TRUE)
    session_started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    run_started <- proc.time()[[3L]]
    cfg$output_dir <- preflight$output_dir
    full_grid <- countdlm_current_task_grid(cfg)
    selected_grid <- countdlm_current_task_grid(
        cfg, replicate_ids, dgps, labels, tracks, methods
    )
    if (identical(cfg$profile, "exact-timing") &&
        nrow(selected_grid) != 1L) {
        stop(
            "The exact-timing profile must select exactly one task so its ",
            "PG cost can be reviewed before the n=100 pilot.", call. = FALSE
        )
    }
    authorized_task_keys <- if (identical(cfg$profile, "exact-timing")) {
        as.character(selected_grid$task_key)
    } else as.character(full_grid$task_key)
    registration <- list(
        schema = cfg$checkpoint_schema,
        signature = preflight$signature,
        sampler_version = cfg$sampler_version,
        benchmark_api_version = cfg$benchmark_api_version,
        prediction_api_version = cfg$prediction_api_version,
        code_commit = cfg$code_commit,
        git_head = preflight$git$head,
        git_clean = preflight$git$clean,
        scientific_role = cfg$scientific_role,
        context_source = preflight$context$source,
        context_sha256 = preflight$context$sha256,
        context_basis_sha256 = digest::digest(
            preflight$context$Phi, algo = "sha256", serialize = TRUE
        ),
        selected_id_sha256 = if (
            length(preflight$context$selected_id_sha256) == 1L
        ) preflight$context$selected_id_sha256 else NA_character_,
        pg_package_version = preflight$pg_package_version,
        runtime = preflight$runtime,
        exact_timing_evidence = preflight$exact_timing_evidence,
        scientific_config = countdlm_current_scientific_config(cfg),
        full_grid = full_grid,
        authorized_task_keys = authorized_task_keys,
        created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        note = preflight$note
    )
    registration_validator <- function(x) {
        is.list(x) && identical(x$schema, cfg$checkpoint_schema) &&
            identical(x$signature, preflight$signature) &&
            identical(x$code_commit, cfg$code_commit) &&
            identical(x$pg_package_version, preflight$pg_package_version) &&
            identical(x$context_sha256, preflight$context$sha256) &&
            identical(x$runtime, preflight$runtime) &&
            identical(
                x$exact_timing_evidence, preflight$exact_timing_evidence
            ) && identical(
                as.character(x$authorized_task_keys), authorized_task_keys
            ) && identical(x$full_grid, full_grid)
    }
    registration <- countdlm_current_save_immutable_rds(
        registration, preflight$registration_path, registration_validator
    )
    if (!all(selected_grid$task_key %in%
             registration$authorized_task_keys)) {
        stop("This run selection is outside the immutable authorization.",
             call. = FALSE)
    }
    context <- preflight$context
    bank <- countdlm_current_label_bank(context, cfg)
    progress_path <- countdlm_current_progress_path(cfg$output_dir)
    summary_path <- file.path(cfg$output_dir, "current-training-summary.csv")
    progress_read_error <- FALSE
    previous_progress <- if (isTRUE(resume) && file.exists(progress_path)) {
        tryCatch(
            readRDS(progress_path),
            error = function(e) {
                progress_read_error <<- TRUE
                NULL
            }
        )
    } else NULL
    if (isTRUE(progress_read_error)) {
        stop(
            "The existing progress checkpoint cannot be read; immutable ",
            "chain evidence was not re-baselined.", call. = FALSE
        )
    }
    if (!is.null(previous_progress) &&
        (!identical(previous_progress$schema, cfg$checkpoint_schema) ||
         !identical(previous_progress$signature, preflight$signature) ||
         !identical(
             as.character(previous_progress$full_task_keys),
             as.character(full_grid$task_key)
         ) || !identical(
             as.character(previous_progress$authorized_task_keys),
             as.character(registration$authorized_task_keys)
         ) || !is.list(previous_progress$session_history) ||
         !is.character(previous_progress$active_task_key) ||
         length(previous_progress$active_task_key) != 1L)) {
        stop("The existing progress checkpoint is incompatible.",
             call. = FALSE)
    }
    completed_keys <- if (is.null(previous_progress)) {
        character(0)
    } else as.character(previous_progress$completed_task_keys)
    requested_keys <- if (is.null(previous_progress)) {
        character(0)
    } else as.character(previous_progress$requested_task_keys)
    if (!is.null(previous_progress) &&
        (anyDuplicated(completed_keys) || anyDuplicated(requested_keys) ||
         !all(completed_keys %in% registration$authorized_task_keys) ||
         !all(requested_keys %in% registration$authorized_task_keys) ||
         !countdlm_current_session_history_valid(
             previous_progress$session_history,
             registration$authorized_task_keys
         ) || (!is.na(previous_progress$active_task_key) &&
               (!previous_progress$active_task_key %in%
                    registration$authorized_task_keys ||
                !previous_progress$active_task_key %in% requested_keys)))) {
        stop("The existing progress task/session state is incompatible.",
             call. = FALSE)
    }
    declared <- countdlm_current_rebuild_completed(
        completed_keys, full_grid, context, bank, cfg, preflight$signature
    )
    if (!is.null(previous_progress) && !identical(
        previous_progress$completed_chain_manifest,
        declared$chain_manifest
    )) {
        stop(
            "A previously declared completed chain changed or its manifest ",
            "is incompatible; resume cannot re-baseline immutable evidence.",
            call. = FALSE
        )
    }
    declared_summary_sha256 <- countdlm_current_summary_sha256(
        declared$summary
    )
    if (!is.null(previous_progress) && !identical(
        previous_progress$summary_sha256, declared_summary_sha256
    )) {
        stop(
            "The previous summary hash does not match the immutable declared ",
            "chains; resume cannot re-baseline it.", call. = FALSE
        )
    }
    previous_summary <- NULL
    summary_read_error <- FALSE
    if (isTRUE(resume) && file.exists(summary_path)) {
        previous_summary <- tryCatch(
            utils::read.csv(summary_path, stringsAsFactors = FALSE),
            error = function(e) {
                summary_read_error <<- TRUE
                NULL
            }
        )
    }
    if (isTRUE(summary_read_error)) {
        stop("The cumulative summary CSV cannot be read.", call. = FALSE)
    }
    summary_keys <- if (is.null(previous_summary)) {
        character(0)
    } else {
        if (!"task_key" %in% names(previous_summary) ||
            anyDuplicated(previous_summary$task_key)) {
            stop("The cumulative summary CSV has invalid task keys.",
                 call. = FALSE)
        }
        as.character(previous_summary$task_key)
    }
    disk_complete_keys <- if (isTRUE(resume)) {
        countdlm_current_complete_task_keys_on_disk(cfg, full_grid)
    } else character(0)
    undeclared_keys <- setdiff(
        union(summary_keys, disk_complete_keys), completed_keys
    )
    if (length(undeclared_keys) &&
        (is.null(previous_progress) || length(undeclared_keys) != 1L ||
         is.na(previous_progress$active_task_key) ||
         !identical(
             as.character(undeclared_keys),
             as.character(previous_progress$active_task_key)
         ) || !undeclared_keys %in% requested_keys)) {
        stop(
            "Undeclared complete output is not the single active crash-window ",
            "task and cannot be re-certified automatically.", call. = FALSE
        )
    }
    recovery_keys <- union(completed_keys, undeclared_keys)
    if (!all(recovery_keys %in% registration$authorized_task_keys)) {
        stop(
            "Completed output exists outside the immutable task authorization.",
            call. = FALSE
        )
    }
    recovered <- countdlm_current_rebuild_completed(
        recovery_keys, full_grid, context, bank, cfg, preflight$signature
    )
    completed_keys <- if (length(recovery_keys)) {
        full_grid$task_key[full_grid$task_key %in% recovery_keys]
    } else character(0)
    partial <- countdlm_current_rebuild_partial(
        completed_keys, full_grid, context, bank, cfg, preflight$signature
    )
    if (!is.null(previous_progress)) {
        old_partial <- previous_progress$partial_chain_manifest
        old_partial_keys <- previous_progress$partial_task_keys
        empty_manifest <- countdlm_current_empty_chain_manifest()
        if (!is.data.frame(old_partial) ||
            !identical(names(old_partial), names(empty_manifest)) ||
            !is.character(old_partial_keys) ||
            !identical(
                full_grid$task_key[
                    full_grid$task_key %in% unique(old_partial$task_key)
                ],
                old_partial_keys
            )) {
            stop("The previous partial-chain manifest is incompatible.",
                 call. = FALSE)
        }
        existing_manifest <- rbind(
            recovered$chain_manifest, partial$chain_manifest
        )
        manifest_id <- function(x) paste(
            x$task_key, x$chain, x$relative_path, sep = "|"
        )
        old_id <- manifest_id(old_partial)
        existing_id <- manifest_id(existing_manifest)
        old_match <- match(old_id, existing_id)
        if (anyNA(old_match) || !identical(
            old_partial,
            existing_manifest[old_match, , drop = FALSE]
        )) {
            stop(
                "A previously recorded partial chain changed or disappeared.",
                call. = FALSE
            )
        }
        current_partial_id <- manifest_id(partial$chain_manifest)
        extra_partial <- which(!current_partial_id %in% old_id)
        if (length(extra_partial) &&
            (is.na(previous_progress$active_task_key) ||
             any(partial$chain_manifest$task_key[extra_partial] !=
                     previous_progress$active_task_key))) {
            stop(
                "New partial chain evidence is outside the active crash window.",
                call. = FALSE
            )
        }
    } else if (nrow(partial$chain_manifest)) {
        stop(
            "Partial chain evidence exists without a readable progress record.",
            call. = FALSE
        )
    }
    cumulative_summary <- recovered$summary
    chain_manifest <- recovered$chain_manifest
    partial_task_keys <- partial$partial_task_keys
    partial_chain_manifest <- partial$chain_manifest
    if (length(completed_keys)) {
        countdlm_current_write_summary(cumulative_summary, summary_path)
        summary_sha256 <- countdlm_current_file_sha256(summary_path)
    } else {
        if (file.exists(summary_path)) {
            stop("A summary exists without any complete validated task.",
                 call. = FALSE)
        }
        summary_sha256 <- NA_character_
    }
    budget_seconds <- if (identical(cfg$profile, "pilot")) {
        as.numeric(cfg$pilot_session_launch_budget_hours) * 3600
    } else Inf
    session_history <- if (is.null(previous_progress)) {
        list()
    } else previous_progress$session_history
    if (length(session_history)) {
        for (history_index in seq_along(session_history)) {
            if (!is.list(session_history[[history_index]])) {
                stop("The existing session history is incompatible.",
                     call. = FALSE)
            }
            if (identical(session_history[[history_index]]$status, "running")) {
                session_history[[history_index]]$status <-
                    "interrupted_before_resume"
                session_history[[history_index]]$ended_at <- session_started_at
                session_history[[history_index]]$last_update <-
                    session_started_at
            }
        }
    }
    requested_keys <- union(requested_keys, selected_grid$task_key)
    session_history[[length(session_history) + 1L]] <- list(
        session_id = sprintf("session-%03d", length(session_history) + 1L),
        started_at = session_started_at,
        ended_at = NA_character_,
        last_update = session_started_at,
        launch_budget_hours = if (is.finite(budget_seconds)) {
            budget_seconds / 3600
        } else NA_real_,
        requested_task_keys = as.character(selected_grid$task_key),
        status = "running",
        stopped_reason = NA_character_,
        elapsed_seconds = 0
    )
    current_session <- length(session_history)
    publish_progress <- function(
        session_status = "running",
        stopped_reason = NA_character_,
        active_task_key = NA_character_,
        end_session = FALSE
    ) {
        now <- format(Sys.time(), tz = "UTC", usetz = TRUE)
        completed_keys <<- full_grid$task_key[
            full_grid$task_key %in% completed_keys
        ]
        session_history[[current_session]]$status <<- session_status
        session_history[[current_session]]$stopped_reason <<- stopped_reason
        session_history[[current_session]]$last_update <<- now
        session_history[[current_session]]$elapsed_seconds <<-
            max(proc.time()[[3L]] - run_started, 0)
        if (isTRUE(end_session)) {
            session_history[[current_session]]$ended_at <<- now
        }
        progress <- list(
            schema = cfg$checkpoint_schema,
            signature = preflight$signature,
            full_task_keys = full_grid$task_key,
            authorized_task_keys = registration$authorized_task_keys,
            requested_task_keys = requested_keys,
            completed_task_keys = completed_keys,
            completed_tasks = length(completed_keys),
            total_tasks = nrow(full_grid),
            active_task_key = active_task_key,
            stopped_reason = stopped_reason,
            summary_sha256 = summary_sha256,
            completed_chain_manifest = chain_manifest,
            partial_task_keys = partial_task_keys,
            partial_chain_manifest = partial_chain_manifest,
            session_history = session_history,
            updated_at = now
        )
        countdlm_current_atomic_replace_rds(progress, progress_path)
        progress
    }
    publish_progress()

    for (index in seq_len(nrow(selected_grid))) {
        task <- as.list(selected_grid[index, , drop = FALSE])
        task <- lapply(task, function(x) x[[1L]])
        if (task$task_key %in% completed_keys) next
        prepared <- countdlm_current_prepare_task(task, context, bank, cfg)
        records <- vector("list", cfg$chains)
        budget_reached <- FALSE
        publish_progress(active_task_key = task$task_key)
        for (chain in seq_len(cfg$chains)) {
            chain_path <- countdlm_current_task_path(cfg, task, chain)
            elapsed <- proc.time()[[3L]] - run_started
            if (!file.exists(chain_path) && elapsed >= budget_seconds) {
                budget_reached <- TRUE
                break
            }
            records[[chain]] <- countdlm_current_run_chain(
                task, chain, prepared, context, cfg,
                preflight$signature, resume
            )
            if (chain < cfg$chains) {
                partial <- countdlm_current_rebuild_partial(
                    completed_keys, full_grid, context, bank, cfg,
                    preflight$signature
                )
                partial_task_keys <- partial$partial_task_keys
                partial_chain_manifest <- partial$chain_manifest
                publish_progress(active_task_key = task$task_key)
            }
        }
        if (budget_reached) {
            progress <- publish_progress(
                session_status = "launch_budget_reached",
                stopped_reason = paste(
                    "pilot launch budget reached before starting a new chain;",
                    "resume in a new authorized session"
                ),
                active_task_key = task$task_key,
                end_session = TRUE
            )
            return(invisible(list(
                summary = cumulative_summary,
                summary_file = if (file.exists(summary_path)) {
                    summary_path
                } else NA_character_,
                registration = registration,
                progress = progress,
                config = cfg,
                signature = preflight$signature
            )))
        }
        current_derived <- countdlm_current_rebuild_completed(
            task$task_key, full_grid, context, bank, cfg,
            preflight$signature
        )
        current_summary <- current_derived$summary
        cumulative_summary <- countdlm_current_merge_summary(
            cumulative_summary, current_summary, full_grid
        )
        countdlm_current_write_summary(cumulative_summary, summary_path)
        summary_sha256 <- countdlm_current_file_sha256(summary_path)
        completed_keys <- union(completed_keys, task$task_key)
        chain_manifest <- rbind(
            chain_manifest[
                chain_manifest$task_key != task$task_key, , drop = FALSE
            ],
            current_derived$chain_manifest
        )
        manifest_order <- order(
            match(chain_manifest$task_key, full_grid$task_key),
            chain_manifest$chain
        )
        chain_manifest <- chain_manifest[manifest_order, , drop = FALSE]
        rownames(chain_manifest) <- NULL
        partial <- countdlm_current_rebuild_partial(
            completed_keys, full_grid, context, bank, cfg,
            preflight$signature
        )
        partial_task_keys <- partial$partial_task_keys
        partial_chain_manifest <- partial$chain_manifest
        publish_progress()
    }
    summary <- cumulative_summary
    progress <- publish_progress(
        session_status = "completed", end_session = TRUE
    )
    invisible(list(
        summary = summary,
        summary_file = if (file.exists(summary_path)) {
            summary_path
        } else NA_character_,
        registration = registration,
        progress = progress,
        config = cfg,
        signature = preflight$signature
    ))
}

#' Read current-algorithm run progress
#'
#' @param output_dir Registered external current-run directory.
#' @return Registration, progress, and optional summary.
#' @export
countdlm_current_status <- function(output_dir) {
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) {
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else NULL
    on.exit({
        if (had_seed) {
            assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(
            ".Random.seed", envir = .GlobalEnv, inherits = FALSE
        )) {
            rm(".Random.seed", envir = .GlobalEnv)
        }
    }, add = TRUE)
    registration_path <- countdlm_current_registration_path(output_dir)
    progress_path <- countdlm_current_progress_path(output_dir)
    if (!file.exists(registration_path) || !file.exists(progress_path)) {
        stop("No complete current-run registration/progress pair was found.",
             call. = FALSE)
    }
    registration <- tryCatch(
        readRDS(registration_path), error = function(e) NULL
    )
    progress <- tryCatch(readRDS(progress_path), error = function(e) NULL)
    if (!is.list(registration) || !is.list(progress) ||
        !is.list(registration$scientific_config)) {
        stop("Current-run registration or progress cannot be read.",
             call. = FALSE)
    }
    cfg <- tryCatch(
        countdlm_current_rebuild_config(
            registration$scientific_config, output_dir = output_dir
        ),
        error = function(e) {
            stop(
                "Registered scientific configuration is invalid: ",
                conditionMessage(e), call. = FALSE
            )
        }
    )
    countdlm_current_validate_config(cfg)
    full_grid <- countdlm_current_task_grid(cfg)
    context <- countdlm_current_load_context(cfg)
    timing_evidence <- if (identical(cfg$profile, "pilot")) {
        countdlm_current_validate_timing_evidence(
            cfg, runtime = registration$runtime
        )
    } else NULL
    if (!is.list(registration$runtime)) {
        stop("Current-run registration lacks runtime provenance.",
             call. = FALSE)
    }
    expected_signature <- countdlm_current_signature(
        cfg,
        context,
        timing_evidence = timing_evidence,
        runtime = registration$runtime
    )
    expected_authorized <- if (identical(cfg$profile, "exact-timing")) {
        as.character(registration$authorized_task_keys)
    } else as.character(full_grid$task_key)
    if (!identical(registration$schema, countdlm_current_checkpoint_schema) ||
        !identical(registration$sampler_version,
                   countdlm_gmde_sampler_version) ||
        !identical(registration$benchmark_api_version,
                   countdlm_current_benchmark_api_version) ||
        !identical(registration$signature, expected_signature) ||
        !identical(registration$exact_timing_evidence, timing_evidence) ||
        !identical(registration$full_grid, full_grid) ||
        !identical(
            as.character(registration$authorized_task_keys),
            expected_authorized
        ) || (identical(cfg$profile, "exact-timing") &&
              length(expected_authorized) != 1L) ||
        !all(expected_authorized %in% full_grid$task_key) ||
        !identical(progress$schema, countdlm_current_checkpoint_schema) ||
        !identical(registration$signature, progress$signature) ||
        !identical(
            as.character(progress$full_task_keys),
            as.character(full_grid$task_key)
        ) || !identical(
            as.character(progress$authorized_task_keys),
            expected_authorized
        ) || anyDuplicated(progress$completed_task_keys) ||
        anyDuplicated(progress$requested_task_keys) ||
        !all(progress$completed_task_keys %in% expected_authorized) ||
        !all(progress$requested_task_keys %in% expected_authorized) ||
        !is.list(progress$session_history) ||
        !length(progress$session_history) ||
        !is.data.frame(progress$completed_chain_manifest) ||
        !is.character(progress$partial_task_keys) ||
        !is.data.frame(progress$partial_chain_manifest)) {
        stop("Current-run registration and progress are incompatible.",
             call. = FALSE)
    }
    completed_keys <- full_grid$task_key[
        full_grid$task_key %in% as.character(progress$completed_task_keys)
    ]
    if (!identical(
        completed_keys, as.character(progress$completed_task_keys)
    ) || !identical(progress$completed_tasks, length(completed_keys)) ||
        !identical(progress$total_tasks, nrow(full_grid))) {
        stop("Current-run progress counts or ordering are incompatible.",
             call. = FALSE)
    }
    if (!countdlm_current_session_history_valid(
        progress$session_history, expected_authorized
    )) {
        stop("Current-run session history is incompatible.", call. = FALSE)
    }
    disk_complete_keys <- countdlm_current_complete_task_keys_on_disk(
        cfg, full_grid
    )
    if (!setequal(disk_complete_keys, completed_keys)) {
        stop(
            "Complete chain files and declared completed tasks disagree; ",
            "resume the run to reconcile the crash-safe derived state.",
            call. = FALSE
        )
    }
    bank <- countdlm_current_label_bank(context, cfg)
    rebuilt <- countdlm_current_rebuild_completed(
        completed_keys, full_grid, context, bank, cfg,
        registration$signature
    )
    partial <- countdlm_current_rebuild_partial(
        completed_keys, full_grid, context, bank, cfg,
        registration$signature
    )
    if (!identical(
        as.character(progress$partial_task_keys),
        as.character(partial$partial_task_keys)
    ) || !all(progress$partial_task_keys %in%
              progress$requested_task_keys) || !identical(
        progress$partial_chain_manifest, partial$chain_manifest
    )) {
        stop("Partial-chain progress failed integrity validation.",
             call. = FALSE)
    }
    summary_path <- file.path(output_dir, "current-training-summary.csv")
    if (length(completed_keys)) {
        if (!file.exists(summary_path) ||
            !is.character(progress$summary_sha256) ||
            length(progress$summary_sha256) != 1L ||
            !identical(
                countdlm_current_file_sha256(summary_path),
                progress$summary_sha256
            ) || !identical(
                countdlm_current_summary_sha256(rebuilt$summary),
                progress$summary_sha256
            ) || !identical(
                progress$completed_chain_manifest,
                rebuilt$chain_manifest
            )) {
            stop(
                "Derived summary or completed-chain manifest failed ",
                "integrity validation.", call. = FALSE
            )
        }
    } else if (file.exists(summary_path) ||
        !identical(progress$summary_sha256, NA_character_) ||
        !identical(
            progress$completed_chain_manifest,
            countdlm_current_empty_chain_manifest()
        )) {
        stop("Empty progress has incompatible derived output.",
             call. = FALSE)
    }
    list(
        registration = registration,
        progress = progress,
        summary = rebuilt$summary
    )
}
