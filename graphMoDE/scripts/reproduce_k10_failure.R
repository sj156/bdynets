# Replay only the known K=10 GMDE-W chain and capture the first threshold
# vector that triggers the passive diagnostic's mass audit.

if (!file.exists(file.path(getwd(), "scripts", "load_all.R"))) {
    stop("Start R in the graphMoDE directory before running this script.",
         call. = FALSE)
}
source(file.path("scripts", "check_inputs.R"))

if (!requireNamespace("BayesLogit", quietly = TRUE)) {
    stop(
        "Package 'BayesLogit' is required for the exact Devroye sampler.",
        call. = FALSE
    )
}
if (!identical(as.character(utils::packageVersion("BayesLogit")), "2.1")) {
    warning(
        "The reviewed run used BayesLogit 2.1; exact trace identity may differ.",
        call. = FALSE
    )
}
if (!identical(R.version.string, "R version 4.4.0 (2024-04-24)")) {
    warning(
        "The reviewed run used R 4.4.0; exact trace identity may differ.",
        call. = FALSE
    )
}

graphmode_threshold_state_without_stop <- function(probability, threshold) {
    probability <- as.numeric(probability)
    probability <- pmin(1, pmax(0, probability))
    state <- numeric(threshold + 1L)
    state[[1L]] <- 1
    for (value in probability) {
        previous <- state
        state[[1L]] <- previous[[1L]] * (1 - value)
        if (threshold > 1L) {
            index <- 2:threshold
            state[index] <- previous[index] * (1 - value) +
                previous[index - 1L] * value
        }
        state[[threshold + 1L]] <-
            previous[[threshold + 1L]] +
            previous[[threshold]] * value
    }
    list(
        state = state,
        state_sum = sum(state),
        mass_error = abs(sum(state) - 1),
        state_min = min(state),
        state_max = max(state)
    )
}

graphmode_reproduce_k10_failure <- function() {
    context <- readRDS(file.path(
        graphmode_root, "data", "osm-derived",
        "central_beijing_100_graph_input.rds"
    ))
    blinded <- readRDS(file.path(
        graphmode_root, "data", "debug", "k10_blinded_simulation_input.rds"
    ))
    controls <- readRDS(file.path(
        graphmode_root, "data", "debug", "k10_debug_controls.rds"
    ))

    stopifnot(
        identical(controls$formal_simulation_authorized, FALSE),
        identical(controls$known_failure$task_id,
                  "GMDE-W-mode-A-seed-2")
    )

    task <- controls$tasks[
        controls$tasks$task_id == controls$known_failure$task_id,
        , drop = FALSE
    ]
    stopifnot(nrow(task) == 1L)
    mode_key <- paste(task$method, task$mode_id, sep = "|")
    Z_init <- controls$modes[[mode_key]]
    expected_trace <- unname(
        controls$expected_scientific_trace_sha256[[task$task_id]]
    )

    config <- list(
        K_fit = 10L,
        state_C0 = diag(c(2, 1)),
        state_G = diag(2),
        state_W = diag(c(1e-6, 5e-7)),
        substantive_min = 5L,
        pg_backend = "devroye-exact"
    )
    method_inputs <- countdlm_road_method_inputs(context)

    original_threshold <- gmde_poisson_binomial_threshold
    threshold_call <- 0L
    captured <- NULL
    on.exit(
        assign(
            "gmde_poisson_binomial_threshold",
            original_threshold,
            envir = .GlobalEnv
        ),
        add = TRUE
    )

    instrumented_threshold <- function(probability, threshold) {
        threshold_call <<- threshold_call + 1L
        tryCatch(
            original_threshold(probability, threshold),
            error = function(problem) {
                diagnostic <- graphmode_threshold_state_without_stop(
                    probability, threshold
                )
                captured <<- list(
                    schema_version =
                        "graphMoDE-first-failing-threshold-2026-09-03-v1",
                    task_id = task$task_id[[1L]],
                    method = task$method[[1L]],
                    mode_id = task$mode_id[[1L]],
                    seed = as.integer(task$seed[[1L]]),
                    threshold_call = threshold_call,
                    inferred_iteration =
                        as.integer((threshold_call - 1L) %/% config$K_fit + 1L),
                    inferred_component =
                        as.integer((threshold_call - 1L) %% config$K_fit + 1L),
                    probability = as.numeric(probability),
                    threshold = as.integer(threshold),
                    probability_summary = summary(as.numeric(probability)),
                    probability_sum = sum(probability),
                    probability_min = min(probability),
                    probability_max = max(probability),
                    state = diagnostic$state,
                    state_sum = diagnostic$state_sum,
                    mass_error = diagnostic$mass_error,
                    state_min = diagnostic$state_min,
                    state_max = diagnostic$state_max,
                    rng_state_sha256 = digest::digest(
                        get(".Random.seed", envir = .GlobalEnv),
                        algo = "sha256", serialize = TRUE
                    ),
                    error = conditionMessage(problem)
                )
                stop(problem)
            }
        )
    }
    assign(
        "gmde_poisson_binomial_threshold",
        instrumented_threshold,
        envir = .GlobalEnv
    )

    cat(
        "Replaying GMDE-W-mode-A-seed-2 only.\n",
        "This is a diagnostic replay, not a formal simulation.\n",
        "Expected current behavior: a mass-audit error after several minutes.\n"
    )
    started <- proc.time()[["elapsed"]]
    attempt <- tryCatch(
        list(
            ok = TRUE,
            fit = countdlm_road_fit_method(
                method = "GMDE-W",
                data = list(Y = blinded$Y, Fmat = blinded$Fmat),
                method_inputs = method_inputs,
                config = config,
                seed = as.integer(task$seed[[1L]]),
                Z_init = Z_init,
                theta_init = controls$neutral_theta,
                gamma_init = controls$neutral_gamma,
                n_iter = 3000L,
                burn = 1000L,
                rho = 1,
                store_prediction_state = FALSE,
                store_sampler_terminal_state = TRUE,
                store_allocation_conditionals = TRUE
            )
        ),
        error = function(problem) list(ok = FALSE, problem = problem)
    )
    elapsed <- proc.time()[["elapsed"]] - started

    if (!isTRUE(attempt$ok)) {
        cat(
            "\nReplay stopped after ", round(elapsed, 3), " seconds.\n",
            "Observed error: ", conditionMessage(attempt$problem), "\n",
            sep = ""
        )
        if (is.null(captured)) {
            stop(
                "The run failed outside the instrumented threshold helper.",
                call. = FALSE
            )
        }
        output_dir <- file.path(graphmode_root, "debug-output")
        if (!dir.exists(output_dir) && !dir.create(output_dir)) {
            stop("Could not create debug-output.", call. = FALSE)
        }
        output_file <- file.path(
            output_dir, "k10-first-failing-threshold.rds"
        )
        saveRDS(captured, output_file, version = 3)
        cat(
            "Captured threshold call: ", captured$threshold_call, "\n",
            "Inferred iteration/component: ", captured$inferred_iteration,
            "/", captured$inferred_component, "\n",
            "Mass error: ", format(captured$mass_error, digits = 17), "\n",
            "State min/max: ", format(captured$state_min, digits = 17),
            " / ", format(captured$state_max, digits = 17), "\n",
            "Saved: ", normalizePath(output_file, winslash = "/"), "\n",
            sep = ""
        )
        return(invisible(list(
            status = "known-failure-reproduced",
            elapsed_seconds = elapsed,
            capture_file = output_file,
            captured = captured
        )))
    }

    observed_trace <- countdlm_road_conditional_allocation_trace_hash(
        attempt$fit
    )
    rng_pass <- all(attempt$fit$allocation_conditionals$rng_unchanged)
    trace_pass <- identical(observed_trace, expected_trace)
    cat(
        "\nThe chain completed; the previous error was not reproduced.\n",
        "Scientific trace matches parent: ", trace_pass, "\n",
        "Conditional diagnostic left RNG unchanged: ", rng_pass, "\n",
        sep = ""
    )
    if (!trace_pass || !rng_pass) {
        stop(
            "Completion did not satisfy trace/RNG acceptance criteria.",
            call. = FALSE
        )
    }
    cat(
        "PASS for this one chain only. The full 12-task gate is still required.\n"
    )
    invisible(list(
        status = "single-chain-pass",
        elapsed_seconds = elapsed,
        scientific_trace_sha256 = observed_trace,
        conditional_rng_unchanged = rng_pass
    ))
}

graphmode_reproduction_result <- graphmode_reproduce_k10_failure()
