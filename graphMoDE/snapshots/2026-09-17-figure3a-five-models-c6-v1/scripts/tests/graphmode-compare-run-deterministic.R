# Focused adapter checks: fixed inputs only; no scientific chain or random draw.
local({
    kind <- RNGkind(); had <- exists('.Random.seed', .GlobalEnv, inherits = FALSE)
    seed <- if (had) get('.Random.seed', .GlobalEnv) else NULL
    checks <- 0L
    assert <- function(x) if (!isTRUE(x)) stop('Comparator assertion: ', paste(deparse(substitute(x)), collapse = ' '))
    test <- function(name, code) { force(code); checks <<- checks + 1L; cat('OK', checks, name, '\n') }
    error <- function(code) inherits(tryCatch({ force(code); NULL }, error = identity), 'error')
    spec <- graphmode_compare_spec()
    policy <- list(gate = graphmode_gate_refresh_policy(4L, .4),
        block = graphmode_expert_blocks_policy(2L, 'uniform'), ess_scheme = 'contrast', factor_cache = TRUE)
    record <- list(base_policy = policy, thresholds = graphmode4_pilot_spec()$thresholds, spec = spec)
    configs <- setNames(lapply(spec$methods, function(m) graphmode_config(
        matrix(1:12, 4, 3), cbind(1, c(-1, 0, 1)), c(.2, -.1), diag(2), m,
        K = 3L, G = diag(2), W = diag(.02, 2), Phi = if (m %in% spec$methods[1:2]) diag(4) else NULL,
        guidance_proposal_sd = .4, rho = 4L,
        dirichlet_alpha = if (m == 'MoDE') rep(.1, 3) else NULL,
        graph_weight = if (m == 'PottsMoDE') matrix(1, 4, 4) - diag(4) else NULL,
        potts_beta = if (m == 'PottsMoDE') 1 else NULL)), spec$methods)
    fixed_environment <- function() {
        e <- new.env(parent = .GlobalEnv)
        for (nm in ls(.GlobalEnv, pattern = '^graphmode')) {
            f <- get(nm, .GlobalEnv)
            if (is.function(f)) { environment(f) <- e; assign(nm, f, e) }
        }
        e$graphmode_uniform <- function(n) rep(.2, n)
        e$graphmode_normal <- function(n) rep(0, n)
        e$graphmode_pg <- function(b, z) rep(.4, length(b))
        e$gamma_calls <- list()
        e$graphmode_gamma <- function(shape, rate) {
            e$gamma_calls[[length(e$gamma_calls) + 1L]] <- list(shape = shape, rate = rate)
            shape / rate
        }
        e
    }
    test('same budget, balanced job order, independent worker seeds and fixed development parameters', {
        assert(nrow(spec$jobs) == 16L && !anyDuplicated(spec$jobs$seed) && !anyDuplicated(spec$jobs$name))
        assert(all(table(spec$jobs$method, spec$jobs$chain) == 1L))
        assert(spec$iterations == 600L && spec$warmup == 300L && spec$thin == 1L)
        assert(identical(spec$start_occupancy, c(1L, 3L, 7L, 10L)) && spec$potts_beta == 1)
        assert(spec$science_seconds == 14400 && spec$diagnostic_seconds == 1800)
    })
    for (method in spec$methods) test(paste(method, 'complete outer step and original numerical guards'), {
        config <- configs[[method]]; state <- graphmode_initial_state(config, c(1L, 1L, 2L, 2L))
        e <- fixed_environment(); out <- e$graphmode_compare_step(state, config, policy)
        graphmode_compare_check(state, out, config, record)
        assert(out$state$iteration == 1L && out$base_diagnostic$expert[[3L]]$empty)
        assert(is.null(out$joint) && is.null(out$candidate))
        if (config$adaptive) {
            expected <- fixed_environment()$graphmode_joint_run_step(state, config, policy, NULL, FALSE, 10L)
            assert(identical(out$state, expected$state) && identical(out$events, expected$events))
        } else {
            # Compare against the original allocation driver with fixed updated experts.
            ref <- fixed_environment(); k <- 0L
            ref$graphmode_update_expert <- function(...) {
                k <<- k + 1L
                list(theta = matrix(out$state$theta[k, , ], 3, 2), sigma2 = NULL)
            }
            expected <- ref$graphmode_sweep(state, config)
            assert(identical(out$state, expected$state) && identical(out$events, expected$events))
            assert(identical(out$base_diagnostic$potts_gross, expected$potts_gross))
        }
        if (method == 'MoDE') {
            assert(length(e$gamma_calls) == 1L)
            assert(identical(e$gamma_calls[[1L]]$shape, config$dirichlet_alpha + c(2L, 2L, 0L)))
        }
    })
    test('nonadaptive stale states and gross-event corruption are rejected', {
        for (method in spec$methods[3:4]) {
            config <- configs[[method]]; state <- graphmode_initial_state(config, c(1L, 1L, 2L, 2L))
            out <- fixed_environment()$graphmode_compare_step(state, config, policy)
            bad <- out; bad$state$theta[1] <- bad$state$theta[1] + 1
            assert(error(graphmode_compare_check(state, bad, config, record)))
            if (method == 'PottsMoDE') {
                bad <- out; bad$base_diagnostic$potts_gross[1] <- 99
                assert(error(graphmode_compare_check(state, bad, config, record)))
            }
        }
    })
    test('fixed full-size configurations preserve saved data, expert priors and initial states', {
        c5 <- '/Users/liuzw/countDLM-local-results/graphmode-joint-partition-20260917-c5'
        prior <- readRDS(file.path(c5, 'registration.rds')); blind <- readRDS(prior$input)
        source_starts <- readRDS(file.path(c5, 'fresh-starts.rds'))
        assert(digest::digest(file = prior$input, algo = 'sha256', serialize = FALSE) ==
            'ce452d778c9aa47fe560117feedffd2731afd95e8851ba489dd6f62e3136580a')
        built <- graphmode_compare_configurations(blind, spec)
        for (fit in built$fits) {
            graphmode4_validate_config(fit)
            for (j in 1:4) {
                initial <- graphmode_compare_initial(fit, source_starts[[j]]$state)
                graphmode_revalidate_state(initial, fit$core)
                assert(identical(initial$theta, source_starts[[j]]$state$theta))
                assert(identical(initial$Z, source_starts[[j]]$state$Z))
                assert(length(unique(initial$Z)) == spec$start_occupancy[j])
                if (fit$core$adaptive) assert(identical(initial$x, source_starts[[j]]$state$x))
            }
        }
        bad <- source_starts[[1L]]$state; bad$iteration <- 1L
        assert(error(graphmode_compare_initial(built$fits[[1L]], bad)))
        cat('Euc geometry-only range:', format(built$euclidean_range, digits = 17), '\n')
    })
    test('truth-blind summary is invariant to model label permutations', {
        fit <- list(core = configs[['MoDE']]); s <- graphmode_initial_state(fit$core, c(1L, 1L, 2L, 2L))
        truth <- list(Z = c(9L, 9L, 4L, 4L), lambda = matrix(exp(gmde_eta(s$theta, fit$core$Fmat))[s$Z, ], 4, 3))
        a <- graphmode_compare_summary(rep(list(s), 1200L), fit, truth)
        b <- s; b$Z <- c(2L, 2L, 1L, 1L); b$theta <- s$theta[c(2L, 1L, 3L), , ]
        b <- graphmode_compare_summary(rep(list(b), 1200L), fit, truth)
        assert(a$metrics$ARI == 1 && a$metrics$Khat == 2L && a$metrics$profile_rmse < 1e-12)
        assert(identical(a$metrics, b$metrics) && all(a$exact_by_chain == 300L))
    })
    test('controller caps and declared R caps agree', {
        controller <- readLines(file.path(graphmode_root, 'scripts/graphmode-compare-controller.py'))
        assert(any(grepl("('science', 14400), ('diagnose', 1800)", controller, fixed = TRUE)))
    })
    assert(identical(kind, RNGkind()) && identical(had, exists('.Random.seed', .GlobalEnv, inherits = FALSE)))
    assert(identical(seed, if (had) get('.Random.seed', .GlobalEnv) else NULL))
    cat('PASS', checks, 'focused adapter groups; RNG unchanged; no simulation.\n')
})
