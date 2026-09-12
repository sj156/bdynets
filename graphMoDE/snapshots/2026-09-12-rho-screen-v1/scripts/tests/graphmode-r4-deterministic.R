# Fixed arrays only: no response generation, PG, Gaussian draws or MCMC.
if (!exists("graphmode_root", inherits = FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent = globalenv())
    files <- c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), pattern = "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), pattern = "^graphmode4-.*[.]R$")))
    for (f in files) sys.source(file.path(graphmode_root, "R", f), envir = kernel)
    test_environment <- environment()
    parent.env(test_environment) <- kernel
    rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    rng_kind <- RNGkind()
    checks <- 0L
    assert <- function(x) if (!isTRUE(x)) stop("r4 assertion failed: ", paste(deparse(substitute(x)), collapse = " "), call. = FALSE)
    near <- function(x, y, tol = 1e-10) {
        compared <- all.equal(x, y, tolerance = tol, check.attributes = FALSE)
        if (!isTRUE(compared)) stop("r4 numeric comparison: ", paste(compared, collapse = "; "),
            "\nactual: ", paste(x, collapse = ", "), "\nexpected: ", paste(y, collapse = ", "), call. = FALSE)
    }
    fails <- function(code, pattern) {
        e <- tryCatch({force(code); NULL}, error = identity)
        assert(inherits(e, "error") && grepl(pattern, conditionMessage(e)))
    }
    test <- function(name, code) {force(code); checks <<- checks + 1L; cat(sprintf("ok %02d - %s\n", checks, name))}
    road <- graphmode4_road("intertwined-spiral")
    rings <- graphmode4_road("rings")
    Y <- matrix(rep(20:30, length.out = 121 * 168), 121L, 168L)
    Fmat <- cbind(1, sin(2 * pi * (1:168) / 24), cos(2 * pi * (1:168) / 24))
    # Numerical values below are test fixtures, not recommended calibration.
    expert <- list(m0 = c(0, 0, 0), C0 = diag(3), family = "poisson", dynamics = "dynamic",
        G = diag(3), W = diag(.02, 3), rho = 2L, variance_prior = NULL)
    gate <- list(guidance_prior = c(1, 1), guidance_proposal_sd = .2, max_ess_steps = 100L)
    config <- function(method = "graphMoDE-W", r = road, e = expert, guidance = "class-specific")
        graphmode4_config(r, Y, Fmat, method, e, gate, "fixed-test-fixture-NOT-CALIBRATION",
            euclidean_range = 1, range_rule = "fixed-test-fixture", potts_beta = 0, guidance = guidance)

    test("dual-arm geometry has immutable path IDs, unique sites and exact center links", {
        assert(nrow(road$coordinates) == 121L && nrow(road$edges) == 120L)
        assert(!anyDuplicated(as.data.frame(road$coordinates)))
        near(road$coordinates[61L, ], c(0, 0))
        near(road$coordinates[1:60, ], -road$coordinates[121:62, ])
        near(sqrt(rowSums(road$coordinates[c(1, 60, 62, 121), ]^2)), c(3, .5, .5, 3))
        near(road$edges$length[60:61], c(.5, .5))
        near(tabulate(c(road$edges$from, road$edges$to), 121), c(1, rep(2, 119), 1))
    })
    test("arc lengths match independent quadrature and exceed chords", {
        a <- 4 * pi * (0:59) / 59
        for (j in c(1L, 20L, 59L)) {
            integral <- integrate(function(t) sqrt((.5 + 5 * t / (8 * pi))^2 + (5 / (8 * pi))^2), a[j], a[j + 1L])$value
            near(road$edges$length[61L + j], integral)
        }
        chords <- sqrt(rowSums((road$coordinates[1:120, ] - road$coordinates[2:121, ])^2))
        assert(all(road$edges$length[-c(60, 61)] > chords[-c(60, 61)]))
        near(road$road_distance[1, 121], sum(road$edges$length))
    })
    test("rings unchanged; old single spiral cannot become r4 by relabeling", {
        old <- graphmode_simple_road("rings")
        near(rings$coordinates, old$coordinates); assert(nrow(rings$edges) == 144L)
        old <- graphmode_simple_road("spiral"); old$protocol <- graphmode4_protocol; old$geometry <- "intertwined-spiral"
        fails(graphmode4_validate_road(old), "Incoherent")
    })
    test("q4, common W/C support, mean-edge scaling and full-rank kernels", {
        for (r in list(road, rings)) {
            d <- graphmode_dependency_graph(r$road_distance, r$ids)
            assert(all(rowSums(d$directed) == 4L))
            assert(identical(d$connectivity > 0, d$weight > 0))
            near(mean(d$weight[upper.tri(d$weight) & d$weight > 0]), 1)
            k <- graphmode_graph_kernel(d$weight)
            assert(k$rank == 121L && all(k$spectrum > 0)); near(mean(diag(k$covariance)), 1)
        }
    })
    test("coherent row/edge/data reorder preserves geometric ties", {
        changed <- graphmode4_reorder(road, 121:1, Y)
        near(changed$Y, Y[121:1, ])
        a <- graphmode_dependency_graph(road$road_distance, road$ids)
        b <- graphmode_dependency_graph(changed$road$road_distance, changed$road$ids)
        near(b$weight, a$weight[121:1, 121:1])
        near(graphmode4_reorder(changed$road, 121:1, changed$Y)$Y, Y)
        fails(graphmode4_reorder(road, rep(1, 121)), "permutation")
    })
    test("12-setting design, five nonempty Voronoi cells and size-preserving uninformative truth", {
        assert(nrow(graphmode4_main_grid()) == 12L && !anyDuplicated(graphmode4_main_grid()$setting))
        for (r in list(road, rings)) {
            a <- graphmode4_partition(r, "road-voronoi", 5:1)
            b <- graphmode4_partition(r, "euclidean-voronoi", 5:1)
            u <- graphmode4_partition(r, "uninformative", 5:1, 121:1)
            assert(all(a$sizes > 0) && all(b$sizes > 0) && a$seed_ids[1] == 1)
            near(a$sizes, u$sizes); near(u$Z, a$Z[121:1])
            reordered <- graphmode4_partition(graphmode4_reorder(r, 121:1)$road, "uninformative", 5:1, 121:1)
            near(reordered$Z, u$Z[121:1])
        }
    })
    innovations <- array(sin(seq_len(5 * 216 * 3)) / 10, c(5L, 216L, 3L))
    test("normalized clustering has equal totals and exact static branch", {
        a <- graphmode4_profiles(innovations[, 1:168, ], .3, .002, "clustering")
        near(rowSums(a$lambda), rep(4200, 5)); near(gmde_eta(a$theta, a$Fmat), a$eta)
        b <- graphmode_normalized_profiles(innovations[, 1:168, ], .3, .002)
        near(a$lambda, b$lambda)
        s <- graphmode4_profiles(innovations[, 1:168, ], .3, 0, "clustering")
        assert(is.null(s$W) && is.null(s$G) && s$dynamics == "static")
        near(s$theta[, 1, ], s$theta[, 168, ])
        fails(graphmode4_profiles(innovations[, 1:168, ], .3, .002, "clustering", 25), "ell0")
    })
    test("future innovations cannot alter prospective prefix; ell0 is mandatory", {
        a <- graphmode4_profiles(innovations, .3, .002, "forecast", 25)
        future <- innovations; future[, 169:216, ] <- future[, 169:216, ] + 10
        b <- graphmode4_profiles(future, .3, .002, "forecast", 25)
        near(a$lambda[, 1:168], b$lambda[, 1:168]); assert(any(a$lambda[, 169:216] != b$lambda[, 169:216]))
        fails(graphmode4_profiles(innovations, .3, .002, "forecast"), "ell0")
    })
    rec <- config()
    test("five adapters use explicit matching experts with W/C/Euc-specific kernels", {
        variants <- lapply(graphmode4_methods, config)
        assert(all(vapply(variants, function(x) identical(x$core$W, expert$W) && x$core$K == 10L, logical(1))))
        assert(!isTRUE(all.equal(tcrossprod(variants[[1]]$core$Phi), tcrossprod(variants[[2]]$core$Phi))))
        assert(!isTRUE(all.equal(tcrossprod(variants[[1]]$core$Phi), tcrossprod(variants[[3]]$core$Phi))))
        near(variants[[4]]$core$dirichlet_alpha, rep(.1, 10))
        assert(variants[[5]]$core$potts_beta == 0)
        assert(rec$core$protocol == graphmode_protocol && rec$protocol == graphmode4_protocol)
        fails(graphmode4_config(road, Y, Fmat, "EucMoDE", expert, gate, "fixture"), "range")
        e <- expert; e$rho <- NULL
        fails(graphmode4_config(road, Y, Fmat, "MoDE", e, gate, "fixture"), "expert fields")
        mutated <- rec; mutated$core$W <- diag(.01, 3)
        fails(graphmode4_validate_config(mutated), "changed")
    })
    test("targeted module grids and categorical empty classes are retained", {
        grid <- graphmode4_module_grid()
        assert(nrow(grid$A) == 6L && nrow(grid$B) == 8L && nrow(grid$wrong_graph) == 3L)
        truth <- graphmode4_categorical_truth(rings, matrix(0, 121, 5), matrix(0, 121, 5), rep(.01, 121), rep(.5, 5))
        assert(truth$actual_Ktrue == 1L && truth$generating_capacity == 5L)
        near(rowSums(truth$probability), rep(1, 121)); assert(sum(truth$sizes == 0) == 4L)
    })
    test("categorical truth uses projected class-specific covariance and immutable input IDs", {
        g <- matrix(sin(seq_len(121 * 5)), 121, 5)
        e <- matrix(cos(seq_len(121 * 5)), 121, 5)
        a <- c(.1, .3, .5, .7, .9)
        uniforms <- (seq_len(121) - .5) / 121
        truth <- graphmode4_categorical_truth(rings, g, e, uniforms, a)
        phi <- graphmode_graph_kernel(graphmode_dependency_graph(rings$road_distance, rings$ids)$weight)$Phi
        projection <- diag(5) - matrix(1/5, 5, 5)
        direct <- (phi %*% g %*% diag(sqrt(a)) + e %*% diag(sqrt(1-a))) %*% projection
        near(truth$utilities, direct)
        changed <- graphmode4_categorical_truth(graphmode4_reorder(rings, 121:1)$road, g, e, uniforms, a)
        near(changed$Z, truth$Z[121:1]); near(changed$probability, truth$probability[121:1, ])
    })
    test("wrong graph changes borrowing only, not responses or physical IDs", {
        original <- config(r = rings)
        altered <- graphmode4_wrong_graph(original, rings, c(2:121, 1L))
        graphmode4_validate_config(altered)
        assert(identical(original$core$Y, altered$core$Y) && identical(original$ids, altered$ids))
        assert(!isTRUE(all.equal(tcrossprod(original$core$Phi), tcrossprod(altered$core$Phi))))
    })
    test("conditional response likelihood and required ID/time diagnostics agree with direct sums", {
        r <- graphmode4_reorder(road, 121:1)$road
        cfg <- config(r = r)
        theta <- array(0, c(10L, 168L, 3L))
        for (k in 1:10) theta[k, , 1L] <- log(k + 10)
        draw <- list(theta = theta, Z = rep(1:10, length.out = 121), v = seq(-1, 1, length.out = 10), iteration = 1L)
        trace <- graphmode4_trace(list(draw), cfg)
        direct <- sum(vapply(1:121, function(i) sum(dpois(Y[i, ], draw$Z[i] + 10, log = TRUE)), numeric(1)))
        near(trace$scalars[1, "log_likelihood"], direct)
        near(trace$scalars[1, "unit_log_mean_1_t84"], log(draw$Z[121] + 10))
        assert(ncol(trace$scalars) == 37L)
        shared <- config(guidance = "shared"); draw$v <- .2
        assert(sum(startsWith(colnames(graphmode4_trace(list(draw), shared)$scalars), "guidance_")) == 1L)
        none <- config(guidance = "none"); draw$v <- numeric()
        assert(!any(startsWith(colnames(graphmode4_trace(list(draw), none)$scalars), "guidance_")))
    })
    test("constant occupancy is uninformative, incompatible constants fail, no fabricated ESS", {
        d <- graphmode4_scalar_diagnostic(matrix(5, 20, 4), TRUE)
        assert(d$status == "uninformative-constant-discrete" && is.na(d$bulk_ess) && is.na(d$rank_rhat))
        assert(graphmode4_scalar_diagnostic(matrix(rep(1:4, each = 20), 20, 4), TRUE)$status == "failed-different-chain-constants")
        assert(graphmode4_scalar_diagnostic(matrix(5, 20, 4), FALSE)$status == "failed-undefined-constant")
    })
    test("separate rank/folded Rhats agree with installed posterior on even/odd inputs", {
        for (n in c(1000L, 1001L)) {
            x <- matrix(qnorm((seq_len(n * 4) * sqrt(2)) %% 1), n, 4)
            d <- graphmode4_scalar_diagnostic(x)
            near(max(d$rank_rhat, d$folded_rhat), posterior::rhat(x))
            near(d$bulk_ess, posterior::ess_bulk(x)); near(d$tail_ess, posterior::ess_tail(x))
            assert(d$status == "passed")
            shifted <- sweep(x, 2L, c(0, 0, 2, 2), "+")
            assert(graphmode4_scalar_diagnostic(shifted)$status == "failed")
        }
    })
    test("PSM compares unique unit pairs, without diagonal dilution", {
        a <- diag(3); b <- a; b[1, 2] <- b[2, 1] <- .1
        near(graphmode4_psm_rms(a, b), .1 / sqrt(3))
        assert(graphmode4_psm_rms(a, b) > .05)
        fails(graphmode4_psm_rms(a, diag(4)), "dimensions")
    })
    test("validity rejects missing chains, changed source inputs and omitted guard evidence", {
        assert(!graphmode4_validity(list(), rec, 1:4, 20, 10, 1)$valid)
        assert(!graphmode4_validity(rep(list(list(complete = FALSE, failure = "resource")), 4), rec, 1:4, 20, 10, 1)$valid)
        fails(graphmode4_validity(list(), rec, c(1, 1, 2, 3), 20, 10, 1), "distinct")
        theta <- array(0, c(10L, 168L, 3L)); theta[, , 1] <- log(25)
        draws <- lapply(11:20, function(i) list(theta = theta, Z = rep(1L, 121), v = rep(0, 10), iteration = i))
        chains <- lapply(1:4, function(j) list(complete = TRUE, seed = j, iterations = 20L, warmup = 10L, thin = 1L,
            config_signature = rec$signature, numerical_guards_passed = TRUE, movement = list(node_moves = rep(0, 20)), draws = draws))
        report <- graphmode4_validity(chains, rec, 1:4, 20, 10, 1)
        assert(!report$valid && nrow(report$psm_rms) == 6L && all(report$psm_rms$rms == 0))
        assert(all(report$scalars$status[report$scalars$scalar == "Kocc"] == "uninformative-constant-discrete"))
        chains[[1]]$numerical_guards_passed <- FALSE
        assert(length(graphmode4_validity(chains, rec, 1:4, 20, 10, 1)$failures) == 1L)
    })
    settings <- graphmode4_main_grid()$setting
    attempts <- expand.grid(method = graphmode4_methods, setting = settings, replicate = 1:20, stringsAsFactors = FALSE)
    attempts$data_id <- paste(attempts$setting, attempts$replicate)
    attempts$valid <- TRUE; attempts$ARI <- .5; attempts$end_to_end_seconds <- 1
    attempts$cost_context <- "deterministic-fixture-single-context"; attempts$failure <- ""
    pool_rows <- data.frame(pool = c("calibration", rep("selection", 240), "formal"),
        data_id = c("calibration-fixture", unique(attempts$data_id), "formal-fixture"), seed = 1:242)
    pools <- graphmode4_pools(pool_rows)
    invalidate <- function(a, rows) {a$valid[rows] <- FALSE; a$ARI[rows] <- NA_real_; a$failure[rows] <- "statistical"; a}
    test("three pools enforce distinct seeds/identities; selection needs all paired attempts", {
        bad <- pool_rows; bad$seed[1] <- bad$seed[2]
        fails(graphmode4_pools(bad), "disjoint")
        fails(graphmode4_select(attempts[-1, ], pools), "1200")
        bad <- attempts; bad$data_id[1] <- "formal-fixture"
        fails(graphmode4_select(bad, pools), "same dataset")
    })
    test("exact ties follow fixed order and freeze one representative without run authority", {
        r <- graphmode4_select(attempts, pools)
        assert(r$selected == "EucMoDE" && all(r$common_counts == 20) && length(r$eligible) == 5L)
        near(r$score, rep(.5, 5)); assert(nrow(r$paired_contrasts) == 10L)
        record <- graphmode4_freeze_representative(attempts, pools, "config-fixture", "decision-fixture")
        assert(record$method == "EucMoDE" && !record$formal_authorized)
    })
    test("no eligibility and insufficient common intersection remain unresolved", {
        none <- invalidate(attempts, rep(TRUE, nrow(attempts)))
        assert(graphmode4_select(none, pools)$status == "unresolved")
        a <- attempts
        for (j in 1:5) a <- invalidate(a, a$method == graphmode4_methods[j] & a$replicate %in% (2*j-c(1,0)))
        r <- graphmode4_select(a, pools)
        assert(length(r$eligible) == 5L && all(r$valid_counts == 18L) && all(r$common_counts == 10L))
        assert(r$status == "unresolved" && is.null(r$selected))
    })
    test("failure ARI is unavailable; missing adapters are blockers, not bad scores", {
        a <- invalidate(attempts, 1L); a$ARI[1] <- 0
        fails(graphmode4_select(a, pools), "unavailable ARI")
        a$ARI[1] <- NA_real_; a$failure[1] <- "missing-adapter"
        fails(graphmode4_select(a, pools), "block the stage")
        a <- attempts; a$cost_context[1] <- "different-hardware"
        fails(graphmode4_select(a, pools), "matched hardware")
    })
    test("practical tie uses validity then cost, and single eligible is not an accuracy win", {
        a <- attempts; a$ARI[a$method == "graphMoDE-W"] <- .509
        assert(graphmode4_select(a, pools)$selected == "EucMoDE")
        a$end_to_end_seconds[a$method == "graphMoDE-C"] <- .5
        assert(graphmode4_select(a, pools)$selected == "graphMoDE-C")
        a <- invalidate(a, a$method != "graphMoDE-W" & a$replicate == 1L)
        assert(graphmode4_select(a, pools)$selected == "graphMoDE-W")
        a <- invalidate(a, a$method != "MoDE" & a$replicate <= 3L)
        r <- graphmode4_select(a, pools)
        assert(r$selected == "MoDE" && r$single_eligible && !r$accuracy_win_claim)
    })
    test("overall validity breaks equal minimum-validity ties before runtime", {
        a <- invalidate(attempts, attempts$replicate == 1L)
        a <- invalidate(a, a$method != "graphMoDE-W" & a$setting != settings[1] & a$replicate == 2L)
        a <- invalidate(a, a$setting == settings[1] & a$replicate == 2L)
        a$end_to_end_seconds[a$method == "graphMoDE-W"] <- 100
        r <- graphmode4_select(a, pools)
        assert(all(apply(r$valid_counts, 1, min) == 18L) && r$selected == "graphMoDE-W")
    })
    test("selection averages settings equally and uses paired-dataset MCSE", {
        a <- attempts
        a$ARI[a$method == "graphMoDE-W"] <- ifelse(a$setting[a$method == "graphMoDE-W"] == settings[1], 1, 0)
        a <- invalidate(a, a$setting == settings[1] & a$replicate <= 2L)
        r <- graphmode4_select(a, pools)
        near(unname(r$score["graphMoDE-W"]), 1/12)
        near(r$paired_contrasts$paired_mcse, rep(0, 10))
        near(as.numeric(r$total_attempt_seconds), rep(240, 5))
    })
    test("selection overlap at 16 is accepted and method order cannot change winner", {
        a <- attempts
        a <- invalidate(a, a$method == "graphMoDE-W" & a$replicate %in% 1:2)
        a <- invalidate(a, a$method == "graphMoDE-C" & a$replicate %in% 3:4)
        r <- graphmode4_select(a, pools)
        assert(all(r$common_counts == 16L) && r$status == "resolved-selection-only")
        assert(identical(r$selected, graphmode4_select(a[nrow(a):1, ], pools)$selected))
    })
    test("48 full-prefix tasks exclude future data and require frozen initial-window tuning", {
        schedule <- graphmode4_forecast_schedule()
        assert(identical(schedule$origin, 168:215) && all(schedule$chains == 4L))
        h <- graphmode4_freeze_hyperparameters(list(test_only = 1), "initial168-fixture", "fixture")
        full <- matrix(rep(1:30, length.out = 121 * 216), 121, 216)
        a <- graphmode4_forecast_prefix(full, 190L, h, 2L)
        future <- full; future[, 191:216] <- 999L
        b <- graphmode4_forecast_prefix(future, 190L, h, 2L)
        assert(identical(a, b) && ncol(a$Y) == 190L && identical(a$independent_start_chains, c(1L, 3L, 4L)))
        assert(!a$reuse_posterior_draws && a$new_warmup_required && a$refit_allocations && a$refit_pnar_coefficients)
        fails(graphmode4_forecast_prefix(full, 168L, h, 1L), "preceding")
        h$values$test_only <- 2
        fails(graphmode4_forecast_prefix(full, 168L, h), "changed")
    })
    test("Bayesian conditional mean averages exponentials with allocation and next-innovation variance", {
        theta <- array(0, c(10L, 168L, 3L)); theta[2L, , 1L] <- log(9)
        draws <- list(list(Z = rep(1L, 121), theta = theta), list(Z = rep(2L, 121), theta = theta))
        means <- graphmode4_predict_mean(draws, rec, c(1, 0, 0))
        near(means, rep(5 * exp(.01), 121)); assert(means[1] > exp(mean(c(0, log(9))) + .01))
        e <- expert; e$dynamics <- "static"; e["G"] <- list(NULL); e["W"] <- list(NULL)
        static <- config(e = e)
        near(graphmode4_predict_mean(draws, static, c(1, 0, 0)), rep(5, 121))
        assert(is.null(static$core$W))
    })
    test("PNAR uses row-normalized road weights, stable coefficients and no true class labels", {
        W <- graphmode_dependency_graph(road$road_distance, road$ids)$weight
        control <- graphmode4_pnar_control(.2, .3, rep(25, 121), 100L, "fixture")
        near(graphmode4_pnar_mean(rep(25, 121), W, control$beta), rep(25, 121))
        assert(is.na(control$Ktrue) && is.na(control$ARI))
        fails(graphmode4_pnar_control(.5, .5, rep(25, 121), 100L, "fixture"), "stable")
    })
    test("MAE and RMSE use identical unrounded predictions and whole-horizon square root", {
        predictions <- matrix(.25, 121, 48); predictions[, 1] <- 2.25
        observed <- matrix(0, 121, 48)
        s <- graphmode4_forecast_score(predictions, observed, rep(TRUE, 48), rep("", 48))
        near(s$MAE, mean(predictions)); near(s$RMSE, sqrt(mean(predictions^2)))
        assert(s$RMSE != mean(sqrt(colMeans(predictions^2))))
    })
    test("one failed origin makes full horizon unavailable and disallows partial restart scores", {
        predictions <- matrix(.25, 121, 48); predictions[, 10:48] <- NA_real_
        valid <- c(rep(TRUE, 9), rep(FALSE, 39)); causes <- c(rep("", 9), "resource", rep("not-attempted", 38))
        s <- graphmode4_forecast_score(predictions, matrix(0, 121, 48), valid, causes)
        assert(!s$complete && is.na(s$MAE) && is.na(s$RMSE) && s$failure_origin == 177L)
        predictions[, 10:48] <- 0
        fails(graphmode4_forecast_score(predictions, matrix(0, 121, 48), valid, causes), "unavailable")
    })
    test("forecast pairing retains all attempts and reports dataset-level conditional contrasts", {
        a <- data.frame(data_id = rep(c("a", "b", "c"), each = 2), method = rep(c("MoDE", "PNAR"), 3),
            complete = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), MAE = c(1, 2, 3, NA, 4, 5),
            RMSE = c(2, 3, 4, NA, 5, 6), failure_origin = c(NA, NA, NA, 170L, NA, NA), seconds = 1)
        r <- graphmode4_forecast_compare(a, "MoDE")
        assert(all(r$paired$n == 2L) && nrow(r$attempts) == 6L)
        near(r$paired$difference, c(-1, -1)); near(unname(r$completion_rate["PNAR"]), 2/3)
        fails(graphmode4_forecast_compare(a[-1, ], "MoDE"), "Missing")
    })
    test("old source glob excludes r4 modules and all checks leave RNG unchanged", {
        old_files <- names(graphmode_source_identity(graphmode_root)$sha256)
        assert(!any(grepl("graphmode4-|graphmode-r4", old_files)))
        identity <- graphmode4_source_identity(graphmode_root)
        assert(identity$requirements_match && !identity$run_authorized)
        assert(all(paste0("R/graphmode4-", c("design", "diagnostics", "selection", "forecast", "provenance"), ".R") %in% names(identity$sha256)))
        now <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
        assert(identical(rng, now) && identical(rng_kind, RNGkind()))
    })
    cat(sprintf("PASS: %d r4 fixed-input groups; no stochastic run.\n", checks))
})
