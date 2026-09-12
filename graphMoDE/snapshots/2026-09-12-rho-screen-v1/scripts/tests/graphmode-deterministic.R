# Source-checkout deterministic verification, separate from legacy stochastic
# package tests. The Terminal launcher sets graphmode_root; no packages installed.
if (!exists("graphmode_root", inherits = FALSE)) graphmode_root <- normalizePath(".")
local({
    kernel <- new.env(parent = globalenv())
    files <- c("R/gmde-helpers.R", "R/gmde-state-update.R",
               sort(list.files(file.path(graphmode_root, "R"),
                   pattern = "^graphmode-.*[.]R$", full.names = FALSE)))
    files <- c(files[1:2], file.path("R", files[-(1:2)]))
    for (f in files) sys.source(file.path(graphmode_root, f), envir = kernel)
    before_rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
        get(".Random.seed", .GlobalEnv) else NULL
    checks <- 0L
    assert <- function(ok) if (!isTRUE(ok)) stop("Assertion failed.", call. = FALSE)
    close <- function(x, y, tol = 1e-10) {
        assert(identical(dim(x), dim(y)))
        assert(length(x) == length(y) && all(is.finite(c(x, y))) &&
            max(abs(x - y), 0) <= tol * max(1, max(abs(y), 0)))
    }
    fails <- function(expr, pattern) {
        error <- tryCatch({force(expr); NULL}, error = identity)
        assert(inherits(error, "error") && grepl(pattern, conditionMessage(error)))
    }
    test <- function(name, code) {
        # Fixed algebraic tapes, not random draws from any distribution.
        kernel$graphmode_normal <- function(n) rep(0, n)
        kernel$graphmode_uniform <- function(n) rep(0.5, n)
        kernel$graphmode_gamma <- function(shape, rate) shape / rate
        kernel$graphmode_pg <- function(b, z) rep(0.25, length(b))
        force(code)
        checks <<- checks + 1L
        cat(sprintf("ok %02d - %s\n", checks, name))
    }
    # Tests execute against an isolated source environment so RNG tapes cannot
    # escape into the package namespace or later real fits.
    testenv <- environment()
    parent.env(testenv) <- kernel
    basic <- function(method = "graphMoDE-W", K = 3L, guidance = "class-specific",
                      family = "gaussian", dynamics = "dynamic") {
        graphmode_config(Y = matrix(c(0, 1, 2, 1, 3, 4), 3L, 2L),
            Fmat = matrix(1, 2L, 1L), m0 = 0, C0 = matrix(1), method = method,
            K = K, family = family, dynamics = dynamics,
            G = if (dynamics == "dynamic") matrix(1) else NULL,
            W = if (dynamics == "dynamic") matrix(0.2) else NULL,
            Phi = diag(3), guidance = guidance, guidance_proposal_sd = 0.2,
            graph_weight = matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3),
            potts_beta = 0.7, dirichlet_alpha = rep(1 / K, K),
            variance_prior = if (family == "gaussian") c(2, 1) else NULL,
            rho = if (family == "poisson") 2L else NULL)
    }
    test("stable log softmax, including huge common offsets", {
        close(exp(graphmode_logsoftmax(matrix(1e20, 2, 3))), matrix(1 / 3, 2, 3))
        close(rowSums(exp(graphmode_logsoftmax(matrix(c(1000, -1000, 0, 1), 2)))), c(1, 1))
        fails(graphmode_logsoftmax(matrix(c(-1e308, 1e308), 1)), "floating-point range")
    })
    test("extreme-precision scalar filter regression", {
        out <- graphmode_information_filter(1e18, 2e18, matrix(1), 0,
            matrix(0.5), matrix(1), matrix(0.5))
        assert(abs(out$root[1, 1, 1]^2 / 1e-18 - 1) < 1e-12)
        close(out$m, matrix(2))
    })
    test("backward conditional retains tiny positive innovation variance", {
        out <- graphmode_backward_condition(0, matrix(1), 2, matrix(1), matrix(1e-9))
        assert(abs(out$root[1, 1]^2 / 1e-18 - 1) < 1e-12)
        close(out$mean, 2)
        fails(graphmode_information_filter(1, 0, matrix(1), 0, matrix(1),
            matrix(1), matrix(0)), "positive definite")
    })
    test("multivariate static information matches independent dense formula", {
        C <- matrix(c(2, 0.4, 0.4, 1), 2)
        F <- cbind(1, c(-1, 0.2, 2))
        precision <- c(2, 3, 0.5)
        natural <- c(1, -2, 3)
        m0 <- c(0.2, -0.4)
        covariance <- solve(solve(C) + crossprod(F, precision * F))
        mean <- as.numeric(covariance %*% (solve(C, m0) + crossprod(F, natural)))
        out <- graphmode_information_condition(m0, t(chol(C)), F, precision, natural)
        close(out$covariance, covariance)
        close(out$mean, mean)
        unchanged <- graphmode_information_condition(m0, t(chol(C)), F, c(0, 0, 0), c(0, 0, 0))
        close(unchanged$mean, m0)
        close(unchanged$covariance, C)
        fails(graphmode_information_condition(m0, t(chol(C)), F, c(0, 1, 1), c(1, 1, 1)), "Zero observation")
    })
    test("full multivariate FFBS mean and covariance by deterministic basis probes", {
        p <- 2L; TT <- 3L
        F <- cbind(1, c(-1, 0.5, 2))
        C <- matrix(c(1.2, .2, .2, .8), 2)
        W <- matrix(c(.3, .05, .05, .4), 2)
        G <- matrix(c(.9, -.1, .2, .8), 2)
        m0 <- c(.4, -.2)
        prior_root <- matrix(0, p * TT, p * (TT + 1L))
        previous <- cbind(t(chol(C)), matrix(0, p, p * TT))
        prior_mean <- numeric(p * TT)
        m <- m0
        design <- matrix(0, TT, p * TT)
        for (t in seq_len(TT)) {
            m <- G %*% m
            pos <- (t - 1L) * p + seq_len(p)
            prior_mean[pos] <- m
            previous <- G %*% previous
            previous[, t * p + seq_len(p)] <- t(chol(W))
            prior_root[pos, ] <- previous
            design[t, pos] <- F[t, ]
        }
        prior <- tcrossprod(prior_root)
        precision <- c(2, 5, 1)
        natural <- c(-1, .2, 2)
        target_cov <- solve(solve(prior) + crossprod(design, precision * design))
        target_mean <- as.numeric(target_cov %*% (solve(prior, prior_mean) + crossprod(design, natural)))
        probe <- function(tape) {
            cursor <- 0L
            kernel$graphmode_normal <- function(n) {
                value <- tape[cursor + seq_len(n)]
                cursor <<- cursor + n
                value
            }
            out <- graphmode_ffbs(precision, natural, F, m0, C, G, W)$theta
            assert(cursor == p * TT)
            as.vector(t(out))
        }
        mean <- probe(numeric(p * TT))
        root <- vapply(seq_len(p * TT), function(j) probe(diag(p * TT)[, j]) - mean,
            numeric(p * TT))
        close(mean, target_mean)
        close(tcrossprod(root), target_cov)
    })
    test("high-precision multivariate conditioning does not use covariance downdates", {
        out <- graphmode_information_condition(c(0, 0), diag(2), matrix(c(1, 1), 1), 1e18, 2e18)
        close(sum(out$mean), 2, 1e-8)
        observed_var <- sum((matrix(c(1, 1), 1) %*% out$root)^2)
        assert(abs(observed_var / 1e-18 - 1) < 1e-6)
        close(sum((matrix(c(1, -1), 1) %*% out$root)^2), 2, 1e-8)
    })
    test("P2 public filter rejects the audited inaccurate roots before drawing", {
        kernel$graphmode_normal <- function(n) stop("Unexpected random draw")
        for (precision in c(1e24, 1e30, 1e32, 1e34)) for (F in list(c(1, 2, 3), c(3, 1, 2))) {
            fails(graphmode_information_filter(precision, 2 * precision, matrix(F, 1),
                rep(0, 3), diag(.5, 3), diag(3), diag(.5, 3)), "covariance-root accuracy")
        }
    })
    test("P2 guard retains analytic scalar and supported multivariate variances", {
        for (precision in c(0, 1, 1e12, 1e18)) {
            F <- matrix(c(1, 2, 3), 1)
            out <- graphmode_information_filter(precision, 2 * precision, F,
                rep(0, 3), diag(.5, 3), diag(3), diag(.5, 3))
            B <- matrix(out$root[, , 1], 3)
            expected <- 1 / (1 / 14 + precision)
            assert(abs(sum((F %*% B)^2) / expected - 1) < 1e-6)
            close(as.numeric(F %*% out$m[1, ]), 2 * precision * expected)
            assert(out$root_residual <= 1e-6 && is.finite(out$root_reciprocal_condition))
        }
        # Large absolute precision alone is not a reason to reject: no rotated
        # near-null direction has to be represented in this one-dimensional case.
        out <- graphmode_information_filter(1e34, 2e34, matrix(1), 0,
            matrix(.5), matrix(1), matrix(.5))
        assert(abs(out$root[1, 1, 1]^2 / 1e-34 - 1) < 1e-12)
        assert(out$root_reciprocal_condition == 1)
    })
    test("P2 root audit checks the solved root and original precision, not only QR", {
        F <- matrix(0, 1, 2)
        good <- graphmode_conditional_root_audit(diag(2), diag(2), F, 0, diag(2))
        assert(good$root_residual == 0)
        fails(graphmode_conditional_root_audit(diag(2), diag(2), F, 0,
            diag(c(1, 1.001))), "covariance-root accuracy")
        # This B exactly inverts R, but R corresponds to the wrong target.
        fails(graphmode_conditional_root_audit(diag(10, 2), diag(2), F, 0,
            diag(.1, 2)), "covariance-root accuracy")
        # Even an accidentally exact residual cannot bypass the conservative
        # condition-number fence for an extremely anisotropic multivariate root.
        fails(graphmode_conditional_root_audit(diag(c(1e17, 1)), diag(2),
            matrix(c(1, 0), 1), 1e34, diag(c(1e-17, 1))), "covariance-root accuracy")
    })
    test("P2 static, FFBS and backward conditionals fail before a Gaussian draw", {
        kernel$graphmode_normal <- function(n) stop("Unexpected random draw")
        kernel$graphmode_gamma <- function(...) stop("Unexpected gamma draw")
        F <- matrix(c(1, 2, 3), 1)
        cfg <- graphmode_config(matrix(0, 1, 1), F, rep(0, 3), diag(3),
            "MoDE", K = 1L, dynamics = "static", family = "gaussian",
            variance_prior = c(2, 1), dirichlet_alpha = 1)
        fails(graphmode_update_expert(matrix(0, 1, 3), cfg$Y, cfg, sigma2 = 1e-34),
            "covariance-root accuracy")
        fails(graphmode_ffbs(1e34, 2e34, F, rep(0, 3), diag(.5, 3),
            diag(3), diag(.5, 3)), "covariance-root accuracy")
        G <- rbind(c(1, 2, 3), c(0, 0, 0), c(0, 0, 0))
        fails(graphmode_backward_condition(rep(0, 3), diag(3), c(2, 0, 0), G,
            diag(c(1e-17, 1, 1))), "covariance-root accuracy")
    })
    test("P2 dynamic Poisson PG information cannot bypass root protection", {
        cfg <- graphmode_config(matrix(1, 1, 1), matrix(c(1, 2, 3), 1),
            rep(0, 3), diag(.5, 3), "MoDE", K = 1L, G = diag(3),
            W = diag(.5, 3), rho = 1L, dirichlet_alpha = 1)
        pg_calls <- 0L
        kernel$graphmode_pg <- function(b, z) {pg_calls <<- pg_calls + 1L; 1e34}
        kernel$graphmode_normal <- function(n) stop("Unexpected random draw")
        kernel$graphmode_uniform <- function(n) stop("Unexpected MH draw")
        fails(graphmode_update_expert(matrix(0, 1, 3), cfg$Y, cfg), "covariance-root accuracy")
        assert(pg_calls == 1L)
    })
    test("P2 root diagnostics include backward steps and reach expert updates", {
        original <- kernel$graphmode_backward_condition
        kernel$graphmode_backward_condition <- function(...) {
            result <- original(...)
            result$root_residual <- 5e-7
            result$root_reciprocal_condition <- .25
            result
        }
        out <- graphmode_ffbs(c(1, 1), c(0, 0), matrix(1, 2, 1), 0,
            matrix(1), matrix(1), matrix(.2))
        close(out$root_residual, 5e-7)
        close(out$root_reciprocal_condition, .25)
        cfg <- basic()
        update <- graphmode_update_expert(matrix(0, 2, 1), cfg$Y, cfg, 1)
        close(update$root_residual, 5e-7)
        close(update$root_reciprocal_condition, .25)
        kernel$graphmode_backward_condition <- original
    })
    test("adaptive gate covariance equals projected raw class-specific covariance", {
        cfg <- basic()
        cfg$Phi <- t(chol(matrix(c(1, .4, .2, .4, 1, .3, .2, .3, 1), 3)))
        cfg <- do.call(graphmode_config, cfg[names(formals(graphmode_config))])
        v <- qlogis(c(.1, .5, .9))
        d <- graphmode_gate_dimension(cfg)
        linear <- vapply(seq_len(d), function(j)
            as.vector(graphmode_gate_utilities(diag(d)[, j], v, cfg)), numeric(cfg$n * cfg$K))
        P <- diag(3) - matrix(1 / 3, 3, 3)
        expected <- kronecker(P, matrix(cfg$s_b^2, 3, 3)) +
            kronecker(P %*% diag(c(.1, .5, .9)) %*% P, tcrossprod(cfg$Phi)) +
            kronecker(P %*% diag(c(.9, .5, .1)) %*% P, diag(cfg$tau^2, 3))
        close(tcrossprod(linear), expected)
        close(rowSums(graphmode_gate_utilities(sin(seq_len(d)), v, cfg)), numeric(3))
    })
    test("shared, K2, K1 and exact endpoint parameter counts", {
        cfg <- basic(K = 2L)
        assert(cfg$guidance == "shared")
        assert(graphmode_gate_dimension(cfg) == 7L)
        f <- graphmode_guidance_factors(qlogis(.3), 2L, "shared")
        close(tcrossprod(f$g), matrix(.3))
        fails(graphmode_guidance_factors(c(0, 0), 2L, "class-specific"), "Binary")
        assert(graphmode_gate_dimension(basic(guidance = "none")) == 8L)
        assert(graphmode_gate_dimension(basic(guidance = "forced")) == 8L)
        cfg1 <- basic(K = 1L)
        assert(graphmode_gate_dimension(cfg1) == 0L)
        close(graphmode_gate_utilities(numeric(), numeric(), cfg1), matrix(0, 3, 1))
        assert(length(graphmode_initial_state(cfg1, rep(1L, 3), sigma2 = 1)$x) == 0L)
        fails(graphmode_initial_state(cfg1, rep(1L, 3), sigma2 = 1, x = 1), "whitened gate")
    })
    test("logit Jacobian, shared prior counted once, and no endpoint clipping", {
        shape <- c(2, 3)
        v <- qlogis(.3)
        close(graphmode_guidance_log_prior(v, shape), 2 * log(.3) + 3 * log(.7))
        close(graphmode_guidance_log_prior(rep(v, 3), shape), 3 * (2 * log(.3) + 3 * log(.7)))
        f <- graphmode_guidance_factors(40, 3L, "shared")
        assert(f$e[1, 1] > 0 && f$e[1, 1] < 1e-8)
        fails(graphmode_guidance_factors(1000, 3L, "shared"), "underflow")
        close(graphmode_guidance_factors(numeric(), 3L, "none")$g, matrix(0, 2, 2))
        close(graphmode_guidance_factors(numeric(), 3L, "forced")$e, matrix(0, 2, 2))
    })
    test("joint ESS cache matches direct utilities, fixed deterministic direction", {
        cfg <- basic()
        d <- graphmode_gate_dimension(cfg)
        x <- cos(seq_len(d)) / 10
        kernel$graphmode_normal <- function(n) sin(seq_len(n)) / 10
        tape <- c(1e-8, .13)
        kernel$graphmode_uniform <- function(n) {out <- head(tape, n); tape <<- tail(tape, -n); out}
        out <- graphmode_gate_ess(x, c(-1, 0, 1), c(1, 2, 3), cfg)
        assert(out$evaluations == 1L)
        close(out$utilities, graphmode_gate_utilities(out$x, c(-1, 0, 1), cfg))
    })
    test("guidance Metropolis step uses the fixed white state and transformed prior", {
        cfg <- basic(guidance = "shared")
        x <- sin(seq_len(graphmode_gate_dimension(cfg)))
        v <- .5
        kernel$graphmode_normal <- function(n) rep(.7, n)
        candidate <- v + .7 * cfg$guidance_proposal_sd
        ratio <- graphmode_gate_loglik(graphmode_gate_utilities(x, candidate, cfg), c(1, 2, 1)) -
            graphmode_gate_loglik(graphmode_gate_utilities(x, v, cfg), c(1, 2, 1)) +
            graphmode_guidance_log_prior(candidate, c(1, 1)) - graphmode_guidance_log_prior(v, c(1, 1))
        out <- graphmode_guidance_update(x, v, c(1, 2, 1), cfg)
        assert(out$accepted == (log(.5) <= min(0, ratio)))
        close(out$utilities, graphmode_gate_utilities(x, out$v, cfg))
    })
    test("Poisson/NB MH correction agrees with exact mass functions", {
        S <- c(0, 1, 10, 50)
        r <- gmde_make_nb_r(S, 3L)
        old <- c(.1, 2, 15, 45)
        new <- c(.2, 1, 8, 60)
        direct <- sum(dpois(S, new, log = TRUE) - dnbinom(S, mu = new, size = r, log = TRUE) -
                      dpois(S, old, log = TRUE) + dnbinom(S, mu = old, size = r, log = TRUE))
        close(sum(gmde_poisson_nb_log_ratio(S, new, r) - gmde_poisson_nb_log_ratio(S, old, r)), direct)
        small <- gmde_poisson_nb_log_ratio(0, 1e-10, 1e6)
        assert(small < 0 && abs(small / (-0.5e-26) - 1) < 1e-12)
    })
    test("static Poisson rejection retains coefficients and refreshes PG each call", {
        cfg <- basic(family = "poisson", dynamics = "static")
        original <- kernel$graphmode_gaussian_draw
        kernel$graphmode_gaussian_draw <- function(condition) 10
        calls <- 0L
        kernel$graphmode_pg <- function(b, z) {
            calls <<- calls + 1L
            close(b, c(2, 2))
            close(z, rep(-log(2), 2))
            rep(.25, length(b))
        }
        for (j in 1:2) {
            out <- graphmode_update_expert(matrix(0, 2, 1), matrix(0, 1, 2), cfg)
            assert(!out$accepted && out$movement == 0)
            close(out$theta, matrix(0, 2, 1))
            close(out$r, c(2, 2))
        }
        kernel$graphmode_gaussian_draw <- original
        assert(calls == 2L)
    })
    test("Gaussian static update and inverse-Gamma sufficient statistics", {
        cfg <- basic(dynamics = "static")
        shape_used <- rate_used <- NULL
        kernel$graphmode_gamma <- function(shape, rate) {
            shape_used <<- shape; rate_used <<- rate; shape / rate
        }
        out <- graphmode_update_expert(matrix(0, 2, 1), cfg$Y, cfg, sigma2 = 2)
        target_mean <- sum(cfg$Y) / 2 / (1 + length(cfg$Y) / 2)
        close(out$theta, matrix(target_mean, 2, 1))
        close(shape_used, 2 + length(cfg$Y) / 2)
        close(rate_used, 1 + sum((cfg$Y - target_mean)^2) / 2)
        assert(is.na(out$accepted))
    })
    test("empty experts use prior, including Gaussian variance, without PG", {
        kernel$graphmode_pg <- function(...) stop("PG must not be called")
        for (dynamics in c("dynamic", "static")) for (family in c("gaussian", "poisson")) {
            cfg <- basic(dynamics = dynamics, family = family)
            out <- graphmode_update_expert(matrix(1, 2, 1), matrix(numeric(), 0, 2), cfg,
                                          sigma2 = if (family == "gaussian") 1 else NULL)
            assert(out$empty && length(out$r) == 0L)
            close(out$theta, matrix(0, 2, 1))
            if (family == "gaussian") close(out$sigma2, .5)
        }
    })
    test("Gaussian and Poisson allocation weights match independent densities", {
        Y <- matrix(c(0, 2, 1, 3), 2)
        eta <- matrix(c(.1, -.3, .2, .6, -.1, 1), 3)
        sigma <- c(.4, 1, 2)
        for (family in c("poisson", "gaussian")) {
            reference <- matrix(0, 2, 3)
            for (i in 1:2) for (k in 1:3) reference[i, k] <- if (family == "poisson")
                sum(dpois(Y[i, ], exp(eta[k, ]), log = TRUE)) else
                sum(dnorm(Y[i, ], eta[k, ], sqrt(sigma[k]), log = TRUE))
            value <- graphmode_response_loglik(Y, eta, family, sigma)
            close(graphmode_logsoftmax(value), graphmode_logsoftmax(reference))
        }
    })
    test("Potts single-site conditional matches joint energy; beta zero uniform", {
        W <- matrix(c(0, 1, .3, 1, 0, .7, .3, .7, 0), 3)
        response <- matrix(c(.2, -.1, .4, .1, .3, -.2), 3)
        Z <- c(1L, 2L, 1L)
        energy <- function(z) sum(response[cbind(1:3, z)]) +
            .8 * sum((W * outer(z, z, "=="))[upper.tri(W)])
        for (i in 1:3) {
            value <- vapply(1:2, function(k) {z <- Z; z[i] <- k; energy(z)}, numeric(1))
            close(graphmode_logsoftmax(matrix(value, 1)),
                graphmode_logsoftmax(matrix(graphmode_potts_logweights(i, Z, response, W, .8), 1)))
        }
        close(graphmode_potts_logweights(1, Z, matrix(0, 3, 2), W, 0), c(0, 0))
    })
    test("exhaustive Potts systematic-scan transition preserves joint law", {
        states <- as.matrix(expand.grid(rep(list(1:2), 3)))
        W <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3)
        response <- matrix(c(.2, -.1, .4, .1, .3, -.2), 3)
        energy <- apply(states, 1, function(z) sum(response[cbind(1:3, z)]) +
            .7 * sum((W * outer(z, z, "=="))[upper.tri(W)]))
        target <- as.numeric(exp(graphmode_logsoftmax(matrix(energy, 1))))
        transition <- matrix(1, nrow(states), nrow(states))
        for (a in seq_len(nrow(states))) for (b in seq_len(nrow(states))) {
            z <- states[a, ]
            for (i in 1:3) {
                prob <- exp(graphmode_logsoftmax(matrix(
                    graphmode_potts_logweights(i, z, response, W, .7), 1)))
                transition[a, b] <- transition[a, b] * prob[1, states[b, i]]
                z[i] <- states[b, i]
            }
        }
        close(rowSums(transition), rep(1, 8))
        close(as.numeric(target %*% transition), target)
    })
    test("pure relabeling creates no births, deaths, crossings or partition moves", {
        before <- c(1, 1, 1, 1, 1, 3)
        after <- c(3, 3, 3, 3, 3, 2)
        events <- graphmode_allocation_events(before, after, 3L, after_to_before = c(2, 3, 1))
        close(unname(events), rep(0, 6))
        fails(graphmode_allocation_events(before, after, 3L, after_to_before = c(2.1, 3, 1)), "permutation")
        changed <- graphmode_allocation_events(before, c(1, 1, 1, 1, 2, 2), 3L)
        assert(changed["births"] == 1 && changed["deaths"] == 1 && changed["downcrossings"] == 1)
    })
    test("actual Potts sweep reads updated neighbors and separates gross from net events", {
        result <- graphmode_potts_sweep(c(1L, 2L), matrix(0, 2, 2), matrix(c(0, 1, 1, 0), 2), 10)
        close(result$Z, c(2, 2))
        tape <- c(.85, .15)
        kernel$graphmode_uniform <- function(n) {out <- head(tape, n); tape <<- tail(tape, -n); out}
        result <- graphmode_potts_sweep(c(1L, 2L), matrix(0, 2, 3), matrix(0, 2, 2), 0)
        assert(result$gross["births"] == 2 && result$gross["deaths"] == 2)
        net <- graphmode_allocation_events(c(1L, 2L), result$Z, 3L)
        assert(net["births"] == 1 && net["deaths"] == 1)
    })
    test("five method sweep interfaces retain all ten experts (fixed tapes only)", {
        for (method in c("graphMoDE-W", "graphMoDE-C", "EucMoDE", "MoDE", "PottsMoDE")) {
            cfg <- basic(method, K = 10L)
            state <- graphmode_initial_state(cfg, c(1, 1, 2), sigma2 = rep(1, 10))
            out <- graphmode_sweep(state, cfg)
            assert(length(out$expert_updates) == 10L && sum(vapply(out$expert_updates,
                function(x) x$empty, logical(1))) == 8L)
            assert(identical(dim(out$state$theta), c(10L, 2L, 1L)))
            assert(sum(out$sizes) == 3 && out$state$iteration == 1L)
            if (method == "PottsMoDE") assert(is.null(out$utilities) && is.null(out$state$pi))
        }
    })
    test("posterior summaries are invariant to per-draw labels and use actual truth occupancy", {
        Z <- rbind(c(1, 1, 2), c(3, 3, 1))
        profiles <- array(0, c(2, 3, 2))
        profiles[1, , ] <- rbind(c(5, 6), c(9, 10), c(0, 0))
        profiles[2, , ] <- rbind(c(9, 10), c(0, 0), c(5, 6))
        out <- graphmode_summarize(Z, profiles)
        close(out$similarity, outer(c(1, 1, 2), c(1, 1, 2), "==") * 1)
        close(out$unit_mean, rbind(c(5, 6), c(5, 6), c(9, 10)))
        assert(out$Khat == 2 && out$Pr_Kocc_5 == 0)
        score <- graphmode_evaluate(out, c(2, 2, 5), out$unit_mean)
        assert(score$true_Kocc == 2L && score$ARI == 1 && score$coclustering_brier == 0)
        one <- graphmode_summarize(matrix(1, 2, 1), array(2, c(2, 1, 1)))
        assert(identical(dim(one$sorted_sizes), c(2L, 1L)))
        close(one$unit_mean, matrix(2))
    })
    test("configuration rejects stale versions, static epsilon innovations and missing choices", {
        cfg <- basic()
        cfg$sampler_version <- "graphmode-adaptive-square-root-2026-09-10-v1"
        fails(graphmode_revalidate_config(cfg), "Incompatible")
        cfg <- basic()
        cfg$n <- 99L
        fails(graphmode_revalidate_config(cfg), "changed outside")
        fails(graphmode_dlm(matrix(1), 0, matrix(1), matrix(1), matrix(1e-20), "static"), "G = W = NULL")
        fails(graphmode_euclidean_kernel(matrix(1:4, 2), range = NULL), "range")
        fails(graphmode_config(matrix(0, 2, 1), matrix(1), 0, matrix(1), "STGNN"), "arg")
    })
    test("both exact 121-site geometries and physical edge lengths", {
        spiral <- graphmode_simple_road("spiral")
        rings <- graphmode_simple_road("rings")
        assert(nrow(spiral$coordinates) == 121L && nrow(spiral$edges) == 120L)
        assert(nrow(rings$coordinates) == 121L && nrow(rings$edges) == 144L)
        close(spiral$road_distance[1, 121], sum(spiral$edges$length))
        # Integrate the arc independently; neighboring turns get no shortcuts.
        arc <- integrate(function(a) sqrt((.5 + 5 * a / (16 * pi))^2 + (5 / (16 * pi))^2),
                         0, 8 * pi, rel.tol = 1e-12)$value
        close(spiral$road_distance[1, 121], arc)
        close(rings$road_distance[1, 18], 3)
        close(rings$road_distance[2, 3], pi / 4)
        assert(all(rings$edges$length > 0))
        assert(sum(rings$edges$from == 1 | rings$edges$to == 1) == 8L)
    })
    test("q4 directed-neighbor median, W/C support and weight normalization", {
        road <- graphmode_simple_road("spiral")
        graph <- graphmode_dependency_graph(road$road_distance, road$ids)
        assert(all(rowSums(graph$directed) == 4L))
        close(graph$bandwidth, median(road$road_distance[graph$directed]))
        kth <- vapply(1:121, function(i) sort(road$road_distance[i, -i])[4], numeric(1))
        assert(abs(graph$bandwidth - median(kth)) > .001)
        assert(identical(graph$weight > 0, graph$connectivity > 0))
        close(mean(graph$weight[upper.tri(graph$weight) & graph$weight > 0]), 1)
    })
    test("geometric tie-breaking and Voronoi are input-permutation equivariant", {
        road <- graphmode_simple_road("rings")
        order <- c(seq(121, 1, -2), seq(120, 2, -2))
        graph <- graphmode_dependency_graph(road$road_distance, road$ids)
        permuted <- graphmode_dependency_graph(road$road_distance[order, order], road$ids[order])
        close(permuted$weight, graph$weight[order, order])
        for (distance in list(road$road_distance, road$euclidean_distance)) {
            cells <- graphmode_voronoi(distance, road$ids)
            swapped <- graphmode_voronoi(distance[order, order], road$ids[order])
            assert(all(cells$sizes > 0) && length(cells$sizes) == 5L)
            close(swapped$Z, cells$Z[order])
            close(swapped$seed_ids, cells$seed_ids)
        }
    })
    test("full-rank graph Matern matches direct inverse and Euclidean nu=1/2 exponential", {
        W <- matrix(c(0, 1, .5, 1, 0, .2, .5, .2, 0), 3)
        graph <- graphmode_graph_kernel(W)
        raw <- solve(diag(3) + graph$laplacian)
        close(graph$covariance, raw * 3 / sum(diag(raw)))
        close(mean(rowSums(graph$Phi^2)), 1)
        coordinates <- cbind(c(0, 1, 3), 0)
        euclidean <- graphmode_euclidean_kernel(coordinates, range = 2, nu = .5)
        close(euclidean$covariance, exp(-as.matrix(dist(coordinates)) / 2))
        smooth <- graphmode_euclidean_kernel(coordinates, range = 2, nu = 1)
        assert(all(eigen(smooth$covariance, symmetric = TRUE)$values > 0))
        close(mean(rowSums(smooth$Phi^2)), 1)
    })
    test("12-setting design and supplied-innovation equal-total profiles, no response draws", {
        grid <- graphmode_main_grid()
        assert(nrow(grid) == 12L && all(grid$Kfit == 10L) && all(grid$Ktrue == 5L))
        innovations <- array(sin(seq_len(5 * 168 * 3)) / 10, c(5, 168, 3))
        profile <- graphmode_normalized_profiles(innovations, amplitude = .05, innovation_variance = .01)
        close(rowSums(profile$lambda), rep(4200, 5))
        close(gmde_eta(profile$theta, profile$Fmat), profile$eta)
        close(profile$W, diag(.05^2 * .01, 3))
        assert(grepl("not SBC or forecasting", profile$role))
    })
    test("plan preflight is read-only, uncommitted sources and formal simulation stay locked", {
        cfg <- basic()
        state <- graphmode_initial_state(cfg, c(1, 2, 3), sigma2 = rep(1, 3))
        source_snapshot <- graphmode_source_identity(graphmode_root)
        destination <- file.path(tempdir(), "graphmode-preflight-must-not-create")
        make_plan <- function(role = "development-pilot", output_dir = destination) graphmode_run_plan(
            cfg, state, source_snapshot, seed = 123L, iterations = 2L, warmup = 1L, thin = 1L,
            checkpoint_every = 1L, budget_seconds = 1, output_dir = output_dir,
            registration_id = "deterministic-guard-fixture-NOT-a-run-registration",
            authorization_reference = "fixture-NOT-user-authorization", role = role,
            diagnostic_thresholds = list(fixture_only = "not scientifically calibrated"),
            provenance = as.list(setNames(rep("deterministic fixture only", 6),
                c("data_identity", "geometry_identity", "data_seed_record", "permutation_seed_record",
                  "calibration_decision", "scientific_role"))))
        plan <- make_plan()
        out <- graphmode_preflight(plan, graphmode_root)
        assert(!file.exists(destination))
        if (!source_snapshot$committed) assert(!out$ready && any(grepl("not committed", out$problems)))
        fails(graphmode_run(plan, graphmode_root), "Explicit separate run authorization")
        fails(make_plan("formal-simulation"), "formal simulation is locked")
        altered <- plan; altered$iterations <- 3L
        fails(graphmode_revalidate_plan(altered), "signature or contents changed")
        out <- graphmode_preflight(make_plan(output_dir = file.path(graphmode_root, "forbidden-results")), graphmode_root)
        assert(!out$ready && any(grepl("external", out$problems)))
        assert(!dir.exists(file.path(graphmode_root, "forbidden-results")))
        fails(graphmode_revalidate_plan(list(schema = "old-v4")), "Incompatible plan")
        state$iteration <- 1L
        fails(graphmode_revalidate_state(state, cfg), "resumed checkpoints")
    })
    test("immutable checkpoint writer refuses overwrite (tiny temporary fixture only)", {
        path <- tempfile("graphmode-deterministic-checkpoint-", fileext = ".rds")
        graphmode_save_new(list(kind = "deterministic fixture", value = 2), path)
        assert(identical(readRDS(path), list(kind = "deterministic fixture", value = 2)))
        fails(suppressWarnings(graphmode_save_new(list(value = 3), path)), "connection|file")
        assert(readRDS(path)$value == 2)
        unlink(path)
    })
    after_rng <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
        get(".Random.seed", .GlobalEnv) else NULL
    assert(identical(before_rng, after_rng))
    cat(sprintf("PASS: %d deterministic test groups; RNG unchanged; no MCMC, pilot or response simulation.\n", checks))
})
