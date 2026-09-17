# Fixed-input checks only. RNG and real PG are forbidden throughout.
if (!exists("graphmode_root", inherits = FALSE)) {
    entry <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
    graphmode_root <- dirname(dirname(dirname(normalizePath(entry, mustWork = TRUE))))
}
local({
    kernel <- new.env(parent = globalenv())
    before_kind <- RNGkind()
    before_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
    files <- c("gmde-helpers.R", "gmde-state-update.R", "graphmode-numerics.R",
        "graphmode-gate.R", "graphmode-sampler.R", "graphmode_joint_partition.R")
    for (file in files) sys.source(file.path(graphmode_root, "R", file), kernel)
    test_environment <- environment()
    parent.env(test_environment) <- kernel
    for (name in c("graphmode_normal", "graphmode_uniform", "graphmode_pg", "graphmode_gamma"))
        assign(name, function(...) stop("Unexpected RNG outside a fixed tape"), kernel)
    assert <- function(x) if (!isTRUE(x)) stop("Joint partition assertion: ",
        paste(deparse(substitute(x)), collapse = " "), call. = FALSE)
    close <- function(a, b, tol = 1e-9) assert(length(a) == length(b) &&
        identical(dim(a), dim(b)) && all(is.finite(c(a, b))) &&
        max(abs(a - b), 0) <= tol * max(1, abs(b)))
    fails <- function(code) assert(inherits(tryCatch({force(code); NULL}, error = identity), "error"))
    patch <- function(bindings, code) {
        old <- mget(names(bindings), kernel); on.exit(list2env(old, kernel), add = TRUE)
        list2env(bindings, kernel); force(code)
    }
    tape <- function(u = numeric(), z = numeric(), code) {
        used <- c(uniform = 0L, normal = 0L)
        take <- function(values, name, n) {
            idx <- used[[name]] + seq_len(n); used[[name]] <<- used[[name]] + n
            if (max(idx) > length(values)) stop("Fixed tape exhausted: ", name)
            values[idx]
        }
        value <- patch(list(graphmode_uniform = function(n) take(u, "uniform", n),
            graphmode_normal = function(n) take(z, "normal", n)), force(code))
        list(value = value, used = used)
    }
    checks <- 0L
    test <- function(name, code) {
        patch(list(graphmode_normal = function(...) stop("Unexpected RNG normal"),
            graphmode_uniform = function(...) stop("Unexpected RNG uniform"),
            graphmode_pg = function(...) stop("Unexpected real PG"),
            graphmode_gamma = function(...) stop("Unexpected RNG gamma")), force(code))
        checks <<- checks + 1L; cat("OK", checks, name, "\n")
    }
    make_config <- function(K = 3L, guidance = "class-specific", TT = 3L, p = 2L, n = 5L) {
        Phi <- cbind(seq_len(n) / n, cos(seq_len(n)))
        Phi <- Phi / sqrt(mean(rowSums(Phi^2)))
        Fmat <- if (p == 1L) matrix(1, TT, 1L) else cbind(1, seq(-1, 1, length.out = TT))
        graphmode_config(matrix((seq_len(n * TT) + 1L) %% 6L, n, TT), Fmat,
            seq(.1, .2, length.out = p), diag(.7, p), "graphMoDE-W", K = K,
            G = if (p == 1L) matrix(.8) else matrix(c(.8, -.1, .2, .7), 2L),
            W = if (p == 1L) matrix(.3) else matrix(c(.3, .04, .04, .2), 2L),
            Phi = Phi, guidance = guidance, guidance_proposal_sd = .3, rho = 4L)
    }
    config <- make_config()
    policy <- graphmode_joint_partition_policy()
    state <- graphmode_initial_state(config, c(1L, 1L, 1L, 2L, 2L),
        array(seq(-.2, .4, length.out = 18L), c(3L, 3L, 2L)),
        x = .1 * sin(seq_len(graphmode_gate_dimension(config))), v = c(-.6, .2, .8))
    state$iteration <- 17L
    path_a <- matrix(c(.1, .4, .6, -.2, .2, .3), 3L)
    path_b <- matrix(c(-.2, .3, .5, .4, -.1, .2), 3L)

    # Independent dense DLM covariance, with theta0 integrated out.
    dense_prior <- function(cfg) {
        T <- nrow(cfg$Fmat); p <- ncol(cfg$Fmat); d <- T * p
        cov <- matrix(0, d, d); means <- matrix(0, T, p)
        idx <- function(t) (t - 1L) * p + seq_len(p)
        cov[idx(1L), idx(1L)] <- cfg$G %*% cfg$C0 %*% t(cfg$G) + cfg$W
        means[1L, ] <- cfg$G %*% cfg$m0
        if (T > 1L) for (t in 2:T) {
            means[t, ] <- cfg$G %*% means[t - 1L, ]
            cov[idx(t), idx(t)] <- cfg$G %*% cov[idx(t - 1L), idx(t - 1L)] %*% t(cfg$G) + cfg$W
            for (s in seq_len(t - 1L)) {
                cov[idx(t), idx(s)] <- cfg$G %*% cov[idx(t - 1L), idx(s), drop = FALSE]
                cov[idx(s), idx(t)] <- t(cov[idx(t), idx(s), drop = FALSE])
            }
        }
        list(mean = as.numeric(t(means)), covariance = cov)
    }
    normal_log <- function(x, mean, covariance) {
        d <- x - mean
        -.5 * (length(x) * log(2 * pi) + as.numeric(determinant(covariance, TRUE)$modulus) +
            sum(d * solve(covariance, d)))
    }
    dense_proposal <- function(cfg, nodes, proposal) {
        prior <- dense_prior(cfg)
        if (!length(nodes)) return(prior)
        TT <- nrow(cfg$Fmat); p <- ncol(cfg$Fmat); H <- matrix(0, TT, TT * p)
        for (t in seq_len(TT)) H[t, (t - 1L) * p + seq_len(p)] <- cfg$Fmat[t, ]
        eta <- as.numeric(H %*% as.numeric(t(proposal$anchor)))
        w <- length(nodes) * exp(eta)
        h <- colSums(cfg$Y[nodes, , drop = FALSE]) - w + w * eta
        Q <- solve(prior$covariance); J <- Q + crossprod(H, w * H)
        list(mean = as.numeric(solve(J, Q %*% prior$mean + crossprod(H, h))), covariance = solve(J))
    }
    log_target <- function(st, cfg) {
        prior <- dense_prior(cfg); value <- -sum(st$x^2) / 2
        for (k in seq_len(cfg$K)) value <- value + normal_log(
            as.numeric(t(matrix(st$theta[k, , ], nrow(cfg$Fmat), ncol(cfg$Fmat)))),
            prior$mean, prior$covariance)
        U <- graphmode_gate_utilities(st$x, st$v, cfg)
        for (i in seq_len(cfg$n)) {
            theta <- matrix(st$theta[st$Z[i], , ], nrow(cfg$Fmat), ncol(cfg$Fmat))
            value <- value + sum(dpois(cfg$Y[i, ], exp(rowSums(theta * cfg$Fmat)), log = TRUE)) +
                U[i, st$Z[i]] - (max(U[i, ]) + log(sum(exp(U[i, ] - max(U[i, ])))))
        }
        value
    }
    test("policy, method, configuration and state guards precede random calls", {
        assert(isTRUE(graphmode_joint_partition_validate(state, config, policy)))
        for (field in c("schema", "adaptation", "split_scale", "gate_scale")) {
            bad <- policy; bad[[field]] <- "changed"
            fails(graphmode_joint_partition_step(state, config, bad))
        }
        for (field in c("method", "family", "dynamics")) {
            bad <- config; bad[[field]] <- "unsupported"
            fails(graphmode_joint_partition_step(state, bad, policy))
        }
        for (field in c("theta", "x", "v", "Z", "iteration")) {
            bad <- state; bad[[field]][1L] <- NA
            fails(graphmode_joint_partition_step(bad, config, policy))
        }
        bad <- state; bad$cache <- 1; fails(graphmode_joint_partition_step(bad, config, policy))
    })
    test("split normalization, support, complement symmetry and deterministic ties", {
        for (n in 2:7) for (same in c(FALSE, TRUE)) {
            Y <- if (same) matrix(2, n, 3L) else matrix(seq_len(n * 3L) %% 5, n, 3L)
            d <- graphmode_joint_partition_distribution(Y, rev(seq_len(n)))
            close(d$p, graphmode_joint_partition_distribution(Y, seq_len(n))$p)
            values <- vapply(seq_len(2^n - 2L), function(mask) {
                B <- which(as.logical(intToBits(mask)[seq_len(n)]))
                q <- graphmode_joint_partition_logq(d, B)
                close(q, graphmode_joint_partition_logq(d, setdiff(seq_len(n), B)))
                exp(q)
            }, numeric(1))
            close(sum(values), 1); assert(all(values > 0))
            if (same) { close(d$p, rep(.5, n)); assert(identical(d$anchors, c(1L, 2L))) }
        }
        fails(graphmode_joint_partition_distribution(config$Y, 1L))
        fails(graphmode_joint_partition_distribution(config$Y, c(1L, 1L)))
    })
    test("conditional split sampler reproduces every branch of enumerated distributions", {
        for (n in 2:5) {
            dist <- graphmode_joint_partition_distribution(matrix(seq_len(n * 3L) %% 4, n), seq_len(n))
            patterns <- t(vapply(seq_len(2^n - 2L), function(mask)
                as.logical(intToBits(mask)[seq_len(n)]), logical(n)))
            mixture <- numeric(nrow(patterns))
            for (orientation in c(FALSE, TRUE)) {
                probs <- if (orientation) 1 - dist$p else dist$p
                mass <- apply(patterns, 1L, function(bits) prod(ifelse(bits, probs, 1 - probs)))
                mass <- mass / sum(mass); mixture <- mixture + mass / 2
                for (row in seq_len(nrow(patterns))) {
                    desired <- patterns[row, ]; keep <- rep(TRUE, nrow(patterns)); values <- numeric(n)
                    for (i in seq_len(n)) {
                        probability <- sum(mass[keep & patterns[, i]]) / sum(mass[keep])
                        values[i] <- if (desired[i]) probability / 2 else (1 + probability) / 2
                        keep <- keep & patterns[, i] == desired[i]
                    }
                    draw <- tape(c(if (orientation) .25 else .75, values), code =
                        graphmode_joint_partition_draw_split(dist))
                    assert(identical(draw$value$B, which(desired)))
                    assert(draw$used[["uniform"]] == n + 1L)
                }
            }
            close(exp(apply(patterns, 1L, function(bits)
                graphmode_joint_partition_logq(dist, which(bits)))), mixture)
        }
    })
    test("DLM marginal first-state prior agrees with independent dense Gaussian", {
        prior <- graphmode_joint_partition_prior(config); dense <- dense_prior(config)
        close(graphmode_joint_partition_prior_log(path_a, prior),
            normal_log(as.numeric(t(path_a)), dense$mean, dense$covariance))
        close(tcrossprod(prior$first_root), config$G %*% config$C0 %*% t(config$G) + config$W)
    })
    test("normalized backward proposal density and mean agree with dense precision algebra", {
        for (steps in c(0L, 2L, 8L)) {
            pol <- graphmode_joint_partition_policy(newton_steps = steps)
            proposal <- graphmode_joint_partition_proposal(config, 1:3, pol)
            dense <- dense_proposal(config, 1:3, proposal)
            close(as.numeric(t(proposal$mean)), dense$mean)
            for (theta in list(path_a, path_b)) close(graphmode_joint_partition_proposal_log(theta, proposal),
                normal_log(as.numeric(t(theta)), dense$mean, dense$covariance))
            if (!steps) assert(max(abs(proposal$mean - proposal$anchor)) > .01)
        }
    })
    test("basis normal tapes recover proposal mean and entire covariance including empty expert", {
        for (nodes in list(integer(), 1:3)) {
            proposal <- graphmode_joint_partition_proposal(config, nodes, policy)
            dense <- dense_proposal(config, nodes, proposal); d <- length(dense$mean)
            zero <- tape(z = numeric(d), code = graphmode_joint_partition_draw_path(proposal))$value
            close(as.numeric(t(zero)), dense$mean)
            L <- vapply(seq_len(d), function(j) {
                z <- numeric(d); z[j] <- 1
                theta <- tape(z = z, code = graphmode_joint_partition_draw_path(proposal))$value
                as.numeric(t(theta - zero))
            }, numeric(d))
            close(tcrossprod(L), dense$covariance)
            close(graphmode_joint_partition_proposal_log(path_a, proposal),
                normal_log(as.numeric(t(path_a)), dense$mean, dense$covariance))
        }
    })
    test("proposal construction is deterministic and objective does not decline", {
        proposal <- graphmode_joint_partition_proposal(config, 1:3, policy)
        assert(identical(proposal, graphmode_joint_partition_proposal(config, 1:3, policy)))
        objective <- function(path) graphmode_joint_partition_response(path, proposal$totals,
            proposal$size, config$Fmat) + graphmode_joint_partition_prior_log(path, proposal$prior)
        assert(objective(proposal$anchor) >= objective(proposal$prior$mean) - 1e-12)
    })
    test("gate operator and adjoint include rectangular basis, correlated guidance and all variants", {
        for (guidance in c("class-specific", "shared", "none", "forced")) {
            cfg <- make_config(guidance = guidance)
            v <- switch(guidance, "class-specific" = c(-1, .1, 1.2), shared = .3, numeric())
            operator <- graphmode_joint_partition_gate_operator(v, cfg)
            x <- sin(seq_len(graphmode_gate_dimension(cfg))) / 4
            U <- matrix(cos(seq_len(cfg$n * cfg$K)), cfg$n)
            close(operator$apply(x), graphmode_gate_utilities(x, v, cfg))
            close(sum(operator$apply(x) * U), sum(x * operator$adjoint(U)))
        }
    })
    test("matrix-free gate center agrees with independent dense ridge solve", {
        operator <- graphmode_joint_partition_gate_operator(state$v, config); dim <- length(state$x)
        A <- vapply(seq_len(dim), function(j) {
            e <- numeric(dim); e[j] <- 1
            as.numeric(graphmode_gate_utilities(e, state$v, config))
        }, numeric(config$n * config$K))
        target <- matrix(-1 / config$K, config$n, config$K)
        target[cbind(seq_len(config$n), state$Z)] <- 1 - 1 / config$K
        dense <- as.numeric(solve(diag(dim) + crossprod(A), crossprod(A, log(config$K) * as.numeric(target))))
        center <- graphmode_joint_partition_gate_center(state$Z, config, operator, policy)
        close(center$x, dense); assert(center$relative_residual < 1e-9)
        short <- graphmode_joint_partition_policy(gate_steps = 1L)
        z <- state$Z; z[3L] <- 3L
        c0 <- graphmode_joint_partition_gate_center(state$Z, config, operator, short)$x
        c1 <- graphmode_joint_partition_gate_center(z, config, operator, short)$x
        close(state$x + (c1 - c0) + (c0 - c1), state$x)
    })
    test("full split MH equals independent original-observation posterior and Gaussian proposals", {
        prepared <- graphmode_joint_partition_prepare(state, config, 1L, 3L, c(2L, 3L), policy)
        split_score <- graphmode_joint_partition_evaluate(state, config, 1L, 3L, path_a, path_b, c(2L, 3L))
        old_paths <- list(matrix(state$theta[1L, , ], 3L, 2L), matrix(state$theta[3L, , ], 3L, 2L))
        new_paths <- list(path_a, path_b)
        sets_old <- list(1:3, integer()); sets_new <- list(1L, 2:3)
        qf <- -log(2 * 2) + prepared$logq; qr <- -log(3 * 2)
        for (i in 1:2) {
            df <- dense_proposal(config, sets_new[[i]], prepared$proposed[[i]])
            dr <- dense_proposal(config, sets_old[[i]], prepared$old[[i]])
            qf <- qf + normal_log(as.numeric(t(new_paths[[i]])), df$mean, df$covariance)
            qr <- qr + normal_log(as.numeric(t(old_paths[[i]])), dr$mean, dr$covariance)
        }
        close(split_score$log_q_forward, qf); close(split_score$log_q_reverse, qr)
        close(split_score$log_ratio, log_target(split_score$candidate, config) - log_target(state, config) + qr - qf)
        close(unname(split_score$components["pair_selection"]), log(2 / 3))
    })
    split_score <- graphmode_joint_partition_evaluate(state, config, 1L, 3L, path_a, path_b, c(2L, 3L))
    test("paired merge restores state and negates complete acceptance ratio", {
        merged <- graphmode_joint_partition_evaluate(split_score$candidate, config, 1L, 3L,
            matrix(state$theta[1L, , ], 3L, 2L), matrix(state$theta[3L, , ], 3L, 2L))
        close(merged$log_ratio, -split_score$log_ratio)
        close(merged$log_q_forward, split_score$log_q_reverse)
        close(merged$log_q_reverse, split_score$log_q_forward)
        close(merged$candidate$x, state$x)
        assert(identical(merged$candidate$theta, state$theta) && identical(merged$candidate$Z, state$Z))
    })
    test("empty expert density cancels and simplified W formula matches raw ratio", {
        bad <- state; bad$theta[3L, , ] <- bad$theta[3L, , ] + 3
        alt <- graphmode_joint_partition_evaluate(bad, config, 1L, 3L, path_a, path_b, c(2L, 3L))
        close(alt$log_ratio, split_score$log_ratio)
        bad$theta[3L, , ] <- 1e8
        huge <- graphmode_joint_partition_evaluate(bad, config, 1L, 3L, path_a, path_b, c(2L, 3L))
        close(huge$log_ratio, split_score$log_ratio)
        prepared <- graphmode_joint_partition_prepare(state, config, 1L, 3L, c(2L, 3L), policy)
        W <- function(theta, proposal) graphmode_joint_partition_response(theta, proposal$totals,
            proposal$size, config$Fmat) + graphmode_joint_partition_prior_log(theta, proposal$prior) -
            graphmode_joint_partition_proposal_log(theta, proposal)
        simple <- sum(split_score$components[c("white_prior", "allocation")]) +
            W(path_a, prepared$proposed[[1L]]) + W(path_b, prepared$proposed[[2L]]) -
            W(matrix(state$theta[1L, , ], 3L, 2L), prepared$old[[1L]]) + log(2 / 3) - prepared$logq
        close(simple, split_score$log_ratio)
        a <- graphmode_joint_partition_evaluate(split_score$candidate, config, 1L, 3L, path_a, path_b)
        b <- graphmode_joint_partition_evaluate(split_score$candidate, config, 1L, 3L, path_a, path_b + 2)
        close(a$log_ratio, b$log_ratio)
        huge <- graphmode_joint_partition_evaluate(split_score$candidate, config, 1L, 3L,
            path_a, matrix(1e8, 3L, 2L))
        close(a$log_ratio, huge$log_ratio)
    })
    test("gate score includes nodes outside the split union", {
        before <- graphmode_logsoftmax(graphmode_gate_utilities(state$x, state$v, config))
        after <- graphmode_logsoftmax(graphmode_gate_utilities(split_score$candidate$x, state$v, config))
        changes <- after[cbind(1:5, split_score$candidate$Z)] - before[cbind(1:5, state$Z)]
        close(unname(split_score$components["allocation"]), sum(changes))
        assert(abs(sum(changes[4:5])) > 1e-7)
    })
    test("accepted split applies all parts atomically and recomputes derived quantities", {
        u <- c(0, .75, .75, .2, .8, .4, 0)
        result <- tape(u, numeric(12L), graphmode_joint_partition_step(state, config))
        out <- result$value; assert(out$accepted && out$move == "split")
        assert(identical(result$used, c(uniform = 7L, normal = 12L)))
        assert(out$state$iteration == 17L && identical(out$state$v, state$v))
        assert(identical(out$state$theta[2L, , ], state$theta[2L, , ]))
        close(out$eta, gmde_eta(out$state$theta, config$Fmat))
        close(out$utilities, graphmode_gate_utilities(out$state$x, state$v, config))
        close(out$response, graphmode_response_loglik(config$Y, out$eta, "poisson"))
        assert(identical(out$sizes, tabulate(out$state$Z, config$K)))
    })
    test("rejected split retains byte-identical scientific state and fresh derived quantities", {
        out <- tape(c(0, .75, .75, .2, .8, .4, .999), rep(8, 12L),
            graphmode_joint_partition_step(state, config))$value
        assert(!out$accepted && out$log_ratio < log(.999))
        assert(identical(out$state, state))
        close(out$eta, gmde_eta(state$theta, config$Fmat))
        close(out$utilities, graphmode_gate_utilities(state$x, state$v, config))
    })
    test("accepted merge redraws emptied expert and preserves unaffected coordinates", {
        input <- split_score$candidate
        out <- tape(c(0, .75, 0), numeric(12L), graphmode_joint_partition_step(input, config))$value
        assert(out$accepted && out$move == "merge" && out$sizes[3L] == 0)
        assert(identical(out$state$Z, state$Z))
        assert(!identical(out$state$theta[3L, , ], input$theta[3L, , ]))
        assert(identical(out$state$theta[2L, , ], input$theta[2L, , ]))
        assert(identical(out$state$v, input$v) && out$state$iteration == input$iteration)
    })
    test("K1 and singleton split are self transitions without path or acceptance draws", {
        cfg <- make_config(K = 1L)
        st <- graphmode_initial_state(cfg, rep(1L, cfg$n))
        out <- graphmode_joint_partition_step(st, cfg)
        assert(out$move == "self" && identical(out$state, st))
        single <- state; single$Z <- c(1L, 2L, 2L, 2L, 2L)
        out <- tape(c(0, .75), code = graphmode_joint_partition_step(single, config))
        assert(out$value$move == "self" && identical(out$value$state, single))
        assert(out$used[["normal"]] == 0)
    })
    test("single-time single-coefficient binary case has reciprocal split and merge", {
        cfg <- make_config(K = 2L, TT = 1L, p = 1L, n = 2L)
        st <- graphmode_initial_state(cfg, c(1L, 1L))
        split <- graphmode_joint_partition_evaluate(st, cfg, 1L, 2L, matrix(.2), matrix(.4), B = 2L)
        merge <- graphmode_joint_partition_evaluate(split$candidate, cfg, 1L, 2L,
            matrix(st$theta[1L, , ]), matrix(st$theta[2L, , ]))
        close(merge$log_ratio, -split$log_ratio)
        close(merge$candidate$x, st$x)
    })
    test("invalid candidates, zero/full splits and bad random inputs fail explicitly", {
        for (B in list(integer(), 1:3, c(2L, 2L), 4L))
            fails(graphmode_joint_partition_evaluate(state, config, 1L, 3L, path_a, path_b, B))
        fails(graphmode_joint_partition_evaluate(state, config, 1L, 1L, path_a, path_b, 2L))
        fails(graphmode_joint_partition_evaluate(state, config, 3L, 1L, path_a, path_b))
        fails(graphmode_joint_partition_evaluate(state, config, 1L, 3L, path_a[-1L, ], path_b, 2L))
        fails(tape(c(1, .3), code = graphmode_joint_partition_step(state, config)))
        fails(tape(c(0, .75, .75, .2, .8, .4), rep(NA_real_, 12L),
            graphmode_joint_partition_step(state, config)))
        assert(identical(state$Z, c(1L, 1L, 1L, 2L, 2L)))
    })
    test("representative 2187-coordinate gate and 504-coordinate paths use bounded deterministic work", {
        TT <- 168L; n <- 121L; K <- 10L
        cfg <- graphmode_config(matrix(seq_len(n * TT) %% 4L, n, TT),
            cbind(1, sin(seq_len(TT) / 24), cos(seq_len(TT) / 24)), rep(0, 3L), diag(3L),
            "graphMoDE-W", K = K, G = diag(3L), W = diag(.05, 3L), Phi = diag(n),
            guidance_proposal_sd = .3, rho = 4L)
        st <- graphmode_initial_state(cfg, rep(1L, n))
        score <- graphmode_joint_partition_evaluate(st, cfg, 1L, 10L,
            matrix(.1, TT, 3L), matrix(.2, TT, 3L), B = seq(2L, n, 2L))
        assert(length(score$candidate$x) == 2187L && length(score$candidate$theta[1L, , ]) == 504L)
        assert(is.finite(score$log_ratio) && all(is.finite(score$gate_center_residuals)))
        assert(score$candidate$iteration == 0L)
    })
    assert(identical(RNGkind(), before_kind))
    after_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
    assert(identical(after_seed, before_seed))
    cat("PASS", checks, "fixed-input groups; RNG unchanged; no real PG or scientific run.\n")
})
