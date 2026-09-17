# Fixed inputs only: no scientific RNG, data generator, PG backend or chains.
if (!exists("graphmode_root", inherits = FALSE)) {
    entry <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
    graphmode_root <- dirname(dirname(dirname(normalizePath(entry, mustWork = TRUE))))
}
local({
    kernel <- new.env(parent = globalenv())
    before_kind <- RNGkind()
    before_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
    files <- c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        "graphmode_dev.R", "graphmode_gate_refresh.R", "graphmode_expert_blocks.R",
        "graphmode_gate_blocks.R", "graphmode_gate_factor_cache.R")
    for (f in files) sys.source(file.path(graphmode_root, "R", f), kernel)
    test_environment <- environment()
    parent.env(test_environment) <- kernel
    checks <- 0L
    assert <- function(x) if (!isTRUE(x)) stop("D-065 assertion: ",
        paste(deparse(substitute(x)), collapse = " "), call. = FALSE)
    fails <- function(code, pattern = NULL) {
        e <- tryCatch({ force(code); NULL }, error = identity)
        assert(inherits(e, "error"))
        if (!is.null(pattern)) assert(grepl(pattern, conditionMessage(e)))
        conditionMessage(e)
    }
    patched <- function(bindings, code) {
        old <- mget(names(bindings), kernel)
        on.exit(list2env(old, kernel), add = TRUE)
        list2env(bindings, kernel)
        force(code)
    }
    tape <- function(code, scale = .12) {
        used <- c(normal = 0L, uniform = 0L, pg = 0L)
        patched(list(graphmode_normal = function(n) {
            i <- used["normal"] + seq_len(n); used["normal"] <<- used["normal"] + n
            scale * sin(i / 5)
        }, graphmode_uniform = function(n) {
            i <- used["uniform"] + seq_len(n); used["uniform"] <<- used["uniform"] + n
            .2 + .6 * (i %% 13) / 13
        }, graphmode_pg = function(b, z) {
            used["pg"] <<- used["pg"] + length(b); rep(.4, length(b))
        }), { value <- force(code); list(value = value, used = used) })
    }
    test <- function(name, code) {
        patched(list(graphmode_normal = function(...) stop("Unexpected normal draw"),
            graphmode_uniform = function(...) stop("Unexpected uniform draw"),
            graphmode_pg = function(...) stop("Unexpected real PG"),
            graphmode_gamma = function(...) stop("Unexpected gamma draw")), force(code))
        checks <<- checks + 1L; cat("OK", checks, name, "\n")
    }
    fixture <- function(K = 3L, n = 4L, q = n, guidance = "class-specific") {
        Phi <- outer(seq_len(n), seq_len(q), function(i, j) .7^abs(i - j))
        Phi <- Phi / sqrt(mean(rowSums(Phi^2)))
        cfg <- graphmode_config(matrix(rep(1:3, length.out = n * 3L), n, 3L),
            cbind(1, c(-1, 0, 1)), c(.2, -.1), diag(2), "graphMoDE-W", K = K,
            G = diag(2), W = diag(.02, 2), Phi = Phi, guidance = guidance,
            guidance_proposal_sd = .4, rho = 4L)
        st <- graphmode_initial_state(cfg, as.integer(1L + (seq_len(n) - 1L) %% K),
            array(.1, c(K, 3L, 2L)), x = .2 * sin(seq_len(graphmode_gate_dimension(cfg))),
            v = if (cfg$guidance == "shared") .2 else seq(-.4, .7, length.out = K))
        list(config = cfg, state = st, policy = graphmode_gate_blocks_policy(cfg, "contrast"))
    }
    f <- fixture(); cfg <- f$config; st <- f$state; pol <- f$policy
    gp <- graphmode_gate_refresh_policy(4L, .4)
    ep <- graphmode_expert_blocks_policy(2L, "fixed-zero")
    same_scan <- function(f) {
        a <- tape(graphmode_gate_blocks_ess(f$state$x, f$state$v, f$state$Z, f$config, f$policy))
        b <- tape(graphmode_gate_factor_cache_ess(f$state$x, f$state$v, f$state$Z, f$config, f$policy))
        assert(identical(a$used, b$used))
        assert(identical(a$value, b$value[names(a$value)]))
        b
    }
    scientific_records <- function(out) lapply(out$records, function(r)
        r[!names(r) %in% c("ess_seconds", "guidance_seconds")])
    without_expert_timing <- function(x) {
        if (!is.list(x)) return(x)
        if (!is.null(names(x))) x <- x[!names(x) %in% c("seconds", "kernel_seconds", "observation_seconds")]
        lapply(x, without_expert_timing)
    }
    same_refresh <- function(f) {
        a <- tape(graphmode_gate_blocks_refresh(f$state$x, f$state$v, f$state$Z, f$config, gp, f$policy))
        b <- tape(graphmode_gate_factor_cache_refresh(f$state$x, f$state$v, f$state$Z, f$config, gp, f$policy))
        assert(identical(a$used, b$used))
        for (name in c("x", "v", "utilities", "counts", "ess_scans", "policy", "ess_policy"))
            assert(identical(a$value[[name]], b$value[[name]]))
        assert(identical(scientific_records(a$value), scientific_records(b$value)))
        b
    }
    test("cached utilities retain bitwise original algebra across white states", {
        ctx <- graphmode_gate_factor_cache_context()
        for (scale in c(-3, 0, .5, 4)) {
            xx <- scale * st$x
            assert(identical(ctx$utilities(xx, st$v, cfg), graphmode_gate_utilities(xx, st$v, cfg)))
        }
        assert(identical(ctx$counts(), list(enabled = TRUE, requests = 4L, builds = 1L, hits = 3L)))
    })
    test("cache is keyed on v K and guidance; changed keys reject stale reuse", {
        for (kind in c("v", "K", "guidance")) {
            ctx <- graphmode_gate_factor_cache_context(); ctx$utilities(st$x, st$v, cfg)
            ff <- if (kind == "K") fixture(4L) else if (kind == "guidance") fixture(guidance = "shared") else f
            if (kind == "v") ff$state$v[1] <- ff$state$v[1] + .1
            fails(ctx$utilities(ff$state$x, ff$state$v, ff$config), "fixed-v scan")
        }
    })
    test("factor cache does not cache Phi products intercept or noise scales", {
        ctx <- graphmode_gate_factor_cache_context(); ctx$utilities(st$x, st$v, cfg)
        changed <- cfg; changed$s_b <- 2; changed$tau <- 2; changed$Phi <- cfg$Phi * 2
        assert(identical(ctx$utilities(st$x, st$v, changed), graphmode_gate_utilities(st$x, st$v, changed)))
        assert(ctx$counts()$builds == 1L)
    })
    test("original underflow and invalid white-state protections stay active", {
        ctx <- graphmode_gate_factor_cache_context()
        fails(ctx$utilities(st$x, rep(1000, cfg$K), cfg), "underflowed")
        fails(ctx$utilities(st$x[-1], st$v, cfg), "whitened")
        assert(ctx$counts()$builds == 0L)
        ctx$utilities(st$x, st$v, cfg)
        bad <- st$x; bad[1] <- Inf
        fails(ctx$utilities(bad, st$v, cfg), "whitened")
        assert(ctx$counts()$requests == 1L)
    })
    test("class-specific fixed scans preserve every result bit and draw count", {
        for (K in c(3L, 4L, 10L)) for (scale in c(-3, .5, 4)) {
            ff <- fixture(K, n = 7L, q = 3L)
            ff$state$x <- ff$state$x * scale; ff$state$v <- ff$state$v * scale
            same_scan(ff)
        }
    })
    test("shared guidance with multiple blocks and rectangular Phi is identical", {
        for (K in c(3L, 10L)) same_scan(fixture(K, n = 7L, q = 2L, guidance = "shared"))
    })
    large <- fixture(10L, 121L, 121L)
    test("2187-dimensional scan replaces 28 factor builds with one", {
        calls <- 0L; original <- kernel$graphmode_guidance_factors
        patched(list(graphmode_guidance_factors = function(...) { calls <<- calls + 1L; original(...) }), {
            tape(graphmode_gate_blocks_ess(large$state$x, large$state$v, large$state$Z, large$config, large$policy))
            assert(calls == 28L); calls <- 0L
            b <- tape(graphmode_gate_factor_cache_ess(large$state$x, large$state$v, large$state$Z, large$config, large$policy))
            assert(calls == 1L)
            assert(identical(b$value$factor_cache, list(enabled = TRUE, requests = 28L, builds = 1L, hits = 27L)))
        })
        same_scan(large)
    })
    test("full-vector mode delegates unchanged without a factor cache", {
        ff <- f; ff$policy <- graphmode_gate_blocks_policy(cfg, "full")
        b <- same_scan(ff)
        assert(!b$value$factor_cache$enabled)
    })
    test("binary shared one-block degeneration remains exactly the original", {
        b <- same_scan(fixture(2L, guidance = "shared"))
        assert(!b$value$factor_cache$enabled)
    })
    test("invalid policy config x v and Z reject before any draws", {
        fails(graphmode_gate_factor_cache_ess(st$x, st$v, st$Z, cfg, NULL))
        changed <- pol; changed$blocks[[1]][1] <- 2L
        fails(graphmode_gate_factor_cache_ess(st$x, st$v, st$Z, cfg, changed))
        changed <- cfg; changed$Phi[1, 1] <- Inf
        fails(graphmode_gate_factor_cache_ess(st$x, st$v, st$Z, changed, pol))
        fails(graphmode_gate_factor_cache_ess(st$x[-1], st$v, st$Z, cfg, pol))
        fails(graphmode_gate_factor_cache_ess(st$x, st$v[-1], st$Z, cfg, pol))
        fails(graphmode_gate_factor_cache_ess(st$x, st$v, c(0L, 1L, 1L, 1L), cfg, pol))
    })
    test("rejected bracket candidates preserve angle stream and scan counts", {
        replay <- function(fun) {
            calls <- 0L
            patched(list(graphmode_gate_loglik = function(...) {
                calls <<- calls + 1L; if (calls %in% c(2L, 5L)) -1e9 else 0
            }), tape(fun(st$x, st$v, st$Z, cfg, pol)))
        }
        a <- replay(graphmode_gate_blocks_ess); b <- replay(graphmode_gate_factor_cache_ess)
        assert(a$value$evaluations == 4L)
        assert(identical(a$used, b$used)); assert(identical(a$value, b$value[names(a$value)]))
    })
    test("exhausted budget after first accepted block preserves original failure", {
        cc <- cfg; cc$max_ess_steps <- 1L
        patched(list(graphmode_gate_loglik = function(...) 0), {
            a <- tape(fails(graphmode_gate_blocks_ess(st$x, st$v, st$Z, cc, pol), "shared bracket budget"))
            b <- tape(fails(graphmode_gate_factor_cache_ess(st$x, st$v, st$Z, cc, pol), "shared bracket budget"))
            assert(identical(a, b))
        })
    })
    test("all-rejection exhaustion has no partial return or extra draw", {
        cc <- cfg; cc$max_ess_steps <- 3L
        replay <- function(fun) {
            calls <- 0L
            patched(list(graphmode_gate_loglik = function(...) { calls <<- calls + 1L; if (calls == 1L) 0 else -1e9 }),
                tape(fails(fun(st$x, st$v, st$Z, cc, pol), "shared bracket budget")))
        }
        assert(identical(replay(graphmode_gate_blocks_ess), replay(graphmode_gate_factor_cache_ess)))
    })
    test("m4 preserves scientific state MH decisions all counts and draw order", {
        b <- same_refresh(f)
        assert(identical(b$value$schema, graphmode_gate_factor_cache_version))
        assert(length(b$value$factor_cache) == 4L)
        assert(all(vapply(b$value$factor_cache, `[[`, integer(1), "builds") == 1L))
        assert(!identical(b$value$v, st$v))
    })
    test("new ESS invocation rebuilds factors even for identical v", {
        calls <- 0L; original <- kernel$graphmode_guidance_factors
        patched(list(graphmode_guidance_factors = function(...) { calls <<- calls + 1L; original(...) }), {
            for (i in 1:2) tape(graphmode_gate_factor_cache_ess(st$x, st$v, st$Z, cfg, pol))
            assert(calls == 2L)
        })
    })
    test("full m4 composition retains all original scientific records", {
        ff <- f; ff$policy <- graphmode_gate_blocks_policy(cfg, "full")
        b <- same_refresh(ff)
        assert(all(!vapply(b$value$factor_cache, `[[`, logical(1), "enabled")))
    })
    test("complete outer sweep preserves state expert decisions and allocation draw", {
        a <- tape(graphmode_gate_blocks_sweep(st, cfg, gp, ep, pol, TRUE))
        b <- tape(graphmode_gate_factor_cache_sweep(st, cfg, gp, ep, pol, TRUE))
        assert(identical(a$used, b$used))
        assert(identical(without_expert_timing(a$value$transition),
            without_expert_timing(b$value$transition)))
        assert(identical(a$value$gate_refresh$counts, b$value$gate_refresh$counts))
        assert(identical(a$value$gate_refresh$ess_scans, b$value$gate_refresh$ess_scans))
        assert(identical(b$value$schema, graphmode_gate_factor_cache_version))
        assert(length(b$value$gate_refresh$factor_cache) == 4L)
        assert(!b$value$run_registered && !b$value$formal_authorized)
    })
    test("empty experts retain full prior refresh and guidance MH proposals", {
        empty <- graphmode_initial_state(cfg, rep(1L, cfg$n), st$theta, x = st$x, v = st$v)
        a <- tape(graphmode_gate_blocks_sweep(empty, cfg, gp, ep, pol, TRUE))
        b <- tape(graphmode_gate_factor_cache_sweep(empty, cfg, gp, ep, pol, TRUE))
        assert(identical(a$used, b$used)); assert(identical(a$value$transition$state, b$value$transition$state))
        assert(sum(vapply(b$value$transition$expert_updates, function(e) e$counts$prior_refreshes, integer(1))) == 2L)
        assert(identical(b$value$gate_refresh$counts$proposals_by_coordinate, rep(4, 3)))
    })
    test("private contexts never replace frozen function definitions", {
        names <- c("graphmode_gate_utilities", "graphmode_guidance_factors", "graphmode_gate_blocks_ess",
            "graphmode_gate_blocks_refresh", "graphmode_gate_blocks_sweep", "graphmode_guidance_update")
        before <- mget(names, kernel)
        tape(graphmode_gate_factor_cache_sweep(st, cfg, gp, ep, pol, TRUE))
        assert(identical(before, mget(names, kernel)))
    })
    test("candidate stays outside old source globs and has no run CLI", {
        assert(!"graphmode_gate_factor_cache.R" %in% list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$"))
        cli <- readLines(file.path(graphmode_root, "scripts/graphmode-gate-factor-cache.R"))
        assert(any(grepl("Only status/check are supported", cli, fixed = TRUE)))
    })
    test("all 92 frozen entries remain byte-identical", {
        manifest <- read.table(file.path(graphmode_root, "docs/provenance/graphmode-gate-block-freeze-2026-09-16.sha256"), stringsAsFactors = FALSE)
        assert(nrow(manifest) == 92L)
        actual <- vapply(file.path(graphmode_root, manifest[[2]]), function(f)
            digest::digest(file = f, algo = "sha256", serialize = FALSE), character(1))
        assert(identical(unname(actual), manifest[[1]]))
    })
    # Bounded microtiming of independent calls with reset fixed interfaces.
    # Every call starts at the same fixture. Outputs are not fed into a chain.
    test("fixed K10 microtiming preserves the compared scientific result", {
        a <- tape(graphmode_gate_blocks_ess(large$state$x, large$state$v, large$state$Z, large$config, large$policy), scale = 1.4)
        b <- tape(graphmode_gate_factor_cache_ess(large$state$x, large$state$v, large$state$Z, large$config, large$policy), scale = 1.4)
        assert(identical(a$used, b$used)); assert(identical(a$value, b$value[names(a$value)]))
        timing <- matrix(NA_real_, 5L, 2L, dimnames = list(NULL, c("original", "cached")))
        for (round in seq_len(5L)) {
            order <- if (round %% 2L) c("original", "cached") else c("cached", "original")
            for (name in order) {
                fun <- if (name == "original") graphmode_gate_blocks_ess else graphmode_gate_factor_cache_ess
                timing[round, name] <- system.time(for (i in seq_len(40L))
                    tape(fun(large$state$x, large$state$v, large$state$Z, large$config, large$policy), scale = 1.4))[["elapsed"]]
            }
        }
        print(data.frame(round = seq_len(5L), calls = 40L, timing,
            speedup = timing[, "original"] / timing[, "cached"]), row.names = FALSE)
        cat("MICROTIMING median original/cached ratio:", median(timing[, "original"] / timing[, "cached"]), "\n")
        cat("Fixed-call microtiming only; not complete-worker performance or ESS-per-second evidence.\n")
    })
    assert(identical(before_kind, RNGkind()))
    assert(identical(before_seed, if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) .Random.seed else NULL))
    cat("PASS", checks, "fixed-input groups; RNG unchanged; no scientific run or diagnostic recomputation.\n")
})
