# Exact selected definitions from R/graphmode_c8_design.R
graphmode_c8_version <- "graphmode-irregular-learned-scales-20260924-v1"

graphmode_c8_road <- function(spacing = c("irregular", "regular")) {
    spacing <- match.arg(spacing)
    slope <- 5 / (8 * pi)
    primitive <- function(a) {
        r <- .5 + slope * a
        (r * sqrt(r^2 + slope^2) + slope^2 * asinh(r / slope)) / (2 * slope)
    }
    curve <- function(n) {
        a <- seq(0, 4 * pi, length.out = n); r <- .5 + slope * a
        arm <- cbind(r * cos(a), r * sin(a))
        list(coordinates = rbind(arm[n:1, ], c(0, 0), -arm),
             s = c(0, cumsum(c(rev(diff(primitive(a))), .5, .5, diff(primitive(a))))))
    }
    old <- curve(60L); dense <- curve(4001L); L <- tail(dense$s, 1L)
    bounds <- c(0, vapply(c(30, 50, 71, 91), function(i) mean(old$s[c(i, i + 1)]), 0), L)
    sizes <- c(30L, 20L, 21L, 20L, 30L)
    if (spacing == "regular") {
        positions <- old$s; xy <- old$coordinates
    } else {
        centers <- seq(.02, .98, length.out = 18) + .008 * sin((1:18) * 1.73)
        widths <- .0055 + .0025 * (1 + cos((1:18) * .93)) / 2
        density <- function(s) .10 + rowSums(sapply(seq_along(centers), function(j)
            exp(-.5 * ((s / L - centers[j]) / widths[j])^2)))
        positions <- unlist(lapply(1:5, function(k) {
            grid <- seq(bounds[k], bounds[k + 1], length.out = 6001)
            f <- density(grid); cdf <- c(0, cumsum((head(f, -1) + tail(f, -1)) * diff(grid) / 2))
            cdf <- cdf / tail(cdf, 1)
            j <- seq_len(sizes[k]); quantiles <- (j - .5 + .12 * sin(j * sqrt(2) + k * .47)) / sizes[k]
            approx(cdf, grid, xout = quantiles)$y
        }))
        xy <- cbind(approx(dense$s, dense$coordinates[, 1], xout = positions)$y,
                    approx(dense$s, dense$coordinates[, 2], xout = positions)$y)
    }
    colnames(xy) <- c("x", "y")
    list(version = graphmode_c8_version, spacing = spacing, ids = seq_len(121),
        coordinates = xy, s = positions, road_distance = abs(outer(positions, positions, "-")),
        physical_edges = data.frame(from = 1:120, to = 2:121, length = diff(positions)),
        partition = rep(seq_len(5L), sizes), boundaries = bounds, full_road_length = L)
}

graphmode_c8_profiles <- function(amplitude, periods = 216L, level = 25) {
    graphmode_positive(amplitude, "amplitude"); graphmode_positive(level, "level")
    periods <- gmde_scalar_integer(periods, "periods", 24L)
    phase <- 2 * pi * (0:4) / 5
    cycle <- outer(phase, 2 * pi * (1:24) / 24, function(p, t) exp(amplitude * sin(t + p)))
    intercept <- log(level) - log(rowMeans(cycle))
    Fmat <- cbind(1, sin(2 * pi * seq_len(periods) / 24), cos(2 * pi * seq_len(periods) / 24))
    theta <- cbind(intercept, amplitude * cos(phase), amplitude * sin(phase))
    # Normalization uses the prescribed deterministic cycle, not future draws.
    list(lambda = exp(theta %*% t(Fmat)), theta = theta, Fmat = Fmat,
        role = "Equal-period-mean static seasonal main scenario; dynamic drift is deferred")
}

graphmode_c8_panel_from_uniforms <- function(amplitude, uniforms, profile_order, unit_order) {
    profile_order <- graphmode4_permutation(profile_order, 5L)
    unit_order <- graphmode4_permutation(unit_order, 121L)
    stopifnot(is.matrix(uniforms), identical(dim(uniforms), c(121L, 216L)),
        all(is.finite(uniforms)), all(uniforms > 0 & uniforms < 1))
    profiles <- graphmode_c8_profiles(amplitude)
    Z <- rep(1:5, c(30, 20, 21, 20, 30))
    lambda <- profiles$lambda[profile_order[Z], , drop = FALSE]
    Y <- matrix(qpois(uniforms, lambda), 121L, 216L)
    list(training = Y[unit_order, 1:168, drop = FALSE],
        evaluation_only = list(Y = Y[unit_order, 169:216, drop = FALSE],
            Z = Z[unit_order], lambda = lambda[unit_order, ], profile_order = profile_order),
        unit_order = unit_order, Fmat = profiles$Fmat)
}

graphmode_c8_spec <- function() {
    list(version = graphmode_c8_version, run_authorized = FALSE, n = 121L,
        train = 168L, horizon = 48L, Kfit = 10L, Ktrue = 5L,
        scenarios = expand.grid(spacing = c("regular", "irregular"), amplitude = c(.08, .15),
            stringsAsFactors = FALSE),
        bayes = list(chains = 4L, iterations = 1200L, warmup = 600L, thin = 1L,
            start_occupancy = c(1L, 3L, 7L, 10L), block = 42L, gate_scans = 4L,
            start_scale_log_offsets = c(-1, -.3, .3, 1), start_q_log_offsets = c(-1, -.3, .3, 1),
            rho = 4L, guidance_proposal_sd = 2.2688962687759413,
            scale_proposal_sd = .25, log_q_proposal_sd = .15, joint = FALSE),
        prior = list(nu = 1, tau = 1, scale_log_sd = 1, neighbor_correlation = .5,
            q_shape = 2, q_rate = 20000),
        validation = list(origins = c(96L, 120L), horizon = 48L, metric = "RMSE",
            tie = "numerical ties only (1e-10 relative); lower p/K, then larger pooling weight"),
        pnar = list(p = c(1L, 2L, 6L, 24L), maxeval = 1000L, xtol_rel = 1e-8),
        stgnn = list(clusters = c(3L, 5L, 7L, 10L), pool_weights = c(.1, 1),
            window = 48L, hidden = 16L, batch = 16L, learning_rate = .001,
            weight_decay = .0001, clip = 5, max_epochs = 300L, validation_every = 25L,
            tuning_restarts = 2L, final_restarts = 4L),
        scope = "Four development panels, one realization per amplitude, paired across geometry; not independent replication",
        storage = list(draw_chunk = 100L, cumulative_checkpoints = FALSE,
            keep_all_failures = TRUE, gzip = TRUE, planned_cap_gib = 4, min_free_gib = 8),
        budget = list(bayes_jobs = 80L, pnar_validation_fits = 32L, pnar_final_fits = 4L,
            stgnn_validation_fits = 128L, stgnn_final_fits = 16L,
            proposed_bayes_worker_seconds = 2400L, proposed_neural_fit_seconds = 600L,
            proposed_pnar_fit_seconds = 300L, summary_seconds = 7200L,
            total_cap_hours = 84, measured_new_runtime = FALSE),
        thresholds = graphmode4_pilot_spec()$thresholds,
        diagnostics_extra = c("log_kernel_scale (three GP methods)", "log_q (all five MoDE methods)"))
}
