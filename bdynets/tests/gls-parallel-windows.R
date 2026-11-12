# Exercise installed-package socket workers and preserve a two-core check budget.
if (.Platform$OS.type == "windows" && isTRUE(parallel::detectCores(logical = FALSE) >= 2L)) {
    library(bdynets)
    set.seed(918)
    graph <- gls_grid(2L)
    Y <- matrix(rpois(24L * nrow(graph$edges), 3), nrow = 24L)
    Q <- cbind(sin(seq_len(nrow(Y)) / 3), cos(seq_len(nrow(Y)) / 3))
    control <- gls_control("smoke", n_chains = 2L, n_iter = 4L, burn_mcmc = 2L,
        parallel_chains = TRUE, state_block_substeps = 1L, shrinkage_substeps = 1L,
        scale_move_steps = 1L, startup_min_static_acceptance = 0,
        startup_min_static_accepts = 0L)

    parallel_fit <- gls_fit_general(Y, graph, Q, control = control)
    pids <- vapply(parallel_fit$chains, `[[`, integer(1), "worker_pid")
    stopifnot(length(unique(pids)) == 2L, all(pids != Sys.getpid()),
        identical(parallel_fit$chain_id, rep(1:2, each = 2L)))

    disk_fit <- gls_fit_general(Y, graph, Q, control = control,
        draw_dir = tempdir(), chunk_size = 1L)
    disk_pids <- vapply(disk_fit$chains, `[[`, integer(1), "worker_pid")
    stopifnot(length(unique(disk_pids)) == 2L, all(disk_pids != Sys.getpid()),
        is.null(disk_fit$B),
        all(lengths(lapply(disk_fit$chains, function(chain) chain$draw_chunks$files)) == 2L))
    loaded <- gls_load_draws(disk_fit)
    stopifnot(identical(vapply(loaded$chains, `[[`, integer(1), "worker_pid"), disk_pids),
        identical(loaded$chain_id, parallel_fit$chain_id))

    control$parallel_chains <- FALSE
    serial_fit <- gls_fit_general(Y, graph, Q, control = control)
    for (field in c("B", "L", "beta_path", "tau", "coefficient_trace")) {
        stopifnot(isTRUE(all.equal(parallel_fit[[field]], serial_fit[[field]], tolerance = 1e-12)),
            isTRUE(all.equal(loaded[[field]], serial_fit[[field]], tolerance = 1e-12)))
    }
}
