# C8 benchmark interfaces. Sourcing this file performs no fitting.
graphmode_c8_folds <- function(training) {
    graphmode_matrix(training, "training")
    if (ncol(training) != 168L) stop("Supply only the 168 training times.")
    lapply(c(96L, 120L), function(origin) list(origin = origin,
        train = training[, seq_len(origin), drop = FALSE],
        validation = training[, origin + seq_len(48L), drop = FALSE]))
}

graphmode_c8_candidates <- function(method = c("STGNN", "PNAR")) {
    method <- match.arg(method); s <- graphmode_c8_spec()
    if (method == "PNAR") return(data.frame(candidate = paste0("p", s$pnar$p), p = s$pnar$p))
    grid <- expand.grid(K = s$stgnn$clusters, pool = s$stgnn$pool_weights)
    grid$candidate <- paste0("K", grid$K, "-pool", grid$pool)
    grid[, c("candidate", "K", "pool")]
}

graphmode_c8_select <- function(records, method = c("STGNN", "PNAR")) {
    method <- match.arg(method); grid <- graphmode_c8_candidates(method)
    epochs <- if (method == "STGNN") seq(25L, 300L, 25L) else 0L
    expected <- expand.grid(candidate = grid$candidate, origin = c(96L,120L),
        restart = if (method == "STGNN") 1:2 else 1L, epoch = epochs,
        stringsAsFactors = FALSE)
    keys <- c("candidate", "origin", "restart", "epoch")
    key <- function(d) do.call(paste, c(d[keys], sep = ":"))
    if (!all(c(keys, "sse", "n", "valid") %in% names(records))) stop("Incomplete selection schema.")
    if (anyDuplicated(key(records)) || !setequal(key(records), key(expected)))
        stop("Missing or duplicated validation cells; never drop failed fits.")
    valid <- records$valid %in% TRUE & is.finite(records$sse) & records$sse >= 0 &
        is.finite(records$n) & records$n == 121L * 48L
    # Any failed fold/restart/checkpoint disqualifies that candidate, not the
    # other candidates. The complete failure rows remain in the receipt.
    eligible <- grid$candidate[vapply(grid$candidate, function(k) all(valid[records$candidate == k]), TRUE)]
    if (!length(eligible)) stop("No complete valid candidate; no automatic refit/retry.")
    scores <- do.call(rbind, lapply(eligible, function(k) do.call(rbind, lapply(epochs, function(e) {
        x <- records[records$candidate == k & records$epoch == e, ]
        data.frame(candidate = k, epoch = e, RMSE = sqrt(sum(x$sse) / sum(x$n)))
    }))))
    best <- min(scores$RMSE); tol <- 1e-10 * max(1, abs(best))
    ties <- merge(scores[scores$RMSE <= best + tol, ], grid, by = "candidate")
    ranking <- if (method == "PNAR") order(ties$p, ties$epoch) else order(ties$K, -ties$pool, ties$epoch)
    list(selected = ties[ranking[1], , drop = FALSE], scores = scores,
        excluded = setdiff(grid$candidate, eligible), records = records,
        rule = "Pooled validation SSE; numerical ties: smaller p/K, larger pool, earlier epoch")
}

graphmode_c8_pnar_forecast <- function(training, weight, coefficients, p, horizon = 48L) {
    graphmode_matrix(training, "training"); graphmode_validate_weight(weight)
    p <- gmde_scalar_integer(p, "p", 1L); horizon <- gmde_scalar_integer(horizon, "horizon", 1L)
    if (nrow(weight) != nrow(training) || ncol(training) <= p ||
        length(coefficients) != 1L + 2L*p || any(!is.finite(coefficients)) ||
        any(coefficients < 0) || sum(coefficients[-1]) > 1 + 1e-8 || any(rowSums(weight) <= 0))
        stop("Invalid PNAR dimensions, parameters or graph.")
    w <- weight / rowSums(weight); history <- training
    out <- matrix(0, nrow(training), horizon)
    for (h in seq_len(horizon)) {
        pred <- rep(coefficients[1L], nrow(training))
        for (lag in seq_len(p)) {
            previous <- history[, ncol(history) + 1L - lag]
            pred <- pred + coefficients[1L + lag] * as.vector(w %*% previous) +
                coefficients[1L + p + lag] * previous
        }
        if (any(!is.finite(pred) | pred < 0)) stop("Invalid PNAR recursive prediction.")
        out[, h] <- pred; history <- cbind(history, pred)
    }
    out
}

graphmode_c8_pnar_arguments <- function(training, weight, p) {
    graphmode_matrix(training, "training"); graphmode_validate_weight(weight)
    if (!p %in% graphmode_c8_spec()$pnar$p || ncol(training) > 168L || ncol(training) <= p ||
        nrow(weight) != nrow(training) || any(rowSums(weight) <= 0) ||
        any(training < 0 | training != round(training))) stop("Invalid PNAR training inputs.")
    list(y = t(training), W = weight, p = p, Z = NULL, uncons = FALSE,
        init = c(.1 * max(.1, mean(training)), rep(.4/p, p), rep(.5/p, p)),
        xtol_rel = 1e-8, maxeval = 1000L)
}

graphmode_c8_pnar_receipt <- function(fitted, solver, arguments) {
    # Copy only numeric fields; never serialize the original nlopt closures.
    beta <- as.numeric(fitted$coefs[, "Estimate"])
    prediction <- graphmode_c8_pnar_forecast(t(arguments$y), arguments$W, beta, arguments$p)
    status <- as.integer(solver$status)
    if (length(status) != 1L || !status %in% 1:4) stop("PNAR optimizer did not converge; retain failure.")
    list(coefficients = beta, status = status, objective = as.numeric(solver$objective),
        iterations = as.integer(solver$iterations), prediction = prediction,
        score = as.numeric(fitted$score), ic = as.numeric(fitted$ic), p = arguments$p)
}

graphmode_c8_pnar_fit <- function(training, weight, p, author_function) {
    # Future authorized worker entry only. No caller in the preparation CLI.
    arguments <- graphmode_c8_pnar_arguments(training, weight, p)
    key <- ".graphmode_c8_solver_receipt"
    ns <- asNamespace("nloptr")
    if (exists(key, .GlobalEnv, inherits = FALSE) || inherits(get("nloptr", ns), "functionWithTrace"))
        stop("Refusing to overwrite existing solver instrumentation.")
    trace("nloptr", where = ns, exit = quote(assign(".graphmode_c8_solver_receipt",
        list(status = returnValue()$status, objective = returnValue()$objective,
             iterations = returnValue()$iterations), envir = .GlobalEnv)), print = FALSE)
    on.exit({untrace("nloptr", where = ns)
        if (exists(key, .GlobalEnv, inherits = FALSE)) rm(list = key, envir = .GlobalEnv)}, add = TRUE)
    tryCatch({
        fit <- do.call(author_function, arguments)
        graphmode_c8_pnar_receipt(fit, get(key, .GlobalEnv), arguments)
    }, error = function(e) {
        e$c8_failure <- list(p = p, solver = if (exists(key, .GlobalEnv, inherits = FALSE))
            get(key, .GlobalEnv) else NULL)
        stop(e)
    })
}
