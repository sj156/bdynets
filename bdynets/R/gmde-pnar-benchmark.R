# Migrated by function from graphMoDE/R/core_35_benchmarks.R.
# Calls the externally pinned PNAR author function; no author source is vendored.
.gmde_pnar_forecast <- function(training, weight, coefficients, p, horizon = 48L) {
    .gmde_matrix(training, "training"); .gmde_dynamic_validate_potts(weight,0,nrow(training))
    p <- .gmde_scalar(p, "p", 1L, .Machine$integer.max, TRUE); horizon <- .gmde_scalar(horizon, "horizon", 1L, .Machine$integer.max, TRUE)
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

.gmde_pnar_arguments <- function(training, weight, p) {
    .gmde_matrix(training, "training"); .gmde_dynamic_validate_potts(weight,0,nrow(training))
    if (!p %in% c(1L,2L,6L,24L) || ncol(training) > 168L || ncol(training) <= p ||
        nrow(weight) != nrow(training) || any(rowSums(weight) <= 0) ||
        any(training < 0 | training != round(training))) stop("Invalid PNAR training inputs.")
    list(y = t(training), W = weight, p = p, Z = NULL, uncons = FALSE,
        init = c(.1 * max(.1, mean(training)), rep(.4/p, p), rep(.5/p, p)),
        xtol_rel = 1e-8, maxeval = 1000L)
}

.gmde_pnar_receipt <- function(fitted, solver, arguments) {
    # Copy only numeric fields; never serialize the original nlopt closures.
    beta <- as.numeric(fitted$coefs[, "Estimate"])
    prediction <- .gmde_pnar_forecast(t(arguments$y), arguments$W, beta, arguments$p)
    status <- as.integer(solver$status)
    if (length(status) != 1L || !status %in% 1:4) stop("PNAR optimizer did not converge; retain failure.")
    list(coefficients = beta, status = status, objective = as.numeric(solver$objective),
        iterations = as.integer(solver$iterations), prediction = prediction,
        score = as.numeric(fitted$score), ic = as.numeric(fitted$ic), p = arguments$p)
}

.gmde_pnar_fit <- function(training, weight, p, author_function) {
    arguments <- .gmde_pnar_arguments(training, weight, p)
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
        .gmde_pnar_receipt(fit, get(key, .GlobalEnv), arguments)
    }, error = function(e) {
        e$c8_failure <- list(p = p, solver = if (exists(key, .GlobalEnv, inherits = FALSE))
            get(key, .GlobalEnv) else NULL)
        stop(e)
    })
}
