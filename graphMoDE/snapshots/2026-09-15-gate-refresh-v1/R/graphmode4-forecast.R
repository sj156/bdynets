# Deterministic forecasting primitives. They do not fit any prefix or generate
# data. A validated, separately registered executor is still required to run.
graphmode4_forecast_schedule <- function() {
    data.frame(origin = 168:215, target = 169:216, prefix_start = 1L,
        chains = 4L, refit = "allocations-and-experts-on-full-prefix",
        hyperparameter_training_end = 168L, protocol = graphmode4_protocol)
}

graphmode4_forecast_prefix <- function(Y, origin, frozen_hyperparameters,
                                     warm_start_chain = NULL) {
    graphmode_matrix(Y, "observed prefix panel")
    origin <- gmde_scalar_integer(origin, "origin", 168L, 215L)
    if (nrow(Y) != 121L || ncol(Y) < origin)
        stop("Need all 121 units and the entire available prefix.", call. = FALSE)
    h <- frozen_hyperparameters
    if (!is.list(h) || !identical(h$protocol, graphmode4_protocol) ||
        !isTRUE(h$training_end == 168L) || !is.list(h$values) || !length(h$values))
        stop("Hyperparameters must be frozen using the initial 168 periods only.", call. = FALSE)
    signature <- h$signature; h$signature <- NULL
    if (!identical(signature, graphmode_digest(h))) stop("Frozen hyperparameters changed.", call. = FALSE)
    if (!is.null(warm_start_chain)) {
        warm_start_chain <- gmde_scalar_integer(warm_start_chain, "one warm-start chain", 1L, 4L)
        if (origin == 168L) stop("There is no preceding origin to warm-start.", call. = FALSE)
    }
    list(Y = Y[, seq_len(origin), drop = FALSE], origin = origin, target = origin + 1L,
        hyperparameter_signature = signature, warm_start_chain = warm_start_chain,
        independent_start_chains = setdiff(1:4, warm_start_chain),
        reuse_posterior_draws = FALSE, new_warmup_required = TRUE,
        refit_allocations = TRUE, refit_experts = TRUE, refit_pnar_coefficients = TRUE,
        protocol = graphmode4_protocol)
}

graphmode4_freeze_hyperparameters <- function(values, training_identity, decision_id) {
    if (!is.list(values) || !length(values) || is.null(names(values)) || anyDuplicated(names(values)) ||
        any(!nzchar(names(values)))) stop("Supply named calibrated values.", call. = FALSE)
    graphmode4_text(training_identity, "initial-168 training identity")
    graphmode4_text(decision_id, "training-only tuning decision ID")
    h <- list(values = values, training_identity = training_identity, decision_id = decision_id,
        training_end = 168L, protocol = graphmode4_protocol)
    h$signature <- graphmode_digest(h)
    h
}

graphmode4_predict_mean <- function(draws, record, Fnext) {
    graphmode4_validate_config(record)
    config <- record$core
    K <- config$K; TT <- nrow(config$Fmat); p <- ncol(config$Fmat); n <- config$n
    if (!is.numeric(Fnext) || length(Fnext) != p || any(!is.finite(Fnext)) ||
        !is.list(draws) || !length(draws)) stop("Supply Fnext and retained posterior draws.", call. = FALSE)
    dynamic <- config$dynamics == "dynamic"
    correction <- if (dynamic) as.numeric(crossprod(Fnext, config$W %*% Fnext)) / 2 else 0
    if (!is.finite(correction) || correction < 0) stop("Invalid state innovation variance.", call. = FALSE)
    conditional <- matrix(0, length(draws), n)
    for (b in seq_along(draws)) {
        d <- draws[[b]]
        Z <- gmde_validate_labels(d$Z, n, K)
        if (!is.numeric(d$theta) || !identical(dim(d$theta), c(K, TT, p)) || any(!is.finite(d$theta)))
            stop("Invalid posterior state paths.", call. = FALSE)
        terminal <- matrix(d$theta[, TT, ], K, p)
        if (dynamic) terminal <- terminal %*% t(config$G)
        mean_eta <- as.vector(terminal %*% Fnext)
        value <- if (config$family == "poisson") exp(mean_eta + correction) else mean_eta
        conditional[b, ] <- value[Z]
    }
    if (any(!is.finite(conditional)) || config$family == "poisson" && any(conditional <= 0))
        stop("Conditional mean overflow/underflow; retain as numerical failure.", call. = FALSE)
    # Allocations may differ in every draw. Equal-length chains can be pooled
    # only after graphmode4_validity passes for this particular prefix target.
    colMeans(conditional)
}

graphmode4_pnar_mean <- function(previous_counts, road_weight, beta) {
    graphmode_validate_weight(road_weight)
    n <- nrow(road_weight)
    if (!is.numeric(previous_counts) || length(previous_counts) != n ||
        any(!is.finite(previous_counts) | previous_counts < 0 | previous_counts != round(previous_counts)))
        stop("Invalid lagged counts.", call. = FALSE)
    if (!is.numeric(beta) || length(beta) != 3L || any(!is.finite(beta)) || beta[1L] <= 0 ||
        any(beta[-1L] < 0) || sum(beta[-1L]) >= 1)
        stop("PNAR requires beta0>0, beta1,beta2>=0 and beta1+beta2<1.", call. = FALSE)
    degree <- rowSums(road_weight)
    if (any(degree <= 0)) stop("PNAR row normalization needs nonisolated units.", call. = FALSE)
    row_weight <- road_weight / degree
    result <- beta[1L] + beta[2L] * as.vector(row_weight %*% previous_counts) + beta[3L] * previous_counts
    if (any(!is.finite(result))) stop("Nonfinite PNAR mean.", call. = FALSE)
    result
}

graphmode4_pnar_control <- function(beta1, beta2, initialization, burnin, decision_id) {
    graphmode_positive(beta1, "beta1", zero = TRUE)
    graphmode_positive(beta2, "beta2", zero = TRUE)
    if (beta1 + beta2 >= 1) stop("PNAR control must be stable.", call. = FALSE)
    if (!is.numeric(initialization) || length(initialization) != 121L ||
        any(!is.finite(initialization) | initialization < 0 | initialization != round(initialization)))
        stop("Supply all 121 initial counts explicitly.", call. = FALSE)
    burnin <- gmde_scalar_integer(burnin, "prospective burnin", 0L)
    graphmode4_text(decision_id, "development calibration decision")
    list(beta = c(25 * (1 - beta1 - beta2), beta1, beta2),
        initialization = initialization, burnin = burnin, decision_id = decision_id,
        Ktrue = NA_integer_, ARI = NA_real_, role = "PNAR forecast boundary control; no true classes",
        protocol = graphmode4_protocol)
}

graphmode4_forecast_score <- function(predictions, observed, origin_valid, failure_causes) {
    if (!is.matrix(predictions) || !is.numeric(predictions) || !identical(dim(predictions), c(121L, 48L)))
        stop("Predictions must keep the full 121 by 48 horizon, including unavailable columns.", call. = FALSE)
    graphmode_matrix(observed, "held-out observations")
    if (!identical(dim(observed), dim(predictions)) || !is.logical(origin_valid) ||
        length(origin_valid) != 48L || anyNA(origin_valid) || !is.character(failure_causes) ||
        length(failure_causes) != 48L || anyNA(failure_causes)) stop("Incomplete horizon audit.", call. = FALSE)
    failed <- which(!origin_valid)
    if (length(failed)) {
        first <- failed[1L]
        if (any(origin_valid[first:48]) || any(!is.na(predictions[, first:48])) ||
            !nzchar(failure_causes[first]))
            stop("Retain completed prefix predictions; failed and subsequent origins must remain unavailable.", call. = FALSE)
    }
    if (any(!is.finite(predictions[, origin_valid, drop = FALSE])) || any(nzchar(failure_causes[origin_valid])))
        stop("Valid origins require finite unrounded predictions and no failure cause.", call. = FALSE)
    complete <- !length(failed)
    error <- if (complete) predictions - observed else NULL
    list(complete = complete, MAE = if (complete) mean(abs(error)) else NA_real_,
        RMSE = if (complete) sqrt(mean(error^2)) else NA_real_,
        failure_origin = if (complete) NA_integer_ else 167L + failed[1L],
        failure_causes = failure_causes, predictions = predictions,
        origin_valid = origin_valid, protocol = graphmode4_protocol)
}

graphmode4_forecast_compare <- function(attempts, representative) {
    graphmode4_text(representative, "frozen representative")
    if (!representative %in% graphmode4_methods) stop("Unknown representative.", call. = FALSE)
    required <- c("data_id", "method", "complete", "MAE", "RMSE", "failure_origin", "seconds")
    if (!is.data.frame(attempts) || !all(required %in% names(attempts)) || !nrow(attempts) ||
        anyNA(attempts[c("data_id", "method", "complete", "seconds")]) ||
        anyDuplicated(attempts[c("data_id", "method")]) ||
        !setequal(attempts$method, c(representative, "PNAR")) || !is.logical(attempts$complete))
        stop("Need paired representative/PNAR records for every attempted dataset.", call. = FALSE)
    ids <- unique(attempts$data_id)
    if (nrow(attempts) != length(ids) * 2L || any(!is.finite(attempts$seconds) | attempts$seconds < 0))
        stop("Missing attempts or costs.", call. = FALSE)
    for (metric in c("MAE", "RMSE")) if (!is.numeric(attempts[[metric]]) ||
        any(!is.finite(attempts[[metric]][attempts$complete]) | attempts[[metric]][attempts$complete] < 0) ||
        any(!is.na(attempts[[metric]][!attempts$complete]))) stop("Incomplete replicates have unavailable full-horizon scores.", call. = FALSE)
    if (any(!is.na(attempts$failure_origin[attempts$complete])) ||
        any(!attempts$failure_origin[!attempts$complete] %in% 168:215)) stop("Record the first failed origin.", call. = FALSE)
    a <- attempts[attempts$method == representative, ]; b <- attempts[attempts$method == "PNAR", ]
    b <- b[match(a$data_id, b$data_id), ]
    joint <- a$complete & b$complete
    differences <- lapply(c("MAE", "RMSE"), function(metric) {
        d <- a[[metric]][joint] - b[[metric]][joint]
        data.frame(metric = metric, n = length(d), difference = if (length(d)) mean(d) else NA_real_,
            paired_mcse = if (length(d) > 1L) stats::sd(d) / sqrt(length(d)) else NA_real_)
    })
    list(attempts = attempts, completion_rate = tapply(attempts$complete, attempts$method, mean),
        paired = do.call(rbind, differences), common_data_ids = a$data_id[joint],
        note = "Call within a setting; datasets, not units/origins, are independent replicates.")
}
