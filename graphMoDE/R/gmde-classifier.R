# Joint graph-classifier and allocation updates for the current manuscript.

gmde_row_logsumexp <- function(x) {
    x <- as.matrix(x)
    if (nrow(x) < 1L || ncol(x) < 1L || any(!is.finite(x))) {
        stop("log-sum-exp input must be a nonempty finite matrix.",
             call. = FALSE)
    }
    row_max <- apply(x, 1L, max)
    row_max + log(rowSums(exp(x - row_max)))
}

#' Softmax allocation log-likelihood from graph utilities
#'
#' @param utilities Finite `n` by `K` utility matrix.
#' @param Z Integer allocation vector.
#' @return Scalar log-likelihood.
#' @export
gmde_classifier_loglik <- function(utilities, Z) {
    utilities <- as.matrix(utilities)
    Z <- gmde_validate_labels(
        Z, nrow(utilities), ncol(utilities), "Z"
    )
    sum(
        utilities[cbind(seq_len(nrow(utilities)), Z)] -
            gmde_row_logsumexp(utilities)
    )
}

#' Joint elliptical slice update of all identifiable graph coefficients
#'
#' @param gamma Whitened `m` by `K - 1` contrast coefficient matrix.
#' @param Phi Variance-calibrated `n` by `m` graph Fourier basis.
#' @param Z Current expert allocations.
#' @param H Orthonormal `K` by `K - 1` contrast matrix.
#' @param max_bracket_steps Fail-closed limit on slice bracket shrinkage.
#' @return Updated coefficients, cached utilities, and bracket diagnostics.
#' @export
gmde_joint_ess <- function(
    gamma,
    Phi,
    Z,
    H = gmde_helmert_contrast(ncol(as.matrix(gamma)) + 1L),
    max_bracket_steps = 1000L
) {
    gamma <- as.matrix(gamma)
    Phi <- as.matrix(Phi)
    H <- as.matrix(H)
    Z <- gmde_validate_labels(Z, nrow(Phi), nrow(H), "Z")
    max_bracket_steps <- gmde_scalar_integer(
        max_bracket_steps, "max_bracket_steps", lower = 1L
    )
    K <- nrow(H)
    if (ncol(H) != K - 1L || nrow(gamma) != ncol(Phi) ||
        ncol(gamma) != K - 1L || length(Z) != nrow(Phi) ||
        any(!is.finite(c(gamma, Phi, H)))) {
        stop("Joint ESS inputs have incompatible dimensions or values.",
             call. = FALSE)
    }
    if (max(abs(crossprod(H) - diag(K - 1L))) > 1e-10 ||
        max(abs(colSums(H))) > 1e-10) {
        stop("H is not an orthonormal class-contrast matrix.",
             call. = FALSE)
    }

    F0 <- Phi %*% gamma %*% t(H)
    current_loglik <- gmde_classifier_loglik(F0, Z)
    log_slice <- current_loglik + log(stats::runif(1))

    direction <- matrix(
        stats::rnorm(length(gamma)),
        nrow = nrow(gamma),
        ncol = ncol(gamma)
    )
    F_direction <- Phi %*% direction %*% t(H)
    angle <- stats::runif(1, 0, 2 * pi)
    lower <- angle - 2 * pi
    upper <- angle

    for (step in seq_len(max_bracket_steps)) {
        candidate_utilities <- F0 * cos(angle) + F_direction * sin(angle)
        candidate_loglik <- gmde_classifier_loglik(candidate_utilities, Z)
        if (candidate_loglik >= log_slice) {
            candidate_gamma <- gamma * cos(angle) + direction * sin(angle)
            direct_utilities <- Phi %*% candidate_gamma %*% t(H)
            if (max(abs(direct_utilities - candidate_utilities)) > 1e-9) {
                stop("Cached and direct ESS utilities disagree.",
                     call. = FALSE)
            }
            return(list(
                gamma = candidate_gamma,
                utilities = candidate_utilities,
                loglik = candidate_loglik,
                log_slice = log_slice,
                bracket_evaluations = step,
                likelihood_evaluations = step + 1L,
                angle = angle
            ))
        }
        if (angle < 0) lower <- angle else upper <- angle
        angle <- stats::runif(1, lower, upper)
    }
    stop("Joint elliptical slice sampling exceeded max_bracket_steps.",
         call. = FALSE)
}

#' Vectorized allocation log weights
#'
#' Computes `Y %*% t(eta) - 1 d' + utilities`, with
#' `d[k] = sum(exp(eta[k, ]))`.
#'
#' @param Y Location-by-time count matrix.
#' @param eta Expert-by-time log-intensity matrix.
#' @param utilities Location-by-expert graph utilities.  For the graph-free
#'   model, pass a matrix whose rows are the common log mixing weights.
#' @return An `n` by `K` log-weight matrix, up to row constants.
#' @export
gmde_allocation_log_weights <- function(Y, eta, utilities) {
    Y <- as.matrix(Y)
    eta <- as.matrix(eta)
    utilities <- as.matrix(utilities)
    if (ncol(Y) != ncol(eta) || nrow(Y) != nrow(utilities) ||
        nrow(eta) != ncol(utilities) ||
        any(!is.finite(c(Y, eta, utilities))) || any(Y < 0)) {
        stop("Allocation inputs have incompatible dimensions or values.",
             call. = FALSE)
    }
    intensity <- exp(eta)
    if (any(!is.finite(intensity))) {
        stop("eta produces a non-finite expert intensity.", call. = FALSE)
    }
    sweep(Y %*% t(eta) + utilities, 2L, rowSums(intensity), "-")
}

#' Vectorized categorical allocation probabilities
#'
#' @inheritParams gmde_allocation_log_weights
#' @return A list containing log weights and normalized probabilities.
#' @export
gmde_allocation_probabilities <- function(Y, eta, utilities) {
    log_weight <- gmde_allocation_log_weights(Y, eta, utilities)
    list(log_weight = log_weight, probability = gmde_softmax_rows(log_weight))
}

gmde_poisson_binomial_threshold <- function(probability, threshold) {
    probability <- as.numeric(probability)
    threshold <- gmde_scalar_integer(
        threshold, "threshold", lower = 1L
    )
    tolerance <- 1e-15
    if (!length(probability) || any(!is.finite(probability)) ||
        any(probability < -tolerance) ||
        any(probability > 1 + tolerance)) {
        stop(
            "Poisson-binomial probabilities must be finite and in [0, 1].",
            call. = FALSE
        )
    }
    probability <- pmin(1, pmax(0, probability))
    if (threshold > length(probability)) {
        return(list(
            probability_less = 1,
            probability_at_least = 0,
            probability_zero = prod(1 - probability),
            mass_error = 0
        ))
    }

    ## States 0, ..., threshold - 1 and one absorbing threshold-or-more
    ## state.  Accumulating the upper tail directly avoids cancellation when
    ## that probability is very small.
    state <- numeric(threshold + 1L)
    state[[1L]] <- 1
    for (value in probability) {
        previous <- state
        state[[1L]] <- previous[[1L]] * (1 - value)
        if (threshold > 1L) {
            index <- 2:threshold
            state[index] <- previous[index] * (1 - value) +
                previous[index - 1L] * value
        }
        state[[threshold + 1L]] <-
            previous[[threshold + 1L]] +
            previous[[threshold]] * value
    }
    mass_error <- abs(sum(state) - 1)
    if (any(state < -tolerance) || any(state > 1 + tolerance) ||
        mass_error > 1e-12) {
        stop("Poisson-binomial threshold recursion failed its mass audit.",
             call. = FALSE)
    }
    state <- pmin(1, pmax(0, state))
    list(
        probability_less = sum(state[seq_len(threshold)]),
        probability_at_least = state[[threshold + 1L]],
        probability_zero = state[[1L]],
        mass_error = mass_error
    )
}

gmde_occupancy_indicator_variance <- function(
    probability, empty_probability, component, event
) {
    probability <- as.matrix(probability)
    empty_probability <- as.numeric(empty_probability)
    component <- as.integer(component)
    event <- match.arg(event, c("occupied", "empty"))
    if (!length(component)) return(0)
    if (length(empty_probability) != ncol(probability) ||
        any(component < 1L | component > ncol(probability))) {
        stop("Occupancy-variance inputs are incompatible.", call. = FALSE)
    }
    event_probability <- if (event == "occupied") {
        1 - empty_probability[component]
    } else empty_probability[component]
    value <- sum(event_probability * (1 - event_probability))
    if (length(component) > 1L) {
        pair <- utils::combn(component, 2L)
        for (column in seq_len(ncol(pair))) {
            first <- pair[1L, column]
            second <- pair[2L, column]
            remaining <- 1 - probability[, first] - probability[, second]
            if (any(remaining < -1e-12) || any(remaining > 1 + 1e-12)) {
                stop("Pairwise empty probability left the unit interval.",
                     call. = FALSE)
            }
            remaining <- pmin(1, pmax(0, remaining))
            log_joint_empty <- sum(log(remaining))
            joint_empty <- exp(log_joint_empty)
            covariance <- joint_empty -
                empty_probability[[first]] * empty_probability[[second]]
            value <- value + 2 * covariance
        }
    }
    if (!is.finite(value) || value < -1e-10) {
        stop("Conditional occupancy-count variance is invalid.",
             call. = FALSE)
    }
    max(0, value)
}

gmde_allocation_conditional_mechanics <- function(
    probability, Z, substantive_min
) {
    probability <- as.matrix(probability)
    n <- nrow(probability)
    K <- ncol(probability)
    Z <- gmde_validate_labels(Z, n, K, "Z")
    substantive_min <- gmde_scalar_integer(
        substantive_min, "substantive_min", lower = 1L
    )
    if (any(!is.finite(probability)) ||
        any(probability < -1e-15) || any(probability > 1 + 1e-15)) {
        stop("Allocation probabilities left the unit interval.",
             call. = FALSE)
    }
    ## Preserve the matrix dimensions while clipping only floating-point
    ## excursions within the validated tolerance.
    probability[probability < 0] <- 0
    probability[probability > 1] <- 1
    row_sum_error <- max(abs(rowSums(probability) - 1))
    if (!is.finite(row_sum_error) || row_sum_error > 1e-12) {
        stop("Allocation probabilities failed their row-sum audit.",
             call. = FALSE)
    }

    size_before <- tabulate(Z, nbins = K)
    log_empty <- colSums(log1p(-probability))
    empty <- exp(log_empty)
    threshold <- lapply(seq_len(K), function(component) {
        gmde_poisson_binomial_threshold(
            probability[, component], substantive_min
        )
    })
    probability_less <- vapply(
        threshold, `[[`, numeric(1), "probability_less"
    )
    probability_substantive <- vapply(
        threshold, `[[`, numeric(1), "probability_at_least"
    )
    probability_zero_dp <- vapply(
        threshold, `[[`, numeric(1), "probability_zero"
    )
    dp_mass_error <- max(vapply(
        threshold, `[[`, numeric(1), "mass_error"
    ))
    zero_error <- max(abs(probability_zero_dp - empty))
    if (!is.finite(zero_error) || zero_error > 1e-12 ||
        !is.finite(dp_mass_error) || dp_mass_error > 1e-12) {
        stop("Conditional allocation probabilities failed their DP audit.",
             call. = FALSE)
    }

    empty_before <- size_before == 0L
    substantive_before <- size_before >= substantive_min
    birth_probability <- ifelse(empty_before, 1 - empty, 0)
    death_probability <- ifelse(!empty_before, empty, 0)
    upcross_probability <- ifelse(
        !substantive_before, probability_substantive, 0
    )
    downcross_probability <- ifelse(
        substantive_before, probability_less, 0
    )
    expected_size <- colSums(probability)
    expected_occupied <- sum(1 - empty)
    expected_substantive <- sum(probability_substantive)
    if (any(!is.finite(c(
        expected_size, expected_occupied, expected_substantive,
        birth_probability, death_probability,
        upcross_probability, downcross_probability
    ))) || expected_occupied < 0 || expected_occupied > K ||
        expected_substantive < 0 || expected_substantive > K) {
        stop("Conditional allocation expectations are invalid.",
             call. = FALSE)
    }

    switch_probability <- 1 - probability[cbind(seq_len(n), Z)]
    component <- data.frame(
        component = seq_len(K),
        size_before = size_before,
        expected_size_after = expected_size,
        log_probability_empty_after = log_empty,
        probability_empty_after = empty,
        probability_substantive_after = probability_substantive,
        probability_nonsubstantive_after = probability_less,
        birth_probability = birth_probability,
        death_probability = death_probability,
        upcross_probability = upcross_probability,
        downcross_probability = downcross_probability,
        stringsAsFactors = FALSE
    )
    total <- c(
        expected_Kocc_after = expected_occupied,
        expected_Ksub_after = expected_substantive,
        expected_births = sum(birth_probability),
        expected_deaths = sum(death_probability),
        expected_upcrossings = sum(upcross_probability),
        expected_downcrossings = sum(downcross_probability),
        expected_node_switches = sum(switch_probability)
    )
    variance <- c(
        Kocc_after = gmde_occupancy_indicator_variance(
            probability, empty, seq_len(K), "occupied"
        ),
        births = gmde_occupancy_indicator_variance(
            probability, empty, which(empty_before), "occupied"
        ),
        deaths = gmde_occupancy_indicator_variance(
            probability, empty, which(!empty_before), "empty"
        ),
        node_switches = sum(
            switch_probability * (1 - switch_probability)
        )
    )
    list(
        component = component,
        total = total,
        variance = variance,
        row_sum_error = row_sum_error,
        dp_mass_error = dp_mass_error,
        zero_probability_error = zero_error
    )
}

gmde_observed_loglik <- function(Y, eta, utilities) {
    Y <- as.matrix(Y)
    log_weight <- gmde_allocation_log_weights(Y, eta, utilities)
    sum(
        gmde_row_logsumexp(log_weight) -
            gmde_row_logsumexp(utilities) -
            rowSums(lgamma(Y + 1))
    )
}

gmde_complete_loglik <- function(Y, eta, utilities, Z) {
    Y <- as.matrix(Y)
    eta <- as.matrix(eta)
    n <- nrow(Y)
    Z <- gmde_validate_labels(Z, n, nrow(eta), "Z")
    eta_by_location <- eta[Z, , drop = FALSE]
    count_part <- sum(
        Y * eta_by_location - exp(eta_by_location) - lgamma(Y + 1)
    )
    count_part + gmde_classifier_loglik(utilities, Z)
}

gmde_relabel_by_mean_intensity <- function(theta, gamma, Z, Fmat, H) {
    eta <- gmde_eta(theta, Fmat)
    intensity <- exp(eta)
    if (any(!is.finite(intensity))) {
        stop("Cannot relabel non-finite intensities.", call. = FALSE)
    }
    order_index <- order(rowMeans(intensity), seq_len(nrow(eta)))
    Z <- gmde_validate_labels(Z, length(Z), nrow(eta), "Z")
    old_to_new <- integer(length(order_index))
    old_to_new[order_index] <- seq_along(order_index)

    beta <- as.matrix(gamma) %*% t(H)
    beta <- beta[, order_index, drop = FALSE]
    gamma_new <- beta %*% H
    theta_new <- theta[order_index, , , drop = FALSE]
    list(
        theta = theta_new,
        gamma = gamma_new,
        beta = beta,
        Z = old_to_new[Z],
        order = order_index
    )
}
