if (!exists("gmde_poisson_binomial_threshold", mode = "function")) {
    if (!file.exists(file.path(getwd(), "scripts", "load_all.R"))) {
        stop("Start R in the graphMoDE directory before running this test.",
             call. = FALSE)
    }
    source(file.path("scripts", "load_all.R"))
}

same_numeric <- function(observed, expected, tolerance = 1e-13) {
    isTRUE(all.equal(
        observed, expected, tolerance = tolerance,
        check.attributes = TRUE
    ))
}

enumerate_bernoulli_sum <- function(probability) {
    outcomes <- as.matrix(expand.grid(
        rep(list(0:1), length(probability)),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    ))
    weights <- apply(outcomes, 1L, function(outcome) {
        prod(ifelse(outcome == 1L, probability, 1 - probability))
    })
    list(sum = rowSums(outcomes), weight = weights)
}

enumerate_categorical_allocations <- function(probability, Z) {
    probability <- as.matrix(probability)
    n <- nrow(probability)
    K <- ncol(probability)
    allocations <- as.matrix(expand.grid(
        rep(list(seq_len(K)), n),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    ))
    storage.mode(allocations) <- "integer"
    weights <- apply(allocations, 1L, function(allocation) {
        prod(probability[cbind(seq_len(n), allocation)])
    })
    sizes <- t(apply(allocations, 1L, tabulate, nbins = K))
    list(
        allocations = allocations,
        weights = weights,
        sizes = sizes,
        node_switches = rowSums(allocations !=
            matrix(Z, nrow(allocations), n, byrow = TRUE))
    )
}

## The absorbing-tail Poisson-binomial recursion agrees with all 2^n
## Bernoulli outcomes, including a threshold larger than n.
probability_bernoulli <- c(0.05, 0.25, 0.5, 0.8)
enumerated_bernoulli <- enumerate_bernoulli_sum(probability_bernoulli)
for (threshold in seq_len(length(probability_bernoulli) + 1L)) {
    observed <- gmde_poisson_binomial_threshold(
        probability_bernoulli, threshold
    )
    stopifnot(
        same_numeric(
            observed$probability_less,
            sum(enumerated_bernoulli$weight[
                enumerated_bernoulli$sum < threshold
            ]),
            1e-14
        ),
        same_numeric(
            observed$probability_at_least,
            sum(enumerated_bernoulli$weight[
                enumerated_bernoulli$sum >= threshold
            ]),
            1e-14
        ),
        same_numeric(
            observed$probability_zero,
            sum(enumerated_bernoulli$weight[
                enumerated_bernoulli$sum == 0L
            ]),
            1e-14
        ),
        observed$mass_error <= 1e-14
    )
}
boundary <- gmde_poisson_binomial_threshold(c(0, 1, 0.3), 2L)
stopifnot(
    same_numeric(boundary$probability_less, 0.7),
    same_numeric(boundary$probability_at_least, 0.3),
    identical(boundary$probability_zero, 0)
)
invalid_probability <- try(
    gmde_poisson_binomial_threshold(c(0.2, NA_real_), 1L),
    silent = TRUE
)
outside_probability <- try(
    gmde_poisson_binomial_threshold(c(0.2, 1.1), 1L),
    silent = TRUE
)
stopifnot(
    inherits(invalid_probability, "try-error"),
    inherits(outside_probability, "try-error")
)

## Exhaust all K^n categorical allocations and compare every component-level
## transition probability, aggregate expectation, and reported variance.
probability <- matrix(
    c(
        0.70, 0.20, 0.10,
        0.15, 0.70, 0.15,
        0.10, 0.35, 0.55,
        0.40, 0.40, 0.20
    ),
    nrow = 4L,
    byrow = TRUE
)
Z <- c(1L, 1L, 2L, 2L)
substantive_min <- 2L
enumerated <- enumerate_categorical_allocations(probability, Z)
stopifnot(same_numeric(sum(enumerated$weights), 1, 1e-14))

observed <- gmde_allocation_conditional_mechanics(
    probability, Z, substantive_min
)
size_before <- tabulate(Z, nbins = ncol(probability))
empty_before <- size_before == 0L
substantive_before <- size_before >= substantive_min
weighted_probability <- function(indicator) {
    colSums(indicator * enumerated$weights)
}
probability_empty <- weighted_probability(enumerated$sizes == 0L)
probability_substantive <- weighted_probability(
    enumerated$sizes >= substantive_min
)
probability_nonsubstantive <- weighted_probability(
    enumerated$sizes < substantive_min
)
births <- rowSums(sweep(
    enumerated$sizes > 0L, 2L, empty_before, "&"
))
deaths <- rowSums(sweep(
    enumerated$sizes == 0L, 2L, !empty_before, "&"
))
Kocc <- rowSums(enumerated$sizes > 0L)

stopifnot(
    identical(observed$component$size_before, size_before),
    same_numeric(
        observed$component$expected_size_after,
        colSums(probability)
    ),
    same_numeric(
        observed$component$probability_empty_after,
        probability_empty
    ),
    same_numeric(
        observed$component$probability_substantive_after,
        probability_substantive
    ),
    same_numeric(
        observed$component$probability_nonsubstantive_after,
        probability_nonsubstantive
    ),
    same_numeric(
        observed$component$birth_probability,
        ifelse(empty_before, 1 - probability_empty, 0)
    ),
    same_numeric(
        observed$component$death_probability,
        ifelse(!empty_before, probability_empty, 0)
    ),
    same_numeric(
        observed$component$upcross_probability,
        ifelse(!substantive_before, probability_substantive, 0)
    ),
    same_numeric(
        observed$component$downcross_probability,
        ifelse(substantive_before, probability_nonsubstantive, 0)
    )
)

expected_total <- c(
    expected_Kocc_after = sum(enumerated$weights * Kocc),
    expected_Ksub_after = sum(enumerated$weights *
        rowSums(enumerated$sizes >= substantive_min)),
    expected_births = sum(enumerated$weights * births),
    expected_deaths = sum(enumerated$weights * deaths),
    expected_upcrossings = sum(probability_substantive[
        !substantive_before
    ]),
    expected_downcrossings = sum(probability_nonsubstantive[
        substantive_before
    ]),
    expected_node_switches = sum(enumerated$weights *
        enumerated$node_switches)
)
weighted_variance <- function(value) {
    center <- sum(enumerated$weights * value)
    sum(enumerated$weights * (value - center)^2)
}
expected_variance <- c(
    Kocc_after = weighted_variance(Kocc),
    births = weighted_variance(births),
    deaths = weighted_variance(deaths),
    node_switches = weighted_variance(enumerated$node_switches)
)
stopifnot(
    same_numeric(observed$total, expected_total),
    same_numeric(observed$variance, expected_variance),
    observed$row_sum_error <= 1e-15,
    observed$dp_mass_error <= 1e-14,
    observed$zero_probability_error <= 1e-14
)

## A paired tiny engineering run verifies that the passive calculations make
## no RNG calls and preserve all stochastic traces and terminal state exactly.
Y_mcmc <- matrix(
    c(
        0, 1, 0, 1,
        1, 0, 1, 0,
        2, 1, 1, 2,
        1, 2, 2, 1,
        0, 0, 1, 1,
        2, 2, 1, 0
    ),
    nrow = 6L,
    byrow = TRUE
)
Fmat <- cbind(1, seq_len(4L) / 4)
Phi <- cbind(
    rep(1 / sqrt(6), 6L),
    seq(-1, 1, length.out = 6L)
)
common <- list(
    Y = Y_mcmc,
    Fmat = Fmat,
    Phi = Phi,
    K = 2L,
    n_iter = 4L,
    burn = 1L,
    rho = 1,
    substantive_min = 2L,
    pg_backend = "truncated",
    pg_trunc = 20L,
    print_freq = 0L,
    seed = 919L,
    Z_init = c(1L, 1L, 1L, 2L, 2L, 2L),
    theta_init = array(0, c(2L, 4L, 2L)),
    gamma_init = matrix(c(0.1, -0.2), 2L, 1L),
    store_prediction_state = TRUE,
    store_sampler_terminal_state = TRUE
)
without <- do.call(
    run_gmde_mcmc,
    c(common, list(store_allocation_conditionals = FALSE))
)
rng_without <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
with <- do.call(
    run_gmde_mcmc,
    c(common, list(store_allocation_conditionals = TRUE))
)
rng_with <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

trajectory_fields <- c(
    "Z", "size", "mean_lambda", "lambda", "loglik",
    "observed_loglik", "occupied_experts", "substantive_experts",
    "occupied_births", "occupied_deaths", "substantive_upcrossings",
    "substantive_downcrossings", "classifier_trace",
    "mean_assignment_probability", "ari", "acc", "initialization",
    "theta_terminal", "classifier_coefficients",
    "classifier_contrasts", "prediction_mixing_probability",
    "state_accepted", "state_log_acceptance", "state_movement",
    "state_pg_shape_sum", "state_pg_shape_max", "state_rho",
    "state_acceptance_rate", "postburn_state_acceptance_rate",
    "ess_bracket_evaluations", "ess_likelihood_evaluations",
    "algorithm_exact", "sampler_terminal_state"
)
for (field in trajectory_fields) {
    if (!identical(with[[field]], without[[field]])) {
        stop("Passive diagnostic changed trajectory field: ", field,
             call. = FALSE)
    }
}
stopifnot(identical(rng_with, rng_without))

expected_diagnostic_fields <- c(
    "size_before", "size_after", "expected_size_after",
    "log_probability_empty_after", "probability_empty_after",
    "probability_substantive_after",
    "probability_nonsubstantive_after", "birth_probability",
    "death_probability", "upcross_probability",
    "downcross_probability", "expected_Kocc_after",
    "expected_Ksub_after", "expected_births", "expected_deaths",
    "expected_upcrossings", "expected_downcrossings",
    "expected_node_switches", "observed_node_switches",
    "Kocc_after_variance", "birth_variance", "death_variance",
    "node_switch_variance", "maximum_row_sum_error",
    "maximum_dp_mass_error", "maximum_zero_probability_error",
    "rng_unchanged"
)
diagnostic <- with$allocation_conditionals
stopifnot(
    identical(names(diagnostic), expected_diagnostic_fields),
    all(diagnostic$rng_unchanged),
    identical(dim(diagnostic$size_before), c(4L, 2L)),
    identical(dim(diagnostic$size_after), c(4L, 2L)),
    same_numeric(
        rowSums(diagnostic$expected_size_after), rep(6, 4L)
    ),
    identical(rowSums(diagnostic$size_after), rep(6, 4L)),
    max(diagnostic$maximum_row_sum_error) <= 1e-12,
    max(diagnostic$maximum_dp_mass_error) <= 1e-12,
    max(diagnostic$maximum_zero_probability_error) <= 1e-12,
    !without$settings$allocation_conditionals_stored,
    with$settings$allocation_conditionals_stored,
    is.null(without$allocation_conditionals)
)

## The diagnostics are intentionally limited to the independent-row GMDE
## allocation update; other models fail before doing any computation.
unsupported <- try(
    gmde_run_mixture_mcmc(
        model = "nograph",
        Y = matrix(0, 2L, 2L),
        Fmat = matrix(1, 2L, 1L),
        K = 2L,
        n_iter = 2L,
        burn = 1L,
        alpha_pi = c(1, 1),
        rho = 1,
        pg_backend = "truncated",
        store_allocation_conditionals = TRUE
    ),
    silent = TRUE
)
stopifnot(
    inherits(unsupported, "try-error"),
    grepl("only the independent-row GMDE", as.character(unsupported))
)

cat("PASS: allocation conditional exact-enumeration and RNG tests.\n")
