# Exact selected definitions from R/graphmode-geometry.R
graphmode_validate_weight <- function(weight) {
    graphmode_matrix(weight, "weight")
    if (nrow(weight) != ncol(weight) || any(weight < 0) ||
        any(diag(weight) != 0) || !isTRUE(all.equal(weight, t(weight),
            tolerance = 1e-12, check.attributes = FALSE)))
        stop("Weights must be symmetric, nonnegative, with zero diagonal.", call. = FALSE)
    invisible(TRUE)
}

graphmode_validate_distance <- function(distance) {
    graphmode_matrix(distance, "distance")
    n <- nrow(distance)
    if (n != ncol(distance) || any(diag(distance) != 0) ||
        any(distance[row(distance) != col(distance)] <= 0) ||
        !isTRUE(all.equal(distance, t(distance), tolerance = 1e-12,
                         check.attributes = FALSE)))
        stop("Distances must be symmetric with positive off-diagonal entries.", call. = FALSE)
    invisible(TRUE)
}

graphmode_geometry_ids <- function(ids, n) {
    if (!is.numeric(ids) || length(ids) != n || any(!is.finite(ids)) || anyDuplicated(ids))
        stop("Supply unique fixed numeric geometric IDs.", call. = FALSE)
    ids
}

graphmode_euclidean_kernel <- function(coordinates, range, nu = 1, tau = 1) {
    graphmode_matrix(coordinates, "coordinates")
    graphmode_positive(range, "explicit geometry-only Euclidean range")
    graphmode_positive(nu, "nu")
    graphmode_positive(tau, "tau")
    distance <- as.matrix(stats::dist(coordinates))
    graphmode_validate_distance(distance)
    z <- sqrt(2 * nu) * distance / range
    covariance <- matrix(tau^2, nrow(z), ncol(z))
    positive <- z > 0
    covariance[positive] <- tau^2 * exp((1 - nu) * log(2) - lgamma(nu) +
        nu * log(z[positive]) + log(besselK(z[positive], nu, expon.scaled = TRUE)) - z[positive])
    root <- t(gmde_chol_spd(covariance, "Euclidean Matern covariance"))
    list(Phi = root, covariance = covariance, range = range, nu = nu, tau = tau,
         rank = nrow(covariance))
}
