# Deterministic physical geometries and statistical dependence. No response
# generation or fitting takes place in this file.
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

graphmode_shortest_paths <- function(n, edges) {
    distance <- matrix(Inf, n, n)
    diag(distance) <- 0
    for (j in seq_len(nrow(edges))) {
        a <- edges$from[j]
        b <- edges$to[j]
        distance[a, b] <- distance[b, a] <- min(distance[a, b], edges$length[j])
    }
    for (k in seq_len(n)) distance <- pmin(distance, outer(distance[, k], distance[k, ], "+"))
    graphmode_validate_distance(distance)
    distance
}

graphmode_simple_road <- function(geometry = c("spiral", "rings")) {
    geometry <- match.arg(geometry)
    if (geometry == "spiral") {
        angle <- 8 * pi * (0:120) / 120
        slope <- 5 / (16 * pi)
        radius <- 0.5 + slope * angle
        coordinates <- cbind(x = radius * cos(angle), y = radius * sin(angle))
        primitive <- (radius * sqrt(radius^2 + slope^2) +
            slope^2 * asinh(radius / slope)) / (2 * slope)
        edges <- data.frame(from = 1:120, to = 2:121, length = diff(primitive),
                            kind = "spiral")
    } else {
        angle <- 2 * pi * (0:7) / 8
        coordinates <- rbind(c(0, 0), do.call(rbind, lapply(1:3,
            function(radius) cbind(radius * cos(angle), radius * sin(angle)))))
        colnames(coordinates) <- c("x", "y")
        junction <- function(radius, spoke) if (radius == 0L) 1L else
            1L + (radius - 1L) * 8L + spoke + 1L
        edge_list <- list()
        # Geometric IDs: center, ring-major junctions, ring-major arc interiors,
        # then radius-major radial interiors. This rule does not use responses.
        add_segment <- function(from, to, interior, segment_length, kind) {
            ids <- nrow(coordinates) + 1:2
            coordinates <<- rbind(coordinates, interior)
            path <- c(from, ids, to)
            edge_list[[length(edge_list) + 1L]] <<- data.frame(
                from = utils::head(path, -1L), to = utils::tail(path, -1L),
                length = rep(segment_length / 3, 3), kind = kind)
        }
        for (radius in 1:3) for (spoke in 0:7) {
            interior_angle <- angle[spoke + 1L] + (1:2) * (2 * pi / 8) / 3
            add_segment(junction(radius, spoke), junction(radius, (spoke + 1L) %% 8L),
                cbind(radius * cos(interior_angle), radius * sin(interior_angle)),
                radius * 2 * pi / 8, "arc")
        }
        for (radius in 1:3) for (spoke in 0:7) {
            interior_radius <- radius - 1 + (1:2) / 3
            add_segment(junction(radius - 1L, spoke), junction(radius, spoke),
                cbind(interior_radius * cos(angle[spoke + 1L]),
                      interior_radius * sin(angle[spoke + 1L])), 1, "radial")
        }
        edges <- do.call(rbind, edge_list)
    }
    n <- nrow(coordinates)
    distance <- graphmode_shortest_paths(n, edges)
    euclidean <- as.matrix(stats::dist(coordinates))
    list(geometry = geometry, ids = seq_len(n), coordinates = coordinates,
        edges = edges, road_distance = distance, euclidean_distance = euclidean,
        protocol = graphmode_protocol)
}

graphmode_geometry_ids <- function(ids, n) {
    if (!is.numeric(ids) || length(ids) != n || any(!is.finite(ids)) || anyDuplicated(ids))
        stop("Supply unique fixed numeric geometric IDs.", call. = FALSE)
    ids
}

# Treat only roundoff-sized differences as mathematical ties. The fixed
# tolerance is geometry-only; the same convention is used before/after reorder.
graphmode_distance_order <- function(value, ids, decreasing = FALSE) {
    ordered <- order(if (decreasing) -value else value, ids)
    tolerance <- 64 * .Machine$double.eps * max(1, max(abs(value)))
    first <- 1L
    while (first <= length(ordered)) {
        last <- first
        while (last < length(ordered) &&
            abs(value[ordered[last + 1L]] - value[ordered[first]]) <= tolerance) last <- last + 1L
        span <- first:last
        ordered[span] <- ordered[span][order(ids[ordered[span]])]
        first <- last + 1L
    }
    ordered
}

graphmode_dependency_graph <- function(distance, ids, q = 4L) {
    graphmode_validate_distance(distance)
    n <- nrow(distance)
    ids <- graphmode_geometry_ids(ids, n)
    q <- gmde_scalar_integer(q, "q", 1L, n - 1L)
    directed <- matrix(FALSE, n, n)
    for (i in seq_len(n)) {
        candidates <- setdiff(seq_len(n), i)
        selected <- graphmode_distance_order(distance[i, candidates], ids[candidates])[seq_len(q)]
        directed[i, candidates[selected]] <- TRUE
    }
    bandwidth <- stats::median(distance[directed])
    value <- matrix(0, n, n)
    value[directed] <- exp(-distance[directed] / bandwidth)
    weight <- (value + t(value)) / 2
    edge_scale <- mean(weight[upper.tri(weight) & weight > 0])
    weight <- weight / edge_scale
    connectivity <- 1 * (weight > 0)
    list(weight = weight, connectivity = connectivity, directed = directed,
         bandwidth = bandwidth, edge_scale = edge_scale, q = q, ids = ids)
}

graphmode_graph_kernel <- function(weight, nu = 1, kappa = sqrt(2), tau = 1) {
    graphmode_validate_weight(weight)
    graphmode_positive(nu, "nu")
    graphmode_positive(kappa, "kappa")
    graphmode_positive(tau, "tau")
    n <- nrow(weight)
    laplacian <- diag(rowSums(weight), n) - weight
    eig <- eigen(laplacian, symmetric = TRUE)
    tolerance <- 64 * .Machine$double.eps * max(1, norm(laplacian, "I"))
    if (min(eig$values) < -tolerance) stop("Invalid Laplacian spectrum.", call. = FALSE)
    # Laplacians have exact zero modes. Only their roundoff-sized negative
    # representations are set to zero; this is not Gaussian posterior repair.
    values <- pmax(eig$values, 0)
    log_spectrum <- -nu * log(2 * nu / kappa^2 + values)
    spectrum <- exp(log_spectrum - max(log_spectrum))
    spectrum <- n * tau^2 * spectrum / sum(spectrum)
    if (any(spectrum <= 0 | !is.finite(spectrum))) stop("Kernel spectrum underflowed.", call. = FALSE)
    Phi <- sweep(eig$vectors, 2L, sqrt(spectrum), "*")
    list(Phi = Phi, covariance = tcrossprod(Phi), laplacian = laplacian,
        eigenvalues = values, spectrum = spectrum, rank = n, nu = nu,
        kappa = kappa, tau = tau, zero_tolerance = tolerance)
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

graphmode_voronoi <- function(distance, ids, K = 5L) {
    graphmode_validate_distance(distance)
    n <- nrow(distance)
    ids <- graphmode_geometry_ids(ids, n)
    K <- gmde_scalar_integer(K, "K", 1L, n)
    seeds <- which.min(ids)
    while (length(seeds) < K) {
        candidates <- setdiff(seq_len(n), seeds)
        nearest <- apply(distance[candidates, seeds, drop = FALSE], 1L, min)
        selected <- graphmode_distance_order(nearest, ids[candidates], decreasing = TRUE)[1L]
        seeds <- c(seeds, candidates[selected])
    }
    labels <- vapply(seq_len(n), function(i)
        graphmode_distance_order(distance[i, seeds], ids[seeds])[1L], integer(1))
    list(Z = labels, seed_ids = ids[seeds], sizes = tabulate(labels, K))
}

graphmode_main_grid <- function() {
    grid <- expand.grid(geometry = c("spiral", "rings"),
        truth = c("road-voronoi", "euclidean-voronoi", "uninformative"),
        signal = c("weak", "moderate"), stringsAsFactors = FALSE)
    grid$n <- 121L
    grid$T <- 168L
    grid$Ktrue <- 5L
    grid$Kfit <- 10L
    grid$protocol <- graphmode_protocol
    grid$status <- "design-only; calibration and run authorization pending"
    grid
}

# Pure transformation of supplied standard-normal innovations. This neither
# samples them nor generates counts. Full-path normalization is deliberately
# excluded from any forecasting interface.
graphmode_normalized_profiles <- function(innovations, amplitude, innovation_variance) {
    if (!identical(dim(innovations), c(5L, 168L, 3L)) || any(!is.finite(innovations)))
        stop("Main-study innovations must be a finite 5 by 168 by 3 array.", call. = FALSE)
    graphmode_positive(amplitude, "independently calibrated amplitude")
    graphmode_positive(innovation_variance, "independently calibrated innovation variance")
    Fmat <- cbind(1, sin(2 * pi * (1:168) / 24), cos(2 * pi * (1:168) / 24))
    theta <- array(0, dim(innovations))
    for (k in 1:5) {
        phase <- 2 * pi * (k - 1L) / 5
        initial <- c(0, cos(phase), sin(phase))
        for (j in 1:3) theta[k, , j] <- initial[j] +
            sqrt(innovation_variance) * cumsum(innovations[k, , j])
    }
    eta <- amplitude * gmde_eta(theta, Fmat)
    shifted <- eta - apply(eta, 1L, max)
    log_normalizer <- apply(eta, 1L, max) + log(rowMeans(exp(shifted)))
    normalized_eta <- eta + log(25) - log_normalizer
    theta <- amplitude * theta
    theta[, , 1L] <- theta[, , 1L] + log(25) - log_normalizer
    list(lambda = exp(normalized_eta), eta = normalized_eta, theta = theta,
        Fmat = Fmat, G = diag(3), W = diag(amplitude^2 * innovation_variance, 3),
        role = "full-path-normalized clustering performance truth; not SBC or forecasting")
}
