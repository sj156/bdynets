# r4 experiment layer. The audited graphmode_* sampler and r2 records are
# intentionally unchanged. All functions in this file are deterministic.
graphmode4_protocol <- "graphMoDE-simple-roads-2026-09-12-r4"
graphmode4_methods <- c("graphMoDE-W", "graphMoDE-C", "EucMoDE", "MoDE", "PottsMoDE")
graphmode4_tie_order <- c("EucMoDE", "graphMoDE-C", "graphMoDE-W", "MoDE", "PottsMoDE")

graphmode4_text <- function(x, name) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
        stop("Specify ", name, ".", call. = FALSE)
    x
}

graphmode4_permutation <- function(x, n) {
    if (!is.numeric(x) || length(x) != n || anyNA(x) ||
        !setequal(x, seq_len(n))) stop("Supply a complete permutation.", call. = FALSE)
    as.integer(x)
}

graphmode4_road <- function(geometry = c("intertwined-spiral", "rings")) {
    geometry <- match.arg(geometry)
    if (geometry == "rings") {
        road <- graphmode_simple_road("rings")
    } else {
        angle <- 4 * pi * (0:59) / 59
        slope <- 5 / (8 * pi)
        radius <- 0.5 + slope * angle
        arm <- cbind(x = radius * cos(angle), y = radius * sin(angle))
        primitive <- (radius * sqrt(radius^2 + slope^2) +
            slope^2 * asinh(radius / slope)) / (2 * slope)
        coordinates <- rbind(arm[60:1, ], c(0, 0), -arm)
        edges <- data.frame(from = 1:120, to = 2:121,
            length = c(rev(diff(primitive)), 0.5, 0.5, diff(primitive)),
            kind = c(rep("plus-arm-arc", 59), rep("center-line", 2),
                     rep("minus-arm-arc", 59)))
        road <- list(geometry = geometry, ids = 1:121, coordinates = coordinates,
            edges = edges, road_distance = graphmode_shortest_paths(121, edges),
            euclidean_distance = as.matrix(stats::dist(coordinates)))
    }
    road$protocol <- graphmode4_protocol
    road
}

graphmode4_validate_road <- function(road) {
    if (!is.list(road) || !identical(road$protocol, graphmode4_protocol))
        stop("An r4 geometry is required; do not relabel r2 inputs.", call. = FALSE)
    # Compare with the actual construction, not just a version string. Reordered
    # inputs must carry all matrices and edge row indices consistently.
    canonical <- graphmode4_road(road$geometry)
    index <- graphmode4_permutation(road$ids, 121L)
    for (field in c("road_distance", "euclidean_distance"))
        if (!isTRUE(all.equal(road[[field]], canonical[[field]][index, index],
                             tolerance = 1e-12, check.attributes = FALSE)))
            stop("Incoherent r4 geometry: ", field, call. = FALSE)
    if (!isTRUE(all.equal(road$coordinates, canonical$coordinates[index, ],
                         tolerance = 1e-12, check.attributes = FALSE)))
        stop("Incoherent r4 coordinates.", call. = FALSE)
    edges <- canonical$edges
    edges$from <- match(edges$from, index)
    edges$to <- match(edges$to, index)
    if (!isTRUE(all.equal(road$edges, edges, tolerance = 1e-12)))
        stop("Incoherent physical edges.", call. = FALSE)
    invisible(TRUE)
}

graphmode4_reorder <- function(road, permutation, Y = NULL) {
    graphmode4_validate_road(road)
    p <- graphmode4_permutation(permutation, 121L)
    if (!is.null(Y)) {
        graphmode_matrix(Y, "Y")
        if (nrow(Y) != 121L) stop("Y must have 121 rows.", call. = FALSE)
        Y <- Y[p, , drop = FALSE]
    }
    road$ids <- road$ids[p]
    road$coordinates <- road$coordinates[p, , drop = FALSE]
    for (field in c("road_distance", "euclidean_distance"))
        road[[field]] <- road[[field]][p, p]
    road$edges$from <- match(road$edges$from, p)
    road$edges$to <- match(road$edges$to, p)
    graphmode4_validate_road(road)
    list(road = road, Y = Y)
}

graphmode4_main_grid <- function() {
    grid <- expand.grid(geometry = c("intertwined-spiral", "rings"),
        truth = c("road-voronoi", "euclidean-voronoi", "uninformative"),
        signal = c("weak", "moderate"), stringsAsFactors = FALSE)
    grid$setting <- paste(grid$geometry, grid$truth, grid$signal, sep = "/")
    grid$n <- 121L; grid$T <- 168L; grid$Ktrue <- 5L; grid$Kfit <- 10L
    grid$protocol <- graphmode4_protocol
    grid
}

graphmode4_partition <- function(road, truth, profile_permutation,
                                uninformative_permutation = NULL) {
    graphmode4_validate_road(road)
    truth <- match.arg(truth, c("road-voronoi", "euclidean-voronoi", "uninformative"))
    profile_permutation <- graphmode4_permutation(profile_permutation, 5L)
    # Construct in geometric order so a supplied random permutation is not
    # accidentally reinterpreted when the fitting rows are reordered.
    canonical <- graphmode4_road(road$geometry)
    distance <- if (truth == "euclidean-voronoi") canonical$euclidean_distance else
        canonical$road_distance
    cells <- graphmode_voronoi(distance, canonical$ids, 5L)
    if (truth == "uninformative") {
        p <- graphmode4_permutation(uninformative_permutation, 121L)
        cells$Z <- cells$Z[p]
    } else if (!is.null(uninformative_permutation)) {
        stop("An uninformative permutation is only applicable to that truth.", call. = FALSE)
    }
    Z <- profile_permutation[cells$Z][road$ids]
    list(Z = Z, sizes = tabulate(Z, 5L), seed_ids = cells$seed_ids,
        role = "evaluation-only truth; never initialization or tuning")
}

graphmode4_profiles <- function(innovations, amplitude, innovation_variance,
                               role, ell0 = NULL) {
    role <- match.arg(role, c("clustering", "forecast"))
    TT <- if (role == "clustering") 168L else 216L
    if (!is.numeric(innovations) || !identical(dim(innovations), c(5L, TT, 3L)) ||
        any(!is.finite(innovations))) stop("Invalid supplied innovations.", call. = FALSE)
    graphmode_positive(amplitude, "independently calibrated amplitude")
    graphmode_positive(innovation_variance, "innovation_variance", zero = TRUE)
    if (role == "forecast") graphmode_positive(ell0, "prospectively fixed ell0") else
        if (!is.null(ell0)) stop("Clustering uses equal full-path totals, not ell0.", call. = FALSE)
    Fmat <- cbind(1, sin(2 * pi * seq_len(TT) / 24), cos(2 * pi * seq_len(TT) / 24))
    theta <- array(0, dim(innovations))
    for (k in 1:5) {
        phase <- 2 * pi * (k - 1L) / 5
        initial <- c(0, cos(phase), sin(phase))
        for (j in 1:3) theta[k, , j] <- amplitude * (initial[j] +
            sqrt(innovation_variance) * cumsum(innovations[k, , j]))
    }
    eta <- gmde_eta(theta, Fmat)
    intercept <- if (role == "forecast") rep(log(ell0), 5L) else {
        maximum <- apply(eta, 1L, max)
        log(25) - maximum - log(rowMeans(exp(eta - maximum)))
    }
    theta[, , 1L] <- theta[, , 1L] + intercept
    eta <- eta + intercept
    lambda <- exp(eta)
    if (any(!is.finite(lambda) | lambda <= 0)) stop("Profile overflow/underflow.", call. = FALSE)
    list(theta = theta, eta = eta, lambda = lambda, Fmat = Fmat,
        dynamics = if (innovation_variance == 0) "static" else "dynamic",
        G = if (innovation_variance == 0) NULL else diag(3),
        W = if (innovation_variance == 0) NULL else diag(amplitude^2 * innovation_variance, 3),
        role = if (role == "forecast") "prospective forecasting law" else
            "full-path-normalized clustering truth; not SBC or forecasting",
        protocol = graphmode4_protocol)
}

# Explicit bridge to the existing audited kernel. No calibration value is
# inherited from the old smoke test; no data or chain is generated here.
graphmode4_config <- function(road, Y, Fmat, method, expert, gate, calibration_id,
                             euclidean_range = NULL, range_rule = NULL,
                             potts_beta = NULL, guidance = "class-specific") {
    graphmode4_validate_road(road)
    method <- match.arg(method, graphmode4_methods)
    graphmode4_text(calibration_id, "calibration decision ID")
    graphmode_matrix(Y, "Y"); graphmode_matrix(Fmat, "Fmat")
    if (nrow(Y) != 121L || !ncol(Y) %in% 168:215)
        stop("r4 fits require 121 rows and a complete 168..215 period prefix.", call. = FALSE)
    time <- seq_len(ncol(Y))
    basis <- cbind(1, sin(2 * pi * time / 24), cos(2 * pi * time / 24))
    if (!isTRUE(all.equal(Fmat, basis, tolerance = 1e-12, check.attributes = FALSE)))
        stop("Use the fixed three-column seasonal basis on the full prefix.", call. = FALSE)
    required <- c("m0", "C0", "family", "dynamics", "G", "W", "rho", "variance_prior")
    if (!is.list(expert) || !setequal(names(expert), required) || anyDuplicated(names(expert)))
        stop("Specify all expert fields, using explicit NULL for inapplicable fields.", call. = FALSE)
    if (!is.list(gate) || !setequal(names(gate), c("guidance_prior", "guidance_proposal_sd", "max_ess_steps")) ||
        anyDuplicated(names(gate))) stop("Specify the frozen gate tuning fields.", call. = FALSE)
    graph <- graphmode_dependency_graph(road$road_distance, road$ids, 4L)
    Phi <- NULL
    if (method == "graphMoDE-W") Phi <- graphmode_graph_kernel(graph$weight)$Phi
    if (method == "graphMoDE-C") Phi <- graphmode_graph_kernel(graph$connectivity)$Phi
    if (method == "EucMoDE") {
        graphmode4_text(range_rule, "response-free geometry-only range rule")
        Phi <- graphmode_euclidean_kernel(road$coordinates, euclidean_range)$Phi
    }
    config <- do.call(graphmode_config, c(list(Y = Y, Fmat = Fmat, method = method,
        K = 10L, Phi = Phi, graph_weight = if (method == "PottsMoDE") graph$weight else NULL,
        guidance = guidance, tau = 1, s_b = 1, potts_beta = potts_beta,
        dirichlet_alpha = if (method == "MoDE") rep(0.1, 10) else NULL), expert, gate))
    record <- list(protocol = graphmode4_protocol, core = config, ids = road$ids,
        geometry_identity = graphmode_digest(road), calibration_id = calibration_id,
        euclidean_range = euclidean_range, range_rule = range_rule,
        note = "r4 experiment inputs using the unchanged r2/v2 mathematical kernel; not a run authorization")
    record$signature <- graphmode_digest(record)
    record
}

graphmode4_module_grid <- function() {
    a <- expand.grid(truth = c("class-specific", "shared"),
        fit_guidance = c("class-specific", "shared", "none"), stringsAsFactors = FALSE)
    a$module <- "A-guidance"; a$geometry <- "rings"; a$signal <- "moderate"
    wrong <- data.frame(module = "A-wrong-graph", geometry = "rings", signal = "moderate",
        truth = "road-voronoi", fit_guidance = c("class-specific", "forced", "none"))
    b <- expand.grid(truth_dynamics = c("static", "dynamic"),
        fit_dynamics = c("static", "dynamic"), fit_guidance = c("shared", "none"),
        stringsAsFactors = FALSE)
    b$module <- "B-dynamics"; b$geometry <- "rings"; b$true_guidance <- "shared-0.5"
    list(A = a, wrong_graph = wrong, B = b,
        note = "Reuse the correct-graph main fit; one separately calibrated Gaussian companion, no duplicate module C.")
}

graphmode4_categorical_truth <- function(road, structured_normals, noise_normals,
                                       uniforms, guidance) {
    graphmode4_validate_road(road)
    if (road$geometry != "rings") stop("Modules A/B use rings only.", call. = FALSE)
    if (!is.numeric(guidance) || (!identical(as.numeric(guidance), c(.1, .3, .5, .7, .9)) &&
        !identical(as.numeric(guidance), rep(.5, 5)))) stop("Use the two declared generating guidance settings.", call. = FALSE)
    for (x in list(structured_normals, noise_normals))
        if (!is.matrix(x) || !is.numeric(x) || !identical(dim(x), c(121L, 5L)) || any(!is.finite(x)))
            stop("Supply full-rank 121 by 5 standard-normal inputs in geometric order.", call. = FALSE)
    if (!is.numeric(uniforms) || length(uniforms) != 121L || any(!is.finite(uniforms) | uniforms < 0 | uniforms >= 1))
        stop("Supply 121 categorical uniforms in [0,1), in geometric order.", call. = FALSE)
    canonical <- graphmode4_road("rings")
    dep <- graphmode_dependency_graph(canonical$road_distance, canonical$ids)
    Phi <- graphmode_graph_kernel(dep$weight)$Phi
    utilities <- sweep(Phi %*% structured_normals, 2L, sqrt(guidance), "*") +
        sweep(noise_normals, 2L, sqrt(1 - guidance), "*")
    # b0=0; project the five independent class coordinates to zero-sum utilities.
    utilities <- utilities - rowMeans(utilities)
    weights <- exp(utilities - apply(utilities, 1L, max))
    probability <- weights / rowSums(weights)
    Z <- vapply(seq_len(121), function(i) {
        cumulative <- cumsum(probability[i, ]); cumulative[5L] <- 1
        which(uniforms[i] < cumulative)[1L]
    }, integer(1))
    list(Z = Z[road$ids], probability = probability[road$ids, ], utilities = utilities[road$ids, ],
        sizes = tabulate(Z, 5L), actual_Ktrue = length(unique(Z)), generating_capacity = 5L,
        role = "evaluation-only categorical truth; empty generating classes are retained")
}

graphmode4_wrong_graph <- function(record, road, permutation) {
    graphmode4_validate_config(record); graphmode4_validate_road(road)
    if (road$geometry != "rings" || record$core$method != "graphMoDE-W" ||
        !identical(record$geometry_identity, graphmode_digest(road)))
        stop("Wrong-graph module needs its original rings graphMoDE-W record.", call. = FALSE)
    p <- graphmode4_permutation(permutation, 121L)
    canonical <- graphmode4_road("rings")
    W <- graphmode_dependency_graph(canonical$road_distance, canonical$ids)$weight
    wrong <- W[p, p][road$ids, road$ids]
    args <- record$core[names(formals(graphmode_config))]
    args$Phi <- graphmode_graph_kernel(wrong)$Phi
    altered <- record
    altered$core <- do.call(graphmode_config, args)
    altered$graph_permutation <- p
    altered$wrong_graph_identity <- graphmode_digest(wrong)
    altered$role <- "module-A independent wrong graph; response and physical geometry unchanged"
    altered$signature <- NULL; altered$signature <- graphmode_digest(altered)
    altered
}
