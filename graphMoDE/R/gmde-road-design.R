# Candidate road-network design for the next countDLM simulation.

countdlm_road_candidate_workflow <-
    "countdlm-central-beijing-road-candidate-2026-09-02-v2"

countdlm_road_candidate_source_sha256 <-
    "3fde9b8b64048ebb3be1e72834edd86ca0f589684fa8a0673c02db21194602ae"

countdlm_road_candidate_default_center <- c(
    longitude = 116.3975,
    latitude = 39.9087
)

countdlm_road_candidate_window_half_widths_km <- 5:15
countdlm_road_candidate_window_rule_start_km <- 8
countdlm_road_candidate_window_drop_ratio <- 0.85
countdlm_road_candidate_selection_seed <- 2026090201L
countdlm_road_candidate_grid_nx <- 5L
countdlm_road_candidate_grid_ny <- 5L
countdlm_road_candidate_per_cell <- 4L
countdlm_road_candidate_K_true <- 5L
countdlm_road_candidate_q <- 4L
countdlm_road_candidate_audit_q <- 2:8
countdlm_road_candidate_basis_m <- 40L
countdlm_road_candidate_basis_nu <- 1.5
countdlm_road_candidate_basis_length_scale <- sqrt(3) / 0.5
countdlm_road_candidate_basis_target_rms_sd <- 1.5

countdlm_road_validate_bbox <- function(bbox) {
    bbox <- as.numeric(bbox)
    if (length(bbox) != 4L || any(!is.finite(bbox)) ||
        bbox[[1L]] >= bbox[[3L]] || bbox[[2L]] >= bbox[[4L]]) {
        stop(
            "bbox must contain finite increasing xmin, ymin, xmax, ymax.",
            call. = FALSE
        )
    }
    stats::setNames(bbox, c("xmin", "ymin", "xmax", "ymax"))
}

countdlm_road_validate_center <- function(center, bbox) {
    center <- as.numeric(center)
    if (length(center) != 2L || any(!is.finite(center))) {
        stop("center must contain finite longitude and latitude.",
             call. = FALSE)
    }
    center <- stats::setNames(center, c("longitude", "latitude"))
    if (center[["longitude"]] < bbox[["xmin"]] ||
        center[["longitude"]] > bbox[["xmax"]] ||
        center[["latitude"]] < bbox[["ymin"]] ||
        center[["latitude"]] > bbox[["ymax"]]) {
        stop("center must lie inside bbox.", call. = FALSE)
    }
    center
}

countdlm_road_require_columns <- function(graph) {
    required <- c(
        "from_id", "to_id", "from_lon", "from_lat", "to_lon", "to_lat",
        "d", "d_weighted"
    )
    if (!is.data.frame(graph) || nrow(graph) < 1L ||
        !all(required %in% names(graph))) {
        stop(
            "The road graph must be a nonempty dodgr-style data frame with ",
            "from/to IDs and longitude/latitude columns.",
            call. = FALSE
        )
    }
    invisible(TRUE)
}

countdlm_road_bbox_from_half_width <- function(center, half_width_km) {
    center <- as.numeric(center)
    if (length(center) != 2L || any(!is.finite(center)) ||
        length(half_width_km) != 1L || !is.finite(half_width_km) ||
        half_width_km <= 0) {
        stop("Invalid center or square half-width.", call. = FALSE)
    }
    earth_radius_km <- 6371.0088
    km_per_latitude_degree <- earth_radius_km * pi / 180
    km_per_longitude_degree <-
        km_per_latitude_degree * cos(center[[2L]] * pi / 180)
    c(
        xmin = center[[1L]] - half_width_km / km_per_longitude_degree,
        ymin = center[[2L]] - half_width_km / km_per_latitude_degree,
        xmax = center[[1L]] + half_width_km / km_per_longitude_degree,
        ymax = center[[2L]] + half_width_km / km_per_latitude_degree
    )
}

countdlm_road_select_window <- function(
    graph,
    center = countdlm_road_candidate_default_center,
    half_widths_km = countdlm_road_candidate_window_half_widths_km,
    rule_start_km = countdlm_road_candidate_window_rule_start_km,
    drop_ratio = countdlm_road_candidate_window_drop_ratio
) {
    countdlm_road_require_columns(graph)
    center <- as.numeric(center)
    half_widths_km <- as.numeric(half_widths_km)
    if (length(center) != 2L || any(!is.finite(center)) ||
        length(half_widths_km) < 4L || any(!is.finite(half_widths_km)) ||
        any(diff(half_widths_km) <= 0) ||
        any(abs(diff(half_widths_km) - 1) > 1e-12) ||
        length(rule_start_km) != 1L || !is.finite(rule_start_km) ||
        length(drop_ratio) != 1L || !is.finite(drop_ratio) ||
        drop_ratio <= 0 || drop_ratio >= 1) {
        stop("Invalid pre-registered road-density window rule.",
             call. = FALSE)
    }

    from_id <- as.character(graph$from_id)
    to_id <- as.character(graph$to_id)
    valid <- nzchar(from_id) & nzchar(to_id) &
        is.finite(graph$from_lon) & is.finite(graph$from_lat) &
        is.finite(graph$to_lon) & is.finite(graph$to_lat) &
        is.finite(graph$d) & graph$d > 0
    from_id <- from_id[valid]
    to_id <- to_id[valid]
    first_id <- pmin(from_id, to_id)
    second_id <- pmax(from_id, to_id)
    edge_key <- paste(first_id, second_id, sep = "|")
    keep <- !duplicated(edge_key)
    midpoint_longitude <-
        (as.numeric(graph$from_lon[valid][keep]) +
             as.numeric(graph$to_lon[valid][keep])) / 2
    midpoint_latitude <-
        (as.numeric(graph$from_lat[valid][keep]) +
             as.numeric(graph$to_lat[valid][keep])) / 2
    length_km <- as.numeric(graph$d[valid][keep]) / 1000
    rm(from_id, to_id, first_id, second_id, edge_key)

    earth_radius_km <- 6371.0088
    km_per_latitude_degree <- earth_radius_km * pi / 180
    km_per_longitude_degree <-
        km_per_latitude_degree * cos(center[[2L]] * pi / 180)
    x_km <- (midpoint_longitude - center[[1L]]) *
        km_per_longitude_degree
    y_km <- (midpoint_latitude - center[[2L]]) *
        km_per_latitude_degree
    square_radius_km <- pmax(abs(x_km), abs(y_km))

    shell_density <- vapply(half_widths_km, function(outer) {
        inner <- outer - 1
        shell <- square_radius_km <= outer & square_radius_km > inner
        sum(length_km[shell]) / (4 * (outer^2 - inner^2))
    }, numeric(1L))
    previous_three_median <- vapply(seq_along(half_widths_km), function(i) {
        if (i <= 3L) return(NA_real_)
        stats::median(shell_density[(i - 3L):(i - 1L)])
    }, numeric(1L))
    density_ratio <- shell_density / previous_three_median
    eligible <- half_widths_km >= rule_start_km &
        is.finite(density_ratio) & density_ratio < drop_ratio
    if (!any(eligible)) {
        stop(
            "The frozen road-density rule found no central-window edge; ",
            "do not choose a window by eye.", call. = FALSE
        )
    }
    selected_index <- which(eligible)[[1L]]
    selected_half_width_km <- half_widths_km[[selected_index]]
    audit <- data.frame(
        half_width_km = half_widths_km,
        shell_inner_km = half_widths_km - 1,
        shell_outer_km = half_widths_km,
        shell_density_km_per_km2 = shell_density,
        previous_three_median = previous_three_median,
        density_ratio = density_ratio,
        rule_eligible = eligible,
        selected = seq_along(half_widths_km) == selected_index
    )
    list(
        bbox = countdlm_road_bbox_from_half_width(
            center, selected_half_width_km
        ),
        half_width_km = selected_half_width_km,
        audit = audit,
        rule = paste0(
            "first H >= ", format(rule_start_km),
            " km with H-shell density < ", format(drop_ratio),
            " times the median of the preceding three shell densities"
        )
    )
}

countdlm_road_vertices_in_bbox <- function(graph, bbox) {
    countdlm_road_require_columns(graph)
    bbox <- countdlm_road_validate_bbox(bbox)

    from_keep <-
        is.finite(graph$from_lon) & is.finite(graph$from_lat) &
        graph$from_lon >= bbox[["xmin"]] &
        graph$from_lon <= bbox[["xmax"]] &
        graph$from_lat >= bbox[["ymin"]] &
        graph$from_lat <= bbox[["ymax"]]
    to_keep <-
        is.finite(graph$to_lon) & is.finite(graph$to_lat) &
        graph$to_lon >= bbox[["xmin"]] &
        graph$to_lon <= bbox[["xmax"]] &
        graph$to_lat >= bbox[["ymin"]] &
        graph$to_lat <= bbox[["ymax"]]

    vertices <- rbind(
        data.frame(
            road_vertex_id = as.character(graph$from_id[from_keep]),
            longitude = as.numeric(graph$from_lon[from_keep]),
            latitude = as.numeric(graph$from_lat[from_keep]),
            stringsAsFactors = FALSE
        ),
        data.frame(
            road_vertex_id = as.character(graph$to_id[to_keep]),
            longitude = as.numeric(graph$to_lon[to_keep]),
            latitude = as.numeric(graph$to_lat[to_keep]),
            stringsAsFactors = FALSE
        )
    )
    vertices <- vertices[
        is.finite(vertices$longitude) & is.finite(vertices$latitude) &
        nzchar(vertices$road_vertex_id),
        ,
        drop = FALSE
    ]
    vertices <- vertices[order(vertices$road_vertex_id), , drop = FALSE]

    duplicated_id <- duplicated(vertices$road_vertex_id)
    if (any(duplicated_id)) {
        first_index <- match(
            vertices$road_vertex_id[duplicated_id], vertices$road_vertex_id
        )
        coordinate_error <-
            abs(vertices$longitude[duplicated_id] -
                    vertices$longitude[first_index]) > 1e-10 |
            abs(vertices$latitude[duplicated_id] -
                    vertices$latitude[first_index]) > 1e-10
        if (any(coordinate_error)) {
            stop(
                "A road vertex ID has inconsistent endpoint coordinates.",
                call. = FALSE
            )
        }
        vertices <- vertices[!duplicated_id, , drop = FALSE]
    }
    eligible_id <-
        vertices$road_vertex_id %in% as.character(graph$from_id) &
        vertices$road_vertex_id %in% as.character(graph$to_id)
    vertices <- vertices[eligible_id, , drop = FALSE]
    rownames(vertices) <- NULL
    vertices$sampling_frame_id <- seq_len(nrow(vertices))
    vertices
}

countdlm_road_assign_grid <- function(vertices, bbox, grid_nx, grid_ny) {
    bbox <- countdlm_road_validate_bbox(bbox)
    grid_nx <- gmde_scalar_integer(grid_nx, "grid_nx", lower = 1L)
    grid_ny <- gmde_scalar_integer(grid_ny, "grid_ny", lower = 1L)
    if (!is.data.frame(vertices) || nrow(vertices) < 1L ||
        !all(c("longitude", "latitude") %in% names(vertices))) {
        stop("vertices must contain longitude and latitude.", call. = FALSE)
    }
    x_breaks <- seq(
        bbox[["xmin"]], bbox[["xmax"]], length.out = grid_nx + 1L
    )
    y_breaks <- seq(
        bbox[["ymin"]], bbox[["ymax"]], length.out = grid_ny + 1L
    )
    cell_x <- findInterval(
        vertices$longitude, x_breaks, all.inside = TRUE
    )
    cell_y <- findInterval(
        vertices$latitude, y_breaks, all.inside = TRUE
    )
    cell_x <- pmin(grid_nx, pmax(1L, cell_x))
    cell_y <- pmin(grid_ny, pmax(1L, cell_y))
    vertices$cell_x <- as.integer(cell_x)
    vertices$cell_y <- as.integer(cell_y)
    vertices$cell_id <- as.integer((cell_y - 1L) * grid_nx + cell_x)
    list(vertices = vertices, x_breaks = x_breaks, y_breaks = y_breaks)
}

countdlm_with_road_seed <- function(seed, code) {
    seed <- gmde_scalar_integer(
        seed, "selection_seed", lower = 1L,
        upper = .Machine$integer.max - 1L
    )
    old_kind <- RNGkind()
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) {
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else NULL
    on.exit({
        do.call(RNGkind, as.list(old_kind))
        if (had_seed) {
            assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(
            ".Random.seed", envir = .GlobalEnv, inherits = FALSE
        )) {
            rm(".Random.seed", envir = .GlobalEnv)
        }
    }, add = TRUE)
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    set.seed(seed)
    force(code)
}

countdlm_stratified_road_sample <- function(
    vertices,
    bbox,
    grid_nx = 5L,
    grid_ny = 5L,
    per_cell = 4L,
    selection_seed = 2026090201L
) {
    per_cell <- gmde_scalar_integer(per_cell, "per_cell", lower = 1L)
    vertices <- vertices[
        order(as.character(vertices$road_vertex_id)), , drop = FALSE
    ]
    assigned <- countdlm_road_assign_grid(
        vertices, bbox, grid_nx = grid_nx, grid_ny = grid_ny
    )
    vertices <- assigned$vertices
    expected_cells <- seq_len(grid_nx * grid_ny)
    cell_count <- tabulate(vertices$cell_id, nbins = length(expected_cells))
    if (any(cell_count < per_cell)) {
        empty <- paste(expected_cells[cell_count < per_cell], collapse = ", ")
        stop(
            "The fixed sampling grid has cells with fewer than per_cell ",
            "eligible vertices: ", empty,
            ". Stop and review the registered window rule; do not change the seed.",
            call. = FALSE
        )
    }

    selected_row <- countdlm_with_road_seed(selection_seed, {
        unlist(lapply(expected_cells, function(cell) {
            rows <- which(vertices$cell_id == cell)
            rows[sample.int(length(rows), size = per_cell,
                            replace = FALSE)]
        }), use.names = FALSE)
    })
    selected <- vertices[selected_row, , drop = FALSE]
    selected <- selected[
        order(selected$cell_id, as.character(selected$road_vertex_id)),
        , drop = FALSE
    ]
    rownames(selected) <- NULL
    selected$location_id <- seq_len(nrow(selected))
    selected$cell_candidate_n <- cell_count[selected$cell_id]
    selected$inclusion_probability <- per_cell / selected$cell_candidate_n

    selected_per_cell <- tabulate(
        selected$cell_id, nbins = length(expected_cells)
    )
    if (nrow(selected) != grid_nx * grid_ny * per_cell ||
        anyDuplicated(selected$road_vertex_id) ||
        any(selected_per_cell != per_cell)) {
        stop("The stratified road sample failed its integrity checks.",
             call. = FALSE)
    }
    list(
        selected = selected,
        frame = vertices,
        cell_count = cell_count,
        x_breaks = assigned$x_breaks,
        y_breaks = assigned$y_breaks,
        per_cell = per_cell,
        sampling_method = paste0(
            per_cell,
            " road vertices sampled uniformly without replacement per ",
            "fixed local-square grid cell"
        )
    )
}

countdlm_road_snap_center <- function(vertices, center) {
    center <- as.numeric(center)
    longitude_scale <- cos(center[[2L]] * pi / 180)
    distance2 <-
        ((vertices$longitude - center[[1L]]) * longitude_scale)^2 +
        (vertices$latitude - center[[2L]])^2
    order_index <- order(distance2, vertices$road_vertex_id)
    snapped <- vertices[order_index[[1L]], , drop = FALSE]
    earth_radius_km <- 6371.0088
    dx <- (snapped$longitude - center[[1L]]) * pi / 180
    dy <- (snapped$latitude - center[[2L]]) * pi / 180
    a <- sin(dy / 2)^2 +
        cos(center[[2L]] * pi / 180) *
        cos(snapped$latitude * pi / 180) * sin(dx / 2)^2
    snapped$snap_distance_m <-
        2000 * earth_radius_km * asin(pmin(1, sqrt(a)))
    snapped
}

countdlm_road_component_labels <- function(adjacency) {
    adjacency <- as.matrix(adjacency) != 0
    if (nrow(adjacency) < 1L || nrow(adjacency) != ncol(adjacency)) {
        stop("adjacency must be a nonempty square matrix.", call. = FALSE)
    }
    labels <- integer(nrow(adjacency))
    component <- 0L
    for (start in seq_len(nrow(adjacency))) {
        if (labels[[start]] != 0L) next
        component <- component + 1L
        labels[[start]] <- component
        queue <- start
        head <- 1L
        while (head <= length(queue)) {
            vertex <- queue[[head]]
            head <- head + 1L
            neighbor <- which(adjacency[vertex, ] & labels == 0L)
            if (length(neighbor)) {
                labels[neighbor] <- component
                queue <- c(queue, neighbor)
            }
        }
    }
    labels
}

countdlm_road_order_dists <- function(
    distance,
    from_id,
    to_id,
    name = "road distances"
) {
    distance <- as.matrix(distance)
    from_id <- as.character(from_id)
    to_id <- as.character(to_id)
    if (!identical(dim(distance), c(length(from_id), length(to_id))) ||
        is.null(rownames(distance)) || is.null(colnames(distance)) ||
        anyDuplicated(rownames(distance)) ||
        anyDuplicated(colnames(distance))) {
        stop(name, " lacks the expected unique ID dimnames.",
             call. = FALSE)
    }
    row_index <- match(from_id, rownames(distance))
    column_index <- match(to_id, colnames(distance))
    if (anyNA(row_index) || anyNA(column_index)) {
        stop(name, " does not match the requested road vertex IDs.",
             call. = FALSE)
    }
    ordered <- distance[row_index, column_index, drop = FALSE]
    dimnames(ordered) <- list(from_id, to_id)
    ordered
}

countdlm_road_qnn <- function(distance_km, tie_id, q) {
    distance_km <- as.matrix(distance_km)
    n <- nrow(distance_km)
    q <- gmde_scalar_integer(q, "q", lower = 1L, upper = n - 1L)
    if (n < 2L || n != ncol(distance_km) ||
        any(!is.finite(distance_km)) || any(distance_km < 0) ||
        length(tie_id) != n || anyDuplicated(tie_id)) {
        stop("Invalid finite road-distance matrix or tie IDs.",
             call. = FALSE)
    }
    if (max(abs(distance_km - t(distance_km))) >
        1e-9 * max(1, abs(distance_km))) {
        stop("distance_km must be symmetric.", call. = FALSE)
    }
    diag(distance_km) <- 0
    nearest <- matrix(NA_integer_, nrow = n, ncol = q)
    q_distance <- numeric(n)
    directed <- matrix(0, n, n)
    for (i in seq_len(n)) {
        ordering <- order(distance_km[i, ], as.character(tie_id))
        ordering <- ordering[ordering != i]
        neighbor <- ordering[seq_len(q)]
        nearest[i, ] <- neighbor
        q_distance[[i]] <- distance_km[i, neighbor[[q]]]
    }
    h <- stats::median(q_distance)
    if (!is.finite(h) || h <= 0) {
        stop("The q-nearest road-distance bandwidth is not positive.",
             call. = FALSE)
    }
    for (i in seq_len(n)) {
        neighbor <- nearest[i, ]
        directed[i, neighbor] <- exp(-distance_km[i, neighbor] / h)
    }
    W <- (directed + t(directed)) / 2
    diag(W) <- 0
    A <- 1L * (W > 0)
    components <- countdlm_road_component_labels(A)
    degree <- rowSums(A)
    mutual <- (directed > 0) & t(directed > 0)
    directed_pairs <- sum(directed > 0)
    list(
        W = W,
        A = A,
        q = q,
        h_nn_km = h,
        q_distance_km = q_distance,
        components = max(components),
        isolates = sum(degree == 0L),
        degree = degree,
        mutual_directed_fraction = if (directed_pairs > 0L) {
            sum(mutual) / directed_pairs
        } else NA_real_
    )
}

countdlm_road_fixed_qnn <- function(
    distance_km,
    tie_id,
    q = 4L,
    audit_q = 2:8
) {
    q <- gmde_scalar_integer(q, "q", lower = 1L,
                             upper = nrow(distance_km) - 1L)
    audit_q <- sort(unique(c(as.integer(audit_q), q)))
    if (anyNA(audit_q) || any(audit_q < 1L) ||
        any(audit_q >= nrow(distance_km))) {
        stop("audit_q must contain valid positive q values.", call. = FALSE)
    }
    fits <- lapply(audit_q, function(q) {
        countdlm_road_qnn(distance_km, tie_id, q)
    })
    names(fits) <- as.character(audit_q)
    audit <- do.call(rbind, lapply(fits, function(fit) {
        data.frame(
            q = fit$q,
            h_nn_km = fit$h_nn_km,
            edges = sum(upper.tri(fit$A) & fit$A > 0),
            components = fit$components,
            isolates = fit$isolates,
            degree_min = min(fit$degree),
            degree_median = stats::median(fit$degree),
            degree_max = max(fit$degree),
            mutual_directed_fraction = fit$mutual_directed_fraction
        )
    }))
    graph <- fits[[as.character(q)]]
    if (graph$components != 1L || graph$isolates != 0L) {
        stop(
            "The fixed sample fails the pre-registered q=", q,
            " connectivity audit. The site seed and q were not changed.",
            call. = FALSE
        )
    }
    list(graph = graph, audit = audit)
}

countdlm_road_equal_groups <- function(distance_km, tie_id, K = 5L) {
    distance_km <- as.numeric(distance_km)
    K <- gmde_scalar_integer(K, "K", lower = 2L)
    n <- length(distance_km)
    if (n < K || n %% K != 0L || any(!is.finite(distance_km)) ||
        any(distance_km < 0) || length(tie_id) != n ||
        anyDuplicated(tie_id)) {
        stop("Equal road-distance groups require valid distances and n %% K = 0.",
             call. = FALSE)
    }
    ordering <- order(distance_km, as.character(tie_id))
    labels <- integer(n)
    labels[ordering] <- rep(seq_len(K), each = n / K)
    labels
}

countdlm_road_haversine_km <- function(longitude, latitude, center) {
    longitude <- as.numeric(longitude) * pi / 180
    latitude <- as.numeric(latitude) * pi / 180
    longitude0 <- as.numeric(center[[1L]]) * pi / 180
    latitude0 <- as.numeric(center[[2L]]) * pi / 180
    a <- sin((latitude - latitude0) / 2)^2 +
        cos(latitude0) * cos(latitude) *
        sin((longitude - longitude0) / 2)^2
    2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}

countdlm_road_ari <- function(first, second) {
    first <- as.integer(factor(first))
    second <- as.integer(factor(second))
    if (length(first) != length(second) || length(first) < 2L ||
        anyNA(first) || anyNA(second)) {
        stop("ARI inputs must be complete vectors of equal length.",
             call. = FALSE)
    }
    choose_two <- function(x) x * (x - 1) / 2
    contingency <- table(first, second)
    index <- sum(choose_two(contingency))
    row_sum <- sum(choose_two(rowSums(contingency)))
    column_sum <- sum(choose_two(colSums(contingency)))
    total <- choose_two(length(first))
    expected <- row_sum * column_sum / total
    maximum <- (row_sum + column_sum) / 2
    if (abs(maximum - expected) < .Machine$double.eps) {
        return(if (identical(first, second)) 1 else 0)
    }
    (index - expected) / (maximum - expected)
}

countdlm_road_graph_diagnostics <- function(
    W,
    distance_km,
    labels
) {
    W <- as.matrix(W)
    A <- W > 0
    upper <- upper.tri(W) & A
    same <- outer(labels, labels, `==`)
    edge_distance <- distance_km[upper]
    edge_weight <- W[upper]
    degree <- rowSums(A)
    group_components <- vapply(sort(unique(labels)), function(group) {
        index <- which(labels == group)
        max(countdlm_road_component_labels(A[index, index, drop = FALSE]))
    }, integer(1L))
    list(
        edges = sum(upper),
        density = 2 * sum(upper) / (nrow(W) * (nrow(W) - 1)),
        components = max(countdlm_road_component_labels(A)),
        isolates = sum(degree == 0L),
        degree_min = min(degree),
        degree_median = stats::median(degree),
        degree_max = max(degree),
        weighted_homophily = sum(edge_weight * same[upper]) /
            sum(edge_weight),
        binary_homophily = mean(same[upper]),
        edge_distance_quantile_km = stats::quantile(
            edge_distance, c(0, 0.25, 0.5, 0.75, 0.95, 1), names = FALSE
        ),
        edge_weight_quantile = stats::quantile(
            edge_weight, c(0, 0.25, 0.5, 0.75, 0.95, 1), names = FALSE
        ),
        group_induced_components = group_components
    )
}

countdlm_road_background <- function(graph, bbox) {
    bbox <- countdlm_road_validate_bbox(bbox)
    intersects <-
        pmin(graph$from_lon, graph$to_lon) <= bbox[["xmax"]] &
        pmax(graph$from_lon, graph$to_lon) >= bbox[["xmin"]] &
        pmin(graph$from_lat, graph$to_lat) <= bbox[["ymax"]] &
        pmax(graph$from_lat, graph$to_lat) >= bbox[["ymin"]]
    road_class <- if ("highway" %in% names(graph)) {
        as.character(graph$highway[intersects])
    } else rep(NA_character_, sum(intersects))
    road_name <- if ("name" %in% names(graph)) {
        as.character(graph$name[intersects])
    } else if ("road_name" %in% names(graph)) {
        as.character(graph$road_name[intersects])
    } else rep(NA_character_, sum(intersects))
    segment <- data.frame(
        from_id = as.character(graph$from_id[intersects]),
        to_id = as.character(graph$to_id[intersects]),
        x = as.numeric(graph$from_lon[intersects]),
        y = as.numeric(graph$from_lat[intersects]),
        xend = as.numeric(graph$to_lon[intersects]),
        yend = as.numeric(graph$to_lat[intersects]),
        road_class = road_class,
        road_name = road_name,
        stringsAsFactors = FALSE
    )
    first_id <- pmin(segment$from_id, segment$to_id)
    second_id <- pmax(segment$from_id, segment$to_id)
    key <- paste(first_id, second_id, segment$road_class, sep = "|")
    segment <- segment[!duplicated(key), , drop = FALSE]
    segment[, c("x", "y", "xend", "yend", "road_class", "road_name"),
            drop = FALSE]
}

countdlm_open_png <- function(file, width, height, resolution) {
    png_arguments <- list(
        filename = file, width = width, height = height, res = resolution
    )
    is_macos <- identical(unname(Sys.info()[["sysname"]]), "Darwin")
    if (is_macos && isTRUE(capabilities("aqua"))) {
        png_arguments$type <- "quartz"
    } else if (isTRUE(capabilities("cairo"))) {
        png_arguments$type <- "cairo"
    }
    do.call(grDevices::png, png_arguments)
    if (grDevices::dev.cur() == 1L) {
        stop("R could not open a PNG graphics device.", call. = FALSE)
    }
    invisible(file)
}

countdlm_require_rendered_files <- function(files) {
    information <- file.info(files)
    failed <- is.na(information$size) | information$size <= 0
    if (any(failed)) {
        stop(
            "PNG rendering failed for: ",
            paste(basename(files[failed]), collapse = ", "),
            ". The candidate was not finalized.", call. = FALSE
        )
    }
    invisible(files)
}

countdlm_plot_road_candidate <- function(
    context,
    file,
    show_sampling_grid = FALSE,
    show_qnn_edges = TRUE,
    show_groups = TRUE,
    main = "Candidate 100-node central-Beijing road graph",
    width = 2200L,
    height = 1800L,
    resolution = 220L,
    paper_style = FALSE
) {
    locations <- context$locations
    roads <- context$road_segments
    bbox <- context$metadata$bbox
    center <- context$metadata$center
    W <- context$W
    road_class <- tolower(ifelse(is.na(roads$road_class), "", roads$road_class))
    major <- road_class %in% c(
        "motorway", "motorway_link", "trunk", "trunk_link"
    )
    middle <- road_class %in% c(
        "primary", "primary_link", "secondary", "secondary_link"
    )
    local <- !(major | middle)
    colors <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")
    countdlm_open_png(file, width, height, resolution)
    on.exit(grDevices::dev.off(), add = TRUE)
    graphics::par(
        mar = if (isTRUE(paper_style)) c(4.2, 4.4, 0.8, 0.8) else {
            c(4.3, 4.5, 4.6, 1.2)
        },
        xaxs = "i", yaxs = "i"
    )
    graphics::plot(
        NA_real_, NA_real_,
        xlim = bbox[c("xmin", "xmax")],
        ylim = bbox[c("ymin", "ymax")],
        asp = 1 / cos(center[["latitude"]] * pi / 180),
        xlab = "Longitude", ylab = "Latitude",
        main = if (isTRUE(paper_style)) "" else main
    )
    if (any(local)) graphics::segments(
        roads$x[local], roads$y[local], roads$xend[local], roads$yend[local],
        col = grDevices::adjustcolor("#BEBEBE", alpha.f = 0.24), lwd = 0.35
    )
    if (any(middle)) graphics::segments(
        roads$x[middle], roads$y[middle],
        roads$xend[middle], roads$yend[middle],
        col = grDevices::adjustcolor("#777777", alpha.f = 0.58), lwd = 0.65
    )
    if (any(major)) graphics::segments(
        roads$x[major], roads$y[major], roads$xend[major], roads$yend[major],
        col = grDevices::adjustcolor("#2F2F2F", alpha.f = 0.82), lwd = 0.95
    )
    if (isTRUE(show_sampling_grid)) {
        graphics::abline(
            v = context$metadata$grid_x_breaks,
            h = context$metadata$grid_y_breaks,
            col = grDevices::adjustcolor("#376AA0", alpha.f = 0.34),
            lty = 3, lwd = 0.55
        )
    }
    if (isTRUE(show_qnn_edges)) {
        edge <- which(upper.tri(W) & W > 0, arr.ind = TRUE)
        graphics::segments(
            locations$longitude[edge[, 1L]],
            locations$latitude[edge[, 1L]],
            locations$longitude[edge[, 2L]],
            locations$latitude[edge[, 2L]],
            col = grDevices::adjustcolor(
                if (isTRUE(paper_style)) "#52616B" else "#4A4A4A",
                alpha.f = if (isTRUE(paper_style)) 0.50 else 0.30
            ),
            lwd = if (isTRUE(paper_style)) 0.75 else 0.65,
            lty = if (isTRUE(paper_style)) 2 else 1
        )
    }
    graphics::points(
        locations$longitude, locations$latitude,
        pch = 21,
        bg = if (isTRUE(show_groups)) {
            colors[locations$ring_label]
        } else "#2574A9",
        col = "white", lwd = 0.65,
        cex = if (isTRUE(paper_style)) 1.16 else 1.08
    )
    graphics::points(
        center[["longitude"]], center[["latitude"]],
        pch = 4, lwd = 2.2, cex = 1.25, col = "black"
    )
    snapped <- context$metadata$snapped_center
    graphics::points(
        snapped$longitude, snapped$latitude,
        pch = 3, lwd = 1.8, cex = 1.05, col = "#D55E00"
    )
    if (isTRUE(show_groups)) {
        graphics::legend(
            "bottomleft",
            legend = if (isTRUE(paper_style)) {
                c("Group 1 (nearest)", "Group 2", "Group 3", "Group 4",
                  "Group 5 (farthest)")
            } else paste("Group", seq_along(colors)),
            pch = 21, pt.bg = colors, col = "white", pt.cex = 1.05,
            bty = "n", horiz = TRUE,
            cex = if (isTRUE(paper_style)) 0.61 else 0.82
        )
    }
    if (isTRUE(paper_style)) {
        graphics::legend(
            "topright",
            legend = c(
                "Geographic center", "Snapped road center",
                sprintf("q=%d graph edge", context$q),
                "Motorway/trunk", "Primary/secondary", "Local/other"
            ),
            pch = c(4, 3, NA, NA, NA, NA),
            lty = c(NA, NA, 2, 1, 1, 1),
            col = c(
                "black", "#D55E00", "#52616B", "#2F2F2F", "#777777",
                "#BEBEBE"
            ),
            pt.lwd = c(2.2, 1.8, NA, NA, NA, NA),
            lwd = c(NA, NA, 0.75, 0.95, 0.65, 0.35),
            title = "Display layers", bg = "white", box.col = "#D0D0D0",
            cex = 0.62
        )
    } else {
        graphics::legend(
            "topright", legend = c("Geographic center", "Snapped center"),
            pch = c(4, 3), col = c("black", "#D55E00"),
            pt.lwd = c(2.2, 1.8), bty = "n", cex = 0.74
        )
        graphics::mtext(
            if (isTRUE(show_qnn_edges)) {
                sprintf(
                    paste0(
                        "Road-distance groups (20 each); 5x5 x 4 sample; ",
                        "seed %d; q=%d; h=%.3f km"
                    ),
                    context$metadata$selection_seed, context$q, context$h
                )
            } else {
                sprintf(
                    paste0(
                        "Road-window audit only; 5x5 x 4 sample; seed %d; ",
                        "no qNN edges or group colors"
                    ),
                    context$metadata$selection_seed
                )
            },
            side = 3, line = 0.45, cex = 0.82
        )
    }
    invisible(file)
}

countdlm_plot_road_manuscript <- function(context, file) {
    countdlm_plot_road_candidate(
        context = context,
        file = file,
        show_sampling_grid = FALSE,
        show_qnn_edges = TRUE,
        show_groups = TRUE,
        main = "",
        width = 3000L,
        height = 3000L,
        resolution = 300L,
        paper_style = TRUE
    )
}

countdlm_plot_road_window_rule <- function(window_audit, file) {
    countdlm_open_png(file, 1800L, 1300L, 220L)
    on.exit(grDevices::dev.off(), add = TRUE)
    graphics::par(mar = c(4.3, 4.8, 4.2, 1.1))
    graphics::plot(
        window_audit$half_width_km,
        window_audit$shell_density_km_per_km2,
        type = "b", pch = 19, col = "#2574A9", lwd = 1.6,
        xlab = "Square half-width H (km)",
        ylab = expression("H-shell road density (km / " * km^2 * ")"),
        main = "Pre-registered central road-window rule"
    )
    selected <- which(window_audit$selected)
    graphics::points(
        window_audit$half_width_km[selected],
        window_audit$shell_density_km_per_km2[selected],
        pch = 21, bg = "#D55E00", col = "white", cex = 1.5
    )
    graphics::text(
        window_audit$half_width_km[selected],
        window_audit$shell_density_km_per_km2[selected],
        labels = paste0("  selected H=",
                        window_audit$half_width_km[selected], " km"),
        pos = 4, cex = 0.82
    )
    invisible(file)
}

countdlm_road_new_output_dir <- function(output_dir) {
    if (length(output_dir) != 1L || !is.character(output_dir) ||
        is.na(output_dir) || !nzchar(trimws(output_dir))) {
        stop("output_dir must be one explicit path.", call. = FALSE)
    }
    parent <- normalizePath(
        dirname(output_dir), winslash = "/", mustWork = TRUE
    )
    output_dir <- file.path(parent, basename(output_dir))
    if (file.exists(output_dir)) {
        stop(
            "output_dir already exists; choose a new candidate directory: ",
            output_dir,
            call. = FALSE
        )
    }
    output_dir
}

countdlm_road_write_lines <- function(lines, path) {
    connection <- file(path, open = "wt", encoding = "UTF-8")
    on.exit(close(connection), add = TRUE)
    writeLines(enc2utf8(lines), connection, useBytes = TRUE)
}

countdlm_road_manifest <- function(directory, exclude = "CHECKSUMS.sha256") {
    files <- sort(list.files(directory, full.names = TRUE))
    files <- files[file.info(files)$isdir %in% FALSE]
    files <- files[basename(files) != exclude]
    digest <- vapply(files, function(path) {
        digest::digest(file = path, algo = "sha256", serialize = FALSE)
    }, character(1L))
    sprintf("%s  %s", digest, basename(files))
}

countdlm_road_validate_code_provenance <- function(code_provenance) {
    if (!is.list(code_provenance) ||
        !is.data.frame(code_provenance$files) ||
        !all(c("relative_path", "sha256") %in%
             names(code_provenance$files)) ||
        nrow(code_provenance$files) < 3L ||
        length(code_provenance$repository_root) != 1L ||
        !is.character(code_provenance$repository_root) ||
        is.na(code_provenance$repository_root) ||
        !nzchar(code_provenance$repository_root)) {
        stop(
            "code_provenance is required; use the registered thin workflow.",
            call. = FALSE
        )
    }
    files <- code_provenance$files[, c("relative_path", "sha256"),
                                   drop = FALSE]
    files$relative_path <- as.character(files$relative_path)
    files$sha256 <- as.character(files$sha256)
    unsafe_path <- !nzchar(files$relative_path) |
        startsWith(files$relative_path, "/") |
        grepl("(^|/)\\.\\.(/|$)", files$relative_path)
    if (any(unsafe_path) || anyDuplicated(files$relative_path) ||
        any(!grepl("^[0-9a-f]{64}$", files$sha256))) {
        stop("Invalid relative code-provenance paths or SHA-256 values.",
             call. = FALSE)
    }
    git_head <- code_provenance$git_head
    if (length(git_head) != 1L ||
        (!is.na(git_head) && !grepl("^[0-9a-f]{40}$", git_head))) {
        stop("Invalid git_head in code_provenance.", call. = FALSE)
    }
    dirty <- code_provenance$git_worktree_dirty
    if (length(dirty) != 1L || (!is.na(dirty) && !is.logical(dirty))) {
        stop("Invalid git_worktree_dirty in code_provenance.",
             call. = FALSE)
    }
    repository_root <- normalizePath(
        code_provenance$repository_root, winslash = "/", mustWork = TRUE
    )
    if (!dir.exists(repository_root)) {
        stop("Invalid repository_root in code_provenance.", call. = FALSE)
    }
    list(
        files = files,
        git_head = as.character(git_head),
        git_worktree_dirty = as.logical(dirty),
        repository_root = repository_root
    )
}

countdlm_render_road_candidate_figures <- function(
    context_file,
    output_dir,
    code_provenance
) {
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("Package 'digest' is required; no package was installed.",
             call. = FALSE)
    }
    code_provenance <- countdlm_road_validate_code_provenance(
        code_provenance
    )
    context_file <- normalizePath(
        context_file, winslash = "/", mustWork = TRUE
    )
    if (!file.exists(context_file) || dir.exists(context_file)) {
        stop("context_file must be an existing RDS file.", call. = FALSE)
    }
    output_dir <- countdlm_road_new_output_dir(output_dir)
    repository_prefix <- paste0(code_provenance$repository_root, "/")
    if (identical(output_dir, code_provenance$repository_root) ||
        startsWith(output_dir, repository_prefix)) {
        stop("output_dir must resolve outside the source repository.",
             call. = FALSE)
    }
    context <- readRDS(context_file)
    required <- c("locations", "W", "road_segments", "window_audit",
                  "metadata", "q", "h")
    if (!is.list(context) || !all(required %in% names(context)) ||
        !identical(
            context$metadata$workflow_version,
            countdlm_road_candidate_workflow
        ) || !identical(context$metadata$canonical, FALSE) ||
        nrow(context$locations) != 100L ||
        !identical(dim(as.matrix(context$W)), c(100L, 100L))) {
        stop("context_file is not a valid pending road candidate.",
             call. = FALSE)
    }

    temporary_dir <- tempfile(
        pattern = paste0(basename(output_dir), "-staging-"),
        tmpdir = dirname(output_dir)
    )
    if (!dir.create(temporary_dir, recursive = FALSE, showWarnings = FALSE)) {
        stop("Unable to create the figure staging directory.",
             call. = FALSE)
    }
    promoted <- FALSE
    on.exit({
        if (!promoted && dir.exists(temporary_dir)) {
            unlink(temporary_dir, recursive = TRUE, force = TRUE)
        }
    }, add = TRUE)

    density_figure <- file.path(
        temporary_dir, "road_window_selection_audit.png"
    )
    road_audit_figure <- file.path(
        temporary_dir, "road_window_audit.png"
    )
    grid_figure <- file.path(
        temporary_dir, "road_sampling_grid_audit.png"
    )
    graph_figure <- file.path(temporary_dir, "road_graph_candidate.png")
    countdlm_plot_road_window_rule(context$window_audit, density_figure)
    countdlm_plot_road_candidate(
        context, road_audit_figure,
        show_sampling_grid = FALSE,
        show_qnn_edges = FALSE,
        show_groups = FALSE,
        main = "Central-Beijing road-window audit"
    )
    countdlm_plot_road_candidate(
        context, grid_figure,
        show_sampling_grid = TRUE,
        show_qnn_edges = FALSE,
        show_groups = FALSE,
        main = "Fixed 5x5 road-sampling grid audit"
    )
    countdlm_plot_road_candidate(
        context, graph_figure,
        show_sampling_grid = FALSE,
        show_qnn_edges = TRUE,
        show_groups = TRUE
    )
    figure_files <- c(
        density_figure, road_audit_figure, grid_figure, graph_figure
    )
    countdlm_require_rendered_files(figure_files)

    context_sha256 <- digest::digest(
        file = context_file, algo = "sha256", serialize = FALSE
    )
    utils::write.csv(
        code_provenance$files,
        file.path(temporary_dir, "CODE_PROVENANCE.csv"),
        row.names = FALSE
    )
    countdlm_road_write_lines(c(
        "countDLM road-candidate figure recovery",
        paste("Source context SHA-256:", context_sha256),
        paste("Source workflow:", context$metadata$workflow_version),
        paste("Selected-ID SHA-256:",
              context$metadata$selected_id_sha256),
        paste("Git HEAD:", if (is.na(code_provenance$git_head)) {
            "unavailable"
        } else code_provenance$git_head),
        paste("Git worktree dirty at render:", if (
            is.na(code_provenance$git_worktree_dirty)
        ) {
            "unavailable"
        } else code_provenance$git_worktree_dirty),
        paste("Rendered at:",
              format(Sys.time(), tz = "Asia/Shanghai", usetz = TRUE)),
        "No road distance, sample, group, weight, basis, or model was recomputed."
    ), file.path(temporary_dir, "RENDER_PROVENANCE.txt"))
    countdlm_road_write_lines(c(
        "OpenStreetMap attribution",
        context$metadata$osm_attribution,
        "https://www.openstreetmap.org/copyright",
        "This local candidate is not a public release decision."
    ), file.path(temporary_dir, "OSM_NOTICE.txt"))
    countdlm_road_write_lines(
        countdlm_road_manifest(temporary_dir),
        file.path(temporary_dir, "CHECKSUMS.sha256")
    )
    if (!file.rename(temporary_dir, output_dir)) {
        stop("Unable to finalize the recovered figure directory.",
             call. = FALSE)
    }
    promoted <- TRUE
    invisible(list(
        output_dir = output_dir,
        window_selection_figure = file.path(
            output_dir, basename(density_figure)
        ),
        road_window_audit = file.path(
            output_dir, basename(road_audit_figure)
        ),
        grid_figure = file.path(output_dir, basename(grid_figure)),
        figure = file.path(output_dir, basename(graph_figure)),
        source_context_sha256 = context_sha256
    ))
}

#' Build a central-Beijing road-network design candidate
#'
#' This function creates a user-review candidate only. It reads a frozen
#' `dodgr` road graph, selects a square central window by a pre-registered road-
#' density rule, and samples four vertices per cell from a fixed 5-by-5 grid
#' using one registered seed. It then computes directed physical-distance
#' shortest paths, symmetrizes them, forms five equal road-distance groups, and
#' builds a fixed q=4 nearest-neighbor graph. It never changes the seed or q in
#' response to labels, homophily, visual appearance, or method results.
#'
#' Candidate RDS/CSV payloads are written to a new external directory and are
#' not a canonical simulation context until the user approves the figure and a
#' later provenance update promotes it.
#'
#' @param road_graph_file Existing frozen `dodgr` road-graph RDS.
#' @param output_dir New external output directory; existing paths are refused.
#' @param code_provenance Hashes and Git identity supplied by the registered
#'   thin workflow.
#' @return Invisibly, a compact list of output paths and audit summaries.
#' @export
countdlm_build_road_context_candidate <- function(
    road_graph_file,
    output_dir,
    code_provenance
) {
    if (!requireNamespace("dodgr", quietly = TRUE)) {
        stop("Package 'dodgr' is required; no package was installed.",
             call. = FALSE)
    }
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("Package 'digest' is required; no package was installed.",
             call. = FALSE)
    }
    if (missing(code_provenance)) code_provenance <- NULL
    code_provenance <- countdlm_road_validate_code_provenance(
        code_provenance
    )
    center <- countdlm_road_candidate_default_center
    selection_seed <- countdlm_road_candidate_selection_seed
    window_half_widths_km <-
        countdlm_road_candidate_window_half_widths_km
    window_rule_start_km <-
        countdlm_road_candidate_window_rule_start_km
    window_drop_ratio <- countdlm_road_candidate_window_drop_ratio
    grid_nx <- countdlm_road_candidate_grid_nx
    grid_ny <- countdlm_road_candidate_grid_ny
    per_cell <- countdlm_road_candidate_per_cell
    K_true <- countdlm_road_candidate_K_true
    q <- countdlm_road_candidate_q
    audit_q <- countdlm_road_candidate_audit_q
    basis_m <- countdlm_road_candidate_basis_m
    nu <- countdlm_road_candidate_basis_nu
    length_scale <- countdlm_road_candidate_basis_length_scale
    target_rms_sd <- countdlm_road_candidate_basis_target_rms_sd
    road_graph_file <- normalizePath(
        road_graph_file, winslash = "/", mustWork = TRUE
    )
    if (!file.exists(road_graph_file) || dir.exists(road_graph_file)) {
        stop("road_graph_file must be an existing RDS file.", call. = FALSE)
    }
    output_dir <- countdlm_road_new_output_dir(output_dir)
    repository_prefix <- paste0(code_provenance$repository_root, "/")
    if (identical(output_dir, code_provenance$repository_root) ||
        startsWith(output_dir, repository_prefix)) {
        stop(
            "output_dir must resolve outside the source repository.",
            call. = FALSE
        )
    }
    center <- as.numeric(center)
    if (length(center) != 2L || any(!is.finite(center))) {
        stop("center must contain finite longitude and latitude.",
             call. = FALSE)
    }
    center <- stats::setNames(center, c("longitude", "latitude"))
    grid_nx <- gmde_scalar_integer(grid_nx, "grid_nx", lower = 1L)
    grid_ny <- gmde_scalar_integer(grid_ny, "grid_ny", lower = 1L)
    per_cell <- gmde_scalar_integer(per_cell, "per_cell", lower = 1L)
    K_true <- gmde_scalar_integer(K_true, "K_true", lower = 2L)
    q <- gmde_scalar_integer(q, "q", lower = 1L)
    n <- grid_nx * grid_ny * per_cell
    if (n != 100L) {
        stop("This review workflow requires exactly 100 sampled locations.",
             call. = FALSE)
    }
    if (n %% K_true != 0L) {
        stop("grid_nx * grid_ny * per_cell must be divisible by K_true.",
             call. = FALSE)
    }
    if (length(basis_m) != 1L || !is.finite(basis_m) ||
        basis_m != round(basis_m) || basis_m < 1L || basis_m > n) {
        stop("basis_m must be an integer from 1 through n.", call. = FALSE)
    }

    cat("[1/9] Hashing and loading the frozen road graph...\n")
    source_sha256 <- digest::digest(
        file = road_graph_file, algo = "sha256", serialize = FALSE
    )
    if (!identical(source_sha256, countdlm_road_candidate_source_sha256)) {
        stop(
            "The road graph is not the registered frozen clean graph. ",
            "Expected SHA-256 ", countdlm_road_candidate_source_sha256,
            "; observed ", source_sha256, ".", call. = FALSE
        )
    }
    graph <- readRDS(road_graph_file)
    countdlm_road_require_columns(graph)

    cat("[2/9] Selecting the window from the fixed road-density rule...\n")
    window <- countdlm_road_select_window(
        graph, center = center,
        half_widths_km = window_half_widths_km,
        rule_start_km = window_rule_start_km,
        drop_ratio = window_drop_ratio
    )
    bbox <- countdlm_road_validate_bbox(window$bbox)
    center <- countdlm_road_validate_center(center, bbox)

    cat("[3/9] Building the eligible central sampling frame...\n")
    frame <- countdlm_road_vertices_in_bbox(graph, bbox)
    if (nrow(frame) < 3L * n) {
        stop(
            "The fixed bbox contains fewer than 3*n unique road vertices; ",
            "the sampling frame is too small.", call. = FALSE
        )
    }
    snapped_center <- countdlm_road_snap_center(frame, center)
    frame <- frame[
        frame$road_vertex_id != snapped_center$road_vertex_id[[1L]],
        , drop = FALSE
    ]

    cat("[4/9] Taking the one-shot grid-stratified random sample...\n")
    sample_result <- countdlm_stratified_road_sample(
        frame, bbox,
        grid_nx = grid_nx, grid_ny = grid_ny,
        per_cell = per_cell,
        selection_seed = selection_seed
    )
    locations <- sample_result$selected
    if (anyDuplicated(locations[, c("longitude", "latitude"),
                                drop = FALSE])) {
        stop(
            "The fixed one-shot sample contains duplicate coordinates. ",
            "The seed was not changed.", call. = FALSE
        )
    }
    selected_id <- as.character(locations$road_vertex_id)

    cat("[5/9] Computing directed physical-length road shortest paths...\n")
    routing_graph <- graph
    routing_graph$d_weighted <- routing_graph$d
    D_directed_raw <- suppressWarnings(dodgr::dodgr_dists(
        routing_graph, from = selected_id, to = selected_id,
        parallel = FALSE
    ))
    D_directed <- countdlm_road_order_dists(
        D_directed_raw, selected_id, selected_id,
        name = "Pairwise dodgr distances"
    )
    rm(D_directed_raw)
    D_directed[is.na(D_directed)] <- Inf
    diag(D_directed) <- 0
    center_id <- as.character(snapped_center$road_vertex_id[[1L]])
    center_out_matrix <- suppressWarnings(dodgr::dodgr_dists(
        routing_graph, from = center_id, to = selected_id,
        parallel = FALSE
    ))
    center_out_matrix <- countdlm_road_order_dists(
        center_out_matrix, center_id, selected_id,
        name = "Center-to-node dodgr distances"
    )
    center_in_matrix <- suppressWarnings(dodgr::dodgr_dists(
        routing_graph, from = selected_id, to = center_id,
        parallel = FALSE
    ))
    center_in_matrix <- countdlm_road_order_dists(
        center_in_matrix, selected_id, center_id,
        name = "Node-to-center dodgr distances"
    )
    center_out <- as.numeric(center_out_matrix[1L, ])
    center_in <- as.numeric(center_in_matrix[, 1L])
    rm(center_out_matrix, center_in_matrix)
    if (length(center_out) != n || length(center_in) != n ||
        any(!is.finite(D_directed)) ||
        any(!is.finite(center_out)) || any(!is.finite(center_in))) {
        stop(
            "The fixed one-shot sample is not mutually road-reachable from ",
            "the snapped center. The seed was not changed.", call. = FALSE
        )
    }
    D_km <- (D_directed + t(D_directed)) / 2000
    diag(D_km) <- 0
    center_road_distance_km <- (center_out + center_in) / 2000

    rm(routing_graph)
    invisible(gc())

    cat("[6/9] Building and hard-auditing the fixed q=4 road graph...\n")
    q_result <- countdlm_road_fixed_qnn(
        D_km, selected_id, q = q, audit_q = audit_q
    )
    sparse <- q_result$graph

    cat("[7/9] Assigning five equal road-distance groups and auditing them...\n")
    ring_label <- countdlm_road_equal_groups(
        center_road_distance_km, selected_id, K = K_true
    )
    haversine_km <- countdlm_road_haversine_km(
        locations$longitude, locations$latitude, center
    )
    haversine_label <- countdlm_road_equal_groups(
        haversine_km, selected_id, K = K_true
    )
    locations$center_road_distance_km <- center_road_distance_km
    locations$center_haversine_km <- haversine_km
    locations$ring_label <- ring_label
    locations$haversine_ring_label <- haversine_label
    locations <- locations[, c(
        "location_id", "sampling_frame_id", "road_vertex_id",
        "longitude", "latitude", "cell_x", "cell_y", "cell_id",
        "cell_candidate_n", "inclusion_probability",
        "center_road_distance_km", "center_haversine_km",
        "ring_label", "haversine_ring_label"
    )]
    graph_diagnostics <- countdlm_road_graph_diagnostics(
        sparse$W, D_km, ring_label
    )
    comparison <- list(
        spearman = stats::cor(
            center_road_distance_km, haversine_km, method = "spearman"
        ),
        kendall = stats::cor(
            center_road_distance_km, haversine_km, method = "kendall"
        ),
        ari = countdlm_road_ari(ring_label, haversine_label),
        changed_nodes = sum(ring_label != haversine_label),
        cross_table = unclass(table(
            road_group = ring_label, geographic_group = haversine_label
        ))
    )
    group_boundaries <- do.call(rbind, lapply(seq_len(K_true), function(k) {
        index <- which(ring_label == k)
        data.frame(
            ring_label = k,
            n = length(index),
            min_road_km = min(center_road_distance_km[index]),
            max_road_km = max(center_road_distance_km[index]),
            min_haversine_km = min(haversine_km[index]),
            max_haversine_km = max(haversine_km[index]),
            induced_components =
                graph_diagnostics$group_induced_components[[k]]
        )
    }))
    Phi <- gmde_graph_basis(
        sparse$W, m = as.integer(basis_m), nu = nu,
        length_scale = length_scale, target_rms_sd = target_rms_sd
    )
    roads <- countdlm_road_background(graph, bbox)

    selected_id_sha256 <- digest::digest(
        paste(selected_id, collapse = ","),
        algo = "sha256", serialize = FALSE
    )
    frame_id_sha256 <- digest::digest(
        paste(sample_result$frame$road_vertex_id, collapse = ","),
        algo = "sha256", serialize = FALSE
    )
    metadata <- list(
        workflow_version = countdlm_road_candidate_workflow,
        canonical = FALSE,
        user_review_status = "pending",
        scientific_role = "road-network visual-design candidate",
        selection_method = sample_result$sampling_method,
        selection_seed = as.integer(selection_seed),
        rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"),
        n = n,
        K_true = K_true,
        bbox = bbox,
        window_half_width_km = window$half_width_km,
        window_rule = window$rule,
        window_half_width_candidates_km =
            as.numeric(window_half_widths_km),
        window_rule_start_km = window_rule_start_km,
        window_drop_ratio = window_drop_ratio,
        bbox_rationale = paste(
            "pre-registered full-road-network density break around the fixed",
            "geographic center; selected without labels or model results"
        ),
        center = center,
        snapped_center = list(
            road_vertex_id = center_id,
            longitude = snapped_center$longitude[[1L]],
            latitude = snapped_center$latitude[[1L]],
            snap_distance_m = snapped_center$snap_distance_m[[1L]]
        ),
        source_graph_file = road_graph_file,
        source_graph_sha256 = source_sha256,
        code_files_sha256 = code_provenance$files,
        code_git_head = code_provenance$git_head,
        code_git_worktree_dirty =
            code_provenance$git_worktree_dirty,
        sampling_frame_n = nrow(sample_result$frame),
        sampling_fraction = n / nrow(sample_result$frame),
        frame_id_sha256 = frame_id_sha256,
        selected_id_sha256 = selected_id_sha256,
        grid_nx = grid_nx,
        grid_ny = grid_ny,
        sampled_per_cell = per_cell,
        grid_cell_count_min = min(sample_result$cell_count),
        grid_cell_count_median = stats::median(sample_result$cell_count),
        grid_cell_count_max = max(sample_result$cell_count),
        grid_x_breaks = sample_result$x_breaks,
        grid_y_breaks = sample_result$y_breaks,
        center_distance_definition = paste(
            "average of directed physical-length shortest-path distances",
            "center-to-node and node-to-center; d_weighted was set equal to",
            "physical edge length d in an in-memory routing copy"
        ),
        graph_definition = paste(
            "fixed q=4; h is the median fourth-nearest road distance;",
            "directed exp(-D/h) weights symmetrized by arithmetic mean;",
            "connectivity is a hard audit"
        ),
        q_fixed = q,
        q_sensitivity_audit = as.integer(audit_q),
        q_nn = sparse$q,
        h_nn_km = sparse$h_nn_km,
        comparison_with_haversine = comparison,
        graph_diagnostics = graph_diagnostics,
        basis_m = as.integer(basis_m),
        basis_nu = nu,
        basis_length_scale = length_scale,
        basis_target_rms_sd = target_rms_sd,
        R_version = R.version.string,
        dodgr_version = as.character(utils::packageVersion("dodgr")),
        digest_version = as.character(utils::packageVersion("digest")),
        generated_at = format(Sys.time(), tz = "Asia/Shanghai", usetz = TRUE),
        osm_attribution = paste(
            "Contains information from OpenStreetMap, which is made available",
            "under the Open Database License (ODbL)."
        )
    )
    context <- list(
        locations = locations,
        D_directed = D_directed,
        D_km = D_km,
        W = sparse$W,
        A = sparse$A,
        L = diag(rowSums(sparse$W)) - sparse$W,
        Phi = Phi,
        q = sparse$q,
        h = sparse$h_nn_km,
        road_segments = roads,
        window_audit = window$audit,
        group_boundaries = group_boundaries,
        q_audit = q_result$audit,
        metadata = metadata
    )
    rm(graph)
    invisible(gc())

    cat("[8/9] Writing the candidate to a new external directory...\n")
    temporary_dir <- tempfile(
        pattern = paste0(basename(output_dir), "-staging-"),
        tmpdir = dirname(output_dir)
    )
    if (!dir.create(temporary_dir, recursive = FALSE, showWarnings = FALSE)) {
        stop("Unable to create the candidate staging directory.",
             call. = FALSE)
    }
    promoted <- FALSE
    on.exit({
        if (!promoted && dir.exists(temporary_dir)) {
            unlink(temporary_dir, recursive = TRUE, force = TRUE)
        }
    }, add = TRUE)

    context_file <- file.path(
        temporary_dir, "central_beijing_road_context_candidate.rds"
    )
    node_file <- file.path(
        temporary_dir, "central_beijing_road_nodes_candidate.csv"
    )
    edge_file <- file.path(
        temporary_dir, "central_beijing_qnn_edges_candidate.csv"
    )
    group_file <- file.path(
        temporary_dir, "central_beijing_road_groups_candidate.csv"
    )
    q_file <- file.path(temporary_dir, "q_sensitivity_audit.csv")
    window_file <- file.path(temporary_dir, "road_window_density_audit.csv")
    figure_file <- file.path(
        temporary_dir, "road_graph_candidate.png"
    )
    road_audit_figure_file <- file.path(
        temporary_dir, "road_window_audit.png"
    )
    grid_figure_file <- file.path(
        temporary_dir, "road_sampling_grid_audit.png"
    )
    density_figure_file <- file.path(
        temporary_dir, "road_window_selection_audit.png"
    )
    audit_file <- file.path(temporary_dir, "ROAD_CANDIDATE_AUDIT.txt")
    code_file <- file.path(temporary_dir, "CODE_PROVENANCE.csv")
    notice_file <- file.path(temporary_dir, "OSM_NOTICE.txt")

    saveRDS(context, context_file, compress = "xz")
    utils::write.csv(locations, node_file, row.names = FALSE)
    edge_index <- which(upper.tri(sparse$W) & sparse$W > 0, arr.ind = TRUE)
    edge_table <- data.frame(
        from_location_id = edge_index[, 1L],
        to_location_id = edge_index[, 2L],
        road_distance_km = D_km[edge_index],
        weight = sparse$W[edge_index]
    )
    utils::write.csv(edge_table, edge_file, row.names = FALSE)
    utils::write.csv(group_boundaries, group_file, row.names = FALSE)
    utils::write.csv(q_result$audit, q_file, row.names = FALSE)
    utils::write.csv(window$audit, window_file, row.names = FALSE)
    utils::write.csv(code_provenance$files, code_file, row.names = FALSE)
    countdlm_plot_road_candidate(
        context, figure_file,
        show_sampling_grid = FALSE,
        show_qnn_edges = TRUE,
        show_groups = TRUE
    )
    countdlm_plot_road_candidate(
        context, road_audit_figure_file,
        show_sampling_grid = FALSE,
        show_qnn_edges = FALSE,
        show_groups = FALSE,
        main = "Central-Beijing road-window audit"
    )
    countdlm_plot_road_candidate(
        context, grid_figure_file,
        show_sampling_grid = TRUE,
        show_qnn_edges = FALSE,
        show_groups = FALSE,
        main = "Fixed 5x5 road-sampling grid audit"
    )
    countdlm_plot_road_window_rule(window$audit, density_figure_file)
    countdlm_require_rendered_files(c(
        density_figure_file,
        road_audit_figure_file,
        grid_figure_file,
        figure_file
    ))

    comparison_table <- utils::capture.output(print(comparison$cross_table))
    audit_lines <- c(
        "countDLM central-Beijing road-network candidate",
        "Status: candidate only; not approved and not canonical",
        paste("Workflow:", metadata$workflow_version),
        paste("Git HEAD:", if (is.na(metadata$code_git_head)) {
            "unavailable"
        } else metadata$code_git_head),
        paste("Git worktree dirty at launch:", if (
            is.na(metadata$code_git_worktree_dirty)
        ) {
            "unavailable"
        } else metadata$code_git_worktree_dirty),
        "Executed source hashes: see CODE_PROVENANCE.csv",
        paste("Input graph SHA-256:", source_sha256),
        paste("Window rule:", metadata$window_rule),
        paste("Selected square half-width (km):",
              metadata$window_half_width_km),
        paste("Selected bbox:", paste(format(bbox, digits = 10),
                                     collapse = ", ")),
        paste("Sampling frame vertices:", metadata$sampling_frame_n),
        paste("Selected vertices:", n),
        paste("Sampling fraction:", format(metadata$sampling_fraction, digits = 8)),
        paste("Selection method:", metadata$selection_method),
        paste("Selection seed:", metadata$selection_seed),
        paste("Selected-ID SHA-256:", selected_id_sha256),
        paste("Grid and sample per cell:",
              paste0(grid_nx, " x ", grid_ny, " x ", per_cell)),
        paste("Cell candidate counts min/median/max:", paste(
            metadata$grid_cell_count_min,
            metadata$grid_cell_count_median,
            metadata$grid_cell_count_max,
            sep = "/"
        )),
        paste("Center:", paste(format(center, digits = 9), collapse = ", ")),
        paste("Snapped center road vertex:", center_id),
        paste("Center snap distance (m):", format(
            metadata$snapped_center$snap_distance_m, digits = 8
        )),
        paste("q used:", sparse$q),
        paste("Road-distance definition:",
              metadata$center_distance_definition),
        paste("h_nn_km:", format(sparse$h_nn_km, digits = 8)),
        paste("Graph edges:", graph_diagnostics$edges),
        paste("Graph components:", graph_diagnostics$components),
        paste("Graph isolates:", graph_diagnostics$isolates),
        paste("Degree min/median/max:", paste(
            graph_diagnostics$degree_min,
            graph_diagnostics$degree_median,
            graph_diagnostics$degree_max,
            sep = "/"
        )),
        paste("Road-vs-Haversine distance Spearman:", format(
            comparison$spearman, digits = 8
        )),
        paste("Road-vs-Haversine distance Kendall:", format(
            comparison$kendall, digits = 8
        )),
        paste("Road-vs-Haversine five-group ARI:", format(
            comparison$ari, digits = 8
        )),
        paste("Nodes changing groups:", comparison$changed_nodes),
        "Road-group by geographic-group cross table:",
        comparison_table,
        paste("Group induced components:", paste(
            graph_diagnostics$group_induced_components, collapse = ", "
        )),
        paste("Weighted/binary homophily:", paste(
            format(graph_diagnostics$weighted_homophily, digits = 8),
            format(graph_diagnostics$binary_homophily, digits = 8),
            sep = "/"
        )),
        "Selection never used labels, homophily, method fits, or ARI.",
        metadata$osm_attribution
    )
    countdlm_road_write_lines(audit_lines, audit_file)
    countdlm_road_write_lines(c(
        "OpenStreetMap attribution",
        "Contains information from OpenStreetMap, which is made available",
        "under the Open Database License (ODbL):",
        "https://www.openstreetmap.org/copyright",
        "This local candidate is not a public release decision."
    ), notice_file)
    checksum_file <- file.path(temporary_dir, "CHECKSUMS.sha256")
    countdlm_road_write_lines(
        countdlm_road_manifest(temporary_dir), checksum_file
    )

    cat("[9/9] Finalizing the checksummed candidate directory...\n")
    if (!file.rename(temporary_dir, output_dir)) {
        stop("Unable to promote the completed candidate directory.",
             call. = FALSE)
    }
    promoted <- TRUE
    cat("Candidate completed. Review these files in this order:\n")
    cat(file.path(output_dir, basename(density_figure_file)), "\n")
    cat(file.path(output_dir, basename(road_audit_figure_file)), "\n")
    cat(file.path(output_dir, basename(grid_figure_file)), "\n")
    cat(file.path(output_dir, basename(figure_file)), "\n")
    cat(file.path(output_dir, basename(audit_file)), "\n")
    invisible(list(
        output_dir = output_dir,
        figure = file.path(output_dir, basename(figure_file)),
        road_window_audit = file.path(
            output_dir, basename(road_audit_figure_file)
        ),
        grid_figure = file.path(output_dir, basename(grid_figure_file)),
        window_selection_figure = file.path(
            output_dir, basename(density_figure_file)
        ),
        audit = file.path(output_dir, basename(audit_file)),
        context = file.path(output_dir, basename(context_file)),
        selected_id_sha256 = selected_id_sha256,
        q = sparse$q,
        h_nn_km = sparse$h_nn_km,
        comparison_with_haversine = comparison
    ))
}
