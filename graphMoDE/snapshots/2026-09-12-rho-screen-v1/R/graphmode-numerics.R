# New implementation for the September 10 specification. Legacy gmde_* kernels
# remain byte-identical so that historical execution snapshots retain identity.
graphmode_sampler_version <- "graphmode-adaptive-square-root-2026-09-11-v2"
graphmode_protocol <- "graphMoDE-simple-roads-2026-09-10-r2"

graphmode_positive <- function(x, name, zero = FALSE) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
        if (zero) x < 0 else x <= 0) {
        stop(name, " must be a finite ", if (zero) "nonnegative" else "positive",
             " scalar.", call. = FALSE)
    }
    x
}

graphmode_matrix <- function(x, name) {
    if (!is.matrix(x) || !is.numeric(x) || any(dim(x) < 1L) ||
        any(!is.finite(x))) stop(name, " must be a finite numeric matrix.",
                               call. = FALSE)
    x
}

graphmode_normal <- function(n) stats::rnorm(n)
graphmode_uniform <- function(n) stats::runif(n)
graphmode_gamma <- function(shape, rate) {
    stats::rgamma(length(shape), shape = shape, rate = rate)
}
graphmode_pg <- function(b, z) gmde_draw_pg(b, z, backend = "devroye-exact")

graphmode_logsoftmax <- function(x) {
    x <- graphmode_matrix(x, "log weights")
    shifted <- x - apply(x, 1L, max)
    if (any(!is.finite(shifted)))
        stop("Log-weight differences exceed floating-point range.", call. = FALSE)
    shifted - log(rowSums(exp(shifted)))
}

graphmode_categorical <- function(log_weights) {
    probability <- exp(graphmode_logsoftmax(log_weights))
    u <- graphmode_uniform(nrow(probability))
    vapply(seq_len(nrow(probability)), function(i) {
        # The final bin absorbs only rounding in the cumulative sum.
        as.integer(1L + sum(u[i] > utils::head(cumsum(probability[i, ]), -1L)))
    }, integer(1))
}

graphmode_gate_loglik <- function(utilities, Z) {
    Z <- gmde_validate_labels(Z, nrow(utilities), ncol(utilities), "Z")
    sum(graphmode_logsoftmax(utilities)[cbind(seq_along(Z), Z)])
}

# A = Q R, unpivoted QR with a positive diagonal convention. Stacking
# whitened likelihood rows avoids forming crossprod(A), which can lose rank
# at large observation precision. No jitter or eigenvalue clipping is used.
graphmode_qr_root <- function(A) {
    A <- graphmode_matrix(A, "precision rows")
    if (nrow(A) < ncol(A)) stop("Too few precision rows.", call. = FALSE)
    # Put large rows first. In highly informative rank-one observations this
    # avoids subtracting two O(sqrt(precision)) residuals to recover an O(1)
    # prior direction. The row permutation is also applied to the RHS below.
    row_order <- order(apply(abs(A), 1L, max), decreasing = TRUE)
    decomposition <- qr(A[row_order, , drop = FALSE], tol = 0, LAPACK = FALSE)
    R <- qr.R(decomposition)
    if (!identical(decomposition$pivot, seq_len(ncol(A))) ||
        any(!is.finite(R)) || any(diag(R) == 0)) {
        stop("Gaussian factorization lost rank.", call. = FALSE)
    }
    R <- R * sign(diag(R))
    residual <- max(abs(crossprod(R) - crossprod(A))) /
        max(1, max(abs(crossprod(A))))
    if (!is.finite(residual) || residual > 1e-10) {
        stop("Gaussian QR factorization failed its residual check.", call. = FALSE)
    }
    list(R = R, residual = residual, decomposition = decomposition, row_order = row_order,
         row_sign = sign(diag(qr.R(decomposition))))
}

# A root B always means covariance B B', not B' B. It need not be lower
# triangular. Keep roots through filtering and smoothing: a rounded dense
# covariance may no longer represent a very small positive eigenvalue.
graphmode_whitener <- function(B) {
    B <- graphmode_matrix(B, "covariance root")
    if (nrow(B) != ncol(B)) stop("Root must be square.", call. = FALSE)
    out <- solve(B, diag(nrow(B)), tol = 0)
    residual <- max(abs(B %*% out - diag(nrow(B))))
    if (!all(is.finite(out)) || !is.finite(residual) || residual > 1e-8) {
        stop("Covariance-root solve failed its residual check.", call. = FALSE)
    }
    out
}

# Audit the *solved covariance root*, not just the QR factor of the precision.
# If P = A'A and B = R^-1 is accurate, RB = I and B'PB = I. These defects
# are dimensionless and cannot be hidden by dividing by a huge norm(P).
# Both multiplication orders are checked: scaling the design before versus
# after projecting it through B can expose different cancellation errors.
graphmode_conditional_root_audit <- function(R, whitening, design, precision, B) {
    p <- ncol(R)
    identity <- diag(p)
    tolerance <- 1e-6
    reciprocal_condition <- rcond(R)
    inverse_residual <- norm(R %*% B - identity, "I")
    prior_projection <- whitening %*% B
    scaled_projection <- (sqrt(precision) * design) %*% B
    direct_projection <- sqrt(precision) * (design %*% B)
    precision_residual <- norm(crossprod(rbind(prior_projection, scaled_projection)) - identity, "I")
    projection_residual <- norm(crossprod(rbind(prior_projection, direct_projection)) - identity, "I")
    residuals <- c(inverse = inverse_residual, precision = precision_residual,
                   projection = projection_residual)
    # Conservative floating-point range guard. It is not a posterior variance
    # floor, an observation-precision cap or a user-tunable simulation setting.
    if (!is.finite(reciprocal_condition) || reciprocal_condition <= .Machine$double.eps ||
        any(!is.finite(residuals)) || any(residuals > tolerance)) {
        stop(sprintf(paste0("Gaussian conditional covariance-root accuracy audit failed ",
            "(rcond=%.3g, inverse=%.3g, precision=%.3g, projection=%.3g; tolerance=%.3g). ",
            "No jitter or clipping applied."), reciprocal_condition, inverse_residual,
            precision_residual, projection_residual, tolerance), call. = FALSE)
    }
    list(root_residual = max(residuals), root_reciprocal_condition = reciprocal_condition,
         inverse_residual = inverse_residual, precision_residual = precision_residual,
         projection_residual = projection_residual, tolerance = tolerance)
}

graphmode_information_condition <- function(mean, root, design, precision, natural) {
    design <- graphmode_matrix(design, "design")
    p <- ncol(design)
    if (length(mean) != p || !identical(dim(root), c(p, p)) ||
        length(precision) != nrow(design) || length(natural) != nrow(design) ||
        any(!is.finite(c(mean, precision, natural))) || any(precision < 0)) {
        stop("Incompatible Gaussian information inputs.", call. = FALSE)
    }
    if (any(precision == 0 & natural != 0))
        stop("Zero observation precision requires zero natural parameter.", call. = FALSE)
    whitening <- graphmode_whitener(root)
    factor <- graphmode_qr_root(rbind(whitening, sqrt(precision) * design))
    scaled_response <- numeric(length(natural))
    positive <- precision > 0
    scaled_response[positive] <- natural[positive] / sqrt(precision[positive])
    rhs <- c(whitening %*% mean, scaled_response)
    rotated <- qr.qty(factor$decomposition, rhs[factor$row_order])[seq_len(p)] * factor$row_sign
    B <- backsolve(factor$R, diag(p))
    # Solve the whitened least-squares problem directly. Forming the natural
    # vector and multiplying twice by B would square the condition number.
    m <- as.numeric(backsolve(factor$R, rotated))
    if (any(!is.finite(c(B, m)))) stop("Nonfinite Gaussian conditional.", call. = FALSE)
    audit <- graphmode_conditional_root_audit(factor$R, whitening, design, precision, B)
    list(mean = m, root = B, covariance = tcrossprod(B),
         factor_residual = factor$residual, root_residual = audit$root_residual,
         root_reciprocal_condition = audit$root_reciprocal_condition, root_audit = audit)
}

graphmode_dlm <- function(Fmat, m0, C0, G, W, dynamics) {
    Fmat <- graphmode_matrix(Fmat, "Fmat")
    p <- ncol(Fmat)
    if (length(m0) != p || any(!is.finite(m0)) ||
        !identical(dim(C0), c(p, p))) stop("Invalid initial moments.", call. = FALSE)
    gmde_chol_spd(C0, "C0")
    if (dynamics == "dynamic") {
        if (!identical(dim(G), c(p, p)) || !identical(dim(W), c(p, p)) ||
            any(!is.finite(G))) stop("Invalid evolution matrices.", call. = FALSE)
        gmde_chol_spd(W, "W: dynamic models require positive innovation variance")
    } else if (!is.null(G) || !is.null(W)) {
        stop("Static coefficients require G = W = NULL.", call. = FALSE)
    }
    invisible(TRUE)
}

graphmode_information_filter <- function(precision, natural, Fmat, m0, C0, G, W) {
    graphmode_dlm(Fmat, m0, C0, G, W, "dynamic")
    TT <- nrow(Fmat)
    p <- ncol(Fmat)
    if (length(precision) != TT || length(natural) != TT ||
        any(!is.finite(c(precision, natural))) || any(precision < 0)) {
        stop("Invalid length-T observation information.", call. = FALSE)
    }
    m <- a <- matrix(0, TT, p)
    roots <- predictive_roots <- array(0, c(p, p, TT))
    residual <- numeric(TT)
    root_residual <- root_reciprocal_condition <- numeric(TT)
    previous_mean <- m0
    previous_root <- t(gmde_chol_spd(C0))
    innovation_root <- t(gmde_chol_spd(W))
    for (tt in seq_len(TT)) {
        a[tt, ] <- G %*% previous_mean
        stacked <- cbind(G %*% previous_root, innovation_root)
        prediction <- t(graphmode_qr_root(t(stacked))$R)
        condition <- graphmode_information_condition(a[tt, ], prediction,
            Fmat[tt, , drop = FALSE], precision[tt], natural[tt])
        m[tt, ] <- condition$mean
        roots[, , tt] <- condition$root
        predictive_roots[, , tt] <- prediction
        residual[tt] <- condition$factor_residual
        root_residual[tt] <- condition$root_residual
        root_reciprocal_condition[tt] <- condition$root_reciprocal_condition
        previous_mean <- condition$mean
        previous_root <- condition$root
    }
    list(m = m, root = roots, a = a, predictive_root = predictive_roots,
         factor_residual = residual, root_residual = root_residual,
         root_reciprocal_condition = root_reciprocal_condition, G = G, W_root = innovation_root)
}

graphmode_backward_condition <- function(mean, root, next_state, G, W_root) {
    whiten_W <- graphmode_whitener(W_root)
    # Treat theta[t+1] as a Gaussian observation of G theta[t].
    graphmode_information_condition(mean, root, whiten_W %*% G,
        rep(1, nrow(G)), as.numeric(whiten_W %*% next_state))
}

graphmode_gaussian_draw <- function(condition) {
    as.numeric(condition$mean + condition$root %*%
                   graphmode_normal(length(condition$mean)))
}

graphmode_ffbs <- function(precision, natural, Fmat, m0, C0, G, W) {
    moments <- graphmode_information_filter(precision, natural, Fmat, m0, C0, G, W)
    TT <- nrow(Fmat)
    p <- ncol(Fmat)
    theta <- matrix(0, TT, p)
    factor_residual <- max(moments$factor_residual)
    root_residual <- max(moments$root_residual)
    root_reciprocal_condition <- min(moments$root_reciprocal_condition)
    theta[TT, ] <- graphmode_gaussian_draw(list(mean = moments$m[TT, ],
        root = matrix(moments$root[, , TT], p, p)))
    if (TT > 1L) for (tt in seq.int(TT - 1L, 1L)) {
        condition <- graphmode_backward_condition(moments$m[tt, ],
            matrix(moments$root[, , tt], p, p), theta[tt + 1L, ], G, moments$W_root)
        factor_residual <- max(factor_residual, condition$factor_residual)
        root_residual <- max(root_residual, condition$root_residual)
        root_reciprocal_condition <- min(root_reciprocal_condition, condition$root_reciprocal_condition)
        theta[tt, ] <- graphmode_gaussian_draw(condition)
    }
    list(theta = theta, factor_residual = factor_residual, root_residual = root_residual,
         root_reciprocal_condition = root_reciprocal_condition)
}

graphmode_prior_expert <- function(Fmat, m0, C0, G, W, dynamics) {
    TT <- nrow(Fmat)
    p <- ncol(Fmat)
    current <- graphmode_gaussian_draw(list(mean = m0, root = t(gmde_chol_spd(C0))))
    if (dynamics == "static") return(matrix(rep(current, each = TT), TT, p))
    root_W <- t(gmde_chol_spd(W))
    path <- matrix(0, TT, p)
    for (tt in seq_len(TT)) {
        current <- graphmode_gaussian_draw(list(mean = as.numeric(G %*% current),
                                                root = root_W))
        path[tt, ] <- current
    }
    path
}

graphmode_response_loglik <- function(Y, eta, family, sigma2 = NULL) {
    graphmode_matrix(Y, "Y")
    graphmode_matrix(eta, "eta")
    if (ncol(Y) != ncol(eta)) stop("Response times disagree.", call. = FALSE)
    if (family == "poisson") {
        if (any(Y < 0 | Y != round(Y))) stop("Y must contain counts.", call. = FALSE)
        value <- sweep(Y %*% t(eta), 2L, rowSums(exp(eta)), "-")
    } else if (family == "gaussian") {
        if (length(sigma2) != nrow(eta) || any(!is.finite(sigma2)) || any(sigma2 <= 0)) {
            stop("Invalid Gaussian variances.", call. = FALSE)
        }
        # Direct residuals avoid subtracting large nearly equal sums of squares.
        value <- vapply(seq_len(nrow(eta)), function(k) {
            residual <- sweep(Y, 2L, eta[k, ], "-")
            -ncol(Y) / 2 * log(sigma2[k]) - rowSums(residual^2) / (2 * sigma2[k])
        }, numeric(nrow(Y)))
        dim(value) <- c(nrow(Y), nrow(eta))
    } else stop("Unsupported response family.", call. = FALSE)
    if (any(!is.finite(value))) stop("Nonfinite response log likelihood.", call. = FALSE)
    value
}

graphmode_update_expert <- function(current, Yk, config, sigma2 = NULL) {
    TT <- nrow(config$Fmat)
    p <- ncol(config$Fmat)
    N <- nrow(Yk)
    started <- proc.time()[[3L]]
    accepted <- NA
    log_acceptance <- NA_real_
    movement <- 0
    r <- numeric()
    factor_residual <- 0
    root_residual <- 0
    root_reciprocal_condition <- NA_real_ # No observation conditional for an empty expert.
    if (N == 0L) {
        updated <- graphmode_prior_expert(config$Fmat, config$m0, config$C0,
                                         config$G, config$W, config$dynamics)
    } else {
        S <- colSums(Yk)
        eta <- rowSums(current * config$Fmat)
        if (config$family == "poisson") {
            r <- gmde_make_nb_r(S, config$rho)
            shapes <- S + r
            if (any(!is.finite(shapes)) || any(shapes != round(shapes)) ||
                any(shapes > .Machine$integer.max)) stop("Invalid exact PG shapes.", call. = FALSE)
            precision <- graphmode_pg(shapes, eta + log(N) - log(r))
            if (length(precision) != TT || any(!is.finite(precision)) || any(precision <= 0)) {
                stop("Invalid exact PG output.", call. = FALSE)
            }
            natural <- (S - r) / 2 + precision * (log(r) - log(N))
        } else {
            precision <- rep(N / sigma2, TT)
            natural <- S / sigma2
        }
        if (config$dynamics == "static") {
            condition <- graphmode_information_condition(config$m0,
                t(gmde_chol_spd(config$C0)), config$Fmat, precision, natural)
            beta <- graphmode_gaussian_draw(condition)
            proposal <- matrix(rep(beta, each = TT), TT, p)
            factor_residual <- condition$factor_residual
        } else {
            condition <- graphmode_ffbs(precision, natural, config$Fmat,
                config$m0, config$C0, config$G, config$W)
            proposal <- condition$theta
            factor_residual <- condition$factor_residual
        }
        root_residual <- condition$root_residual
        root_reciprocal_condition <- condition$root_reciprocal_condition
        eta_proposal <- rowSums(proposal * config$Fmat)
        if (config$family == "poisson") {
            old_mu <- gmde_mu_from_eta(N, eta)
            new_mu <- gmde_mu_from_eta(N, eta_proposal)
            if (any(!is.finite(c(old_mu, new_mu)))) stop("Nonfinite Poisson mean.", call. = FALSE)
            ratio <- sum(gmde_poisson_nb_log_ratio(S, new_mu, r) -
                             gmde_poisson_nb_log_ratio(S, old_mu, r))
            if (!is.finite(ratio)) stop("Nonfinite Poisson/NB correction.", call. = FALSE)
            log_acceptance <- min(0, ratio)
            accepted <- log(graphmode_uniform(1L)) <= log_acceptance
            updated <- if (accepted) proposal else current
            if (accepted) movement <- sum(pmax(S, 1) * (eta_proposal - eta)^2)
        } else {
            updated <- proposal
            movement <- sum(precision * (eta_proposal - eta)^2)
        }
    }
    if (config$family == "gaussian") {
        residual <- sweep(Yk, 2L, rowSums(updated * config$Fmat), "-")
        sigma2 <- 1 / graphmode_gamma(config$variance_prior[1L] + N * TT / 2,
            config$variance_prior[2L] + sum(residual^2) / 2)
        graphmode_positive(sigma2, "sampled variance")
    }
    if (!is.finite(movement)) stop("Nonfinite accepted movement.", call. = FALSE)
    list(theta = updated, sigma2 = sigma2, empty = N == 0L,
         accepted = accepted, log_acceptance = log_acceptance, r = r,
         movement = movement, seconds = proc.time()[[3L]] - started,
         factor_residual = factor_residual, root_residual = root_residual,
         root_reciprocal_condition = root_reciprocal_condition)
}
