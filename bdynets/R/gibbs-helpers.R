# Generated from _main.Rmd: do not edit by hand

#' Sample a prior state trajectory from a Dynamic Linear Model (DLM).
#'
#' Simulates a state sequence \eqn{\theta_{1:T}} from a Gaussian DLM defined by:
#' \deqn{\theta_0 \sim N(m_0, C_0)}
#' \deqn{\theta_t = G \theta_{t-1} + w_t, \quad w_t \sim N(0, W)}
#'
#' @param TT Number of time steps.
#' @param m0 Prior mean vector for the initial state.
#' @param C0 Prior covariance matrix for the initial state.
#' @param G State transition matrix.
#' @param W State evolution covariance matrix.
#'
#' @return A matrix of size TT x p containing the sampled state trajectory.
sample_prior_theta <- function(TT, m0, C0, G, W){
    p <- length(m0)

    theta <- matrix(NA, nrow = TT, ncol = p)

    ## Sample initial state theta_0
    theta0 <- rmvn(m0, C0)
    prev <- theta0

    ## Forward simulation
    for(tt in seq_len(TT)){
        ## Mean of theta_t given theta_{t-1}
        mu_t <- as.vector(G %*% prev)
        
        ## Sample theta_t
        th <- rmvn(mu_t, W)
        
        theta[tt, ] <- th
        prev <- th
    }

    return(theta)
}


#' Approximate Polya-Gamma sampler.
#'
#' If BayesLogit is installed, this uses BayesLogit::rpg().
#' Otherwise, it uses a truncated infinite-sum approximation.
#'
#' @param n Number of samples.
#' @param h Shape parameter.
#' @param z Location parameter.
#' @param trunc Truncation level for approximation. Defaults to 80.
rpg <- function(n, h, z, trunc = 80){
    h <- rep(h, length.out = n)
    z <- rep(z, length.out = n)

    if(requireNamespace("BayesLogit", quietly = TRUE)){
        return(as.numeric(BayesLogit::rpg(n, h, z)))
    }

    out <- numeric(n)
    m <- seq_len(trunc)

    for(i in seq_len(n)){
        denom <- (m - 0.5)^2 + z[i]^2 / (4 * pi^2)
        g <- rgamma(trunc, shape = h[i], rate = 1)
        out[i] <- sum(g / denom) / (2 * pi^2)
    }

    return(out)
}

#' Sample omega_{k,t} once per MCMC sweep.
#'
#' @param Y Response matrix (n x TT).
#' @param Z Cluster allocation vector.
#' @param theta State parameters (K x TT x p).
#' @param Fmat Design matrix (TT x p).
#' @param r_tune Tuning parameter for Polya-Gamma augmentation.
#' @param K Number of clusters.
sample_omega_all <- function(Y, Z, theta, Fmat, r_tune, K){
    TT <- ncol(Y)

    stats <- cluster_stats(Z, Y, K)
    N <- stats$N
    S <- stats$S

    omega <- matrix(NA_real_, nrow = K, ncol = TT)

    eta <- get_eta(theta, Fmat)

    for(k in seq_len(K)){
        if(N[k] == 0) next

        for(tt in seq_len(TT)){
            psi_kt <- eta[k, tt] + log(N[k]) - log(r_tune)
            B_kt   <- r_tune + S[k, tt]

            omega[k, tt] <- max(rpg(1, B_kt, psi_kt), 1e-8)
        }
    }

    ret <- list(omega = omega, stats = stats)
    return(ret)
}

#' Forward-filtering backward-sampling for one cluster under the
#' Polya-Gamma Gaussian representation.
#'
#' @param N Cluster size.
#' @param S Cluster-specific sufficient statistics.
#' @param omega_k Polya-Gamma auxiliary variables for cluster k.
#' @param Fmat Design matrix (TT x p).
#' @param m0 Prior mean vector.
#' @param C0 Prior covariance matrix.
#' @param G State transition matrix.
#' @param W State evolution covariance.
#' @param r_tune Tuning parameter for Polya-Gamma augmentation.
ffbs_one_cluster <- function(N, S, omega_k, Fmat,
                             m0, C0, G, W, r_tune){

    TT <- length(S)
    p  <- length(m0)

    ## If the cluster is empty, draw from the prior DLM.
    if(N == 0){
        return(sample_prior_theta(TT, m0, C0, G, W))
    }

    rvec  <- rep(r_tune, TT)
    bar_r <- log(rvec) - log(N)
    kappa <- 0.5 * (S - rvec)

    omega_k <- pmax(omega_k, 1e-8)

    ## Pseudo observation:
    ## y*_{k,t} = F' theta_{k,t} + epsilon, epsilon ~ N(0, 1 / omega)
    ystar <- bar_r + kappa / omega_k
    V     <- 1 / omega_k

    a_pred <- matrix(NA, nrow = TT, ncol = p)
    m_filt <- matrix(NA, nrow = TT, ncol = p)

    R_pred <- array(NA, dim = c(p, p, TT))
    C_filt <- array(NA, dim = c(p, p, TT))

    m_prev <- m0
    C_prev <- C0

    ## Forward filtering
    for(tt in seq_len(TT)){
        Ft <- matrix(Fmat[tt, ], ncol = 1)

        a <- as.vector(G %*% m_prev)
        R <- G %*% C_prev %*% t(G) + W
        R <- (R + t(R)) / 2

        f <- as.numeric(t(Ft) %*% a)
        Q <- as.numeric(t(Ft) %*% R %*% Ft + V[tt])

        K_gain <- as.vector(R %*% Ft) / Q

        m <- a + K_gain * (ystar[tt] - f)
        C <- R - (R %*% Ft %*% t(Ft) %*% R) / Q
        C <- (C + t(C)) / 2

        a_pred[tt, ] <- a
        R_pred[, , tt] <- R

        m_filt[tt, ] <- m
        C_filt[, , tt] <- C

        m_prev <- m
        C_prev <- C
    }

    ## Backward sampling
    theta <- matrix(NA, nrow = TT, ncol = p)

    theta[TT, ] <- rmvn(m_filt[TT, ], C_filt[, , TT])

    if(TT > 1){
        for(tt in seq(TT - 1, 1)){
            B_smooth <-
                C_filt[, , tt] %*% t(G) %*% solve(R_pred[, , tt + 1])

            h <- m_filt[tt, ] +
                B_smooth %*% (theta[tt + 1, ] - a_pred[tt + 1, ])

            H <- C_filt[, , tt] -
                B_smooth %*% R_pred[, , tt + 1] %*% t(B_smooth)

            H <- (H + t(H)) / 2

            theta[tt, ] <- rmvn(as.vector(h), H)
        }
    }

    return(theta)
}

#' Sample all state trajectories using FFBS.
#'
#' @param Y Response matrix (n x TT).
#' @param Z Cluster allocation vector.
#' @param theta Current state parameters (K x TT x p).
#' @param Fmat Design matrix (TT x p).
#' @param m0 Prior mean vector.
#' @param C0 Prior covariance matrix.
#' @param G State transition matrix.
#' @param W State evolution covariance.
#' @param r_tune Tuning parameter for Polya-Gamma augmentation.
#' @param K Number of clusters.
sample_theta_all <- function(Y, Z, theta, Fmat,
                             m0, C0, G, W,
                             r_tune, K){

    out <- sample_omega_all(Y, Z, theta, Fmat, r_tune, K)

    omega <- out$omega
    N <- out$stats$N
    S <- out$stats$S

    TT <- ncol(Y)
    p  <- length(m0)

    theta_new <- array(NA, dim = c(K, TT, p))

    for(k in seq_len(K)){
        theta_new[k, , ] <- ffbs_one_cluster(
            N = N[k],
            S = S[k, ],
            omega_k = omega[k, ],
            Fmat = Fmat,
            m0 = m0,
            C0 = C0,
            G = G,
            W = W,
            r_tune = r_tune
        )
    }

    return(theta_new)
}

#' Sample cluster allocations Z_i conditional on theta.
#'
#' @param Y Response matrix (n x TT).
#' @param theta State parameters (K x TT x p).
#' @param Fmat Design matrix (TT x p).
#' @param pi_vec Cluster probability vector.
update_Z_given_theta <- function(Y, theta, Fmat, pi_vec){
    n  <- nrow(Y)
    TT <- ncol(Y)
    K  <- dim(theta)[1]

    eta <- get_eta(theta, Fmat)
    lambda <- exp(eta)

    Z_new <- integer(n)

    for(i in seq_len(n)){
        logp <- numeric(K)

        for(k in seq_len(K)){
            logp[k] <-
                log(pi_vec[k] + 1e-16) +
                sum(Y[i, ] * eta[k, ] - lambda[k, ])
        }

        prob <- exp(logp - logsumexp(logp))
        Z_new[i] <- sample(seq_len(K), size = 1, prob = prob)
    }

    return(Z_new)
}

#' Label-switching correction via ordering by average fitted intensity.
#'
#' Simple identifiability constraint: order clusters by average
#' fitted intensity. This is suitable when true clusters are generated
#' with increasing average intensity.
#'
#' @param theta State parameters (K x TT x p).
#' @param pi_vec Cluster probability vector.
#' @param Z Cluster allocation vector.
#' @param Fmat Design matrix (TT x p).
relabel_by_mean_intensity <- function(theta, pi_vec, Z, Fmat){
    K <- dim(theta)[1]

    lambda <- get_lambda(theta, Fmat)
    mean_lambda <- rowMeans(lambda)

    old_order <- order(mean_lambda)

    theta_new <- theta[old_order, , , drop = FALSE]
    pi_new <- pi_vec[old_order]

    old_to_new <- integer(K)
    old_to_new[old_order] <- seq_len(K)

    Z_new <- old_to_new[Z]

    ret <- list(
        theta = theta_new,
        pi = pi_new,
        Z = Z_new,
        order = old_order
    )
    return(ret)
}

