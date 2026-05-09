# Generated from _main.Rmd: do not edit by hand

#' Run the full Gibbs sampler for Bayesian dynamic network clustering.
#'
#' This function runs the complete MCMC algorithm for fitting Bayesian
#' dynamic network models with Polya-Gamma augmentation and forward-filtering
#' backward-sampling (FFBS).
#'
#' @param Y Response matrix (n x TT) of count data.
#' @param Fmat Design matrix (TT x p) for the state-space model.
#' @param K Number of clusters.
#' @param n_iter Total number of MCMC iterations.
#' @param burn Number of burn-in iterations to discard.
#' @param alpha_pi Dirichlet prior concentration for cluster probabilities.
#'   Can be a scalar (recycled) or vector of length K. Defaults to rep(1, K).
#' @param m0 Prior mean vector for initial state (length p).
#' @param C0 Prior covariance matrix for initial state (p x p).
#' @param G State transition matrix (p x p).
#' @param W State evolution covariance matrix (p x p).
#' @param r_tune Tuning parameter for Polya-Gamma augmentation.
#'   If NULL, automatically computed from Y.
#' @param Z_true Optional true cluster labels for diagnostics.
#' @param relabel_freq Frequency of label-switching correction.
#'   Defaults to 1 (every iteration).
#' @param print_freq Frequency of progress printing. Defaults to 100.
#' @param store_theta Whether to store full theta samples. Defaults to FALSE.
#' @param seed Optional random seed for reproducibility.
#'
#' @return A list containing:
#'   \item{theta}{Posterior samples of state parameters (if store_theta = TRUE).}
#'   \item{pi}{Posterior samples of cluster probabilities.}
#'   \item{Z}{Posterior samples of cluster allocations.}
#'   \item{lambda}{Posterior samples of fitted intensities.}
#'   \item{mean_lambda}{Posterior samples of mean intensities per cluster.}
#'   \item{size}{Posterior samples of cluster sizes.}
#'   \item{ari}{Adjusted Rand Index at each iteration (if Z_true provided).}
#'   \item{acc}{Best label accuracy at each iteration (if Z_true provided).}
#'   \item{accept_rate}{Acceptance rate diagnostics.}
#'   \item{settings}{List of MCMC settings used.}
#'
#' @export
gibbs_sampler <- function(Y, Fmat, K,
                          n_iter = 600,
                          burn = 300,
                          alpha_pi = NULL,
                          m0 = NULL,
                          C0 = NULL,
                          G = NULL,
                          W = NULL,
                          r_tune = NULL,
                          Z_true = NULL,
                          relabel_freq = 1,
                          print_freq = 100,
                          store_theta = FALSE,
                          seed = NULL){

    ## Set seed for reproducibility
    if(!is.null(seed)){
        set.seed(seed)
    }

    ## Extract dimensions
    n  <- nrow(Y)
    TT <- ncol(Y)
    p  <- ncol(Fmat)

    ## Set default MCMC settings if not provided
    if(is.null(alpha_pi)){
        alpha_pi <- rep(1, K)
    }

    if(is.null(m0)){
        m0 <- c(log(mean(Y) + 0.1), 0)
        if(length(m0) < p){
            m0 <- c(m0, rep(0, p - length(m0)))
        }
    }

    if(is.null(C0)){
        C0 <- diag(c(5, 2))
        if(nrow(C0) < p){
            C0 <- diag(p) * 5
        }
    }

    if(is.null(G)){
        G <- diag(p)
    }

    if(is.null(W)){
        W <- diag(c(0.02, 0.005))
        if(nrow(W) < p){
            W <- diag(p) * 0.01
        }
    }

    if(is.null(r_tune)){
        r_tune <- (max(Y) + sqrt(max(Y)) * 3) * 50
    }

    ## Initialize chain
    Z <- sample(seq_len(K), size = n, replace = TRUE)
    pi_vec <- rdirichlet1(alpha_pi + tabulate(Z, nbins = K))

    theta <- array(NA, dim = c(K, TT, p))
    for(k in seq_len(K)){
        theta[k, , ] <- sample_prior_theta(TT, m0, C0, G, W)
    }

    ## Initial label correction
    rel <- relabel_by_mean_intensity(theta, pi_vec, Z, Fmat)
    theta <- rel$theta
    pi_vec <- rel$pi
    Z <- rel$Z

    ## Storage
    pi_store <- matrix(NA, nrow = n_iter, ncol = K)
    size_store <- matrix(NA, nrow = n_iter, ncol = K)
    mean_lambda_store <- matrix(NA, nrow = n_iter, ncol = K)
    lambda_store <- array(NA, dim = c(n_iter, K, TT))
    Z_store <- matrix(NA_integer_, nrow = n_iter, ncol = n)

    theta_store <- NULL
    if(store_theta){
        theta_store <- array(NA, dim = c(n_iter, K, TT, p))
    }

    ari_store <- numeric(n_iter)
    acc_store <- numeric(n_iter)

    start_time <- Sys.time()

    ## Run MCMC
    for(iter in seq_len(n_iter)){

        ## 1. Sample theta_{k,1:T} using PG + FFBS
        theta <- sample_theta_all(
            Y = Y,
            Z = Z,
            theta = theta,
            Fmat = Fmat,
            m0 = m0,
            C0 = C0,
            G = G,
            W = W,
            r_tune = r_tune,
            K = K
        )

        ## 2. Sample allocations Z_i conditional on theta
        Z <- update_Z_given_theta(Y, theta, Fmat, pi_vec)

        ## 3. Sample pi (cluster probabilities)
        Nk <- tabulate(Z, nbins = K)
        pi_vec <- rdirichlet1(alpha_pi + Nk)

        ## 4. Label-switching correction
        ##    This imposes the ordering:
        ##    cluster 1 has smallest average intensity,
        ##    cluster K has largest average intensity.
        if(iter %% relabel_freq == 0){
            rel <- relabel_by_mean_intensity(theta, pi_vec, Z, Fmat)
            theta <- rel$theta
            pi_vec <- rel$pi
            Z <- rel$Z
        }

        ## Store samples
        lambda_cur <- get_lambda(theta, Fmat)

        pi_store[iter, ] <- pi_vec
        size_store[iter, ] <- tabulate(Z, nbins = K)
        mean_lambda_store[iter, ] <- rowMeans(lambda_cur)
        lambda_store[iter, , ] <- lambda_cur
        Z_store[iter, ] <- Z

        if(store_theta){
            theta_store[iter, , , ] <- theta
        }

        ## Diagnostics (if true labels provided)
        if(!is.null(Z_true)){
            ari_store[iter] <- adj_rand_index(Z_true, Z)
            acc_store[iter] <- best_label_accuracy(Z_true, Z, K)
        }

        ## Print progress
        if(iter %% print_freq == 0){
            printprogress(
                isim = iter,
                nsim = n_iter,
                type = "MCMC iteration",
                start.time = start_time,
                fill = TRUE
            )
            if(!is.null(Z_true)){
                cat(
                    " | ARI =", round(ari_store[iter], 3),
                    "| best accuracy =", round(acc_store[iter], 3),
                    "\n"
                )
            } else {
                cat("\n")
            }
        }
    }

    ## Compute effective sample size (optional diagnostic)
    end_time <- Sys.time()
    total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

    ## Prepare output
    ret <- list(
        theta = theta_store,
        pi = pi_store,
        Z = Z_store,
        lambda = lambda_store,
        mean_lambda = mean_lambda_store,
        size = size_store,
        ari = ari_store,
        acc = acc_store,
        settings = list(
            n_iter = n_iter,
            burn = burn,
            K = K,
            n = n,
            TT = TT,
            p = p,
            alpha_pi = alpha_pi,
            m0 = m0,
            C0 = C0,
            G = G,
            W = W,
            r_tune = r_tune,
            total_time = total_time,
            seed = seed
        )
    )

    class(ret) <- "bdynets_mcmc"
    return(ret)
}

#' Summarize MCMC results from gibbs_sampler.
#'
#' @param object Object of class "bdynets_mcmc".
#' @param burn Number of burn-in iterations to discard.
#' @param ... Additional arguments (not used).
#'
#' @export
summary.bdynets_mcmc <- function(object, burn = NULL, ...){

    if(is.null(burn)){
        burn <- object$settings$burn
    }

    n_iter <- object$settings$n_iter
    keep_idx <- seq(burn + 1, n_iter)

    cat("Bayesian Dynamic Network MCMC Summary\n")
    cat("=====================================\n\n")

    cat("MCMC Settings:\n")
    cat("  Total iterations:", n_iter, "\n")
    cat("  Burn-in:", burn, "\n")
    cat("  Kept samples:", length(keep_idx), "\n")
    cat("  Number of clusters (K):", object$settings$K, "\n")
    cat("  Number of observations (n):", object$settings$n, "\n")
    cat("  Time points (TT):", object$settings$TT, "\n")
    cat("  State dimension (p):", object$settings$p, "\n")
    cat("  Total runtime:", round(object$settings$total_time, 2), "seconds\n\n")

    cat("Cluster Probabilities (pi) - Posterior Mean:\n")
    pi_mean <- colMeans(object$pi[keep_idx, , drop = FALSE])
    print(round(pi_mean, 4))
    cat("\n")

    cat("Cluster Sizes - Posterior Mean:\n")
    size_mean <- colMeans(object$size[keep_idx, , drop = FALSE])
    print(round(size_mean, 2))
    cat("\n")

    if(length(object$ari) > 0 && any(object$ari > 0)){
        cat("Clustering Accuracy (post burn-in):\n")
        cat("  Mean ARI:", round(mean(object$ari[keep_idx]), 4), "\n")
        cat("  Mean Best Accuracy:", round(mean(object$acc[keep_idx]), 4), "\n\n")
    }

    cat("Mean Intensity per Cluster - Posterior Mean:\n")
    lambda_mean <- colMeans(object$mean_lambda[keep_idx, , drop = FALSE])
    print(round(lambda_mean, 4))

    invisible(object)
}

#' Plot MCMC diagnostics for bdynets_mcmc object.
#'
#' @param x Object of class "bdynets_mcmc".
#' @param burn Number of burn-in iterations to discard.
#' @param type Type of plot: "trace", "density", "ari", or "all".
#' @param ... Additional arguments passed to plot.
#'
#' @export
plot.bdynets_mcmc <- function(x, burn = NULL, type = "all", ...){

    if(is.null(burn)){
        burn <- x$settings$burn
    }

    keep_idx <- seq(burn + 1, x$settings$n_iter)
    K <- x$settings$K

    if(type == "trace" || type == "all"){
        ## Trace plots for cluster probabilities
        matplot(
            keep_idx, x$pi[keep_idx, ],
            type = "l",
            lty = 1,
            col = seq_len(K),
            xlab = "Iteration",
            ylab = "Cluster Probability",
            main = "Trace Plot: Cluster Probabilities (pi)"
        )
        legend("topright", legend = paste("Cluster", seq_len(K)),
               col = seq_len(K), lty = 1, cex = 0.8)
    }

    if(type == "ari" || type == "all"){
        if(length(x$ari) > 0 && any(x$ari > 0)){
            ## ARI over iterations
            plot(
                keep_idx, x$ari[keep_idx],
                type = "l",
                col = "blue",
                lwd = 2,
                xlab = "Iteration",
                ylab = "Adjusted Rand Index",
                main = "Clustering Accuracy (ARI) Over Iterations"
            )
            abline(h = mean(x$ari[keep_idx]), col = "red", lty = 2)
        }
    }

    if(type == "size" || type == "all"){
        ## Cluster sizes over iterations
        matplot(
            keep_idx, x$size[keep_idx, ],
            type = "l",
            lty = 1,
            col = seq_len(K),
            xlab = "Iteration",
            ylab = "Cluster Size",
            main = "Trace Plot: Cluster Sizes"
        )
        legend("topright", legend = paste("Cluster", seq_len(K)),
               col = seq_len(K), lty = 1, cex = 0.8)
    }

    invisible(x)
}
