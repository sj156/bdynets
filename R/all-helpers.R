# Generated from _main.Rmd: do not edit by hand

#' Stable computation of the log-sum-exp function.
#'
#' @param x Numeric vector.
logsumexp <- function(x){
    m <- max(x)
    ret <- m + log(sum(exp(x - m)))
    return(ret)
}

#' Helper function to sample from a Dirichlet distribution.
#'
#' @param alpha Numeric vector of Dirichlet concentration parameters.
rdirichlet1 <- function(alpha){
    z <- rgamma(length(alpha), shape = alpha, rate = 1)
    ret <- z / sum(z)
    return(ret)
}

#' Helper function to compute a Cholesky factor for a numerically symmetric
#' positive definite matrix.
#'
#' @param Sigma Covariance matrix.
chol_spd <- function(Sigma){
    Sigma <- (Sigma + t(Sigma)) / 2
    p <- nrow(Sigma)

    for(eps in c(0, 1e-10, 1e-8, 1e-6, 1e-4)){
        out <- try(chol(Sigma + eps * diag(p)), silent = TRUE)
        if(!inherits(out, "try-error")){
            return(out)
        }
    }

    stop("Matrix is not numerically positive definite.")
}

#' Helper function to sample from a multivariate normal distribution.
#'
#' @param mu Mean vector.
#' @param Sigma Covariance matrix.
rmvn <- function(mu, Sigma){
    R <- chol_spd(Sigma)
    ret <- as.vector(mu + t(R) %*% rnorm(length(mu)))
    return(ret)
}

#' Helper function to compute cluster counts and cluster-specific sums.
#'
#' @param Z Vector of cluster allocations.
#' @param Y Matrix of observations.
#' @param K Number of clusters.
cluster_stats <- function(Z, Y, K){
    TT <- ncol(Y)

    N <- tabulate(Z, nbins = K)
    S <- matrix(0, nrow = K, ncol = TT)

    for(k in seq_len(K)){
        idx <- which(Z == k)
        if(length(idx) > 0){
            S[k, ] <- colSums(Y[idx, , drop = FALSE])
        }
    }

    ret <- list(N = N, S = S)
    return(ret)
}

#' Helper function to compute eta from theta and the design matrix.
#'
#' @param theta Array of parameters.
#' @param Fmat Design matrix.
get_eta <- function(theta, Fmat){
    K <- dim(theta)[1]
    TT <- dim(theta)[2]
    p <- dim(theta)[3]

    eta <- matrix(0, nrow = K, ncol = TT)

    for(k in seq_len(K)){
        theta_k <- matrix(theta[k, , ], nrow = TT, ncol = p)
        eta[k, ] <- rowSums(Fmat * theta_k)
    }

    return(eta)
}

#' Helper function to compute lambda from theta and the design matrix.
#'
#' @param theta Array of parameters.
#' @param Fmat Design matrix.
get_lambda <- function(theta, Fmat){
    ret <- exp(get_eta(theta, Fmat))
    return(ret)
}

#' Stable computation of the log-sum-exp function.
#'
#' @param x Numeric vector.
logsumexp <- function(x){
    m <- max(x)
    ret <- m + log(sum(exp(x - m)))
    return(ret)
}

#' Helper function to sample from a Dirichlet distribution.
#'
#' @param alpha Numeric vector of Dirichlet concentration parameters.
rdirichlet1 <- function(alpha){
    z <- rgamma(length(alpha), shape = alpha, rate = 1)
    ret <- z / sum(z)
    return(ret)
}

#' Helper function to compute a Cholesky factor for a numerically symmetric
#' positive definite matrix.
#'
#' @param Sigma Covariance matrix.
chol_spd <- function(Sigma){
    Sigma <- (Sigma + t(Sigma)) / 2
    p <- nrow(Sigma)

    for(eps in c(0, 1e-10, 1e-8, 1e-6, 1e-4)){
        out <- try(chol(Sigma + eps * diag(p)), silent = TRUE)
        if(!inherits(out, "try-error")){
            return(out)
        }
    }

    stop("Matrix is not numerically positive definite.")
}

#' Helper function to sample from a multivariate normal distribution.
#'
#' @param mu Mean vector.
#' @param Sigma Covariance matrix.
rmvn <- function(mu, Sigma){
    R <- chol_spd(Sigma)
    ret <- as.vector(mu + t(R) %*% rnorm(length(mu)))
    return(ret)
}

#' Helper function to compute cluster counts and cluster-specific sums.
#'
#' @param Z Vector of cluster allocations.
#' @param Y Matrix of observations.
#' @param K Number of clusters.
cluster_stats <- function(Z, Y, K){
    TT <- ncol(Y)

    N <- tabulate(Z, nbins = K)
    S <- matrix(0, nrow = K, ncol = TT)

    for(k in seq_len(K)){
        idx <- which(Z == k)
        if(length(idx) > 0){
            S[k, ] <- colSums(Y[idx, , drop = FALSE])
        }
    }

    ret <- list(N = N, S = S)
    return(ret)
}

#' Helper function to compute eta from theta and the design matrix.
#'
#' @param theta Array of parameters.
#' @param Fmat Design matrix.
get_eta <- function(theta, Fmat){
    K <- dim(theta)[1]
    TT <- dim(theta)[2]
    p <- dim(theta)[3]

    eta <- matrix(0, nrow = K, ncol = TT)

    for(k in seq_len(K)){
        theta_k <- matrix(theta[k, , ], nrow = TT, ncol = p)
        eta[k, ] <- rowSums(Fmat * theta_k)
    }

    return(eta)
}

#' Helper function to compute lambda from theta and the design matrix.
#'
#' @param theta Array of parameters.
#' @param Fmat Design matrix.
get_lambda <- function(theta, Fmat){
    ret <- exp(get_eta(theta, Fmat))
    return(ret)
}
