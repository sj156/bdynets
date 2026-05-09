# Generated from _main.Rmd: do not edit by hand

#' A helper function to print the progress of a simulation. Place directly at
#' the beginning of the loop, before any computation happens.
#' @param isim isim.
#' @param nsim number of sim.
#' @param type type of job you're running. Defaults to "simulation".
#' @param lapsetime lapsed time, in seconds (by default).
#' @param lapsetimeunit "second".
#' @param start.time Start time, usually obtained using \code{Sys.time()}
#' @param fill Whether or not to fill the line.
#' @param beep Whether to beep when done.
printprogress <- function(isim, nsim, type="simulation", lapsetime=NULL,
                          lapsetimeunit="seconds", start.time=NULL,
                          fill=FALSE, beep=FALSE){

    ## If lapse time is present, then use it
    if(fill) cat(fill=TRUE)
    if(is.null(lapsetime) & is.null(start.time)){
            cat("\r", type, " ", isim, "out of", nsim)
    } else {
        if(!is.null(start.time)){
          if(isim == 1){
            lapsetime = 0
            remainingtime = "unknown"
            endtime = "unknown time"
          } else {
            lapsetime = round(difftime(Sys.time(), start.time,
                                       units = "secs"), 0)
            remainingtime = round(lapsetime * (nsim - (isim - 1)) / (isim - 1), 0)
            endtime = strftime((Sys.time() + remainingtime))
          }
        }
        cat("\r", type, " ", isim, "out of", nsim, "with lapsed time",
            lapsetime, lapsetimeunit, "and remaining time", remainingtime,
            lapsetimeunit, "and will finish at", endtime, ".")
        if(beep & isim==nsim){beepr::beep()}
    }
    if(fill) cat(fill=TRUE)
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


#' Compute Adjusted Rand Index for clustering accuracy.
#'
#' @param x First clustering vector.
#' @param y Second clustering vector.
#' 
#' @export 
adj_rand_index <- function(x, y){
    tab <- table(x, y)

    choose2 <- function(z) z * (z - 1) / 2

    nij <- as.vector(tab)
    ai  <- rowSums(tab)
    bj  <- colSums(tab)

    sum_nij <- sum(choose2(nij))
    sum_ai  <- sum(choose2(ai))
    sum_bj  <- sum(choose2(bj))

    n <- sum(tab)
    total <- choose2(n)

    expected <- sum_ai * sum_bj / total
    max_idx  <- 0.5 * (sum_ai + sum_bj)

    if(max_idx == expected) return(0)

    ret <- (sum_nij - expected) / (max_idx - expected)
    return(ret)
}

#' Compute best label accuracy over all permutations.
#'
#' @param true True cluster labels.
#' @param est Estimated cluster labels.
#' @param K Number of clusters.
#' 
#' @export 
best_label_accuracy <- function(true, est, K){
    perms <- as.matrix(expand.grid(rep(list(seq_len(K)), K)))
    perms <- perms[apply(perms, 1, function(z) length(unique(z)) == K), , drop = FALSE]

    acc <- apply(perms, 1, function(map){
        mean(true == map[est])
    })

    ret <- max(acc)
    return(ret)
}

#' Compute the mode of an integer vector.
#'
#' @param x Integer vector.
#' @param K Maximum possible value (number of bins).
mode_int <- function(x, K){
    ret <- which.max(tabulate(x, nbins = K))
    return(ret)
}
