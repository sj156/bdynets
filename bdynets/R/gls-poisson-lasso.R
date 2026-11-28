# Fit a solution path with an unpenalized intercept and covariate coefficients.
gls_poisson_lasso_fit <- function(Y, X, covariates, train_rows, penalties = NULL,
                                  n_penalties = 30L, min_ratio = .005,
                                  gradient_tolerance = 3e-4, max_iter = 8000L) {
  Y <- .validate_count_matrix(Y)
  stopifnot(is.numeric(X), identical(dim(X),dim(Y)), all(is.finite(X)),
    is.matrix(covariates),nrow(covariates)==nrow(Y),all(is.finite(covariates)),
    length(train_rows)>=2L,all(is.finite(train_rows)),all(train_rows==as.integer(train_rows)),
    all(diff(train_rows)==1L),min(train_rows)>=1L,max(train_rows)<=nrow(Y),
    length(n_penalties)==1L,n_penalties>=1L,n_penalties==as.integer(n_penalties),
    min_ratio>0,min_ratio<=1,gradient_tolerance>0,max_iter>=1,max_iter==as.integer(max_iter))
  Z <- cbind(1,covariates[train_rows,,drop=FALSE], X[train_rows,,drop=FALSE])
  Y <- Y[train_rows,,drop=FALSE]
  k <- ncol(Y)
  penalized <- tail(seq_len(ncol(Z)),k)
  B <- matrix(0,ncol(Z),k)
  B[1,] <- log(pmax(colMeans(Y),.5/nrow(Y)))
  loss <- function(eta) sum(exp(eta)-Y*eta)/nrow(Z)
  if(is.null(penalties)) {
    max_lambda <- max(abs(crossprod(Z[,penalized,drop=FALSE],exp(Z%*%B)-Y)))/nrow(Z)
    penalties <- max(max_lambda,1e-6)*exp(seq(0,log(min_ratio),length.out=n_penalties))
  }
  if(!length(penalties) || any(!is.finite(penalties) | penalties<0) ||
      any(diff(penalties)>0)) stop("Specify finite nonincreasing nonnegative penalties.")
  fits <- vector("list",length(penalties))
  step <- .02
  for (i in seq_along(penalties)) {
    converged <- FALSE
    for (it in seq_len(max_iter)) {
      eta <- Z %*% B
      objective <- loss(eta)
      grad <- crossprod(Z,exp(eta)-Y)/nrow(Z)
      step <- min(1,step*1.15)
      repeat {
        next_b <- B-step*grad
        v <- next_b[penalized,,drop=FALSE]
        next_b[penalized,] <- sign(v)*pmax(0,abs(v)-step*penalties[i])
        delta <- next_b-B
        bound <- objective+sum(grad*delta)+sum(delta^2)/(2*step)
        if (loss(Z%*%next_b) <= bound+1e-10) break
        step <- step/2
        if (step < 1e-14) stop("Poisson lasso line search failed")
      }
      B <- next_b
      if (max(abs(delta))/step <= gradient_tolerance) { converged <- TRUE; break }
    }
    if (!converged) stop("Poisson lasso failed the gradient-mapping tolerance")
    fits[[i]] <- B
  }
  structure(list(coefficients=fits,penalties=penalties,lag_rows=penalized,
                 n_series=k,n_covariates=ncol(covariates)),class="gls_poisson_lasso")
}

predict.gls_poisson_lasso <- function(object, X, covariates, index, ...) {
  stopifnot(is.matrix(X),all(is.finite(X)),ncol(X)==object$n_series,
    is.matrix(covariates),all(is.finite(covariates)),ncol(covariates)==object$n_covariates,
    nrow(X)==nrow(covariates),length(index)==1L,index %in% seq_along(object$penalties))
  exp(cbind(1,covariates,X) %*% object$coefficients[[index]])
}
