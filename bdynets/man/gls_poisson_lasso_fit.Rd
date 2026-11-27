\name{gls_poisson_lasso_fit}
\alias{gls_poisson_lasso_fit}
\alias{predict.gls_poisson_lasso}
\title{All-pairs Poisson lasso solution path}
\usage{
gls_poisson_lasso_fit(Y, X, covariates, train_rows, penalties = NULL,
  n_penalties = 30L, min_ratio = .005, gradient_tolerance = 3e-4,
  max_iter = 8000L)
\method{predict}{gls_poisson_lasso}(object, X, covariates, index, ...)
}
\arguments{
\item{Y}{Time-by-target matrix of nonnegative integer observations.}
\item{X}{Time-by-source lagged predictor matrix, preprocessed on training rows. For prediction supply the desired rows in their original predictor units.}
\item{covariates}{Numeric covariate matrix aligned with the rows of X.}
\item{train_rows}{Consecutive training indices.}
\item{penalties}{Optional decreasing nonnegative penalty vector for the objective of mean negative log likelihood per target plus the total absolute lag coefficient penalty.}
\item{n_penalties, min_ratio}{Number and minimum-to-maximum ratio of the default logarithmic penalty grid.}
\item{gradient_tolerance, max_iter}{Gradient-mapping convergence threshold and maximum iterations per penalty.}
\item{object}{Fitted solution path.}
\item{index}{Selected penalty index.}
\item{...}{Reserved for the prediction generic.}
}
\description{Fits targetwise Poisson log-linear regressions over all lagged sources using a common lasso penalty. Intercepts and supplied covariates are unpenalized. Proximal gradient iterations with backtracking are warm-started along a decreasing penalty sequence.}
\value{The fitted object records path coefficients, penalties and coefficient-row mapping. Prediction returns a matrix of conditional means for the requested penalty.}
