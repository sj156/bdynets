#' Forecast independent count series with a state-augmented Poisson--Gamma filter
#'
#' Implements the discount-state observation model of Irie, Glynn and Aktekin
#' (2022), doi:10.1214/21-AOAS1505. This benchmark chooses the discount AR(1)
#' hyperparameters per series by validation log predictive density. Conditional
#' Gamma updates are analytic and the latent logit discount is particle-filtered.
#' @param Y Time-by-series count matrix.
#' @param train,validation,test Consecutive row indices, jointly spanning Y in order.
#' @param grid Data frame with discount median `discount`, persistence `phi` and
#'   innovation standard deviation `sigma`. Defaults to the manuscript's 84 settings.
#' @param tuning_particles,particles Number of particles during model selection
#'   and final sequential filtering.
#' @param seed Integer random seed. Candidate settings use common random numbers.
#' @return A list of test-row matrices `mean`, `logprob`, `lower` and `upper`
#'   (pointwise 95 percent predictive intervals), selected settings and provenance.
#' @export
gls_sa_pgss <- function(Y, train, validation, test,
    grid = expand.grid(discount = c(.70,.75,.80,.90,.95,.98,.995),
                       phi = c(.5,.8,.95), sigma = c(.02,.05,.15,.30)),
    tuning_particles = 300L, particles = 2000L, seed = 1L) {
  Y <- as.matrix(Y)
  if (!is.numeric(Y) || nrow(Y) < 3L || ncol(Y) < 1L ||
      any(!is.finite(Y) | Y < 0 | Y != floor(Y)))
    stop("Y must be a nonnegative integer-valued count matrix with at least three rows.")
  if (min(length(train), length(validation), length(test)) < 1L ||
      !identical(as.numeric(c(train, validation, test)), as.numeric(seq_len(nrow(Y)))))
    stop("Training, validation and test indices must partition all rows chronologically.")
  if (!is.data.frame(grid) || !all(c("discount","phi","sigma") %in% names(grid)) ||
      nrow(grid) < 1L || !all(vapply(grid[c("discount", "phi", "sigma")], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(grid[c("discount", "phi", "sigma")]))) ||
      any(grid$discount <= 0 | grid$discount >= 1 | abs(grid$phi) >= 1 | grid$sigma < 0))
    stop("Invalid discount-state parameters.")
  integers <- c(tuning_particles, particles, seed)
  if (length(integers) != 3L || any(!is.finite(integers) | integers != floor(integers)) ||
      any(integers < 1 | integers >= .Machine$integer.max))
    stop("Particle counts and seed must be positive bounded integers.")
  old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit(if (is.null(old_seed)) rm(".Random.seed", envir = .GlobalEnv)
          else assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  k <- ncol(Y)
  new_matrix <- function() matrix(NA_real_, length(test), k,
                                  dimnames = list(rownames(Y)[test], colnames(Y)))
  out <- list(mean = new_matrix(), logprob = new_matrix(), lower = new_matrix(), upper = new_matrix())
  selected <- integer(k)
  validation_scores <- matrix(NA_real_, nrow(grid), k)
  for (a in seq_len(k)) {
    edge_seed <- as.integer((as.double(seed) + 104729 * a) %% (.Machine$integer.max - 1) + 1)
    for (j in seq_len(nrow(grid))) {
      set.seed(edge_seed)
      validation_scores[j,a] <- .gls_sa_pgss_filter(Y[,a], grid[j,], tuning_particles,
                                                    validation, score_only = TRUE)
    }
    selected[a] <- which.max(validation_scores[,a])
    set.seed(as.integer((as.double(edge_seed) + 1) %% (.Machine$integer.max - 1) + 1))
    forecast <- .gls_sa_pgss_filter(Y[,a], grid[selected[a],], particles, test)
    for (field in names(out)) out[[field]][,a] <- forecast[,field]
  }
  out$selected <- grid[selected,,drop = FALSE]
  out$validation_scores <- validation_scores
  out$provenance <- list(model = "Irie, Glynn & Aktekin (2022), sa-PGSS",
    doi = "10.1214/21-AOAS1505", implementation = "validation-tuned particle filter",
    initial_rate_prior = "Gamma(shape=1,rate=1)", initial_logit_discount = "stationary AR(1)",
    grid = grid, tuning_particles = tuning_particles, particles = particles, seed = seed,
    intervals = "2.5% and 97.5% empirical count quantiles, one draw per predictive particle")
  out
}

.gls_sa_pgss_filter <- function(y, parameters, n, rows, score_only = FALSE) {
  m <- stats::qlogis(parameters$discount)
  phi <- parameters$phi
  sigma <- parameters$sigma
  z <- stats::rnorm(n, m, sigma / sqrt(1-phi^2))
  alpha <- rate <- rep(1, n)
  if (!score_only) out <- matrix(NA_real_, length(rows), 4L,
                                 dimnames = list(NULL, c("mean","logprob","lower","upper")))
  total <- 0
  for (t in seq_len(max(rows))) {
    z <- stats::rnorm(n, m + phi*(z-m), sigma)
    d <- stats::plogis(z)
    size <- d*alpha
    rate_prior <- d*rate
    mu <- alpha/rate
    log_weight <- stats::dnbinom(y[t], size = size, mu = mu, log = TRUE)
    pivot <- max(log_weight)
    weight <- exp(log_weight-pivot)
    if (t >= rows[1L]) {
      log_prob <- pivot + log(mean(weight))
      total <- total + log_prob
      if (!score_only) {
        count <- stats::rnbinom(n, size = size, mu = mu)
        bounds <- stats::quantile(count, c(.025,.975), names = FALSE, type = 1)
        out[t-rows[1L]+1L,] <- c(mean(mu), log_prob, bounds)
      }
    }
    # Systematic resampling of the updated particle triples.
    cumulative <- pmin(1, cumsum(weight / sum(weight)))
    cumulative[n] <- 1
    draws <- ((seq_len(n)-1L) + stats::runif(1L)) / n
    ancestors <- pmin(n, findInterval(draws, cumulative) + 1L)
    alpha <- size[ancestors] + y[t]
    rate <- rate_prior[ancestors] + 1
    z <- z[ancestors]
  }
  if (score_only) total else out
}
