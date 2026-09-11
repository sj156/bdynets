#' Fit a fixed-K Gaussian/static graph-informed mixture
#'
#' Each series has one label for its entire observed history. All components,
#' including empty components, remain in the chain. Raw labels are stored with
#' no within-chain relabeling. Inputs must be complete and use one common ordered
#' time grid. Row names of Y are required and must match graph IDs in order.
#'
#' Each sweep updates experts, gate white coordinates jointly by ESS, guidance
#' weights by logit MH, then allocations. Guidance scales are fixed throughout.
#' Diagnostic collection consumes no additional random numbers. Numerical
#' failures stop the fit with the sweep number; no incomplete fit is returned.
#' @param Y Complete finite numeric n-by-T panel, with series IDs as row names.
#' @param graph A gmde_graph object.
#' @param K Fixed positive integer component count; may exceed n.
#' @param expert A gmde_expert object.
#' @param gate A gmde_gate object, or a named list of its arguments.
#' @param mcmc A gmde_mcmc object, or a named list of its arguments.
#' @return A versioned gmde_fit containing retained beta (draw-by-K-by-p), sigma2
#'   (draw-by-K), Z (draw-by-n), a (draw-by-K, zero columns for K=1), and guidance
#'   logits v (draw-by-number-of-free-weights). Includes the final white state,
#'   graph and expert specifications, diagnostics, IDs and reproducibility
#'   metadata. White states/utilities are not stored for every draw. This version
#'   does not implement prediction or convergence certification.
#' @examples
#' ids <- c("u1", "u2", "u3")
#' W <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' graph <- gmde_graph(W, ids)
#' B <- cbind(intercept = 1, contrast = c(1, 1, -1, -1))
#' expert <- gmde_expert("gaussian", B, prior = list(
#'   m0 = c(0, 0), C0 = diag(c(25, 25)),
#'   variance = list(shape = 2, scale = 1)))
#' Y <- rbind(u1 = c(1, 0.8, -1, -0.8), u2 = c(0.9, 1.1, -0.9, -1.1),
#'            u3 = c(-1, -0.8, 1, 0.8))
#' fit <- gmde_fit(Y, graph, 2, expert, mcmc = list(iterations = 20, warmup = 10))
#' gmde_partition(fit)$partition
#' @export
gmde_fit <- function(Y, graph, K, expert, gate = gmde_gate(), mcmc = gmde_mcmc()) {
  .gmde_matrix(Y, "Y")
  .gmde_scalar(K, "K", 1, .Machine$integer.max, TRUE)
  K <- as.integer(K)
  if (!inherits(graph, "gmde_graph")) stop("graph must be a gmde_graph object.")
  if (!inherits(expert, "gmde_expert")) stop("expert must be a gmde_expert object.")
  if (nrow(Y) != length(graph$unit_ids) ||
      !identical(rownames(Y), graph$unit_ids))
    stop("Y row names must match graph unit_ids in exactly the same order.")
  if (ncol(Y) != nrow(expert$design)) stop("Y and design must share the time grid.")
  if (!is.null(colnames(Y)) && !is.null(rownames(expert$design)) &&
      !identical(colnames(Y), rownames(expert$design)))
    stop("Y time names and design row names disagree.")
  # Revalidate specifications to catch unsupported fields and edited objects.
  expert <- gmde_expert(expert$family, expert$design, expert$mode,
                        expert$prior, expert$state)
  gate <- do.call(gmde_gate, .gmde_options(unclass(gate), as.list(formals(gmde_gate)), "gate"))
  gate <- .gmde_resolve_gate(gate, K)
  mcmc <- do.call(gmde_mcmc, .gmde_options(unclass(mcmc), as.list(formals(gmde_mcmc)), "mcmc"))
  n <- nrow(Y); p <- ncol(expert$design)
  if (!is.null(mcmc$seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
    old_kind <- RNGkind()
    on.exit({
      do.call(RNGkind, as.list(old_kind))
      if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(mcmc$seed)
  }
  initial_rng <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  start <- proc.time()[["elapsed"]]
  H <- .gmde_contrasts(K)
  x <- .gmde_white_draw(n, graph$rank, K - 1L)
  v <- if (gate$free) stats::qlogis(if (gate$shared) gate$a[1L] else gate$a) else numeric(0)
  pair <- if (gate$free) .gmde_weight_pair(v, K, gate$shared) else
    list(a = gate$a, independent = 1 - gate$a)
  F <- if (K == 1L) matrix(0, n, 1L) else
    .gmde_utilities(x, graph, H, gate, pair$a, pair$independent)
  Z <- .gmde_categorical(exp(.gmde_log_softmax(F)))
  sigma2 <- vapply(seq_len(K), function(k)
    .gmde_variance_draw(expert$prior$variance$shape, expert$prior$variance$scale), numeric(1))
  initialization <- list(Z = Z, sigma2 = sigma2, a = pair$a, v = v, white = x)
  sufficient <- .gmde_sufficient(Y, Z, K)
  retained <- seq.int(mcmc$warmup + mcmc$thin, mcmc$iterations, by = mcmc$thin)
  ndraw <- length(retained)
  class_ids <- paste0("class", seq_len(K))
  coefficient_ids <- colnames(expert$design)
  if (is.null(coefficient_ids)) coefficient_ids <- paste0("beta", seq_len(p))
  draws <- list(beta = array(NA_real_, c(ndraw, K, p),
                             dimnames = list(NULL, class_ids, coefficient_ids)),
                sigma2 = matrix(NA_real_, ndraw, K, dimnames = list(NULL, class_ids)),
                Z = matrix(NA_integer_, ndraw, n, dimnames = list(NULL, graph$unit_ids)),
                a = matrix(NA_real_, ndraw, if (K == 1L) 0L else K),
                v = matrix(NA_real_, ndraw, gate$free))
  if (K > 1L) colnames(draws$a) <- class_ids
  ess_evaluations <- integer(mcmc$iterations)
  guidance_accepted <- matrix(FALSE, mcmc$iterations, gate$free)
  cluster_sizes <- matrix(0L, mcmc$iterations, K)
  log_likelihood <- numeric(mcmc$iterations)
  store_index <- 0L
  for (iteration in seq_len(mcmc$iterations)) {
    tryCatch({
      updated <- .gmde_expert_update(Y, Z, sufficient, sigma2, expert, K)
      sigma2 <- updated$sigma2
      if (K > 1L) {
        ess <- .gmde_ess_update(x, F, Z, graph, H, gate, pair$a, pair$independent,
                           mcmc$ess_max_steps)
        x <- ess$x; F <- ess$F; ess_evaluations[iteration] <- ess$evaluations
        if (gate$free) {
          guidance <- .gmde_guidance_update(x, F, v, Z, graph, H, gate)
          v <- guidance$v; F <- guidance$F
          guidance_accepted[iteration, ] <- guidance$accepted
          pair <- .gmde_weight_pair(v, K, gate$shared)
        }
      }
      likelihood <- .gmde_gaussian_loglik(Y, updated$eta, sigma2)
      probabilities <- exp(.gmde_log_softmax(F + likelihood))
      Z <- .gmde_categorical(probabilities)
      sufficient <- .gmde_sufficient(Y, Z, K)
      cluster_sizes[iteration, ] <- sufficient$N
      log_likelihood[iteration] <- sum(likelihood[cbind(seq_len(n), Z)])
      if (iteration > mcmc$warmup && (iteration - mcmc$warmup) %% mcmc$thin == 0L) {
        store_index <- store_index + 1L
        draws$beta[store_index, , ] <- updated$beta
        draws$sigma2[store_index, ] <- sigma2
        draws$Z[store_index, ] <- Z
        draws$a[store_index, ] <- pair$a
        draws$v[store_index, ] <- v
      }
    }, error = function(e) stop("graphMoDE sweep ", iteration, " failed: ",
                                conditionMessage(e), call. = FALSE))
  }
  structure(list(schema_version = "0.1.0", draws = draws, K = K,
                 unit_ids = graph$unit_ids, time_ids = colnames(Y),
                 data_dimensions = dim(Y), graph = graph, expert = expert, gate = gate,
                 mcmc = mcmc, retained_iterations = retained,
                 final_state = list(beta = updated$beta, sigma2 = sigma2, Z = Z,
                                    white = x, v = v, a = pair$a, utilities = F,
                                    probabilities = probabilities, sufficient = sufficient),
                 diagnostics = list(ess_evaluations = ess_evaluations,
                                    guidance_accepted = guidance_accepted,
                                    cluster_sizes = cluster_sizes,
                                    conditional_log_likelihood = log_likelihood),
                 metadata = list(package_name = "bdynets", package_version = "0.1.0", milestone = 1L,
                                 target = "fixed-K Gaussian/static adaptive logistic-normal mixture",
                                 label_policy = "raw chain labels",
                                 tuning = list(rw_sd = gate$rw_sd, adaptation = "none"),
                                 seed = mcmc$seed, initial_rng = initial_rng,
                                 final_rng = get(".Random.seed", envir = .GlobalEnv),
                                 RNGkind = RNGkind(), initialization = initialization,
                                 sessionInfo = utils::sessionInfo(),
                                 dependency_versions = c(stats = as.character(utils::packageVersion("stats")),
                                                         utils = as.character(utils::packageVersion("utils"))),
                                 elapsed_seconds = proc.time()[["elapsed"]] - start,
                                 failures = character(0))), class = "gmde_fit")
}

#' @export
print.gmde_fit <- function(x, ...) {
  cat("graphMoDE Gaussian/static reference fit\n",
      x$data_dimensions[1L], " series; ", x$data_dimensions[2L], " times; K = ",
      x$K, "; ", nrow(x$draws$Z), " retained draws.\n",
      "Raw labels; chain convergence has not been certified.\n", sep = "")
  invisible(x)
}
