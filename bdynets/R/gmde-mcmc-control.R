#' Configure one reference MCMC chain
#'
#' Defaults are smoke-test lengths, not an inferential recommendation. Proposal
#' tuning stays fixed throughout warmup and retained sampling. Thin is applied
#' after warmup: retain warmup + thin, warmup + 2 * thin, and so on.
#' @param iterations Total number of sweeps, including warmup.
#' @param warmup Number of initial sweeps to discard.
#' @param thin Retain every thin-th post-warmup sweep.
#' @param seed Integer seed, or NULL to use and advance the caller's RNG.
#'   With an explicit seed the caller's RNG state is restored on exit.
#' @param ess_max_steps Explicit ESS diagnostic cap; default Inf (no cap).
#'   Exceeding a finite cap raises an error, never returns the old state as a draw.
#' @return A gmde_mcmc specification.
#' @export
gmde_mcmc <- function(iterations = 100L, warmup = 50L, thin = 1L,
                      seed = 90601L, ess_max_steps = Inf) {
  .gmde_scalar(iterations, "iterations", 1, .Machine$integer.max, TRUE)
  .gmde_scalar(warmup, "warmup", 0, iterations - 1, TRUE)
  .gmde_scalar(thin, "thin", 1, iterations - warmup, TRUE)
  if (!is.null(seed)) .gmde_scalar(seed, "seed", 0, .Machine$integer.max, TRUE)
  if (!identical(ess_max_steps, Inf))
    .gmde_scalar(ess_max_steps, "ess_max_steps", 1, .Machine$integer.max, TRUE)
  structure(list(iterations = as.integer(iterations), warmup = as.integer(warmup),
                 thin = as.integer(thin), seed = seed, ess_max_steps = ess_max_steps),
            class = "gmde_mcmc")
}
