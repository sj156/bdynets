#' Summarize a posterior partition without relabeling
#'
#' The representative is the retained partition minimizing the squared
#' Frobenius distance of its co-membership matrix to the posterior similarity
#' matrix (PSM), including both triangles. Ties select the first minimum.
#' This deterministic summary consumes no random numbers.
#' @param fit A gmde_fit object with retained allocation draws.
#' @return A gmde_partition list with psm, representative partition (original
#'   labels), draw_index, iteration, loss for each draw, IDs and criterion.
#' @export
gmde_partition <- function(fit) {
  if (!inherits(fit, "gmde_fit")) stop("fit must be a gmde_fit object.")
  Z <- fit$draws$Z
  if (!is.matrix(Z) || !nrow(Z) || !ncol(Z) || anyNA(Z))
    stop("fit must contain complete retained allocations.")
  n <- ncol(Z)
  psm <- matrix(0, n, n, dimnames = list(fit$unit_ids, fit$unit_ids))
  for (s in seq_len(nrow(Z))) psm <- psm + outer(Z[s, ], Z[s, ], "==")
  psm <- psm / nrow(Z)
  loss <- vapply(seq_len(nrow(Z)), function(s)
    sum((outer(Z[s, ], Z[s, ], "==") - psm)^2), numeric(1))
  chosen <- which.min(loss)
  structure(list(psm = psm, partition = stats::setNames(as.integer(Z[chosen, ]), fit$unit_ids),
                 draw_index = chosen, iteration = fit$retained_iterations[chosen],
                 loss = loss, unit_ids = fit$unit_ids,
                 criterion = "Squared Frobenius distance to PSM; first minimum"),
            class = "gmde_partition")
}
