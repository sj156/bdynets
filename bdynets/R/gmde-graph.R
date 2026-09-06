#' Construct a graph covariance on series IDs
#'
#' Adjacency must be exactly symmetric, nonnegative and zero diagonal. It is
#' never silently symmetrized. Kernel trace is normalized to n * tau^2.
#' Eigenvalues are sorted in increasing order; rank requests expand to preserve
#' the entire numerical zero eigenspace and tied blocks. Truncation changes the
#' prior. Tiny negative Laplacian eigenvalues within the declared tolerance are
#' treated as numerical zeros, and their original values are retained.
#' @param W_graph Numeric n-by-n adjacency matrix.
#' @param unit_ids Unique character IDs, in adjacency order. If adjacency has
#'   dimension names, both row and column names must match these IDs exactly.
#' @param nu,kappa,tau Positive fixed kernel parameters.
#' @param rank "full" (default), or an integer from 1 to n.
#' @param eigen_tolerance Relative tolerance for zero and tied eigenvalues.
#' @param provenance List describing externally supplied graph construction.
#' @return A gmde_graph object including adjacency, Laplacian, Phi, covariance,
#'   IDs, eigenvalues, effective rank and provenance.
#' @export
gmde_graph <- function(W_graph, unit_ids, nu = 1, kappa = sqrt(2), tau = 1,
                       rank = "full", eigen_tolerance = 1e-10,
                       provenance = list()) {
  .gmde_matrix(W_graph, "W_graph")
  n <- nrow(W_graph)
  if (ncol(W_graph) != n || any(W_graph < 0) ||
      any(diag(W_graph) != 0) || !all(W_graph == t(W_graph)))
    stop("W_graph must be square, symmetric, nonnegative and zero-diagonal.",
         call. = FALSE)
  .gmde_ids(unit_ids, n)
  if (!is.null(dimnames(W_graph)) &&
      (!identical(rownames(W_graph), unit_ids) ||
       !identical(colnames(W_graph), unit_ids)))
    stop("Adjacency row and column names must match unit_ids in order.",
         call. = FALSE)
  .gmde_positive(nu, "nu"); .gmde_positive(kappa, "kappa"); .gmde_positive(tau, "tau")
  .gmde_positive(eigen_tolerance, "eigen_tolerance")
  if (eigen_tolerance >= 1) stop("eigen_tolerance must be less than 1.")
  if (!is.list(provenance)) stop("provenance must be a list.")
  requested <- if (identical(rank, "full")) n else {
    .gmde_scalar(rank, "rank", 1, n, integer = TRUE)
    as.integer(rank)
  }
  dimnames(W_graph) <- list(unit_ids, unit_ids)
  L <- diag(rowSums(W_graph), n) - W_graph
  if (any(!is.finite(L))) stop("Laplacian overflow; rescale inputs explicitly.")
  eig <- eigen(L, symmetric = TRUE)
  ord <- order(eig$values)
  raw <- eig$values[ord]
  tol <- eigen_tolerance * max(1, abs(raw))
  if (any(raw < -tol)) stop("Laplacian has negative eigenvalues beyond tolerance.")
  lambda <- raw
  lambda[abs(lambda) <= tol] <- 0
  effective <- max(requested, sum(lambda == 0))
  while (effective < n && lambda[effective + 1L] - lambda[effective] <= tol)
    effective <- effective + 1L
  at <- seq_len(effective)
  # Log weights avoid overflow in powers and trace normalization.
  offset <- 2 * nu / kappa^2
  if (!is.finite(offset) || offset <= 0) stop("Kernel offset is not representable.")
  log_r <- -nu * log(offset + lambda[at])
  if (any(!is.finite(log_r))) stop("Kernel log weights are not finite.")
  r <- exp(log_r - max(log_r))
  if (any(r == 0)) stop("Kernel weights underflow; revise fixed kernel parameters.")
  Phi <- sweep(eig$vectors[, ord[at], drop = FALSE], 2L,
               tau * sqrt(n * r / sum(r)), "*")
  rownames(Phi) <- unit_ids
  covariance <- tcrossprod(Phi)
  if (any(!is.finite(covariance))) stop("Graph covariance overflow.")
  structure(list(W_graph = W_graph, unit_ids = unit_ids, L = L, Phi = Phi,
                 covariance = covariance, eigenvalues = lambda,
                 raw_eigenvalues = raw, retained = at, requested_rank = requested,
                 rank = effective, nu = nu, kappa = kappa, tau = tau,
                 eigen_tolerance = eigen_tolerance, provenance = provenance),
            class = "gmde_graph")
}
