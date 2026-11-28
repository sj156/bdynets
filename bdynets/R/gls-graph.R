#' Construct a directed network for GLS
#'
#' @param edges Data frame in count-series order with a unique `name` per
#'   series. To derive adjacency it must also have nonmissing `from` and `to`
#'   endpoint IDs. Additional columns are preserved.
#' @param A Optional supplied square, finite, nonnegative adjacency matrix.
#'   Row `a`, column `b` gives the weight from source series `b` to target
#'   series `a`. The matrix must follow the row order of `edges`.
#' @param exclude_uturns When deriving adjacency, exclude a predecessor that
#'   immediately reverses the target edge. The derived diagonal is always zero.
#' @return A list with `edges`, adjacency `A`, and row-normalized `W`.
#'   A target with no predecessors has a zero row in `W`.
#' @export
gls_graph <- function(edges, A = NULL, exclude_uturns = FALSE) {
  if (!is.data.frame(edges) || !nrow(edges) || !"name" %in% names(edges) ||
      anyNA(edges$name) || any(!nzchar(as.character(edges$name))) || anyDuplicated(edges$name)) {
    stop("edges must contain one unique, nonempty name per series.", call. = FALSE)
  }
  if (!is.logical(exclude_uturns) || length(exclude_uturns) != 1L || is.na(exclude_uturns))
    stop("exclude_uturns must be TRUE or FALSE.", call. = FALSE)
  if (is.null(A)) {
    if (!all(c("from", "to") %in% names(edges)) || anyNA(edges$from) || anyNA(edges$to))
      stop("edges must supply from and to endpoint IDs to derive adjacency.", call. = FALSE)
    from <- as.character(edges$from)
    to <- as.character(edges$to)
    A <- outer(from, to, "==")
    if (exclude_uturns) A <- A & !outer(to, from, "==")
    diag(A) <- FALSE
    dimnames(A) <- list(target = as.character(edges$name), source = as.character(edges$name))
  } else {
    k <- nrow(edges)
    if (exclude_uturns) stop("exclude_uturns applies only when deriving adjacency.", call. = FALSE)
    if (!(is.numeric(A) || is.logical(A)) || !identical(dim(A), c(k, k)) ||
        any(!is.finite(A)) || any(A < 0)) {
      stop("A must be a finite nonnegative square adjacency matrix in series order.", call. = FALSE)
    }
    if (any(vapply(dimnames(A), function(ids) !is.null(ids) &&
        !identical(ids, as.character(edges$name)), logical(1)))) {
      stop("Adjacency dimnames must follow the series order in edges.", call. = FALSE)
    }
  }
  storage.mode(A) <- "double"
  totals <- rowSums(A)
  totals[totals == 0] <- 1
  list(edges = edges, A = A, W = A / totals)
}

#' Construct consecutive training, validation and test row indices
#'
#' @param n Total number of time points.
#' @param train_end Last training row.
#' @param validation_end Last validation row. The remaining rows form the test set.
#' @return A named list of integer row indices, in chronological order.
#' @export
gls_split_rows <- function(n, train_end = floor(2 * n / 3), validation_end = floor(5 * n / 6)) {
  ends <- c(train_end, validation_end, n)
  if (length(n) != 1L || length(train_end) != 1L || length(validation_end) != 1L ||
      !is.numeric(ends) || any(!is.finite(ends)) || any(ends != floor(ends)) ||
      train_end < 1 || validation_end <= train_end || n <= validation_end || n > .Machine$integer.max) {
    stop("Split boundaries must be integers satisfying 1 <= train_end < validation_end < n.", call. = FALSE)
  }
  ends <- as.integer(ends)
  list(train = seq_len(ends[1L]), validation = seq.int(ends[1L] + 1L, ends[2L]),
       test = seq.int(ends[2L] + 1L, ends[3L]))
}
