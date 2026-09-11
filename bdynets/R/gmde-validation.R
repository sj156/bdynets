# Shared input and numerical checks. Fail explicitly; never repair a target.
.gmde_scalar <- function(x, name, lower = -Inf, upper = Inf, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < lower || x > upper || (integer && x != floor(x)))
    stop(name, " must be a finite ", if (integer) "integer " else "number ",
         "in [", lower, ", ", upper, "].", call. = FALSE)
  invisible(x)
}

.gmde_positive <- function(x, name) {
  .gmde_scalar(x, name)
  if (x <= 0) stop(name, " must be positive.", call. = FALSE)
}

.gmde_matrix <- function(x, name) {
  if (!is.matrix(x) || !is.numeric(x) || any(dim(x) < 1L) ||
      any(!is.finite(x)))
    stop(name, " must be a nonempty finite numeric matrix.", call. = FALSE)
}

.gmde_ids <- function(ids, n) {
  if (!is.character(ids) || length(ids) != n || anyNA(ids) ||
      any(!nzchar(trimws(ids))) || anyDuplicated(ids))
    stop("unit_ids must contain one unique, nonempty string per series.",
         call. = FALSE)
}

.gmde_chol <- function(x, name) {
  if (any(!is.finite(x))) stop(name, " is not finite.", call. = FALSE)
  tryCatch(chol(x), error = function(e)
    stop(name, " must be numerically positive definite; no jitter was added.",
         call. = FALSE))
}

.gmde_options <- function(x, defaults, name) {
  if (!is.list(x) || (length(x) &&
      (is.null(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)))))
    stop(name, " must be a uniquely named list.", call. = FALSE)
  unknown <- setdiff(names(x), names(defaults))
  if (length(unknown)) stop("Unknown ", name, " option: ",
                            paste(unknown, collapse = ", "), call. = FALSE)
  for (key in names(x)) defaults[key] <- x[key]
  defaults
}
