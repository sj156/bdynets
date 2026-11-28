#' Publish a complete result file
#' @param path Destination in an existing directory.
#' @param writer Function that writes its result to the supplied temporary path.
#' @return The destination path, invisibly.
#' @export
bdynets_atomic_write <- function(path, writer) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      !dir.exists(dirname(path)) || dir.exists(path) || !is.function(writer))
    stop("Supply a file path in an existing directory and a writer function.")
  temporary <- tempfile(".bdynets-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  writer(temporary)
  if (!file.exists(temporary) || dir.exists(temporary) || !file.rename(temporary, path))
    stop("Could not publish result file: ", path)
  invisible(path)
}
