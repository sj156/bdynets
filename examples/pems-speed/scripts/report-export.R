report_export_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("An export directory is required.", call. = FALSE)
  }
  path <- path.expand(path)
  windows <- .Platform$OS.type == "windows"
  if (windows) path <- chartr("\\", "/", path)
  if (!grepl("^/+$", path) && !(windows && grepl("^[A-Za-z]:/+$", path))) {
    path <- sub("/+$", "", path)
  }
  if (startsWith(path, "/") || (windows && grepl("^[A-Za-z]:/", path))) return(path)
  file.path(getwd(), path)
}

report_path_occupied <- function(path) {
  link <- Sys.readlink(path)
  file.exists(path) || (!is.na(link) && nzchar(link))
}

require_new_report_export <- function(path) {
  if (report_path_occupied(path)) {
    stop("Choose a new export directory. Already exists: ", path, call. = FALSE)
  }
  if (basename(path) %in% c(".", "..")) {
    stop("Choose a new export directory with its own name: ", path, call. = FALSE)
  }
}

publish_report_export <- function(output_dir, copy_into, finalize = NULL) {
  output_dir <- report_export_path(output_dir)
  require_new_report_export(output_dir)
  parent <- dirname(output_dir)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE) &&
      !dir.exists(parent)) {
    stop("Cannot create export parent directory: ", parent, call. = FALSE)
  }
  lock <- paste0(output_dir, ".export-lock")
  if (!dir.create(lock, showWarnings = FALSE)) {
    if (report_path_occupied(lock)) {
      stop("Another export is using this destination: ", output_dir, call. = FALSE)
    }
    stop("Cannot create the export lock: ", lock, call. = FALSE)
  }
  on.exit(unlink(lock, recursive = TRUE, expand = FALSE), add = TRUE)
  require_new_report_export(output_dir)
  stage <- tempfile(".report-export-", tmpdir = parent)
  if (!dir.create(stage, showWarnings = FALSE)) {
    stop("Cannot create report export staging directory in: ", parent, call. = FALSE)
  }
  on.exit(unlink(stage, recursive = TRUE, expand = FALSE), add = TRUE, after = FALSE)
  copy_into(stage)
  require_new_report_export(output_dir)
  if (!suppressWarnings(file.rename(stage, output_dir))) {
    stop("Cannot publish the report export to: ", output_dir, call. = FALSE)
  }
  if (!is.null(finalize)) {
    completed <- FALSE
    on.exit({
      if (!completed && unlink(output_dir, recursive = TRUE, expand = FALSE) != 0L) {
        warning("Cannot remove export after report publication failed: ", output_dir, call. = FALSE)
      }
    }, add = TRUE, after = FALSE)
    finalize()
    completed <- TRUE
  }
  invisible(output_dir)
}

export_static_report <- function(output_dir, prepared_dir, map_chunks) {
  map_files <- vapply(unname(map_chunks), knitr::fig_chunk, character(1), ext = "png")
  publish_report_export(output_dir, function(stage) {
    map_dir <- file.path(stage, "maps")
    if (!dir.create(map_dir) ||
        !file.copy(prepared_dir, stage, recursive = TRUE) ||
        !all(file.copy(map_files, file.path(map_dir, names(map_chunks))))) {
      stop("Cannot save the prepared data and static maps.", call. = FALSE)
    }
  })
}
