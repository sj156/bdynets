.gls_prepare_draw_store <- function(draw_dir, chunk_size) {
  if (is.null(draw_dir)) return(NULL)
  if (!is.character(draw_dir) || length(draw_dir) != 1L ||
      is.na(draw_dir) || !nzchar(draw_dir)) {
    stop("draw_dir must be one directory path.", call. = FALSE)
  }
  if (!is.numeric(chunk_size) || length(chunk_size) != 1L ||
      !is.finite(chunk_size) || chunk_size < 1 ||
      chunk_size > .Machine$integer.max || chunk_size != floor(chunk_size)) {
    stop("chunk_size must be a positive integer.", call. = FALSE)
  }
  draw_dir <- path.expand(draw_dir)
  if (!dir.exists(draw_dir)) {
    dir.create(draw_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(draw_dir)) {
    stop("Could not create draw directory: ", draw_dir, call. = FALSE)
  }
  path <- tempfile("gls-", tmpdir = normalizePath(draw_dir, winslash = "/"))
  if (!dir.create(path, showWarnings = FALSE)) {
    stop("Could not create GLS draw store: ", path, call. = FALSE)
  }
  list(path = path, chunk_size = as.integer(chunk_size), version = 1L)
}

.gls_draw_buffer <- function(draw_store, chain_id, n_saved) {
  if (is.null(draw_store)) return(list(capacity = n_saved, directory = NULL))
  chain <- sprintf("chain-%04d", chain_id)
  directory <- file.path(draw_store$path, chain)
  if (!dir.create(directory, showWarnings = FALSE)) {
    stop("Could not create GLS chain directory: ", directory, call. = FALSE)
  }
  list(capacity = min(n_saved, draw_store$chunk_size), directory = directory,
       chain = chain, chain_id = chain_id, files = character(),
       sizes = integer(), hashes = character(), n_saved = 0L, last_iteration = 0L)
}

.gls_draw_rows <- function(x, rows) {
  switch(as.character(length(dim(x))),
    "0" = x[rows],
    "2" = x[rows, , drop = FALSE],
    "3" = x[rows, , , drop = FALSE],
    stop("Unexpected GLS draw dimensions.", call. = FALSE))
}

.gls_write_draw_chunk <- function(buffer, draws, n, iteration) {
  if (n == 0L) return(buffer)
  if (n < buffer$capacity) draws <- lapply(draws, .gls_draw_rows, seq_len(n))
  name <- sprintf("chunk-%08d.rds", length(buffer$files) + 1L)
  path <- file.path(buffer$directory, name)
  temp <- tempfile(".chunk-", tmpdir = buffer$directory)
  on.exit(unlink(temp), add = TRUE)
  last_draw <- as.integer(buffer$n_saved + n)
  saveRDS(list(chain_id = buffer$chain_id, first_draw = buffer$n_saved + 1L,
               last_draw = last_draw, iteration = iteration, draws = draws), temp)
  hash <- unname(tools::md5sum(temp))
  if (is.na(hash) || file.exists(path) || !file.rename(temp, path)) {
    stop("Could not finalize GLS draw chunk: ", path, call. = FALSE)
  }
  buffer$files <- c(buffer$files, file.path(buffer$chain, name))
  buffer$sizes <- c(buffer$sizes, as.integer(n))
  buffer$hashes <- c(buffer$hashes, hash)
  buffer$n_saved <- last_draw
  buffer$last_iteration <- iteration
  buffer
}

.gls_draw_chunks <- function(buffer) {
  buffer[c("files", "sizes", "hashes", "n_saved", "last_iteration")]
}

.gls_check_chains <- function(chains, ids) {
  failed <- vapply(chains, function(chain) {
    is.null(chain) || inherits(chain, "try-error")
  }, logical(1))
  if (any(failed)) {
    messages <- vapply(chains[failed], function(chain) {
      if (is.null(chain)) "Worker returned no result." else as.character(chain)
    }, character(1))
    stop("MCMC chain failure(s): ", paste(ids[failed], collapse = ", "), "\n",
         paste(messages, collapse = "\n"), call. = FALSE)
  }
}

.gls_read_chain_draws <- function(chain, path, control) {
  index <- chain$draw_chunks
  count <- floor((control$n_iter - control$burn_mcmc) / control$thin)
  if (!length(index$files) || length(index$files) != length(index$sizes) ||
      length(index$files) != length(index$hashes) || any(index$sizes <= 0L) ||
      index$n_saved != count || sum(index$sizes) != count) {
    stop("Incomplete GLS draw index for chain ", chain$chain_id, ".", call. = FALSE)
  }
  history <- NULL
  saved <- 0L
  for (i in seq_along(index$files)) {
    chunk_path <- file.path(path, index$files[i])
    if (!file.exists(chunk_path)) {
      stop("Missing GLS draw chunk: ", chunk_path, call. = FALSE)
    }
    if (!identical(unname(tools::md5sum(chunk_path)), index$hashes[i])) {
      stop("Checksum mismatch for GLS draw chunk: ", chunk_path, call. = FALSE)
    }
    last <- saved + index$sizes[i]
    chunk <- readRDS(chunk_path)
    if (chunk$chain_id != chain$chain_id || chunk$first_draw != saved + 1L ||
        chunk$last_draw != last ||
        chunk$iteration != control$burn_mcmc + last * control$thin) {
      stop("Unexpected draw range in GLS chunk: ", chunk_path, call. = FALSE)
    }
    if (is.null(history)) {
      history <- lapply(chunk$draws, .gls_draw_rows, rep(NA_integer_, count))
    }
    rows <- seq.int(saved + 1L, last)
    for (field in names(history)) {
      values <- chunk$draws[[field]]
      if (is.null(dim(values))) history[[field]][rows] <- values
      else if (length(dim(values)) == 2L) history[[field]][rows, ] <- values
      else history[[field]][rows, , ] <- values
    }
    saved <- last
    rm(chunk, values)
  }
  history
}

#' Load the retained draws of a disk-backed GLS fit
#'
#' Reconstructs the ordinary in-memory fit layout, including the per-chain
#' arrays. An in-memory fit is returned as supplied. Keep the draw directory
#' with serialized fits. Loading uses RAM proportional to the retained history.
#'
#' @param fit A fit returned by [gls_fit()], [gls_fit_general()], or
#'   [gls_fit_study()].
#' @param draw_dir Optional relocated directory for this fit's draw store. It
#'   must contain the `chain-*` subdirectories. The path recorded in `fit` is
#'   used by default.
#' @return The GLS fit with its posterior draws in memory.
#' @export
gls_load_draws <- function(fit, draw_dir = NULL) {
  if (!inherits(fit, c("gls_fit", "gls_general_fit", "gls_study_fit"))) {
    stop("fit must be a GLS fit.", call. = FALSE)
  }
  if (is.null(fit$draw_store)) return(fit)
  path <- if (is.null(draw_dir)) fit$draw_store$path else draw_dir
  if (!identical(fit$draw_store$version, 1L)) {
    stop("Unknown GLS draw store version.", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !dir.exists(path)) {
    stop("GLS draw store directory is unavailable: ", path, call. = FALSE)
  }
  study <- inherits(fit, "gls_study_fit")
  chains <- lapply(fit$chains, function(chain) {
    draws <- .gls_read_chain_draws(chain, path, fit$control)
    chain$draw_chunks <- NULL
    if (study) .gls_study_merge_chain_draws(draws, chain)
    else .gls_merge_chain_draws(draws, chain)
  })
  out <- if (study) {
    .gls_study_combine_chains(chains, fit$pairs, fit$Fg, fit$control)
  } else if (inherits(fit, "gls_general_fit")) {
    .gls_combine_general_chains(chains, fit$pairs, fit$control, fit$elapsed)
  } else {
    .gls_combine_chains(chains, fit$W, fit$pairs, fit$label, fit$control, fit$elapsed)
  }
  for (name in setdiff(names(fit), c("chains", "draw_store"))) out[[name]] <- fit[[name]]
  structure(out, class = class(fit))
}

.gls_retained_draw_count <- function(fit) {
  if (is.null(fit$draw_store)) nrow(fit$alpha)
  else sum(vapply(fit$chains, function(chain) chain$draw_chunks$n_saved, integer(1)))
}
