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
  if (!dir.create(directory, showWarnings = FALSE) &&
      (is.null(draw_store$checkpoint_dir) || !dir.exists(directory))) {
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

.gls_draw_store_path <- function(fit, draw_dir) {
  path <- if (is.null(draw_dir)) fit$draw_store$path else draw_dir
  if (!identical(fit$draw_store$version, 1L)) {
    stop("Unknown GLS draw store version.", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !dir.exists(path)) {
    stop("GLS draw store directory is unavailable: ", path, call. = FALSE)
  }
  path
}

# ids are ordered, global, one-based retained draw indices across these chains.
.gls_read_disk_draws <- function(chains, path, control, ids, fields = NULL) {
  count <- floor((control$n_iter - control$burn_mcmc) / control$thin)
  history <- NULL
  offset <- 0L
  for (chain in chains) {
    index <- chain$draw_chunks
    if (!length(index$files) || length(index$files) != length(index$sizes) ||
        length(index$files) != length(index$hashes) || any(index$sizes <= 0L) ||
        index$n_saved != count || sum(index$sizes) != count) {
      stop("Incomplete GLS draw index for chain ", chain$chain_id, ".", call. = FALSE)
    }

    ends <- c(0L, cumsum(index$sizes))
    positions <- findInterval(offset + ends, ids)
    for (i in which(diff(positions) > 0L)) {
      rows <- seq.int(positions[i] + 1L, positions[i + 1L])
      path_i <- file.path(path, index$files[i])
      if (!file.exists(path_i)) stop("Missing GLS draw chunk: ", path_i, call. = FALSE)
      if (!identical(unname(tools::md5sum(path_i)), index$hashes[i])) {
        stop("Checksum mismatch for GLS draw chunk: ", path_i, call. = FALSE)
      }
      chunk <- readRDS(path_i)
      last <- ends[i + 1L]
      if (chunk$chain_id != chain$chain_id || chunk$first_draw != ends[i] + 1L ||
          chunk$last_draw != last || chunk$iteration != control$burn_mcmc + last * control$thin) {
        stop("Unexpected draw range in GLS chunk: ", path_i, call. = FALSE)
      }
      if (is.null(fields)) fields <- names(chunk$draws)
      missing <- setdiff(fields, names(chunk$draws))
      if (length(missing)) stop("Unavailable draw fields: ", paste(missing, collapse = ", "), call. = FALSE)
      if (is.null(history)) {
        history <- lapply(chunk$draws[fields], .gls_draw_rows, rep(NA_integer_, length(ids)))
      }
      source_rows <- ids[rows] - offset - ends[i]
      for (field in fields) {
        value <- chunk$draws[[field]]
        if (is.null(dim(value))) history[[field]][rows] <- value[source_rows]
        else if (length(dim(value)) == 2L) history[[field]][rows, ] <- value[source_rows, , drop = FALSE]
        else history[[field]][rows, , ] <- value[source_rows, , , drop = FALSE]
      }
      rm(chunk, value)
    }
    offset <- offset + count
  }
  history
}

.gls_read_chain_draws <- function(chain, path, control) {
  count <- floor((control$n_iter - control$burn_mcmc) / control$thin)
  .gls_read_disk_draws(list(chain), path, control, seq_len(count))
}

.gls_general_chain_ids <- function(fit) {
  if (is.null(fit$draw_store)) return(fit$chain_id)
  rep(vapply(fit$chains, `[[`, integer(1), "chain_id"),
      times = vapply(fit$chains, function(ch) ch$draw_chunks$n_saved, integer(1)))
}

.gls_general_draw_indices <- function(chain_ids, n, seed) {
  if (is.null(n) || n >= length(chain_ids)) return(seq_along(chain_ids))
  # Preserve the existing seeded draws whenever they meet the requested balance.
  selected <- unname(balanced_indices(chain_ids, n, seed))
  counts <- tabulate(match(chain_ids[selected], unique(chain_ids)), length(unique(chain_ids)))
  if (length(selected) == n && !anyDuplicated(selected) && diff(range(counts)) <= 1L) return(selected)
  set.seed(seed)
  groups <- split(seq_along(chain_ids), chain_ids)
  per <- rep(n %/% length(groups), length(groups))
  extra <- sample.int(length(groups), n %% length(groups))
  per[extra] <- per[extra] + 1L
  sort(unlist(Map(function(rows, size) rows[sample.int(length(rows), size)], groups, per),
              use.names = FALSE))
}

.gls_general_draw_fields <- function(fit, ids, fields, draw_dir = NULL) {
  if (!is.null(fit$draw_store)) {
    return(.gls_read_disk_draws(fit$chains, .gls_draw_store_path(fit, draw_dir),
                                fit$control, ids, fields))
  }
  missing <- fields[vapply(fit[fields], is.null, logical(1))]
  if (length(missing)) stop("Unavailable draw fields: ", paste(missing, collapse = ", "), call. = FALSE)
  if (length(ids) == nrow(fit$alpha)) return(fit[fields])
  lapply(fit[fields], .gls_draw_rows, ids)
}

#' Read selected posterior arrays from a general GLS fit
#'
#' @param fit A fit returned by [gls_fit_general()].
#' @param fields Names of retained draw arrays. Available fields are
#'   `B`, `discount`, `initial_beta`, `reference_mean`, `reference_variance`,
#'   `alpha`, `gamma`, `beta_path`, `final_beta`, `L`, `amplitude`,
#'   `theta_average`, `tau`, `network_rms`, and `chi` for learned affinity.
#' @param max_draws Optional positive integer maximum. Draws are sampled across
#'   chains as evenly as possible. The default uses all retained iterations.
#' @param seed Integer seed for subset selection.
#' @inheritParams gls_load_draws
#' @return A list of requested arrays with draws in their first dimension,
#'   plus `draw_id` (one-based positions in the full history) and `chain_id`.
#' @details Only chunks containing requested draws are read and checksum-checked.
#'   Each visited chunk is decoded in full. Only the requested rows and fields
#'   are retained in memory across chunks. Use [gls_load_draws()] for the
#'   full fit with its per-chain arrays.
#' @export
gls_read_general_draws <- function(fit, fields, max_draws = NULL, seed = 1L, draw_dir = NULL) {
  if (!inherits(fit, "gls_general_fit")) stop("fit must be a general GLS fit.", call. = FALSE)
  available <- c("B", "discount", "initial_beta", "reference_mean", "reference_variance",
                 "alpha", "gamma", "beta_path", "final_beta", "L", "amplitude",
                 "theta_average", "tau", "network_rms", "chi")
  if (!is.character(fields) || !length(fields) || anyDuplicated(fields) || !all(fields %in% available)) {
    stop("fields must contain distinct general GLS draw field names.", call. = FALSE)
  }
  if (!is.null(max_draws) && (!is.numeric(max_draws) || length(max_draws) != 1L ||
      !is.finite(max_draws) || max_draws < 1 || max_draws != floor(max_draws))) {
    stop("max_draws must be NULL or a positive integer.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      abs(seed) > .Machine$integer.max || seed != floor(seed)) {
    stop("seed must be an integer.", call. = FALSE)
  }
  chains <- .gls_general_chain_ids(fit)
  if (!is.null(max_draws) && max_draws < length(chains)) {
    old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(if (is.null(old_seed)) rm(".Random.seed", envir = .GlobalEnv)
            else assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  }
  ids <- .gls_general_draw_indices(chains, max_draws, seed)
  c(list(draw_id = ids, chain_id = chains[ids]), .gls_general_draw_fields(fit, ids, fields, draw_dir))
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
  path <- .gls_draw_store_path(fit, draw_dir)
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
