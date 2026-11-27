# A run manifest binds every chain to one immutable data/control configuration.
# Committed draw files are immutable. Checkpoint generations are published by
# renaming a new file, so replacement does not rely on platform overwrite rules.
.gls_checkpoint_directory <- function(path, every) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop("checkpoint_dir must be one directory path.", call. = FALSE)
  if (!is.numeric(every) || length(every) != 1L || !is.finite(every) ||
      every < 1 || every > .Machine$integer.max || every != floor(every))
    stop("checkpoint_every must be a positive integer.", call. = FALSE)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.gls_checkpoint_lock <- function(path) {
  lock <- filelock::lock(path, timeout = 0)
  if (is.null(lock)) stop("Another process owns checkpoint lock: ", path, call. = FALSE)
  lock
}

.gls_checkpoint_publish <- function(object, path) {
  temp <- tempfile(".pending-", tmpdir = dirname(path))
  on.exit(unlink(temp), add = TRUE)
  saveRDS(object, temp, compress = FALSE)
  if (file.exists(path) || !file.rename(temp, path))
    stop("Could not commit checkpoint: ", path, call. = FALSE)
}

.gls_checkpoint_store <- function(path, draw_dir, chunk_size, every, resume, inputs) {
  if (RNGkind()[2L] == "Box-Muller" || any(RNGkind() == "user-supplied"))
    stop("Choose an R RNG whose full state is kept in .Random.seed (e.g. the default Inversion normal generator).",
         call. = FALSE)
  settings <- list(format = 1L, data_control = digest::digest(inputs, algo = "sha256"),
    chunk_size = as.numeric(chunk_size), every = as.numeric(every),
    runtime = list(R.version$version.string, R.version$platform, RNGkind(),
      extSoftVersion(), as.character(utils::packageVersion("bdynets")),
      as.character(utils::packageVersion("BayesLogit"))))
  manifest <- file.path(path, "run.rds")
  if (file.exists(manifest)) {
    if (!resume) stop("Checkpoint run already exists; use resume = TRUE or a new directory.", call. = FALSE)
    saved <- readRDS(manifest)
    if (!identical(settings, saved$settings))
      stop("Inputs, controls, storage settings or runtime differ from the checkpoint.", call. = FALSE)
    store <- saved$store
    if (!dir.exists(store$path)) stop("Checkpoint draw directory is missing: ", store$path, call. = FALSE)
    if (!is.null(draw_dir) && !identical(normalizePath(draw_dir, winslash = "/", mustWork = TRUE),
                                         dirname(store$path)))
      stop("draw_dir differs from the checkpoint manifest.", call. = FALSE)
  } else {
    # A worker only starts after this manifest is committed.
    if (resume) stop("No committed run manifest in checkpoint_dir.", call. = FALSE)
    if (length(setdiff(list.files(path), "run.lock")))
      stop("A new checkpoint_dir must be empty.", call. = FALSE)
    store <- .gls_prepare_draw_store(if (is.null(draw_dir)) path else draw_dir, chunk_size)
    .gls_checkpoint_publish(list(settings = settings, store = store), manifest)
  }
  store$checkpoint_dir <- path
  store$checkpoint_every <- as.integer(every)
  store
}

.gls_chain_snapshots <- function(dir) {
  sort(list.files(dir, pattern = "^state-[0-9]{10}\\.rds$", full.names = TRUE))
}

.gls_commit_chain <- function(record, dir) {
  path <- file.path(dir, sprintf("state-%010d.rds", record$iteration))
  .gls_checkpoint_publish(record, path)
  unlink(setdiff(.gls_chain_snapshots(dir), path))
}

.gls_restore_chain <- function(dir, store, chain_id) {
  snapshots <- .gls_chain_snapshots(dir)
  record <- if (length(snapshots)) readRDS(tail(snapshots, 1L)) else NULL
  chain <- sprintf("chain-%04d", chain_id)
  index <- record$index
  if (!is.null(record)) {
    if (!identical(chain_id, record$chain_id) ||
        !identical(index$files, file.path(chain, sprintf("chunk-%08d.rds", seq_along(index$files)))))
      stop("Invalid checkpoint chain or chunk index.", call. = FALSE)
    if (length(index$files) &&
        !identical(unname(tools::md5sum(file.path(store$path, index$files))), index$hashes))
      stop("Missing or corrupt committed draw chunk for chain ", chain_id, ".", call. = FALSE)
  }
  # A killed process can have published draw chunks after its last chain
  # checkpoint. They are reproducible from the preceding state and RNG.
  draws <- file.path(store$path, chain)
  tail_chunks <- setdiff(list.files(draws, "^chunk-[0-9]+\\.rds$"), basename(as.character(index$files)))
  if (length(tail_chunks) && unlink(file.path(draws, tail_chunks)) != 0L)
    stop("Cannot remove uncommitted draw chunks.", call. = FALSE)
  unlink(list.files(draws, "^\\.chunk-", all.files = TRUE, full.names = TRUE))
  unlink(list.files(dir, "^\\.pending-", all.files = TRUE, full.names = TRUE))
  if (length(snapshots) > 1L) unlink(head(snapshots, -1L))
  record
}

.gls_general_state_names <- function(estimate_tau, learn_affinity) {
  c("alpha", "gamma", "beta", "L", "B", "H", "offset", "ref", "discount",
    "u", "v", "nu", "affinity_sq", "static_log_step", "covariance_accepted",
    "covariance_proposed", "accepted", "proposed", "scale_log_jump",
    "interweave_log_sd", "interweave_accepted", "interweave_log_jump",
    if (estimate_tau) c("xi", "move_sd"),
    if (learn_affinity) c("chi", "chi_log_sd", "chi_accepted"))
}
