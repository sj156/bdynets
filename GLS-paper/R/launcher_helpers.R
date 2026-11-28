# Shared by the simulation and prepared-data command-line launchers.
gls_paper_output_path <- function(output, action, repo) {
  if (is.null(output)) {
    if (action != "plan") stop("--output is required for run or report.")
    return(NULL)
  }
  output <- path.expand(output)
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", output))
    stop("--output must be an absolute external path.")
  parent <- normalizePath(dirname(output), winslash = "/", mustWork = TRUE)
  output <- file.path(parent, basename(output))
  if (startsWith(paste0(output, "/"), paste0(repo, "/")))
    stop("Keep run results outside the repository.")
  if (action != "report" && file.exists(output)) stop("Output already exists; choose a new directory.")
  output
}

gls_paper_options <- function(args, repo, extra = character()) {
  if (any(!grepl("^--[^=]+=.+$", args))) stop("Use --name=value options.")
  keys <- sub("^--([^=]+)=.*$", "\\1", args)
  if (anyDuplicated(keys) || any(!keys %in% c("action", "profile", "cores", "chunk-size", "output", extra)))
    stop("Unknown or repeated option.")
  opts <- list(action = "plan", profile = "smoke", cores = "12", `chunk-size` = "100")
  opts[keys] <- as.list(sub("^--[^=]+=", "", args))
  action <- match.arg(opts$action, c("plan", "run", "report"))
  profile <- match.arg(opts$profile, c("smoke", "quick", "full"))
  cores <- suppressWarnings(as.numeric(opts$cores))
  if (!is.finite(cores) || cores < 1 || cores != floor(cores))
    stop("--cores must be a positive integer.")
  chunk_size <- suppressWarnings(as.numeric(opts[["chunk-size"]]))
  if (!is.finite(chunk_size) || chunk_size < 1 || chunk_size != floor(chunk_size) ||
      chunk_size > .Machine$integer.max)
    stop("--chunk-size must be a positive integer.")
  output <- gls_paper_output_path(opts$output, action, repo)
  list(action = action, profile = profile, cores = cores, chunk_size = as.integer(chunk_size),
       output = output, draw_dir = if (!is.null(output)) file.path(output, "draws"),
       input = opts$input, data_root = opts[["data-root"]])
}

gls_paper_chain_workers <- function(control, cores, windows_parallel = FALSE) {
  physical <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (length(physical) != 1L || is.na(physical) || physical < 1L) physical <- 1L
  possible <- min(control$n_chains, physical)
  if (isTRUE(control$parallel_chains) && (.Platform$OS.type != "windows" || windows_parallel) &&
      cores >= possible && possible > 1L) possible else 1L
}

gls_paper_provenance <- function(config, script, helper_file, workers) {
  list(profile = config$profile, requested_cores = config$cores, chain_workers = workers,
       draw_dir = config$draw_dir, chunk_size = config$chunk_size,
       package_version = as.character(utils::packageVersion("bdynets")),
       package_path = find.package("bdynets"), launcher_md5 = unname(tools::md5sum(script)),
       launcher_helpers_md5 = unname(tools::md5sum(helper_file)), command = commandArgs(),
       started = as.character(Sys.time()), session = capture.output(utils::sessionInfo()),
       status = "started")
}
