# Install this checkout and its required R dependencies, or check an installation.
# From R, source this file and call setup_gls_environment() at the repository root.
setup_gls_environment <- function(repo_root = ".", check = FALSE,
                                  repos = getOption("repos")) {
  pkg <- normalizePath(file.path(repo_root, "bdynets"), winslash = "/", mustWork = TRUE)
  description <- read.dcf(file.path(pkg, "DESCRIPTION"))
  stopifnot(description[1L, "Package"] == "bdynets")
  if (getRversion() < "4.1.0") stop("bdynets requires R 4.1 or later.", call. = FALSE)

  fields <- intersect(c("Depends", "Imports", "LinkingTo"), colnames(description))
  entries <- strsplit(paste(description[1L, fields], collapse = ","), ",", fixed = TRUE)[[1L]]
  deps <- setdiff(trimws(sub("[(].*$", "", entries)), c("R", ""))

  if (!check) {
    if ("bdynets" %in% loadedNamespaces())
      stop("Restart R before reinstalling the loaded bdynets package.", call. = FALSE)
    # R adds this directory to its search path on startup once it exists.
    user_libs <- strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE)[[1L]]
    user_libs <- user_libs[nzchar(user_libs) & user_libs != "NULL"]
    lib <- if (length(user_libs)) path.expand(user_libs[1L]) else .libPaths()[1L]
    if (!dir.exists(lib) && !dir.create(lib, recursive = TRUE))
      stop("Cannot create R library: ", lib, call. = FALSE)
    lib <- normalizePath(lib, winslash = "/")
    .libPaths(c(lib, .libPaths()))

    missing <- deps[!vapply(deps, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) {
      if (!length(repos)) repos <- c(CRAN = "https://cloud.r-project.org")
      repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
      utils::install.packages(missing, lib = lib, repos = repos, dependencies = NA)
      missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
      if (length(missing))
        stop("Could not load required dependencies: ", paste(missing, collapse = ", "),
             call. = FALSE)
    }

    # R CMD INSTALL runs in a child process. Pass along libraries added in R.
    old_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
    on.exit(if (is.na(old_libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old_libs))
    Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
    r <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
    message("Installing ", pkg, " into ", lib)
    status <- system2(r, c("CMD", "INSTALL", "--preclean", "--clean",
                            shQuote(paste0("--library=", lib)), shQuote(pkg)))
    if (status != 0L) stop("bdynets source installation failed. See the compiler or dependency error above.",
                           call. = FALSE)
  }

  if (!requireNamespace("bdynets", quietly = TRUE))
    stop("Cannot load bdynets. Run GLS-paper/setup_environment.R without --check to install it.",
         call. = FALSE)
  installed <- getNamespaceVersion("bdynets")
  if (installed != description[1L, "Version"])
    stop("Loaded bdynets ", installed, "; checkout version is ", description[1L, "Version"],
         ". Run setup again from a fresh R session.", call. = FALSE)
  # Some earlier builds carry the same version but predate disk-backed draws.
  required_api <- c("gls_fit_general", "gls_run_grid_study", "gls_load_draws")
  if (!all(required_api %in% getNamespaceExports("bdynets")) ||
      !all(c("draw_dir", "chunk_size") %in% names(formals(bdynets::gls_fit_general))) ||
      !all(c("draw_dir", "chunk_size") %in% names(formals(bdynets::gls_run_grid_study)))) {
    stop("The loaded bdynets build lacks the draw-storage API used by the GLS launchers. ",
         "Run Rscript GLS-paper/setup_environment.R from this checkout to reinstall it.",
         call. = FALSE)
  }
  control <- bdynets::gls_control("smoke")
  probe <- bdynets::poisson_discount_filter(c(1, 2))
  stopifnot(control$backend == "cpp", length(probe$filtered_mean) == 2L,
            all(is.finite(probe$filtered_mean)))
  location <- getNamespaceInfo("bdynets", "path")
  message("GLS environment ready: bdynets ", installed, " in ", location)
  invisible(location)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) && !identical(args, "--check"))
    stop("Usage: Rscript GLS-paper/setup_environment.R [--check]", call. = FALSE)
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  root <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
  setup_gls_environment(root, check = identical(args, "--check"))
}
