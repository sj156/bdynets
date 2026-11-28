# Both case and application drivers source this file; resolve the Python
# frontend relative to this file rather than the invoker's working directory.
.graph_wavenet_driver <- local({
  path <- Filter(function(x) length(x) == 1L && basename(x) == "external.R",
                 lapply(sys.frames(), function(frame) frame$ofile))
  file.path(dirname(normalizePath(tail(path, 1L)[[1L]])), "graph_wavenet.py")
})

# Inspect the executable and libraries that will actually run the model. Keep
# an unavailable dependency representable so failed jobs can still be counted.
graph_wavenet_runtime <- function() {
  python <- Sys.getenv("BDYNETS_PYTHON", unset="python3")
  if (!nzchar(Sys.which(python))) return(list(available=FALSE, reason="missing interpreter"))
  query <- paste(
    "import json,platform,sys,numpy,torch",
    "print(json.dumps(dict(python=platform.python_version(),",
    "implementation=platform.python_implementation(), platform=sys.platform,",
    "machine=platform.machine(), numpy=numpy.__version__, torch=str(torch.__version__),",
    "torch_git=str(torch.version.git_version), torch_config=torch.__config__.show())))",
    sep="\n")
  result <- suppressWarnings(system2(python,c("-c",shQuote(query)),stdout=TRUE,stderr=TRUE))
  if (!is.null(attr(result,"status"))) return(list(available=FALSE, reason="Python import failed"))
  runtime <- jsonlite::fromJSON(tail(result,1L),simplifyVector=FALSE)
  list(available=TRUE,versions=runtime)
}

.graph_wavenet_pin <- list(commit="6b162e80c59a1d494809252eca055cff93dc66b1",
  sha256="f357319260657716c18916d691d31179e2380dcb882b9c41556c9a55912f1059")

graph_wavenet_model_identity <- function(output) {
  local <- Sys.getenv("BDYNETS_GWNET_MODEL")
  path <- if(nzchar(local)) local else file.path(output,"upstream-model.py")
  # The fetch on first execution verifies this pin before fitting.
  if(!nzchar(local) && !file.exists(path)) return(.graph_wavenet_pin$sha256)
  if(!file.exists(path)) return("missing-explicit-upstream-model")
  digest::digest(file=path,algo="sha256")
}

external_forecast <- function(method, panel, profile, seed, output) {
  profile <- match.arg(profile, c("smoke","quick","pilot","full"))
  method <- match.arg(method, c("sa-pgss","graph-wavenet"))
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(output, mustWork = TRUE)
  s <- panel$split
  if (method == "sa-pgss") {
    args <- list(Y = panel$Y, train = s$train, validation = s$validation, test = s$test, seed = seed)
    if (profile == "smoke") {
      args$grid <- expand.grid(discount = c(.8,.95), phi = .8, sigma = .15)
      args$tuning_particles <- 30L
      args$particles <- 80L
    }
    out <- do.call(bdynets::gls_sa_pgss, args)
    saveRDS(list(selected = out$selected, validation_scores = out$validation_scores,
                 settings = out$provenance), file.path(output, "sa-pgss-tuning.rds"))
    return(out)
  }
  # Official nnzhan/Graph-WaveNet implementation, also usable offline when
  # BDYNETS_GWNET_MODEL is set to the pinned file in a local checkout.
  commit <- .graph_wavenet_pin$commit
  hash <- .graph_wavenet_pin$sha256
  url <- sprintf("https://raw.githubusercontent.com/nnzhan/Graph-WaveNet/%s/model.py", commit)
  model <- Sys.getenv("BDYNETS_GWNET_MODEL")
  if (!nzchar(model)) {
    model <- file.path(output, "upstream-model.py")
    if (!file.exists(model)) {
      temp <- tempfile("upstream-", output)
      on.exit(unlink(temp), add = TRUE)
      utils::download.file(url, temp, quiet = TRUE, mode = "wb")
      if (!identical(digest::digest(file = temp, algo = "sha256"), hash) ||
          !file.rename(temp, model)) stop("Could not verify and save upstream Graph WaveNet.")
    }
  }
  if (!file.exists(model) || !identical(digest::digest(file = model, algo = "sha256"), hash))
    stop("Graph WaveNet requires the pinned upstream model.py from ", url)
  python <- Sys.getenv("BDYNETS_PYTHON", unset = "python3")
  if (!nzchar(Sys.which(python))) stop("Set BDYNETS_PYTHON to a Python with NumPy and PyTorch.")
  work <- tempfile("gwnet-input-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  write_matrix <- function(x, file) utils::write.table(x, file.path(work, file),
    sep = ",", row.names = FALSE, col.names = FALSE)
  write_matrix(panel$Y, "Y.csv")
  write_matrix(panel$graph$A, "A.csv")
  if (ncol(panel$covariates)) write_matrix(panel$covariates, "Q.csv")
  if (!identical(as.numeric(unlist(s[c("train","validation","test")], use.names = FALSE)),
                 as.numeric(seq_len(nrow(panel$Y)))))
    stop("Training, validation and test indices must partition the count panel chronologically.")
  args <- c(.graph_wavenet_driver, "--model", normalizePath(model),
            "--input", work, "--output", output, "--profile", profile,
            "--train", length(s$train), "--validation", length(s$validation), "--seed", seed)
  log_file <- file.path(output, "graph-wavenet.log")
  status <- system2(python, vapply(as.character(args), shQuote, character(1)),
                    stdout = log_file, stderr = log_file)
  if (status != 0L) stop("Graph WaveNet failed: ", paste(tail(readLines(log_file), 4L), collapse = "\n"))
  pred <- as.matrix(utils::read.csv(file.path(output, "graph-wavenet-mean.csv"), header = FALSE))
  if (!identical(dim(pred), c(length(s$test), ncol(panel$Y))) || any(!is.finite(pred) | pred < 0))
    stop("Invalid Graph WaveNet forecast dimensions or means.")
  list(mean = unname(pred), provenance = jsonlite::read_json(file.path(output, "graph-wavenet.json")))
}
