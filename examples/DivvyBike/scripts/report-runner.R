run_example_report <- function(base, report, args = commandArgs(TRUE)) {
  usage <- paste0("Usage: Rscript run.R [HH:MM]",
                  " [--offline] [--no-basemap] [--export [--output-dir DIR]]")
  invalid <- function(message = NULL) stop(paste(c(message, usage), collapse = "\n"), call. = FALSE)
  if (length(args) == 1L && args %in% c("--help", "-h")) {
    cat(usage, "\n",
        "HH:MM selects a prepared frame start. With no time argument, the report setting is used.\n",
        "Reports use local basemaps by default. --offline keeps them. --no-basemap omits them.\n",
        "Exports default to a new output/ directory next to run.R.\n",
        "Relative --output-dir paths are resolved from the current working directory.\n", sep = "")
    return(invisible(NULL))
  }

  source(file.path(base, "scripts", "report-export.R"), local = TRUE)
  export_dir <- file.path(base, "output")
  output_option <- grep("^--output-dir($|=)", args)
  if (length(output_option) > 1L) invalid("Supply --output-dir at most once.")
  if (length(output_option)) {
    if (identical(args[[output_option]], "--output-dir")) {
      value <- output_option + 1L
      if (value > length(args) || startsWith(args[[value]], "--")) {
        invalid("Supply a directory after --output-dir.")
      }
      export_dir <- args[[value]]
      args <- args[-c(output_option, value)]
    } else {
      export_dir <- sub("^--output-dir=", "", args[[output_option]])
      args <- args[-output_option]
    }
    if (!"--export" %in% args) invalid("--output-dir requires --export.")
    if (!nzchar(export_dir)) invalid("Supply a directory after --output-dir.")
    export_dir <- report_export_path(export_dir)
  }
  parameters <- list(basemap = !"--no-basemap" %in% args,
                     export = "--export" %in% args)
  positional <- args[!args %in% c("--no-basemap", "--offline", "--export")]
  if (length(positional) > 1L ||
      (length(positional) && !grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", positional[[1L]]))) invalid()
  if (length(positional)) parameters$plot_time <- positional[[1L]]

  final_report <- file.path(base, paste0(report, ".html"))
  export_conflict <- FALSE
  if (parameters$export) {
    require_new_report_export(export_dir)
    candidate <- export_dir
    parent <- dirname(candidate)
    while (!dir.exists(parent) && !identical(parent, candidate)) {
      candidate <- parent
      parent <- dirname(candidate)
    }
    first_new_path <- file.path(normalizePath(parent, winslash = "/", mustWork = FALSE), basename(candidate))
    export_conflict <- identical(first_new_path, final_report)
  }
  if (dir.exists(final_report) || export_conflict) {
    stop("The rendered report needs a file destination: ", final_report, call. = FALSE)
  }

  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      !requireNamespace("knitr", quietly = TRUE) || !rmarkdown::pandoc_available()) {
    stop("Report rendering requires rmarkdown, knitr and Pandoc.", call. = FALSE)
  }
  staged_report <- tempfile(".report-", tmpdir = base, fileext = ".html")
  scratch <- tempfile("report-render-")
  intermediates <- file.path(scratch, "intermediates")
  rendered_dir <- file.path(scratch, "rendered")
  on.exit(unlink(scratch, recursive = TRUE, expand = FALSE), add = TRUE)
  on.exit(unlink(staged_report, expand = FALSE), add = TRUE)
  if (!dir.create(intermediates, recursive = TRUE) || !dir.create(rendered_dir)) {
    stop("Cannot create report rendering directories.", call. = FALSE)
  }

  render_report <- function(destination) {
    if (dir.exists(final_report)) {
      stop("The rendered report needs a file destination: ", final_report, call. = FALSE)
    }
    parameters$export_dir <- destination
    rendered_name <- basename(tempfile("rendered-report-", tmpdir = rendered_dir, fileext = ".html"))
    rendered <- rmarkdown::render(file.path(base, paste0(report, ".Rmd")),
      output_file = rendered_name, output_dir = rendered_dir, intermediates_dir = intermediates,
      knit_root_dir = base, params = parameters, envir = new.env(parent = globalenv()),
      quiet = TRUE, clean = TRUE)
    if (!file.copy(rendered, staged_report)) stop("Cannot stage the rendered report.", call. = FALSE)
  }
  publish_html <- function() {
    if (!suppressWarnings(file.rename(staged_report, final_report))) {
      stop("Cannot publish the rendered report: ", final_report, call. = FALSE)
    }
  }

  started <- proc.time()[["elapsed"]]
  if (parameters$export) {
    # Knitting can finish the export before Pandoc finishes the HTML.
    publish_report_export(export_dir, function(stage) {
      payload <- tempfile("payload-", tmpdir = stage)
      render_report(payload)
      if (!dir.exists(payload)) stop("The report did not produce an export.", call. = FALSE)
      entries <- list.files(payload, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      if (!all(file.rename(entries, file.path(stage, basename(entries)))) ||
          unlink(payload, recursive = TRUE, expand = FALSE) != 0L) {
        stop("Cannot stage the completed report export.", call. = FALSE)
      }
    }, finalize = publish_html)
  } else {
    render_report(export_dir)
    publish_html()
  }
  cat("Built", basename(final_report), "in", round(proc.time()[["elapsed"]] - started, 1), "seconds\n")
}
