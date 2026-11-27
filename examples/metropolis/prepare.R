# The source map combines three native five-minute observations per quarter hour.
metropolis_bins <- function(values, clock, prepared_values) {
  if (!is.matrix(values) || !nrow(values) || ncol(values) != length(clock) ||
      !is.numeric(clock) || any(!is.finite(clock)))
    stop('Native observations need a road-by-time matrix and complete source times.')
  if (length(clock) != 288L || clock[1] %% 86400 || any(diff(clock) != 300))
    stop('The source day must contain 288 consecutive five-minute slots beginning at midnight.')
  if (any(!is.finite(values) | values < 0))
    stop('The selected roads need complete finite, nonnegative native volume.')

  starts <- seq.int(1L, ncol(values), 3L)
  frame_dims <- c(nrow(values), length(starts))
  if (!identical(dim(prepared_values), frame_dims) || any(!is.finite(prepared_values) | prepared_values < 0))
    stop('Prepared quarter-hour volume must be finite, nonnegative and aligned to the source roads and times.')
  frame_names <- list(rownames(values), NULL)
  dimnames(prepared_values) <- frame_names
  status <- matrix(as.raw(ifelse(values == 0, 2L, 1L)), nrow(values), ncol(values),
                   dimnames = frame_names)
  list(values = prepared_values, times = clock[starts],
       coverage = list(native_times = clock, native_status = status,
                       frame_observed_n = matrix(3L, frame_dims[1], frame_dims[2], dimnames = frame_names)))
}

metropolis_input <- function(native) {
  if (!identical(native$schema, 'metropolis-flow-native-v1'))
    stop('Expected original five-minute volume and static road attributes.')
  ids <- as.character(native$ids)
  static <- native$static
  if (!length(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) ||
      nrow(static) != length(ids) || nrow(native$volume) != length(ids))
    stop('Road IDs, original attributes and volume rows do not align.')
  rownames(native$volume) <- ids
  volume <- metropolis_bins(native$volume, native$times, native$frame_values$volume)
  roads <- data.frame(id = as.integer(ids), length = round(static$length, 1))
  if (all(c('region_id', 'category', 'edge_type', 'speed_limit') %in% names(static))) {
    code <- as.integer(static$speed_limit)
    code[is.na(code)] <- 0L
    limits <- c(NA, 5, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110)
    if (any(!code %in% 0:11)) stop('Unknown source speed-limit code.')
    roads$source_region <- as.integer(static$region_id)
    roads$category <- as.integer(static$category)
    roads$edgeType <- as.integer(static$edge_type)
    roads$speedLimitCode <- code
    roads$speedLimit <- ifelse(code == 0, 'unknown', paste(limits[code + 1L], 'km/h'))
  }
  roads$road_id <- ids
  roads$geometry <- lapply(seq_along(ids), function(i) rbind(
    c(round(static$x_coordinate_start[i], 6), round(static$y_coordinate_start[i], 6)),
    c(round(static$x_coordinate_end[i], 6), round(static$y_coordinate_end[i], 6))))
  times <- volume$times
  flow <- data.frame(road_id = rep(ids, length(times)), time = rep(times, each = length(ids)),
                     flow = as.vector(round(volume$values, 2)))
  means <- native$source_summaries$flow
  if (length(means) != length(times) || any(!is.finite(means)))
    stop('Source volume summaries must match the selected full-day frame domain.')
  trends <- data.frame(time = times, metric = 'flow', value = as.numeric(means),
                       definition = 'Mean unrounded frame volume across the selected source roads.', label = 'flow')
  list(crs = 'EPSG:4326', roads = roads, flow = flow, trends = trends,
       title = 'City-traffic-M traffic volume', time_label_format = '%Y-%m-%d %H:%M',
       times = times, interval_seconds = rep(900, length(times)), unit = 'source volume',
       provenance = list(
         source_dataset = 'mightyneghbor/city-traffic-benchmarks v4',
         clock = 'Source wall clock; timezone is unspecified.',
         selection = sprintf('The supplied %d-road register and %s source date.',
                             length(ids), format(as.POSIXct(native$times[1L], origin = '1970-01-01', tz = 'UTC'),
                                                 '%Y-%m-%d', tz = 'UTC')),
         source_files = native$source_files[c('static-features', 'volume')],
         source_summary_definition = native$summary_definition,
         native_coverage = list(status_codes = c(observed_nonzero = 1L, observed_zero = 2L),
                                volume = volume$coverage)),
       metric_info = list(flow = list(unit = 'source volume',
         definition = 'Mean of the three native five-minute volume values in each quarter hour.')))
}

metropolis_raw_files <- function(raw_dir) {
  file.path(raw_dir, paste0('city-traffic-M-raw-', c('static-features', 'volume'), '.parquet'))
}

read_metropolis_raw <- function(base, raw_dir, selection_file = file.path(base, 'data', 'selected-roads.csv'),
                                python = 'python3', day = '2024-10-18') {
  required <- c(metropolis_raw_files(raw_dir), selection_file)
  if (any(!file.exists(required)))
    stop('Missing original Parquet input or road register: ',
         paste(basename(required[!file.exists(required)]), collapse = ', '))
  path <- tempfile('metropolis-native-', fileext = '.json')
  diagnostic <- tempfile('parquet-reader-', fileext = '.log')
  on.exit(unlink(c(path, diagnostic)), add = TRUE)
  arguments <- c(file.path(base, 'read_parquet.py'), raw_dir, selection_file, path, day)
  status <- system2(python, shQuote(arguments), stderr = diagnostic)
  if (status != 0L) {
    lines <- readLines(diagnostic, warn = FALSE)
    detail <- grep('^(ValueError|ModuleNotFoundError|ImportError|KeyError|IndexError):', lines, value = TRUE)
    stop('The source Parquet reader failed. ',
         if (length(detail)) tail(detail, 1) else 'Check original fields and Python, NumPy, pandas and PyArrow.')
  }
  native <- jsonlite::read_json(path, simplifyVector = TRUE)
  native$static <- as.data.frame(native$static)
  native$times <- as.numeric(native$times)
  native
}

prepare_example <- function(base = '.', output_dir = file.path(base, 'prepared'), raw_dir = NULL, python = 'python3') {
  if (is.null(raw_dir)) raw_dir <- file.path(base, 'data', 'raw')
  selection_file <- file.path(base, 'data', 'selected-roads.csv')
  write_metropolis_prepared(
    read_metropolis_raw(base, raw_dir, selection_file = selection_file, python = python),
    output_dir, raw_dir, selection_file)
}

write_metropolis_prepared <- function(native, output_dir, raw_dir, selection_file) {
  meanings <- c(id = 'Zero-based static Parquet row ID.',
                length = 'Source road length in metres, rounded to one decimal.',
                source_region = 'Source region_id category.', category = 'Source category code.',
                edgeType = 'Source edge_type code.', speedLimitCode = 'Source speed_limit category.',
                speedLimit = 'Source speed-limit category decoded using the source map lookup.')
  dictionary <- rbind(data.frame(table = 'roads', field = names(meanings), meaning = unname(meanings)),
    data.frame(table = 'trends', field = c('definition', 'label'),
               meaning = c('Definition of this derived source summary.', 'Display label for the summary metric.')))
  write_traffic_input(metropolis_input(native), output_dir,
    source_files = c(metropolis_raw_files(raw_dir), selection_file),
    native = native, dictionary = dictionary)
}

if (sys.nframe() == 0L) {
  script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
  if (length(script) != 1L) stop('Rscript entrypoint is required.')
  if (!file.exists(script)) script <- gsub('~+~', ' ', script, fixed = TRUE)
  base <- dirname(normalizePath(script, mustWork = TRUE))
  source(file.path(base, 'scripts', 'prepare-input.R'))
  args <- commandArgs(TRUE)
  if (!length(args)) {
    result <- prepare_example(base)
  } else if (length(args) == 1L && !startsWith(args[1], '--')) {
    result <- prepare_example(base, output_dir = args[1])
  } else {
    stop('Usage: Rscript prepare.R [NEW_DIRECTORY]')
  }
  cat('Drawing input', file.path(dirname(result), 'flow-frames.rds'), '\n')
}
