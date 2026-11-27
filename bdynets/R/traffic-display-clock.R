.od_parse_time <- function(x, timezone = "UTC", time_format = NULL) {
  if (!is.character(timezone) || length(timezone) != 1L || is.na(timezone) ||
      !timezone %in% c("UTC", "GMT", OlsonNames())) {
    stop("timezone must be one recognized IANA timezone.", call. = FALSE)
  }
  if (inherits(x, "POSIXt")) {
    value <- as.numeric(as.POSIXct(x))
  } else {
    if (!is.character(x) && !is.factor(x)) {
      stop("Times must be POSIXt instants or explicit character timestamps.", call. = FALSE)
    }
    text <- trimws(as.character(x))
    if (anyNA(text) || any(!nzchar(text))) stop("Missing timestamps.", call. = FALSE)
    if (!is.null(time_format)) {
      if (!is.character(time_format) || length(time_format) != 1L || is.na(time_format) || !nzchar(time_format)) {
        stop("time_format must be one format string.", call. = FALSE)
      }
      value <- as.numeric(as.POSIXct(strptime(text, format = time_format, tz = timezone)))
    } else {
      pattern <- paste0("^([0-9]{4}-[0-9]{2}-[0-9]{2})",
        "(?:[T ]([0-9]{2}:[0-9]{2})(?::([0-9]{2}(?:\\.[0-9]+)?))?",
        "([Zz]|[+-][0-9]{2}:?[0-9]{2})?)?$")
      parts <- regmatches(text, regexec(pattern, text, perl = TRUE))
      if (any(lengths(parts) != 5L)) {
        stop("Unsupported or incomplete timestamp; supply a complete ISO timestamp or an explicit time_format.", call. = FALSE)
      }
      value <- rep(NA_real_, length(text))
      if (length(text)) {
        parts <- do.call(rbind, parts)
        clock <- ifelse(nzchar(parts[, 3]), parts[, 3], "00:00")
        seconds <- ifelse(nzchar(parts[, 4]), parts[, 4], "00")
        stamp <- paste(parts[, 2], paste(clock, seconds, sep = ":"))
        offset <- which(nzchar(parts[, 5]))
        local <- which(!nzchar(parts[, 5]))
        if (length(offset)) {
          zone <- ifelse(tolower(parts[offset, 5]) == "z", "+0000",
            gsub(":", "", parts[offset, 5], fixed = TRUE))
          value[offset] <- as.numeric(as.POSIXct(strptime(paste0(stamp[offset], zone),
            format = "%Y-%m-%d %H:%M:%OS%z", tz = "UTC")))
        }
        if (length(local)) {
          parsed <- as.POSIXct(strptime(stamp[local], format = "%Y-%m-%d %H:%M:%OS", tz = timezone))
          value[local] <- as.numeric(parsed)
          # Nonexistent wall-clock times must not silently move to another hour.
          shifted <- !is.na(parsed) & format(parsed, "%Y-%m-%d %H:%M", tz = timezone) != paste(parts[local, 2], clock[local])
          value[local[shifted]] <- NA_real_
        }
      }
    }
  }
  if (any(!is.finite(value))) stop("Invalid timestamps for the supplied format and timezone.", call. = FALSE)
  as.POSIXct(value, origin = "1970-01-01", tz = timezone)
}


# One relative double-precision rounding unit at the scale of the operands.
.tm_time_resolution <- function(...) .Machine$double.eps * pmax(...)

# Find the shortest decimal fraction that matches the stored start at clock precision.
.tm_clock_fractions <- function(times, scale = abs(times)) {
  whole <- floor(times)
  fraction <- times - whole
  labels <- rep("", length(times))
  remaining <- which(fraction != 0)
  for (digits in seq_len(17L)) {
    if (!length(remaining)) break
    candidate <- round(fraction[remaining], digits)
    matched <- candidate > 0 & candidate < 1 &
      abs(whole[remaining] + candidate - times[remaining]) <= .tm_time_resolution(scale[remaining])
    labels[remaining[matched]] <- sub("^0", "", sprintf("%.*f", digits, candidate[matched]))
    remaining <- remaining[!matched]
  }
  if (length(remaining)) labels[remaining] <- sub("^0", "", format(fraction[remaining],
    digits = 17L, scientific = FALSE, trim = TRUE))
  labels
}

# Subtract a fractional suffix from one as decimal text, without cancellation.
.tm_fraction_complement <- function(label) {
  digits <- strsplit(substring(label, 2L), "", fixed = TRUE)[[1L]]
  nonzero <- which(digits != "0")
  if (!length(nonzero)) return("")
  last <- utils::tail(nonzero, 1L)
  prefix <- if (last > 1L) chartr("0123456789", "9876543210", substring(label, 2L, last)) else ""
  paste0(".", prefix, 10L - as.integer(digits[last]))
}

# Select the stored start, independently of the label chosen for display.
.tm_start_request <- function(label) {
  pattern <- paste0("^(?:([0-9]{4}-[0-9]{2}-[0-9]{2})[ T])?",
    "(-?)([0-9]{2,}):([0-5][0-9])(?::([0-5][0-9])([.][0-9]+)?)?",
    "(?: ?([Zz]|[+-][0-9]{2}:?[0-9]{2}))?$")
  parts <- regmatches(label, regexec(pattern, label, perl = TRUE))[[1L]]
  if (!length(parts)) return(NULL)
  offset <- gsub(":", "", parts[8L], fixed = TRUE)
  if (offset %in% c("Z", "z")) offset <- "+0000"
  list(date = parts[2L], negative = parts[3L] == "-", hour = as.numeric(parts[4L]),
    minute = as.numeric(parts[5L]), second = as.numeric(paste0("0", parts[6L])),
    fraction = as.numeric(paste0("0", parts[7L])), fraction_text = parts[7L], offset = offset)
}

.tm_matches_start <- function(times, request, calendar, timezone = NULL) {
  if (is.null(request)) return(rep(FALSE, length(times)))
  if (!calendar) {
    if (nzchar(request$date) || nzchar(request$offset)) return(rep(FALSE, length(times)))
    seconds <- request$hour * 3600 + request$minute * 60 + request$second + request$fraction
    requested <- if (request$negative) -seconds else seconds
    if (request$fraction == 0) return(times == requested)
    return(abs(times - requested) <= .tm_time_resolution(abs(times), seconds))
  }
  if (request$negative || request$hour > 23) return(rep(FALSE, length(times)))
  timezone <- .tm_display_timezone(timezone)
  whole <- floor(times)
  local <- as.POSIXct(whole, origin = "1970-01-01", tz = timezone)
  clock <- sprintf("%02.0f:%02.0f:%02.0f", request$hour, request$minute, request$second)
  requested <- whole + request$fraction
  negative <- which(whole < 0 & request$fraction > 0)
  if (length(negative)) {
    remainder <- as.numeric(paste0("0", .tm_fraction_complement(request$fraction_text)))
    requested[negative] <- -(-whole[negative] - 1 + remainder)
  }
  # POSIXlt parsing may round the seconds field before combining it with the date.
  same_start <- if (request$fraction == 0) times == requested else
    abs(times - requested) <= .tm_time_resolution(abs(times), abs(requested), request$second)
  matches <- format(local, "%H:%M:%S", tz = timezone) == clock & same_start
  if (nzchar(request$date)) matches <- matches & format(local, "%Y-%m-%d", tz = timezone) == request$date
  if (nzchar(request$offset)) matches <- matches & format(local, "%z", tz = timezone) == request$offset
  matches
}
