# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

.divvy_available <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes")
}

