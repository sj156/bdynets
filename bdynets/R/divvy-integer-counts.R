# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

.divvy_integer_counts <- function(x) {
  is.numeric(x) && all(is.finite(x)) && all(x >= 0) &&
    all(abs(x - round(x)) < .Machine$double.eps^0.5)
}

