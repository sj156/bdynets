# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

# Divvy OD aggregation and static mapping utilities.
#
# The functions in this file deliberately keep raw-data preparation separate
# from reporting.  Radius aggregation starts from nodes_raw.csv and the sparse
# hourly OD table, preserves the total trip count, and records the membership
# of every raw station node in its aggregate node.

.divvy_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Install the '", package, "' package before using this function.",
         call. = FALSE)
  }
}

