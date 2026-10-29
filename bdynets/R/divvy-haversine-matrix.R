# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

#' Pairwise great-circle distance matrix
#'
#' @param latitude,longitude Numeric vectors in degrees.
#' @return Symmetric distance matrix in metres.
#' @export
divvy_haversine_matrix <- function(latitude, longitude) {
  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)
  if (length(latitude) != length(longitude) || !length(latitude) ||
      any(!is.finite(latitude)) || any(!is.finite(longitude))) {
    stop("Invalid coordinates.", call. = FALSE)
  }
  rad <- pi / 180
  lat <- latitude * rad
  lon <- longitude * rad
  dlat <- outer(lat, lat, "-")
  dlon <- outer(lon, lon, "-")
  a <- sin(dlat / 2)^2 + outer(cos(lat), cos(lat), "*") * sin(dlon / 2)^2
  # Subassignment keeps the matrix dimensions; pmin/pmax with a scalar first
  # argument can otherwise drop them on some R versions.
  a[a < 0] <- 0
  a[a > 1] <- 1
  2 * 6371008.8 * asin(sqrt(a))
}

