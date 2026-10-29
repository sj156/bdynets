# Divvy Bike OD utilities for the bdynets package.
# This file is intentionally limited to one function.

#' Read the bundled, clipped City of Chicago street centre-line background
#'
#' The RDS is a compact derivative of Transportation.geojson from the City of
#' Chicago's open-source street-centre-line repository. It is bundled so report
#' rendering is reproducible without a live map-tile service.
#'
#' @param street_file Path to chicago_streets_divvy.rds.
#' @param bounds Optional numeric c(west,east,south,north) used for cropping.
#' @return A data.table of ordered street-line vertices.
#' @export
read_divvy_street_background <- function(
    street_file = file.path("data", "chicago_streets_divvy.rds"),
    bounds = NULL) {
  .divvy_require("data.table")
  if (!file.exists(street_file)) {
    stop(
      "Missing bundled street background: ", street_file,
      ". Keep cache/chicago_streets_divvy.rds beside the Rmd.",
      call. = FALSE
    )
  }
  streets <- data.table::as.data.table(readRDS(street_file))
  required <- c("street_group", "street_class", "longitude", "latitude")
  if (length(setdiff(required, names(streets)))) {
    stop("Invalid Chicago street background file.", call. = FALSE)
  }
  if (!is.null(bounds)) {
    if (length(bounds) != 4L || any(!is.finite(bounds))) stop("Invalid map bounds.")
    # Keep full polylines that have at least one vertex inside the map window.
    keep_groups <- streets[
      longitude >= bounds[1] & longitude <= bounds[2] &
        latitude >= bounds[3] & latitude <= bounds[4],
      unique(street_group)
    ]
    streets <- streets[street_group %in% keep_groups]
  }
  streets
}

