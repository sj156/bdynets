# Read the case's separately prepared, attributed OpenStreetMap background.
read_case_streets <- function(paths) {
  coordinates <- function(rows) do.call(rbind, lapply(rows, unlist, use.names = FALSE))
  collections <- lapply(paths, function(path) {
    connection <- gzfile(path, "rb")
    on.exit(close(connection))
    map <- jsonlite::fromJSON(connection, simplifyVector = FALSE)
    map$features <- lapply(map$features, function(feature) {
      geometry <- feature$geometry
      geometry$coordinates <- switch(geometry$type,
        Point = unlist(geometry$coordinates, use.names = FALSE),
        LineString = coordinates(geometry$coordinates),
        Polygon = lapply(geometry$coordinates, coordinates))
      feature$geometry <- geometry
      feature
    })
    map
  })
  list(type = "FeatureCollection",
    features = unlist(lapply(collections, `[[`, "features"), recursive = FALSE),
    attribution = collections[[1L]]$attribution,
    provenance = list(requests = lapply(collections, `[[`, "provenance")))
}
