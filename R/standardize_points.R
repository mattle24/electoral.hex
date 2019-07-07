
#' Standard state hexagon points
#'
#' Given a simple feature of hexagon centroids, return
#' a simple features object with standardized x, y
#' @importFrom magrittr %>%
#'
standard_hex_points <- function(hex_centroids_sf) {
  hex_centroids_sf$x <- sf::st_coordinates(hex_centroids_sf)[ ,1]
  hex_centroids_sf$y <- sf::st_coordinates(hex_centroids_sf)[ ,2]

  x_range_hex <-  sf::st_bbox(hex_centroids_sf)[["xmax"]] - sf::st_bbox(hex_centroids_sf)[["xmin"]]
  y_range_hex <-  sf::st_bbox(hex_centroids_sf)[["ymax"]] - sf::st_bbox(hex_centroids_sf)[["ymin"]]

  hex_centroids_sf$x <- (hex_centroids_sf$x - sf::st_bbox(hex_centroids_sf)[["xmin"]]) / x_range_hex
  hex_centroids_sf$y <- (hex_centroids_sf$y - sf::st_bbox(hex_centroids_sf)[["ymin"]]) / y_range_hex

  hex_centroids_sf
}

# in a singular state, get the centroid in standardized units by
# utilizing the bounding box
#' @importFrom magrittr %>%
standard_state_points <- function(cd_sf) {
  cd_sf$lon <- sf::st_coordinates(cd_sf)[ ,1]
  cd_sf$lat <- sf::st_coordinates(cd_sf)[ ,2]

  # get the range of X values and Y values for standardizing
  x_range <-  max(cd_sf$lon) - min(cd_sf$lon)
  y_range <-  max(cd_sf$lat) - min(cd_sf$lat)

  cd_sf$x <- (cd_sf$lon - min(cd_sf$lon)) / x_range
  cd_sf$y <- (cd_sf$lat - min(cd_sf$lat)) / y_range

  cd_sf
}
