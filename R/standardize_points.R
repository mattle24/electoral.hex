#' Standard state hexagon points
#'
#' Given a simple feature of hexagon centroids for a state, return a simple
#' features object with standardized x, y.
#'
#' @param hex_centroids sf. An object containing the hexagon centroids.
#'
#' @importFrom magrittr %>%
standard_hex_points <- function(hex_centroids) {
  hex_centroids$x <- sf::st_coordinates(hex_centroids)[ ,1]
  hex_centroids$y <- sf::st_coordinates(hex_centroids)[ ,2]

  x_range_hex <-  sf::st_bbox(hex_centroids)[["xmax"]] - sf::st_bbox(hex_centroids)[["xmin"]]
  y_range_hex <-  sf::st_bbox(hex_centroids)[["ymax"]] - sf::st_bbox(hex_centroids)[["ymin"]]

  hex_centroids$x <- (hex_centroids$x - sf::st_bbox(hex_centroids)[["xmin"]]) / x_range_hex
  hex_centroids$y <- (hex_centroids$y - sf::st_bbox(hex_centroids)[["ymin"]]) / y_range_hex

  hex_centroids
}

#' Standard state points
#'
#' Given a simple feature of true centroids for a state, return a simple
#' features object with standardized x, y.
#'
#' @param real_centroids sf. An object containing the true Congressional
#'   centroids.
#' @importFrom magrittr %>%
standard_state_points <- function(real_centroids) {
  real_centroids$lon <- sf::st_coordinates(real_centroids)[ ,1]
  real_centroids$lat <- sf::st_coordinates(real_centroids)[ ,2]

  # get the range of X values and Y values for standardizing
  x_range <-  max(real_centroids$lon) - min(real_centroids$lon)
  y_range <-  max(real_centroids$lat) - min(real_centroids$lat)

  real_centroids$x <- (real_centroids$lon - min(real_centroids$lon)) / x_range
  real_centroids$y <- (real_centroids$lat - min(real_centroids$lat)) / y_range

  real_centroids
}
