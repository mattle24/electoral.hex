#' Match districts
#'
#' For one state, given a dataset of centroids for the true Congressional
#' districts and a dataset of centroids for hexagon Congressional districts,
#' assign the hexagon districts to closely mirror the real geography of the
#' state's districts.
#'
#' @param hex_centroids sf. An object with hexagon Congressional district
#'   geographies where the geography is centroids.
#' @param state_fips numeric. An integer with the state FIPS to match.
#' @param iter numeric. An integer with the maximum number of iterations of
#'   optimization to preform. Default is 10.
#' @param wts character. Weights to use for optimization. For now, either
#'   "log_area" or NULL. Defaults to NULL.
#' @param compare boolean. Whether or not to print a plot showing the real
#'   centroids and the hexagon centroids on a standardized scale. Good for
#'   checking if the assigned districts are correct. Default is FALSE.
#'
#' @return A vector with the district assignments in the same order as
#'   `hex_centroids`.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang %||%
#' @importFrom purrr map2
#' @export
match_districts_state <- function(state_fips, hex_centroids,
                                  iter = 10, wts = NULL, compare = FALSE) {
  # get state centroids
  real_centroids <- electoral.hex::real_cd_centroids_sf %>%
    dplyr::filter(fips == !!state_fips)

  if (!is.null(wts) && wts == "log_area") {wts <- log(real_centroids$area_land)} else wts <- NULL


  if (nrow(real_centroids) != nrow(hex_centroids)) {
    abort(paste0("Real and hex have different number of rows. FIPS ", state_fips))
  }

  # if only one district, assignment is easy!
  if (nrow(hex_centroids) == 1) {
    return(1)
  }

  # standardize centroid points
  hex_centroids <- hex_centroids %>%
    standard_hex_points()

  real_centroids <- real_centroids %>%
    standard_state_points() %>%
    dplyr::arrange(district)

  n_districts <- as.integer(nrow(real_centroids))


  data <- purrr::map2(hex_centroids$x, hex_centroids$y,
                      ~ distance(.x, .y, real_centroids$x, real_centroids$y))
  # D is the matrix that has distances from each centroid to each district centroid.
  # Hexes are rows, districts are columns.
  # Eg, D[5, 9] refers to the distance between the fifth he
  D <- matrix(data = unlist(data), nrow = n_districts, ncol = n_districts, byrow = TRUE)

  # if two districts, can get pretty common edge case where both hexagon
  # centroids are equidistant from both real centroids (standardized)
  if (n_districts == 2 && all(D == 1)) {
    # match by top hexagon = top district and bottom hexagon = bottom district
    districts <- rep(NA_real_, n_districts)
    # which hexagon is higher?
    districts[match(1, hex_centroids$y)] <- match(1, real_centroids$y)
    districts[match(0, hex_centroids$y)] <- match(0, real_centroids$y)
    if (compare) {
      hex_centroids$district <- districts
      print(compare_plot(real_pts = real_centroids, hex_pts = hex_centroids))
    }
    return(districts)
  }

  # initialize assignments by using minimizing distance greedily
  D_copy <- D # make a copy to play with
  districts_initial <- c() # to store district assignments
  for (i in 1L:n_districts) {
    min_dist_index <- which.min(D_copy[i, ]) # column index for the district w/ minimum distance
    districts_initial <- c(districts_initial, min_dist_index) # keep track of what districts have been assigned
    D_copy[ ,min_dist_index] <- NA_real_ # remove this district from further options
  }

  # X is the matrix that shoes assignment of hexes to districts
  # Hexes are rows, districts are columns.
  # Eg, X[3, 6] = 1 means that the third is assigned to District 6
  # Each row and each column should have one and only one `1`
  X <- matrix(0, nrow = n_districts, ncol = n_districts)
  # random_assignment <- sample(n_districts, n_districts)
  for (r in 1L:n_districts) X[r, districts_initial[r]] <- 1
  wts <- wts %||% rep(1, n_districts) # default weights are 1s for all
  curr_loss <- loss_function(D, X, wts) # loss to beat
  going <- TRUE # should the optimizer keep going?
  runs <- 0 # dev variable to keep track of how many runs

  # now, iterate through and see if any should be changed
  while (going) {
    n_changes <- 0 # dev variable to keep track of how many changes
    for (i in 1L:n_districts) {
      for (q in 1L:n_districts) {
        if (i == q) next
        runs <- runs + 1
        # switch indices and see if that does anything
        ind_i <- match(1, X[i, ]) # index of hex i's district
        ind_q <- match(1, X[q, ]) # index of hex q's district
        Y <- X # make a copy of `X` to play around with
        Y[i, ] <- 0 # switch district assignment of hex i and hex q
        Y[i, ind_q] <- 1
        Y[q, ] <- 0
        Y[q, ind_i] <- 1
        new_loss <- loss_function(D, Y, wts) # calculate new loss
        if (new_loss < curr_loss) {
          # if the new loss is less than the current loss, reassign the
          # districts (by making `Y` the new `X`) and update what current loss
          # is
          X <- Y
          curr_loss <- new_loss
          n_changes <- n_changes + 1
          # dev print statement to see how assignment works
          # print(glue::glue("{i} is changing from district {ind_i} to {ind_q}"))
        }
      }
    }
    iter <- iter - 1 # update iterations left
    going <- iter > 0 && n_changes > 0 # if no iterations left or no changes were
                                       # made in the last iteration, then stop
  }
  # find out what the final assignments are and store them in a vector
  districts <- c()
  for (i in 1L:n_districts) districts <- c(districts, match(1, X[i, ]))

  # dev: if compare, then print a plot comparing the real centroids to hex centroids
  # with labels to see how accurate assignment is
  if (compare) {
    hex_centroids$district <- districts
    print(compare_plot(real_pts = real_centroids, hex_pts = hex_centroids))
  }
  # browser() # for debugging
  districts
}


#' Match districts usa
#'
#' For any number of states, given a dataset of centroids for the true
#' Congressional districts and a dataset of centroids for hexagon Congressional
#' districts, assign the hexagon districts to closely mirror the real geography
#' of the state's districts.
#'
#' @param congress_hex sf. An object with geometries for Congressional hexagons.
#'   Geometries are polygons.
#' @param fips_key character. The name of the field in `congress_hex` which has
#'   state fips codes. Default is "fips".
#' @param iter numeric. An integer with the maximum number of iterations of
#'   optimization to preform. Default is 10.
#' @param wts character. Weights to use for optimization. For now, either
#'   "log_area" or NULL.
#' @param compare boolean. Whether or not to print a plot showing the real
#'   centroids and the hexagon centroids on a standardized scale. Good for
#'   checking if the assigned districts are correct. Default is FALSE.
#'
#' @return Returns the `congress_hex` object given as an argument with a field
#'   for assigned district number.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang abort
#' @export
match_districts_usa <- function(congress_hex,
                                fips_key = "fips",
                                iter = 10,
                                wts = NULL,
                                compare = FALSE) {
  congress_hex <- congress_hex %>%
    dplyr::mutate(.res_matching_id = dplyr::row_number())

  hex_centroids <- congress_hex %>%
    sf::st_centroid()

  res_list <- list() # list to hold state results
  fips <- hex_centroids[ , fips_key]  # get vector of fips to iterate through
  sf::st_geometry(fips) <- NULL
  fips <- unlist(fips)
  # iterate through unique fips. find best matches of hexes to districts and
  # append to the results list
  for (f in sort(unique(fips))) {
    # filter to state
    state_hex_centroids <- hex_centroids %>%
      dplyr::filter(!!as.name(fips_key) == f)


    state_hex_centroids$district <- match_districts_state(hex_centroids = state_hex_centroids,
                                                           state_fips = f,
                                                           iter = iter,
                                                           wts = "log_area")
  state_hex_centroids$geometry <- NULL
    res_list[[length(res_list) + 1]] <- state_hex_centroids %>%
      dplyr::select(district, .res_matching_id)
  }

  res_df <- purrr::reduce(res_list, rbind) %>%
    dplyr::select(district, .res_matching_id)

  congress_hex %>%
    dplyr::left_join(res_df, by = ".res_matching_id") %>%
    dplyr::select(-.res_matching_id)
}

#' Compare plot
#'
#' @importFrom magrittr %>%
compare_plot <- function(real_pts, hex_pts) {
  sf::st_geometry(real_pts) <- sf::st_sfc(purrr::map2(real_pts$x, real_pts$y, ~ sf::st_point(c(.x, .y))))

  sf::st_geometry(hex_pts) <- sf::st_sfc(purrr::map2(hex_pts$x, hex_pts$y, ~ sf::st_point(c(.x, .y))))

  all_pts <- real_pts %>%
    dplyr::mutate(type = "real", hex_id = "") %>%
    dplyr::select(district, type) %>%
    rbind(hex_pts %>%
            dplyr::mutate(type = "hex") %>%
            dplyr::select(district, type))

  ggplot2::ggplot(all_pts) +
    ggplot2::geom_sf(ggplot2::aes(fill = type, color = type)) +
    ggplot2::geom_sf_text(aes(
      label = district,
      # below for more in-depth debugging
      # label = ifelse(type == "real", district, paste0(hex_id, "-", district)),
      color = type),
      nudge_x = 0.03, nudge_y = 0.03) +
    ggplot2::scale_fill_discrete(aesthetics = c("colour", "fill"))
}

#' A simple Euclidean distance function
distance <- function(x_1, y_1, x_2, y_2) {
  sqrt(
    ((x_1 - x_2)^2) +
      ((y_1 - y_2)^2)
  )
}

#' A simple squared distance loss function
loss_function <- function(D, X, wts) {
  sum((D * X)^2 * wts)
}

