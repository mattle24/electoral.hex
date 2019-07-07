#' Match districts
#'
#' Description
#'
#' @importFrom magrittr %>%
#' @importFrom rlang %||%
#' @importFrom purrr map2
#' @export
match_districts_state <- function(real_centroids, hex_centroids,
                                  iter = 10, wts = NULL, compare = FALSE) {
  # standardize centroid points
  hex_centroids <- hex_centroids %>%
    standard_hex_points()

  real_centroids <- real_centroids %>%
    standard_state_points() %>%
    dplyr::arrange(district)

  n_districts <- as.integer(nrow(real_centroids))
  stopifnot(n_districts == nrow(hex_centroids))

  data <- map2(hex_centroids$x, hex_centroids$y,
                      ~ distance(.x, .y, real_centroids$x, real_centroids$y))
  # D is the matrix that has distances from each centroid to each district centroid.
  # Hexes are rows, districts are columns.
  # Eg, D[5, 9] refers to the distance between the fifth he
  D <- matrix(data = unlist(data), nrow = n_districts, ncol = n_districts, byrow = TRUE)

  # initialize assignments by using minimizing distance greedily
  min_dist <- min(D[1, ]) # minimum distance between hex and all districts
  index <- match(min_dist, D[1, ]) # column index for the district w/ minimum distance
  used_indicies <- c(index) # keep track of what districts have been assigned
  for (i in 2L:n_districts) {
    min_dist <- min(D[i, -used_indicies]) # minimum distance between hex and unassigned districts
    index <- match(min_dist, D[i, ]) # column index for the district w/ minimum distance
    used_indicies <- c(used_indicies, index)
  }

  # X is the matrix that shoes assignment of hexes to districts
  # Hexes are rows, districts are columns.
  # Eg, X[3, 6] = 1 means that the third is assigned to District 6
  # Each row and each column should have one and only one `1`
  X <- matrix(0, nrow = n_districts, ncol = n_districts)
  # random_assignment <- sample(n_districts, n_districts)
  for (r in 1L:n_districts) X[r, used_indicies[r]] <- 1
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
#' Description
#'
#' @importFrom magrittr %>%
#' @importFrom rlang abort
#' @export
match_districts_usa <- function(congress_hex_sf,
                                fips_key = "fips",
                                iter = 10,
                                wts = NULL) {
  congress_hex_sf <- congress_hex_sf %>%
    dplyr::mutate(.res_matching_id = dplyr::row_number())

  hex_centroids_sf <- congress_hex_sf %>%
    sf::st_centroid()

  res_list <- list() # list to hold state results
  fips <- hex_centroids_sf[ , fips_key]  # get vector of fips to iterate through
  sf::st_geometry(fips) <- NULL
  fips <- unlist(fips)
  # iterate through unique fips. find best matches of hexes to districts and
  # append to the results list
  for (f in sort(unique(fips))) {
    # filter to state
    std_hex_centroids_sf <- hex_centroids_sf %>%
      dplyr::filter(!!as.name(fips_key) == f)

    std_real_centroids_sf <- electoral.hex::real_cd_centroids_sf %>%
      dplyr::filter(fips == f)

    if (nrow(std_real_centroids_sf) != nrow(std_hex_centroids_sf)) {
      abort(paste0("Real and hex have different number of rows. FIPS ", f))
    }

    if (nrow(std_hex_centroids_sf) == 1) {
      std_hex_centroids_sf$district <- 1
    } else {
      if (!is.null(wts) && wts == "log_area") {
        wts <- log(std_real_centroids_sf$area_land)
      } else {
        wts <- NULL
      }
      std_hex_centroids_sf$district <- match_districts_state(real_centroids = std_real_centroids_sf,
                                        hex_centroids = std_hex_centroids_sf,
                                        iter = iter,
                                        wts = wts)
    }
    std_hex_centroids_sf$geometry <- NULL
    res_list[[length(res_list) + 1]] <- std_hex_centroids_sf %>%
      dplyr::select(district, .res_matching_id)
  }

  res_df <- purrr::reduce(res_list, rbind) %>%
    dplyr::select(district, .res_matching_id)

  congress_hex_sf %>%
    dplyr::left_join(res_df, by = ".res_matching_id") %>%
    dplyr::select(-.res_matching_id)
}

#' Compare plot
#'
#' Description
#' @importFrom magrittr %>%
#' @import ggplot2
compare_plot <- function(real_pts, hex_pts) {
  sf::st_geometry(real_pts) <- sf::st_sfc(purrr::map2(real_pts$x, real_pts$y, ~ sf::st_point(c(.x, .y))))

  sf::st_geometry(hex_pts) <- sf::st_sfc(purrr::map2(hex_pts$x, hex_pts$y, ~ sf::st_point(c(.x, .y))))

  all_pts <- real_pts %>%
    dplyr::mutate(type = "real"
                  ,hex_id = "") %>%
    dplyr::select(district, type, hex_id) %>%
    rbind(hex_pts %>%
            dplyr::mutate(type = "hex") %>%
            dplyr::select(district, type, hex_id))

  ggplot(all_pts) +
    geom_sf(aes(fill = type, color = type)) +
    geom_sf_text(aes(
      label = district,
      # below for more in-depth debugging
      # label = ifelse(type == "real", district, paste0(hex_id, "-", district)),
      color = type),
      nudge_x = 0.03, nudge_y = 0.03) +
    scale_fill_discrete(aesthetics = c("colour", "fill"))
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

