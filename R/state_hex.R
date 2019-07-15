#' Hex states join
#'
#' Takes a dataframe or plot with state data and matches the data to a
#' simple features object with the package state hexagon data.
#'
#' @param x A dataframe like or ggplot object with state-level data
#' @param state_key characer. The name of the colum in `x` with the state
#'   identifier
#' @param state_key_type character. The type of state key. Either "state_abb",
#'   "name", or "fips".
#'
#' @return An object of the type class as `x` with attached simple features for
#'   state hexagons.
#'
#' @importFrom magrittr %>%
#' @export
hex_states_join <- function(x, state_key = NULL, state_key_type = c("state_abb", "name", "fips")) {
  join_key <- setNames(nm = state_key_type, state_key)
  if (any(class(x) == "gg")) {
    x$data <- electoral.hex::states_hex %>%
      dplyr::select(!!state_key_type) %>% # to eliminate unwanted columns
      dplyr::right_join(x$data, by = join_key) %>%
      dplyr::rename(!!state_key := !!state_key_type)

    if (nrow(x$data) != 50) rlang::warn(paste0("Plotting ", nrow(x$data), " observations."))
  } else {
    x <- electoral.hex::states_hex %>%
      dplyr::select(!!state_key_type) %>%
      dplyr::right_join(x, by = join_key) %>%
      dplyr::rename(!!state_key := !!state_key_type)
      # drop column from `states_hex`

    if (nrow(x) != 50) rlang::warn(paste0("Returning ", nrow(x), " observations."))
  }
  x
}
