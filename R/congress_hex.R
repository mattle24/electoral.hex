#' Hex states join
#'
#' Takes a dataframe or plot with Congressional data and matches the data to a
#' simple features object with the package Congressional hexagon data.
#'
#' @inheritParams hex_states_join
#' @param district_key character. The name of the column in `x` with the
#'   district number
#'
#' @return An object of the type class as `x` with attached simple features for
#'   Congressional hexagons.
#'
#' @include state_hex.R
#' @importFrom magrittr %>%
#' @export
hex_cd_join <- function(x, district_key, state_key = NULL, state_key_type = c("state_abb", "name", "fips")) {
  state_join_key <- setNames(nm = state_key_type, state_key)
  district_join_key <- setNames(nm = "district", district_key)
  if (any(class(x) == "gg")) {
    x$data <- electoral.hex::congress_hex %>%
      dplyr::select(!!state_key_type, district) %>% # to eliminate unwanted columns
      dplyr::right_join(x$data, by = c(state_join_key, district_join_key)) %>%
      dplyr::rename(!!state_key := !!state_key_type,
                    !!district_key := district)

    if (nrow(x$data) != 435) rlang::warn(paste0("Plotting ", nrow(x$data),  " observations."))
  } else {
    x <- electoral.hex::congress_hex %>%
      dplyr::select(!!state_key_type, district) %>% # to eliminate unwanted columns
      dplyr::right_join(x, by = c(state_join_key, district_join_key)) %>%
      dplyr::rename(!!state_key := !!state_key_type,
                    !!district_key := district)

    if (nrow(x) != 435) rlang::warn(paste0("Returning ", nrow(x), " observations."))
  }
  x
}
