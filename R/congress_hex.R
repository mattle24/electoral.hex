#' Hex states join
#'
#' Takes a dataframe or plot with Congressional data and matches the data to a
#' simple features object with the package Congressional hexagon data.
#'
#' @inheritParams hex_states_join
#' @param district_key character. The name of the column in `x` with the
#'   district number
#'
#'
#' @return An object of the type class as `x` with attached simple features for
#'   Congressional hexagons.
#'
#' @include state_hex.R
#' @importFrom magrittr %>%
#' @export
hex_cd_join <- function(x, district_key, state_key = NULL, state_key_type = c("state_abb", "name", "fips")) {
  if (any(class(x) == "gg")) {
    x$data <- congress_hex %>%
      dplyr::right_join(x$data,
                        by = c(setNames(nm = "district", district_key),
                          setNames(nm = state_key_type, state_key)))
    if (nrow(x$data) != 435) rlang::warn(paste0("Plotting ", nrow(x$data),  " observations."))
  } else {
    x <- congress_hex %>%
      dplyr::right_join(x,
                        by = c(setNames(nm = "district", district_key),
                               setNames(nm = state_key_type, state_key)))
    if (nrow(x) != 435) rlang::warn(paste0("Returning ", nrow(x), " observations."))
  }
  x
}
