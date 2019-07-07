#' Hex states join
#'
#' Description
#'
#'
#' @importFrom magrittr %>%
#' @export
hex_states_join <- function(x, key = NULL, key_type = c("state_abb", "name", "fips")) {
  if (any(class(x) == "gg")) {
    x$data <- electoral.hex::states_hex %>%
      dplyr::right_join(x$data, by = setNames(nm = key_type, key))
    if (nrow(x$data) != 50) rlang::warn(paste0("Plotting ", nrow(x$data), " observations."))
  } else {
    x <- electoral.hex::states_hex %>%
      dplyr::right_join(x, by = setNames(nm = key_type, key))
    if (nrow(x) != 50) rlang::warn(paste0("Returning ", nrow(x), " observations."))
  }
  invisible(x)
}
