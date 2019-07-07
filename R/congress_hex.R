#' Hex states join
#'
#' Description
#'
#' @importFrom magrittr %>%
#' @export
hex_cd_join <- function(x, district_key, state_key = NULL, state_key_type = c("state_abb", "name", "fips")) {
  if (any(class(x) == "gg")) {
    x$data <- electoral.hex::congress_hex %>%
      dplyr::right_join(x$data,
                        by = c(setNames(nm = "district", district_key),
                          setNames(nm = state_key_type, state_key)))
    if (nrow(x$data) != 435) rlang::warn(paste0("Plotting ", nrow(x$data),  " observations."))
  } else {
    x <- electoral.hex::congress_hex %>%
      dplyr::right_join(x,
                        by = c(setNames(nm = "district", district_key),
                               setNames(nm = state_key_type, state_key)))
    if (nrow(x) != 435) rlang::warn(paste0("Returning ", nrow(x), " observations."))
  }
  invisible(x)
}
