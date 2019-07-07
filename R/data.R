#' Hexagon for all 50 US states
#'
#' A dataset containing geometries for plotting US states as hexagons.
#'
#' Fields:
#'
#' \itemize{
#'   \item `fips`: numeric. State FIPS code.
#'   \item `name`: character. State name in upper case.
#'   \item `state_abb`: charatcer. State abbreviation in upper case.
#'   \item `geometry`: POLYGON. Geometries for hexagons.
#' }
#'
#' @format A simple features object with 50 rows and 3 fields
#' @source https://pitchinteractiveinc.github.io/tilegrams/
"states_hex"


#' Hexagon for US Congressional districts
#'
#' A dataset containing geometries for plotting US Congressional districts as
#' hexagons, broken out by state.
#'
#' Fields:
#'
#' \itemize{
#'   \item `fips`: numeric. State FIPS code.
#'   \item `name`: character. State name in upper case.
#'   \item `state_abb`: charatcer. State abbreviation in upper case.
#'   \item `district`: numeric. District number. `1` for states with only one at-large
#' representative.
#'   \item `geometry`: POLYGON. Geometries for hexagons.
#' }
#' @format A simple features object with 435 rows and 4 fields
#' @source https://pitchinteractiveinc.github.io/tilegrams/
"congress_hex"


