library(magrittr)
library(electoral.hex)

states_bridge <- tibble::tibble(state_abb = state.abb, name = toupper(state.name))
states_bridge$fips <- c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21,
                        22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
                        37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53,
                        54, 55, 56)

states_hex <- sf::st_read("C:/Users/Matt_2/Downloads/state_tiles.topo.json",
                          stringsAsFactors = FALSE)

states_hex <- states_hex %>%
  dplyr::select(fips = id, name) %>%
  dplyr::mutate(fips = as.numeric(fips)) %>%
  dplyr::left_join(states_bridge, by = c("fips", "name")) %>%
  dplyr::filter(name != "DISTRICT OF COLUMBIA") %>%
  dplyr::arrange(fips)

congress_hex_sf <- sf::st_read("C:/Users/Matt_2/Downloads/tiles.topo.json",
                                    stringsAsFactors = FALSE)
congress_hex_sf <- sf::st_cast(congress_hex_sf, "POLYGON")

tn_correction <- sf::st_read("C:/Users/Matt_2/Downloads/tiles.topo_tn.json",
                               stringsAsFactors = FALSE)
tn_correction <- sf::st_cast(tn_correction, "POLYGON") %>%
  dplyr::filter(id == 47)

congress_hex_sf <- congress_hex_sf %>%
  dplyr::filter(id != 47) %>%
  rbind(tn_correction)

congress_hex_sf <- congress_hex_sf %>%
  dplyr::select(fips = id, name) %>%
  dplyr::mutate(fips = as.numeric(fips)) %>%
  dplyr::left_join(states_bridge, by = c("fips", "name")) %>%
  dplyr::arrange(fips)

congress_hex <- match_districts_usa(congress_hex_sf, wts = "log_area")

ggplot(congress_hex) +
  geom_sf() +
  geom_sf_text(aes(label = district))


ny_centroids <- congress_hex_sf %>%
  dplyr::filter(fips == 36) %>%
  sf::st_centroid()

match_districts_state(state_fips = 36, hex_centroids = ny_centroids, wts = "log_area", compare = TRUE)
# download congressional district simple features from tigris (US Census)
real_cd_centroids_sf <- tigris::congressional_districts(class = "sf") %>%
  sf::st_centroid()


# filter to only data from 50 states
real_cd_centroids_sf <- real_cd_centroids_sf %>%
  dplyr::mutate(fips = as.numeric(STATEFP)) %>%
  dplyr::right_join(states_bridge, by = "fips") %>%
  dplyr::filter(NAMELSAD != "Congressional Districts not defined") %>%  # weird data handling
  dplyr::mutate(district = as.numeric(stringr::str_extract(NAMELSAD, "[[:digit:]]{1,}"))
                ,district = ifelse(is.na(district), 1, district)) %>%
  dplyr::select(fips, state_abb, district, area_land = ALAND)

usethis::use_data(states_hex, congress_hex, real_cd_centroids_sf, overwrite = TRUE)
usethis::use_data(congress_hex, overwrite = TRUE)
