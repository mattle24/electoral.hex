library(magrittr)
library(ggplot2)
library(electoral.hex)

theme_set(theme_minimal())

dw_senate <- politicaldata::get_senate_nominate(116)
dw_senate <- dw_senate %>%
  dplyr::group_by(state_abbrev) %>%
  dplyr::top_n(1, born) %>%
  dplyr::ungroup()


plot_hex_states <- hex_states_join(dw_senate, key = "state_abbrev", key_type = "state_abb")

ggplot(plot_hex_states) +
  geom_sf(aes(fill = nominate_dim1)) +
  coord_sf(datum = NA) +
  geom_sf_text(aes(label = state_abb)) +
  scale_fill_gradient2(low = "darkblue", high = "darkred", name = "DW-Nominate")


ggplot(data = dw_senate) %>%
  hex_states_join(key = "state_abbrev", key_type = "state_abb") +
  geom_sf(aes(fill = nominate_dim1)) +
  coord_sf(datum = NA) +
  geom_sf_text(aes(label = state_abb)) +
  scale_fill_gradient2(low = "darkblue", high = "darkred", name = "DW-Nominate")

dw_house <- politicaldata::get_house_nominate(116)

ggplot(data = dw_house) %>%
  hex_cd_join(district_key = "district_code", state_key = "state_abbrev", state_key_type = "state_abb") +
  geom_sf(aes(fill = nominate_dim1)) +
  coord_sf(datum = NA) +
  geom_sf_text(aes(label = ifelse(district == 1, state_abb, ""))) +
  scale_fill_gradient2(low = "darkblue", high = "darkred", name = "DW-Nominate")

ggplot(data = dw_house %>% dplyr::filter(state_abbrev == "NY")) %>%
  hex_cd_join(district_key = "district_code", state_key = "state_abbrev", state_key_type = "state_abb") +
  geom_sf(aes(fill = nominate_dim1)) +
  coord_sf(datum = NA) +
  geom_sf_text(aes(label = district)) +
  scale_fill_gradient2(low = "darkblue", high = "darkred", name = "DW-Nominate")

