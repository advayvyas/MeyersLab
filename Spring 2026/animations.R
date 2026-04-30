# render_animations.R
library(dplyr)
library(tidyverse)
library(sf)
library(gganimate)
library(gifski)

# Load data
message("Loading cache...")
d = readRDS("report_data_cache.rds")
list2env(d, envir = environment())

lower48 = setdiff(c(state.name, "District of Columbia"), c("Alaska", "Hawaii"))

us_map_weekly = us_map %>%
  filter(state %in% lower48) %>%
  select(-geometry_county) %>%
  left_join(
    weekly_map_data %>% rename(hsa_nci_id = ID),
    by = c("hsa_nci_id", "state")
  )

win_labels = c("1" = "2022/23", "2" = "2023/24", "3" = "2024/25")
n_weeks = n_distinct(weekly_map_data$week_index)
message(sprintf("Unique weeks: %d", n_weeks))

# Magnitude ratio
message("Building p_mag...")
p_mag = ggplot() +
  geom_sf(data = st_set_geometry(st_as_sf(us_map_weekly), "geometry_hsa"),
          aes(fill = weekly_mag_ratio), color = "gray20", linewidth = 0.1) +
  geom_sf(data = st_set_geometry(st_as_sf(us_map_weekly), "geometry"),
          fill = NA, color = "grey20", linewidth = 0.25) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 1,
    limits = c(0.5, 2), oob = scales::squish,
    na.value = "gray90", name = "HSA / State\nincidence"
  ) +
  facet_wrap(~peak_window, ncol = 3,
             labeller = labeller(peak_window = win_labels)) +
  coord_sf() + theme_void() +
  labs(title = "Week {closest_state}") +
  theme(
    strip.text   = element_text(size = 14, face = "bold", margin = margin(b = 6)),
    plot.title   = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10),
    legend.position = "right",
    plot.margin  = margin(5, 5, 5, 5)
  ) +
  transition_states(week_index, transition_length = 1, state_length = 2) +
  ease_aes("linear")

message("Rendering weekly_mag_ratio.gif ...")
animate(
  p_mag,
  nframes  = n_weeks,
  fps      = 2,
  width    = 1400, height = 500,
  renderer = gifski_renderer("weekly_mag_ratio.gif")
)
message("weekly_mag_ratio.gif done!")

# Cumulative lead/lag 
message("Building p_lag...")
p_lag = ggplot() +
  geom_sf(data = st_set_geometry(st_as_sf(us_map_weekly), "geometry_hsa"),
          aes(fill = lead_lag), color = "gray20", linewidth = 0.1) +
  geom_sf(data = st_set_geometry(st_as_sf(us_map_weekly), "geometry"),
          fill = NA, color = "grey20", linewidth = 0.25) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    limits = c(-0.5, 0.5), oob = scales::squish,
    na.value = "gray90", name = "Cumulative\nlead/lag"
  ) +
  facet_wrap(~peak_window, ncol = 3,
             labeller = labeller(peak_window = win_labels)) +
  coord_sf() + theme_void() +
  labs(title = "Week {closest_state}") +
  theme(
    strip.text   = element_text(size = 14, face = "bold", margin = margin(b = 6)),
    plot.title   = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10),
    legend.position = "right",
    plot.margin  = margin(5, 5, 5, 5)
  ) +
  transition_states(week_index, transition_length = 1, state_length = 2) +
  ease_aes("linear")

message("Rendering weekly_lead_lag.gif ...")
animate(
  p_lag,
  nframes  = n_weeks,
  fps      = 2,
  width    = 1400, height = 500,
  renderer = gifski_renderer("weekly_lead_lag.gif")
)
message("weekly_lead_lag.gif done!")

message("All done!")