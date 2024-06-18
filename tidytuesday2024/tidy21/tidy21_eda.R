library(tidyverse)
library(tidytuesdayR)
library(paletteer)
library(here)

tuesdata <- tt_load(2024, week = 21)
emissions <- tuesdata$emissions

saveRDS(emissions, here("tidytuesday2024", "tidy21", "emissions.rds"))

emissions <- readRDS(here("tidytuesday2024", "tidy21", "emissions.rds"))

## ggplot theme
text_color <- "beige"
fill_color <- "#1f2d36"

theme_set(theme_minimal(base_family = "Liberation Mono", base_size = 12))

theme_update(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  text = element_text(color = text_color),
  axis.text = element_text(color = text_color),
  strip.text = element_text(color = text_color),
  axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
  axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
  plot.title = element_text(hjust = 0),
  plot.title.position = "plot",
  plot.background = element_rect(fill = fill_color, color = NA),
  plot.margin = margin(rep(5, 4)),
  panel.background = element_rect(fill = fill_color, color = NA),
  legend.background = element_rect(fill = fill_color, color = NA),
  legend.key = element_rect(fill = fill_color, color = NA),
  strip.background = element_rect(fill = fill_color, color = NA)
  )

# How many of each commodity?
prods <- emissions %>% 
  group_by(commodity) %>% 
  summarize(count = n()) %>% 
  ungroup()

# How many of each production unit?
prod_units <- emissions %>% 
  group_by(production_unit) %>% 
  summarize(count = n()) %>% 
  ungroup()

# Is the relationship between each production_unit and total emissions linear?
emissions_prodgrp <- emissions %>%
  group_by(production_unit)

ggplot(emissions_prodgrp, aes(x = production_value, y = total_emissions_MtCO2e, color = production_unit)) +
  geom_point() +
  labs(title = "Emissions Rate by Production Unit",
       x = "Total Emissions",
       y = "Amount Produced",
       color = "Production Unit") +
  scale_colour_paletteer_d("LaCroixColoR::PeachPear")

# What is the linear relationship between each production_unit wrt/ total_emissions?
unit_slope <- emissions %>% 
  mutate(slope = total_emissions_MtCO2e/production_value) 

# What is the variance of this slope wrt/ each production_unit
ggplot(unit_slope %>% group_by(production_unit), aes(x = slope, color = production_unit)) +
  geom_freqpoly(binwidth = .01) +
  labs(title = "Variance in Emissions Rate",
       x = "Emissions Rate",
       y = "Frequency",
       color = "Production Unit") +
  scale_colour_paletteer_d("LaCroixColoR::PeachPear")

# Closer look at "Million tonnes/yr"
mtyr <- unit_slope %>% 
  filter(production_unit == "Million tonnes/yr")

ggplot(mtyr %>% group_by(commodity), aes(x = slope, fill = commodity)) +
  geom_histogram(color = "beige", binwidth = .1) +
  labs(title = "Coal Emission Rate Variance",
       x = "Coal Emissions Rate",
       y = "Frequency",
       fill = "Commodity") +
  scale_fill_paletteer_d("LaCroixColoR::Pamplemousse")

# Closer look at "Million Tonnes CO2"
mtco2 <- unit_slope %>% 
  filter(production_unit == "Million Tonnes CO2")

ggplot(mtco2 %>% group_by(commodity), aes(x = slope, fill = parent_entity)) +
  geom_histogram(color = "beige", binwidth = .01) +
  labs(title = "Cement Emissions Rate Variance",
       x = "Emissions Rate",
       y = "Frequency",
       fill = "Parent Entity") +
  scale_fill_paletteer_d("LaCroixColoR::Pamplemousse")

# Date correlation for cement slopes
ggplot(mtco2, aes(x = year, y = slope, color = parent_entity)) +
  geom_point() +
  facet_wrap(~ parent_entity) +
  labs(title = "Cement Emissions Rate Trend by Parent Entity",
       x = "Cement Emissions Rate",
       y = "Year") +
  scale_x_continuous(breaks = seq(1930, max(mtco2$year), by = 30)) +
  scale_color_paletteer_d("tvthemes::Opal") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    panel.border = element_rect(color = "beige", fill = NA, size = 1)
  )

