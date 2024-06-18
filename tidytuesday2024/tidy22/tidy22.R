library(tidyverse)
library(tidytuesdayR)
library(here)
library(paletteer)

# Download
tuesdata <- tidytuesdayR::tt_load(2024, week = 22)

# Save
saveRDS(tuesdata, here("tidytuesday2024", "tidy22", "tuesdata22.rds"))

# Import
tuesdata <- readRDS(here("tidytuesday2024", "tidy22", "tuesdata22.rds"))

# Account
harvest_2020 <- tuesdata$harvest_2020
harvest_2021 <- tuesdata$harvest_2021
planting_2020 <- tuesdata$planting_2020
planting_2021 <- tuesdata$planting_2021
spending_2020 <- tuesdata$spending_2020
spending_2021 <- tuesdata$spending_2021

# Theme
# ggplot theme
text_color <- "beige"
fill_color <- "#1f2d36"

theme_set(theme_minimal(
  base_family = "Liberation Mono", 
  base_size = 12))

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

# Yield 2020
h20_weight <- harvest_2020 %>% 
  group_by(vegetable) %>% 
  summarize(total_weight = sum(weight)) %>% 
  ungroup() %>% 
  filter(vegetable != "strawberries")

p20_seeds <- planting_2020 %>% 
  group_by(vegetable) %>% 
  summarize(total_seeds = sum(number_seeds_planted)) %>% 
  ungroup() %>% 
  filter(vegetable != "strawberries")

yield20 <- p20_seeds %>% 
  inner_join(h20_weight, by = "vegetable") %>% 
  mutate(yield = total_weight/total_seeds)

# Yield 2021
h21_weight <- harvest_2021 %>% 
  group_by(vegetable) %>% 
  summarize(total_weight = sum(weight)) %>% 
  ungroup() %>% 
  filter(vegetable != "strawberries")

p21_seeds <- planting_2021 %>% 
  group_by(vegetable) %>% 
  summarize(total_seeds = sum(number_seeds_planted)) %>% 
  ungroup() %>% 
  filter(vegetable != "strawberries")

yield21 <- p21_seeds %>% 
  inner_join(h20_weight, by = "vegetable") %>% 
  mutate(yield = total_weight/total_seeds)

# Yield join
yields <- yield20 %>% 
  inner_join(yield21, by = "vegetable")

