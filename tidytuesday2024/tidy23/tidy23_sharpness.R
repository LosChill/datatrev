library(tidyverse)
library(here)
library(stringr)
library(ggrepel)
library(paletteer)

# Import ----
one_hot <- readRDS(here::here("tidytuesday2024", "tidy23", "one_hot.rds"))
cheese_flavor <- one_hot$cheese_flavor
cheese_aroma <- one_hot$cheese_aroma

sharp_library <- readRDS(here::here("tidytuesday2024", "tidy23", "sharp_library.rds"))
flavor_scale <- sharp_library$flavor_scale
aroma_scale <- sharp_library$aroma_scale

tuesdata <- readRDS(here::here("tidytuesday2024", "tidy23", "tuesdata23.rds"))
cheeses <- tuesdata$cheeses

# Theme ----
## Colors ----
text_color <- "beige"
fill_color <- "#1f2d36"
smooth_color <- "#002B36"

palette_aroma <- paletteer::paletteer_c("ggthemes::Classic Area-Brown", n = 39)
palette_flavor <- paletteer::paletteer_c("ggthemes::Classic Area-Brown", n = 45)
palette_country <- paletteer::paletteer_c("ggthemes::Classic Area-Brown", n = 51)

## Script theme ----
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
  plot.title = element_text(hjust = 1),
  plot.title.position = "plot",
  plot.subtitle = element_text(hjust = 1),
  plot.background = element_rect(fill = fill_color, color = NA),
  plot.margin = margin(rep(10, 4)),
  panel.background = element_rect(fill = fill_color, color = NA),
  legend.background = element_rect(fill = fill_color, color = NA),
  legend.key = element_rect(fill = fill_color, color = NA),
  strip.background = element_rect(fill = fill_color, color = NA),
  axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
  legend.text = element_blank(),
  legend.ticks = element_blank(),
  legend.location = "plot",
  legend.position = "top",
  legend.direction = "horizontal",
  legend.justification = "right",
  legend.margin = margin(0, 0, 0, 0)
)

# Flavor Average ----
flavor_avgs <- cheese_flavor
for (i in 1:45) {
  col_sym <- flavor_scale %>% 
    slice(i) %>% 
    pull(flavor) %>% 
    sym()
  
  flavor_avgs <- flavor_avgs %>% 
    mutate(!!col_sym := ifelse(!!col_sym == 1, i, !!col_sym))
  }
flavor_avgs <- flavor_avgs %>% 
  mutate(`NA` = 0)

## Calculate Flavor Average ----
flavor_avgs <- flavor_avgs %>% 
  rowwise() %>% 
  mutate(flav_average = mean(c_across(where(is.numeric))[c_across(where(is.numeric)) != 0])) %>%
  ungroup() %>% 
  select(cheese, flav_average)

# Aroma Average ----
aroma_avgs <- cheese_aroma
for (i in 1:39) {
  col_sym <- aroma_scale %>% 
    slice(i) %>% 
    pull(aroma) %>% 
    sym()
  
  aroma_avgs <- aroma_avgs %>% 
    mutate(!!col_sym := ifelse(!!col_sym == 1, i, !!col_sym))
}
aroma_avgs <- aroma_avgs %>% 
  mutate(`NA` = 0)

## Calculate Aroma Average ----
aroma_avgs <- aroma_avgs %>% 
  rowwise() %>% 
  mutate(aroma_average = mean(c_across(where(is.numeric))[c_across(where(is.numeric)) != 0])) %>%
  ungroup() %>% 
  select(cheese, aroma_average)

## Join flavor and aroma averages with cheeses
cheeses_flav_aroma <- left_join(cheeses, flavor_avgs, by = "cheese") %>% 
  left_join(aroma_avgs, by = "cheese") %>% 
  filter(!is.nan(flav_average)) %>% 
  filter(!is.nan(aroma_average)) %>% 
  filter(!is.na(aroma_average))

# Country
country_avgs <- cheeses_flav_aroma %>% 
  select(country, flav_average, aroma_average) %>% 
  separate_rows(country, sep = ", ") %>% 
  group_by(country) %>% 
  summarise(
    flav_average = mean(flav_average), 
    aroma_average = mean(aroma_average),
    country_count = n()) %>% 
  ungroup() %>% 
  drop_na()

# Scatter Plot
ggplot(data = country_avgs, aes(x = flav_average, y = aroma_average)) +
  geom_smooth(color = fill_color, fill = text_color, method = "lm") +
  geom_point(aes(color = country_count)) +
  geom_text_repel(aes(label = country, color = country_count), max.overlaps = 10000) +
  paletteer::scale_color_paletteer_c("ggthemes::Classic Area-Brown")
