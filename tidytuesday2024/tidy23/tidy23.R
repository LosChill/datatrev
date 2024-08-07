library(tidytuesdayR)
library(tidyverse)
library(here)
library(paletteer)
library(ggwordcloud)
library(stringr)

# Import ----
# tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
# saveRDS(tuesdata, here("tidytuesday2024", "tidy23", "tuesdata23.rds"))

tuesdata <- readRDS(here::here("tidytuesday2024", "tidy23", "tuesdata23.rds"))
cheeses <- tuesdata$cheeses

# Theme ----
## Colors ----
text_color <- "beige"
fill_color <- "#1f2d36"

palette_aroma <- paletteer::paletteer_c("ggthemes::Classic Area-Brown", n = 39)
palette_flavor <- paletteer::paletteer_c("ggthemes::Classic Area-Brown", n = 45)


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
  plot.caption.position = "plot",
  plot.subtitle = element_text(hjust = 1),
  plot.caption = element_text(size = 7.5, color = text_color, hjust = 1),
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

# EDA ----

## Distinct flavors and aromas ----
unique_flavors <- cheeses %>% 
  distinct(flavor) %>% 
  separate_rows(flavor, sep = ", ") %>% 
  distinct(flavor)

unique_aromas <- cheeses %>% 
  distinct(aroma) %>% 
  separate_rows(aroma, sep = ", ") %>% 
  distinct(aroma)

## Libraries for flavor and aroma scales ----

### Flavor scale ----
flavor_scale <- tibble(
  rank = 1:45,
  flavor = c(
    "mild",
    "smooth",
    "subtle",
    "milky",
    "creamy",
    "buttery",
    "sweet",
    "fruity",
    "nutty",
    "herbaceous",
    "grassy",
    "floral",
    "vegetal",
    "savory",
    "mellow",
    "rustic",
    "caramel",
    "butterscotch",
    "full-flavored",
    "pronounced",
    "earthy",
    "woody",
    "mushroomy",
    "salty",
    "burnt caramel",
    "licorice",
    "smokey",
    "meaty",
    "umami",
    "oceanic",
    "crunchy",
    "tangy",
    "citrusy",
    "lemony",
    "tart",
    "sour",
    "acidic",
    "bitter",
    "sharp",
    "spicy",
    "piquant",
    "garlicky",
    "strong",
    "yeasty",
    "pungent"
  )
)

### Aroma scale ----
aroma_scale <- tibble(
  rank = 1:39,
  aroma = c(
    "mild", 
    "fresh", 
    "subtle", 
    "pleasant", 
    "clean", 
    "buttery", 
    "milky", 
    "lactic", 
    "nutty", 
    "raw nut", 
    "pecan", 
    "rich", 
    "toasty", 
    "sweet", 
    "caramel", 
    "floral", 
    "fruity", 
    "perfumed", 
    "herbal", 
    "grassy", 
    "earthy", 
    "woody", 
    "mushroom", 
    "spicy", 
    "smokey", 
    "yeasty", 
    "aromatic", 
    "lanoline", 
    "whiskey", 
    "pronounced", 
    "strong", 
    "fermented", 
    "ripe", 
    "musty", 
    "garlicky", 
    "goaty", 
    "barnyardy", 
    "stinky", 
    "pungent"
  )
)

### Export ----
sharp_library <- list(
  flavor_scale = flavor_scale,
  aroma_scale = aroma_scale
  )

saveRDS(sharp_library, file = here("tidytuesday2024", "tidy23", "sharp_library.rds"))

# One-hot ----
cheese_flavor <- cheeses %>%
  select(cheese, flavor) %>% 
  separate_rows(flavor, sep = ", ") %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = flavor, values_from = value, values_fill = list(value = 0))

cheese_aroma <- cheeses %>% 
  select(cheese, aroma) %>% 
  separate_rows(aroma, sep = ", ") %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = aroma, values_from = value, values_fill = list(value = 0))

## Export
one_hot <- list(
  cheese_flavor = cheese_flavor,
  cheese_aroma = cheese_aroma
  )

saveRDS(one_hot, file = here::here("tidytuesday2024", "tidy23", "one_hot.rds"))

# Summary ----
cheese_flavor_sum <- cheese_flavor %>% 
  select_if(is.numeric) %>% 
  summarise_all(sum) %>% 
  pivot_longer(cols = everything(), names_to = "flavor", values_to = "count") %>% 
  filter(flavor != "NA") %>% 
  mutate(flavor = str_trim(flavor)) %>%
  group_by(flavor) %>%
  summarize(count = sum(count), .groups = 'drop')

cheese_aroma_sum <- cheese_aroma %>% 
  select_if(is.numeric) %>% 
  summarise_all(sum) %>% 
  pivot_longer(cols = everything(), names_to = "aroma", values_to = "count") %>% 
  filter(aroma != "NA") %>% 
  mutate(aroma = str_trim(aroma)) %>%
  group_by(aroma) %>%
  summarize(count = sum(count), .groups = 'drop')

# Plots ----
set.seed(35)

## Flavor Plots ----
flavor_plot <- cheese_flavor_sum %>% 
  left_join(flavor_scale, by = "flavor") %>% 
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

### Flavor Column ----
flavor_column <- ggplot(flavor_plot, aes(
    x = reorder(flavor, rank), 
    y = count, 
    fill = rank)) +
  geom_col() +
  scale_fill_gradientn(colors = palette_flavor) +
  scale_y_continuous(limits = c(0, 300)) +
  labs(
    x = "Described Flavor", 
    y = "Number of Varieties", 
    fill = "Sharpness",
    title = "Cheese Flavor Frequency",
    subtitle = "Across 1000+ Varieties",
    caption = "@datatrev // Trevor Pendras // www.pendras.com"
    )

## Flavor Cloud ----
flavor_cloud <- ggplot(flavor_plot, 
  aes(
    label = flavor,
    size = count,
    color = rank,
    angle =angle)
    ) +
  ggwordcloud::geom_text_wordcloud_area(eccentricity = 1) +
  scale_size_area(max_size = 60) +
  labs(
    title = "Cheese Flavor Frequency",
    subtitle = "Across 1000+ Varieties",
    caption = "@datatrev // Trevor Pendras // www.pendras.com"
  ) +
  scale_color_gradientn(colors = palette_flavor) +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0)
    )

## Aroma Plots ----
aroma_plot <- cheese_aroma_sum %>% 
  left_join(aroma_scale, by = "aroma") %>% 
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

### Aroma Column ----
aroma_column <- ggplot(aroma_plot, aes(
    x = reorder(aroma, rank),
    y = count,
    fill = rank)
    ) +
  geom_col() +
  scale_y_continuous(limits = c(0, 200)) +
  scale_fill_gradientn(colors = palette_aroma) +
  labs(
    x = "Described Aroma", 
    y = "Number of Varieties", 
    fill = "Pungency",
    title = "Cheese Aroma Frequency",
    subtitle = "Across 1000+ Varieties",
    caption = "@datatrev // Trevor Pendras // www.pendras.com"
    )

### Aroma Cloud ----
aroma_cloud <- ggplot(aroma_plot, 
  aes(
    label = aroma,
    size = count,
    color = rank,
    angle =angle)
    ) +
  ggwordcloud::geom_text_wordcloud_area(eccentricity = 1) +
  scale_size_area(max_size = 60) +
  labs(
    title = "Cheese Aroma Frequency",
    subtitle = "Across 1000+ Varieties",
    caption = "@datatrev // Trevor Pendras // www.pendras.com"
    ) +
  scale_color_gradientn(colors = palette_aroma) +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0)
    )

flavor_column
flavor_cloud
aroma_column
aroma_cloud
