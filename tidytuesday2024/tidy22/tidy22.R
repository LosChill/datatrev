library(tidyverse)
library(tidytuesdayR)
library(here)
library(scales)
library(paletteer)

# Load ----
## Download ----
# tuesdata <- tidytuesdayR::tt_load(2024, week = 22)

## Save ----
# saveRDS(tuesdata, here("tidytuesday2024", "tidy22", "tuesdata22.rds"))

## Import ----
tuesdata <- readRDS(here::here("tidytuesday2024", "tidy22", "tuesdata22.rds"))

## Account ----
harvest_2020 <- tuesdata$harvest_2020
harvest_2021 <- tuesdata$harvest_2021
planting_2020 <- tuesdata$planting_2020
planting_2021 <- tuesdata$planting_2021
spending_2020 <- tuesdata$spending_2020
spending_2021 <- tuesdata$spending_2021

# Theme ----
## Colors ----
text_color <- "beige"
fill_color <- "#1f2d36"

brown <- "#896C4CFF"
palette <- paletteer::paletteer_d("ggsci::default_jama")
palette_green18 <- paletteer::paletteer_c("ggthemes::Temperature Diverging", n = 18)
palette_green25 <- paletteer::paletteer_c("ggthemes::Temperature Diverging", n = 25)

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
  plot.title = element_text(hjust = 0),
  plot.title.position = "plot",
  plot.background = element_rect(fill = fill_color, color = NA),
  plot.margin = margin(rep(10, 4)),
  panel.background = element_rect(fill = fill_color, color = NA),
  legend.background = element_rect(fill = fill_color, color = NA),
  legend.key = element_rect(fill = fill_color, color = NA),
  strip.background = element_rect(fill = fill_color, color = NA)
)

# EDA ----
## Yield 2020 ----
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
  mutate(yield = total_weight/total_seeds) %>% 
  mutate(vegetable = tolower(vegetable))

### Yield 2020 plot ----
ggplot(data = yield20) +
  geom_bar(
    aes(
      x = reorder(vegetable, -total_seeds), 
      y = total_weight, 
      fill = vegetable
      ),
    stat = "identity"
    ) +
  geom_bar(
    aes(x = vegetable, y = -total_seeds * 100),
    stat = "identity",
    fill = brown
    ) +
  scale_y_continuous(
    limits = c(-60000, 180000)
    ) +
  geom_text(
    aes(
      x = reorder(vegetable, -total_seeds), 
      y = total_weight, 
      label = round(total_weight / 1000, 1),
      color = vegetable
      ),
    vjust = .5,
    hjust = -.25,
    angle = 90
    ) +
  geom_text(
    aes(
      x = reorder(vegetable, -total_seeds), 
      y = -total_seeds * 100, 
      label = round(total_seeds, 1)
      ),
    color = brown,
    vjust = .5,
    hjust = 1.5,
    angle = 90
    ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Yield from Seed Comparison",
    subtitle = "2020"
    ) +
  annotate(
    "text", 
    x = -Inf, 
    y = 0, 
    label = "← Total Seeds Planted | Total Weight Yield (kg) →",
    color = "beige",
    angle = 90,
    hjust = .47,
    vjust = -1
    ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(10, 10, 10, 30),
    legend.position="none"
  ) +
  scale_fill_manual(values = palette_green25) +
  scale_color_manual(values = palette_green25)

## Yield 2021 ----
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
  inner_join(h21_weight, by = "vegetable") %>% 
  mutate(yield = total_weight/total_seeds)

### Yield 2021 plot ----
ggplot(data = yield21) +
  geom_bar(
    aes(
      x = reorder(vegetable, -total_seeds), 
      y = total_weight, 
      fill = vegetable
    ),
    stat = "identity"
  ) +
  geom_bar(
    aes(x = vegetable, y = -total_seeds * 100),
    stat = "identity",
    fill = brown
  ) +
  scale_y_continuous(
    limits = c(-60000, 180000)
  ) +
  geom_text(
    aes(
      x = reorder(vegetable, -total_seeds), 
      y = total_weight, 
      label = round(total_weight / 1000, 1),
      color = vegetable
    ),
    vjust = .5,
    hjust = -.25,
    angle = 90
  ) +
  geom_text(
    aes(
      x = reorder(vegetable, -total_seeds), 
      y = -total_seeds * 100, 
      label = round(total_seeds, 1)
    ),
    color = brown,
    vjust = .5,
    hjust = 1.5,
    angle = 90
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Yield from Seed Comparison",
    subtitle = "2021"
  ) +
  annotate(
    "text", 
    x = -Inf, 
    y = 0, 
    label = "← Total Seeds Planted | Total Weight Yield (kg) →",
    color = "beige",
    angle = 90,
    hjust = .47,
    vjust = -1
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(10, 10, 10, 30),
    legend.position="none"
  ) +
  scale_fill_manual(values = palette_green25) +
  scale_color_manual(values = palette_green25)

## Yields Change ----
yields <- inner_join(
  yield20, 
  yield21, 
  by = "vegetable", 
  suffix = c(".20", ".21")
  ) %>% 
  group_by(vegetable) %>% 
  ungroup() %>% 
  mutate(
    change_yield = (yield.21 - yield.20) / yield.20
  )

### Yield Change ----
ggplot(
  data = yields,
  aes(
    x = reorder(vegetable, -change_yield), 
    y = change_yield,
    fill = vegetable,
    )
  ) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(-1, 5, by = 2)
    ) +
  labs(
    title = "Difference in Yield",
    subtitle = "2020 v 2021",
    y = "Percent Change"
    ) +
  theme(
    panel.grid.major.y = element_line(color = text_color, linewidth = 0.05),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 90, 
      hjust = 1, 
      vjust = .5),
    axis.title.x = element_blank()
    ) +
  scale_fill_manual(values = palette_green18)
