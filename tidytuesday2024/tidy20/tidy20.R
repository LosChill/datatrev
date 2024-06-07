library(tidytuesdayR)
library(tidyverse)
library(RColorBrewer)
library(ggforce)
library(knitr)
library(kableExtra)

# Set Theme
brew_colors <- brewer.pal(n = 6, name = "YlOrBr")

coffee_theme <- theme_minimal() +
  theme(
    plot.title = element_text(family = "Hot Ink", face = "plain", size = 26, color = brew_colors[1], hjust = 0.5),
    axis.title.x = element_text(family = "Fira Sans", face = "italic", size = 12, color = brew_colors[1], margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")),
    axis.title.y = element_text(family = "Fira Sans", face = "italic", size = 12, color = brew_colors[1], margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")),
    axis.text.x = element_text(family = "Fira Sans", face = "plain", size = 10, color = brew_colors[2], angle = 45, hjust = 1),
    axis.text.y = element_text(family = "Fira Sans", face = "plain", size = 10, color = brew_colors[2]),
    legend.title = element_text(family = "Fira Sans", face = "bold", size = 12, color = brew_colors[1]),
    legend.text = element_text(family = "Fira Sans", face = "plain", size = 10, color = brew_colors[2]),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 35, r = 10)
  )

theme_set(coffee_theme)

# Import data
coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

# Trim out free-text data, fix "spend" typo ("spend" is not a noun, stop trying to make it one)
clean_tib <- coffee_survey %>%
  select(-matches("other|specify|coffee_a|coffee_b|coffee_c|coffee_d|prefer")) %>%
  rename(monthly_expenditure = total_spend) %>%
  mutate(across(everything(), ~ if(is.character(.)) tolower(.) else .)) %>%
  mutate(across(c(age, cups, favorite, style, strength, roast_level, caffeine,
                  expertise, wfh, monthly_expenditure, taste, know_source,
                  most_paid, most_willing, value_cafe, spent_equipment,
                  value_equipment, gender, education_level, ethnicity_race,
                  employment_status, number_children, political_affiliation
                  ), as.factor
                )
         )

# List of columns with multi-value cells
hot_columns <- c("where_drink",
                 "brew",
                 "purchase",
                 "additions",
                 "dairy",
                 "sweetener",
                 "why_drink"
)

# Drop multi-value columns before dropping NA cells
strip_tib <- clean_tib %>%
  select(-all_of(hot_columns)) %>%
  drop_na()

# Age vs Cups
age_levels = c("<18 years old", "18-24 years old", "25-34 years old",
               "35-44 years old", "45-54 years old", "55-64 years old",
               ">65 years old")
cups_levels = c("less than 1", "1", "2", "3", "4", "more than 4")

ageVcups <- strip_tib %>% 
  select(age, cups) %>% 
  mutate(
    age = factor(age, levels = age_levels, ordered = TRUE),
    cups = factor(cups, levels = cups_levels, ordered = TRUE)
  )

ggplot(ageVcups, aes(x = age, fill = cups)) +
  geom_bar(position = "fill") +
  labs(
    title = "Cups of Coffee per Day by Age Group",
    x = "Age Group",
    y = "Percentage",
    fill = "Number of Cups"  # Custom legend title
  ) +
  scale_fill_brewer(palette = "YlOrBr") +
  scale_y_continuous(labels = ~ paste0(. * 100, "%")) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Expertise vs Know Source
expertiseVknowsource_tib <- strip_tib %>%
  select(expertise, know_source) %>% 
    mutate(know_source = case_when(
      tolower(know_source) == "yes" ~ "Yes",
      tolower(know_source) == "no" ~ "No",
      TRUE ~ know_source))

evk_lite <- expertiseVknowsource_tib %>% 
  filter(!(know_source == "No" & expertise == 10))

evk_hilite <- expertiseVknowsource_tib %>% 
  filter(know_source == "No" & expertise == 10)

p <- ggplot(evk_lite, aes(
    x = know_source, 
    y = as.numeric(expertise))
  ) +
  geom_boxplot(
    data = expertiseVknowsource_tib,
    outlier.shape = NA, 
    fill = "brown", 
    color = "white", 
    alpha = 0.25
  ) +
  geom_point(
    data = evk_lite,
    position = position_jitter(seed = 119, width = 0.12, height = 0.69),
    color = "tan",
    alpha = 0.25
  ) +
  geom_point(
    data = evk_hilite,
    position = position_jitter(seed = 119, width = 0.1, height = 0.69),
    color = "tan",
    alpha = 0.25
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, by = 1)
  ) +
  labs(
    title = "Do Coffee Experts Know Their Sources?",
    x = "Do You Know Where Your Coffee Comes From?",
    y = "Claimed Level of Coffee Expertise"
  ) +
  theme(
    legend.position = "none"
  )

hilite <- expertiseVknowsource_tib %>% 
  filter(know_source == "No" & expertise == 10)

p_hilite <- p +
  geom_point(data = hilite, aes(
    x = know_source, 
    y = as.numeric(expertise)),
    position = position_jitter(seed = 119, width = 0.1, height = 0.69), 
    color = "brown",
    alpha = .88
    ) +
  annotate(
    "text",
    x = .75,
    y = 10,
    label = "EXPERTS",
    size = 2.5,
    #fontface = "italic",
    color = "brown"
  )

p_hilite

expVsource_outliers <- strip_tib %>%
  mutate(expertise = as.integer(expertise)) %>%
  select(-submission_id) %>% 
  filter(expertise == 10) %>%
  filter(str_detect(know_source, "no"))

# Print Table
kable(expVsource_outliers, "html") %>%
  kable_styling() %>%
  row_spec(0, bold = TRUE, color = brew_colors[1], background = "brown") %>%
  row_spec(1:nrow(expVsource_outliers), color = brew_colors[6], background = brew_colors[1])