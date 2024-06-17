# datatrev custom theme

library(paletteer)
library(ggplot2)

# Define variables

# Define the path to save the custom theme
save_path <- here("tidytuesday2024/tidy21/tidy21_theme.RData")

# Generate a palette
palette_name <- "BrBG"
num_colors <- 11
colors <- as.character(paletteer_c("grDevices::Earth", 11))

# Create a data frame to use for plotting
color_data <- data.frame(
  x = 1:num_colors,
  y = rep(1, num_colors),
  color = colors
)

# Plot the colors
ggplot(color_data, aes(x = factor(x), y = y, fill = factor(x))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = paste("Colors from Brewer Palette:", palette_name))

# Custom settings
axis_text_size <- 12
axis_text_color <- colors[5]
axis_line_color <- colors[5]
axis_ticks_color <- colors[5]
title_size <- 16
title_color <- colors[4]
title_hjust <- 0.5
subtitle_size <- 14
subtitle_color <- colors[3]
subtitle_hjust <- 0.5
plot_background_fill <- colors[11]
panel_background_fill <- colors[11]

# Define the minimal custom theme based on theme_void()
custom_theme <- theme_void() +
  theme(
    # Axis text settings
    axis.text = element_text(size = axis_text_size, color = axis_text_color),
    axis.text.x = element_text(size = axis_text_size, color = axis_text_color),
    axis.text.y = element_text(size = axis_text_size, color = axis_text_color),
    
    # Axis line settings
    axis.line = element_line(color = axis_line_color),
    axis.line.x = element_line(color = axis_line_color),
    axis.line.y = element_line(color = axis_line_color),
    
    # Axis ticks settings
    axis.ticks = element_line(color = axis_ticks_color),
    axis.ticks.x = element_line(color = axis_ticks_color),
    axis.ticks.y = element_line(color = axis_ticks_color),
    
    # Title settings
    plot.title = element_text(size = title_size, color = title_color, hjust = title_hjust),
    plot.subtitle = element_text(size = subtitle_size, color = subtitle_color, hjust = subtitle_hjust),
    
    # Background settings
    plot.background = element_rect(fill = plot_background_fill),
    panel.background = element_rect(fill = panel_background_fill)
  )

save(custom_theme, file = save_path)