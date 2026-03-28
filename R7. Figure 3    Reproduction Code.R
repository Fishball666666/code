library(tidyverse)
library(ggrepel)
library(scales)
library(patchwork)
library(ggsci) # Professional journal color palette package, required
library(viridis)

# Read region ordering data
order <- read.csv("地区排序.csv", header = F)
region_order <- order$V1
unique(Risk$measure)

# Data processing: get top 3 risk factors for each region in 2021 (using Deaths)
top3_data <- Risk %>%
  filter(age == "Age-standardized",
         metric == "Rate",
         measure == "Deaths",  # Use Deaths
         sex == "Both",
         cause == "All causes",
         year == 2021) %>%
  group_by(location) %>%
  arrange(desc(val)) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  mutate(location = factor(location, levels = region_order))

# Get historical data for all top 3 risk factors (using Deaths)
all_top3_rei <- unique(top3_data$rei)
historical_data <- Risk %>%
  filter(age == "Age-standardized",
         metric == "Rate",
         measure == "Deaths",  # Use Deaths
         sex == "Both",
         cause == "All causes",
         rei %in% all_top3_rei) %>%
  mutate(location = factor(location, levels = region_order))


n_colors <- length(all_top3_rei)
base_colors <- c(pal_nejm()(8), pal_aaas()(4), viridis(6, option = "D", end = 0.9))
rei_colors <- setNames(base_colors[1:n_colors], all_top3_rei)

clean_theme <- function() {
  theme_minimal(base_size = 7) +
    theme(
      plot.title = element_text(size = 8, face = "bold", hjust = 0.5, margin = margin(b = 1), color = "black"),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 6, color = "black", margin = margin(t = 2)),
      axis.text.y = element_text(size = 6, color = "black", margin = margin(r = 2)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(3, 3, 3, 3),
      legend.position = "none",
      axis.line = element_line(color = "black", size = 0.3),
      axis.ticks = element_line(color = "black", size = 0.3),
      axis.ticks.length = unit(1.5, "pt") # Shorter tick marks for more refined appearance
    )
}

create_region_plot <- function(region) {
  # Get top 3 risk factors for this region
  top3_rei <- top3_data %>% 
    filter(location == region) %>% 
    pull(rei)
  
  # Filter historical data for this region's top 3 risk factors
  region_data <- historical_data %>% 
    filter(location == region, rei %in% top3_rei)
  
  # Create label data (only mark points for 2021)
  label_data <- region_data %>%
    filter(year == 2021) %>%
    arrange(desc(val))
  
  # Calculate appropriate vertical offset
  y_range <- max(region_data$val) - min(region_data$val)
  nudge_y_factor <- ifelse(y_range > 1000, 0.1, 0.05)
  
  # Core plotting: add confidence intervals + reduce point size to perfect dimensions + layer optimization
  ggplot(region_data, aes(x = year, y = val, group = rei, color = rei, fill = rei)) +
    # 1. Confidence intervals [critical for GBD data]: ribbon for 95% UI, low transparency, bottom layer
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.12, color = NA, size = 0) +
    # 2. Trend lines: slightly adjusted line thickness, optimized transparency, middle layer
    geom_line(size = 0.55, alpha = 0.95) +
    # 3. Scatter points: core optimization [points reduced to 0.5, perfectly adapted for small panels], top layer, not obtrusive
    geom_point(size = 0.5, alpha = 1) +
    # 4. Label optimization: smaller, more refined font, adapted for compact layout
    geom_text_repel(
      data = label_data,
      aes(label = rei),
      size = 1.7,  # Slightly adjusted font size for refinement
      nudge_x = 2,
      nudge_y = y_range * nudge_y_factor,
      direction = "y",
      hjust = 0,
      segment.size = 0.12,
      segment.alpha = 0.25,
      min.segment.length = 0.01,
      box.padding = 0.12,
      point.padding = 0.12,
      max.iter = 15000,
      max.overlaps = Inf,
      force = 0.25,
      family = "sans",
      seed = 123
    ) +
    # Color mapping: use professional color scheme
    scale_color_manual(values = rei_colors) +
    scale_fill_manual(values = rei_colors) + # Confidence interval fill matches line color
    # Axis optimization
    scale_x_continuous(breaks = c(1990, 2000, 2010, 2021), limits = c(1990, 2040)) +
    scale_y_continuous(labels = comma_format(accuracy = 1), expand = expansion(mult = c(0.05, 0.20))) +
    labs(title = region) +
    clean_theme()
}

# Generate plots for all regions
region_plots <- lapply(region_order, create_region_plot)

# Combine plots and optimize title/subtitle/caption
final_plot <- wrap_plots(region_plots, ncol = 3, nrow = 9) +
  plot_annotation(
    title = "Top 3 Deaths Risk Factors by Region (1990-2021)",
    subtitle = "Age-standardized Deaths rates per 100,000 population (95% Uncertainty Intervals)",
    caption = "Data Source: Global Burden of Disease Study (GBD)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
      plot.caption = element_text(size = 9, hjust = 1, margin = margin(t = 5, r = 10)),
      plot.background = element_rect(fill = "white", color = NA)
    )
  ) +
  plot_layout(guides = 'collect')

# Save as vertical A4 PDF with high resolution
ggsave("Deaths_risk_factors_vertical_optimized.pdf", final_plot, 
       width = 8.3, height = 11.7, units = "in", 
       device = "pdf", bg = "white", dpi = 300)

message("✅ Optimization complete! Deaths analysis saved to Deaths_risk_factors_vertical_optimized.pdf")

# Save top 3 data
write.csv(top3_data, "table_2_Deaths.csv", row.names = F)