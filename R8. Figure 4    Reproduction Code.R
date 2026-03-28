setwd('G:\\Dietary disease burden\\Diet and Disease R Input')  # Additionally determine the location of the custom region sorting file
library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(scales)
library(viridis)
library(ggsci) 

# Read data
order <- read.csv("27 regional documents.csv", header = F)
region_order <- order$V1

# Data processing: get top 3 risk factors for each region in 2021 (using YLDs)
top3_data <- Risk %>%
  filter(age == "Age-standardized",
         metric == "Rate",
         measure == "YLDs",  # Use YLDs
         sex == "Both",
         cause == "All causes",
         year == 2021) %>%
  group_by(location) %>%
  arrange(desc(val)) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  mutate(location = factor(location, levels = region_order))

# Get historical data for all top 3 risk factors (using YLDs)
all_top3_rei <- unique(top3_data$rei)
historical_data <- Risk %>%
  filter(age == "Age-standardized",
         metric == "Rate",
         measure == "YLDs",  # Use YLDs
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
      plot.title = element_text(size = 8, face = "bold", hjust = 0.5, 
                                margin = margin(b = 1), color = "black"),
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
      axis.ticks.length = unit(1.5, "pt") # More refined tick marks
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
    # Sort by value to optimize label positions
    arrange(desc(val))
  
  # Calculate appropriate vertical offset
  y_range <- max(region_data$val) - min(region_data$val)
  nudge_y_factor <- ifelse(y_range > 1000, 0.1, 0.05)  # Adjust offset based on Y-axis range
  
  # Create plot - all core optimizations here
  ggplot(region_data, aes(x = year, y = val, group = rei, color = rei, fill = rei)) +
    # 1. GBD standard 95% confidence intervals [new core], bottom layer, semi-transparent without occlusion
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.12, color = NA, size = 0) +
    # 2. Trend lines, finely adjusted thickness for refinement
    geom_line(size = 0.55, alpha = 0.95) +
    # 3. Scatter points size 0.5 [perfect size], completely solves the issue of points being too large
    geom_point(size = 0.5, alpha = 1) +
    # 4. Label optimization, slightly adjusted font/spacing for better fit
    geom_text_repel(
      data = label_data,
      aes(label = rei),
      size = 1.7,  # Slightly refined font size
      nudge_x = 2,  # Horizontal offset
      nudge_y = y_range * nudge_y_factor,  # Dynamic vertical offset
      direction = "y",  # Adjust only in y direction
      hjust = 0,
      segment.size = 0.12,  # Thinner connecting lines
      segment.alpha = 0.25,
      min.segment.length = 0.01,
      box.padding = 0.12,
      point.padding = 0.12,
      max.iter = 15000,  # Increase iterations to prevent overlap
      max.overlaps = Inf,
      force = 0.25,
      family = "sans",
      seed = 123
    ) +
    # Color mapping - professional color scheme
    scale_color_manual(values = rei_colors) +
    scale_fill_manual(values = rei_colors) +
    # Axis settings unchanged
    scale_x_continuous(
      breaks = c(1990, 2000, 2010, 2021),
      limits = c(1990, 2040)  # Leave more space for labels
    ) +
    scale_y_continuous(
      labels = comma_format(accuracy = 1),
      expand = expansion(mult = c(0.05, 0.20))  # Adjust top space
    ) +
    labs(title = region) +
    clean_theme()
}

# Generate plots for all regions
region_plots <- lapply(region_order, create_region_plot)

# Combine all 27 plots onto a vertical A4 page (3 columns, 9 rows layout)
final_plot <- wrap_plots(region_plots, ncol = 3, nrow = 9) +
  plot_annotation(
    title = "Top 3 YLDs Risk Factors by Region (1990-2021)",
    subtitle = "Age-standardized YLDs rates per 100,000 population (95% Uncertainty Intervals)",
    caption = "Data Source: Global Burden of Disease Study",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                                margin = margin(t = 10, b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, 
                                   margin = margin(b = 10)),
      plot.caption = element_text(size = 9, hjust = 1, 
                                  margin = margin(t = 5, r = 10)),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  ) +
  plot_layout(guides = 'collect')

# Save as vertical A4 PDF with high resolution
ggsave("YLDs_risk_factors_vertical.pdf", final_plot, 
       width = 8.3, height = 11.7, units = "in",  # Vertical A4 dimensions
       device = "pdf", bg = "white", dpi = 300) # High resolution 300 dpi

message("✅ YLDs analysis saved to YLDs_risk_factors_vertical.pdf")

# Save top 3 data
write.csv(top3_data, "table_2_YLDs.csv", row.names = FALSE)