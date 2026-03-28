library(ggplot2)
library(dplyr)
library(patchwork)
library(RColorBrewer)

# Define mapping between risk factors and regions
region_risk_mapping <- list(
  "Diet high in sodium" = c("High-middle SDI", "Middle SDI", "Central Europe", "Southeast Asia", 
                            "East Asia", "High-income Asia Pacific", "Central Latin America", 
                            "Tropical Latin America"),
  
  "Diet low in whole grains" = c("High SDI", "Eastern Europe", "Western Europe", "Central Asia",
                                 "Southern Latin America", "High-income North America", 
                                 "North Africa and Middle East", "Australasia", "Oceania"),
  
  "Diet low in fruits" = c("Low-middle SDI", "Low SDI", "South Asia", 
                           "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa"),
  
  "Diet low in vegetables" = c("Andean Latin America", "Central Sub-Saharan Africa", 
                               "Eastern Sub-Saharan Africa", "Caribbean")
)

# Create function for longitudinal violin plots - final version (uniform axes + full optimization)
create_longitudinal_violin <- function(risk_factor, regions) {
  plot_data <- subset(Risk, 
                      year == 2021 & 
                        age == 'Age-standardized' & 
                        metric == 'Percent' &
                        measure == 'Deaths' &
                        cause != "All causes" &
                        location %in% regions &
                        rei == risk_factor &
                        sex == "Both") %>%
    mutate(percent_val = val * 100) %>%
    mutate(cause = factor(cause)) 
  
  # Select major diseases (sorted by median)
  top_diseases <- plot_data %>%
    group_by(cause) %>%
    summarise(median_percent = median(percent_val, na.rm = TRUE)) %>%
    arrange(desc(median_percent)) %>%
    slice_head(n = 6) %>%
    pull(cause)
  
  plot_data <- plot_data %>%
    filter(cause %in% top_diseases)
  
  # Highly distinguishable shape library (12 non‑repeating, highly distinct, easy to identify)
  shapes_full <- c(16, 17, 15, 18, 8, 3, 4, 2, 23, 24, 25, 22) 
  # Corresponding: solid circle, solid triangle, solid square, solid diamond, star, cross, X, hollow triangle, octagon, pentagon, hexagon, square
  n_regions <- length(regions)
  region_shapes <- shapes_full[1:n_regions]
  
  # Professional journal‑level color palette (high discrimination, grayscale‑friendly, no repetition)
  color_palettes <- c(brewer.pal(8, "Set1"), brewer.pal(8, "Set3"), brewer.pal(8, "Dark2"))
  region_colors <- color_palettes[1:n_regions] %>% unique()
  
  ggplot(plot_data, aes(x = reorder(cause, percent_val), y = percent_val)) +
    # Violin plot: light grey fill with thin border, background does not dominate
    geom_violin(scale = "width", fill = "#F0F0F0", alpha = 0.7, 
                color = "#666666", trim = FALSE, linewidth = 0.4) +
    # Boxplot: white fill + black border + bold red median line, highlights core statistics
    geom_boxplot(width = 0.12, fill = "white", alpha = 1, 
                 outlier.shape = NA, color = "#222222", linewidth = 0.5,
                 median.colour = "#E63946", median.linewidth = 0.7) +
    # Points: color + shape dual mapping, enhanced border, highly identifiable, jitter to avoid overlap
    geom_point(aes(color = location, shape = location), 
               position = position_jitter(width = 0.12, height = 0), 
               size = 2.5, alpha = 0.9, stroke = 0.4) +
    coord_flip() +
    labs(
      title = risk_factor,
      x = "Disease Cause",
      y = "Attributable Mortality (%)"
    ) +
    theme_minimal(base_size = 10, base_family = "sans") +
    theme(
      # Title styling: bold, prominent
      plot.title = element_text(size = 13, face = "bold", hjust = 0, color = "#111111", margin = margin(b=6)),
      axis.title = element_text(size = 11, face = "bold", color = "#222222"),
      axis.text.y = element_text(size = 9.5, face = "bold", color = "#333333"),
      # Axis values bold, clearer ticks
      axis.text.x = element_text(size = 9, face = "bold", color = "#444444"),
      # Legend optimization: larger size + compact layout, shapes and colors clearly visible
      legend.position = "right",
      legend.title = element_text(size = 9, face = "bold", color = "#222222"),
      legend.text = element_text(size = 8, color = "#333333"),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.15, "cm"),
      # Gridlines: only horizontal light grey grid for reading, no clutter
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#DDDDDD", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.margin = margin(4, 8, 4, 4, "pt"),
      panel.border = element_blank()
    ) +
    # Independent mapping for colors and shapes, one‑to‑one with regions
    scale_color_manual(
      name = "Region",
      values = setNames(region_colors, regions)
    ) +
    scale_shape_manual(
      name = "Region",
      values = setNames(region_shapes, regions)
    ) +
    # Core update: force y‑axis breaks at 0,10,20,30,40,50,60,70, all sub‑plots fully aligned
    scale_y_continuous(
      breaks = c(0,10,20,30,40,50,60,70),
      limits = c(0,70),
      expand = c(0,0)
    ) +
    # Combine color and shape legends, reducing redundancy and improving aesthetics
    guides(
      color = guide_legend(ncol = 1, byrow = TRUE, override.aes = list(size = 2)),
      shape = guide_legend(ncol = 1, byrow = TRUE)
    ) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0))
}

# Generate plots for each risk factor (all sub‑plots have uniform axes)
violin_sodium <- create_longitudinal_violin("Diet high in sodium", region_risk_mapping[["Diet high in sodium"]])
violin_whole_grains <- create_longitudinal_violin("Diet low in whole grains", region_risk_mapping[["Diet low in whole grains"]])
violin_fruits <- create_longitudinal_violin("Diet low in fruits", region_risk_mapping[["Diet low in fruits"]])
violin_vegetables <- create_longitudinal_violin("Diet low in vegetables", region_risk_mapping[["Diet low in vegetables"]])

# Combine plots with global title and annotations
combined_violin_plot <- violin_sodium / violin_whole_grains / violin_fruits / violin_vegetables +
  plot_annotation(
    title = "Distribution of Mortality Attributable to Key Dietary Risk Factors",
    subtitle = "Age-standardized Percentage of Deaths by Disease Cause and Socio-demographic Region, 2021",
    caption = "Data Source: Global Burden of Disease Study (GBD) 2021\nEach point denotes a distinct region, with unique color and shape for identification",
    theme = theme(
      plot.title = element_text(size = 17, face = "bold", hjust = 0.5, color = "#111111", margin = margin(b=8)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#333333", margin = margin(b = 12)),
      plot.caption = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(t = 12), lineheight = 1.2)
    )
  ) +
  plot_layout(heights = c(1, 1, 1, 1))

# Display the plot
print(combined_violin_plot)

# Save as high‑resolution A4 PDF (journal submission standard, vector graphics infinitely scalable, white background)
ggsave("Dietary_Risk_Violin_Longitudinal_Final_Version.pdf", 
       combined_violin_plot, 
       width = 8.27, 
       height = 11.69, 
       device = "pdf",
       dpi = 600,
       bg = "white")