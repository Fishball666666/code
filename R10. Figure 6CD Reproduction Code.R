## 0. Environment Setup -------------------------------------------------------------
rm(list = ls())
# Set working directory (modify according to your own path)
setwd('G:\\Dietary disease burden\\Diet and Disease R Input') 

## Only modify these lines to configure the task
data_file      <- "2021_Deaths.csv"  # Re-download the region data with name and id
save_main_name <- "Top1_Risk_Factor_World_Map_2021_Deaths"    # Output file name prefix

unique(top1_data$rei_name)
# Custom color palette: named vector (format: `Risk factor name = "color value"`, colors support hex/English/RGB)
my_colors <- c(
  "Diet low in whole grains" = "#E63946", 
  "Diet high in sodium" = "#F77F00",       
# "Low birth weight" = "#FCBF49",       
  "Diet low in fruits" = "#06D6A0",             
  "Diet low in vegetables" = "#118AB2"         
# "Diet low in fruits" = "#073B4C",   
# "Physical inactivity" = "#9381FF"
  # Can add more: "Your risk factor name" = "color value"
)

# Silently load required packages
suppressPackageStartupMessages({
  library(vroom)   # Fast reading of text files
  library(sf)      # Handling spatial vector data
  library(dplyr)   # Data manipulation
  library(ggplot2) # Plotting
  library(ggsci)   # Journal color schemes
  library(viridis) # High-quality alternative color schemes
})

## 1. Read and process GBD data ----------------------------------------------------------
GBD <- vroom::vroom(data_file) %>%
  rename(location = location_name) %>%
  # Filter conditions (year can be adjusted based on data, original code 2021 may not match file name, modify as needed)
  filter(year == 2021,
         age_name == 'Age-standardized',
         metric_name == 'Percent',
         measure_name == 'Deaths',
         sex_name == "Both",
         cause_name == "All causes")

# Calculate total burden and percentage contribution of each risk factor per country (location_id)
burden_summary <- GBD %>%
  group_by(location_id) %>%
  mutate(total_val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent = (val / total_val) * 100)

# Find the top 1 risk factor for each country
top1_data <- burden_summary %>%
  group_by(location_id) %>%
  slice_max(percent, n = 1, with_ties = FALSE) %>% # Take maximum, avoid ties
  ungroup() %>%
  dplyr::select(location_id, location, rei_name, percent)

## 2. Load world map and merge data ------------------------------------------------------
# Load pre-prepared RData (should contain world_GBD with geometry and Location.ID)
load("GBD_maps.RData") # Please ensure this file is in the working directory

# Merge data with map via location_id
map_data <- left_join(world_GBD, top1_data, by = c("Location.ID" = "location_id"))

# Convert risk factor to factor to control legend order (optional: specify levels=c("factor1","factor2") to customize order)
map_data$rei_name <- factor(map_data$rei_name)

## 3. Create main plot (core: scale_fill_manual implements custom colors)------------------------------
main_map <- ggplot(map_data) +
  geom_sf(aes(geometry = geometry, fill = rei_name), # Fixed original field name error
          color = NA, size = 0.05) + # No borders for a cleaner look
  # Custom color core code: values = custom color vector, na.value sets color for missing data regions
  scale_fill_manual(
    values = my_colors,          # Use the custom color palette defined above
    na.value = "lightgray",      # Fill missing/unmatched countries with light gray
    drop = FALSE                 # Display all custom color categories even if some have no data
  ) +
  theme_void(base_family = "sans") + # Sans-serif font, modern look
  labs(
    title = "Leading Risk Factor for Deaths by Country (2021)",
    subtitle = "Based on percentage contribution to total Deaths",
    caption = "Data Source: Global Burden of Disease Study 2021",
    fill = "Top Risk Factor" # Legend title
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 1, color = "grey50"),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm")
  ) +
  guides(fill = guide_legend(ncol = 1)) # Legend in one column

# Display the map
print(main_map)

## 4. Save results --------------------------------------------------------------
# Save as PDF (vector graphic, meets journal publication requirements)
ggsave(
  filename = paste0(save_main_name, ".pdf"),
  plot = main_map,
  device = "pdf",
  width = 14,
  height = 7,
  units = "in"
)

# Optional: Save as high-resolution PNG
ggsave(
  filename = paste0(save_main_name, ".png"),
  plot = main_map,
  device = "png",
  width = 14,
  height = 7,
  units = "in",
  dpi = 300 
)