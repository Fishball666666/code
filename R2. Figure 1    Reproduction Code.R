library(dplyr)
library(ggsci)
library(scales)
library(ggplot2)

setwd('G:\\Dietary disease burden\\Diet and Disease R Input')  # Additionally determine the location of the custom region sorting file

# Read data for 27 regions
order <- read.csv('SDI Regional Sorting File.csv', header = FALSE) # You can choose the regional files you want, but these must be included in the original data and correspond accordingly
order$V1 <- rev(order$V1)

# Select key risk factors
key_risks <- c("Diet high in sodium", 
               "Diet low in whole grains", 
               "Diet low in fruits", 
               "Diet low in vegetables",
               "Diet low in omega-6 polyunsaturated fatty acids",
               "Iron deficiency")

# Filter data for 2021
Risk_2021 <- subset(Risk, 
                    year == 2021 & 
                      cause == "All causes" &
                      age == "Age-standardized" &
                      rei != "All risk factors" &
                      sex == "Both" &
                      metric == 'Percent')

# Keep only regions present in the ordering file
Risk_2021 <- Risk_2021[Risk_2021$location %in% order$V1, ]

# Set region ordering
Risk_2021$location <- factor(Risk_2021$location, 
                             levels = order$V1, 
                             ordered = TRUE)

# Create cleaner labels for risk factors
risk_labels <- c(
  "Diet high in sodium" = "Diet high in sodium",
  "Diet low in whole grains" = "Diet low in whole grains",
  "Diet low in fruits" = "Diet low in fruits",
  "Diet low in vegetables" = "Diet low in vegetables",
  "Diet low in omega-6 polyunsaturated fatty acids" = "Diet low in omega-6 polyunsaturated fatty acids",
  "Iron deficiency" = "Iron deficiency"
)

# First filter to keep only key risk factors
Risk_2021 <- Risk_2021[Risk_2021$rei %in% key_risks, ]

# Create factor variable for labels
Risk_2021$rei_label <- factor(Risk_2021$rei, 
                              levels = key_risks,
                              labels = risk_labels[key_risks])

measure_labels <- c("Deaths" = "Deaths", "DALYs" = "DALYs", "YLLs" = "YLLs", "YLDs" = "YLDs")
Risk_2021$measure_en <- factor(Risk_2021$measure)

Risk_2021 <- Risk_2021 %>%
  mutate(lower = ifelse(lower < 0, 0, lower))

# Recalculate max_value
max_value <- max(Risk_2021$upper, na.rm = TRUE) * 1

# Use traditional error bars with caps
p1 <- ggplot(Risk_2021, aes(x = location, y = val, fill = measure_en)) +
  # Draw bars first
  geom_bar(stat = "identity", 
           color = 'black', 
           width = 0.7, 
           position = position_dodge(width = 0.8),
           size = 0.3) +
  
  # Add I-shaped error bars
  geom_errorbar(aes(ymin = lower, ymax = upper, group = measure_en),
                position = position_dodge(width = 0.8),
                width = 0.25,
                size = 0.4,
                color = "black",
                alpha = 0.8) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = percent_format(accuracy = 1),
    limits = c(0, max_value)
  ) +
  
  scale_fill_nejm(name = "Metric", 
                  labels = c("Deaths", "DALYs", "YLDs", "YLLs")) +
  
  coord_flip() + 
  facet_wrap(~ rei_label, 
             ncol = 2,
             scales = "fixed") +
  
  labs(title = "Impact of Selected Dietary Risk Factors on All-Cause Burden (Age-Standardized), 2021",
       subtitle = "Error bars represent 95% uncertainty intervals",
       x = "Country/Region", 
       y = "Percentage of Total Burden",
       caption = "Data source: GBD 2021") +
  
  theme_light(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "#2c3e50"),
    strip.text = element_text(color = "white", size = 11, face = "bold"),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.box = "horizontal",
    legend.box.margin = margin(b = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(color = "gray70", fill = NA, size = 0.5),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, 
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40",
                                 margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 1,
                                margin = margin(t = 10))
  )

print(p1)
