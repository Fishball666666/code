library(ggplot2)
library(treemapify)

# Merge diseases below threshold into "Other"
threshold <- 1.0
top_n <- 5  # Only show top 5 diseases

death_percent <- subset(Risk, 
                        year == 2021 & 
                          age == 'Age-standardized' & 
                          metric == 'Percent' & 
                          measure == 'Deaths' &
                          cause != "All causes" &
                          location == "Global" &
                          rei == "Diet high in sodium" &
                          sex == "Both") %>%
  arrange(desc(val)) %>%
  mutate(percent_val = val * 100) %>%
  # Add rank
  mutate(rank = row_number()) %>%
  # Calculate proportion of Other
  mutate(
    is_other = rank > top_n,
    display_group = ifelse(!is_other, cause, "Other causes")
  ) %>%
  # Sum up the proportion of Other
  group_by(display_group) %>%
  summarise(percent_val = sum(percent_val)) %>%
  ungroup() %>%
  # Create concise labels
  mutate(
    cause_short = gsub(" disease| Disease| \\(.*\\)| due to.*", "", display_group),
    cause_short = ifelse(cause_short == "Other causes", "Other", cause_short),
    label = ifelse(display_group == "Other causes", 
                   paste0(round(percent_val, 1), "%"),  # Other only shows percentage
                   paste0(cause_short, "\n", round(percent_val, 1), "%"))
  )

# Nature/Science style color palette
nature_palette <- c(
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD"
)

# Calculate Other percentage for caption
other_percent <- death_percent %>%
  filter(display_group == "Other causes") %>%
  pull(percent_val) %>%
  round(1)

# Keep only top 5 diseases for plotting
plot_data <- death_percent %>%
  filter(display_group != "Other causes") %>%
  mutate(display_group = factor(display_group, levels = display_group))

# Use Nature journal style color fill
# Concise version: directly explain in title
ggplot(plot_data, 
       aes(area = percent_val, 
           fill = reorder(display_group, -percent_val),
           label = label)) +
  
  geom_treemap(
    color = "white", 
    size = 1.5, 
    alpha = 0.95,
    layout = "squarified"
  ) +
  
  geom_treemap_text(
    color = "white",
    place = "centre",
    size = 12,
    fontface = "bold",
    reflow = TRUE
  ) +
  
  # Nature style color fill
  scale_fill_manual(
    values = nature_palette[1:nrow(plot_data)],
    guide = "none"
  ) +
  
  labs(
    title = paste0("Global Mortality from Diet high in sodium (2021)"),
    subtitle = paste0(
      "Top ", top_n, " causes | Other causes (", other_percent, "%) not shown"
    ),
    caption = "Data from Global Burden of Disease Study 2021 | Age-standardized percentages"
  ) +
  
  theme_void() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 5, t = 10)
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      color = "#555555",
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#777777",
      hjust = 1,
      margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )