
#### subdistrict level compliance with national and who guideline===================

## install libraries if not already installed.
## plot was made under R version 4.3.2

## load raw data: Note this script uses the data from the raw.data.for.plot folder and then creates
## the compliance_summary object, which is the same as the data stored in the final.data.for.plot folder.

raw_data_for_plot <- read_csv("/path/to/your/data") # enter your local path.

library(tidyverse)
library(patchwork)

# Calculate compliance counts by year-month
compliance_summary <- raw_data_for_plot %>%
  group_by(year, month) %>%
  summarise(
    national_compliant = sum(avg_pm2.5 <= 40, na.rm = TRUE),
    who_compliant = sum(avg_pm2.5 <= 5, na.rm = TRUE),
    total_subdistricts = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    national_pct = (national_compliant / total_subdistricts) * 100,
    who_pct = (who_compliant / total_subdistricts) * 100,
    month_proper = str_to_title(month),
    month_name = factor(month_proper,
                        levels = rev(c("January", "February", "March", "April",
                                       "May", "June", "July", "August",
                                       "September", "October", "November", "December"))),
    # Conditional text color: white for low % (red/dark), black for high % (yellow/light)
    national_text_color = ifelse(national_pct < 60, "black", "white"),
    who_text_color = ifelse(who_pct < 60, "black", "white")
  )

# National guideline heatmap
p1 <- ggplot(compliance_summary,
             aes(x = factor(year), y = month_name, fill = national_pct)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = sprintf("%.1f%%", national_pct), color = national_text_color),
            size = 6, fontface = "bold") +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#fee090",
    high = "#1a9850",
    midpoint = 50,
    limits = c(0, 100),
    name = "% Compliant"
  ) +
  scale_color_identity() +  # Use the actual colors specified
  labs(
    title = "National Standard (≤40 µg/m³)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 22) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
    axis.text.y = element_text(size = 20),
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.key.width = unit(5, "cm"),
    legend.key.height = unit(1, "cm")
  )

# WHO guideline heatmap
p2 <- ggplot(compliance_summary,
             aes(x = factor(year), y = month_name, fill = who_pct)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = sprintf("%.1f%%", who_pct), color = who_text_color),
            size = 6, fontface = "bold") +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#fee090",
    high = "#1a9850",
    midpoint = 50,
    limits = c(0, 100),
    name = "% Compliant"
  ) +
  scale_color_identity() +  # Use the actual colors specified
  labs(
    title = "WHO Guideline (≤5 µg/m³)",
    x = "Year",
    y = NULL
  ) +
  theme_minimal(base_size = 22) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 10)),
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.key.width = unit(5, "cm"),
    legend.key.height = unit(1, "cm")
  )

# Combine plots with descriptive title, subtitle, and caption
combined_plot <- p1 / p2 +
  plot_annotation(
    title = "The Air Quality Crisis in India: A 27-Year View of PM2.5 Compliance",
    subtitle = "Percentage of 5,968 subdistricts meeting national and WHO air quality standards (1998-2024) Months show clear seasonal patterns, with monsoon months (June-September) exhibiting better compliance.There is also a diminishing\n 'greeness' in the national plot as we move from 1998 towards 2024. In any given month, later years have gotten 'on average' worse (especially October to April months) in terms of compliance compared to previous years.\nWhile the WHO guideline comparison is clear - practically no compliance in more than 2 decades.",
    caption = "Data: Population-weighted monthly PM2.5 concentrations across Indian subdistricts\nNational Standard: 40 µg/m³ annual average | WHO Guideline: 5 µg/m³ annual average\nGreen = high compliance, Red = low compliance | Each cell shows percentage of subdistricts meeting the threshold\nGraphic: Aarsh Batra (github.com/AarshBatra/biteSizedAQ)",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold", hjust = 0.5, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 20, hjust = 0.5, color = "gray20",
                                   lineheight = 1.3, margin = margin(b = 20)),
      plot.caption = element_text(size = 16, hjust = 0, color = "gray40",
                                  lineheight = 1.4, margin = margin(t = 20))
    )
  )

print(combined_plot)

# Save as VERY LARGE high-resolution plot
ggsave("pm25_compliance_heatmap.png",
       combined_plot,
       width = 30,
       height = 22,
       dpi = 300,
       bg = "white")
