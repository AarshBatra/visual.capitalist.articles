#### subdistrict level compliance with national and who guideline===================

library(tidyverse)
library(patchwork)
library(ggplot2)

# download final_all_years_cbind_df_sh_mapped_long. See raw.data.for.plot folder
# for instructions on how to download this dataset and then run the code below.
# Also, make sure to download any appropriate libraries used below apart from 
# the ones loaded above.

# Calculate compliance counts by year-month
compliance_summary <- final_all_years_cbind_df_sh_mapped_long %>%
  group_by(year, month, state_name) %>%
  summarise(
    national_compliant = sum(avg_pm2.5 <= 40, na.rm = TRUE),
    who_compliant = ifelse(first(year) >= 2021, sum(avg_pm2.5 <= 5, na.rm = TRUE), sum(avg_pm2.5 <= 10, na.rm = TRUE)),
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


# Create binned categories with distinct colors
compliance_summary_binned <- compliance_summary %>%
  mutate(
    compliance_bin_nat = cut(
      national_pct,
      breaks = c(0, 20, 40, 60, 80, 100),
      labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
      include.lowest = TRUE
    ), 
     compliance_bin_who = cut(
      who_pct,
      breaks = c(0, 20, 40, 60, 80, 100),
      labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
      include.lowest = TRUE
    )
  ) %>%
  mutate(state_name = str_to_title(state_name)) %>%
  mutate(month_name = recode(month_name, !!!setNames(month.abb, month.name))) %>%
   mutate(year_short = paste0("'", substr(as.character(year), 3, 4)))

# Define a bold, distinct color palette
stripe_colors <- c(
  "0-20%" = "#d73027",    # Deep red
  "20-40%" = "#fc8d59",   # Orange
  "40-60%" = "#fee090",   # Yellow
  "60-80%" = "#91cf60",   # Light green
  "80-100%" = "#1a9850"   # Deep green
)

#### w.r.t national standard
p1 <- ggplot(
  compliance_summary_binned,
  aes(x = factor(year), y = month_name, fill = compliance_bin_nat)
) +
  geom_tile(color = "white", linewidth = 0.3, width = 0.95, height = 0.95) +
  scale_fill_manual(
    values = stripe_colors,
    name = "% of Subdistricts\nMeeting National PM2.5 Standard",
    drop = FALSE
  ) +
  scale_x_discrete(
    breaks = seq(1998, 2024, 2),  # Show every 2 years
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  labs(
    title = "Compliance Stripes: All Indian States Across 27 Years (1998 to 2024)",
    subtitle = "National PM2.5 Standard (≤40 µg/m³ annual average PM2.5)",
    x = "Year",
    y = NULL,
    caption = "Each stripe represents the percentage of subdistricts meeting the National PM2.5 standard in a given month-year\nGraphic: Aarsh Batra (github.com/AarshBatra/biteSizedAQ)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 52, margin = margin(b = 8)),
    plot.subtitle = element_text(hjust = 0.5, size = 33, color = "grey30", margin = margin(b = 20)),
    plot.caption = element_text(hjust = 1, size = 23, color = "grey40", margin = margin(t = 20), lineheight = 1.2),
    panel.grid = element_blank(),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(1.5, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 22),
    axis.text.y = element_text(size = 22),
    axis.title.x = element_text(size = 31, margin = margin(t = 1.2, b = 1, unit = "cm"), face = "bold"),
    strip.text = element_text(face = "bold", size = 28, margin = margin(b = 6, t = 6)),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 25),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.background = element_rect(fill = "white", color = "grey60", linewidth = 0.5),
    legend.box.background = element_rect(fill = "white", color = "grey60", linewidth = 0.5),
    legend.margin = margin(12, 12, 12, 12),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  facet_wrap(~state_name, nrow = 5, ncol = 7, scales = "free")

# Save with appropriate dimensions for 27 states
ggsave("[test3]compliance_stripes_chart_all_states_nat.png", p1, 
       width = 54, height = 34, dpi = 520, bg = "white", limits = FALSE)


#### w.r.t who guideline
p2 <- ggplot(
  compliance_summary_binned,
  aes(x = factor(year), y = month_name, fill = compliance_bin_who)
) +
  geom_tile(color = "white", linewidth = 0.3, width = 0.95, height = 0.95) +
  scale_fill_manual(
    values = stripe_colors,
    name = "% of Subdistricts\nMeeting WHO PM2.5 Guideline",
    drop = FALSE
  ) +
  scale_x_discrete(
    breaks = seq(1998, 2024, 2),  # Show every 2 years
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  labs(
    title = "Compliance Stripes: All Indian States Across 27 Years (1998 to 2024)",
    subtitle = "WHO PM2.5 Guideline (≤5 µg/m³ annual average PM2.5)",
    x = "Year",
    y = NULL,
    caption = "Each stripe represents the percentage of subdistricts meeting the WHO PM2.5 guideline in a given month-year\nGraphic: Aarsh Batra (github.com/AarshBatra/biteSizedAQ)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 52, margin = margin(b = 8)),
    plot.subtitle = element_text(hjust = 0.5, size = 33, color = "grey30", margin = margin(b = 20)),
    plot.caption = element_text(hjust = 1, size = 23, color = "grey40", margin = margin(t = 20), lineheight = 1.2),
    panel.grid = element_blank(),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(1.5, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 22),
    axis.text.y = element_text(size = 22),
    axis.title.x = element_text(size = 31, margin = margin(t = 1.2, b = 1, unit = "cm"), face = "bold"),
    strip.text = element_text(face = "bold", size = 28, margin = margin(b = 6, t = 6)),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 25),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.background = element_rect(fill = "white", color = "grey60", linewidth = 0.5),
    legend.box.background = element_rect(fill = "white", color = "grey60", linewidth = 0.5),
    legend.margin = margin(12, 12, 12, 12),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  facet_wrap(~state_name, nrow = 5, ncol = 7, scales = "free")

# Save with appropriate dimensions for 27 states
ggsave("[test3]compliance_stripes_chart_all_states_who.png", p2, 
       width = 54, height = 34, dpi = 520, bg = "white", limits = FALSE)
