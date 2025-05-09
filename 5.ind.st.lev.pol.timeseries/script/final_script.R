library(tidyverse)
library(jsonlite)
library(here)
library(sf)
library(ggplot2)
library(geojson)
library(devtools)
library(geojsonio)
library(geojsonsf)
library(roxygen2)
library(magrittr)
library(stringr)
library(data.table)
library(viridis)
library(ggthemes)
library(hrbrthemes)
library(RColorBrewer)
library(haven)
library(janitor)
library(lwgeom)
library(rvest)
library(dplyr)
library(ggrepel)
library(ggsflabel)
library(fuzzyjoin)
library(tidytext)
library(tm)
library(wordcloud2)
library(recipes)
library(forcats)
library(forcats)
library(geofacet)
library(pak)
library(camcorder)
library(ggbeeswarm)
library(ggtext)
library(ggimage)
library(tidyverse)
library(ggbeeswarm)
library(cowplot)
library(ggtext)
library(scales)
library(grid)
library(png)
library(here)
library(showtext)
library(sysfonts)
library(ggimage)
library(Cairo)
library(extrafont)
library(MetBrewer)
library(rnaturalearth)
library(scico)
library(ggridges)

# Negation of the "%in%" operator function
`%notin%` <- Negate(`%in%`)

#### load raw data
ind_bl_lev_pol_wide <- read_csv(paste0(here(), "/4.ind.bl.lev.pol.dist/raw.data/ind_block_level_pop_weighted_pol_1998_2022.csv"))

#### reshape and keep only year 2022
ind_bl_lev_pol_long <- ind_bl_lev_pol_wide %>%
  pivot_longer(cols = starts_with("avg_pm"), names_to = "pol_year", values_to = "ann_avg_pm2.5") %>%
  mutate(pol_year = as.numeric(str_remove(str_extract(pol_year, "_\\d+"), "_")))

#### state, year wise summary
ind_st_lev_pol_long_summary <- ind_bl_lev_pol_long %>%
  group_by(state_name, pol_year) %>%
  mutate(pop_weights =  subdistrict_population/sum(subdistrict_population, na.rm = TRUE),
         pm2.5_pop_weighted = ann_avg_pm2.5*pop_weights) %>%
  summarize(
    min_pm25 = min(ann_avg_pm2.5, na.rm = TRUE),
    max_pm25 = max(ann_avg_pm2.5, na.rm = TRUE),
    avg_pm25 = sum(pm2.5_pop_weighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate_if(is.character, str_to_title) %>%
  rename(name = state_name) %>%
  mutate(name = ifelse(name == "Jammu Kashmir", "Jammu and Kashmir", name),
         name = ifelse(name == "Nct Of Delhi", "Delhi", name),
         name = ifelse(name == "Daman Diu", "Daman and Diu", name),
         name = ifelse(name == "Dadra Nagar Haveli", "D&N Haveli", name),
         name = ifelse(name == "Andaman Nicobar Islands", "A&N Islands", name),
         name = ifelse(name == "Andhra Pradesh", "AP & Telangana", name))

#### plt2

f1 <- "Roboto Condensed"
india_map <- geofacet::in_state_ut_grid1
india_map <- india_map[c(1:23, 25:36), ]
india_map[29, 4] <- "AP & Telangana"
india_map[35, 4] <- "A&N Islands"
india_map[23, 4] <- "D&N Haveli"


plt <- ggplot(ind_st_lev_pol_long_summary, aes(x = pol_year, group = name)) +
  # Ribbon for min-max PM2.5 range
  geom_ribbon(aes(ymin = min_pm25, ymax = max_pm25),
              fill = "gray70", alpha = 0.2) +  # Smoky ribbon

  # Line for average PM2.5 mapped to color
  geom_line(aes(y = avg_pm25, color = avg_pm25), size = 5) +

  geom_hline(yintercept = 40, linetype = "solid", color = "white", size = 0.8) +
  geom_hline(yintercept = 5, linetype = "solid", color = "white", size = 0.8) +

  # Custom Color Scale
  scale_color_gradientn(
    colours = c("#fbe3c2", "#ffd353", "#ffb242", "#ef8737", "#de4f33", "#bb292c", "#9f2d55", "#62205f", "#341648"),  # Elegant palette
    name = expression("Annual Average PM"["2.5"]*" (µg/m³)"),  # Title for legend
    breaks = c(0, 5, 10, 20, 40, 80, 100, 130),  # Custom breaks
    limits = c(0, 130)  # Ensure limits are consistent
  ) +

  # Custom Y-axis and X-axis scales
  scale_y_continuous(breaks = seq(0, 130, 20), limits = c(0, 130)) +
  scale_x_continuous(breaks = seq(1998, 2022, 4)) +

  # Facet for state-wise trends on India map
  facet_geo(~name, grid = india_map) +

  # Labels and titles
  labs(
    title = expression("Tracking 25 Years of" ~ PM[2.5] ~ "Pollution Across Indian States and UTs"),
    x = "",
    y = expression("Annual Average PM"["2.5"]*" (µg/m³)"),
    caption = expression("Graphic · Aarsh Batra | Raw Pol Data · ACAG, U Wash. 2022" ~ PM[2.5] ~ "Sat. Data | Processed · github.com/AarshBatra/biteSizedAQ"),
    subtitle = str_wrap(
      expression("Over 25 years from 1998 to 2022, not a single state or UT met the WHO’s annual average PM2.5 guideline of 5 µg/m³ even once, and only a handful consistently adhered to the corresponding India’s national standard of 40 µg/m³. The data highlights stark regional disparities and a growing need for urgent, targeted interventions"),
      width = 75
    )
  ) +

  # Dark theme adjustments
  theme_minimal(base_family = "Arial") +
  theme(
    # Legend styling
    legend.position = c(0.8, 1.1),  # Place legend at the top
    legend.direction = "horizontal",  # Horizontal legend
    legend.title = element_text(size = 43, face = "bold", color = "gray90", hjust = 0.5),  # Title formatting
    legend.text = element_text(size = 38, color = "gray80", face = "bold"),  # Text formatting
    legend.key.width = unit(8, "cm"),  # Wider legend keys
    legend.key.height = unit(1.2, "cm"),  # Adjust key height
    legend.background = element_rect(fill = "gray15", color = NA),  # Background for the legend box
    legend.box.background = element_rect(color = "gray15", fill = "gray15", size = 0.5),  # Add a box
    # Strip and other plot settings
    legend.box.margin = margin(b = 1, unit = "cm"),
    legend.title.position = "top",
    strip.text = element_text(size = 40, face = "bold", color = "gray95"),
    strip.background = element_rect(fill = "gray20", color = NA),
    panel.background = element_rect(fill = "gray10", color = NA),
    plot.background = element_rect(fill = "gray15", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 30, color = "gray80", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 30, color = "gray80"),
    axis.title.x = element_text(size = 40, color = "gray90"),
    axis.title.y = element_text(size = 40, color = "gray90", margin = margin(l = 1.5, r = 1.5, b = 0.2, unit = "cm")),
    plot.title = element_text(size = 62, face = "bold", color = "gray65", hjust = 0, margin = margin(t = 1, b = 0.5, unit = "cm")),
    plot.subtitle = element_text(size = 50, color = "white", hjust = 0, face = "italic", margin = margin(b = 3.5, unit = "cm")),
    plot.caption = element_text(size = 40, color = "gray60", hjust = 0.5, margin = margin(t = 3, b = 3, unit = "cm")),
    plot.margin = margin(t = 50, r = 50, b = 10, l = 10)
  )

# ind_st_lev_pol_long_summary %>% rename(state_ut = name, year = pol_year) %>% write_csv("./ind_st_lev_pol_1998_2020_final.csv")

#ggsave("./final_plt.png", plt, width = 49, height = 41, units = "in", dpi = 420)


