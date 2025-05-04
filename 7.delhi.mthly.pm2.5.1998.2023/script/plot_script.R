
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

# Load in SHRUG blk level shp file (download from here: https://drive.google.com/file/d/1Np9Sg_V4AJGO0bvJSJAaLK3jyxP-tviz/view?usp=drive_link)
ind_shr_subdist_shp <- ind_shr_subdist_shp %>%
  mutate_if(is.character, tolower)

delhi_monthly_blk_data_1998_2023 <- read_csv("./7.delhi.mthly.pm2.5.1998.2023/raw.data/raw_data.csv")

#### Create Delhi Monthly PM2.5 snapshot for selected years b/w 1998 and 2023
delhi_mth_pm2.5_plt <- delhi_monthly_blk_data_1998_2023 %>%
  mutate(month = factor(month, levels = str_to_lower(month.name), labels = month.abb)) %>%
  left_join(
    ind_shr_subdist_shp %>%
      dplyr::select(state_name, district_name, subdistrict_name, geom),
    by = c("state_name", "district_name", "subdistrict_name")
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = avg_pm2.5), color = NA) +
  scale_fill_stepsn(
    colors = rev(viridisLite::magma(7)),
    breaks = c(0, 40, 80, 120, 200, 300),
    limits = c(0, 300),
    na.value = "white"
  ) +
  theme_map() +
  labs(
    fill = expression("Monthly Average" ~ PM[2.5] ~ "(μg/m³)"),
    title = expression("Block-level Monthly Average" ~ PM[2.5] ~ "Pollution in NCT of Delhi Blocks"),
    caption = expression("Graphic · Aarsh Batra | github.com/AarshBatra/biteSizedAQ")
  ) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.key.width = unit(3, "cm"),
    legend.box.margin = margin(0, 0, 15, 0),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Mono"),
    plot.title = element_text(size = 16, family = "Mono", hjust = 0, margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    legend.title = element_text(size = 14, family = "Mono", margin = margin(t = 0.2, b = 0.1, unit = "cm")),
    legend.text = element_text(size = 12, family = "Mono"),
    plot.caption = element_text(size = 9, family = "Mono", margin = margin(t = 0.8, unit = "cm"), hjust = 0.5),
    plot.margin = margin(l = 0.5, r = 0.5, unit = "cm")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  facet_grid(year ~ month, switch = "y")
