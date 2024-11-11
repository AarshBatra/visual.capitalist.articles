### load libraries

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


# Negation of the "%in%" operator function
`%notin%` <- Negate(`%in%`)





### Part 1: How much longer can you live if you breathed clean air and how does it compare with other health threats? - Air Quality Life Index

# load in the aqli helper script (from the R/ subfolder of the repo) and download all relevant datasets as specified in the readme file of the repo.

# Define the target projection (Equal Earth - EPSG:8857)
equal_earth_proj <- "+proj=eqearth +datum=WGS84 +wktext"

# converting AQLI shapefiles to a Equal Earth Projection
gadm2_aqli_2021_shp <- st_transform(gadm2_aqli_2021_shp, equal_earth_proj)
gadm1_aqli_2021_shp <- st_transform(gadm1_aqli_2021_shp, equal_earth_proj)
gadm0_aqli_2021_shp <- st_transform(gadm0_aqli_2021_shp, equal_earth_proj)

# figuring out the objectids for gadm1 for which population is not available
obid_gadm1_pop_na <- gadm1_aqli_2021 %>%
  filter(is.na(population)) %>%
  select(objectid_gadm1) %>%
  unlist() %>%
  as.vector()

# figuring out the objectids for gadm0 for which population is not available
obid_gadm0_pop_na <- gadm0_aqli_2021 %>%
  filter(is.na(population)) %>%
  select(objectid_gadm0) %>%
  unlist() %>%
  as.vector()


# global shapefile gadm2 map data
aqli_global_gadm2_data <- gadm2_aqli_2021 %>%
  left_join(gadm2_aqli_2021_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

## removing extremely tiny polygons from all three shapefiles to focus on the main map

aqli_global_gadm2_data$area <- st_area(aqli_global_gadm2_data)

# minimum area threshold for the polygons
threshold <- 10000000 # units are m^2

# adding a "area without units" column
aqli_global_gadm2_data <- aqli_global_gadm2_data %>%
  mutate(area_without_units = as.numeric(str_remove(area, " [m^2]")))

aqli_global_gadm2_data <- aqli_global_gadm2_data %>%
  filter(area_without_units > threshold)

# filter out small island regions from all gadm shapefiles
aqli_global_gadm2_data <- aqli_global_gadm2_data %>%
  filter(country %notin% c("Samoa", "American Samoa", "French Polynesia", "Fiji",
                           "Tonga", "Niue", "Cook Islands", "Vanuatu", "Solomon Islands",
                           "Vanuatu", "New Caledonia", "Marshall Islands", "Micronesia",
                           "French Southern Territories", "Saint Helena, Ascension and Tris")) %>%
  filter(!((country  == "New Zealand") & (name_1 == "Galápagos"))) %>%
  filter(!((country  == "United States") & (name_1 == "Hawaii"))) %>%
  filter(!is.na(population))

gadm1_aqli_2021_shp <- gadm1_aqli_2021_shp %>%
  filter(name0 %notin% c("Samoa", "American Samoa", "French Polynesia", "Fiji",
                         "Tonga", "Niue", "Cook Islands", "Vanuatu", "Solomon Islands",
                         "Vanuatu", "New Caledonia", "Marshall Islands", "Micronesia",
                         "French Southern Territories", "Saint Helena, Ascension and Tris")) %>%
  filter(!((name0  == "New Zealand") & (name1 == "Galápagos"))) %>%
  filter(!((name0  == "United States") & (name1 == "Hawaii"))) %>%
  filter(obidgadm1 %notin% obid_gadm1_pop_na)

gadm0_aqli_2021_shp <- gadm0_aqli_2021_shp %>%
  filter(name0 %notin% c("Samoa", "American Samoa", "French Polynesia", "Fiji",
                         "Tonga", "Niue", "Cook Islands", "Vanuatu", "Solomon Islands",
                         "Vanuatu", "New Caledonia", "Marshall Islands", "Micronesia",
                         "French Southern Territories", "Saint Helena, Ascension and Tris")) %>%
  filter(obidgadm0 %notin% obid_gadm0_pop_na)


# creating a tibble for text boxes annotation positions and labels in order: LA, Delhi, Ouest(Cameroon), Melbourne (Australia), Lima (Peru), Warsaw (Poland)
tb_annotation <- tibble(
  label_x = c(-139, 64.83, -9, 126.5, -87.72, -29.66),
  label_y = c(18.11, 7.5, -0.54, -49.09, -20.34, 40.04),
  text = c("**Los Angeles (US)**, 0.65 years = 7.8 months",
           "**Delhi (India)**, 11.9 years",
           "**Ouest (Cameroon)**, 4.5 years",
           "**Melbourne (Australia)** is in compliance with the WHO PM₂.₅ guideline of 5 µg/m³",
           "**Lima (Peru)**, 2.3 years",
           "**Warsaw (Poland)**, 1.2 years"))


# global AQLI map for explained.media website
aqli_global_gadm2_map <- aqli_global_gadm2_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "transparent", lwd = 0) +
  geom_sf(data = gadm1_aqli_2021_shp, color = "transparent", fill = "transparent", lwd = 0) +
  geom_sf(data = gadm0_aqli_2021_shp, color = "aliceblue", fill = "transparent", lwd = 0.3) +
  scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                               "0.1 to < 0.5" = "#ffeda0",
                               "0.5 to < 1" = "#fed976",
                               "1 to < 2" = "#feb24c",
                               "2 to < 3" = "#fd8d3c",
                               "3 to < 4" = "#fc4e2a",
                               "4 to < 5" = "#e31a1c",
                               "5 to < 6" = "#bd0026",
                               ">= 6" = "#800026")) +
  geom_textbox(data = tb_annotation, aes(x = label_x, y = label_y, label = text),
               colour = "white",
               box.colour = "#022531",
               fill = NA,
               family = "Segoe UI",
               size = 6.5) +
  #geom_curve(x = -118.25, xend = -134, y = 34.09, yend = 23.2, color = "white", arrow = arrow(), curvature = 0.5) +
  annotate("segment", x = -118.25, xend = -134, y = 34.09, yend = 23.2, arrow = arrow(type = "closed", length = unit(0.2, "inches")), colour = "white") + # US
  annotate("segment", x = 77.14, xend = 65.09, y = 28.68, yend = 11.82, arrow = arrow(type = "closed", length = unit(0.2, "inches")), colour = "white") +
  annotate("curve", x = 10.18, xend = -12.28, y = 5.5, yend = -20.96, arrow = arrow(type = "closed", length = unit(0.2, "inches")), colour = "white", curvature  = -0.5) +
  annotate("segment", x = 144.97, xend = 131.18, y = -37.97, yend = -41.01, arrow = arrow(type = "closed", length = unit(0.2, "inches")), colour = "white") +
  annotate("segment", x = -77.03, xend = -82.74, y = -12.02, yend = -16.39, arrow = arrow(type = "closed", length = unit(0.2, "inches")), colour = "white") +
  annotate("segment", x = 21.03, xend = -19.7, y = 52.24, yend = 43, arrow = arrow(type = "closed", length = unit(0.2, "inches")), colour = "white") +
  coord_sf(default_crs = sf::st_crs(4326)) +
  ggthemes::theme_map() +
  labs(title = "How much longer can you live if you breathed clean air?",
       subtitle = expression("Life expectancy gains, if 2021" ~ PM[2.5] ~ "pollution is reduced to meet the WHO" ~ PM[2.5] ~ "guideline"),
       fill = "Potential gain in life expectancy (years)",
       caption = expression("Source · Air Quality Life Index, Annual Report 2023 | Graphic · Aarsh Batra · github.com/AarshBatra | explained.media")) +
  theme(legend.position = c(0.6, 0.98),
        legend.background = element_rect(fill = "#022531"),
        legend.text = element_text(color = "white", face = "bold" , size = 21),
        plot.background = element_rect(colour = "#022531",
                                       fill = "#022531"),
        plot.caption = element_text(size = 15, hjust = 0.03, color = "white"),
        plot.title = element_text(size = 35, family = "Segoe UI", face = "bold", color = "white", hjust = 0.03,
                                  margin = margin(b = 0.5, unit = "cm")),
        plot.subtitle = element_text(size = 22, family = "Segoe UI", face = "italic", color = "white", hjust = 0.03,
                                     margin = margin(b = 0.5, unit = "cm")),
        legend.spacing.x = unit(1.5, "lines"),
        legend.title = element_text(color = "white", hjust = 0.5, size = 25, family = "Segoe UI")) +
  # legend.margin = margin(b = 1, unit = "cm")) +
  guides(fill = guide_legend(
    keywidth = unit(2, "lines"),  # Adjust key width
    keyheight = unit(2, "lines"),  # Adjust key height
    label.position = "bottom",  # Position labels below keys
    label.hjust = 0.5,  # Center labels horizontally
    label.vjust = 1,  # Adjust vertical positioning of labels
    nrow = 1,
    shape = guide_legend(override.aes = list(shape = 19))
  ))


# save plot
ggsave(paste0(here(), "/output/aqli_intro_map_vis_cap.png"), aqli_global_gadm2_map, width = 17, height = 9, dpi = 320, scale = 2)





## Part 1 fig 2: Comparing PM2.5 with other health threats-----------------------------------------------------


# create a version of the figure with the same diseases as used in the same figure in last year's report

pm2.5_comparison_oth_threats_data <- gbd_results_master_2021 %>%
  filter(country == "Global", cause_of_death %in% c("PM2.5 relative to WHO guideline", "Tobacco", "Alcohol use",
                                                    "Unsafe water, sanitation, and handwashing",
                                                    "Transport injuries",
                                                    "HIV/AIDS and sexually transmitted infections",
                                                    "Neglected tropical diseases and malaria",
                                                    "Nutritional deficiencies",
                                                    "Child and maternal malnutrition"))
colnames(pm2.5_comparison_oth_threats_data)[3] <- c("llpp_who_2021")

pm2.5_comparison_oth_threats_data <- pm2.5_comparison_oth_threats_data %>%
  mutate(lyl_bucket = ifelse((llpp_who_2021 >= 0) & (llpp_who_2021 < 0.1), "0 - < 0.1", NA),
         lyl_bucket = ifelse((llpp_who_2021 >= 0.1) & (llpp_who_2021 <= 0.5), "0.1 - 0.5", lyl_bucket),
         lyl_bucket = ifelse((llpp_who_2021 > 0.5) & (llpp_who_2021 <= 1), "> 0.5 - 1", lyl_bucket),
         lyl_bucket = ifelse((llpp_who_2021 > 1) & (llpp_who_2021 <= 2), "> 1 - 2", lyl_bucket),
         lyl_bucket = ifelse((llpp_who_2021 > 2) & (llpp_who_2021 <= 3), "> 2 - 3", lyl_bucket),
         lyl_bucket = ifelse((llpp_who_2021 > 3) & (llpp_who_2021 <= 4), "> 3 - 4", lyl_bucket),
         lyl_bucket = ifelse((llpp_who_2021 > 4) & (llpp_who_2021 <= 5), "> 4 - 5", lyl_bucket),
         lyl_bucket = ifelse((llpp_who_2021 > 5) & (llpp_who_2021 < 6), "> 5 - < 6", lyl_bucket),
         lyl_bucket = ifelse((llpp_who_2021 >= 6), ">= 6", lyl_bucket)) %>%
  mutate(order_lyl_bucket = ifelse(lyl_bucket == "0 - < 0.1", 1, NA),
         order_lyl_bucket = ifelse(lyl_bucket == "0.1 - 0.5", 2, order_lyl_bucket),
         order_lyl_bucket = ifelse(lyl_bucket == "> 0.5 - 1", 3, order_lyl_bucket),
         order_lyl_bucket = ifelse(lyl_bucket == "> 1 - 2", 4, order_lyl_bucket),
         order_lyl_bucket = ifelse(lyl_bucket == "> 2 - 3", 5, order_lyl_bucket),
         order_lyl_bucket = ifelse(lyl_bucket == "> 3 - 4", 6, order_lyl_bucket),
         order_lyl_bucket = ifelse(lyl_bucket == "> 4 - 5", 7, order_lyl_bucket),
         order_lyl_bucket = ifelse(lyl_bucket == "> 5 - 6", 8, order_lyl_bucket),
         order_lyl_bucket = ifelse(lyl_bucket == ">= 6", 9, order_lyl_bucket))





# pm2.5 compared to other health threats plot

f1 <- "Rubik"

pm2.5_comparison_oth_threats_plt <- pm2.5_comparison_oth_threats_data %>%
  mutate(llpp_who_2021 = round(llpp_who_2021, 1),
         cause_of_death = ifelse(cause_of_death == "PM2.5 relative to WHO guideline", "PM₂.₅ relative to WHO guideline", cause_of_death)) %>%
  ggplot(mapping = aes(x = forcats::fct_reorder(cause_of_death, llpp_who_2021), y = llpp_who_2021)) +
  geom_blank() +
  #     annotate("segment", y = c(0.25, 0.75, 1.25), yend = c(0.25, 0.75, 1.25), x = 0, xend = 9,
  # colour = "#0C475F") +
  # geom_hline(mapping = aes(yintercept = 0.25), color = "#0C475F", linetype = "solid") +
  #   geom_hline(mapping = aes(yintercept = 0.75), color = "#0C475F", linetype = "solid") +
  #   geom_hline(mapping = aes(yintercept = 1.25), color = "#0C475F", linetype = "solid") +
  geom_col(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), width = 0.4, color = "white") +
  geom_text(aes(label = llpp_who_2021, y = (llpp_who_2021 - 0.095)), hjust = -0.1, color = "#663300", size = 7, family = f1,
            fontface = "bold") +
  labs(x = "", y = "Life years lost", fill = "Life years lost",
       title = expression("How does" ~ PM[2.5] ~ "compare with other heath threats?"),
       caption = "Sources · Air Quality Life Index, Annual Report 2023; Global Burden of Disease; WHO life tables | Graphic · Aarsh Batra · github.com/AarshBatra | explained.media") +
  coord_flip() +
  ggthemes::theme_tufte() +
  scale_y_continuous(breaks = seq(0, 3, 0.5), limits = c(0, 2.5)) +
  # scale_x_discrete(limits = cause_of_death_ordered[seq(1, length(cause_of_death_ordered), by = 2)]) +
  scale_fill_manual(values = c("0 - < 0.1" = "#FFFFFF",
                               "0.1 - 0.5" = "#FFE6B3",
                               "> 0.5 - 1" = "#FFD25D",
                               "> 1 - 2" = "#FFBA00",
                               "> 2 - 3" = "#FF9600",
                               "> 3 - 4" = "#FF6908",
                               "> 4 - 5" = "#E63D23",
                               "> 5 - < 6" = "#BD251C",
                               ">= 6" = "#8C130E")) +
  theme(legend.position = "none",
        legend.background = element_rect(fill = "#022531"),
        legend.text = element_text(color = "white", face = "bold" , size = 9),
        plot.background = element_rect(colour = "#022531",
                                       fill = "#022531"),
        plot.caption = element_text(size = 14, hjust = 0, color = "white", margin = margin(t = 1, unit = "cm")),
        plot.title = element_text(size = 15, family = "Segoe UI", face = "bold", color = "white", hjust = -3.3,
                                  margin = margin(b = 0, unit = "cm")),
        plot.subtitle = element_text(size = 11, family = "Segoe UI", face = "italic", color = "white", hjust = 0,
                                     margin = margin(b = 0.5, unit = "cm")),
        legend.spacing.x = unit(1.5, "lines"),
        legend.title = element_text(color = "white", hjust = 0.5, size = 15, family = "Segoe UI"),
        axis.text.y = element_text(color = c("PM₂.₅ relative to WHO guideline" = "#FFFFFF",
                                             "Tobacco" = "#FFE6B3",
                                             "Child and maternal malnutrition" = "#FFE6B3",
                                             "Alcohol use" = "#FFE6B3",
                                             "Unsafe water, sanitation, and handwashing" = "#FFD25D",
                                             "Transport injuries" = "#FFD25D",
                                             "HIV/AIDS and sexually transmitted infections" = "#FFBA00",
                                             "Neglected tropical diseases and malaria" = "#FF9600",
                                             "Nutritional deficiencies" = "#FF9600"), hjust = 0.5, size = 20, family = f1),
        axis.text.x = element_text(color = "white", hjust = 0.5, size = 20, family = f1),
        axis.title.x = element_text(color = "white", hjust = 0.99, size = 21, family = f1, margin = margin(b = 0.5, t = 0.2, unit = "cm")),
        axis.line = element_line(color = "#0C475F")) +
  ggtext::geom_textbox(aes(x = 4.9,
                           y = 1.3),
                       label = "How does the threat from **<span style=\"color:#CC6633\">particle pollution</span>** compare with other global health threats?",
                       hjust = -0.005, halign = 0,
                       vjust = 1, valign = 1,
                       family = f1,
                       lineheight = 0.35,
                       width = unit(35, "lines"),
                       size = 13,
                       box.colour = NA,
                       fill = NA,
                       color = "white")
# save plot
ggsave(paste0(here(), "/output/pm2.5_compared_to_other_threats_vis_cap.png"), pm2.5_comparison_oth_threats_plt, width = 10, height = 8, dpi = 320, scale = 2)

ggsave(paste0(here(), "/[upd]pm2.5_compared_to_other_threats_vis_cap.png"), pm2.5_comparison_oth_threats_plt, width = 10, height = 8, dpi = 320, scale = 2)
