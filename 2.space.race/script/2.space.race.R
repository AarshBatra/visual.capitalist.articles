
# load libraries
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

#### Read directly from GitHub and clean column names
outer_space_objects <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv')

outer_space_objects <- outer_space_objects %>%
  clean_names() %>%
  mutate_if(is.character, tolower)


#### group data into decades--------------
outer_space_objects <- outer_space_objects %>%
  mutate(decade = paste0(floor(year / 10) * 10, "-", floor(year / 10) * 10 + 10)) %>%
  arrange(year) %>%
  filter(entity != "world")


#### calculate the total number of objects sent in space by each entity in each decade
outer_space_objects_summary <- outer_space_objects %>%
  group_by(entity, decade) %>%
  summarise(num_objects = sum(num_objects, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(entity, decade, num_objects) %>%
  mutate(entity = str_to_title(entity))


#### Plot the number of unique entities that sent objects into space in each decade

# Load rocket image (replace with the path to your rocket image)
rocket_img <- grid::rasterGrob(png::readPNG(paste0(here(), "/images/thin_rocket_bg_rm_adobe.png")), width = unit(1, "npc"), height = unit(1, "npc"))
satellite_img <- png::readPNG(paste0(here(), "/images/tiny_satellite.png"))
background_img <- grid::rasterGrob(readPNG(paste0(here(), "/images/starry_night_2_png.png")), width = unit(1, "npc"), height = unit(1, "npc"))

fmly_font <- "Audiowide"

# font_add_google(fmly_font)
# showtext_auto()

font_import()
loadfonts(device = "win")


# gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

# Download GitHub logo and read it as an image
github_logo <- magick::image_read("https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png")

# Save logo to a temporary file
temp_logo_path <- tempfile(fileext = ".png")
magick::image_write(github_logo, path = temp_logo_path, format = "png")



# Create the plot
plt <- outer_space_objects_summary %>%
  ggplot() +
  # Main plot with geom_quasirandom, using deep space colors
  geom_quasirandom(mapping = aes(x = num_objects, y = decade, size = num_objects, alpha = num_objects),
                   groupOnX = FALSE, width = 0.4, color = "cyan") +
  scale_size(range = c(5, 35), breaks = c(1, 10, 100, 1000, 5000)) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.1))) +
  scale_alpha_continuous(range = c(0.25, 0.8), breaks = c(1, 10, 100, 1000, 5000)) +

  # # Highlighting Russia and USA with elegant space-themed colors
  #   geom_point(data = filter(outer_space_objects_summary, num_objects > 300),
  #              aes(x = num_objects, y = decade, size= num_objects, color = entity)) +
  # scale_colour_paletteer_d("dichromat::Categorical_12") +

  # Labels for high object counts > 1000
  geom_text(data = filter(outer_space_objects_summary, num_objects > 800),
            aes(x = num_objects, y = decade, label = paste(entity)),
            hjust = 0.5, vjust = -1.2, size = 8, color = "white") +

  #  # Labels for high object counts
  geom_text(data = filter(outer_space_objects_summary, num_objects > 100, num_objects < 1000, decade %in% c("1950-1960", "1960-1970", "1970-1980", "1980-1990")),
            aes(x = num_objects, y = decade, label = paste(entity)),
            hjust = 0.5, vjust = -1.2, size = 8, color = "white") +
  #
  geom_text(data = filter(outer_space_objects_summary, num_objects < 100, decade %in% c("1950-1960")),
            aes(x = num_objects, y = decade, label = paste(entity)),
            hjust = 0.5, vjust = -1.2, size = 8, color = "white") +

  ggplot2::annotate(
    "text", label = "Australia, Germany",
    x = 1.4 , y = "1960-1970" , size = 8, colour = "white"
  ) +

  ggplot2::annotate(
    "text", label = "UK",
    x = 4.1 , y = "1960-1970" , size = 8, colour = "white"
  ) +

  ggplot2::annotate(
    "text", label = "China",
    x = 6.7 , y = "1970-1980" , size = 8, colour = "white"
  ) +
  ggplot2::annotate(
    "text", label = "India",
    x = 3.1 , y = "1970-1980" , size = 8, colour = "white"
  ) +
  ggplot2::annotate(
    "text", label = "China",
    x = 19.3 , y = "1980-1990" , size = 8, colour = "white"
  ) +
  ggplot2::annotate(
    "text", label = "France",
    x = 5 , y = "1980-1990" , size = 8, colour = "white"
  ) +

  ggplot2::annotate(
    "text", label = "India",
    x = 14.5 , y = "1990-2000" , size = 8, colour = "white"
  ) +

  ggplot2::annotate(
    "text", label = "India",
    x = 28 , y = "2000-2010" , size = 8, colour = "white"
  ) +

  ggplot2::annotate(
    "text", label = "UK",
    x = 16 , y = "2000-2010" , size = 8, colour = "white"
  ) +

  ggplot2::annotate(
    "text", label = "India, UK",
    x = 75 , y = "2010-2020" , size = 8, colour = "white"
  ) +

  ggplot2::annotate(
    "text", label = "Japan",
    x = 75 , y = "2020-2030" , size = 8, colour = "white"
  ) +

  ggplot2::annotate(
    "text", label = "Luxembourg",
    x = 55 , y = "2020-2030" , size = 8, colour = "white"
  ) +

  ggplot2::annotate(
    "text", label = "India",
    x = 25 , y = "2020-2030" , size = 8, colour = "white"
  ) +


  geom_text(data = filter(outer_space_objects_summary, num_objects > 100, decade %in% c("2000-2010", "2010-2020", "2020-2030")),
            aes(x = num_objects, y = decade, label = paste(entity)),
            hjust = 0.5, vjust = -1.2, size =8, color = "white") +

  geom_text(data = filter(outer_space_objects_summary, decade %in% c("1960-1970"), num_objects > 8),
            aes(x = num_objects, y = decade, label = paste(entity)),
            hjust = 0.5, vjust = -1.2, size = 8, color = "white") +

  ggplot2::annotate(
    "text", label = "Japan, France",
    x = 21.5 , y = "1970-1980" , size = 8, colour = "white"
  ) +

  geom_text(data = filter(outer_space_objects_summary, num_objects > 25, num_objects < 100, decade %in% c("1980-1990")),
            aes(x = num_objects, y = decade, label = paste(entity)),
            hjust = 0.5, vjust = -1.2, size = 8, color = "white") +

  ggplot2::annotate(
    "text", label = "US, Russia",
    x = 650 , y = "1990-2000" , size = 8, colour = "white"
  ) +

  geom_text(data = filter(outer_space_objects_summary, num_objects > 60, num_objects <= 150, decade %in% c("2000-2010")),
            aes(x = num_objects, y = decade, label = paste(entity)),
            hjust = 0.5, vjust = -1.2, size = 8, color = "white") +

  geom_text(data = filter(outer_space_objects_summary, num_objects > 30, num_objects < 100, decade %in% c("1990-2000")),
            aes(x = num_objects, y = decade, label = paste(entity)),
            hjust = 0.5, vjust = -1.2, size = 8, color = "white") +



  # Log scale for y-axis
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000), limits = c(1, 10000)) +

  # geom_image(aes(x = num_objects, y = decade, image = paste0(here(), "/images/tiny_satellite.png")),
  #        size = 0.05,  # Adjust size of the satellite image
  #        position = position_quasirandom(width = 0.1)) +

  # Title and subtitle with space-related theme
  labs(title = "Space Race Through the Decades",
       subtitle = "Number of space launches from 1950-present",
       y = "Decade",
       x = "Number of Objects Launched",
       color = "",
       caption = "Graphic · github.com/AarshBatra | Data Source · UNOOSA | Data Processed · Our World in Data") +

  # Apply a dark space-themed background with stars as minor gridlines
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        legend.text = element_text(color = "white", family = fmly_font),
        plot.background = element_rect(fill = "#001f3f", color = NA),  # Deep space color
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +  # Add space for the rocket image

  # Add rocket image along the y-axis (adjust width/height and coordinates)
  annotation_custom(rocket_img,
                    ymin = -0.5, ymax = 1.3, xmin = -Inf, xmax = Inf) +
  coord_flip() +

  theme(
    plot.title = element_text(family = fmly_font, size = 50, face = "bold", color = "#FFB300", hjust = 0),
    plot.subtitle = element_text(family = fmly_font, size = 33, color = "white", hjust = 0,
                                 margin = margin(b = 1.3, unit = "cm")),
    axis.text = element_text(family = fmly_font, size = 25, color = "white", face = "bold"),
    axis.title.x = element_text(family = fmly_font, size = 30, face = "bold", color = "white", margin = margin(b = 1.5, t = 0.5, unit = "cm")),
    axis.title.y = element_text(family = fmly_font, size = 30, face = "bold", color = "white", margin = margin(l = 0.5, r = 0.5, unit = "cm")),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    legend.box.background = element_rect(fill = NA,  color = "white", linewidth = 0.3),
    plot.caption = element_text(family = fmly_font, size = 18, hjust = 0.5, color = "white", margin = margin(b = 0.2, unit = "cm"))
  )

plt1 <- ggimage::ggbackground(plt, "./images/starry_night_3_png.png")


pggsave("plt4.pdf", plt1, width = 20, height = 11, dpi = 350, units = "in")
extrafont::embed_fonts("plt4.pdf")

