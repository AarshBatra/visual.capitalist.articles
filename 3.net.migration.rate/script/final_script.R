#### Load necessary libraries
# These libraries will be used for data manipulation, visualization, font management, and handling geographic data
library(ggplot2)      # Data visualization
library(tidyverse)    # Data manipulation
library(stringr)      # String handling
library(showtext)     # Font management for ggplot
library(sysfonts)     # Font management
library(sf)           # Spatial data manipulation
library(here)         # File path management
library(rnaturalearth) # Access to country shapefiles
library(camcorder)    # Record the ggplot output
library(MetBrewer)    # Color palettes
library(ggthemes)     # Extra themes for ggplot
library(ggtext)       # Advanced text formatting in ggplot
library(ggimage)      # For adding images to ggplot
library(magick)       # Image manipulation
library(extrafont)    # Font management
library(png)          # Handle PNG images

#### Load raw data and perform basic cleaning
# Read the CSV file containing net migration data for countries
net_migr_rate <- read_csv(paste0(here(), "/3.net.migration.rate/raw.data/net_migration_rate_cia_world_factbook.csv"))

# Clean up country names and create a cleaner dataframe for migration rate data
net_migr_rate <- net_migr_rate %>%
  select(name, `migrant(s)/1`, ranking) %>%                # Select relevant columns
  rename(country = name,                                   # Rename columns for clarity
         migrants_per_thous_pop = `migrant(s)/1`,
         region = ranking) %>%
  # Standardize country names for consistency
  mutate(country = ifelse(country == "Bahamas, The", "The Bahamas", country),
         country = ifelse(country == "Congo, Republic of the", "Democratic Republic of the Congo", country),
         country = ifelse(country == "Congo, Democratic Republic of the", "Republic of Congo", country),
         country = ifelse(country == "Cabo Verde", "Cape Verde", country),
         country = ifelse(country == "Curacao", "Curaçao", country),
         country = ifelse(country == "Czechia", "Czech Republic", country),
         country = ifelse(country == "Micronesia, Federated States of", "Federated States of Micronesia", country),
         country = ifelse(country == "Gambia, The", "Gambia", country),
         country = ifelse(country == "Guinea-Bissau", "Guinea Bissau", country),
         country = ifelse(country == "Hong Kong", "Hong Kong S.A.R.", country),
         country = ifelse(country == "Macau", "Macao S.A.R", country),
         country = ifelse(country == "North Macedonia", "Macedonia", country),
         country = ifelse(country == "Korea, South", "South Korea", country),
         country = ifelse(country == "Burma", "Myanmar", country),
         country = ifelse(country == "Korea, North", "North Korea", country),
         country = ifelse(country == "Saint Helena, Ascension, and Tristan da Cunha", "Saint Helena", country),
         country = ifelse(country == "Serbia", "Republic of Serbia", country),
         country = ifelse(country == "Eswatini", "Swaziland", country),
         country = ifelse(country == "Timor-Leste", "East Timor", country),
         country = ifelse(country == "Turkey (Turkiye)", "Turkey", country),
         country = ifelse(country == "Tanzania", "United Republic of Tanzania", country),
         country = ifelse(country == "United States", "United States of America", country))

#### Load country-level polygons and merge with migration data
# Load the shapefile data of world countries as an sf (simple feature) object
world <- ne_countries(scale = "medium", returnclass = "sf")

# Clean and prepare the world data
world <- world %>%
  select(admin, geometry) %>%     # Select only country names and geometry
  rename(country = admin) %>%     # Rename 'admin' to 'country'
  st_as_sf()                      # Convert to sf object for spatial operations

# Merge world shapefile data with the migration data, ensuring the country names match
world_joined_net_migr <- world %>%
  left_join(net_migr_rate, by = "country") %>%  # Merge data by country name
  select(-geometry, geometry) %>%              # Keep geometry as the last column
  st_as_sf()                                   # Ensure the result is an sf object

# Add a new categorical column to classify countries based on net migration rate
world_joined_net_migr <- world_joined_net_migr %>%
  mutate(net_migr_rate_categ = ifelse(migrants_per_thous_pop > 0, "More Moving In", NA),
         net_migr_rate_categ = ifelse(migrants_per_thous_pop < 0, "More Moving Out", net_migr_rate_categ),
         net_migr_rate_categ = ifelse(migrants_per_thous_pop == 0, "No Net Change", net_migr_rate_categ),
         net_migr_rate_categ = ifelse(is.na(migrants_per_thous_pop), "NA", net_migr_rate_categ))

#### Visualization

# Set up a ggplot recording environment to save images
gg_record(dir = "./3.net.migration.rate/viz_record", device = "png", width = 10, height = 7, units = "in", dpi = 320)

# Define font and adjust palette (commented out unused color palette and font functions)
f1 <- "Baskerville Old Face"  # Set the font to be used in the plot
# pal <- MetBrewer::met.brewer("Hiroshige", n = 9)  # Color palette with 9 colors (commented out)
# font_add_google(f1)   # Add the Google font (commented out)
# showtext_auto()       # Enable automatic text rendering with showtext (commented out)

# Create the plot
plt <- ggplot(data = world_joined_net_migr %>% filter(country != "Antarctica")) +
  # Plot countries with migration categorization based on the data
  geom_sf(mapping = aes(fill = net_migr_rate_categ), color = "white") +
  # Highlight countries with specific migration rates
  geom_sf(data = . %>% filter(migrants_per_thous_pop < 0), mapping = aes(fill = net_migr_rate_categ), color = "#aad8d3", linewidth = 0.1) +
  geom_sf(data = . %>% filter(migrants_per_thous_pop > 0), mapping = aes(fill = net_migr_rate_categ), color = "#d1b18f", linewidth = 0.1) +
  geom_sf(data = . %>% filter(migrants_per_thous_pop == 0), mapping = aes(fill = net_migr_rate_categ), color = "#b9a99d", linewidth = 0.1) +
  scale_size_continuous(range = c(3.2, 5)) +  # Adjust size of plotted regions
  coord_sf(crs = "+proj=eqearth +wktext", expand = FALSE) +  # Set the map projection to 'Equal Earth'
  # Set custom fill colors for the migration categories
  scale_fill_manual(values = c("NA" = "#f0f0f0", "More Moving In" = "#e8d5b7", "More Moving Out" = "#8fbfb2", "Neutral" = "#b2a8a1")) +
  # Add titles and captions with customized colors
  labs(title = "<span style='color:#4b4b4d;'>A World on the Move: Global Migration Trends in 2024</span>",
       subtitle = "<span style='color:#f0f2c0;'>More Moving In,</span><span style='color:#6f5e56;'> No Net Movement,</span><span style='color:#aad8d3;'> More Moving Out</span>",
       caption = "Source: CIA World Factbook · Graphic: Aarsh Batra"
  ) +
  theme_map() +  # Use a predefined minimal map theme
  theme(
    legend.position = "none",  # Hide the legend
    plot.background = element_rect(fill = "white", color = NA),  # Set plot background to white
    legend.text = element_text(size = 16),  # Set legend text size
    axis.text = element_blank(),  # Hide axis text
    axis.title = element_blank(),  # Hide axis titles
    plot.caption = ggtext::element_markdown(hjust = 0.5, size = 18, family = f1, margin = margin(t = 2, b = 0, unit = "cm")),
    plot.title = ggtext::element_markdown(size = 40, family = f1, hjust = 0.5, margin = margin(b = 0.5, unit = "cm")),
    plot.subtitle = ggtext::element_markdown(size = 28, family = f1, hjust = 0.5, margin = margin(b = 1.8, unit = "cm"))
  )

# adding image gradient background
plt1 <- ggimage::ggbackground(plt, "./3.net.migration.rate/images/beige_paper_gradient.png", alpha = 0.3)

# save plot
ggsave("plt_fin.pdf", plt1, width = 16, height = 10.5, dpi = 620, units = "in")

