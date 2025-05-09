#### load relevant libraries

#### load raw data
ind_bl_lev_pol_wide <- read_csv(paste0(here(), "/4.ind.bl.lev.pol.dist/raw.data/ind_block_level_pop_weighted_pol_1998_2022.csv"))

#### reshape and keep only year 2022
ind_bl_lev_pol_long_2022 <- ind_bl_lev_pol_wide %>%
  pivot_longer(cols = starts_with("avg_pm"), names_to = "pol_year", values_to = "ann_avg_pm2.5") %>%
  mutate(pol_year = as.numeric(str_remove(str_extract(pol_year, "_\\d+"), "_")))

#### create a 2022 only wide version
ind_bl_lev_pol_wide_2022 <- ind_bl_lev_pol_wide %>%
  select(pc11_state_id:subdistrict_population, avg_pm2.5_2022)

#### join 2022 wide with shrug block shp
ind_bl_lev_pol_wide_2022_shp <- ind_bl_lev_pol_wide_2022 %>%
  left_join(ind_shr_subdist_shp, by = c("pc11_state_id", "pc11_district_id",
                                        "pc11_subdistrict_id", "state_name",
                                        "district_name", "subdistrict_name")) %>%
  select(-geom, geom) %>%
  st_as_sf() %>%
  mutate(pollution_level = cut(
    avg_pm2.5_2022,
    breaks = c(0, 5, 20, 40, 80, 120),  # Set the breaks for pollution levels
    labels = c("Complying with WHO", "5 to 20", "20 to 40", "40 to 80", "80+"),  # Adjust labels to match the number of intervals
    include.lowest = TRUE,
    right = FALSE  # Open intervals on the right side, so a value of 5 falls in the second category
  )) %>%
  select(-geom, geom)


#### collapsing by pollution level
ind_bl_lev_pol_wide_2022_shp_summary <- ind_bl_lev_pol_wide_2022_shp %>%
  group_by(pollution_level) %>%
  summarise(geom = st_union(geom)) %>%
  st_simplify()



#### Define color palette (use colors that get more concerning as pollution increases)
colors_grey <- c("#FFFFFF", "#707070", "#3C3C3C", "#2B2B2B", "black")


#### load fonts
font_import()
loadfonts()
windowsFonts()

f1 <- "Baskerville Old Face"
font_add_google(f1)
showtext_auto(FALSE)


#### Plot the map with a stepped color scale
plt <- ggplot(ind_bl_lev_pol_wide_2022_shp) +
  # Add country boundary with transparent fill and thin black outline
  geom_sf(data = ind_country_shp, fill = "transparent", color = "#CCCCCC", linewidth = 0.2) +
  geom_sf(data = ind_bl_lev_pol_wide_2022_shp_summary, fill = "transparent", color = "#CCCCCC", linewidth = 0.2) +

  # Map PM2.5 data with transparent borders to avoid clutter
  geom_sf(mapping = aes(fill = avg_pm2.5_2022), color = "transparent") +

  # Define fill scale with grayscale color palette and custom breaks/labels
  scale_fill_stepsn(
    name = expression(PM[2.5] ~ "(µg/m³)"),
    colors = colors_grey,
    breaks = c(0, 5, 20, 40, 80),  # Define thresholds
    labels = c("0", "5 (WHO Guideline)", "20", "40 (National Standard)", "80"),
    limits = c(0, 110),
    oob = scales::squish
  ) +

  # Ensure map coordinates are fitted properly
  coord_sf(expand = FALSE) +

  # Add titles, subtitles, and captions
  labs(
    title = "When the Air Chokes: Can India Break Free from Pollution’s Grip?",
    subtitle = str_wrap(
      "2022 satellite-derived PM2.5 data shows all 5,969 Indian blocks, home to over 1.3 billion people, exceed the WHO annual avg PM2.5 guideline. Nearly half of these blocks, housing over 900 million people (65% of India’s population), exceed the National Standard, with 20% of this population (approx. 180 million) exposed to levels over twice that limit, more than 16 times the WHO guideline",
      width = 90
    ),
    caption = expression("Graphic · Aarsh Batra | Pol Data · ACAG, U Wash. 2022" ~ PM[2.5] ~ "Sat. Data | Shp file · SHRUG, DDL | Processed · github.com/AarshBatra/biteSizedAQ")
  ) +

  # Theme adjustments for map and text elements
  theme_void() +  # Use a blank theme to highlight map features only
  theme(
    plot.title = element_text(size = 34, family = f1, hjust = 0, color = "black", face = "bold", margin = margin(b = 0.3, t = 2.2, l = 1, r = 0.8,  unit = "cm")),  # Dark gray title
    plot.subtitle = element_text(size = 22, family = f1, hjust = 0, color = "#202020", margin = margin(b = 1, l = 1, r = 1,  unit = "cm"), face = "bold.italic"),  # Medium gray subtitle
    plot.caption = element_text(family = f1, size = 15, color = "#ffffff", margin = margin(t = 2, b = 2, l = 1, r = 1,  unit = "cm"), hjust = 0),  # Lighter gray caption
    legend.text = element_text(family = f1, size = 20, color = "#CCCCCC"),  # Mid-gray legend text
    legend.title = element_text(family = f1, size = 20, color = "#CCCCCC", hjust = 0),  # Mid-gray legend text
    legend.position.inside = c(1.8, 0.7),
    legend.position = c(1.4, 0.7),
    legend.key.height = unit(1.5, "cm"),  # Adjust height for better visibility
    legend.spacing.y = unit(0.5, "cm")
    # plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )



plt1 <- ggbackground(plt, "./4.ind.bl.lev.pol.dist/images/light_smoke_2.png")



ggsave("./plt1.pdf", plt1, width = 16, height = 10, dpi = 320)
