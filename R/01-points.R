# Define location with here
here::i_am("R/01-points.R")

# Load packages
librarian::shelf(
  ggrepel, # Customize labels
  ggtext, # Style labels
  giscoR, # Get geographic data
  here,  # Manage filepaths
  janitor,  # Clean data
  scales, # Format scales
  sf, # Handle shapefiles
  showtext, # Use modern fonts
  tidyverse # Manipulate data
  )

# Make fonts available
showtext::showtext_auto()
font_add_google("Roboto", "roboto")

# Load map of Germany
shp_ger <- giscoR::gisco_get_nuts(
  country = "Germany", 
  nuts_level = 1, 
  resolution = "20", 
  epsg = "4326")

# Load landings data
# Source: https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh/data_preview
df_meteorites<- read_csv(file = "data/Meteorite_Landings_20241031.csv") %>% 
  janitor::clean_names() %>% 
  drop_na(reclong, reclat, mass_g)

# Convert landings data to shapefile
shp_meteorites <- st_as_sf(df_meteorites, 
                           coords = c("reclong", "reclat"), 
                           crs = 4326) %>% 
  drop_na()

# Filter for landings in GErmany
shp_meteorites_ger <-st_join(shp_meteorites, shp_ger, join = st_intersects) %>% 
  drop_na()

# Filter for specific meteorites
tmp_largest <- shp_meteorites_ger %>% 
  filter(mass_g == max(mass_g, na.rm = TRUE))

tmp_smallest <- shp_meteorites_ger %>% 
  filter(mass_g == min(mass_g, na.rm = TRUE))

tmp_oldest <- shp_meteorites_ger %>% 
  filter(year == min(year, na.rm = TRUE))

tmp_youngest <- shp_meteorites_ger %>% 
  filter(year == max(year, na.rm = TRUE))

# Create labels
df_meteorite_labels <- bind_rows(
  tmp_largest %>% mutate(label = paste("<b>Largest Meteorite</b><br><br>",
                                       "<b>Location:</b> ", name, "<br>",
                                       "<b>Found:</b> ", year, "<br>", 
                                       "<b>Weight:</b> ", comma(mass_g), "g")),
  tmp_smallest %>% mutate(label = paste("<b>Smallest Meteorite</b><br><br>",
                                        "<b>Location:</b> ", name, "<br>", 
                                        "<b>Found:</b> ", year, "<br>",
                                        "<b>Weight:</b> ", comma(mass_g), "g")),
  tmp_oldest %>% mutate(label = paste("<b>Oldest Meteorite</b><br><br>", 
                                      "<b>Location:</b> ", name, "<br>",
                                      "<b>Found:</b> ", year, "<br>", 
                                      "<b>Weight:</b> ", comma(mass_g), "g")),
  tmp_youngest %>% mutate(label = paste("<b>Youngest Meteorite</b><br><br>", 
                                        "<b>Location:</b> ", name, "<br>",
                                        "<b>Found:</b> ", year, "<br>",
                                        "<b>Weight:</b> ", comma(mass_g), "g"))
)
# Remove temporary files
rm(list = ls(pattern = "^tmp_"))

# Add original coordinates for connecting lines
df_meteorite_labels <- df_meteorite_labels %>% 
  mutate(
    original_x = st_coordinates(geometry)[, 1],  
    original_y = st_coordinates(geometry)[, 2], 
    label_x = st_coordinates(geometry)[, 1] + c(-1.7, -3.7, -1.5, 2.5),
    label_y = st_coordinates(geometry)[, 2] + c(0, 1.8, 0, 0)
  )

# Calculate limits with a buffer
x_buffer <- 2 # Adjust as necessary
y_buffer <- 0  # Adjust as necessary


# Plot shapefile
p <- ggplot() +
  geom_sf(data = shp_ger,
          fill = "grey95",
          color = "grey20",
          linewidth = 0.3) +
  geom_segment(data = df_meteorite_labels,
               aes(x = original_x,
                   y = original_y,
                   xend = label_x,
                   yend = label_y),
               color = "black",  
               size = 0.3)  +
  geom_richtext(data = df_meteorite_labels,
                aes(x = label_x,
                    y = label_y,
                    label = label),
                color = "black",
                fill = "white",
                size = 5,
                label.padding = unit(0.2, "lines"),
                lineheight = 0.4,
                label.size = 0.4) +
  geom_sf(data = shp_meteorites_ger,
          shape  = 21,
          color = "black",
          fill = "red",
          aes(size = log(mass_g))) +
  scale_size_continuous(range = c(1, 4),
                        name = "Log Mass (g)",
                        breaks = log(c(100, 1000, 100000, 1000000)),
                        labels = c("100g", "1,000g", "100,000g", "1,000,000g"),
                        guide = guide_legend(
                          title.position = "top",
                          title.theme = element_text(face = "bold", family = "roboto"),
                          direction = "horizontal"
                        )) +
  coord_sf(xlim = c(st_bbox(shp_ger)[1] - x_buffer, st_bbox(shp_ger)[3] + x_buffer),
           ylim = c(st_bbox(shp_ger)[2] - y_buffer, st_bbox(shp_ger)[4] + y_buffer)) +
  labs(title = "Meteorite Landings in Germany",
       subtitle = "by Tassilo Heinrich | Code at github.com/tazheinrich",
       caption= "Data: Eurostat- GISCO, The Meteoritical Society/NASA",
       x = NULL, 
       y = NULL) +   
  theme_minimal(base_family = "roboto",
                base_size = 20) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        plot.background = element_rect(fill = "grey90", color = NA), 
        legend.position = "bottom",
        legend.margin = margin(t = -20, unit = "pt"),
        legend.box.margin = margin(t = 10, unit = "pt"),
        legend.title = element_text(hjust = 0.5, face = "bold", size = 26, family = "roboto"),
        legend.text = element_text(size = 24, family = "roboto"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 40, family = "roboto"),
        plot.subtitle = element_text(hjust = 0.5, size = 32, color = "grey30", family = "roboto"),
        plot.caption = element_text(size = 20, family = "roboto", color = "grey30"))

# Save plit
ggsave(filename = "01-points-meteorite-landings-germany.png",
       plot = p, 
       path = "output",
       dpi = 300,
       width = 5,
       height = 5/7*10,
       scale = 5/7)