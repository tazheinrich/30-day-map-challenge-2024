# Load required libraries
librarian::shelf(osmdata, rnaturalearth, rnaturalearthdata, sf, showtext, sp, tidyverse)

# Make fonts available
showtext::showtext_auto()
font_add_google("Roboto", "roboto")

# Define area of Germany and load its borders
germany_border <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf")

# Project Germany's borders to an equal-area CRS (EPSG:3035)
germany_border_proj <- st_transform(germany_border, crs = 3035)

# Define the bounding box of Germany in the projected CRS
germany_bbox_proj <- st_bbox(germany_border_proj)

# Get pharmacy data with a timeout and extract points
germany_bbox <- getbb("Germany")
pharmacies <- opq(bbox = germany_bbox, timeout = 120) %>%
  add_osm_feature(key = "amenity", value = "pharmacy") %>%
  osmdata_sf()

# Extract and project points
pharmacy_points <- pharmacies$osm_points
pharmacy_points_proj <- st_transform(pharmacy_points, crs = 3035)

# Create a hexagonal grid in the projected CRS
hex_grid_proj <- st_make_grid(
  pharmacy_points_proj, 
  cellsize = 20000,  # Adjust this size as needed (in meters for EPSG:3035)
  square = FALSE,
  what = "polygons"
)

# Convert the hexagonal grid to an sf-object
hex_sf_proj <- st_sf(geometry = hex_grid_proj)

# Clip the hexagonal grid to the German borders
hex_sf_clipped_proj <- st_intersection(hex_sf_proj, germany_border_proj)

# Perform spatial join to count pharmacies in each hexagon
hex_density_proj <- st_join(hex_sf_clipped_proj, pharmacy_points_proj) %>%
  group_by(geometry) %>%
  summarise(count = n())

# Replace NA values with 0
hex_density_proj$count[is.na(hex_density_proj$count)] <- 0

# Plot the density map with ggplot2
# Load the required libraries
library(ggplot2)
library(viridis)

# Create the density map with the specified modifications
p <- ggplot() +
  geom_sf(data = hex_density_proj, aes(fill = count), color = "#d3cbc1", size = 0.5) +  # Hexagon borders in grey
  geom_sf(data = germany_border_proj, fill = NA, color = "#cbc3b7", size = 0.5) +
  scale_fill_gradient(low = "#f0eeea", high = "red", name = NULL) +  # Custom color palette
  labs(
    title = "Density of Pharmacies in Germany",
    subtitle = "by Tassilo Heinrich | Code at github.com/tazheinrich",
    caption = "Data: OpenStreetMap") +
  theme_minimal(base_size = 40) + 
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.title = element_blank(),         
    axis.text = element_blank(),          
    axis.ticks = element_blank(),   
    plot.background = element_rect(fill = "#d3cbc1", color = NA),  
    panel.background = element_rect(fill = "#d3cbc1", color = NA),
    legend.key = element_rect(fill = "#f0eeea", color = NA),
    legend.background = element_rect(fill = "#d3cbc1", color = "#d3cbc1"),
    legend.text = element_text(color = "black", family = "roboto"),
    plot.title = element_text(color = "black", family = "roboto"),    
    plot.subtitle = element_text(color = "black", family = "roboto"), 
    plot.caption = element_text(color = "black",  family = "roboto")
  ) 

# Save plot
ggsave(filename = "04-hexagons-pharmacies-in-germany.png",
       plot = p, 
       path = "output",
       dpi = 300,
       width = 8,
       height = 10,
       scale = 1)  

