# Load the packages
library(osmdata)
library(sf)
library(ggplot2)
library(elevatr)
library(raster)
library(ggspatial)
library(showtext)

# Make fonts available
showtext::showtext_auto()
font_add_google("Roboto", "roboto")

# Define the bounding box for the Danube region
bbox <- c(8, 42, 31, 50) 

# Query OSM for the Danube River
danube_osm <- opq(bbox = bbox, timeout = 360) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  add_osm_feature(key = "name", value = "Danube") %>%
  osmdata_sf()

# Extract the lines for the Danube
danube_river <- danube_osm$osm_lines

# Define the locations as an sf object for fetching elevation data
locations <- data.frame(x = c(8, 31), y = c(42, 50))
locations_sf <- st_as_sf(locations, coords = c("x", "y"), crs = 4326)

# Fetch elevation raster data for the bounding box
elevation_raster <- get_elev_raster(locations_sf, z = 7, clip = "bbox")

# Project the Danube and elevation data
danube_river_utm <- st_transform(danube_river, crs = 4326)
elevation_raster_utm <- projectRaster(elevation_raster, crs = "EPSG:4326")

# Convert the projected raster to a data
elevation_df_utm <- as.data.frame(rasterToPoints(elevation_raster_utm), xy = TRUE)
colnames(elevation_df_utm) <- c("lon", "lat", "elevation")

# Plot the data
p <- ggplot() +
  geom_raster(data = elevation_df_utm, aes(x = lon, y = lat, fill = elevation)) +
  scale_fill_gradient(low = "#1c1c1c", high = "lightgreen") +
  # Overlay the Danube River with a thicker black line
  geom_sf(data = danube_river_utm, color = "#1c1c1c", linewidth = 1.5) +
  labs(title = "Elevation Map of the Danube River and Its Surrounding Area",
       subtitle = "by Tassilo Heinrich | Code at github.com/tazheinrich",
       caption = "Data: OpenStreetMap") +
  theme_minimal(base_family = 60) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.title = element_blank(),         
    axis.text = element_blank(),          
    axis.ticks = element_blank(),   
    plot.background = element_rect(fill = "#f0edea", color = "#f0edea"),  
    panel.background = element_rect(fill = "#f0edea", color = "#f0edea"),
    legend.position = "none",  # Position legends at the bottom
    plot.title = element_text(color = "#1c1c1c", family = "roboto", size = 60),    
    plot.subtitle = element_text(color = "#1c1c1c", family = "roboto", size = 40), 
    plot.caption = element_text(color = "#1c1c1c", family = "roboto", size = 30),
    plot.margin = margin(10, 10, 10, 10)  # Reduce margins
  ) +
  # Clip the areas outside the Danube region (seas and Mediterranean) with a mask
  coord_sf(xlim = c(9.4, 29), ylim = c(43.7, 49)) 


# Save plot
ggsave(filename = "06-raster-danube.png",
       plot = p, 
       path = "output",
       dpi = 300,
       width = 16,
       height = 9,
       scale = 1)  
