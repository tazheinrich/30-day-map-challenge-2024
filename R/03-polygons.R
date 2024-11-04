# Load required libraries
librarian::shelf(ggspatial, osmdata, tidyverse, sf, showtext)

# Make fonts available
showtext::showtext_auto()
font_add_google("Roboto", "roboto")

# Define the location and feature
munich_bbox <- getbb("Munich, Germany") 

# Fetch park data as polygons
parks <- opq(bbox = munich_bbox) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()
parks_sf <- parks$osm_polygons

# Fetch the boundary of Munich
munich_city_boundary <- opq(bbox = munich_bbox) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "admin_level", value = "9") %>%
  osmdata_sf()
munich_city_boundary <- munich_city_boundary$osm_multipolygons

# Check if boundary data is available
if (is.null(munich_city_boundary) || nrow(munich_city_boundary) == 0) {
  stop("Munich boundary data could not be retrieved.")
}

# Fetch building data within Munich
buildings <- opq(bbox = munich_bbox) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()
buildings_sf <- buildings$osm_polygons

# Set CRS to WGS84 (EPSG:4326) if not already set
if (is.na(st_crs(parks_sf))) {
  st_crs(parks_sf) <- 4326
}
if (is.na(st_crs(munich_city_boundary))) {
  st_crs(munich_city_boundary) <- 4326
}
if (is.na(st_crs(buildings_sf))) {
  st_crs(buildings_sf) <- 4326
}

# Ensure all layers have the same CRS
parks_sf <- st_transform(parks_sf, st_crs(munich_city_boundary))
buildings_sf <- st_transform(buildings_sf, st_crs(munich_city_boundary))

# Clip parks and buildings to the Munich boundary
parks_within_munich <- st_intersection(parks_sf, munich_city_boundary)
buildings_within_munich <- st_intersection(buildings_sf, munich_city_boundary)

# Plot the map with buildings and parks
p <- ggplot() +
  geom_sf(data = buildings_within_munich, fill = "lightgrey", color = NA) +      
  geom_sf(data = munich_city_boundary, fill = NA, color = "grey20", size = 1) +   
  geom_sf(data = parks_within_munich, fill = "forestgreen", color = "darkgreen", size = 0.3) +
  labs(title = "Parks vs. Buildings in Munich",
       subtitle = "by Tassilo Heinrich | Code at github.com/tazheinrich",
       caption = "Data: OpenStreetMap") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.title = element_blank(),         
    axis.text = element_blank(),          
    axis.ticks = element_blank(),          
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.title = element_text(color = "lightgrey", family = "roboto", size = 40),    
    plot.subtitle = element_text(color = "lightgrey", family = "roboto", size = 32), 
    plot.caption = element_text(color = "lightgrey",  family = "roboto", size = 20)
  )

# Save plot
ggsave(filename = "03-polygons-parks-in-munich.png",
       plot = p, 
       path = "output",
       dpi = 300,
       width = 5,
       height = 5,
       scale = 1)
