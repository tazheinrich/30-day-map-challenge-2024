# Load libraries
librarian::shelf(here, sf, tidyverse, ggpattern, ggspatial, ggfx, ggtext)

# Load administrative shapefile for Singapore
shp <- st_read(here("input/gadm41_SGP_shp/gadm41_SGP_1.shp"))
shp <- st_transform(shp, crs = 4326)
shp <- st_make_valid(shp)

# Define patterns and a vintage-style beige palette
patterns <- seq(from = 1, to = 5, by = 1)
colors <- c("#d4b59c", "#c9a06d", "#b78c54", "#a77a3f", "#967239")

# Assign a pattern and color to each district
shp$pattern <- rep(patterns, length.out = nrow(shp))
shp$color <- rep(colors, length.out = nrow(shp))

# Calculate centroids for labeling
shp <- shp %>% mutate(centroid = st_centroid(geometry))

# Extract coordinates for centroids
centroids <- st_coordinates(shp$centroid) %>%
  as.data.frame() %>%
  mutate(NAME_1 = shp$NAME_1)

# Create the map
p <- ggplot() +
  # Add shadow effect using ggfx
  with_shadow(
    geom_sf_pattern(
      data = shp,
      aes(fill = color, pattern_type = NAME_1),  # Map patterns directly here
      pattern = "magick",                        # Use "magick" as pattern type for image manipulation
      pattern_fill = "black",                    # Pattern fill color
      pattern_density = 0.2,                     # Pattern density
      pattern_angle = 45,                        # Pattern angle
      color = "black",                           # Border color
      size = 0.5                                 # Border thickness
    ),
    colour = "grey50", x_offset = 3, y_offset = 3, sigma = 2
  ) +
  scale_fill_identity() +
  scale_pattern_type_discrete(choices = gridpattern::names_magick) +
  geom_textbox(
    data = centroids,
    aes(x = X, y = Y, label = NAME_1),
    color = "#4e3629",           
    fill = "white",              
    box.color = NA,              
    box.r = unit(3, "pt"),       
    size = 3.5,                  
    family = "serif",
    halign = 0.5,                
    valign = 0.5                 
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    text_col = "black",
    line_col = "black"
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(fill = c("#4e3629", "#d4b59c"))
  ) +
  labs(
    title = "Vintage Map of Singapore Administrative Districts",
    subtitle = "by Tassilo Heinrich | Code at github.com/tazheinrich",
    caption = "Data: GADM"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.background = element_rect(fill = "#f7f4e8", color = NA),
    panel.background = element_rect(fill = "#f7f4e8", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5, color = "#4e3629", family = "serif"),
    plot.subtitle = element_text(size = 32, hjust = 0.5, color = "#4e3629", family = "serif"),
    plot.caption = element_text(size = 20, color = "#4e3629", family = "serif"),
    axis.title = element_blank(), 
    axis.text = element_text(color = "#4e3629", family = "serif"),  
    panel.grid.major = element_line(color = "#d0c6b0", linewidth = 0.5),
    panel.grid.minor = element_line(color = "#d0c6b0", linewidth = 0.25), 
    legend.position = "none",
  )

# Save plot
ggsave(filename = "07-vintage-singapore.png",
       plot = p, 
       path = "output",
       dpi = 300,
       width = 16,
       height = 9,
       scale = 1)
