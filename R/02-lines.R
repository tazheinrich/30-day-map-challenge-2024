# Define location with here
here::i_am(path = "R/02-lines.R")

# Load packages
librarian::shelf(giscoR, igraph, janitor, sf, tidyverse)

# Get nodes data from orbis
raw_nodes = read_csv(curl::curl("https://stacks.stanford.edu/file/druid:mn425tz9757/orbis_nodes_0514.csv"))

# Get valid nodes
tmp <- raw_nodes %>% filter(!is.na(x) & x != 0)
lst_valid_cities <- tmp$id

nodes <- raw_nodes %>% filter(id %in% lst_valid_cities)

# Get edges data from orbis
raw_edges = read_csv(curl::curl("https://stacks.stanford.edu/file/druid:mn425tz9757/orbis_edges_0514.csv"))

# Get edges for valid nodes
edges <- raw_edges %>% filter(source %in% lst_valid_cities & target %in% lst_valid_cities)

# Convert nodes to sf object
nodes_sf <- st_as_sf(nodes, coords = c("y", "x"), crs = 4326)

# Create an igraph object from the edges data
graph <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

# Define start and end nodes
start_node <- "50325"
end_node <- "50327"

# Find the shortest path from start_node to end_node
shortest_path <- shortest_paths(graph, from = start_node, to = end_node)$vpath[[1]]

# Convert path to an edge list
path_edges <- as_edgelist(induced_subgraph(graph, shortest_path))

# Create a data frame of path edges
path_df <- data.frame(
  from = as.numeric(path_edges[, 1]),
  to = as.numeric(path_edges[, 2])
)

# Merge coordinates for each edge
path_sf <- path_df %>%
  left_join(nodes, by = c("from" = "id")) %>% 
  rename(long_from = y, lat_from = x) %>%
  left_join(nodes, by = c("to" = "id")) %>%
  rename(long_to = y, lat_to = x) %>%
  # Create a linestring geometry for each path segment
  rowwise() %>%
  mutate(geometry = st_sfc(st_linestring(matrix(c(long_from, lat_from, long_to, lat_to), ncol = 2, byrow = TRUE)))) %>%
  ungroup() %>%
  st_as_sf(crs = 4326)

# Define bounding box coordinates for filtering
min_long <- -11
max_long <- 35
min_lat <- 30
max_lat <- 60

# Merge edges with nodes to get coordinates for each edge
edges_sf <- edges %>%
  left_join(nodes, by = c("source" = "id")) %>%
  rename(long_from = y, lat_from = x) %>%
  left_join(nodes, by = c("target" = "id")) %>%
  rename(long_to = y, lat_to = x) %>%
  rowwise() %>%
  # Create an sf linestring geometry for each edge
  mutate(geometry = st_sfc(st_linestring(matrix(c(long_from, lat_from, long_to, lat_to), ncol = 2, byrow = TRUE)))) %>%
  ungroup() %>%
  st_as_sf(crs = 4326)

# Filter nodes and edges based on bounding box
filtered_nodes <- nodes_sf %>%
  filter(st_coordinates(.)[, 1] >= min_long & st_coordinates(.)[, 1] <= max_long &
           st_coordinates(.)[, 2] >= min_lat & st_coordinates(.)[, 2] <= max_lat)

filtered_edges <- edges_sf %>%
  filter(long_from >= min_long & long_from <= max_long &
           lat_from >= min_lat & lat_from <= max_lat &
           long_to >= min_long & long_to <= max_long &
           lat_to >= min_lat & lat_to <= max_lat)


# Get world map
sf_world <- gisco_get_countries(resolution = 20,
                                 epsg = "4326")

# Create plot
p <- ggplot() +
  geom_sf(data = sf_world, color = "dimgrey", fill = "grey19", linewidth = 1) + 
  geom_sf(data = filtered_edges, aes(geometry = geometry), color = "lightgrey", linewidth = 0.5) + 
  geom_sf(data = path_sf, aes(geometry = geometry), color = "cyan", linewidth = 1) +
  geom_sf(data = nodes_sf %>% filter(id %in% as.numeric(shortest_path)), aes(geometry = geometry), color = "cyan", size = 3) +
  coord_sf(xlim = c(min_long, max_long), ylim = c(min_lat, max_lat)) + 
  labs(title = "Travelling on the Roads of the Roman Empire:\nFrom Castra Regina to Rome",
       subtitle = "by Tassilo Heinrich | Code at github.com/tazheinrich",
       caption= "Data: Orbis, Eurostat/GISCO",
       x = NULL, 
       y = NULL) +  
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.title = element_blank(),         
    axis.text = element_blank(),          
    axis.ticks = element_blank(),          
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.title = element_text(color = "lightgrey"),    
    plot.subtitle = element_text(color = "lightgrey"), 
    plot.caption = element_text(color = "lightgrey")
  )

# Save plot
ggsave(filename = "02-lines-roads-roman-empire.png",
       plot = p, 
       path = "output",
       dpi = 300,
       width = 8,
       height = 10,
       scale = 1)

