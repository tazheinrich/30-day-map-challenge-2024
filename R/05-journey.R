# Load packages
librarian::shelf(osmdata, rnaturalearth, rnaturalearthdata, rvest, sf, tidygeocoder, tidyverse)

# Define url
url <- "https://en.wikipedia.org/wiki/2024_ATP_Tour"

# Read HTML content
html <- read_html(url)

html %>% 
  html_elements(".wikitable") %>% 
  html_table() %>% 
  purrr::map(as.data.frame) -> wikitables

# Select tables
wikitables <- wikitables[2:13]

# Create dataframe
tournament_calendar <- bind_rows(wikitables) %>% 
  select(Week, Tournament)

# Function to split the string at the first capital letter not preceded by a space
split_at_first_capital <- function(event_string) {
  # Check for the exception "'s-Hertogenbosch"
  if (grepl("'s-Hertogenbosch", event_string)) {
    # If found, modify the string to retain the apostrophe during the split
    event_string <- sub("('s-Hertogenbosch),", "\\1,", event_string)  # Keep the apostrophe with Hertogenbosch
    parts <- strsplit(event_string, "'", fixed = TRUE)[[1]]
    if (length(parts) > 1) {
      return(list(tmp1 = parts[1], tmp2 = parts[2]))
    }
  }
  
  # Check for the exception for "IIMarrakesh"
  if (grepl("IIMarrakesh", event_string)) {
    # If found, modify the string to retain "II" during the split
    event_string <- sub("(II)(Marrakesh),", "\\1, \\2", event_string)  # Keep "II" on the left side
    parts <- strsplit(event_string, ", ", fixed = TRUE)[[1]]
    if (length(parts) > 1) {
      return(list(tmp1 = parts[1], tmp2 = parts[2]))
    }
  }
  
  # Find the position of the first capital letter that is not preceded by a space, ignoring the first letter, dots and hyphens
  match_pos <- str_locate(event_string, "(?<![\\s.-])(?<![A-Z])(?=[A-Z])(?!^)")
  
  # If no match is found, return the original string in tmp1 and NA in tmp2
  if (is.na(match_pos[1])) {
    return(list(tmp1 = event_string, tmp2 = NA))
  }
  
  # Split the string at the position found
  tmp1 <- substr(event_string, 1, match_pos[1] - 1)
  tmp2 <- substr(event_string, match_pos[1], nchar(event_string))
  
  return(list(tmp1 = tmp1, tmp2 = tmp2))
}

# Clean Data
tournament_calendar <- tournament_calendar %>% 
  mutate(start_date = str_extract(Week, "^[^ ]+ [A-Za-z]{3}"),
         start_date = paste0(start_date, " 2024"),
         start_date = dmy(start_date)) %>% 
  mutate(category = str_extract(Tournament, "(ATP \\d+|ATP Masters \\d+|Grand Slam|ATP Finals)")) %>% 
  mutate(surface = str_extract(Tournament, "(Clay|Grass|Hard)")) %>%
  mutate(country = str_extract(Tournament, "(?<=,\\s)(.*?)(?=ATP|Grand|United Cup)")) %>%
  rowwise() %>%
  mutate(tmp = list(split_at_first_capital(Tournament)),
         event = tmp$tmp1,
         city_tmp = tmp$tmp2) %>%
  mutate(city = ifelse(grepl("Marrakesh", city_tmp),
                       sub("Marrakesh.*", "Marrakesh", city_tmp),
                       str_extract(city_tmp, "^[^,]+"))) %>%
  mutate(rest = ifelse(grepl("Marrakesh", city_tmp),
                       sub(".*?Marrakesh,?\\s*", "", city_tmp),
                       str_extract(city_tmp, ", (.+)"))) %>%
  separate_rows(city, sep = "/") %>%
  mutate(category = if_else(event == "United Cup", "ATP 500", category)) %>%
  mutate(category_num = case_when(category == "ATP 250" ~ 250,
                                  category == "ATP 500" ~ 500,
                                  category == "ATP Masters 1000" ~ 1000,
                                  category == "ATP Finals" ~ 1500,
                                  category == "Grand Slam" ~ 2000)) %>% 
  filter(!is.na(country) & event != "Next Gen ATP Finals") %>%
  select(-Week, -tmp, -city_tmp, -rest, -Tournament) %>%
  distinct()

# Get coordinates
tournament_calendar <- tournament_calendar %>% 
  mutate(address = paste(city, country, sep = ", ")) %>% 
  geocode(address = address, 
          method = "osm",
          lat = latitude, 
          long = longitude,
          timeout = 420) %>% 
  # For some weird reason I don't get coordinates for Eastbourne
  mutate(latitude = if_else(is.na(latitude) & city == "Eastbourne" & country == "Great Britain", 50.768, latitude),
         longitude = if_else(is.na(longitude) & city == "Eastbourne" & country == "Great Britain", 0.284, longitude))

# Convert data to sf object
tournament_calendar_sf <- tournament_calendar %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 3395)

# Arrage the data by start_data
tournament_calendar_sf <- tournament_calendar_sf %>% 
  arrange(start_date)

# Get unique dates
unique_dates <- unique(tournament_calendar_sf$start_date)

# Initialize new dataframe
event_lines <- data.frame()

# Loop through unique dates to connect events on one date to the next date
for (i in 1:(length(unique_dates) - 1)) {
  current_date <- unique_dates[i]
  next_date <- unique_dates[i + 1]
  
  # Filter events for current and next date
  current_events <- tournament_calendar_sf %>% filter(start_date == current_date)
  next_events <- tournament_calendar_sf %>% filter(start_date == next_date)
  
  # Create all combinations of current and next events
  if (nrow(current_events) > 0 & nrow(next_events) > 0) {
    for (j in 1:nrow(current_events)) {
      for (k in 1:nrow(next_events)) {
        line_geom <- st_sfc(st_linestring(rbind(
          st_coordinates(current_events[j, ]), 
          st_coordinates(next_events[k, ])
        )), crs = st_crs(tournament_calendar_sf))  # Preserve the CRS
        
        # Combine into a data frame
        event_lines <- rbind(event_lines, st_sf(geometry = line_geom))
      }
    }
  }
}


# Assuming event_lines is an sf object with geometry
# Convert geometry to a data frame to extract coordinates
event_lines_df <- event_lines %>%
  st_transform(crs = st_crs(tournament_calendar_sf)) %>%  # Ensure same CRS
  st_coordinates() %>%
  as.data.frame() %>%
  group_by(L1) %>%
  summarize(
    x = first(X), 
    y = first(Y), 
    xend = last(X), 
    yend = last(Y)
  )

# Gett world map
world <- ne_countries(scale = 50, returnclass = "sf") %>% 
  filter(name != "Antarctica")


# Create the plot
# Define colors
border_colors <- c("#355D34", "#3D2273", "#377DB8")
fill_colors <- c("#C23B22", "#046a38", "#1E8FD5")

# Create a named vector for border colors based on surface categories
border_color_mapping <- setNames(border_colors, levels(tournament_calendar_sf$surface))
fill_color_mapping <- setNames(fill_colors, levels(tournament_calendar_sf$surface))

# Create the plot
p <- ggplot() +
  geom_sf(data = world, fill = "#ccbcaf", color = "#f0edea", linewidth = 1) +
  geom_sf(data = tournament_calendar_sf, 
          aes(geometry = geometry, 
              color = surface, 
              size = category_num,
              fill = surface),
          shape = 21,
          stroke = 1.2) + 
  geom_segment(data = event_lines_df, 
               aes(x = x, y = y, xend = xend, yend = yend), 
               color = "#334350", 
               size = .4) + 
  scale_size_continuous(range = c(2, 6)) +
  scale_color_manual(values = border_color_mapping) +
  scale_fill_manual(values = fill_color_mapping) + 
  guides(color = "none",
         fill = guide_legend(override.aes = list(size = 4,
                                                 color = border_color_mapping,
                                                 fill = fill_color_mapping),
                             title.position = "top",
                             title.hjust = 0.5),
         size = guide_legend(override.aes = list(color = "#c0c0c0",
                                                 fill = "#f0edea"),
                             title.position = "top",
                             title.hjust = 0.5)) +
  labs(title = "The Roads to the NITTO ATP Finals in Turin 2024",
       fill = "Surface",
       size = "Points for Winning",
       subtitle = "by Tassilo Heinrich | Code at github.com/tazheinrich",
       caption = "Data: ATP, OpenStreetMap, Wikipedia") +
  theme_minimal(base_size = 60) + 
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.title = element_blank(),         
    axis.text = element_blank(),          
    axis.ticks = element_blank(),   
    plot.background = element_rect(fill = "#f0edea", color = "#f0edea"),  
    panel.background = element_rect(fill = "#f0edea", color = "#f0edea"),
    legend.key = element_rect(fill = "#f0edea", color = "#f0edea"),
    legend.background = element_rect(fill = "#f0edea", color = "#f0edea"),
    legend.text = element_text(color = "black", family = "roboto"),
    legend.position = "bottom",  # Position legends at the bottom
    legend.box = "horizontal",    # Arrange legends side by side
    plot.title = element_text(color = "black", family = "roboto"),    
    plot.subtitle = element_text(color = "black", family = "roboto"), 
    plot.caption = element_text(color = "black", family = "roboto"),
    plot.margin = margin(10, 10, 10, 10)  # Reduce margins
  ) +
  coord_sf(crs = st_crs(3395), expand = TRUE) 


# Save plot
ggsave(filename = "05-journey-roads-to-atp-finals-2024.png",
       plot = p, 
       path = "output",
       dpi = 300,
       width = 16,
       height = 16,
       scale = 1)  
