# what international routes to Europe and Asia did American fly in the 90s, but not as of 2023?

#source('~/GitHub/Airlines/2025 Route History Analysis/20250510 Route History.R')

library(sf)
library(rnaturalearth)
library(tidyverse)
library(ggrepel)
library(janitor)

set.seed(123)

airline_var <- 'AA'

# international routes flown in the 90s
flights_90s <- history |>
  filter(carrier %in% c(airline_var),
         year <= 2000,
         international_service == 1,
         seats > 0) |>
  left_join(airportr::airports, by = c("origin" = "IATA")) |>
  rename(origin_lat = Latitude, origin_lon = Longitude) |>
  left_join(airportr::airports, by = c("dest" = "IATA")) |>
  rename(dest_lat = Latitude, dest_lon = Longitude) |>
  clean_names() |>
  filter(grepl('Asia',timezone_y) | grepl('Europe',timezone_y) | grepl('Africa',timezone_y)) |>
  select(carrier,
         origin,
         dest,
         origin_lat,
         origin_lon,
         dest_lat,
         dest_lon) |>
  distinct() |>
  dplyr::filter(!is.na(origin_lon), !is.na(origin_lat), !is.na(dest_lon), !is.na(dest_lat))

flights_23 <- history |>
  filter(carrier %in% c(airline_var),
         year == 2023,
         international_service == 1,
         seats > 0) |>
  left_join(airportr::airports, by = c("origin" = "IATA")) |>
  rename(origin_lat = Latitude, origin_lon = Longitude) |>
  left_join(airportr::airports, by = c("dest" = "IATA")) |>
  rename(dest_lat = Latitude, dest_lon = Longitude) |>
  clean_names() |>
  filter((grepl('Asia',timezone_y) | grepl('Europe',timezone_y))) |>
  select(carrier,
         origin,
         dest) |>
  distinct()

flights_from_90s <- flights_90s |>
  anti_join(flights_23)

history |>
  filter(carrier %in% c(airline_var),
         #year <= 2000,
         international_service == 1,
         seats > 0) |>
  left_join(airportr::airports, by = c("origin" = "IATA")) |>
  rename(origin_lat = Latitude, origin_lon = Longitude) |>
  left_join(airportr::airports, by = c("dest" = "IATA")) |>
  rename(dest_lat = Latitude, dest_lon = Longitude) |>
  clean_names() |>
  filter(grepl('Asia',timezone_y) | grepl('Europe',timezone_y)) |>
  anti_join(flights_23) |>
  inner_join(flights_90s) |>
  select(carrier,
         origin,
         dest,
         year) |>
  arrange(carrier, origin, dest, year) %>%
  group_by(carrier, origin, dest) %>%
  mutate(
    year = as.integer(year),
    gap = year - lag(year, default = first(year)),
    grp = cumsum(gap != 1)
  )%>%
  group_by(carrier, origin, dest, grp) %>%
  summarise(
    start = min(year),
    end = max(year),
    .groups = "drop"
  )%>%
  mutate(
    streak = ifelse(start == end, as.character(start), paste0(start, "-", end))
  ) %>%
  group_by(carrier, origin, dest) %>%
  summarise(service_streaks = paste(streak, collapse = ", "),
            last_year = max(end),
            .groups = "drop"
  ) %>%
  arrange(desc(last_year),origin) |>
  as.data.frame()
  

# Define LAEA projection centered on the Americas
laea_proj <- "+proj=laea +lat_0=55 +lon_0=30 +datum=WGS84 +units=m +no_defs"

# Load and reproject world data
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(laea_proj)

# Bounding box in projected coordinates (meters)
bbox <- st_bbox(c(
  xmin = -7300000,   
  xmax = 7500000,   
  ymin = -1000000,   
  ymax = 9000000     
), crs = laea_proj)
world_crop <- st_crop(world, bbox)

make_arch_curve <- function(p1, p2, arc_height = 0.3, direction = 1, n = 100) {
  # Midpoint
  mx <- (p1[1] + p2[1]) / 2
  my <- (p1[2] + p2[2]) / 2
  
  # Perpendicular vector for arch
  dx <- p2[1] - p1[1]
  dy <- p2[2] - p1[2]
  perp <- c(-dy, dx)
  norm <- sqrt(sum(perp^2))
  if (norm == 0) norm <- 1
  perp <- perp / norm
  
  # Apply direction (1 = up, -1 = down)
  control <- c(mx, my) + direction * arc_height * perp * sqrt(dx^2 + dy^2)
  
  # Generate Bezier curve
  bez_coords <- bezier::bezier(t = seq(0, 1, length.out = n), 
                               p = rbind(p1, control, p2))
  return(bez_coords)
}

routes <- flights_from_90s |>
  select(origin, dest, origin_lon, origin_lat, dest_lon, dest_lat) |>
  pmap_dfr(function(origin, dest, origin_lon, origin_lat, dest_lon, dest_lat) {
    p1 <- st_transform(st_sfc(st_point(c(origin_lon, origin_lat)), crs = 4326), laea_proj) |> st_coordinates()
    p2 <- st_transform(st_sfc(st_point(c(dest_lon, dest_lat)), crs = 4326), laea_proj) |> st_coordinates()
    
    # Flip up/down based on longitudes
    direction <- sample(c(-1, 1), 1)
    
    coords <- make_arch_curve(p1, p2, arc_height = 0.43, direction = direction)
    
    st_linestring(coords) |>
      st_sfc(crs = laea_proj) |>
      st_sf(route = paste(origin, "to", dest), geometry = _)
  })

city_points <- flights_from_90s |>
  select(city = origin, lon = origin_lon, lat = origin_lat) |>
  bind_rows(
    flights_from_90s |> select(city = dest, lon = dest_lon, lat = dest_lat)
  ) |>
  distinct() 

# Convert to sf points
cities_sf <- st_as_sf(city_points, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(laea_proj)

cities_label_df <- cities_sf |>
  mutate(
    coords = st_coordinates(geometry),
    x = coords[,1],
    y = coords[,2]
  ) |>
  left_join(airportr::airports,
            by = c('city' = 'IATA')) |>
  select(city,
         geometry,
         coords,
         x,
         y,
         City) |>
  mutate(City = case_when(city == 'NRT' ~ 'Tokyo-Narita',
                          city == 'LGW' ~ 'London-Gatwick',
                          city == 'LHR' ~ 'London-Heathrow',
                          city == 'STN' ~ 'London-Stansted',
                          city == 'ORD' ~ "Chicago-O'Hare",
                          city == 'ORY' ~ 'Paris-Orly',
                          city == 'CDG' ~ 'Paris-de Gaulle',
                          city == 'KIX' ~ 'Osaka-Kansai',
                          city == 'SVO' ~ 'Moscow-Sheremetyevo',
                          city == 'LGA' ~ 'New York-LaGuardia',
                          city == 'JFK' ~ 'New York-JFK',
                          city == 'IAD' ~ 'Washington-Dulles',
                          city == 'MXP' ~ 'Milan-Malpensa',
                          city == 'GUM' ~ 'Guam',
                          city == 'TXL' ~ 'Berlin-Tegel',
                          city == 'PEK' ~ 'Beijing-Capital',
                          city == 'PVG' ~ 'Shanghai-Pudong',
                          city == 'FCO' ~ 'Rome-Fiumicino',
                          city == 'RDU' ~ 'Raleigh-Durdam',
                          city == 'NGO' ~ 'Nagoya-Centrair',
                          TRUE ~ City))

ggplot() +
  geom_sf(data = world_crop, fill = "gray85", color = "white", size = 12) +
  geom_sf(data = routes, size = 0.005, alpha = 0.2, color = '#bd0026') +
  geom_sf(data = cities_sf, color = '#bd0026', size = 0.7) +
  geom_sf(data = cities_sf, color = 'black', size = 0.7,shape = 1) +
  coord_sf(crs = laea_proj, expand = FALSE) +
  geom_text_repel(
    data = cities_label_df,
    aes(x = x, y = y, label = City),
    size = 2,
    fontface = "bold",
    box.padding = 0.3,
    point.padding = 0.1,
    max.overlaps = Inf,
    segment.color = "black",
    segment.size = 0.1
  ) +
  labs(title = "Discontinued European and Asian Routes by American Airlines",
       subtitle = 'International nonstop routes flown throughout the 1990s but absent by 2023',
       caption = 'Code by Alex Elfering | Source: Bureau of Transportation Statistics T-100 International Segment Data') +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = 'top',
    plot.title = element_text(face = 'bold', 
                              size = 18),
    plot.caption = element_text(size = 8,
                                family = 'Noto Sans',
                                hjust = 0,
                                color = 'gray50'),
  ) 

ggsave(file = ('~/GitHub/Airlines/20250614 American Airlines Historical International Routes/historic route map.png'), width = 10, height = 7, dpi = 350, units = "in")
