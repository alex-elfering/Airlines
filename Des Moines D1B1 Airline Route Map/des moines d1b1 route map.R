# create a data visualization map that visualizes routes flown by outbound-Des Moines passengers in 2019
# using d1b1 data and on-time performance data from the Bureau of Transportation Statistics
# code by alex elfering

library(tidyverse)
library(data.table)
library(tidylog)
library(albersusa)
library(sp)
library(raster)
library(ggplot2)
library(broom)
library(ggrepel)

# function thanks to: https://stackoverflow.com/questions/2261079/how-can-i-trim-leading-and-trailing-white-space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# data load
on_time <- list.files("C:/Users/alexe/Desktop/Iowa on time", pattern = "*.csv", full.names = TRUE)
on_time_df <- rbindlist(lapply(on_time, fread))

db1b <- list.files("C:/Users/alexe/Desktop/Iowa", pattern = "*.csv", full.names = TRUE)
db1b_DF <- rbindlist(lapply(db1b, fread))

colnames(on_time_df) <- tolower(colnames(on_time_df))
colnames(db1b_DF) <- tolower(colnames(db1b_DF))

  # lat and long airport data
lat_long <- read.csv('~/airports.csv') %>% filter(grepl('US-', iso_region))
airport_lat_long <- dplyr::select(lat_long, 14, 5, 6) %>% mutate(iata_code = trim(iata_code)) #%>% filter(iata_code == 'MSP')

# clean the on-time performance data
non_stops <- on_time_df %>%
  filter(origin == 'DSM') %>%
  distinct(year,
           quarter,
           origin,
           dest,
           mkt_unique_carrier)

non_stop_list <- unique(non_stops$dest)

# clean the D1B1 data
filter_outbound <- dplyr::filter(db1b_DF, origin == 'DSM') %>%
  mutate(airport_group = gsub('\\:', ' ', airport_group),
         connections = sapply(strsplit(airport_group, " "), length)-2) %>%
  filter(connections <= 2,
         year == 2019) %>%
  mutate(connecting_airports = case_when(connections == 0 ~ '',
                                         connections == 1 ~ substr(airport_group, 5, 7),
                                         connections == 2 ~ substr(airport_group, 5, 11))) %>%
  separate(connecting_airports, into = c('first_stop', 'second_stop'), sep = ' ') %>%
  mutate(first_stop = trim(first_stop),
         second_stop = trim(second_stop)) %>%
  as.tibble() %>%
  select(year,
         reporting_carrier,
         ticket_carrier,
         airport_group,
         origin,
         dest,
         dest_state_abr,
         first_stop,
         second_stop,
         connections,
         passengers)

non_stop <- filter_outbound %>%
  filter(connections == 0,
         dest %in% non_stop_list) %>%
  group_by(year,
           origin,
           dest,
           dest_state_abr,
           first_stop,
           second_stop,
           connections) %>%
  summarise(pax = sum(passengers)) %>%
  ungroup()

one_stop <- filter_outbound %>%
  filter(connections == 1,
         first_stop %in% non_stop_list) %>%
  group_by(year,
           origin,
           dest,
           dest_state_abr,
           first_stop,
           second_stop,
           connections) %>%
  summarise(pax = sum(passengers)) %>%
  ungroup() %>%
  # remove outlier routes
  filter(pax > 2)

# prepare airport lat and long data for visualization
all_airports <- tibble(airports = trim(unique(c(one_stop$origin, one_stop$dest, non_stop$origin, non_stop$dest)))) %>%
  inner_join(airport_lat_long, by = c('airports' = 'iata_code')) # %>% filter(airports == 'MSP')

airports_lat_long <- dplyr::select(all_airports, 3, 2) %>% rename(x = longitude_deg, y = latitude_deg)

coordinates(airports_lat_long) <- ~x+y
proj4string(airports_lat_long) <- CRS(us_longlat_proj)
airports_lat_long <- points_elided(airports_lat_long) 
airports_lat_long <- spTransform(airports_lat_long, CRSobj = CRS(us_aeqd_proj))
airports_lat_long <- as.data.frame(coordinates(airports_lat_long))

airport_lat_long_df <- cbind(all_airports, airports_lat_long)

non_stop_coord <- non_stop %>%
  left_join(airport_lat_long_df,
            by = c('origin' = 'airports')) %>%
  dplyr::select(-latitude_deg,
                -longitude_deg) %>%
  left_join(airport_lat_long_df,
            by = c('dest' = 'airports')) %>%
  dplyr::select(-latitude_deg,
                -longitude_deg) %>%
  rename(origin_long = x.x,
         origin_lat = y.x,
         destin_long = x.y,
         destin_lat = y.y)

one_stop_coord <- one_stop %>%
  inner_join(airport_lat_long_df,
             by = c('origin' = 'airports')) %>%
  dplyr::select(-latitude_deg,
                -longitude_deg) %>%
  inner_join(airport_lat_long_df,
             by = c('dest' = 'airports')) %>%
  dplyr::select(-latitude_deg,
                -longitude_deg) %>%
  inner_join(airport_lat_long_df,
             by = c('first_stop' = 'airports')) %>%
  dplyr::select(-latitude_deg,
                -longitude_deg) %>%
  rename(origin_long = x.x,
         origin_lat = y.x,
         destin_long = x.y,
         destin_lat = y.y,
         first_long = x,
         first_lat = y)

airport_flip <- c('CA')

us_map <- fortify(us, region="name")

airport_labels <- bind_rows(one_stop_coord, non_stop_coord) %>%
  distinct(dest,
           destin_long,
           destin_lat)

one_stop_labels <- one_stop_coord %>%
  distinct(first_stop,
           first_long,
           first_lat)

USA <- usa_composite(proj="laea")  # creates map projection 
USA_MAP <- tidy(USA, region="name")

# visualization
final_map <- ggplot() +
  geom_map(
    data = USA_MAP, 
    map = USA_MAP,
    aes(x = long, 
        y = lat, 
        map_id = id),
    color = "white", 
    size = 1, 
    alpha = 0.2,
    fill = 'gray75'
  ) +
  geom_text_repel(airport_labels %>% filter(!dest %in% one_stop_labels$first_stop),
                  mapping = aes(x = destin_long,
                                y = destin_lat,
                                label = dest),
                  fontface = 'bold',
                  family = 'IBM Plex Sans') +
  geom_curve(one_stop_coord, 
             mapping = aes(x = origin_long, 
                           y = origin_lat, 
                           xend = first_long, 
                           yend = first_lat ),
             color = '#6baed6',
             curvature = 0.1,
             size = 1,
             alpha = 0.4) +
  geom_curve(one_stop_coord, 
             mapping = aes(x = first_long, 
                           y = first_lat, 
                           xend = destin_long, 
                           yend = destin_lat),
             color = '#6baed6',
             curvature = -0.1,
             size = 1,
             alpha = 0.2) +
  geom_curve(non_stop_coord, 
             mapping = aes(x = origin_long, 
                           y = origin_lat, 
                           xend = destin_long, 
                           yend = destin_lat),
             #linetype = 'dashed',
             curvature = 0.1,
             color = '#6baed6',
             size = 1.5,
             alpha = 0.4) +
  geom_point(one_stop_coord, 
             mapping = aes(x = first_long, 
                           y = first_lat),
             color = 'darkorange',
             size = 10) +
  geom_point(one_stop_coord, 
             mapping = aes(x = origin_long, 
                           y = origin_lat),
             color = '#de2d26',
             size = 10) +
  geom_point(one_stop_coord %>% filter(!dest %in% one_stop_labels$first_stop), 
             mapping = aes(x = destin_long, 
                           y = destin_lat),
             color = 'black',
             size = 4) +
  geom_text_repel(one_stop_labels,
                  mapping = aes(x = first_long,
                                y = first_lat,
                                label = first_stop),
                  size = 8,
                  box.padding = 1,
                  color = 'darkorange',
                  fontface = 'bold',
                  family = 'IBM Plex Sans') +
  geom_text_repel(mapping = aes(x = 527052.9,
                                y = -365024.6),
                  label = 'DSM',
                  size = 8,
                  box.padding = 1,
                  color = '#de2d26',
                  fontface = 'bold',
                  family = 'IBM Plex Sans') +
  coord_equal()  +
  labs(x = '', 
       y = '',
       title = 'The United States According to Des Moines Travelers in 2019',
       subtitle = 'Non-stop and one-stop domestic flights by Des Moines travelers according to the D1B1 Market Airline Survey',
       caption = 'Map created by Alex Elfering | Source: Bureau of Transportation Statistics | O&Ds with 2 or less passengers were filtered out') +
  theme(plot.title = element_text(face = 'bold',family = 'IBM Plex Sans', size = 16),
        plot.subtitle = element_text(size = 14,family = 'IBM Plex Sans'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 12, color = "#c1c1c1",family = 'IBM Plex Sans'),
        axis.title = element_text(size = 12),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank())

ggsave(final_map, file = '~/GitHub/Airlines/Des Moines D1B1 Airline Route Map/des moines route map d1b1.png', width = 21, height = 12, units = c('in'))
