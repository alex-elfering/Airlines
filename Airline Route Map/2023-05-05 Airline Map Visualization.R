# airline route map data visualization exploration

library(tidylog)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(ggpattern)
library(sysfonts)
library(ggtext)
library(showtext)
library(airportr)
library(stringr)
library(glue)
library(zoo)
library(gghighlight)
library(ggstream)

library(albersusa)
library(sp)
library(raster)
library(ggplot2)
library(broom)
library(ggrepel)


options("device" = "windows")
options(scipen = 999)

# data cleaning ----

setwd("C:/Users/alexe/OneDrive/Desktop/Full Schedule Data")
sched_list <- list.files("C:/Users/alexe/OneDrive/Desktop/Full Schedule Data", pattern = "*.csv")
schedule_df <- rbindlist(lapply(sched_list, fread))

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

colnames(schedule_df) <- trim(tolower(colnames(schedule_df)))

clean_schedule_df <- schedule_df %>%
  filter(originstate %in% state.abb,
         deststate %in% state.abb) %>%
  dplyr::select(fl_date = flightdate,
                mkt_unique_carrier = marketing_airline_network,
                op_unique_carrier = operating_airline,
                mkt_carrier_fl_num = flight_number_marketing_airline,
                origin,
                dest,
                crs_dep_time = crsdeptime,
                crs_arr_time = crsarrtime,
                crs_elapsed_time = crselapsedtime,
                dep_delay = depdelayminutes,
                taxi_out = taxiout,
                wheels_off = wheelsoff,
                wheels_on = wheelson,
                arr_delay = arrdelay,
                cancelled,
                flights,
                distance) %>%
  filter(mkt_unique_carrier %in% c('AS', 'AA', 'DL', 'F9', 'HA', 'B6', 'WN', 'NK', 'UA', 'G4', 'SY')) %>%
  mutate(crs_dep = case_when(nchar(crs_dep_time) == 1 ~ paste0('000', crs_dep_time),
                             nchar(crs_dep_time) == 2 ~ paste0('00', crs_dep_time),
                             nchar(crs_dep_time) == 3 ~ paste0('0', crs_dep_time),
                             TRUE ~ as.character(crs_dep_time))) %>%
  mutate(crs_arr = case_when(nchar(crs_arr_time) == 1 ~ paste0('000', crs_arr_time),
                             nchar(crs_arr_time) == 2 ~ paste0('00', crs_arr_time),
                             nchar(crs_arr_time) == 3 ~ paste0('0', crs_arr_time),
                             TRUE ~ as.character(crs_arr_time))) %>%
  mutate(fl_date = ymd((fl_date)),
         crs_dep = format(strptime(crs_dep, format = "%H%M"), format = "%H:%M"),
         crs_arr = format(strptime(crs_arr, format = "%H%M"), format = "%H:%M")) %>%
  mutate(crs_dep_date = ymd_hm(paste(fl_date, crs_dep) )) %>%
  mutate(init_arrival = (crs_dep_date+minutes(crs_elapsed_time)),
         init_arrival_extract = format(init_arrival, '%H:%M')) %>%
  mutate(init_hour = as.numeric(substr(init_arrival_extract, 1, 2)),
         actu_hour = as.numeric(substr(crs_arr, 1, 2)),
         adj = init_hour-actu_hour) %>%
  mutate(new_adj = case_when(adj >= 20 ~ adj - 24,
                             adj <= -20 ~ adj + 24,
                             TRUE ~ adj),
         final_arrival = init_arrival - hours(new_adj),
         delayed = ifelse(dep_delay >= 15, 1, 0)) %>%
  dplyr::select(fl_date,
                mkt_unique_carrier,
                op_unique_carrier,
                mkt_carrier_fl_num,
                origin,
                dest,
                dep_delay,
                crs_dep_date,
                taxi_out,
                wheels_off,
                arr_delay,
                crs_arr_date = final_arrival,
                wheels_on,
                taxi_in,
                crs_elapsed_time,
                delayed,
                flights,
                cancelled) %>% 
  as.tibble() %>%
  distinct() %>%
  arrange(crs_dep_date)

flights_not_cancelled <- dplyr::filter(clean_schedule_df, cancelled != 1)

# latitude and longitude of airports  ----
airports_list <- flights_not_cancelled %>%
  distinct(origin)

airports_list_vector <- airports_list$origin

airport_list <- list()
for(i in airports_list_vector){
  
  tryCatch({
    
    airport_coord <- airportr::airport_detail(i)
    airport_coord_final <- airport_coord %>% mutate(airport = i)
    
    airport_list[[i]] <- airport_coord_final
    print(i)
    
  }, error=function(e){})
  
}

airport_city_df <- rbindlist(airport_list) %>% dplyr::select(airport, City)
colnames(airport_city_df) <- tolower(colnames(airport_city_df))

airport_coord_df <- rbindlist(airport_list) %>% dplyr::select(airport, Latitude, Longitude)
colnames(airport_coord_df) <- tolower(colnames(airport_coord_df))

airports_lat_long <- dplyr::select(airport_coord_df, 2, 3) %>% rename(x = longitude, y = latitude)

coordinates(airports_lat_long) <- ~x+y
proj4string(airports_lat_long) <- CRS(us_longlat_proj)
airports_lat_long <- points_elided(airports_lat_long) 
airports_lat_long <- as.data.frame(coordinates(airports_lat_long))

airport_lat_long_df <- cbind(airport_coord_df, airports_lat_long) 

# data exploration: stream graph  ----
airline_var <- c('WN')

monthly_delays <- flights_not_cancelled %>%
  filter(fl_date >= as.Date('2022-05-27') & fl_date <= as.Date('2022-09-05')) %>%
  filter(mkt_unique_carrier %in% airline_var ) %>%
  group_by(mkt_unique_carrier,
    origin,
    dest) %>%
  summarise(flights = sum(flights),
            delays = sum(delayed)) %>%
  ungroup() %>%
  filter(flights > 3) %>%
  arrange(desc(delays)) 

monthly_delays_round_trip <- monthly_delays %>%
  inner_join(monthly_delays,
             by = c('mkt_unique_carrier' = 'mkt_unique_carrier',
               'dest' = 'origin',
               'origin' = 'dest')) %>%
  mutate(total_delays = delays.x + delays.y,
         total_flights = flights.x + flights.y,
         rate = total_delays/total_flights)

de_dupe_rows <- monthly_delays_round_trip[!duplicated(apply(monthly_delays_round_trip[,1:3], 1, function(row) paste(sort(row), collapse=""))),]

airport_routes <- de_dupe_rows %>%
  inner_join(airport_lat_long_df,
             by = c('origin' = 'airport')) %>%
  dplyr::select(-latitude,
                -longitude) %>%
  inner_join(airport_lat_long_df,
             by = c('dest' = 'airport')) %>%
  dplyr::select(-latitude,
                -longitude) %>%
  arrange(desc(total_flights)) %>%
  mutate(rank_flights = dense_rank(desc(total_flights)),
         top_route = ifelse(rank_flights <= 25, 'Top Rank', 'Bottom Rank'),
         top_route = factor(top_route, levels = c('Top Rank', 'Bottom Rank')),
         top_route = factor(top_route, levels = rev(levels(airport_routes$top_route))))

USA <- usa_composite(proj="laea")  # creates map projection 
USA_MAP <- tidy(USA, region="name")

airport_origin <- airport_routes %>%
  distinct(origin,
           x.x,
           y.x) %>%
  rename(airport = origin,
         x = x.x,
         y = y.x)

airport_dest <- airport_routes %>%
  distinct(dest,
           x.y,
           y.y) %>%
  rename(airport = dest,
         x = x.y,
         y = y.y)

full_airport <- bind_rows(airport_origin, airport_dest) %>% distinct() %>%
  inner_join(airport_city_df) %>%
  mutate(city = case_when(airport == 'DCA' ~ 'Washington National',
                          airport == 'EWR' ~ 'Newark',
                          airport == 'ORD' ~ "Chicago O'Hare",
                          airport == 'IAH' ~ 'Houston Intercontinental',
                          airport == 'IAD' ~ 'Washington Dulles',
                          airport == 'LGA' ~ 'New York La Guardia',
                          airport == 'JFK' ~ 'New York JFK',
                          airport == 'ACV' ~ 'Arcata',
                          airport == 'CHO' ~ 'Charlottesville',
                          airport == 'MHT' ~ 'Manchester',
                          airport == 'MTJ' ~ 'Montrose',
                          airport == 'PAH' ~ 'Paducah',
                          airport == 'RIW' ~ 'Riverton',
                          airport == 'ROA' ~ 'Roanoke',
                          airport == 'SCE' ~ 'State College',
                          airport == 'AZA' ~ 'Phoenix Mesa',
                          airport == 'LEX' ~ 'Lexington',
                          airport == 'HOU' ~ 'Houston Hobby',
                          airport == 'DAL' ~ 'Dallas Love',
                          airport == 'MDW' ~ 'Chicago Midway',
                          airport == 'RFD' ~ 'Chicago Rockford',
                          airport == 'SNA' ~ 'Orange County',
                          airport == 'SFB' ~ 'Orlando Sanford',
                          airport == 'BDL' ~ 'Hartford',
                          airport == 'TRI' ~ 'Bristol',
                          airport == 'MGM' ~ 'Montgomery',
                          airport == 'MVY' ~ "Martha's Vineyard",
                          airport == 'RDU' ~ "Raleigh-Durham",
                          airport == 'GTR' ~ "Columbus",
                          airport == 'XNA' ~ "Northwest Arkansas",
                          airport == 'MLI' ~ "Moline/Quad Cities",
                          airport == 'HDN' ~ "Hayden/Steamboat Springs",
                          TRUE ~ city))

ggplot() +
  geom_map(
    data = USA_MAP, 
    map = USA_MAP,
    aes(x = long, 
        y = lat, 
        map_id = id),
    color = 'white',
    size = 0.5,
    fill = 'gray90'
  ) +
  geom_curve(airport_routes %>% filter(top_route != 'Top Rank'), 
             mapping = aes(x = x.x, 
                           y = y.x, 
                           xend = x.y, 
                           yend = y.y,
                           color = top_route),
             color = '#fd8d3c',
             curvature = 0.3,
             size = 0.25,
             alpha = 0.4) +
  geom_curve(airport_routes %>% filter(top_route == 'Top Rank'), 
             mapping = aes(x = x.x, 
                           y = y.x, 
                           xend = x.y, 
                           yend = y.y,
                           color = top_route),
             color = '#fc4e2a',
             curvature = 0.3,
             size = 1.25,
             alpha = 0.8) +
  geom_point(full_airport, 
             mapping = aes(x = x, 
                           y = y),
             color = 'gray20',
             size = 1.5) +
  geom_text_repel(full_airport,
                  mapping = aes(x = x,
                                y = y,
                                label = city),
                  color = 'gray20',
                  #alpha = 0.6, 
                  label.size = NA,
                  max.overlaps = 20,
                  seed = 999,
                  size = 2,
                  fontface = 'bold') +
  labs(title = 'Southwest Highways in the Sky',
       subtitle = 'The busiest flights flown by Southwest Airlines between Memorial Day through Labor Day 2022 by flights flown',
       y = '',
       x = '',
       caption = 'Source: On-Time Marketing Carrier On-Time Performance from the Bureau of Transporation Statistics\nVisualization by Alex Elfering\nCancelled flights are excluded; Flight date is based on original CRS date') +
  theme(plot.title = element_text(face = 'bold',family = 'Arial', size = 16),
        plot.subtitle = element_text(size = 14,family = 'Arial'),
        legend.position = 'none',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title = element_text(size = 16,
                                    color = 'black',
                                    face = 'bold',
                                    family = 'Arial'),
        legend.text = element_text(size = 16,
                                   color = 'black',
                                   family = 'Arial'),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 10, color = "#c1c1c1",family = 'Arial'),
        axis.title = element_text(size = 12),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        strip.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank())

ggsave(file = glue('C:/Users/alexe/OneDrive/Desktop/{airline_var} route map.png'), dpi = 300,  width = 16, height = 9, units = c('in'))
