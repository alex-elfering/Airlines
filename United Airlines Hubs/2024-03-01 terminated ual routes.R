# touch and go
# how many times has an airline started, ended, and started service again?

library(data.table)
library(tidyverse)
library(ggplot2)
library(rvest)
library(stringi)
library(glue)
library(lubridate)
library(janitor)
library(airportr)

# data gathering from T-100 ----
setwd("C:/Users/alexe/OneDrive/Desktop/intl seg")
t_100_intl <- list.files("C:/Users/alexe/OneDrive/Desktop/intl seg", pattern = "*.csv")
t_100_intl_df <- rbindlist(lapply(t_100_intl, fread)) |> clean_names()

setwd("C:/Users/alexe/OneDrive/Desktop/t-100 dom")
t_100_dom <- list.files("C:/Users/alexe/OneDrive/Desktop/t-100 dom", pattern = "*.csv")
t_100_dom_df <- rbindlist(lapply(t_100_intl, fread)) |> clean_names()

aircraft <- read.csv('C:/Users/alexe/OneDrive/Desktop/L_AIRCRAFT_TYPE.csv') |> clean_names()

# filtering and cleaning  ----

airline_var <- c('F9', 'AA','UA','DL','CO','TW','NW','B6','WN','G4', 'NK','HA','AS','VX', 'US', 'HP')
#hub_var <- c('ATL', 'MDW', 'ORD', 'CVG', 'CLE', 'DFW', 'DEN', 'LAS', 'MIA', 'MCO', 'PHL', 'PHX', 'SJU', 'TPA', 'MKE', 'MCI')

international_sum <- t_100_intl_df %>%
  filter(carrier %in% c('UA')) |>
  filter(departures_performed >= 10) %>%
  group_by(unique_carrier_name,
           origin_country,
           origin_city_name,
           origin,
           dest_city_name,
           dest_country,
           dest,
           year,
           month,
           aircraft_type,
           carrier) %>%
  summarise(seats = sum(seats),
            passengers = sum(passengers),
            departures = sum(departures_performed),
            months = n_distinct(month)) %>%
  ungroup() |>
  as.data.frame()

international_stats <- bind_rows(international_sum)  %>%
  inner_join(aircraft, by = c('aircraft_type' = 'code')) |>
  arrange(year) |>
  as.data.frame() |>
  mutate(load_factor = round(passengers/seats,3),
         seats_per_departure = round(seats/departures),
         passengers_per_departure = round(passengers/departures)) |>
  arrange(desc(load_factor)) |>
  mutate(international_service = ifelse(dest_country == 'US', 0, 1))

united_dest_history <- international_stats |>
  group_by(year,
           #carrier,
           origin,
           origin_city_name,
           dest,
           dest_city_name,
           international_service) |>
  summarise(seats = sum(seats)) |>
  ungroup()

origin_dest_service_history <- united_dest_history |>
  group_by(#carrier,
    origin,
    origin_city_name,
    dest,
    dest_city_name) |>
  complete(year = seq.int(min(year), 2023, by = 1)) |>
  mutate(service = ifelse(!is.na(seats), 1, 0),
         service_ends = ifelse(lead(is.na(seats)) & !is.na(seats), 1, 0),
         service_resumes = ifelse( !is.na(seats) & lag(is.na(seats)),1,0 ),
         new_service = ifelse(is.na(service_resumes), 1, 0)) |>
  mutate(consecutive_streak = rowid(rleid(service)) * service) |>
  filter(!is.na(seats)) |>
  mutate(last_served = ifelse(service_resumes == 1, lag(year), NA),
         last_streak = ifelse(service_resumes == 1, lag(consecutive_streak), NA),
         years_since = year-last_served,
         previously_began = last_served-(last_streak-1)) |>
  ungroup() |>
  mutate(service_level = case_when(new_service == 1 & service_ends == 0 ~ 'Service Established',
                                   new_service == 1 & service_ends == 1 ~ 'Service Established, then Terminated',
                                   service_ends == 1 & service_resumes == 0 ~ 'Service Terminated',
                                   service_ends == 1 & service_resumes == 1 ~ 'Service Resumed, then Terminated',
                                   service_ends == 0 & service_resumes == 1 ~ 'Service Resumed',
                                   TRUE ~ 'Service')) |>
  mutate(service_level = case_when(service_level == 'Service Established' & year == 1990 ~ 'Service',
                                   TRUE ~ service_level)) |>
  mutate(service_level = factor(service_level, levels = c('Service Established', 'Service Resumed', 'Service', 'Service Established, then Terminated', 'Service Resumed, then Terminated', 'Service Terminated'))) |>
  separate(dest_city_name, into = c('city', 'city1'), sep = ', ') |>
  separate(origin_city_name, into = c('origin_city', 'city2'), sep = ', ') |>
  mutate(state = ifelse(international_service == 0, city1, ''),
         country = ifelse(international_service == 1, city1, 'United States')) |>
  select(-city1,
         -city2) |>
  # osaka-itami, seoul-gimpo, and berlin brandenburg are not in the airportr package
  mutate(origin = case_when(origin == 'SEL' ~ 'GMP',
                            origin == 'BER' ~ 'SXF',
                            origin == 'OSA' ~ 'ITM',
                            TRUE ~ origin)) |>
  mutate(dest = case_when(dest == 'SEL' ~ 'GMP',
                          dest == 'BER' ~ 'SXF',
                          dest == 'OSA' ~ 'ITM',
                          TRUE ~ dest))


origin_dest_list <- unique(c(origin_dest_service_history$origin, origin_dest_service_history$dest))

airport_loc_list <- list()
for(i in origin_dest_list){
  
  tryCatch({
  
  print(i)
  airport_loc <- airportr::airport_location(i) |>
    mutate(airport= i)
  
  airport_loc_list[[i]] <- airport_loc
  
  }, error=function(e){})
}

airport_loc_df <- rbindlist(airport_loc_list) |> clean_names()

origin_dest_service_history |>
  left_join(airport_loc_df,
            by = c('origin' = 'airport')) |>
  rename(origin_latitude = latitude,
         origin_longitude = longitude) |>
  left_join(airport_loc_df,
            by = c('dest' = 'airport')) |>
  rename(dest_latitude = latitude,
         dest_longitude = longitude) |>
  group_by(origin,
           dest) |>
  filter(year == max(year),
         grepl('Terminate', service_level)) |> 
  ungroup() |>
  # filter(year == 'JFK')
  write.csv('C:/Users/alexe/OneDrive/Documents/GitHub/Airlines/United Airlines Hubs/united airlines terminated routes.csv')