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

aircraft <- read.csv('C:/Users/alexe/OneDrive/Desktop/L_AIRCRAFT_TYPE.csv') |> clean_names()

# filtering and cleaning  ----

#airline_var <- c('F9', 'AA','UA','DL','CO','TW','NW','B6','WN','G4', 'NK','HA','AS','VX', 'US', 'HP')
#hub_var <- c('ATL', 'MDW', 'ORD', 'CVG', 'CLE', 'DFW', 'DEN', 'LAS', 'MIA', 'MCO', 'PHL', 'PHX', 'SJU', 'TPA', 'MKE', 'MCI')

international_sum <- t_100_intl_df %>%
  filter(carrier == 'DL',
         origin == 'JFK') |>
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

international_stats <- international_sum  %>%
  inner_join(aircraft, by = c('aircraft_type' = 'code')) |>
  arrange(year) |>
  as.data.frame() |>
  mutate(load_factor = round(passengers/seats,3),
         seats_per_departure = round(seats/departures),
         passengers_per_departure = round(passengers/departures)) |>
  arrange(desc(load_factor)) |>
  mutate(international_service = ifelse(dest_country == 'US', 0, 1))

delta_dest_history <- international_stats |>
  group_by(year,
           carrier,
           origin,
           dest,
           dest_city_name,
           international_service) |>
  summarise(seats = sum(seats)) |>
  ungroup()

origin_dest_service_history <- delta_dest_history |>
  group_by(carrier,
           origin,
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
  filter(service_level != 'Service') |>
  mutate(service_level = factor(service_level, levels = c('Service Established', 'Service Resumed', 'Service Established, then Terminated', 'Service Resumed, then Terminated', 'Service Terminated')))

write.csv(origin_dest_service_history,"C:/Users/alexe/OneDrive/Documents/GitHub/Airlines/Delta Air Lines JFK Hub/origin_dest_service_history.csv")