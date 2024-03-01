library(tidylog)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(sysfonts)
library(ggtext)
library(showtext)
library(gghighlight)
library(ggbump)
library(maps)

options("device" = "windows")
options(scipen = 999)

# data cleaning ----
setwd("C:/Users/alexe/OneDrive/Desktop/International Segment")
intl <- list.files("C:/Users/alexe/OneDrive/Desktop/International Segment", pattern = "*.csv")
intl_df <- rbindlist(lapply(intl, fread))

colnames(intl_df) <- tolower(colnames(intl_df))

aircraft <- read.csv('C:/Users/alexe/OneDrive/Desktop/L_AIRCRAFT_TYPE.csv')
colnames(aircraft) <- tolower(colnames(aircraft))

outbound_intl_fleet <- intl_df |>
  inner_join(aircraft,
             by = c('aircraft_type' = 'code')) |>
  filter(origin_country_name == 'United States',
         carrier %in% c('UA', 'AA', 'DL', 'US', 'CO', 'NW', 'TW', 'PA', 'EA', 'CS')) |>
  mutate(flight_month = ymd(paste(year, month, 1, sep = '-'))) |>
  select(year,
         month,
         flight_month,
         carrier,
         carrier_name,
         origin,
         region,
         dest,
         dest_city_name,
         dest_country,
         dest_country_name,
         description,
         departures_scheduled,
         departures_performed,
         seats,
         passengers) |>
  filter(year >= 2000 & year <= 2010) |>
  filter(departures_scheduled > 0,
         departures_performed > 4,
         passengers > 0,
         seats > 0) 

# continental 757-200 ----
# how common is the 757 on international routes?

# what destinations did Continental serve in Europe?
dest_757 <- outbound_intl_fleet |>
  filter(origin == 'EWR',
         region == 'A',
         carrier == 'CO',
         description == 'Boeing 757-200') |>
  group_by(flight_month,
           dest) |>
  summarise(seats = sum(seats),
            departures_scheduled = sum(departures_scheduled)) |>
  ungroup() |>
  distinct(dest)

# are these new destinations, or did continental complement existing service with 757s

outbound_intl_fleet |>
  filter(origin == 'EWR',
         region == 'A',
         carrier == 'CO') |>
  inner_join(dest_757) |>
  group_by(dest,
           flight_month,
           description) |>
  summarise(seats = sum(seats)) |>
  ungroup() |>
  group_by(dest,
           description) |>
  complete(flight_month = seq.Date(min(flight_month), max(flight_month), 'month')) |>
  ungroup() |>
  ggplot() + 
  geom_line(mapping = aes(x = flight_month,
                          y = seats,
                          color = description),
            size = 1) +
  facet_wrap(~dest,
             scales = 'free')
  
