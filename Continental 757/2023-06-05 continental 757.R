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

options("device" = "windows")
options(scipen = 999)

# data cleaning ----
setwd("C:/Users/alexe/OneDrive/Desktop/International Segment")
intl <- list.files("C:/Users/alexe/OneDrive/Desktop/International Segment", pattern = "*.csv")
intl_df <- rbindlist(lapply(intl, fread))

colnames(intl_df) <- tolower(colnames(intl_df))

aircraft <- read.csv('C:/Users/alexe/OneDrive/Desktop/L_AIRCRAFT_TYPE.csv')
colnames(aircraft) <- tolower(colnames(aircraft))

intl_aircraft <- intl_df |>
  inner_join(aircraft,
             by = c('aircraft_type' = 'code'))

# continental 757-200 ----
intl_aircraft |>
  filter(grepl('757-2', description),
         carrier %in% c('CO')) |>
  filter(origin_country_name == 'United States') |>
  select(year,
         month,
         carrier,
         origin,
         region,
         dest,
         dest_city_name,
         description,
         departures_scheduled,
         departures_performed,
         seats,
         passengers) |>
  filter(departures_scheduled > 0,
         departures_performed > 4) |>
  filter(region == 'A') |>
  arrange(year,
          month) |>
  group_by(year,
           dest) |>
  summarise(seats = sum(seats)) |>
  ungroup() |>
  group_by(year) |>
  mutate(seats_share = seats/sum(seats)) |>
  ungroup() |>
  ggplot() + 
  geom_bar(mapping = aes(x = year,
                          y = seats_share,
                          fill = dest),
           stat = 'identity',
           position = 'stack')