# United Airlines Historical Service to London Heathrow 1991-2022

library(tidylog)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(sysfonts)
library(ggtext)
library(showtext)
library(ggpattern)
library(gghighlight)
library(gt)
library(zoo)

options("device" = "windows")
options(scipen = 999)

# data cleaning ----
setwd("C:/Users/alexe/OneDrive/Desktop/Domestic Segment")
intl <- list.files("C:/Users/alexe/OneDrive/Desktop/Domestic Segment", pattern = "*.csv")
intl_df <- rbindlist(lapply(intl, fread))

colnames(intl_df) <- tolower(colnames(intl_df))

aircraft <- read.csv('C:/Users/alexe/OneDrive/Desktop/L_AIRCRAFT_TYPE.csv')
colnames(aircraft) <- tolower(colnames(aircraft))

#https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# filter on FedEx
city_var <- 'OMA'

outbound_flight_details <- intl_df |>
  filter(origin == city_var) |>
  arrange(year,
          month) |>
  select(origin,
         dest_city_name,
         dest,
         carrier,
         carrier_name,
         month,
         year,
         departures_scheduled,
         departures_performed,
         seats,
         passengers,
         aircraft_type,
         air_time,
         ramp_to_ramp) |>
  inner_join(aircraft,
             by = c('aircraft_type' = 'code')) |>
  mutate(seats_departure = floor(seats/departures_performed),
         air_time_departure = round(air_time/departures_performed),
         ramp_to_ramp_dep = round(ramp_to_ramp/departures_performed),
         load_factor = round((passengers/seats)*100,1),
         flight_month = ymd(paste(year, month, '1', '-'))) |>
  filter(seats > 0,
         passengers > 0,
         departures_performed > 0,
         departures_scheduled > 4) |>
  select(-year,
         -month)

year_var <- 1995

aircraft_used <- outbound_flight_details |>
  filter(year(flight_month) == year_var) |>
  select(carrier,
         description) |>
  distinct() |>
  mutate(description = case_when(grepl('Super 80', description) ~ 'MD-80 series',
                        grepl('727-2', description) ~ 'Boeing 727-200',
                        grepl('McDonnell Douglas DC-9', description) ~ gsub('McDonnell Douglas ', '', description),
                        grepl('McDonnell Douglas DC-10', description) ~ gsub('McDonnell Douglas ', '', description),
                        TRUE ~ description)) |>
  arrange(carrier,
          description) |>
  group_by(carrier) |>
  mutate(carrier_des = row_number()) |>
  ungroup() |>
  pivot_wider(names_from = carrier_des,
              values_from = description) |>
  unite(aircraft_used , -c(carrier), sep =', ', na.rm = TRUE)

stats_overall <- outbound_flight_details |>
  filter(year(flight_month) == year_var) |>
  #filter(dest == 'LNK') |>
  group_by(dest,
           carrier) |>
  summarise(median_tot_seats = median(seats),
            median_tot_passengers = median(passengers),
            median_tot_departures = median(departures_performed)) |>
  ungroup()

outbound_flight_details |>
  filter(year(flight_month) == year_var) |>
  #filter(dest == 'LNK') |>
  group_by(dest,
           carrier,
           carrier_name) |>
  summarise(from_date = as.yearmon(min(flight_month)),
            to_date = as.yearmon(max(flight_month)),
            median_seats = median(seats_departure),
            median_ld_fctr = median(load_factor),
            median_air_time = median(air_time_departure)) |>
  ungroup() |>
  inner_join(stats_overall) |>
  inner_join(aircraft_used) |>
  arrange(desc(median_tot_passengers)) |>
  unite(date_range, c('from_date', 'to_date'), sep = '-')  |>
  mutate(carrier_name = case_when(grepl(' Inc.', carrier_name) ~ gsub(' Inc.', '', carrier_name),
                                  grepl(' Corporation', carrier_name) ~ gsub(' Corporation', '', carrier_name),
                                  grepl(' Co.', carrier_name) ~ gsub(' Co.', '', carrier_name),
                                  TRUE ~ carrier_name)) |>
  gt() |>
  fmt_integer(columns = c('median_seats', 'median_ld_fctr', 'median_air_time', 'median_tot_seats', 'median_tot_passengers', 'median_tot_departures'),
              use_seps = TRUE) |>
  tab_spanner(
    label = "Per Departure",
    columns = c(median_seats, median_ld_fctr, median_air_time)
  ) |>
  tab_spanner(
    label = "Per Month",
    columns = c(median_tot_seats, median_tot_passengers, median_tot_departures)
  )
  