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
library(glue)

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
         load_factor = round((passengers/seats)*80,1),
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
         dest,
         description) |>
  distinct() |>
  mutate(description = case_when(grepl('Super 80', description) ~ 'MD-80 series',
                        grepl('727-2', description) ~ 'Boeing 727-200',
                        grepl('McDonnell Douglas DC-9', description) ~ gsub('McDonnell Douglas ', '', description),
                        grepl('McDonnell Douglas DC-10', description) ~ gsub('McDonnell Douglas ', '', description),
                        TRUE ~ description)) |>
  arrange(carrier,
          dest,
          description) |>
  group_by(carrier,
           dest) |>
  mutate(carrier_des = row_number()) |>
  ungroup() |>
  pivot_wider(names_from = carrier_des,
              values_from = description) |>
  unite(aircraft_used , -c(carrier, dest), sep =', ', na.rm = TRUE)

stats_overall <- outbound_flight_details |>
  filter(year(flight_month) == year_var) |>
  #filter(dest == 'LNK') |>
  group_by(dest,
           carrier) |>
  summarise(mean_tot_seats = mean(seats),
            mean_tot_passengers = mean(passengers),
            mean_tot_departures = mean(departures_performed)) |>
  ungroup()


# https://malco.io/2020/05/16/replicating-an-nyt-table-of-swedish-covid-deaths-with-gt/
plus_percent <- function(.x) {
  glue::glue("{.x}%")
}

outbound_flight_details |>
  filter(year(flight_month) == year_var) |>
  #filter(dest == 'LNK') |>
  group_by(dest,
           dest_city_name,
           carrier,
           carrier_name) |>
  summarise(from_date = month.abb[month(min(flight_month))],
            to_date = month.abb[month(max(flight_month))],
            mean_seats = mean(seats_departure),
            mean_ld_fctr = round(mean(load_factor),1),
            mean_air_time = mean(air_time_departure)) |>
  ungroup() |>
  inner_join(stats_overall) |>
  inner_join(aircraft_used) |>
  arrange(desc(mean_tot_passengers)) |>
  unite(date_range, c('from_date', 'to_date'), sep = '-')  |>
  unite(city_airport, c('dest', 'dest_city_name'), sep = ' ') |>
  mutate(carrier_name = case_when(grepl(' Inc.', carrier_name) ~ gsub(' Inc.', '', carrier_name),
                                  grepl(' Corporation', carrier_name) ~ gsub(' Corporation', '', carrier_name),
                                  grepl(' Co.', carrier_name) ~ gsub(' Co.', '', carrier_name),
                                  TRUE ~ carrier_name)) |>
  select(-carrier) |>
  select(city_airport,
         carrier_name,
         date_range,
         mean_tot_seats,
         mean_tot_passengers,
         mean_tot_departures,
         mean_seats,
         mean_ld_fctr,
         mean_air_time,
         aircraft_used) |>
  filter(carrier_name != 'Casino Express') %>%
  gt() |>
  fmt_integer(columns = c('mean_seats', 'mean_ld_fctr', 'mean_air_time', 'mean_tot_seats', 'mean_tot_passengers', 'mean_tot_departures'),
              use_seps = TRUE) |>
  fmt(mean_ld_fctr, fns = plus_percent) |>
  tab_spanner(
    label = "Per Departure",
    columns = c(mean_seats, mean_ld_fctr, mean_air_time)
  ) |>
  tab_spanner(
    label = "Per Month",
    columns = c(mean_tot_seats, mean_tot_passengers, mean_tot_departures)
  )|>
  tab_style(
    style = cell_text(
      size = px(13),
      color = "black",
      font = "arial",
      weight = 'bold',
    ),
    locations = cells_column_labels(columns = everything())
  ) |>
  tab_style(
    style = cell_text(
      size = px(13),
      color = "#999",
      font = "arial",
    ),
    locations = cells_body(columns = c(date_range))
  ) |>
  cols_label(
    city_airport = "Destination",
    carrier_name = "Airline",
    date_range = "Served Between",
    mean_seats = 'Seats',
    mean_ld_fctr = 'Load Factor',
    mean_air_time = 'Air Time (minutes)',
    mean_tot_seats = 'Seats',
    mean_tot_passengers = 'Passengers',
    mean_tot_departures = 'Departures',
    aircraft_used = 'Aircraft Used') |>
  tab_options(table.font.size = 13,
              table.font.names = 'arial') |>
  tab_header(
    title = md(glue("**{year_var} Omaha Eppley Airfield Passenger Stats**")),
    subtitle = md(glue("Outbound passenger stats by airline and destination YE {year_var}, sorted by passengers per month"))
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("left", 'right'),
      color = "white",
      weight = px(0.5),
      style = "solid"
    ),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("left"),
      color = "gray80",
      weight = px(2),
      style = "solid"
    ),
    locations = cells_body(columns = c(mean_tot_seats, mean_seats, aircraft_used))
  ) %>%
  data_color(
    columns = vars(mean_tot_seats, mean_tot_passengers, mean_tot_departures),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = (c('#f1eef6', '#d0d1e6', '#a6bddb', '#74a9cf', '#2b8cbe', '#045a8d')),
      domain = NULL
    )
  )  %>%
  data_color(
    columns = vars(mean_seats,mean_air_time,mean_ld_fctr),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = (c('#fef0d9', '#fdd49e', '#fdbb84', '#fc8d59', '#e34a33', '#b30000')),
      domain = NULL
    )
  ) |>
  cols_align(
    align = c("center"),
    columns = c(mean_seats, mean_ld_fctr, mean_air_time, mean_tot_seats, mean_tot_passengers, mean_tot_departures)
  )