# US Airline fleet optimization -> domestic vs international

library(tidylog)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(ggpattern)
library(sysfonts)
library(ggtext)
library(showtext)

options("device" = "windows")
options(scipen = 999)

# data cleaning ----
setwd("C:/Users/alexe/OneDrive/Desktop")
airline_t2 <- read.csv("C:/Users/alexe/OneDrive/Desktop/airline t2.csv")

colnames(airline_t2) <- tolower(colnames(airline_t2))

aircraft <- read.csv('C:/Users/alexe/OneDrive/Desktop/L_AIRCRAFT_TYPE.csv')
colnames(aircraft) <- tolower(colnames(aircraft))

us_airlines <- c('AA', 'UA', 'DL', 'US', 'NW', 'CO')

# data prep ----

intl_domestic_seats_fleet <- airline_t2 %>%
  filter(unique_carrier %in% us_airlines) %>%
  select(year,
         unique_carrier,
         unique_carrier_name,
         carrier_region,
         aircraft_type,
         avl_seat_miles_320,
         rev_pax_miles_140,
         rev_acrft_dep_perf_510) %>%
  group_by(year,
           unique_carrier,
           unique_carrier_name,
           carrier_region,
           aircraft_type) %>%
  summarise_if(is.numeric,
               sum,
               na.rm = TRUE) %>%
  ungroup() %>%
  inner_join(aircraft,
             by = c('aircraft_type' = 'code')) %>%
  mutate(intl = ifelse(carrier_region == 'D', 0, 1)) %>%
  group_by(year,
           unique_carrier,
           unique_carrier_name,
           #carrier_region,
           description,
           aircraft_type,
           intl) %>%
  summarise_if(is.numeric,
               sum,
               na.rm = TRUE) %>%
  ungroup() %>%
  filter(description %in% c('Aerospatiale/Aeritalia ATR-42', 'Aerospatiale Caravelle SE-210', 'British Aerospace BAe-146-200', 'Canadair (Bombardier) Challlenger 601'))

intl_domestic_airline <- intl_domestic_seats_fleet %>%
  group_by(year,
           intl) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct_seat_miles = avl_seat_miles_320/sum(avl_seat_miles_320)) %>%
  ungroup() %>%
  select(-aircraft_type)


intl_domestic_airline <- intl_domestic_seats_fleet %>%
  group_by(year,
           unique_carrier,
           unique_carrier_name,
           intl) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  group_by(year,
           unique_carrier,
           unique_carrier_name) %>%
  mutate(pct_seat_miles = avl_seat_miles_320/sum(avl_seat_miles_320)) %>%
  ungroup() 

intl_domestic_seats_fleet %>%
  distinct(description) %>%
  arrange(description) %>%
  as.data.frame()
