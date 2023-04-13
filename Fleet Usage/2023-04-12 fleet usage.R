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
  filter(!description %in% c('Canadair RJ-200ER /RJ-440', 
                             'Aerospatiale/Aeritalia ATR-42',
                             'Aerospatiale Caravelle SE-210', 
                             'British Aerospace BAe-146-200', 
                             'Canadair (Bombardier) Challlenger 601', 
                             'Gates Learjet Lear-31/35/36', 
                             'Gates Learjet Lear-25', 
                             'Fokker Friendship F-27/Fairchild F-27/A/B/F/J', 
                             'Fokker F28-4000/6000 Fellowship', 
                             'Fokker F28-1000 Fellowship', 
                             'Embraer EMB-120 Brasilia', 
                             'Dornier 328', 
                             'Embraer 190', 
                             'Embraer-Emb-170' )) %>%
  mutate(twin_aisle = case_when( grepl('A300', description) | 
                                   grepl('A310', description) | 
                                   grepl('A330', description) | 
                                   grepl('A350', description) | 
                                   grepl('B787', description) | 
                                   grepl('747', description)| 
                                   grepl('767', description)| 
                                   grepl('777', description) | 
                                   grepl('Lockheed L-1011', description) | 
                                   grepl('DC-10', description) | 
                                   grepl('MD-11', description) ~ 1,
                                 TRUE ~ 0))

intl_domestic_seats_fleet %>%
  filter(intl == 0) %>%
  group_by(unique_carrier,
           year,
           twin_aisle) %>%
  summarise(asm = sum(avl_seat_miles_320)) %>%
  ungroup() %>%
  group_by(year,
           unique_carrier) %>%
  mutate(pct_asm = asm/sum(asm)) %>%
  ungroup() %>%
  filter(twin_aisle == 1) %>%
  ggplot() + 
  geom_line(mapping = aes(x = year,
                          y = pct_asm,
                          color = unique_carrier))
