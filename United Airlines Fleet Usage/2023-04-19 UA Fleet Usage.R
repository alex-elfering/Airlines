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
library(ggstream)
library(ggrepel)
library(gghighlight)

font_add_google("Noto Sans", "Noto Sans")
showtext_auto()

options("device" = "windows")
options(scipen = 999)

# data cleaning ----
setwd("C:/Users/alexe/OneDrive/Desktop")
airline_t2 <- read.csv("C:/Users/alexe/OneDrive/Desktop/airline t2.csv")

colnames(airline_t2) <- tolower(colnames(airline_t2))

aircraft <- read.csv('C:/Users/alexe/OneDrive/Desktop/L_AIRCRAFT_TYPE.csv')
colnames(aircraft) <- tolower(colnames(aircraft))

# data prep ----

intl_domestic_seats_fleet <- airline_t2 %>%
  filter(unique_carrier %in% c('UA')) %>%
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
                             'Convair CV-340/440',
                             'Convair CV-580',
                             'Dornier 328', 
                             'De Havilland DHC7 Dash-7',
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

shorter_plane_names <- intl_domestic_seats_fleet %>%
  distinct(description) %>%
  as.data.frame() %>%
  mutate(group = case_when( grepl('737-3', description) | grepl('737-5', description) | grepl('737-1', description) | grepl('737-2', description) | grepl('737-4', description) ~ 'B737 Classic',
                            grepl('737-7', description) | grepl('737-8', description) | grepl('737-9', description) | grepl("B737", description) ~ 'B737 NG/MAX',
                            grepl('Super 80', description) | grepl('MD-90', description) | grepl('Super 87', description) ~ 'MD-80 series/87/90',
                            grepl('A300', description) ~ 'A300',
                            grepl('DC-10', description) ~ 'DC10',
                            grepl('DC-8', description) ~ 'DC8',
                            grepl('747', description) ~ 'B747',
                            grepl('MD-11', description) ~ 'MD11',
                            grepl('727', description) ~ 'B727',
                            grepl('717', description) ~ 'B717',
                            grepl('A330', description) ~ 'A330',
                            grepl('A310', description) ~ 'A310',
                            grepl('757', description) ~ 'B757',
                            grepl('767', description) ~ 'B767',
                            grepl('777', description) ~ 'B777',
                            grepl('787', description) ~ 'B787',
                            grepl('L-1011', description) ~ 'L1011',
                            grepl('DC-9', description) ~ 'DC9',
                            grepl('Fokker 100', description) ~ 'Fokker 100',
                            grepl('Embraer 190', description) ~ 'Embraer 190',
                            grepl('A220', description) | grepl('A200', description) ~ 'A220',
                            grepl('A350', description) ~ 'A350',
                            grepl('A319', description) | grepl('A320', description) | grepl('A321', description) | grepl('A-318', description) ~ 'A320 Family/neo'
  )) %>%
  arrange(description)

# fleet utilization ----

time_series_ua <- intl_domestic_seats_fleet %>%
  filter(intl == 1) %>%
  left_join(shorter_plane_names) %>%
  group_by(year,
           group,
           twin_aisle) %>%
  summarise(asm = sum(avl_seat_miles_320)) %>%
  ungroup() %>%
  arrange(year, twin_aisle) %>%
  group_by(group,
           twin_aisle) %>%
  complete(year = seq(1991, 2022)) %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  group_by(year) %>%
  mutate(share_asm = asm/sum(asm)) %>%
  ungroup()

time_series_ua %>%
  ggplot(aes(x = year,
             y = share_asm,
             group = group,
             fill = group)) + 
  geom_bar(stat = 'identity',
           position = 'stack',
           width = 1,
           color = 'white',
           size = 1) +
  facet_wrap(~group) +
  gghighlight(year > 0) 
