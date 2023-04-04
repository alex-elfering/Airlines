# DL Fleet Optimization at FL Airports 2003-2022

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
florida <- list.files("C:/Users/alexe/OneDrive/Desktop/Florida Segment", pattern = "*.csv")
florida_df <- rbindlist(lapply(florida, fread))

colnames(florida_df) <- tolower(colnames(florida_df))

aircraft <- read.csv('C:/Users/alexe/OneDrive/Desktop/L_AIRCRAFT_TYPE.csv')
colnames(aircraft) <- tolower(colnames(aircraft))

florida_df %>%
  filter(departures_performed > 0,
         carrier == 'DL',
         origin %in% c('MIA', 'FLL', 'MCO', 'RSW', 'TPA', 'JAX')) %>%
  inner_join(aircraft, by = c('aircraft_type' = 'code')) %>%
  mutate(average_seats = seats/departures_performed) %>%
  group_by(carrier,
           year,
           origin) %>%
  summarise(average_capacity = mean(average_seats)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(mapping = aes(x = year,
                          y = average_capacity,
                          color = origin),
            size = 2) +
  labs(title = 'Average Outbound Capacity',
       subtitle = 'Seats divided by departures performed') +
  facet_wrap(~origin)
