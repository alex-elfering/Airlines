# Northwest Airlines International Service from Minneapolis

library(tidylog)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(sysfonts)
library(ggpattern)
library(ggtext)
library(showtext)
library(gghighlight)
library(glue)

font_add_google("Noto Sans", "Noto Sans")
showtext_auto()

options("device" = "windows")
options(scipen = 999)

# data cleaning ----
setwd("C:/Users/alexe/OneDrive/Desktop/International Segment")
intl <- list.files("C:/Users/alexe/OneDrive/Desktop/International Segment", pattern = "*.csv")
intl_df <- rbindlist(lapply(intl, fread))

colnames(intl_df) <- tolower(colnames(intl_df))

aircraft <- read.csv('C:/Users/alexe/OneDrive/Desktop/L_AIRCRAFT_TYPE.csv')
colnames(aircraft) <- tolower(colnames(aircraft))

# exploration ----

northwestern_msp_intl <- intl_df %>%
  filter(carrier == 'NW',
         origin == 'MSP',
         dest != 'TRF',
         departures_scheduled > 0,
         seats > 0) %>%
  group_by(year,
           dest,
           dest_city_name) %>%
  summarise(departures_performed = sum(departures_performed),
            departures_scheduled = sum(departures_scheduled),
            seats = sum(seats),
            months_served = n_distinct(month)) %>%
  ungroup() %>%
  filter(departures_performed > 5) %>%
  group_by(dest,
           dest_city_name) %>%
  complete(year = seq.int(1990, 2009, 1)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(seats_share = seats/sum(seats, na.rm = TRUE)) %>%
  ungroup() %>%
  separate(dest_city_name, into = c('city', 'country'), sep = ', ') %>%
  mutate(city = case_when(dest == 'SXM' ~ 'St. Maarten',
                                    dest == 'NRT' ~ 'Tokyo-Narita',
                                    dest == 'LIR' ~ 'Liberia (CR)',
                                    dest == 'LHR' ~ 'London Heathrow',
                                    dest == 'LGW' ~ 'London Gatwick',
                                    dest == 'MAN' ~ 'Manchester (UK)',
                                    TRUE ~ city)) %>%
  mutate(airport_dest = paste(city, ' (', dest, ')', sep = ''))

dest_max_year <- northwestern_msp_intl %>%
  filter(!is.na(seats)) %>%
  group_by(dest) %>%
  summarise(max_year = max(year)) %>%
  ungroup() %>%
  mutate(currently_served = ifelse(max_year == 2009, 'Current Service', 'Former')) 

vector_order <- northwestern_msp_intl %>%
  filter(year == 2009) %>%
  replace(is.na(.), 0) %>%
  inner_join(dest_max_year) %>%
  arrange(max_year,
          (seats_share)) %>%
  select(airport_dest)

airport_2009 <- northwestern_msp_intl %>%
  inner_join(dest_max_year) %>%
  filter(year == 2009) %>%
  filter(!is.na(seats))%>%
  arrange(max_year,
          (seats_share)) %>%
  select(airport_dest)

x_axis_color <- ifelse(vector_order$airport_dest %in% airport_2009$airport_dest, 'black', 'gray')
  
nw_df_join <- northwestern_msp_intl %>%
  inner_join(dest_max_year) %>%
  filter(!is.na(seats),
         dest != 'TRF') %>%
  mutate(seats_share_group = case_when(seats_share < 0.01 & seats_share != 0 ~ '<1%',
                                       seats_share >= 0.01 & seats_share < 0.1 ~ '1-10%',
                                       seats_share >= 0.1 & seats_share < 0.2 ~ '10-20%',
                                       seats_share >= 0.2 & seats_share < 0.3 ~ '20-30%',
                                       TRUE ~ '>30%'),
         seats_share_group = factor(seats_share_group, levels = c('<1%', '1-10%','10-20%','20-30%','>30%')),
         currently_served = factor(currently_served, levels = c('Current Service', 'Former')),
         airport_dest = factor(airport_dest, levels = vector_order$airport_dest))

nw_df_join %>%
  ggplot(aes(x = airport_dest,
             y = year,
             fill = seats_share_group,
             pattern = factor(currently_served))) +
  geom_tile(color = 'white',
            size = 1) +
  scale_fill_manual(#name = '% of Outbound Seats',
                    values = c('<1%' = '#dddddd',
                               '1-10%' = '#a5d5d8',
                               '10-20%' = '#73a2c6',
                               '20-30%' = '#4771b2',
                               '>30%' = '#00429d')) +
  geom_tile_pattern(fill = NA,
                    pattern_color = "white",
                    pattern_fill = "white",
                    pattern_angle = 45,
                    pattern_density = 0.05,
                    pattern_spacing = 0.025,
                    pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(
    name = "Current and Former Service",
    values = c(
      'Former' = 'stripe', 
      'Current Service' = 'none'),
    guide = 'none'
  ) +
  scale_y_continuous(limits=c(1990,2010),
                     breaks = seq(1990, 2009, by = 3)) +
  scale_x_discrete(position = "top",
                   breaks = vector_order$airport_dest) +
  coord_flip() +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Northwest Airlines Minneapolis Int'l Service 1990-2009",
       subtitle = "Based on total % of outbound seats annually.",
       caption = 'Source: Bureau of Transportation Statistics T-100 International Segment\nNote:Only examines mainline fleet and does not account for regional partners\nVisualization by Alex Elfering',
       fill = '',
       x = '',
       y = '') +
  theme(plot.title = element_text(face = 'bold',
                                  family = 'Arial', 
                                  size = 16),
        plot.subtitle = element_text(size = 14,
                                     family = 'Arial'),
        legend.position = 'top',
        strip.placement = "outside",
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title = element_text(size = 10,
                                    color = 'black',
                                    face = 'bold',
                                    family = 'Arial'),
        legend.text = element_text(size = 10,
                                   color = 'black',
                                   family = 'Arial'),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 10, 
                                    color = "#c1c1c1",
                                    family = 'Arial', 
                                    hjust = 0),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10,
                                   family = 'Arial'),
        axis.text.y = element_text(size = 10,
                                   colour = x_axis_color,
                                   family = 'Arial'),
        axis.line.x.bottom = element_blank(),
        axis.line.y.right = element_line(color = 'gray30'),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12,
                                  color = 'black',
                                  face = 'bold',
                                  family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.spacing = unit(2, "lines"))