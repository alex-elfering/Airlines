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

# preparing the data  ----
aircraft_seats <- florida_df %>%
  filter(departures_scheduled > 0,
         carrier == 'DL',
         origin %in% c('MIA', 'FLL', 'MCO', 'RSW', 'TPA', 'JAX')) %>%
  group_by(unique_carrier_name,
           origin,
           year,
           aircraft_type) %>%
  summarise(seats = sum(seats),
            passengers = sum(passengers),
            departures = sum(departures_performed)) %>%
  ungroup() %>%
  inner_join(aircraft, by = c('aircraft_type' = 'code')) %>%
  group_by(year,
           origin) %>%
  mutate(seat_share = seats/sum(seats),
         pax_share = passengers/sum(passengers),
         dep_share = departures/sum(departures)) %>%
  ungroup() 

aircraft_complete <- aircraft_seats %>%
  select(origin,
         year,
         description,
         seat_share,
         pax_share,
         dep_share) %>%
  group_by(origin,
           description) %>%
  complete(year = seq(2003, 2022, by = 1)) %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  mutate(aircraft_groups = case_when(grepl('767', description) | grepl('777', description) ~ 'Boeing 767/777',
                                     grepl('MD-11', description) ~ 'MD-11',
                                     grepl('757', description) ~ 'Boeing 757',
                                     grepl('Super 80', description) | grepl('MD-90', description) ~ 'MD-88/90',
                                     grepl('Airbus', description) ~ 'Airbus a320 family',
                                     grepl('Boeing 737', description) ~ 'Boeing 737',
                                     TRUE ~ 'Other')) %>%
  group_by(origin,
           year,
           aircraft_groups) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  mutate(aircraft_groups = factor(aircraft_groups, levels = c('MD-11', 'Boeing 767/777', 'MD-88/90','Boeing 757',  'Boeing 737', 'Airbus a320 family', 'Other'))) %>%
  mutate(highlight_year = ifelse(year %in% c(2005, 2010, 2020), 1, 0)) 

year_df <- aircraft_complete %>%
  distinct(year, origin, highlight_year)

# final plot  ----
final_plot <- aircraft_complete %>%
  #filter(origin == 'MIA') %>%
  ggplot() + 
  geom_bar(mapping = aes(x = year,
                         y = seat_share,
                         fill = aircraft_groups),
           width = 1,
           stat = 'identity',
           position = 'stack') +
  geom_bar(year_df,
           mapping = aes(x = year,
                         y = highlight_year),
           size = 1,
           fill = NA,
           color = 'black',
           linetype = 'dashed',
           width = 1,
           stat = 'identity',
           position = 'stack') +
  geom_text(year_df %>% filter(year == 2005, origin == 'FLL'),
            mapping = aes(label='Bankruptcy',
                          x = year,
                          y = highlight_year), 
            position=position_dodge(width=0.9), 
            family = 'Noto Sans',
            vjust=-0.25) +
  geom_text(year_df %>% filter(year == 2010, origin == 'FLL'),
            mapping = aes(label='NW Merger',
                          x = year,
                          y = highlight_year), 
            position=position_dodge(width=0.9), 
            family = 'Noto Sans',
            vjust=-0.25) +
  geom_text(year_df %>% filter(year == 2020, origin == 'FLL'),
            mapping = aes(label='Covid-19',
                          x = year,
                          y = highlight_year), 
            position=position_dodge(width=0.9), 
            family = 'Noto Sans',
            vjust=-0.25) +
  facet_wrap(~origin,
             scales = 'free',
             nrow = 2) +
  scale_fill_manual(values = c('MD-11' = '#b35806', 
                               'Boeing 767/777' = '#e08214', 
                               'MD-88/90' = '#d8daeb',
                               'Boeing 757' = '#b2abd2', 
                               'Boeing 737' = '#8073ac', 
                               'Airbus a320 family' = '#542788',  
                               'Other' = 'gray85')) +
  labs(title = "Delta Air Lines Fleet Utilization",
       fill = '',
       subtitle = "<span style = 'font-size:13pt'>The share of outbound seats from Florida airports between <b><span style = 'color:#b35806;'>wide-body</span></b> and <b><span style = 'color:#542788;'>narrow-body</span></b></span> planes 2003-2022.",
       caption = '\nVisualization by Alex Elfering; Bureau of Transportation Statistics T-100 Domestic Segment\nNote: Boeing 757 includes the 757-200 and 757-300; Boeing 767/777 includes the 767-200, 767-300, and 777-200; Boeing 737 includes the 737-200, 737-300, 737-700, 737-800 and 737-900;\na320 Family includes the a319-200, a320-200, and a321-200') +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = c('0', '25', '50', '75', '100%'),
                     limits = c(0,1.1),
                     expand = c(0, 0.000001)
                     ) +
  expand_limits(y = 1.18) + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.5,
             linetype = 'dashed') +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    plot.title = element_text(face = 'bold', 
                              size = 14, 
                              family = 'Noto Sans'),
    plot.subtitle = element_markdown(),
    plot.caption = element_text(size = 10,
                                family = 'Noto Sans',
                                hjust = 0),
    axis.title =  ggplot2::element_blank(),
    axis.text.x = element_text(size = 12, 
                               #face= bold_label,
                               family = 'Noto Sans'),
    axis.text.y = element_text(size = 12, 
                               family = 'Noto Sans'),
    strip.text = ggplot2::element_text(size = 12, 
                                       face = 'bold',
                                       hjust = 0, 
                                       family = 'Noto Sans'),
    plot.title.position = "plot", 
    plot.caption.position = 'plot',
    legend.position = 'top',
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 12),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_blank(),
    axis.ticks.y= ggplot2::element_blank(), 
    axis.ticks.x = ggplot2::element_blank(),
    strip.background = element_rect(fill = NA),
    #plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = ggplot2::element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = ggplot2::element_blank()) 

ggsave(final_plot, file="final_plot.png", width=6, height=4, units = 'in', dpi = 500)
final_plot
