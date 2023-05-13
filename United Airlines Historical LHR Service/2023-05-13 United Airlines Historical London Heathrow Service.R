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
library(gghighlight)
library(threejs)
library(globe)
library(glue)

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
origin_order <- c('JFK', 'SEA', 'BOS', 'MIA', 'DEN', 'LAX', 'IAH', 'ORD', 'IAD', 'SFO', 'EWR')

origin_order <- rev(origin_order)

intl_df %>%
  filter(dest_country != 'US',
         carrier == 'UA',
         departures_scheduled > 0,
         #origin == 'BOS',
         dest  %in% c('LHR')) %>%
  group_by(year,
           aircraft_type,
           origin) %>%
  summarise(seats = sum(seats)) %>%
  ungroup() %>%
  inner_join(aircraft,
             by = c('aircraft_type' = 'code')) %>%
  as.data.frame() %>%
  filter(description == 'Boeing 747SP')

ua_lhr <- intl_df %>%
  filter(dest_country != 'US',
         carrier == 'UA',
         departures_scheduled > 0,
         dest  %in% c('LHR')) %>%
  group_by(year,
           carrier,
           origin,
           dest) %>%
  summarise(seats = sum(seats)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(share = seats/sum(seats)) %>%
  ungroup() %>%
  filter(share >= 0.005) %>%
  group_by(year) %>%
  mutate(share = seats/sum(seats),
         origin = factor(origin, levels = origin_order)) %>%
  ungroup() %>%
  group_by(carrier,
           origin,
           dest) %>%
  complete(year = seq(1991, 2022, 1)) %>%
  ungroup() 

max_share <- max(ua_lhr$share, na.rm = TRUE)

ua_lhr %>%
  mutate(origin = factor(origin, levels = origin_order)) %>%
  ggplot() +
  geom_vline(xintercept = 2012,
             size = 0.5,
             linetype = 'dashed') +
  geom_step(mapping = aes( x= year,
                           y = share,
                           group = origin),
            color = 'navy',
            size = 1) +
  geom_point(mapping = aes( x= year,
                            y = share,
                            group = origin),
             color = 'navy',
             size = 1.25) +
  geom_point(mapping = aes( x= year,
                            y = share,
                            group = origin),
             color = 'white',
             size = 0.5) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = 'gray85', alpha = 0.3)) +
  scale_y_continuous(labels = c('0', '10', '20', '30', '40%'),
                     limits = c(0,max_share*1.1),
                     expand = c(0, 0.000001)
  ) +
  scale_x_continuous(labels = c("'90", "00", "10", "20")) +
  facet_wrap(~origin,
             scales = 'free_x') +
  labs(title = 'United Airlines Seat Capacity to London Heathrow',
       subtitle = 'The share of outbound seats on United Airlines from the United States to London Heathrow by origin airport between 1991-2022*',
       caption = '\n*As of October 2022\nSource: Bureau of Transportation Statistics T-100 International Segment\nVisualization by Alex Elfering') +
  theme(
    plot.title = element_text(face = 'bold', 
                              size = 14, 
                              family = 'Noto Sans'),
    plot.subtitle = element_markdown(),
    plot.caption = element_text(size = 8,
                                family = 'Noto Sans',
                                color = 'gray30',
                                hjust = 0),
    axis.title =  ggplot2::element_blank(),
    axis.text.x = element_text(size = 10, 
                               face = 'bold', 
                               color = 'gray30',
                               #face= bold_label,
                               family = 'Noto Sans'),
    axis.text.y = element_text(size = 10,
                               face = 'bold', 
                               color = 'gray30',
                               family = 'Noto Sans'),
    strip.text = ggplot2::element_text(size = 12, 
                                       face = 'bold',
                                       hjust = 0.5, 
                                       family = 'Noto Sans'),
    plot.title.position = "plot", 
    plot.caption.position = 'plot',
    legend.position = 'top',
    panel.spacing.x = unit(2, "lines"),
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 12),
    axis.line.x.bottom = element_line(color = 'gray30'),
    axis.line.y.left = element_line(color = 'gray30'),
    axis.ticks.y= ggplot2::element_blank(), 
    axis.ticks.x = ggplot2::element_blank(),
    strip.background = element_rect(fill = NA),
    #plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = ggplot2::element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = ggplot2::element_blank()) 

ggsave(file = glue('C:/Users/alexe/OneDrive/Desktop/UA LHR Service.png'), dpi = 300,  width = 16, height = 9, units = c('in'))
