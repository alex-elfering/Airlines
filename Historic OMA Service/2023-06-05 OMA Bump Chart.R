# Top Destinations from Eppley Airfield by Seats

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
library(ggbump)

options("device" = "windows")
options(scipen = 999)

# variables for the data  ----
city_var <- 'OMA'

# data cleaning ----
setwd("C:/Users/alexe/OneDrive/Desktop/Domestic Segment")
intl <- list.files("C:/Users/alexe/OneDrive/Desktop/Domestic Segment", pattern = "*.csv")
intl_df <- rbindlist(lapply(intl, fread))

colnames(intl_df) <- tolower(colnames(intl_df))

aircraft <- read.csv('C:/Users/alexe/OneDrive/Desktop/L_AIRCRAFT_TYPE.csv')
colnames(aircraft) <- tolower(colnames(aircraft))

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
  filter(#month == month_var,
         seats > 0,
         passengers > 0,
         departures_performed > 0,
         departures_scheduled > 4)

dest_rank_data <- outbound_flight_details %>%
  filter(carrier %in% c('TW', 'F9', 'G4', 'AA', 'UA', 'DL', 'CO', 'US', 'NW', 'AS', 'WN', 'YX', 'ML', 'HP')) |>
  group_by(year,
           dest) |>
  summarise(total_seats = sum(seats)) |>
  ungroup() |>
  group_by(year) |>
  mutate(seat_rank = dense_rank(desc(total_seats))) |>
  ungroup() |>
  filter(year <= 2000,
         seat_rank <= 10)

airport_order <- dest_rank_data %>%
  group_by(dest) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(year),
          seat_rank)

final_df <- dest_rank_data |>
  #filter(dest %in% airport_order$dest) |>
  mutate(dest = factor(dest, levels = airport_order$dest)) 

final_df |>
  ggplot() + 
  geom_bump(mapping = aes(x = year,
                          y = seat_rank,
                          color = dest),
            color = '#ee6c41',
            size = 1) +
  geom_point(mapping = aes(x = year,
                          y = seat_rank,
                          color = dest),
             size = 2,
             color = '#ee6c41') +
  geom_point(final_df %>% filter(year == 2000),
             mapping = aes(x = year,
                           y = seat_rank,
                           color = dest),
             size = 2,
             shape = 1,
             color = 'black') +
  scale_y_reverse(limits = c(10, 1),
                  breaks = seq(1, 10, by = 1)) +
  scale_x_continuous(limits=c(1990,2000),
                     breaks = seq(1990, 2000, by = 2)) +
  gghighlight(max_highlight = 5L,
              use_direct_label = FALSE,
              unhighlighted_params = list(colour = '#cccccc', alpha = 0.5)) +
  facet_wrap(~dest,
             ncol = 5,
             scales = 'free')  +
  labs(title = 'Omaha Eppley Airfield Top Destinations, 1990-2000',
       subtitle = 'Top 10 destinations by mainline carriers, ranked by total seats',
       caption = 'Visualization by Alex Elfering\nSource: Bureau of Transportation Statistics T-100 Domestic Segment\nNote: Displays mainline airline flight stats') +
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
    legend.position = 'none',
    panel.spacing.x = unit(2, "lines"),
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 12),
    axis.line.x.bottom = element_line(color = 'gray30'),
    axis.line.y.left = ggplot2::element_blank(),
    axis.ticks.y= ggplot2::element_blank(), 
    axis.ticks.x = ggplot2::element_blank(),
    strip.background = element_rect(fill = NA),
    #plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = ggplot2::element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = ggplot2::element_blank()) 

ggsave(file = glue('C:/Users/alexe/OneDrive/Documents/GitHub/Airlines/Historic OMA Service/oma bump.png'), dpi = 300,  width = 16, height = 9, units = c('in'))
