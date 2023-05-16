# the percentage of flights cancelled at Des Moines
# huge help thanks to: https://thisisdaryn.netlify.app/post/making-a-calendar-visualization-with-ggplot2/

library(tidylog)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(ggpattern)
library(sysfonts)
library(ggtext)
library(showtext)
library(airportr)
library(stringr)
library(glue)

#font_add_google("Orbitron", "Orbitron")
#showtext_auto()

options("device" = "windows")
options(scipen = 999)

# data cleaning ----

setwd("C:/Users/alexe/OneDrive/Desktop/Full Schedule Data")
sched_list <- list.files("C:/Users/alexe/OneDrive/Desktop/Full Schedule Data", pattern = "*.csv")
schedule_df <- rbindlist(lapply(sched_list, fread))

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

colnames(schedule_df) <- trim(tolower(colnames(schedule_df)))

clean_schedule_df <- schedule_df %>%
  dplyr::select(fl_date = flightdate,
                mkt_unique_carrier = marketing_airline_network,
                op_unique_carrier = operating_airline,
                mkt_carrier_fl_num = flight_number_marketing_airline,
                origin,
                dest,
                crs_dep_time = crsdeptime,
                crs_arr_time = crsarrtime,
                crs_elapsed_time = crselapsedtime,
                dep_delay = depdelayminutes,
                taxi_out = taxiout,
                wheels_off = wheelsoff,
                wheels_on = wheelson,
                taxi_in = taxiin,
                arr_delay = arrdelay,
                cancelled,
                flights,
                distance,
                carrier_delay = carrierdelay,
                weather_delay = weatherdelay,
                nas_delay = nasdelay,
                security_delay = securitydelay,
                late_aircraft_delay = lateaircraftdelay) %>%
  filter(mkt_unique_carrier %in% c('AS', 'AA', 'DL', 'F9', 'HA', 'B6', 'WN', 'NK', 'UA', 'G4', 'SY')) %>%
  mutate(crs_dep = case_when(nchar(crs_dep_time) == 1 ~ paste0('000', crs_dep_time),
                             nchar(crs_dep_time) == 2 ~ paste0('00', crs_dep_time),
                             nchar(crs_dep_time) == 3 ~ paste0('0', crs_dep_time),
                             TRUE ~ as.character(crs_dep_time))) %>%
  mutate(crs_arr = case_when(nchar(crs_arr_time) == 1 ~ paste0('000', crs_arr_time),
                             nchar(crs_arr_time) == 2 ~ paste0('00', crs_arr_time),
                             nchar(crs_arr_time) == 3 ~ paste0('0', crs_arr_time),
                             TRUE ~ as.character(crs_arr_time))) %>%
  mutate(fl_date = ymd((fl_date)),
         crs_dep = format(strptime(crs_dep, format = "%H%M"), format = "%H:%M"),
         crs_arr = format(strptime(crs_arr, format = "%H%M"), format = "%H:%M")) %>%
  mutate(crs_dep_date = ymd_hm(paste(fl_date, crs_dep) )) %>%
  mutate(init_arrival = (crs_dep_date+minutes(crs_elapsed_time)),
         init_arrival_extract = format(init_arrival, '%H:%M')) %>%
  mutate(init_hour = as.numeric(substr(init_arrival_extract, 1, 2)),
         actu_hour = as.numeric(substr(crs_arr, 1, 2)),
         adj = init_hour-actu_hour) %>%
  mutate(new_adj = case_when(adj >= 20 ~ adj - 24,
                             adj <= -20 ~ adj + 24,
                             TRUE ~ adj),
         final_arrival = init_arrival - hours(new_adj),
         delayed = ifelse(dep_delay >= 15, 1, 0)) %>%
  dplyr::select(fl_date,
                mkt_unique_carrier,
                op_unique_carrier,
                mkt_carrier_fl_num,
                origin,
                dest,
                dep_delay,
                crs_dep_date,
                taxi_out,
                wheels_off,
                arr_delay,
                crs_arr_date = final_arrival,
                wheels_on,
                taxi_in,
                crs_elapsed_time,
                carrier_delay,
                weather_delay,
                nas_delay,
                security_delay,
                late_aircraft_delay,
                delayed,
                flights,
                cancelled) %>% 
  as.tibble() %>%
  distinct() %>%
  arrange(crs_dep_date) 

# data on cancelled and non-cancelled flights ----
flights_not_cancelled <- dplyr::filter(clean_schedule_df, cancelled != 1)

# calendar visualization for cancellations  ----
wday_vec <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

month_vec <- c("January", "February", "March", "April", "May", "June",
               "July", "August", "September", "October", "November", "December") 

cancelled_totals <- clean_schedule_df %>%
  filter(origin %in% c('DSM')) %>%
  group_by(fl_date,
           origin) %>%
  summarise(flights = sum(flights),
            cancelled = sum(cancelled)) %>%
  ungroup() %>%
  complete(fl_date = seq.Date(min(fl_date), max(fl_date), by = 'day')) %>%
  replace(is.na(.), 0) %>%
  mutate(rate = cancelled/flights) %>%
  mutate(wday = str_sub(weekdays(fl_date), 1, 3),
         month_day = day(fl_date),
         month = month(fl_date),
         week_increment = ifelse(month_day == 1 | wday == "Sun", 1, 0)) %>%
  group_by(month) %>% 
  mutate(week = cumsum(week_increment),
         text_month = months(fl_date)) %>% 
  ungroup() %>%
  mutate(wday = factor(wday, levels = wday_vec),
         text_month = factor(text_month, levels = month_vec)) %>%
  mutate(cancelled_group = case_when(rate == 0 ~ '0%',
                                   rate > 0 & rate <= 0.10 ~ '1-10%',
                                   rate > 0.1 & rate <= 0.2 ~ '10-20%',
                                   rate > 0.2 & rate <= 0.3 ~ '20-30%',
                                   TRUE ~ '>=30%'),
         cancelled_grop = factor(cancelled_group, levels = c('0%', '1-10%', '10-20%', '20-30%', '>=30%')),
         day_int = wday(fl_date),
         row = row_number())

cancelled_totals %>%
  ggplot(aes(x = day_int, 
             y = week,
             fill = cancelled_group,
             pattern = cancelled_group), 
         colour = "white") + 
  geom_tile(colour = "white",
            size = 1) +
  geom_tile_pattern(fill = NA,
                    pattern_color = "white",
                    pattern_fill = "white",
                    pattern_angle = 45,
                    pattern_density = 0.1,
                    pattern_spacing = 0.025,
                    pattern_key_scale_factor = 0.5) +
  geom_rect(cancelled_totals %>% 
              filter(fl_date >= as.Date('2022-12-22') & fl_date <= as.Date('2022-12-24')) %>%
              filter(fl_date == min(fl_date) | fl_date == max(fl_date)) ,
            mapping = aes(xmin = min(day_int)-0.5,
                          xmax = max(day_int)+0.5,
                          ymin = week-0.5,
                          ymax = week+0.5),
            size = 0.5,
            fill = NA,
            color = 'black') +
  geom_rect(cancelled_totals %>% 
              filter(fl_date >= as.Date('2022-09-04') & fl_date <= as.Date('2022-09-17')) %>%
              filter(fl_date == min(fl_date) | fl_date == max(fl_date)) ,
            mapping = aes(xmin = min(day_int)-0.5,
                          xmax = max(day_int)+0.5,
                          ymin = min(week)-0.5,
                          ymax = max(week)+0.5),
            size = 0.5,
            fill = NA,
            color = 'black') +
  geom_rect(cancelled_totals %>% 
              filter(fl_date >= as.Date('2022-11-13') & fl_date <= as.Date('2022-11-26')) %>%
              filter(fl_date == min(fl_date) | fl_date == max(fl_date)) ,
            mapping = aes(xmin = min(day_int)-0.5,
                          xmax = max(day_int)+0.5,
                          ymin = min(week)-0.5,
                          ymax = max(week)+0.5),
            size = 0.5,
            fill = NA,
            color = 'black') +
  geom_rect(cancelled_totals %>% 
              filter(fl_date >= as.Date('2022-11-27') & fl_date <= as.Date('2022-11-30')) %>%
              filter(fl_date == min(fl_date) | fl_date == max(fl_date)) ,
            mapping = aes(xmin = min(day_int)-0.5,
                          xmax = max(day_int)+0.5,
                          ymin = min(week)-0.5,
                          ymax = max(week)+0.5),
            size = 0.5,
            fill = NA,
            color = 'black') +
  geom_rect(cancelled_totals %>% 
              filter(fl_date >= as.Date('2022-12-04') & fl_date <= as.Date('2022-12-10')) %>%
              filter(fl_date == min(fl_date) | fl_date == max(fl_date)) ,
            mapping = aes(xmin = min(day_int)-0.5,
                          xmax = max(day_int)+0.5,
                          ymin = min(week)-0.5,
                          ymax = max(week)+0.5),
            size = 0.5,
            fill = NA,
            color = 'black') +
  geom_rect(cancelled_totals %>% 
              filter(fl_date >= as.Date('2022-12-01') & fl_date <= as.Date('2022-12-03')) %>%
              filter(fl_date == min(fl_date) | fl_date == max(fl_date)) ,
            mapping = aes(xmin = min(day_int)-0.5,
                          xmax = max(day_int)+0.5,
                          ymin = min(week)-0.5,
                          ymax = max(week)+0.5),
            size = 0.5,
            fill = NA,
            color = 'black') +
  geom_segment(mapping = aes(x = max(day_int)+0.5, 
                             y = max(week)-0.5, 
                             xend = max(day_int)+0.5, 
                             yend = max(week)+0.5), 
               data = cancelled_totals %>% 
                 filter(fl_date >= as.Date('2022-11-27') & fl_date <= as.Date('2022-11-30')) %>%
                 filter(fl_date == min(fl_date) | fl_date == max(fl_date)),
               color = 'white',
               size = 0.5) +
  geom_segment(mapping = aes(x = min(day_int)-0.5, 
                             y = max(week)+0.5, 
                             xend = max(day_int)+0.5, 
                             yend = max(week)+0.5), 
               data = cancelled_totals %>% 
                 filter(fl_date >= as.Date('2022-11-20') & fl_date <= as.Date('2022-11-26')) %>%
                 filter(fl_date == min(fl_date) | fl_date == max(fl_date)),
               color = 'white',
               size = 0.5) +
  geom_segment(mapping = aes(x = min(day_int)-0.5, 
                             y = max(week)+0.5, 
                             xend = max(day_int)+0.5, 
                             yend = max(week)+0.5), 
               data = cancelled_totals %>% 
                 filter(fl_date >= as.Date('2022-11-27') & fl_date <= as.Date('2022-11-30')) %>%
                 filter(fl_date == min(fl_date) | fl_date == max(fl_date)),
               color = 'white',
               size = 0.5) +
  geom_segment(mapping = aes(x = min(day_int)-0.5, 
                             y = max(week)-0.5, 
                             xend = max(day_int)+0.5, 
                             yend = min(week)-0.5), 
               data = cancelled_totals %>% 
                 filter(fl_date >= as.Date('2022-12-01') & fl_date <= as.Date('2022-12-03')) %>%
                 filter(fl_date == min(fl_date) | fl_date == max(fl_date)),
               color = 'white',
               size = 0.5) +
  geom_segment(mapping = aes(x = min(day_int)-0.5, 
                             y = max(week)-0.5, 
                             xend = max(day_int)+0.5, 
                             yend = min(week)-0.5), 
               data = cancelled_totals %>% 
                 filter(fl_date >= as.Date('2022-12-04') & fl_date <= as.Date('2022-12-10')) %>%
                 filter(fl_date == min(fl_date) | fl_date == max(fl_date)),
               color = 'white',
               size = 0.5) +
  geom_segment(mapping = aes(x = min(day_int)-0.5, 
                             y = min(week)+0.5, 
                             xend = min(day_int)-0.5, 
                             yend = max(week)-0.5), 
               data = cancelled_totals %>% 
                 filter(fl_date >= as.Date('2022-12-01') & fl_date <= as.Date('2022-12-03')) %>%
                 filter(fl_date == min(fl_date) | fl_date == max(fl_date)),
               color = 'white',
               size = 0.5) +
  facet_wrap(~text_month, 
             scales = "free",
             nrow = 3,
             ncol = 4) +
  scale_fill_manual(name = '% of Cancelled Flights: ',
                    values = c('0%' = 'gray85', 
                               '1-10%' = '#fdbb84', 
                               '10-20%' = '#fc8d59', 
                               '20-30%' = '#e34a33', 
                               '>=30%' = '#b30000'),
                    breaks = c('0%', '1-10%', '10-20%', '20-30%', '>=30%')) +
  scale_pattern_manual(
    name = "% of Cancelled Flights: ",
    values = c(
      '0%' = 'stripe', 
      '1-10%' = 'none', 
      '10-20%' = 'none', 
      '20-30%' = 'none', 
      '>=30%' = 'none'),
    breaks = c('0%', '1-10%', '10-20%', '20-30%', '>=30%')
  ) +
  scale_x_continuous(breaks = seq(1, 7, 1),
                     labels = wday_vec,
                     position = 'top') +
  labs(title = 'Cancelled Des Moines Flights in 2022',
       subtitle = 'The daily percentage of flights cancelled, broken out by week and month',
       caption = 'Source: On-Time Marketing Carrier On-Time Performance from the Bureau of Transporation Statistics\nVisualization by Alex Elfering',
       fill = '% of Cancelled Flights: ',
       x = '',
       y = '') +
  scale_y_reverse() +
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
        #axis.text.x = ifelse(cancelled_totals$text_month == 'January', element_text(size = 10), element_blank()),
        axis.text.x = element_text(size = 10,
                                   family = 'Arial'),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
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

ggsave(file = glue('C:/Users/alexe/OneDrive/Desktop/cancelled.png'), dpi = 300,  width = 13, height = 9, units = c('in'))


