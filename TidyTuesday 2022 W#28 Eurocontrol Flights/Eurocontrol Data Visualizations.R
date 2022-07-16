# create a data visualization that shows how a country's airports have recovered from the outset of the COVID-19 pandemic
# #TidyTuesday 2022 Week 28
# code by alex elfering

library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)
library(ggpattern)

# data load
tuesdata <- tidytuesdayR::tt_load('2022-07-12')
tuesdata <- tidytuesdayR::tt_load(2022, week = 28)

airports <- flights %>%
  filter(STATE_NAME == 'Italy') %>%
  distinct(APT_ICAO)

flt_data <- flights %>%
  select(country = STATE_NAME,
         airport = APT_NAME,
         icao = APT_ICAO,
         month = MONTH_NUM,
         year = YEAR,
         date = FLT_DATE,
         total_flights = FLT_TOT_1) %>%
  mutate(date = lubridate::ymd(date),
         month = as.numeric(month)) %>%
  group_by(airport,
           icao) %>%
  complete(date = seq.Date(as.Date('2016-01-01'), as.Date('2022-05-31'), by = 'day')) %>%
  mutate(month = month(date),
         year = year(date),
         total_flights = ifelse(is.na(total_flights), 0, total_flights)) %>%
  ungroup() %>%
  group_by(#country,
           airport,
           icao,
           month,
           year) %>%
  summarise(flights = sum(total_flights)) %>%
  ungroup()

flt_data19 <- flt_data %>%
  filter(year == 2019) %>%
  select(-year) %>%
  rename(flights_19 = flights)

cut_yoy <- seq(0, 2, 0.25)

flt_yoy <- flt_data %>%
  filter(year > 2019) %>%
  inner_join(flt_data19) %>%
  mutate(flights = ifelse(flights_19 == 0, 0, flights),
         yoy = flights/flights_19,
         yoy = ifelse(is.nan(yoy), 0, yoy)) %>%
  #filter(icao == 'LATI') %>%
  mutate(month_year = as.Date(paste(year, month, '1', sep = '-')),
         month_name = substr(month.name[month],1,1),
         group_yoy = as.character((cut(yoy, breaks = cut_yoy))),
         group_yoy = case_when(yoy == 0 & is.na(group_yoy) ~ 'No flights', 
                               yoy > 1 ~ '+100%',
                               group_yoy == '(0,0.25]' ~ '0-25%',
                               group_yoy == '(0.25,0.5]' ~ '25-50%',
                               group_yoy == '(0.5,0.75]' ~ '50-75%',
                               group_yoy == '(0.75,1]' ~ '75-100%',
                               TRUE ~ group_yoy),
         airport = ifelse(icao == 'LIML', paste0('*', airport), airport)) %>%
  #filter(icao == 'LIML')
  mutate(group_yoy = factor(group_yoy, levels = c('No flights',
                                                  '0-25%',
                                                  '25-50%',
                                                  '50-75%',
                                                  '75-100%',
                                                  '+100%'))) %>%
  arrange(icao,
          month_year) %>%
  inner_join(airports,
             by = c('icao' = 'APT_ICAO')) %>%
  filter(icao != 'LICJ') %>%
  mutate(year = as.numeric(substr(year, 3, 4)))

airport_order <- flt_yoy %>%
  filter(month_year == max(month_year)) %>%
  arrange(desc(yoy)) %>%
  select(airport)

flt_order <- flt_yoy %>%
  mutate(airport = factor(airport, levels = airport_order$airport))

fl_plot <- flt_order %>%
  ggplot() + 
  geom_tile(mapping = aes(x = factor(month),
                          y = year,
                          fill = group_yoy),
            color = 'white',
            size = 1) +
  geom_tile_pattern(flt_order %>% filter(group_yoy == 'No flights'),
                    mapping = aes(x = factor(month),
                                  y = year),
                    fill = NA,
                    pattern_color = "white",
                    pattern_fill = "white",
                    pattern_angle = 45,
                    pattern_density = 0.1,
                    pattern_spacing = 0.05,
                    pattern_key_scale_factor = 0.5) +
  geom_tile_pattern(flt_order %>% filter(month %in% c(7, 10), icao == 'LIML'),
                    mapping = aes(x = factor(month),
                                  y = year),
                    fill = NA,
                    pattern_color = "white",
                    pattern_fill = "white",
                    pattern_angle = 45,
                    pattern_density = 0.1,
                    pattern_spacing = 0.05,
                    pattern_key_scale_factor = 0.5) +
  scale_fill_manual(values = c('No flights' = 'gray75',
                               '0-25%' = '#d7191c',
                               '25-50%' = '#fdae61',
                               '50-75%' = '#ffffbf',
                               '75-100%' = '#abd9e9',
                               '+100%' = '#2c7bb6')) +
  #coord_equal() +
  scale_y_reverse() +
  facet_wrap(~airport,
             ncol = 3,
             scales = 'free') +
  labs(title = 'Italian Airport Movements',
       subtitle = 'Percent of total IFR movements vs. same month in 2019 among Italian airports',
       fill = 'Legend:',
       x = '',
       y = '',
       caption = 'Source: Eurocontrol | Total IFR movements from Network Manager was utilized\nData Visualization by Alex Elfering | Tidy Tuesday 2022 Week #28\n*Milan-Linate was closed from 27-July to 27-October for runway resurfacing and other upgrades, impacting movements') +
  theme(plot.title = element_text(face = 'bold', size = 24),
        plot.subtitle = element_text(size = 16),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 10, color = '#969696'),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = '#969696'),
        axis.text.x.bottom = element_text(size = 10, color = '#969696'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        #axis.line = element_line(colour = "#222222", linetype = "dashed"),
        axis.line = element_blank(),
        panel.grid.major.x = element_blank()
        ) 

ggsave('fl_plot.png',
       fl_plot,
       width = 8,
       height = 10,
       units = c('in'))

#write.csv(flt_yoy, 'flt_yoy.csv')
