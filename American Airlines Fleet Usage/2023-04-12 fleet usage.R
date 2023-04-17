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
  filter(unique_carrier %in% c('AA')) %>%
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
                            grepl('Super 80', description) | grepl('MD-90', description) | grepl('Super 87', description) ~ 'MD-80 series/90',
                            grepl('A300', description) ~ 'A300',
                            grepl('DC-10', description) ~ 'DC-10',
                            grepl('DC-8', description) ~ 'DC-8',
                            grepl('747', description) ~ 'B747',
                            grepl('MD-11', description) ~ 'MD-11',
                            grepl('727', description) ~ 'B727',
                            grepl('717', description) ~ 'B717',
                            grepl('A330', description) ~ 'A330',
                            grepl('A310', description) ~ 'A310',
                            grepl('757', description) ~ 'B757',
                            grepl('767', description) ~ 'B767',
                            grepl('777', description) ~ 'B777',
                            grepl('787', description) ~ 'B787',
                            grepl('L-1011', description) ~ 'L-1011',
                            grepl('DC-9', description) ~ 'DC-9',
                            grepl('Fokker 100', description) ~ 'Fokker 100',
                            grepl('Embraer 190', description) ~ 'Embraer 190',
                            grepl('A220', description) | grepl('A200', description) ~ 'A220',
                            grepl('A350', description) ~ 'A350',
                            grepl('A319', description) | grepl('A320', description) | grepl('A321', description) | grepl('A-318', description) ~ 'A320 Family/neo'
  )) %>%
  arrange(description)

# fleet utilization ----

time_series_ua <- intl_domestic_seats_fleet %>%
  filter(unique_carrier == 'AA') %>%
  filter(intl == 0) %>%
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
  ungroup() %>%
  mutate(group_highlight = ifelse(group %in% c('B737 NG/MAX', 'A320 Family/neo', 'B777', 'B787', 'MD-80 series/90', 'B757', 'B767', 'B727'), group, 'Other'),
         group_highlight = factor(group_highlight, levels = c('B737 NG/MAX', 'A320 Family/neo', 'B777', 'B787', 
                                                              'MD-80 series/90', 'B757', 'B767', 'B727', 'Other'))) %>%
  group_by(year,
           group_highlight) %>%
  summarise(share_asm = sum(share_asm)) %>%
  ungroup() 

final_viz <- time_series_ua %>%
  ggplot(aes(x = year,
             y = share_asm,
             group = group_highlight,
             fill = group_highlight)) + 
  geom_area() +
  geom_label_repel(data = time_series_ua %>% filter(year == last(year), share_asm > 0),
            aes(x = year + 0.5, 
                y = share_asm, 
                label = group_highlight,
                color = group_highlight),
            position = position_stack(vjust = 0.5),
            fill = 'white',
            family = 'Noto Sans',
            fontface = 'bold',
            direction = "y",
            hjust = -0) + 
  geom_label_repel(data = time_series_ua %>% filter(year == first(year)+3, share_asm > 0),
            aes(x = year - 0.5, 
                y = share_asm, 
                label = group_highlight,
                color = group_highlight),
            fill = 'white',
            family = 'Noto Sans',
            fontface = 'bold',
            direction = "y",
            position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values = c('B737 NG/MAX' = '#f46d43',
                               'A320 Family/neo' = '#fb9b7b',
                               'B777' = '#fcb097',
                               'B787' = '#f9d1c3',
                               'MD-80 series/90' = '#f0f0f0',
                               'B757' = '#d9d9d9',
                               'B767' = '#bdbdbd',
                               'B727' = '#969696',
                               'Other' = 'gray30')) +
  scale_color_manual(values = c('B737 NG/MAX' = '#f46d43',
                               'A320 Family/neo' = '#fb9b7b',
                               'B777' = '#fcb097',
                               'B787' = '#f9d1c3',
                               'MD-80 series/90' = 'gray30',
                               'B757' = '#d9d9d9',
                               'B767' = '#bdbdbd',
                               'B727' = '#969696',
                               'Other' = 'gray30')) +
  labs(title = "American Airlines Domestic Fleet Capacity",
       fill = '',
       subtitle = "Percentage of ASMs by aircraft between 1991-2022*\n",
       caption = '\nVisualization by Alex Elfering; *Through September 2022\nSource: Bureau of Transportation Statistics T2: U.S. Air Carrier TRAFFIC And Capacity Statistics by Aircraft Type') +
  scale_x_continuous(limits = c(1991,2025),
                     expand = c(0, 0),
                     labels = seq(1991, 2025, 5),
                     breaks = seq(1991, 2025, 5)) +
  scale_y_continuous(labels = c('0', '25', '50', '75', '100%'),
                     limits = c(0,1.07),
                     expand = c(0, 0)) +
  #expand_limits(y = 1.18) + 
  #geom_hline(yintercept = 0) +
  #geom_hline(yintercept = 0.5,
  #           linetype = 'dashed') +
  guides(fill = guide_legend(nrow = 2)) +
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
    legend.position = 'none',
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
final_viz
