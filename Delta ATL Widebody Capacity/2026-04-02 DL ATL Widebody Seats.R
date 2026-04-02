# 30 Day Chart Challenge Day 1
# what is the share of domestic seats dedicated to dual-aisle or jumbo aircraft YOY on major routes?

library(tidyverse)
library(ggplot2)
library(janitor)
library(data.table)
library(showtext)

font_add_google("Noto Sans", "Noto Sans")
showtext_auto()     
showtext_opts(dpi = 300) 

widebodies <- c('767','777','747','787','A300','A310','A330','A350','DC-10','MD-11','L-1011')

# aircraft df 
equipment_df <- read.csv("~/Airline Analysis/L_AIRCRAFT_TYPE.csv") |> clean_names()

# t-100 segment domestic
setwd("~/Airline Analysis/T100 Domestic")
airport_capacity_data <- rbindlist(lapply(list.files(pattern = "*.csv"), fread)) |> 
  clean_names() |>
  filter(origin == 'ATL') |>
  inner_join(equipment_df,
             by = c('aircraft_type' = 'code')) |>
  mutate(dual_aisle = str_detect(description,paste(widebodies,collapse = '|'))
  )

dual_aisle_share <- airport_capacity_data |>
  filter(departures_scheduled > 0,
         departures_performed > 1) |>
  filter(unique_carrier == 'DL') |>
  mutate(
    route_a = pmin(origin, dest),
    route_b = pmax(origin, dest)
  ) |>
  group_by(
    
    year,
    route_a,
    route_b,
    dual_aisle) |>
  summarise(
    seats = sum(seats),
    passengers = sum(passengers),
    .groups = 'drop'
  ) |>
  arrange(desc(seats)) |>
  group_by(
    year,
    route_a,
    route_b) |>
  mutate(
    pct_total = seats/sum(seats)
  ) |>
  filter(
    dual_aisle == T
  ) |>
  ungroup()

top_1990_seats <- dual_aisle_share |>
  filter(year == 1990) |>
  filter(rank(desc(seats)) <= 10) |>
  as.data.frame() |>
  select(route_a,route_b) |>
  mutate(segment = as.factor(paste0(route_a,'-',route_b)))

dual_aisle_share |>
  inner_join(top_1990_seats) |>
  unite(segment, c('route_a','route_b'),sep='-') |>
  mutate(segment = factor(segment,levels = top_1990_seats$segment)) |>
  group_by(segment) |>
  complete(year = seq(min(year), 2024, by = 1)) %>%
  ungroup() |>
  ggplot() + 
  geom_hline(yintercept = 0.5,
             linewidth = 1,
             color = 'gray80') +
  geom_vline(data = tibble(year = c(2005, 2020)),
             aes(xintercept = year),
             linetype = 'dashed', color = '#777777') +
  geom_text(
    data = tibble(
    year    = c(2005, 2020),
    label   = c('Bankruptcy', 'Covid-19'),
    segment = factor('ATL-MCO', levels = top_1990_seats$segment)
  ),
  aes(x = year, y = 1.02, label = label),
  family = 'Noto Sans',
  color = 'gray50',
  size = 3,
  hjust = -0.08) +
  geom_step(mapping = aes(x = year,
                          y = pct_total),
            color = '#e34a2e',
            linewidth = 1) +
  facet_wrap(~segment,
             nrow = 2) +
  scale_y_continuous(labels = c('0', '25', '50', '75', '100%\nof seats'),
                     limits = c(-0.05,1.1),
                     expand = c(0, 0.000001)
  ) +
  labs(x = '',
       y = '',
       title = "Delta's Use of Widebodies on ATL Domestic Routes Fell During Bankruptcy",
       subtitle = "The percentage of outbound seats flown by wide-body aircraft on Delta's 10 largest widebody routes in 1990\n",
       caption = '\nVisualization by Alex Elfering | Day #1 of the 2026 Chart Challenge: Part-to-Whole\nSource: Bureau of Transportation Statistics T-100 Domestic Segment Data') +
  theme(
    plot.title = element_text(face = 'bold', 
                              size = 16, 
                              family = 'Noto Sans'),
    plot.subtitle = element_text(#face = 'bold', 
                                 size = 14, 
                                 family = 'Noto Sans'),
    plot.caption = element_text(size = 10,
                                family = 'Noto Sans',
                                color = 'gray70',
                                hjust = 0),
    axis.title =  ggplot2::element_blank(),
    axis.text.x = element_text(size = 12, 
                               color = 'gray70',
                               family = 'Noto Sans'),
    axis.text.y = element_text(size = 12,
                               color = 'gray70',
                               family = 'Noto Sans'),
    strip.text = ggplot2::element_text(size = 12, 
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
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_blank(),
    axis.ticks.y= ggplot2::element_blank(), 
    axis.ticks.x = ggplot2::element_blank(),
    strip.background = element_rect(fill = NA),
    #plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = ggplot2::element_blank(),
    #panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed',
                                      color = 'gray90'),
    panel.grid.major = ggplot2::element_blank()) 

ggsave("~/GitHub/Airlines/Delta Air Lines FL Fleet Optimization/chart.png", plot = last_plot(), 
       width = 14, height = 6, dpi = 300, bg = "white")
