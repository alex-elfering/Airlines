# dsm shiny app

# load the packages
list.of.packages <- c('shiny',
                      'shinyWidgets',
                      'glue',
                      'tidyverse',
                      'data.table',
                      'tidylog',
                      'albersusa',
                      'sp',
                      'raster',
                      'ggplot2',
                      'ggrepel')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinyWidgets)
library(glue)
library(tidyverse)
library(data.table)
library(tidylog)
library(albersusa)
library(sp)
library(raster)
library(ggplot2)
library(broom)
library(ggrepel)
library(BiocManager)

options(repos = BiocManager::repositories())

full_dsm_data <- read.csv("full_dsm_data.csv")
airline_hubs <- read.csv("airline_hubs.csv")
lat_long <- read.csv('airports.csv') %>% filter(grepl('US-', iso_region))
airport_lat_long <- dplyr::select(lat_long, 14, 5, 6) %>% mutate(iata_code = trim(iata_code))

airline_hubs_list <- sort(unique(airline_hubs$first_stop))
destinations_list <- sort(unique(full_dsm_data$dest))

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

####  App ####
ui <- fluidPage(
  
  #titlePanel("College Football Team Performance"),    
  sidebarLayout(position = "left",
                sidebarPanel(h4("Airlines, Hubs, and Destinations"),
                             pickerInput("locInputAirlines",
                                         "Select Airline(s):", 
                                         choices= sort(c('AA', 'UA', 'DL', 'WN')), 
                                         options = list(`actions-box` = TRUE),
                                         selected = c('AA', 'UA'),
                                         multiple = T),
                             pickerInput("locInputhubs",
                                         "Select Hub(s):", 
                                         choices= airline_hubs_list, 
                                         options = list(`actions-box` = TRUE),
                                         selected = c('ORD', 'DEN', 'DFW', 'CLT'),
                                         multiple = T),
                             pickerInput("locInput",
                                         "Select Destination(s):", 
                                         choices= destinations_list, 
                                         options = list(`actions-box` = TRUE),
                                         selected = c('LAX', 'SFO'),
                                         multiple = T),
                             h4("Map Customization"),
                             sliderInput("curve", 
                                         "Line Curvature:",
                                         min = 0,
                                         max = .2,
                                         step = 0.01,
                                         value = 0.1),
                             sliderInput("line_size", 
                                         "Line Size:",
                                         min = 0.5,
                                         max = 3,
                                         step = 0.5,
                                         value = 1),
                            sliderInput("hub_font", 
                                        "Hub Font Size:",
                                        min = 1,
                                        max = 10,
                                        step = 1,
                                        value = 5),
                            sliderInput("dest_font", 
                                        "Destination Font Size:",
                                        min = 1,
                                        max = 10,
                                        step = 1,
                                        value = 3),
                  width=2),
                mainPanel(plotOutput("plot", width = "100%"))
  )) 

server <- function(input, output, session){
  
  output$plot <- renderPlot({
    
    filter_dsm <- full_dsm_data %>%
      dplyr::mutate(origin = trim(origin),
                    dest = trim(dest),
                    first_stop = trim(first_stop),
                    second_stop = trim(second_stop),
                    ticket_carrier = trim(ticket_carrier),
                    pax = trim(pax),
                    connections = trim(connections)) %>%
      filter(ticket_carrier %in% input$locInputAirlines,
             (dest %in% input$locInput) & first_stop %in% input$locInputhubs)
    #filter(ticket_carrier %in% input$airline,
    #       dest %in% input$locInput & first_stop %in% input$locInputhubs)
    
    dsm_non_stop <- filter(filter_dsm, connections == 0)
    dsm_mul_stop <- filter(filter_dsm, connections == 1)
    
    # prepare airport lat and long data for visualization
    all_airports <- tibble(airports = trim(unique(c(dsm_non_stop$origin, dsm_non_stop$dest, dsm_mul_stop$origin, dsm_mul_stop$first_stop, dsm_mul_stop$dest)))) %>%
      inner_join(airport_lat_long, by = c('airports' = 'iata_code'))
    
    airports_lat_long <- dplyr::select(all_airports, 3, 2) %>% rename(x = longitude_deg, y = latitude_deg)
    
    coordinates(airports_lat_long) <- ~x+y
    proj4string(airports_lat_long) <- CRS(us_longlat_proj)
    airports_lat_long <- points_elided(airports_lat_long) 
    airports_lat_long <- as.data.frame(coordinates(airports_lat_long))
    
    airport_lat_long_df <- cbind(all_airports, airports_lat_long)
    
    non_stop_coord <- dsm_non_stop %>%
      left_join(airport_lat_long_df,
                by = c('origin' = 'airports')) %>%
      dplyr::select(-latitude_deg,
                    -longitude_deg) %>%
      left_join(airport_lat_long_df,
                by = c('dest' = 'airports')) %>%
      dplyr::select(-latitude_deg,
                    -longitude_deg) %>%
      rename(origin_long = x.x,
             origin_lat = y.x,
             destin_long = x.y,
             destin_lat = y.y)
    
    one_stop_coord <- dsm_mul_stop %>%
      inner_join(airport_lat_long_df,
                 by = c('origin' = 'airports')) %>%
      dplyr::select(-latitude_deg,
                    -longitude_deg) %>%
      inner_join(airport_lat_long_df,
                 by = c('dest' = 'airports')) %>%
      dplyr::select(-latitude_deg,
                    -longitude_deg) %>%
      inner_join(airport_lat_long_df,
                 by = c('first_stop' = 'airports')) %>%
      dplyr::select(-latitude_deg,
                    -longitude_deg) %>%
      rename(origin_long = x.x,
             origin_lat = y.x,
             destin_long = x.y,
             destin_lat = y.y,
             first_long = x,
             first_lat = y)
    
    airport_flip <- c('CA')
    
    #us_map <- fortify(us, region="name")
    
    USA <- usa_composite(proj="laea")  # creates map projection 
    USA_MAP <- tidy(USA, region="name")
    
    airport_labels <- bind_rows(one_stop_coord, non_stop_coord) %>%
      distinct(dest,
               destin_long,
               destin_lat)
    
    one_stop_labels <- one_stop_coord %>%
      distinct(first_stop,
               first_long,
               first_lat)
    
    ggplot() +
      geom_map(
        data = USA_MAP, 
        map = USA_MAP,
        aes(x = long, 
            y = lat, 
            map_id = id),
        color = "white", 
        size = 1, 
        alpha = 0.2,
        fill = 'gray75'
      ) +
      geom_text_repel(airport_labels %>% filter(!dest %in% one_stop_labels$first_stop),
                      mapping = aes(x = destin_long,
                                    y = destin_lat,
                                    label = dest),
                      size = input$dest_font,
                      #fontface = 'bold',
                      family = 'Arial') +
      geom_curve(one_stop_coord, 
                 mapping = aes(x = origin_long, 
                               y = origin_lat, 
                               xend = first_long, 
                               yend = first_lat,
                               color = ticket_carrier),
                 #color = '#6baed6',
                 curvature = input$curve,
                 size = input$line_size,
                 alpha = 0.4) +
      geom_curve(one_stop_coord, 
                 mapping = aes(x = first_long, 
                               y = first_lat, 
                               xend = destin_long, 
                               yend = destin_lat,
                               color = ticket_carrier),
                 #color = '#6baed6',
                 curvature = input$curve,
                 size = input$line_size,
                 alpha = 0.2) +
      geom_curve(non_stop_coord, 
                 mapping = aes(x = origin_long, 
                               y = origin_lat, 
                               xend = destin_long, 
                               yend = destin_lat,
                               color = ticket_carrier),
                 #linetype = 'dashed',
                 curvature = input$curve,
                 #color = '#6baed6',
                 size = input$line_size,
                 alpha = 0.4) +
      geom_point(one_stop_coord, 
                 mapping = aes(x = first_long, 
                               y = first_lat),
                 color = 'black',
                 size = 4) +
      geom_point(one_stop_coord, 
                 mapping = aes(x = origin_long, 
                               y = origin_lat),
                 color = 'darkorange',
                 size = 4) +
      geom_point(one_stop_coord, 
                 mapping = aes(x = origin_long, 
                               y = origin_lat),
                 color = 'black',
                 shape = 1,
                 size = 4) +
      geom_point(one_stop_coord %>% filter(!dest %in% one_stop_labels$first_stop), 
                 mapping = aes(x = destin_long, 
                               y = destin_lat),
                 color = 'black',
                 size = 1) +
      geom_label_repel(one_stop_labels,
                       mapping = aes(x = first_long,
                                     y = first_lat,
                                     label = first_stop),
                       alpha = 0.6, 
                       label.size = NA,
                       seed = 999,
                       size = input$hub_font,
                       color = 'black',
                       fontface = 'bold',
                       family = 'Arial') +
      geom_label_repel(one_stop_labels,
                       mapping = aes(x = first_long,
                                     y = first_lat,
                                     label = first_stop),
                       label.size = NA,
                       seed = 999,
                       size = input$hub_font,
                       fill = NA,
                       color = 'black',
                       fontface = 'bold',
                       family = 'Arial') +
      geom_label_repel(mapping = aes(x = 527052.9,
                                     y = -365024.6),
                       label = 'DSM',
                       label.size = NA,
                       seed = 999,
                       size = input$hub_font,
                       fill = NA,
                       color = 'black',
                       fontface = 'bold',
                       family = 'Arial') +
      geom_label_repel(mapping = aes(x = 527052.9,
                                     y = -365024.6),
                       label = 'DSM',
                       alpha = 0.6, 
                       label.size = NA,
                       seed = 999,
                       size = input$hub_font,
                       color = 'black',
                       fontface = 'bold',
                       family = 'Arial') +
      labs(title = 'The United States According to Des Moines Travelers in 2019',
           subtitle = glue('Non-stop and one-stop domestic flights by Des Moines travelers according to the D1B1 Market Airline Survey'),
           caption = 'Map created by Alex Elfering | Source: Bureau of Transportation Statistics',
           color = 'Airlines: ',
           x = '',
           y = '') +
      theme(plot.title = element_text(face = 'bold',family = 'Arial', size = 16),
            plot.subtitle = element_text(size = 14,family = 'Arial'),
            legend.position = 'top',
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.title = element_text(size = 16,
                                        color = 'black',
                                        face = 'bold',
                                        family = 'Arial'),
            legend.text = element_text(size = 16,
                                       color = 'black',
                                       family = 'Arial'),
            plot.title.position = "plot",
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 12, color = "#c1c1c1",family = 'Arial'),
            axis.title = element_text(size = 12),
            axis.text = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            strip.text = element_blank(),
            strip.background = element_rect(fill = NA),
            panel.background = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank())
    
    
  }, height = 1200, width = 1600)
  
  
}

shinyApp(ui, server)