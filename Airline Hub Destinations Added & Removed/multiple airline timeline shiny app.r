# R Shiny app showing destinations added & terminated by current US Airlines 

library(shiny)
library(data.table)
library(tidyverse)
library(ggplot2)
library(rvest)
library(stringi)
library(glue)
library(lubridate)
library(janitor)
library(shinyWidgets)

# pull data ----
airline_hub_data <- read.csv('hub history1.csv')

# airline names
airline_names <- data.frame(code = c('F9', 
                                     'AA',
                                     'UA',
                                     'DL',
                                     'B6',
                                     'WN',
                                     'G4', 
                                     'NK',
                                     'HA',
                                     'AS',
                                     'SY'),
                            airline = c('Frontier Airlines',
                                        'American Airlines',
                                        'United Airlines',
                                        'Delta Air Lines',
                                        'JetBlue',
                                        'Southwest Airlines',
                                        'Allegiant Air',
                                        'Spirit Airlines',
                                        'Haiwaiian Airlines',
                                        'Alaska Airlines',
                                        'Sun Country'))



ui <- fluidPage(
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput("airline", "Airline:",
                              sort(c('Frontier Airlines',
                                     'American Airlines',
                                     'United Airlines',
                                     'Delta Air Lines',
                                     'JetBlue',
                                     'Southwest Airlines',
                                     'Allegiant Air',
                                     'Spirit Airlines',
                                     'Haiwaiian Airlines',
                                     'Alaska Airlines',
                                     'Sun Country'))),
                  sliderInput("year", 
                              "Highlight Year:",
                              min = 1990,
                              max = 2022,
                              step = 1,
                              value = 2015,
                              sep = ''),
                  pickerInput("service_level",
                              "Service Level:", 
                              choices= c('Service Established',
                                         'Service Resumed',
                                         'Service',
                                         'Service Terminated',
                                         'Service Established, then Terminated',
                                         'Service Resumed, then Terminated'), 
                              options = list(`actions-box` = TRUE),
                              selected = c('Service Established',
                                           'Service Resumed',
                                           'Service',
                                           'Service Terminated',
                                           'Service Established, then Terminated',
                                           'Service Resumed, then Terminated'),
                              multiple = T),
                width=2),
  mainPanel(plotOutput("plot", width = "100%"))
)) 

server = shinyServer(function(input, output, session) {
  
  output$plot <- renderPlot({
    
    filter_airline_name <- airline_names |>
      filter(airline == input$airline)
    
    var_airline_name <- filter_airline_name$airline
    var_code_name <- filter_airline_name$code
    
    #var_airline_name <- 'United Airlines'
    #var_code_name <- 'UA'
    
    airline_origin_order <- airline_hub_data |>
      group_by(carrier) |>
      filter(year == max(year)-1,
             !grepl('Terminate', service_level)) |>
      ungroup() |>
      group_by(carrier,
               origin) |>
      summarise(count = n_distinct(dest)) |>
      ungroup() |>
      arrange(carrier,
              desc(count)) |>
      filter(carrier == var_code_name)
    
    airports <- airline_origin_order$origin
    
    final_df <- airline_hub_data |>
      filter(carrier == var_code_name) |>
      filter(year <= 2022) |>
      mutate(origin = factor(origin, levels = airports),
             service_level = factor(service_level, 
                                    levels = c('Service Established',
                                               'Service Resumed',
                                               'Service',
                                               'Service Terminated',
                                               'Service Established, then Terminated',
                                               'Service Resumed, then Terminated'))) |>
      group_by(year,
               origin,
               service_level) |>
      summarise(n = n_distinct(dest)) |>
      ungroup() |>
      mutate(n = ifelse(grepl('Terminate', service_level), n*-1,n)) #|>
      #filter(service_level %in% input$service_level)
    
    total_hubs <- n_distinct(final_df$origin)
    min_year <- min(final_df$year)
    max_year <- max(final_df$year)
    
    if(input$airline %in% c('G4','F9','WN','NK')){
      
      subtitle_statement <- glue('Among the top {total_hubs} cities with the most destinations, between {min_year} and {max_year}')
      caption_statement <- glue('. Because {var_airline_name} does not have "official" hubs, the top cities with the most destinations was pulled.')
      
    }else{
      
      subtitle_statement <- glue('Among its {total_hubs} current hubs, between {min_year} and {max_year}')
      caption_statement <- glue('.')
      
    }
    
    final_df |>
      ggplot(aes(x = year,
                 y = n,
                 fill = service_level)) + 
      geom_bar(width = 1,
               stat = 'identity',
               position = 'stack') +
      geom_vline(xintercept = input$year,
                 linetype = 'dashed',
                 size = 1) +
      facet_wrap(~origin,
                 ncol = 5,
                 scales = 'free_x') +
      scale_fill_manual(values = c('Service Established' = '#2166ac',
                                   'Service Resumed' = '#67a9cf',
                                   'Service' = 'gray80',
                                   'Service Terminated' = '#b2182b',
                                   'Service Established, then Terminated' = '#ef8a62',
                                   'Service Resumed, then Terminated' = '#fddbc7'))+
      geom_hline(yintercept = 0,
                 linetype = 'dotted') +
      guides(fill = guide_legend(nrow = 2)) +
      scale_x_continuous(limits=c(min_year,max_year),
                         breaks = seq(min_year, max_year, by = 5)) +
      labs(title = glue('Destinations Added & Terminated by {var_airline_name}'),
           subtitle = subtitle_statement,
           fill = '',
           caption = glue('Visualization by Alex Elfering\nData Source: T-100 Segment Data from the Bureau of Transportation Statistics\nNote: Only includes mainline operations, data is filtered to remove segments <= 10 flights{caption_statement}')
           ) +
      theme(
        plot.title = element_text(face = 'bold', 
                                  size = 18, 
                                  family = 'Noto Sans',
                                  hjust = 0.5),
        plot.subtitle = element_text(#face = 'bold', 
                                     size = 17, 
                                     family = 'Noto Sans',
                                     hjust = 0.5),
        plot.caption = element_text(size = 10,
                                    family = 'Noto Sans',
                                    hjust = 0),
        axis.title =  ggplot2::element_blank(),
        axis.text.x = element_text(size = 10, 
                                   #face= bold_label,
                                   family = 'Noto Sans',
                                   color = 'gray50'),
        axis.text.y = element_text(size = 12, 
                                   family = 'Noto Sans',
                                   color = 'gray50'),
        strip.text = ggplot2::element_text(size = 12, 
                                           face = 'bold',
                                           hjust = 0.5, 
                                           family = 'Noto Sans'),
        plot.title.position = "plot", 
        plot.caption.position = 'plot',
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.line.x.bottom = element_blank(),
        axis.line.y.left = element_blank(),
        axis.ticks.y= ggplot2::element_blank(), 
        axis.ticks.x = ggplot2::element_blank(),
        strip.background = element_rect(fill = NA),
        #plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = ggplot2::element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.major.y = element_line(color = "gray70",
                                          size = 0.5,
                                          linetype = 2),
        #panel.border = element_rect(color = "gray90", fill = NA, size = 0.5)
        ) +
      annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="gray60",lwd=1.5)
    
  }, height = 850,width = 1500)
  
})

shinyApp(ui,server)