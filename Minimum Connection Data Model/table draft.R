# table to show connection results
# 最小乗継時間

library(gt)
library(gtExtras)
library(lubridate)
library(hms)
set.seed(99)

origin_var <- 'ABE' 
dest_var   <- 'GEG'

##### table ----
table_output_df <- full_minimum_connection_time |>
  
  # filter parameters
  
  filter(
    (origin == origin_var &
       dest == dest_var) 
  ) |>
  
  # adding market city names for origin and destination
  left_join(airportr::airports |> select(IATA, City), by = c('origin' = 'IATA')) |>
  rename(origin_city = City) |>
  left_join(airportr::airports |> select(IATA, City), by = c('dest' = 'IATA')) |>
  rename(dest_city = City) |>
  mutate(
    origin_city = standardize_airport_names(origin, origin_city, origin_city),
    dest_city   = standardize_airport_names(dest, dest_city, dest_city)
    ) |>

  # sorting
  mutate(stops = ifelse(is.na(hub),0,1)) |>
  arrange(
    stops,
    arr_datetime_dest
    ) |>
  
  # time formatting
  mutate(
    first_departure_time  = trimws(format(origin_departure, "%l:%M %p")),
    first_arrival_time    = trimws(format(origin_hub_arrival, "%l:%M %p")),
    last_departure_time   = trimws(format(departure_local, "%l:%M %p")),
    last_arrival_time     = trimws(format(arr_datetime_dest, "%l:%M %p")),
    origin_day            = as.Date(floor_date(origin_departure,'day')),
    arrival_day           = as.Date(floor_date(arr_datetime_dest,'day'))
  ) |>
    
  # table display columns
  mutate(
    next_day              = ifelse((origin_day) != (arrival_day),'Next Day Arrival',''),
    city_itinerary        = paste(origin,dest,sep=' → '),
    departure_to_arrival  = paste(first_departure_time,last_arrival_time,sep=' → '),

    connection_label = case_when(
      !is.na(hub) ~ paste('✈︎',hub,'-',connection_label,' layover',sep = ''),
      is.na(hub)  ~ toupper('<span style="color:#2a9d3f; font-weight:600;">Nonstop flight</span>')
    ),
    itinerary_flight_nos = case_when(
      !is.na(hub) ~ paste(airline,origin_flight_no, ' & ',airline,flight_no,sep=''),
      is.na(hub)  ~ paste0(airline,flight_no)
    )
  ) |>
  
  # inbound and outbound details
  mutate(
    inbound = case_when(
      !is.na(hub) ~ paste0(
        '**Depart**: ',origin,' @ ',first_departure_time, '<br><span style="color:gray; font-size:14px;font-style:italic">---', origin_flight_time_label, '---</span><br>','**Arrival**: ',hub,' @ ',first_arrival_time),
      is.na(hub) ~ paste0(
        '**Depart**: ',origin,' @ ',first_departure_time,'<br><span style="color:gray; font-size: 14px;font-style:italic">---',total_flight_time_label,'---</span><br>','**Arrival**: ',dest,' @ ',last_arrival_time)
    ),
    connecting = case_when(
      !is.na(hub) ~ paste0(
        '**Depart**: ',hub,' @ ',last_departure_time,'<br><span style="color:gray; font-size: 14px;font-style:italic">---',dest_flight_time_label, '---</span><br>**Arrival**: ',dest,' @ ',last_arrival_time),
      is.na(hub) ~ ''
    )
  )

# details for title
origin_airport_name <- paste0(unique(table_output_df$origin_city), " (",unique(table_output_df$origin),")")
dest_airport_name   <- paste0(unique(table_output_df$dest_city), " (",unique(table_output_df$dest),")")
row_count           <- nrow(table_output_df)
table_title         <- paste0('Nonstop and One-Stop Options: <span style="background-color:#fff7bc; padding:2px 4px;">', origin_airport_name, " → ", dest_airport_name,'</span> ')
table_subtitle      <- paste0(glue::glue('<span style="color:black; font-size: 14px">{row_count} possible options found for Oct. 31, 2025; Sorted by non-stop options, then by arrival time</span>',
                                         '<br><span style="color:gray; font-size: 14px;font-style:italic">Note: Layover time ends 15 minutes before outbound departure</span>'))

table_output_df |>
  select(departure_to_arrival,
         total_flight_time_label,
         itinerary_flight_nos,
         next_day,
         city_itinerary,
         connection_label,
         inbound,
         connecting) |>
  gt() |>
  fmt_markdown(columns = c(inbound,connecting,connection_label)) |>
  gt_merge_stack(
    col1 = departure_to_arrival, 
    col2 = total_flight_time_label,
    small_cap = T,
    palette = c("black", "grey50"),
    font_size = c("14px", "12px"),
    font_weight = c("bold", "normal")
  ) |>
  # combines itinerary
  gt_merge_stack(
    col1 = city_itinerary, 
    col2 = connection_label,
    small_cap = T,
    palette = c("black", "grey50"),
    font_size = c("14px", "14px"),
    font_weight = c("bold", "normal")
  ) |>
  # combines flight numbers & overnight info
  gt_merge_stack(
    col1 = itinerary_flight_nos, 
    col2 = next_day,
    small_cap = T,
    palette = c("black", "grey50"),
    font_size = c("14px", "14px"),
    font_weight = c("bold", "normal")
    
  ) |>
  cols_label(
    departure_to_arrival = 'Travel Time',
    city_itinerary = 'Itinerary',
    itinerary_flight_nos = 'Inbound/Outbound Flight',
    inbound = 'inbound flight',
    connecting = 'outbound flight'
  ) |>
  cols_width(
    departure_to_arrival ~ px(180),
    city_itinerary       ~ px(150),
    itinerary_flight_nos ~ px(200),
    inbound              ~ px(200),
    connecting           ~ px(200)
  ) |>
  gt_theme_538() |>
  tab_header(
    title = html(table_title),
    subtitle = html(table_subtitle)
    ) |>
  opt_align_table_header(align = "left") |>
  tab_style(
    style = cell_text(weight = "bold", color = "gray40"),
    locations = cells_column_labels()
  ) #|>
  #tab_source_note("Source: BTS Data") |>
  #tab_style(
  #  style = cell_text(
  #    size   = px(11),
  #    color  = "gray50",
  #    style  = "italic"
  #  ),
  #  locations = cells_source_notes()
  #)

