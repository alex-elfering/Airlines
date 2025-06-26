# MINIMUM CONNECTION MODEL
# how many one-stop flights can you catch from Des Moines to other destinations?

# ----  libraries ----
library(data.table)
library(janitor)
library(stringr)
library(fs)
library(airportr)
library(tidyverse)
library(lubridate)

# ----  variables and custom dataframes ----
main_airport <- 'OMA'

base_path <- "C:/Users/alexe/OneDrive/Documents/Airline Analysis/Schedule Data"
years_to_include <- c("2025")
target_airports <- c("OMA")

year_pattern <- paste0("/", years_to_include, "/") |> paste(collapse = "|")
all_files <- list.files(path = base_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
selected_files <- all_files[str_detect(all_files, year_pattern)]

airline_hubs <- data.frame(
  airline = c(rep("AA", 10),
              rep("UA",9),
              rep("DL",11),
              rep('WN',13),
              rep('F9',15),
              rep('AS',8),
              rep('NK',10),
              rep('B6',5),
              rep('HA',2),
              rep('G4',23)
  ),
  hub     = c("PHL", "ORD", "CLT",'DFW','LAX','MIA','JFK','LGA','PHX','DCA',
              'ORD','DEN','IAH','LAX','EWR','SFO','IAD','GUM','HNL',
              'ATL','BOS','DTW','LAX','MSP','JFK','LGA','SLC','SEA','AUS','RDU',
              'ATL','BWI','MDW','DAL','DEN','HOU','LAS','LAX','BNA','OAK','MCO','PHX','STL',
              'ATL','MDW','ORD','CVG','CLE','DFW','DEN','LAS','MIA','MCO','PHL','PHX','SJU','TPA','TTN',
              'ANC','LAX','PDX','SAN','SFO','SEA','BOI','SJC',
              'ATL','ORD','DFW','DTW','FLL','IAH','LAS','MIA','EWR','MCO',
              'BOS','FLL','JFK','MCO','SJU',
              'HNL','OGG',
              'ABE','ATW','AVL','BLI','CVG','VPS','DSM','FNT','FLL','GRR','IND','TYS','LAS','LAX','BNA','SFB','AZA','PIT','PVU','PGD','SRQ','SAV','PIE'
  ) 
) |>
  # create a larger airline group for Alaska and Hawaiian (merging) and Alaska and American (OneWorld partners)
  mutate(
    merger_group = case_when(
      airline %in% c("AS", "HA") ~ "ASHA",
      TRUE ~ airline
    ),
    codeshare_group = case_when(
      airline %in% c("AA", "AS") ~ "AAAS",
      TRUE ~ airline
    )
  )

# ----  functions ----

#format_time <- function(x) {
#  x <- suppressWarnings(as.integer(x))
#  ifelse(is.na(x), NA_character_, sprintf("%02d:%02d", x %/% 100, x %% 100))
#}

assign_airline_groups <- function(df) {
  df |>
    mutate(
      merger_group = if_else(marketing_airline_network %in% c("AS", "HA"), "ASHA", marketing_airline_network),
      codeshare_group = if_else(marketing_airline_network %in% c("AA", "AS"), "AAAS", marketing_airline_network)
    )
}

process_flight_times <- function(flight_data, airport_timezones) {
  
  format_time <- function(time_num) {
    # Helper to format times like 815 -> "08:15"
    time_str <- sprintf("%04d", as.integer(time_num))
    paste0(substr(time_str, 1, 2), ":", substr(time_str, 3, 4))
  }
  
  flight_data |>
    select(
      year,
      flight_date,
      marketing_airline_network,
      flight_number_marketing_airline,
      operating_airline,
      tail_number,
      origin,
      dest,
      crs_dep_time,
      crs_arr_time
    ) |>
    left_join(airport_timezones, by = c("origin" = "iata")) |>
    rename(origin_tz = timezone) |>
    left_join(airport_timezones, by = c("dest" = "iata")) |>
    rename(dest_tz = timezone) |>
    mutate(
      dep_time_str = format_time(crs_dep_time),
      arr_time_str = format_time(crs_arr_time),
      
      # Naive local datetime objects (no tz info)
      dep_local_naive = ymd_hm(paste(flight_date, dep_time_str)),
      arr_naive_same_day = ymd_hm(paste(flight_date, arr_time_str)),
      
      # Calculate tz offsets in seconds relative to naive departure time
      origin_offset = map2_dbl(dep_local_naive, origin_tz, ~ as.integer(with_tz(.x, .y)) - as.integer(.x)),
      dest_offset = map2_dbl(dep_local_naive, dest_tz, ~ as.integer(with_tz(.x, .y)) - as.integer(.x)),
      
      # Simulated UTC times by subtracting offsets (not using force_tz)
      dep_utc = dep_local_naive - seconds(origin_offset),
      arr_utc = arr_naive_same_day - seconds(dest_offset),
      
      # Determine if arrival is next day (same tz: local comparison, diff tz: UTC comparison)
      arrived_next_day = if_else(
        origin_tz == dest_tz,
        arr_naive_same_day < dep_local_naive,
        arr_utc < dep_utc
      ),
      
      # Correct arrival datetime adding a day if arrived next day
      arr_local_naive = ymd_hm(paste(as.Date(flight_date) + if_else(arrived_next_day, 1, 0), arr_time_str))
    ) |>
    arrange(flight_date) |>
    select(
      marketing_airline_network,
      flight_number_marketing_airline,
      tail_number,
      dep_local_naive,
      arr_local_naive,
      origin,
      dest,
      arrived_next_day
    )
}

# ----  data loading  ----

# pull airports within ~250 miles of DSM
close_airports <- airportr::airports_near_airport(main_airport,250) |> clean_names() |> select(iata)

# airport timezones
airport_timezones <- airportr::airports |>
  clean_names() |>
  select(iata,
         timezone) |>
  bind_rows(tibble::tibble(
    iata = c("XWA", "EAR"),  # replace with your missing codes
    timezone = c("America/Chicago", "America/Chicago")
  ))

# compile CSVs - one for DSm and another for airline hubs
dsm_airport_schedule <- rbindlist(
  lapply(selected_files, function(file) {
    df <- fread(file) |> clean_names()
    df <- df[df$origin %in% target_airports, ]
    df <- df[df$marketing_airline_network %in% unique(airline_hubs$airline), ]
    
    df <- df |>
      as_tibble() |>
      rename_with(~str_replace_all(., " ", "_")) |>
      mutate(
        merger_group = case_when(
          marketing_airline_network %in% c("AS", "HA") ~ "ASHA",
          TRUE ~ marketing_airline_network
        ),
        codeshare_group = case_when(
          marketing_airline_network %in% c("AA", "AS") ~ "AAAS",
          TRUE ~ marketing_airline_network
        )
      ) 
    
    return(df)
  })
)

hub_schedule_data <- rbindlist(
  lapply(selected_files, function(file) {
    df <- fread(file) |> clean_names()

    before <- nrow(df)    
    
    df <- df |>
      as_tibble() |>
      rename_with(~str_replace_all(., " ", "_")) |>
      mutate(
        merger_group = case_when(
          marketing_airline_network %in% c("AS", "HA") ~ "ASHA",
          TRUE ~ marketing_airline_network
        ),
        codeshare_group = case_when(
          marketing_airline_network %in% c("AA", "AS") ~ "AAAS",
          TRUE ~ marketing_airline_network
        )
      ) |>
      inner_join(airline_hubs, by = c("codeshare_group" = "codeshare_group", "origin" = "hub"))
    
    after <- nrow(df)
    message("Dropped ", before - after, " rows during hub join.")
    
    return(df)
  })
)

# ---- join DSM schedule with hub data to find connections  ----

target_flights <- process_flight_times(dsm_airport_schedule,airport_timezones) |> 
  select(-arrived_next_day) |>
  mutate(
    codeshare_group = case_when(
      marketing_airline_network %in% c("AA", "AS") ~ "AAAS",
      TRUE ~ marketing_airline_network
    )
  ) |>
  rename(
    origin_flight_number = flight_number_marketing_airline,
    origin_tail_number = tail_number
  )

# to remove nonstop flights from the MCT dataframe below
non_stop_flights <- target_flights |>
  distinct(origin,
           dest)

schedule_flights <- process_flight_times(hub_schedule_data,airport_timezones) |>  
  select(-arrived_next_day) |> 
  mutate(
    merger_group = case_when(
      marketing_airline_network %in% c("AS", "HA") ~ "ASHA",
      TRUE ~ marketing_airline_network
    ),
    codeshare_group = case_when(
      marketing_airline_network %in% c("AA", "AS") ~ "AAAS",
      TRUE ~ marketing_airline_network
    )
  ) |>
  rename(
    hub = origin, 
    final_dest = dest,
    hub_departure = dep_local_naive,
    hub_arrival = arr_local_naive,
    hub_departing_flight_number = flight_number_marketing_airline,
    hub_tail_number = tail_number
  )


setkey(schedule_flights, codeshare_group, hub)

min_connection_1_stop <- schedule_flights[
  target_flights,
  on = .(codeshare_group, hub = dest),
  allow.cartesian = TRUE
][
  hub != origin
][
  , layover_mins := as.numeric(difftime(hub_departure, arr_local_naive, units = "mins"))
][
  layover_mins >= 30 & layover_mins <= 60
][
  , .(
    group_airline = codeshare_group,
    marketing_airline_network,
    origin,
    origin_flight_number,
    origin_tail_number,
    dep_local_naive,
    arr_local_naive,
    hub,
    hub_departing_flight_number,
    hub_tail_number,
    hub_departure,
    hub_arrival,
    final_dest,
    layover_mins
  )
]
min_connection_1_stop[
  , total_travel_time_mins := as.numeric(difftime(hub_arrival, dep_local_naive, units = "mins"))/60
]

min_connection_1_stop |>
  arrange(hub_arrival) 
