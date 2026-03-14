
# Libraries ----
library(tidyverse)
library(data.table)
library(janitor)
library(airportr)
library(lubridate)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(maps)


# Helper Functions ----
hhmm_to_minutes <- function(x) {
  (x %/% 100) * 60 + (x %% 100)
}

make_arch_curve <- function(p1, p2, arc_height = 0.3, direction = 1, n_points = 100) {
  x1 <- p1[1, 1]; y1 <- p1[1, 2]
  x2 <- p2[1, 1]; y2 <- p2[1, 2]
  
  mx <- (x1 + x2) / 2; my <- (y1 + y2) / 2
  dist <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
  perp_len <- sqrt((y1 - y2)^2 + (x2 - x1)^2)
  offset_x <- -(y2 - y1) / perp_len * dist * arc_height * direction
  offset_y <- (x2 - x1) / perp_len * dist * arc_height * direction
  
  cx <- mx + offset_x; cy <- my + offset_y
  
  t <- seq(0, 1, length.out = n_points)
  cbind(
    x = (1 - t)^2 * x1 + 2 * (1 - t) * t * cx + t^2 * x2,
    y = (1 - t)^2 * y1 + 2 * (1 - t) * t * cy + t^2 * y2
  )
}

create_route_sf <- function(df, cols, arc_height = 0.43, laea_proj) {
  df |>
    select(all_of(cols)) |>
    distinct() |>
    pmap_dfr(function(...) {
      args <- list(...)
      p1 <- st_transform(st_sfc(st_point(c(args[[3]], args[[4]])), crs = 4326), laea_proj) |> 
        st_coordinates()
      p2 <- st_transform(st_sfc(st_point(c(args[[5]], args[[6]])), crs = 4326), laea_proj) |> 
        st_coordinates()
      
      coords <- make_arch_curve(p1, p2, arc_height = arc_height, direction = sample(c(-1, 1), 1))
      
      st_linestring(coords) |>
        st_sfc(crs = laea_proj) |>
        st_sf(route = paste(args[[1]], "to", args[[2]]), geometry = _)
    })
}

# Airport name standardization
airport_states <- read.csv("https://raw.githubusercontent.com/datasets/airport-codes/master/data/airport-codes.csv") |>
  filter(
    iata_code %in% unique(full_minimum_connection_time$origin),
    iso_country == "US"
  ) |>
  mutate(state = stringr::str_remove(iso_region, "US-")) |>
  select(
    iata_code, 
    municipality, 
    state
  ) |>
  mutate(municipality = case_when(
    iata_code == 'JFK' ~ 'New York JFK',
    iata_code == 'LGA' ~ 'New York LaGuardia',
    iata_code == 'EWR' ~ 'Newark',
    iata_code == 'DCA' ~ 'Washington National',
    iata_code == 'IAD' ~ 'Washington Dulles',
    iata_code == 'ORD' ~ "Chicago O'Hare",
    iata_code == 'MDW' ~ 'Chicago Midway',
    iata_code == 'IAH' ~ 'Houston Intercontinental',
    iata_code == 'HOU' ~ 'Houston Hobby',
    iata_code == 'DAL' ~ 'Dallas Love Field',
    iata_code == 'ABY' ~ 'Albany',
    iata_code == 'ALB' ~ 'Albany',
    iata_code == 'CSG' ~ 'Columbus',
    iata_code == 'GTR' ~ 'Columbus',
    iata_code == 'CRW' ~ 'Charleston',
    iata_code == 'XNA' ~ 'Fayetteville/Bentonville',
    iata_code == 'JAN' ~ 'Jackson',
    iata_code == 'SGF' ~ 'Springfield',
    iata_code == 'LWS' ~ 'Lewiston',
    iata_code == 'BHM' ~ 'Birmingham',
    iata_code == 'BGR' ~ 'Bangor',
    iata_code == 'LEX' ~ 'Lexington',
    iata_code == 'OAJ' ~ 'Jacksonville',
    iata_code == 'CAE' ~ 'Columbia',
    iata_code == 'HLN' ~ 'Helena',
    iata_code == 'RDU' ~ 'Raleigh/Durham',
    iata_code == 'GSP' ~ 'Greenville/Spartanburg',
    iata_code == 'GPT' ~ 'Gulfport/Biloxi',
    iata_code == 'MLI' ~ 'Moline/Quad Cities',
    iata_code == 'COD' ~ 'Cody/Yellowstone',
    iata_code == 'HDN' ~ 'Hayden/Steamboat Springs',
    iata_code == 'WYS' ~ 'West Yellowstone',
    iata_code == 'MVY' ~ "Martha's Vineyard",
    iata_code == 'HHH' ~ 'Hilton Head',
    iata_code == 'SNA' ~ 'Orange County',
    iata_code == 'MFE' ~ 'McAllen',
    iata_code == 'MTJ' ~ 'Montrose',
    iata_code == 'SCE' ~ 'State College',
    iata_code == 'TRI' ~ 'Tri-Cities',
    iata_code == 'AGS' ~ 'Augusta',
    iata_code == 'ACV' ~ 'Eureka',
    iata_code == 'MHT' ~ 'Manchester',
    iata_code == 'MLB' ~ 'Melbourne/Orlando',
    iata_code == 'MGM' ~ 'Montgomery',
    iata_code == 'CHO' ~ 'Charlottesville',
    iata_code == 'MHK' ~ 'Manhattan',
    iata_code == 'MAF' ~ 'Midland/Odessa',
    iata_code == 'CID' ~ 'Cedar Rapids/Iowa City',
    iata_code == 'COU' ~ 'Columbia',
    iata_code == 'DRO' ~ 'Durango',
    iata_code == 'CMI' ~ 'Champaign/Urbana',
    iata_code == 'SGF' ~ 'Springfield/Branson',
    iata_code == 'GRK' ~ 'Fort Hood/Killeen',
    iata_code == 'SUN' ~ 'Sun Valley',
    iata_code == 'AZA' ~ 'Phoenix/Mesa',
    iata_code == 'USA' ~ 'Charlotte/Concord',
    iata_code == 'BLV' ~ 'Belleville/St Louis',
    iata_code == 'CMH' ~ 'Columbus-Glenn',
    iata_code == 'LCK' ~ 'Columbus-Rickenbacker',
    iata_code == 'PIE' ~ 'St. Petersburg/Clearwater',
    iata_code == 'VPS' ~ 'Destin/Fort Walton Beach',
    iata_code == 'JAC' ~ 'Jackson Hole',
    iata_code == 'OTH' ~ 'North Bend/Coos Bay',
    iata_code == 'RDM' ~ 'Redmond/Bend',
    iata_code == 'CLD' ~ 'San Diego/Carlsbad',
    iata_code == 'PSC' ~ 'Pasco/Tri-Cities',
    iata_code == 'CIU' ~ 'Sault Ste. Marie',
    iata_code == 'PVD' ~ 'Providence',
    iata_code == 'SFB' ~ 'Orlando-Sanford',
    iata_code == 'MQT' ~ 'Marquette',
    iata_code == 'TTN' ~ 'Trenton',
    iata_code == 'SWF' ~ 'New York-Stewart',
    iata_code == 'TYS' ~ 'Knoxville',
    iata_code == 'XNA' ~ 'Fayetteville/Bentonville',
    iata_code == 'HNL' ~ 'Honolulu',
    iata_code == 'LIH' ~ 'Lihue',
    
    
    TRUE ~ municipality
  )) |>
  unite(col = market,municipality,state,sep=', ', na.rm = TRUE, remove = FALSE)

# Parameters ----
setwd("~/Airline Analysis/Schedule Data/2024")

fixed_date_var <- as.Date('2024-06-30')

laea_proj <- "+proj=laea +lat_0=40 +lon_0=-100 +datum=WGS84 +units=m +no_defs"

# airframe loading  ----

read.csv("C:/Users/alexe/OneDrive/Desktop/airframes.csv") |> clean_names() |>
  filter(carrier %in% c('DL','HA','UA','AA','AS','B6','WN','F9','NK','G4') ) |>
  select(
    carrier,
    carrier_name,
    tail_number,
    manufacturer,
    model
  ) |>
  mutate(
    manu_clean = case_when(
      grepl('Airbus',manufacturer,ignore.case = T) ~ 'Airbus',
      grepl('Boeing',manufacturer,ignore.case = T) ~ 'Boeing',
      TRUE ~ manufacturer
    ),
    model_clean = case_when(
      model == '717-200' | grepl('717',model) ~ '717',
      grepl('737-7',model) ~ '737-700',
      grepl('737-8',model) ~ '737-800',
      grepl('737-9',model) ~ '737-900',
      grepl('757-2',model) ~ '757-200',
      grepl('757-3',model) ~ '757-300',
      grepl('767-3',model) ~ '767-300',
      grepl('767-4',model) ~ '767-400',
    )
  ) |>
  distinct(
    manufacturer,
    model,
    manu_clean,
    model_clean
  ) |>
  arrange(
    manufacturer,
    model,
    manu_clean,
    model_clean
  )

# Data Loading ----
airport_performance_data <- rbindlist(lapply(list.files(pattern = "*.csv"), fread)) |> 
  clean_names()

# Data Loading ----

# find timezones for airports in dataset
airport_tz <- airportr::airports |> 
  clean_names() |>
  filter(iata %in% unique(c(ord_sched_df$origin, ord_sched_df$dest))) |>
  select(iata, timezone) |>
  mutate(timezone = if_else(iata == 'BIH', 'America/Los_Angeles', timezone))

# timeliness data ----
segment_performance <- airport_performance_data |>
  #filter(origin == 'SFO',dest == 'IAD') |>
  select(
    airline = marketing_airline_network, 
    flight_no = flight_number_marketing_airline,
    crs_dep_time, 
    crs_elapsed_time, 
    origin, 
    dest, 
    flight_date,
    dep_del15,
    cancelled,
    dep_delay_minutes
  ) |>
  left_join(airport_tz, by = c("origin" = "iata")) |>
  rename(timezone_origin = timezone) |>
  left_join(airport_tz, by = c("dest" = "iata")) |>
  rename(timezone_dest = timezone) |>
  filter(!is.na(timezone_origin), !is.na(timezone_dest),
         timezone_dest %in% OlsonNames(), timezone_origin %in% OlsonNames()) |>
  group_by(airline,origin,dest,timezone_origin, timezone_dest) |>
  mutate(
    dep_datetime = force_tz(as.POSIXct(flight_date) + minutes(hhmm_to_minutes(crs_dep_time)), 
                            first(timezone_origin)),  
    arr_datetime = with_tz(dep_datetime + minutes(crs_elapsed_time), first(timezone_dest)),
    dep_datetime1 = as.POSIXct(flight_date) + minutes(hhmm_to_minutes(crs_dep_time))
  ) |>
  filter(
    as.Date(dep_datetime) >= as.Date('2024-05-01'),
    as.Date(dep_datetime) <= as.Date('2024-06-30')
         ) |>
  group_by(
    airline,
    origin,
    dest
  ) |>
  summarise(flights = n(),
            delay = sum(dep_del15,na.rm = T),
            cancelled = sum(cancelled,na.rm = T),
            median_delay_length = mean(dep_delay_minutes[dep_del15 == 1 & cancelled == 0], na.rm = TRUE),
            .groups = 'drop') |>
  mutate(
    delay_rate = delay/(flights-cancelled),
    on_time_rate = 1-(delay/(flights-cancelled)),
    cancellation_rate = cancelled/flights,
  ) |>
  arrange((on_time_rate)) |>
  select(
    airline,
    origin,
    dest,
    on_time_rate,
    delay_rate,
    cancellation_rate
  )

# Schedule Processing ----
sched_df_adjusted <- airport_performance_data |>
  select(
    airline = marketing_airline_network, 
    flight_no = flight_number_marketing_airline,
    crs_dep_time, 
    crs_elapsed_time, 
    origin, 
    dest, 
    flight_date,
    tail_number) |>
  left_join(airport_tz, by = c("origin" = "iata")) |>
  rename(timezone_origin = timezone) |>
  left_join(airport_tz, by = c("dest" = "iata")) |>
  rename(timezone_dest = timezone) |>
  filter(!is.na(timezone_origin), !is.na(timezone_dest),
         timezone_dest %in% OlsonNames(), timezone_origin %in% OlsonNames()) |>
  group_by(airline,origin,dest,timezone_origin, timezone_dest) |>
  mutate(
    dep_datetime = force_tz(as.POSIXct(flight_date) + minutes(hhmm_to_minutes(crs_dep_time)), 
                            first(timezone_origin)),  
    arr_datetime = with_tz(dep_datetime + minutes(crs_elapsed_time), first(timezone_dest)),
    dep_datetime1 = as.POSIXct(flight_date) + minutes(hhmm_to_minutes(crs_dep_time))
  ) |>
  ungroup() |>
  filter(as.Date(floor_date(dep_datetime,'day')) == as.Date('2025-10-31')) |>
  select(
    airline, 
    flight_no, 
    origin, 
    dest,
    departure_local = dep_datetime,
    arr_datetime_dest = arr_datetime,
    elapsed_flight_time = crs_elapsed_time,
    origin_tail_number = tail_number)

non_stop_schedule <- sched_df_adjusted |>
  rename(
    origin_departure = departure_local,
    arr_datetime_dest = arr_datetime_dest,
    total_travel_time = elapsed_flight_time
  ) |>
  mutate(total_flight_time_label = case_when(
    total_travel_time < 60 ~ paste0(total_travel_time, ' min'),
    total_travel_time %% 60 == 0 ~ paste0(total_travel_time %/% 60, "h"),
    TRUE ~ paste0(
      total_travel_time %/% 60, "h ",
      total_travel_time %% 60, "min"
    )
  )
  )

# Minimum Connection Dataframe ----
min_mct <- sched_df_adjusted |>
  rename(
    hub = dest,
    origin_departure = departure_local, 
    origin_hub_arrival = arr_datetime_dest, 
    origin_flight_no = flight_no,
    origin_elapsed_time = elapsed_flight_time
    ) |>
  inner_join(sched_df_adjusted, by = c('airline', 'hub' = 'origin')) |>
  filter(
    origin != dest,
    departure_local > origin_hub_arrival
         ) |>
  rename(
    origin_tail_number = origin_tail_number.x,
    tail_number = origin_tail_number.y
  ) |>
  mutate(
    doors_close_time = departure_local - minutes(16),
    connection_time_min = as.numeric(difftime(doors_close_time, origin_hub_arrival, units = "mins"))
  ) |>
  filter(connection_time_min >= 30, connection_time_min <= 360) |>
  mutate(total_travel_time = origin_elapsed_time + connection_time_min + elapsed_flight_time  ) |> 
  mutate(
    connection_length = case_when(
      connection_time_min < 45 ~  '30-45min Connection',
      connection_time_min < 90 ~  '<90min Connection',
      connection_time_min <= 120 ~ '≤2hr Connection',
      connection_time_min <= 180 ~ '≤3hr Connection',
      TRUE ~ '3-6hr Connection'
    ),
    connection_label = case_when(
      connection_time_min < 60 ~ paste0(connection_time_min, ' min'),
      connection_time_min %% 60 == 0 ~ paste0(connection_time_min %/% 60, "h"),
      TRUE ~ paste0(
        connection_time_min %/% 60, "h ",
        connection_time_min %% 60, "min"
      )
    ),
    origin_flight_time_label = case_when(
      origin_elapsed_time < 60 ~ paste0(origin_elapsed_time, ' min'),
      origin_elapsed_time %% 60 == 0 ~ paste0(origin_elapsed_time %/% 60, "h"),
      TRUE ~ paste0(
        origin_elapsed_time %/% 60, "h ",
        origin_elapsed_time %% 60, "min"
      )
    ),
    dest_flight_time_label = case_when(
      elapsed_flight_time < 60 ~ paste0(elapsed_flight_time, ' min'),
      elapsed_flight_time %% 60 == 0 ~ paste0(elapsed_flight_time %/% 60, "h"),
      TRUE ~ paste0(
        elapsed_flight_time %/% 60, "h ",
        elapsed_flight_time %% 60, "min"
      )
    ),
    total_flight_time_label = case_when(
      total_travel_time < 60 ~ paste0(total_travel_time, ' min'),
      total_travel_time %% 60 == 0 ~ paste0(total_travel_time %/% 60, "h"),
      TRUE ~ paste0(
        total_travel_time %/% 60, "h ",
        total_travel_time %% 60, "min"
      )
    )
           )|>
  arrange((total_travel_time)) |>
  select(
    airline, 
    origin_flight_no,
    origin_tail_number,
    origin, 
    hub, 
    dest,
    total_flight_time_label,
    total_travel_time,
    origin_departure, 
    origin_hub_arrival,
    origin_flight_time_label,
    connection_label, 
    connection_length,
    flight_no, 
    tail_number,
    departure_local,
    arr_datetime_dest,
    dest_flight_time_label)

# find nearby airports to reduce redundancy (removing OMA-ORD-DSM or LAX-ORD-SAN) ----

# exclude airports in hawaii and alaska
hawaii_airport_list <- airports |>
  filter(Timezone == 'Pacific/Honolulu' | Timezone == 'America/Anchorage') |>
  filter(!grepl('\\\\',IATA)) |>
  pull(IATA)

airport_vector <- unique(c(min_mct$origin,min_mct$dest))

full_airport_vector <- setdiff(airport_vector,hawaii_airport_list)

airports_sf <- airportr::airports |>
  filter(IATA %in% airport_vector) |>
  filter(!grepl('\\\\', IATA)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

airport_distances <- st_distance(airports_sf, airports_sf)

airport_200_radius <- expand.grid(
  origin = airports_sf$IATA,
  nearby_airports = airports_sf$IATA,
  stringsAsFactors = FALSE
) |>
  mutate(
    distance_m = as.vector(airport_distances),
    distance_km = distance_m / 1000
  ) |>
  filter(
    origin != nearby_airports,  # Exclude self
    distance_km <= 200
  ) |>
  select(origin, nearby_airports)

unique(full_minimum_connection_time$airline)

full_minimum_connection_time <- min_mct |>
  anti_join(airport_200_radius, by = c('origin' = 'origin', 'dest' = 'nearby_airports')) |>
  mutate(airline_name = 
           case_when(airline == 'DL' ~ 'Delta Air Lines',
                     airline == 'HA' ~ 'Hawaiian Airlines',
                     airline == 'UA' ~ 'United Airlines',
                     airline == 'AA' ~ 'American Airlines',
                     airline == 'AS' ~ 'Alaska Airlines',
                     airline == 'B6' ~ 'JetBlue',
                     airline == 'WN' ~ 'Southwest Airlines',
                     airline == 'F9' ~ 'Frontier Airlines',
                     airline == 'NK' ~ 'Spirit Airlines',
                     airline == 'G4' ~ 'Allegiant Airlines')) |>
  bind_rows(non_stop_schedule) #|>
  #left_join(segment_performance,
  #          by = c('airline' = 'airline',
  #                 'origin' = 'origin',
  #                 'hub' = 'dest')) |>
  #rename(
  #  origin_on_time = on_time_rate,
  ##  origin_delay = delay_rate,
  #  origin_cancellation = cancellation_rate
  #) |>
  #left_join(segment_performance,
  #          by = c('airline' = 'airline',
  #                 'hub' = 'origin',
  #                 'dest' = 'dest')) |>
  #rename(
  #  dest_on_time = on_time_rate,
  #  dest_delay = delay_rate,
  #  dest_cancellation = cancellation_rate
  #) |>
  #left_join(segment_performance,
  #          by = c('airline' = 'airline',
  #                 'origin' = 'origin',
  #                 'dest' = 'dest')) |>
  #rename(
  #  nonstop_on_time = on_time_rate,
  #  nonstop_delay = delay_rate,
  #  nonstop_cancellation = cancellation_rate
  #) 