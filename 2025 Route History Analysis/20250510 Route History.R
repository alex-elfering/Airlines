library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)

#### data pulling & variables ----
DOM_PATH <- "~/Airline Analysis/T100 Domestic"
INTL_PATH <- "~/Airline Analysis/T100 International All"
AIRCRAFT_PATH <- "~/Airline Analysis/L_AIRCRAFT_TYPE.csv"

CARRIERS <- c("F9","AA","UA","DL","B6","WN","G4","NK","HA","AS","SY",
              "NW","US","CO","VX","TW")

ac_map <- tibble::tibble(
  # clean up messy or inconsistent aircraft descriptions
  pattern = c(
    "McDonnell Douglas DC-10",
    "McDonnell Douglas DC-8",
    "McDonnell Douglas DC-9",
    "McDonnell Douglas DC9 Super 80/MD81/82/83/88",
    "McDonnell Douglas DC9 Super 87",
    "McDonnell Douglas MD-11",
    "McDonnell Douglas MD-90",
    "A220|A200",
    "A-318",
    "A319",
    "A320-200n",
    "A321-200n",
    "A321",
    "A330-200",
    "A330-300",
    "A330-900",
    "A300",
    "A310",
    "A320-100/200",
    "A350-900",
    "B787-800",
    "B787-900",
    "Boeing 787-10",
    "Boeing 727-200",
    "Boeing 737-900ER|Boeing 737-900",
    "Boeing B737 Max 800",
    "Boeing B737 Max 900",
    "Boeing 737-700/700LR/Max 7",
    "Boeing 737-200C",
    "Boeing 767-200",
    "Boeing 767-300",
    "Boeing 767-400",
    "Boeing 777-200",
    "Boeing 777-300",
    "L-1011"
  ),
  clean = c(
    "DC-10", "DC-8", "DC-9", "MD-80 Series", "MD-87", "MD-11", "MD-90",
    "Airbus A220", "Airbus A318", "Airbus A319", "Airbus A320neo", "Airbus A321neo", "Airbus A321",
    "Airbus A330-200", "Airbus A330-300", "Airbus A330-900", "Airbus A300", "Airbus A310", "Airbus A320",
    "Airbus A350-900",
    "Boeing 787-800", "Boeing 787-900", "Boeing 787-10",
    "Boeing 727-200", "Boeing 737-900",
    "Boeing 737 MAX 8", "Boeing 737 MAX 9", "Boeing 737-700/MAX 7", "Boeing 737-200",
    "Boeing 767-200", "Boeing 767-300", "Boeing 767-400",
    "Boeing 777-200", "Boeing 777-300",
    "Lockheed L-1011"
  )
)

excluded_desc <- c(
  # fleet-types to exclude (unlikely to be passenger service)
  "Aerospatiale Caravelle SE-210",
  "Aerospatiale/Aeritalia ATR-42",
  "British Aerospace BAe-146-200",
  "Embraer-Emb-170",
  "Canadair RJ-200ER /RJ-440",
  "British Aerospace BAe-146-100/RJ70",
  "De Havilland DHC7 Dash-7",
  "Dornier 328",
  "Fokker F28-1000 Fellowship",
  "Fokker F28-4000/6000 Fellowship",
  "Fokker Friendship F-27/Fairchild F-27/A/B/F/J",
  "Embraer EMB-120 Brasilia",
  "Fairchild-Hiller FH-227"
)

# functions ----
read_t100 <- function(path) {
  # Reads all CSVs in a folder and combines them into one dataset
  # Assumes the files are T-100 format (domestic or international)
  # Input: folder path
  
  list.files(path, "\\.csv$", full.names = TRUE) |>
    lapply(fread) |>
    rbindlist(fill = TRUE) |>
    clean_names()
}

summarise_t100 <- function(df) {
  # Filters and summarizes T-100 data
  # Keeps routes flown 10+ times, filters to selected carriers
  # Groups by month, origin-destination, and aircraft type
  # Returns total seats, passengers, and departures
  
  df |>
    filter(carrier %in% CARRIERS, departures_performed >= 10) |>
    group_by(carrier, origin, dest, year, month, aircraft_type) |>
    summarise(
      seats       = sum(seats,       na.rm = TRUE),
      passengers  = sum(passengers,  na.rm = TRUE),
      departures  = sum(departures_performed, na.rm = TRUE),
      .groups     = "drop"
    )
}

clean_aircraft <- function(desc, map_df) {
  # Simplifies aircraft names by matching patterns in a lookup table
  # Uses the first matching pattern, otherwise keeps the original name
  # Input: vector of aircraft names, and a pattern→name mapping table
  
  result <- rep(NA_character_, length(desc))
  for (i in seq_len(nrow(map_df))) {
    pattern <- map_df$pattern[i]
    match_idx <- str_detect(desc, regex(pattern, ignore_case = TRUE)) & is.na(result)
    result[match_idx] <- map_df$clean[i]
  }
  # fallback: keep original if no match
  ifelse(is.na(result), desc, result)
}

service_history <- function(df) {
  # Looks at service history for each route and carrier by year
  # Identifies when service started, stopped, or resumed
  # Adds flags for new service, gaps in service, and service streaks
  
  df |>
    group_by(carrier, origin, dest, year, international_service) |>
    summarise(seats = sum(seats, na.rm = TRUE), .groups = "drop") |>
    group_by(carrier, origin, dest) |>
    complete(year = seq.int(min(year), 2024, by = 1)) |>
    mutate(
      service          = as.integer(!is.na(seats)),
      service_ends     = as.integer( lead(is.na(seats)) &  !is.na(seats)),
      service_resumes  = as.integer(!is.na(seats) & lag(is.na(seats))),
      new_service      = if_else(is.na(service_resumes), 1L, 0L),
      consecutive_streak = data.table::rowid(data.table::rleid(service)) * service
    ) |>
    filter(service == 1L) |>
    mutate(
      last_served     = if_else(service_resumes == 1L, lag(year), NA_integer_),
      last_streak     = if_else(service_resumes == 1L, lag(consecutive_streak), NA_integer_),
      years_since     = year - last_served,
      previously_began= last_served - (last_streak - 1L),
      service_level   = case_when(
        new_service==1L & service_ends==0L ~ "Service Established",
        new_service==1L & service_ends==1L ~ "Service Established, then Terminated",
        service_ends==1L & service_resumes==0L ~ "Service Terminated",
        service_ends==1L & service_resumes==1L ~ "Service Resumed, then Terminated",
        service_ends==0L & service_resumes==1L ~ "Service Resumed",
        TRUE ~ "Service"
      )
    ) |>
    ungroup()
}
# execute ----
domestic_sum  <- read_t100(DOM_PATH) |> summarise_t100() |> mutate(international_service = 0)
international_sum <- read_t100(INTL_PATH) |> summarise_t100() |> mutate(international_service = 1)

aircraft <- fread(AIRCRAFT_PATH) |> clean_names()

combined <- bind_rows(domestic_sum, international_sum) |>
  left_join(aircraft, by = c("aircraft_type" = "code")) |>
  filter(!description %in% excluded_desc) |>
  mutate(clean_description = clean_aircraft(description, ac_map))

history <- service_history(combined)