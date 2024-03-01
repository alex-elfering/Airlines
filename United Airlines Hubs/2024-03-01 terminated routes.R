source('C:/Users/alexe/OneDrive/Documents/GitHub/Airlines/United Airlines Hubs/2024-02-23 United Airlines Hubs.R')

old_routes <- origin_dest_service_history |>
  group_by(dest) |>
  filter(year == max(year)) |>
  ungroup() |>
  filter(grepl('Terminated', service_level)) |>
  arrange(year)

old_routes

write.csv(origin_dest_service_history,"C:/Users/alexe/OneDrive/Documents/GitHub/Airlines/Historic Airline Hub Service/terminated routes.csv")