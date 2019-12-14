trains <- read.csv("data/french-sncf-trains-regularities/full_trains.csv")

# Custom functions - we define them here in order not to overcharge ui.R
getStationNames <- function() {
  # Get unique values for "departure_station", sorted
  stations <- trains %>% 
    select(departure_station) %>% 
    distinct() %>% 
    arrange(departure_station) %>%
    rename(Station = departure_station)
  
  return(stations)
}