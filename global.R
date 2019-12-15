library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)

TRAINS <- read.csv("data/french-sncf-trains-regularities/full_trains.csv") %>%
  replace_na(list(num_arriving_late=0, avg_delay_late_on_arrival=0, num_greater_15_min_late=0, avg_delay_late_greater_15_min=0, num_greater_30_min_late=0, num_greater_60_min_late=0))
MIN_DATE <- TRAINS %>% select(year) %>% min() %>% as.numeric()
MAX_DATE <- TRAINS %>% select(year) %>% max() %>% as.numeric()
STATION_NAMES <-  TRAINS %>% select(departure_station) %>% distinct() %>% 
                    arrange(departure_station) %>% rename(Station = departure_station)

createAggregatedDFs <- function() {
  
  aggregate(TRAINS %>% select(total_num_trips, num_of_canceled_trains,
                              num_late_at_departure, num_arriving_late),
            by = list(year = TRAINS$year, station = TRAINS$departure_station),
            FUN = sum) %>%
    mutate(carried_out = total_num_trips - num_of_canceled_trains) %>%
    write.csv("data/temp_sumAgg_ByYearStation.csv", row.names=FALSE)
  
  aggregate(TRAINS %>% select(total_num_trips, num_of_canceled_trains,
                              num_late_at_departure, num_arriving_late),
            by = list(year = TRAINS$year),
            FUN = sum) %>%
    mutate(carried_out = total_num_trips - num_of_canceled_trains) %>%
    write.csv("data/temp_sumAgg_ByYear.csv", row.names=FALSE)
}

createAggregatedDFs()