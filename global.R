library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)

TRAINS <- read.csv("data/french-sncf-trains-regularities/full_trains.csv") %>%
  replace_na(list(num_arriving_late=0,
                  avg_delay_late_on_arrival=0,
                  num_greater_15_min_late=0,
                  avg_delay_late_greater_15_min=0,
                  num_greater_30_min_late=0,
                  num_greater_60_min_late=0))
MIN_DATE <- TRAINS %>% select(year) %>% min() %>% as.numeric()
MAX_DATE <- TRAINS %>% select(year) %>% max() %>% as.numeric()
STATION_NAMES <-  TRAINS %>% select(departure_station) %>% distinct() %>% 
                    arrange(departure_station) %>% rename(Station = departure_station)

createAggregatedDFs <- function() {
  
  agg_sum_year_st <- aggregate(TRAINS %>% select(total_num_trips, num_of_canceled_trains,
                                              num_late_at_departure, num_arriving_late),
                            by = list(year = TRAINS$year, station = TRAINS$departure_station),
                            FUN = sum) %>%
                      mutate(carried_out = total_num_trips - num_of_canceled_trains)
                  
  agg_sum_year <- aggregate(TRAINS %>% select(total_num_trips, num_of_canceled_trains,
                                              num_late_at_departure, num_arriving_late),
                            by = list(year = TRAINS$year),
                            FUN = sum) %>%
                    mutate(carried_out = total_num_trips - num_of_canceled_trains)
  
  agg_avg_year_st <- aggregate(TRAINS %>% select(num_late_at_departure, num_arriving_late,
                                                 avg_delay_late_at_departure, avg_delay_all_departing,
                                                 avg_delay_late_on_arrival, avg_delay_all_arriving),
                               by = list(year = TRAINS$year, station = TRAINS$departure_station),
                               FUN = mean) %>%
                      rename(avg_late_at_departure = num_late_at_departure) %>%
                      rename(avg_arriving_late = num_arriving_late)
  
  agg_avg_year <- aggregate(TRAINS %>% select(num_late_at_departure, num_arriving_late,
                                              avg_delay_late_at_departure, avg_delay_all_departing,
                                              avg_delay_late_on_arrival, avg_delay_all_arriving),
                            by = list(year = TRAINS$year),
                            FUN = mean) %>%
                    rename(avg_late_at_departure = num_late_at_departure) %>%
                    rename(avg_arriving_late = num_arriving_late)
  
  merge(agg_sum_year_st, agg_avg_year_st) %>% write.csv("data/agg_byYearStation.csv", row.names=FALSE)
  merge(agg_sum_year,    agg_avg_year)    %>% write.csv("data/agg_byYear.csv",        row.names=FALSE)
}

createAggregatedDFs()