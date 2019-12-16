library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(reshape2)

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
  
  delays <- read.csv("data/french-sncf-trains-regularities/regularite-mensuelle-tgv-aqst.csv",
                            sep=';', check.names=FALSE, encoding="UTF-8")
  
  delays <- delays %>% select("Année",
                              "Gare de départ",
                              "Retard pour causes externes",
                              "Retard à cause infrastructure ferroviaire",
                              "Retard à cause gestion trafic",
                              "Retard à cause matériel roulant",
                              "Retard à cause gestion en gare et réutilisation de matériel",
                              "Retard à cause prise en compte voyageurs") %>% drop_na()
  
  delays_year_st <- aggregate(delays %>% select(starts_with("Retard")),
                              by = list(year = delays[,1], station = delays[,2]),
                              FUN = mean)
  
  delays_year <- aggregate(delays %>% select(starts_with("Retard")),
                           by = list(year = delays[,1]),
                           FUN = mean)
  
  delays_melted_year_st <- delays_year_st %>% melt(
    id.vars = c("year", "station"),
    measure.vars = c("Retard pour causes externes",
                     "Retard à cause infrastructure ferroviaire",
                     "Retard à cause gestion trafic",
                     "Retard à cause matériel roulant",
                     "Retard à cause gestion en gare et réutilisation de matériel",
                     "Retard à cause prise en compte voyageurs"),
    variable.name = "delay_cause",
    value.name = "proportion")
    
  delays_melted_year <- delays_year %>% melt(
    id.vars = c("year"),
    measure.vars = c("Retard pour causes externes",
                     "Retard à cause infrastructure ferroviaire",
                     "Retard à cause gestion trafic",
                     "Retard à cause matériel roulant",
                     "Retard à cause gestion en gare et réutilisation de matériel",
                     "Retard à cause prise en compte voyageurs"),
    variable.name = "delay_cause",
    value.name = "proportion")
  
  agg_sum_year_st <- aggregate(
    TRAINS %>% select(total_num_trips, num_of_canceled_trains,
                      num_late_at_departure, num_arriving_late),
    by = list(year = TRAINS$year, station = TRAINS$departure_station),
    FUN = sum) %>%
    mutate(carried_out = total_num_trips - num_of_canceled_trains)
  
  agg_sum_year <- aggregate(
    TRAINS %>% select(total_num_trips, num_of_canceled_trains,
                      num_late_at_departure, num_arriving_late),
    by = list(year = TRAINS$year),
    FUN = sum) %>%
    mutate(carried_out = total_num_trips - num_of_canceled_trains)
  
  agg_avg_year_st <- aggregate(
    TRAINS %>% select(num_late_at_departure, num_arriving_late,
                      avg_delay_late_at_departure, avg_delay_all_departing,
                      avg_delay_late_on_arrival, avg_delay_all_arriving),
    by = list(year = TRAINS$year, station = TRAINS$departure_station),
    FUN = mean) %>%
    rename(avg_late_at_departure = num_late_at_departure) %>%
    rename(avg_arriving_late = num_arriving_late)
  
  agg_avg_year <- aggregate(
    TRAINS %>% select(num_late_at_departure, num_arriving_late,
                      avg_delay_late_at_departure, avg_delay_all_departing,
                      avg_delay_late_on_arrival, avg_delay_all_arriving),
    by = list(year = TRAINS$year),
    FUN = mean) %>%
    rename(avg_late_at_departure = num_late_at_departure) %>%
    rename(avg_arriving_late = num_arriving_late)
  
  agg_sum_year_st %>% merge(agg_avg_year_st) %>%
    write.csv("data/agg_byYearStation.csv", row.names=FALSE)
  
  agg_sum_year %>% merge(agg_avg_year) %>% 
    write.csv("data/agg_byYear.csv", row.names=FALSE)
  
  delays_melted_year_st %>% write.csv("data/delays_melted_byYearStation.csv", row.names=FALSE)
  
  delays_melted_year %>% write.csv("data/delays_melted_byYear.csv", row.names=FALSE)
}

createAggregatedDFs()