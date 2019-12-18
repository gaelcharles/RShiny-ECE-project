###
# This script is used to generate aggregated CSV files in order to load into shiny lighter files.
# It shall only be run once
library(dplyr)
library(tidyr)
library(reshape2)


### CSV FOR TRAINS ###

tryCatch({
  TRAINS <- read.csv("data/french-sncf-trains-regularities/full_trains.csv") %>%
    replace_na(list(num_arriving_late=0,
                    avg_delay_late_on_arrival=0,
                    num_greater_15_min_late=0,
                    avg_delay_late_greater_15_min=0,
                    num_greater_30_min_late=0,
                    num_greater_60_min_late=0))
  },
  error = function(e) {
    message("ERROR - Raw trains and flights CSV files must be put under their respective folders in a 'data' folder at the root of the project.")
    message(e)
    stop()
  }
)

tryCatch({
  delays <- read.csv("data/french-sncf-trains-regularities/regularite-mensuelle-tgv-aqst.csv",
                     sep=';', check.names=FALSE, encoding="Latin-1") %>% 
    select("Année",
           "Gare de départ",
           "Retard pour causes externes",
           "Retard à cause infrastructure ferroviaire",
           "Retard à cause gestion trafic",
           "Retard à cause matériel roulant",
           "Retard à cause gestion en gare et réutilisation de matériel",
           "Retard à cause prise en compte voyageurs") %>% drop_na()
},
error = function(e) {
  message("ERROR - Raw trains and flights CSV files must be put under their respective folders in a 'data' folder at the root of the project.")
  message(e)
  stop()
}
)

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
  mutate(num_carried_out = total_num_trips - num_of_canceled_trains) %>%
  mutate(canceled = 100*(num_of_canceled_trains/total_num_trips)) %>%
  mutate(carried_out = 100-canceled)

agg_sum_year <- aggregate(
  TRAINS %>% select(total_num_trips, num_of_canceled_trains,
                    num_late_at_departure, num_arriving_late),
  by = list(year = TRAINS$year),
  FUN = sum) %>%
  mutate(num_carried_out = total_num_trips - num_of_canceled_trains) %>%
  mutate(canceled = 100*(num_of_canceled_trains/total_num_trips)) %>%
  mutate(carried_out = 100-canceled)

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

agg_pct_year_st <- agg_sum_year_st %>% select(year, station, canceled, carried_out)

agg_pct_year_st <- agg_pct_year_st %>% melt(
  id.vars = c("year", "station"),
  measure.vars = c("canceled", "carried_out"),
  variable.name = "train_state",
  value.name = "proportion"
)

agg_pct_year <- agg_sum_year %>% select(year, canceled, carried_out)

agg_pct_year <- agg_pct_year %>% melt(
  id.vars = c("year"),
  measure.vars = c("canceled", "carried_out"),
  variable.name = "train_state",
  value.name = "proportion"
)

# CSV Outputs
tryCatch({
  agg_sum_year_st %>% merge(agg_avg_year_st) %>%
    write.csv("data/french-sncf-trains-regularities/agg_byYearStation.csv", row.names=FALSE)
  
  agg_sum_year %>% merge(agg_avg_year) %>% 
    write.csv("data/french-sncf-trains-regularities/agg_byYear.csv", row.names=FALSE)
  
  delays_melted_year_st %>% write.csv("data/french-sncf-trains-regularities/delays_melted_byYearStation.csv", row.names=FALSE)
  
  delays_melted_year %>% write.csv("data/french-sncf-trains-regularities/delays_melted_byYear.csv", row.names=FALSE)
  
  agg_pct_year_st %>% write.csv("data/french-sncf-trains-regularities/agg_melted_byYearStation.csv", row.names=FALSE)
  
  agg_pct_year %>% write.csv("data/french-sncf-trains-regularities/agg_melted_byYear.csv", row.names=FALSE)
},
error = function(e) {
  message("ERROR - You must create a 'data' folder at the root of the project with the respective train and flights folders not renamed.")
  message(e)
  stop()
}
)

