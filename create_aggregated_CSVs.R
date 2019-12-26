###
# This script is used to generate aggregated CSV files in order to load into shiny lighter files.
# It shall only be run once
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)

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


### CSVs for flights ###

print("Loading...")

tryCatch({
  bd_airlines = read.csv("data/usa-flight-delays/airlines.csv", header = TRUE)
  bd_airports = read.csv("data/usa-flight-delays/airports.csv", header = TRUE)
  bd_flights = read.csv("data/usa-flight-delays/flights.csv", header = TRUE)
},
error = function(e) {
  message("ERROR - You must create a 'data' folder at the root of the project with the respective train and flights folders not renamed.")
  message(e)
  stop()
})


print("Cleaning...")

#We are iterested only in few columns :
#bd_flights$AIRLINE
#bd_flights$ORIGIN_AIRPORT
#bd_flights$DESTINATION_AIRPORT
#bd_flights$DEPARTURE_DELAY
#bd_flights$TAXI_OUT
#bd_flights$ELAPSED_TIME
#bd_flights$AIR_TIME
#bd_flights$DISTANCE
#bd_flights$TAXI_IN
#bd_flights$ARRIVAL_DELAY
#bd_flights$AIR_SYSTEM_DELAY
#bd_flights$SECURITY_DELAY
#bd_flights$AIRLINE_DELAY
#bd_flights$LATE_AIRCRAFT_DELAY
#bd_flights$WEATHER_DELAY
#bd_flights$CANCELLED
#bd_flights$DIVERTED

bd_flights_reduced <- select(bd_flights, "AIRLINE", 
                             "ORIGIN_AIRPORT", 
                             "DESTINATION_AIRPORT",
                             "DISTANCE",
                             "DEPARTURE_DELAY",
                             "ARRIVAL_DELAY",
                             "ELAPSED_TIME",
                             "AIR_TIME",
                             "TAXI_OUT",
                             "TAXI_IN",
                             "AIR_SYSTEM_DELAY",
                             "SECURITY_DELAY",
                             "AIRLINE_DELAY",
                             "LATE_AIRCRAFT_DELAY",
                             "WEATHER_DELAY",
                             "CANCELLED",
                             "DIVERTED")

#Test if the airlines are present in the airlines data
Unknown_Airlines = filter(bd_flights_reduced, !(bd_flights_reduced$AIRLINE %in% bd_airlines$IATA_CODE))
if(nrow(Unknown_Airlines) > 0){
  print("Unknown Airlines Removed !")
  print(Unknown_Airlines)
  bd_flights_reduced = filter(bd_flights_reduced, bd_flights_reduced$AIRLINE %in% bd_airlines$IATA_CODE)
} else {
  print("All Airlines Regognized")
}

#Test if the origin airports are present in the airports data
Unknown_Airports = filter(bd_flights_reduced, !(bd_flights_reduced$ORIGIN_AIRPORT %in% bd_airports$IATA_CODE))
if(nrow(Unknown_Airports) > 0){
  print("Unknown Origin Airports Removed !")
  print(Unknown_Airports)
  bd_flights_reduced = filter(bd_flights_reduced, bd_flights_reduced$ORIGIN_AIRPORT %in% bd_airports$IATA_CODE)
} else {
  print("All Origin Airports Regognized")
}

#Test if the origin airports are present in the airports data
Unknown_Airports = filter(bd_flights_reduced, !(bd_flights_reduced$DESTINATION_AIRPORT %in% bd_airports$IATA_CODE))
if(nrow(Unknown_Airports) > 0){
  print("Unknown destination Airports Remouved !")
  print(Unknown_Airports)
  bd_flights_reduced = filter(bd_flights_reduced, bd_flights_reduced$DESTINATION_AIRPORT %in% bd_airports$IATA_CODE)
} else {
  print("All destination Airports Regognized")
}

#Let's add a column testing if the flight is delayed or not
bd_flights_reduced$DELAYED <- ifelse(bd_flights_reduced$ARRIVAL_DELAY > 0, 1, 0)

#Let's add a column to count the number of aggregated flights
bd_flights_reduced$COUNT <- rep(1,nrow(bd_flights_reduced))

#Fist we split the flight in 2 db (diverted and non diverted)
bd_flights_incomplete <- filter(bd_flights_reduced, DIVERTED == 1 | CANCELLED == 1)
bd_flights_complete <- filter(bd_flights_reduced, DIVERTED == 0 & CANCELLED == 0)


print("Aggregations...")
#We aggregate the data 

#bd_flights_reduced$AIRLINE               | KEY
#bd_flights_reduced$ORIGIN_AIRPORT        | KEY
#bd_flights_reduced$DESTINATION_AIRPORT   | KEY
#bd_flights_reduced$DEPARTURE_DELAY       | Count
#bd_flights_reduced$TAXI_OUT              | Count
#bd_flights_reduced$ELAPSED_TIME          | Count
#bd_flights_reduced$AIR_TIME              | Count
#bd_flights_reduced$DISTANCE              | Count
#bd_flights_reduced$TAXI_IN               | Count
#bd_flights_reduced$ARRIVAL_DELAY         | Count
#bd_flights_reduced$AIR_SYSTEM_DELAY      | Count
#bd_flights_reduced$SECURITY_DELAY        | Count
#bd_flights_reduced$AIRLINE_DELAY         | Count
#bd_flights_reduced$LATE_AIRCRAFT_DELAY   | Count
#bd_flights_reduced$WEATHER_DELAY         | Count
#bd_flights_reduced$CANCELLED             | Count
#bd_flights_reduced$DIVERTED              | Count
#bd_flights_reduced$DELAYED               | Count
#bd_flights_reduced$COUNT                 | Count

dt_flights_complete <- data.table(bd_flights_complete)
dt_flights_incomplete <- data.table(bd_flights_incomplete)

dt_flights_complete_agg <- dt_flights_complete[, 
                                               list(s.DEPARTURE_DELAY=sum(DEPARTURE_DELAY),
                                                    s.TAXI_OUT=sum(TAXI_OUT),
                                                    s.ELAPSED_TIME=sum(ELAPSED_TIME),
                                                    s.AIR_TIME=sum(AIR_TIME),
                                                    s.DISTANCE=sum(DISTANCE),
                                                    s.TAXI_IN=sum(TAXI_IN),
                                                    s.ARRIVAL_DELAY=sum(ARRIVAL_DELAY),
                                                    s.AIR_SYSTEM_DELAY=sum(AIR_SYSTEM_DELAY, na.rm=TRUE),
                                                    s.SECURITY_DELAY=sum(SECURITY_DELAY, na.rm=TRUE),
                                                    s.AIRLINE_DELAY=sum(AIRLINE_DELAY, na.rm=TRUE),
                                                    s.LATE_AIRCRAFT_DELAY=sum(LATE_AIRCRAFT_DELAY, na.rm=TRUE),
                                                    s.WEATHER_DELAY=sum(WEATHER_DELAY, na.rm=TRUE),
                                                    s.DELAYED=sum(DELAYED),
                                                    s.COUNT=sum(COUNT)),
                                               by=c("AIRLINE", "ORIGIN_AIRPORT", "DESTINATION_AIRPORT")]

dt_flights_incomplete_agg <- dt_flights_incomplete[, 
                                                   list(s.DEPARTURE_DELAY=sum(DEPARTURE_DELAY),
                                                        s.TAXI_OUT=sum(TAXI_OUT),
                                                        s.ELAPSED_TIME=sum(ELAPSED_TIME),
                                                        s.AIR_TIME=sum(AIR_TIME),
                                                        s.DISTANCE=sum(DISTANCE),
                                                        s.TAXI_IN=sum(TAXI_IN),
                                                        s.ARRIVAL_DELAY=sum(ARRIVAL_DELAY),
                                                        s.AIR_SYSTEM_DELAY=sum(AIR_SYSTEM_DELAY, na.rm=TRUE),
                                                        s.SECURITY_DELAY=sum(SECURITY_DELAY, na.rm=TRUE),
                                                        s.AIRLINE_DELAY=sum(AIRLINE_DELAY, na.rm=TRUE),
                                                        s.LATE_AIRCRAFT_DELAY=sum(LATE_AIRCRAFT_DELAY, na.rm=TRUE),
                                                        s.WEATHER_DELAY=sum(WEATHER_DELAY, na.rm=TRUE),
                                                        s.DELAYED=sum(DELAYED),
                                                        s.COUNT=sum(COUNT),
                                                        s.DIVERTED=sum(DIVERTED),
                                                        s.CANCELLED=sum(CANCELLED)),
                                                   by=c("AIRLINE", "ORIGIN_AIRPORT", "DESTINATION_AIRPORT")]

final_flights_agg <- merge(dt_flights_complete_agg,select(dt_flights_incomplete_agg, "AIRLINE", "ORIGIN_AIRPORT", "DESTINATION_AIRPORT", "s.DIVERTED", "s.CANCELLED"), by=c("AIRLINE", "ORIGIN_AIRPORT", "DESTINATION_AIRPORT"), all.x=TRUE)
final_flights_agg$s.DIVERTED[is.na(final_flights_agg$s.DIVERTED)] <- 0
final_flights_agg$s.CANCELLED[is.na(final_flights_agg$s.CANCELLED)] <- 0

print("Exporting...")

write.csv(final_flights_agg, file = "data/usa-flight-delays/flights_complete_agg.csv")


