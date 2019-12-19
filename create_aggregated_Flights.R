## Flight Aggregation
library(dplyr)

print("Loading...")

bd_airlines = read.csv("data/usa-flight-delays/airlines.csv", header = TRUE)
bd_airports = read.csv("data/usa-flight-delays/airports.csv", header = TRUE)
bd_flights = read.csv("data/usa-flight-delays/flights.csv", header = TRUE)


print("Cleanning...")

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
  print("Unknown Airlines Remouved !")
  print(Unknown_Airlines)
  bd_flights_reduced = filter(bd_flights_reduced, bd_flights_reduced$AIRLINE %in% bd_airlines$IATA_CODE)
} else {
  print("All Airlines Regognised")
}

#Test if the origin airports are present in the airports data
Unknown_Airports = filter(bd_flights_reduced, !(bd_flights_reduced$ORIGIN_AIRPORT %in% bd_airports$IATA_CODE))
if(nrow(Unknown_Airports) > 0){
  print("Unknown Origin Airports Remouved !")
  print(Unknown_Airports)
  bd_flights_reduced = filter(bd_flights_reduced, bd_flights_reduced$ORIGIN_AIRPORT %in% bd_airports$IATA_CODE)
} else {
  print("All Origin Airports Regognised")
}

#Test if the origin airports are present in the airports data
Unknown_Airports = filter(bd_flights_reduced, !(bd_flights_reduced$DESTINATION_AIRPORT %in% bd_airports$IATA_CODE))
if(nrow(Unknown_Airports) > 0){
  print("Unknown destination Airports Remouved !")
  print(Unknown_Airports)
  bd_flights_reduced = filter(bd_flights_reduced, bd_flights_reduced$DESTINATION_AIRPORT %in% bd_airports$IATA_CODE)
} else {
  print("All destination Airports Regognised")
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

#We use datatable as it is easyier
require(data.table)

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

