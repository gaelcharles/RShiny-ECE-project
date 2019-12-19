#Global Libraries
library(shiny)
library(dplyr)

#Train Libraries
library(ggplot2)
library(gridExtra)

#Flight Libraries
library(leaflet)
library(igraph)
library(sp)

# File checker - if any of these does not exist, source the script that creates them
for (file in c(
  "agg_byYear.csv",
  "agg_byYearStation.csv",
  "agg_melted_byYear.csv",
  "agg_melted_byYearStation.csv",
  "delays_melted_byYear.csv",
  "delays_melted_byYearStation.csv"
)) {
  if(!file.exists(paste('data/french-sncf-trains-regularities/', file, sep=''))) {
    message("Aggregated CSVs not found. Creating these files now...")
    source('create_aggregated_CSVs.R')
    message("CSVs created. The app is ready to start.")
    break
  }
}

temp <- read.csv("data/french-sncf-trains-regularities/agg_byYearStation.csv")
MIN_DATE <- temp %>% select(year) %>% min() %>% as.numeric()
MAX_DATE <- temp %>% select(year) %>% max() %>% as.numeric()
STATION_NAMES <-  temp %>% select(station) %>% distinct() %>% arrange(station)


#Flights Import

bd_airlines = read.csv("data/usa-flight-delays/airlines.csv", header = TRUE)
bd_airports = read.csv("data/usa-flight-delays/airports.csv", header = TRUE)
bd_flights = read.csv("data/usa-flight-delays/flights_complete_agg.csv", header = TRUE)

#Generating networks
print("Generating Networks...")
airlines_edges <- list()
network <- list()
for (airline in 1:nrow(bd_airlines)) {
  print(airline/nrow(bd_airlines) *100)
  
  vert <- select(bd_airports, 
                 "IATA_CODE",
                 "LATITUDE",
                 "LONGITUDE")
  
  n <- graph_from_data_frame(select(filter(bd_flights, AIRLINE == bd_airlines[airline, 1]),
                                    "ORIGIN_AIRPORT", 
                                    "DESTINATION_AIRPORT",
                                    "s.COUNT"),
                             directed = TRUE,
                             vertices = vert )
  network[[airline]] <- get.data.frame(n, "both")
  vert <- network[[airline]]$vertices
  coordinates(vert) <- ~ LONGITUDE + LATITUDE
  edges <- network[[airline]]$edges
  edges <- lapply(1:nrow(edges), function(i) {
    as(rbind(vert[vert$name == edges[i, "from"], ],
             vert[vert$name == edges[i, "to"], ]),
       "SpatialLines")
  })
  
  for (i in seq_along(edges)) {
    edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
  }
  
  airlines_edges[[airline]] <- do.call(rbind, edges)
}

print("Aggregations...")
bd_airports$Flight_Count <- aggregate(bd_flights$s.COUNT, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x
bd_airports$Delayed_Count <- aggregate(bd_flights$s.DELAYED, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x
bd_airports$Diverted_Count <- aggregate(bd_flights$s.DIVERTED, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x
bd_airports$Canceled_Count <- aggregate(bd_flights$s.CANCELLED, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x
bd_airports$Total_Distance <- aggregate(bd_flights$s.DISTANCE, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x
bd_airports$Average_Distance <- bd_airports$Total_Distance / bd_airports$Flight_Count
bd_airports$Average_Flight_Time <- aggregate(bd_flights$s.ELAPSED_TIME, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x / bd_airports$Flight_Count
bd_airports$Average_Departure_Delay <- aggregate(bd_flights$s.DEPARTURE_DELAY, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x / bd_airports$Flight_Count
bd_airports$Average_Taxi_Out <- aggregate(bd_flights$s.TAXI_OUT, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x / bd_airports$Flight_Count
bd_airports$Average_Air_System_Delay <- aggregate(bd_flights$s.AIR_SYSTEM_DELAY, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x / bd_airports$Flight_Count
bd_airports$Average_Security_Delay <- aggregate(bd_flights$s.SECURITY_DELAY, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x / bd_airports$Flight_Count
bd_airports$Average_Airline_Delay <- aggregate(bd_flights$s.AIRLINE_DELAY, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x / bd_airports$Flight_Count
bd_airports$Average_Late_Aircraft_Delay <- aggregate(bd_flights$s.LATE_AIRCRAFT_DELAY, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x / bd_airports$Flight_Count
bd_airports$Average_Weather_Delay <- aggregate(bd_flights$s.WEATHER_DELAY, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x / bd_airports$Flight_Count
bd_airports$Average_Weather_Delay <- aggregate(bd_flights$s.WEATHER_DELAY, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x / bd_airports$Flight_Count
bd_airports$Average_Arrival_Delay <- aggregate(bd_flights$s.ARRIVAL_DELAY, by=list(Category=bd_flights$ORIGIN_AIRPORT), FUN=sum)$x / bd_airports$Flight_Count

bd_airlines$Flight_Count <- aggregate(bd_flights$s.COUNT, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x
bd_airlines$Delayed_Count <- aggregate(bd_flights$s.DELAYED, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x
bd_airlines$Diverted_Count <- aggregate(bd_flights$s.DIVERTED, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x
bd_airlines$Canceled_Count <- aggregate(bd_flights$s.CANCELLED, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x
bd_airlines$Total_Distance <- aggregate(bd_flights$s.DISTANCE, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x
bd_airlines$Average_Distance <- bd_airlines$Total_Distance / bd_airlines$Flight_Count
bd_airlines$Average_Flight_Time <- aggregate(bd_flights$s.ELAPSED_TIME, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x / bd_airlines$Flight_Count
bd_airlines$Average_Departure_Delay <- aggregate(bd_flights$s.DEPARTURE_DELAY, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x / bd_airlines$Flight_Count
bd_airlines$Average_Taxi_Out <- aggregate(bd_flights$s.TAXI_OUT, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x / bd_airlines$Flight_Count
bd_airlines$Average_Air_System_Delay <- aggregate(bd_flights$s.AIR_SYSTEM_DELAY, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x / bd_airlines$Flight_Count
bd_airlines$Average_Security_Delay <- aggregate(bd_flights$s.SECURITY_DELAY, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x / bd_airlines$Flight_Count
bd_airlines$Average_Airline_Delay <- aggregate(bd_flights$s.AIRLINE_DELAY, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x / bd_airlines$Flight_Count
bd_airlines$Average_Late_Aircraft_Delay <- aggregate(bd_flights$s.LATE_AIRCRAFT_DELAY, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x / bd_airlines$Flight_Count
bd_airlines$Average_Weather_Delay <- aggregate(bd_flights$s.WEATHER_DELAY, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x / bd_airlines$Flight_Count
bd_airlines$Average_Weather_Delay <- aggregate(bd_flights$s.WEATHER_DELAY, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x / bd_airlines$Flight_Count
bd_airlines$Average_Arrival_Delay <- aggregate(bd_flights$s.ARRIVAL_DELAY, by=list(Category=bd_flights$AIRLINE), FUN=sum)$x / bd_airlines$Flight_Count

print("Done !")



