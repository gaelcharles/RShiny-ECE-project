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

Filter(Negate(function(x) is.null(unlist(x))), network)