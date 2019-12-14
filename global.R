library(ggplot2)
library(dplyr)
library(gridExtra)

TRAINS <- read.csv("data/french-sncf-trains-regularities/full_trains.csv")
MIN_DATE <- TRAINS %>% select(year) %>% min() %>% as.numeric()
MAX_DATE <- TRAINS %>% select(year) %>% max() %>% as.numeric()
STATION_NAMES <-  TRAINS %>% select(departure_station) %>% distinct() %>% 
                    arrange(departure_station) %>% rename(Station = departure_station)