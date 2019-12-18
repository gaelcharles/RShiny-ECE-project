library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)

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