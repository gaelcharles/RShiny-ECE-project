getStationNames <- function() {
  # Retrieve raw data
  trains = read.csv("data/french-sncf-trains-regularities/full_trains.csv")
  
  # Get unique values for "departure_station", sorted
  stations <- trains %>% 
    select(departure_station) %>% 
    distinct() %>% 
    arrange(departure_station) %>%
    rename(Station = departure_station)
  
  return(stations)
}

fluidPage(
  
  #   # tags$head(
  #   #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  #   # ),
  #   
  #   # includeCSS("style.css"),
  
  navbarPage("Trains and flights dashboards",
    
    tabPanel("Trains",
      sidebarPanel(
        h3("Trains control panel"),
        hr(),
        selectInput("departure_station",
                    label = h4("Departure station"),
                    choices = getStationNames()),
        sliderInput("year_range",
                    label = h4("Year range"),
                    min = 2015,
                    max = 2018,
                    value = c(2015, 2018),
                    ticks = FALSE,
                    sep = ""),
        div(class="aggregation_type_class",
            radioButtons("aggregation_type",
                         label = NULL,
                         choices=c("Total aggregations"          = "tot",
                                   "Average/median aggregations" = "avg",
                                   "Proportion aggregations (%)" = "pct"))
        )
      ),
      mainPanel(h3("Future plots"))
    ),
    tabPanel("Flights",
     sidebarPanel(
       h3("Flights control panel"),
       hr()
     ),
     mainPanel(h3("Future plots"))
    )
  )
)

