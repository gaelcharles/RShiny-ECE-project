fluidPage(
  
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),

  # includeCSS("style.css"),
  shinyjs::useShinyjs(),
  
  navbarPage("Trains and flights dashboards",
    
    ### TRAINS ###
    tabPanel("Trains",
             
      ## SIDEBAR PANEL ##
      sidebarPanel(
        h3("Trains control panel"),
        hr(),
        
        radioButtons("aggregation_by",
          label = h4("Aggregate by:"),
          choices=c("Year" = "year", "Station" = "station")),
        
        hr(),
        # Departures
        selectInput("departure_station",
          label = h4("Departure station"),
          choices = STATION_NAMES),
        checkboxInput("all_stations",
          label = "Aggregate on all stations",
          value = FALSE),
        
        # Year range
        sliderInput("year",
          label = h4("Year"),
          min = MIN_DATE,
          max = MAX_DATE,
          value = MAX_DATE,
          ticks = FALSE,
          sep = ""),
        
        # Aggregation types (different plots depending on radio buttons)
        radioButtons("aggregation_type",
         label = NULL,
         choices=c("Total aggregations" = "tot",
                   "Average/median aggregations" = "avg",
                   "Proportion aggregations (%)" = "pct"))
      ),
      
      ## MAIN PANEL ##
      mainPanel(
        h3("Resulting plots"),
        textOutput("trains_plotParams"),
        plotOutput("trains_plot")
      )
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

