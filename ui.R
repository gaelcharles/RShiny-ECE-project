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
        
        # Departures
        selectInput("departure_station",
          label = h4("Departure station"),
          choices = getStationNames()),
        checkboxInput("all_stations",
          label = "Aggregate on all stations",
          value = FALSE),
        
        # Year range
        sliderInput("year_range",
          label = h4("Year range"),
          min = 2015,
          max = 2018,
          value = c(2015, 2018),
          ticks = FALSE,
          sep = ""),
        
        # Aggregation types (different plots depending on radio buttons)
        radioButtons("aggregation_type",
         label = NULL,
         choices=c("Total aggregations"          = "tot",
                   "Average/median aggregations" = "avg",
                   "Proportion aggregations (%)" = "pct"))
      ),
      
      ## MAIN PANEL ##
      mainPanel(
        h3("Resulting plots"),
        h4(textOutput("trains_plotParams"))
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

