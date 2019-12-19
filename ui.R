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
        fluidRow(
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
                   "Proportion aggregations (%)" = "pct")),
        
        hr(),
        
        # User-defined graph size
        column(6, sliderInput("dynamic_width",
                              label = "Graph width",
                              min = 500,
                              max = 1200,
                              value = 600)),
        column(6, sliderInput("dynamic_height",
                              label = "Graph height",
                              min = 500,
                              max = 2000,
                              value = 600)))
      ),
      
      ## MAIN PANEL ##
      mainPanel(
        h3("Resulting plots"),
        textOutput("trains_plotParams"),
        uiOutput("trains_plotUI")
      )
    ),
    
    ###FLIGHTS###
    tabPanel("Flights",
            leafletOutput("mymap",height = "90vh", width = "95vw"),
            
            absolutePanel(
              top = "10vh",
              right = "5vw",
              height = "85vh",
              width = "38vw",
              draggable = TRUE,
              style = "background-color: rgba(100,100,100,0.3); padding:20px;",
              
              selectInput("Selected_Airline", "Airline on the Map", bd_airlines$AIRLINE, selected = bd_airlines$AIRLINE[1]),
              selectInput("Type_of_agg", "Compare", c("Number of Flights", 
                                                      "Number of delayed flights", 
                                                      "Average flight duration",
                                                      "Average flight distance", 
                                                      "The total distance covered", 
                                                      "The total distance covered",
                                                      "Average departure delay",
                                                      "Average arrival delay")),
              selectInput("Airlines_Or_Airports", "Bettween", c("Airlines", "Airports")),
              plotOutput("plot_flights", width = "35vw", height = "50vh")
            )
             
            )
    
  )
)


