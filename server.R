function(input, output) {

  # REACTIVE ELEMENTS #
  react_allStations <- reactive({
    input$all_stations # Checkbox "aggregate on all station"
  })
  
  react_aggregationBy <- reactive({
    input$aggregation_by # Radio buttons "Aggregate by"
  })
  
  # OUTPUT ELEMENTS #
  output$trains_plotParams <- renderText({
    
    # Retrieve aggregation type
    agg_type <- switch(input$aggregation_type,
      "tot" = "Total aggregations",
      "avg" = "Average/median aggregations",
      "pct" = "Proportion aggregations (%)")
    
    # Retrieve by what element the aggregation will be done
    agg_by <- input$aggregation_by
    
    # Retrieve station name
    station <- if(!input$all_stations) input$departure_station else "every station"
    
    # Retrieve date
    year <- input$year
    
    if(agg_by == "year") {
      params <- sprintf("%s per year for %s:", agg_type, station)
    } else if(agg_by == "station") {
      params <- sprintf("%s per station in %d:", agg_type, year)
    }
    params
  })
  
  output$trains_plot <- renderPlot({
    plot1 <- ggplot(TRAINS, aes()) + geom_blank()
    plot2 <- ggplot(TRAINS, aes()) + geom_blank()
    plot3 <- ggplot(TRAINS, aes()) + geom_blank()
    plot4 <- ggplot(TRAINS, aes()) + geom_blank()
    plot5 <- ggplot(TRAINS, aes()) + geom_blank()
    plot6 <- ggplot(TRAINS, aes()) + geom_blank()
    
    grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=3, ncol=2)
  })
  
  # EVENTS #
  observeEvent(react_allStations(), {
    # Deactivate departure_station if checkbox "all stations" is checked
    shinyjs::toggleState("departure_station", condition = !react_allStations())
  })
  
  observeEvent(react_aggregationBy(), {
    # Deactivate departure_station if checkbox "all stations" is checked
    if(react_aggregationBy() == "year") {
      shinyjs::toggleState("year", condition = FALSE)
      shinyjs::toggleState("departure_station", condition = !react_allStations())
      shinyjs::toggleState("all_stations", condition = TRUE)
      
    } else if(react_aggregationBy() == "station") {
      shinyjs::toggleState("year", condition = TRUE)
      shinyjs::toggleState("departure_station", condition = FALSE)
      shinyjs::toggleState("all_stations", condition = FALSE)
    }

  })
}