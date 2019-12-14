function(input, output) {

  # REACTIVE ELEMENTS #
  react <- reactive({
    input$all_stations # Checkbox "aggregate on all station" deactivating the dropdown
  })
  
  # OUTPUT ELEMENTS #
  
  output$trains_plotParams <- renderText({
    # Retrieve aggregation type
    agg_type <- switch(input$aggregation_type,
      "tot" = "Total aggregations",
      "avg" = "Average/median aggregations",
      "pct" = "Proportion aggregations (%)")
    
    # Retrieve station name
    station <- if(!input$all_stations) input$departure_station else "every station"
    
    # Retrieve date range
    if(input$year_range[1] != input$year_range[2]) {
      years <- sprintf("%d to %d", input$year_range[1], input$year_range[2])
    } else {
      years <- sprintf("%d", input$year_range[1])
    }
    
    sprintf("%s for %s, %s:", agg_type, station, years)
  })
  
  # EVENTS #
  observeEvent(react(), {
    shinyjs::toggleState("departure_station", condition = !react())
  })
}