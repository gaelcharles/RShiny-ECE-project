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
    
    params <- ""
    
    if(agg_by == "year") {
      params <- sprintf("%s per year for %s:", agg_type, station)
    } else if(agg_by == "station") {
      params <- sprintf("%s per station in %d:", agg_type, year)
    }
    
    # Render text
    params
  })
  
  output$trains_plot <- renderPlot({
    
    plot1 <- ggplot(TRAINS, aes())
    plot2 <- ggplot(TRAINS, aes())
    plot3 <- ggplot(TRAINS, aes())
    plot4 <- ggplot(TRAINS, aes())
    plot5 <- ggplot(TRAINS, aes())
    plot6 <- ggplot(TRAINS, aes())
    
    # Aggregation with sums
    if(input$aggregation_type == "tot" & input$aggregation_by == "year") {
      
      if(!input$all_stations) {
        print("reading temp_sumAgg_ByYearStation.csv")
        aggDF <- read.csv("data/temp_sumAgg_ByYearStation.csv") %>% filter(station == input$departure_station)
        #x <- aggDF$year
      } else {
        print("reading temp_sumAgg_ByYear.csv")
        aggDF <- read.csv("data/temp_sumAgg_ByYear.csv")
      }
      
      plot1 <- ggplot(aggDF, aes(x=year, y=carried_out, fill=as.factor(year))) + 
        geom_col() + 
        scale_fill_brewer(palette="Blues") +
        geom_text(aes(label=carried_out), vjust=0) + 
        labs(title="Carried out trains (total)") + 
        guides(fill="none") + 
        theme_minimal()
      
      plot2 <- ggplot(aggDF, aes(x=year, y=num_of_canceled_trains, fill=as.factor(year))) + 
        geom_col() + 
        scale_fill_brewer(palette="Purples") +
        geom_text(aes(label=num_of_canceled_trains), vjust=0) + 
        labs(title="Canceled trains (total)") + 
        guides(fill="none") + 
        theme_minimal()
      
      plot3 <- ggplot(aggDF, aes(x=year, y=num_late_at_departure, fill=as.factor(year))) + 
        geom_col() + 
        scale_fill_brewer(palette="Oranges") +
        geom_text(aes(label=num_late_at_departure), vjust=0) + 
        labs(title="Late trains at departure (total)") + 
        guides(fill="none") + 
        theme_minimal()
      
      plot4 <- ggplot(aggDF, aes(x=year, y=num_arriving_late, fill=as.factor(year))) + 
        geom_col() + 
        scale_fill_brewer(palette="Greens") +
        geom_text(aes(label=num_arriving_late), vjust=0) + 
        labs(title="Late trains at arrival (total)") + 
        guides(fill="none") + 
        theme_minimal()
      
      grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)
    
    } else if(input$aggregation_type == "tot" & input$aggregation_by == "station") {
      
      aggDF <- read.csv("data/temp_sumAgg_ByYearStation.csv") %>% filter(year == input$year)
      
      plot1 <- ggplot(aggDF, aes(x=station, y=carried_out, fill=as.factor(station))) + 
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.text.x=element_text(size = 7, angle = -90, hjust = 0), axis.title.x=element_blank())
      
      grid.arrange(plot1, plot2, plot3, plot4, nrow=4, ncol=1)
    }

    
  })
  
  output$trains_plotUI <- renderUI({
    fluidRow(
      column(10,
        plotOutput("trains_plot", width=input$dynamic_width, height=input$dynamic_height)
      )
    )
  })
  
  # EVENTS #
  observeEvent(react_allStations(), {
    # Deactivate departure_station if checkbox "all stations" is checked
    shinyjs::toggleState("departure_station", condition = !react_allStations())
  })
  
  observeEvent(react_aggregationBy(), {
    # Deactivate departure station or year slider depending on what we aggregate
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