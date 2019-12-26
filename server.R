function(session, input, output) {

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
    
    # Aggregation with sums, per year
    if(input$aggregation_type == "tot" & input$aggregation_by == "year") {
      
      updateSliderInput(session, inputId = "dynamic_height", value = 800)
      updateSliderInput(session, inputId = "dynamic_width", value = 800)
      
      aggDF <- if(!input$all_stations)
        read.csv("data/french-sncf-trains-regularities/agg_byYearStation.csv") %>% filter(station == input$departure_station)
      else read.csv("data/french-sncf-trains-regularities/agg_byYear.csv")
      
      plot1 <- ggplot(aggDF, aes(x=year, y=num_carried_out, fill=as.factor(year))) + 
        geom_col() + 
        scale_fill_brewer(palette="Blues") +
        geom_label(aes(label=num_carried_out), vjust=0) + 
        labs(title="Carried out trains per year") + 
        guides(fill="none") + 
        theme_minimal() +
        theme(axis.title.y=element_blank())
      
      plot2 <- ggplot(aggDF, aes(x=year, y=num_of_canceled_trains, fill=as.factor(year))) + 
        geom_col() + 
        scale_fill_brewer(palette="Purples") +
        geom_label(aes(label=num_of_canceled_trains), vjust=0) + 
        labs(title="Canceled trains per year") + 
        guides(fill="none") + 
        theme_minimal() +
        theme(axis.title.y=element_blank())
      
      plot3 <- ggplot(aggDF, aes(x=year, y=num_late_at_departure, fill=as.factor(year))) + 
        geom_col() + 
        scale_fill_brewer(palette="Oranges") +
        geom_label(aes(label=num_late_at_departure), vjust=0) + 
        labs(title="Late trains at departure per year") + 
        guides(fill="none") + 
        theme_minimal() +
        theme(axis.title.y=element_blank())
      
      plot4 <- ggplot(aggDF, aes(x=year, y=num_arriving_late, fill=as.factor(year))) + 
        geom_col() + 
        scale_fill_brewer(palette="Greens") +
        geom_label(aes(label=num_arriving_late), vjust=0) + 
        labs(title="Late trains at arrival per year") + 
        guides(fill="none") + 
        theme_minimal() +
        theme(axis.title.y=element_blank())
      
      grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)
      
    }
    # Aggregation with sums, per station
    else if(input$aggregation_type == "tot" & input$aggregation_by == "station") {
      
      updateSliderInput(session, inputId = "dynamic_height", value = 2000)
      updateSliderInput(session, inputId = "dynamic_width", value = 1000)
      
      aggDF <- read.csv("data/french-sncf-trains-regularities/agg_byYearStation.csv") %>% filter(year == input$year)
      
      plot1 <- ggplot(aggDF, aes(x=station, y=num_carried_out, fill=as.factor(station))) + 
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
        labs(title="Carried out trains per station") + coord_flip()
      
      plot2 <- ggplot(aggDF, aes(x=station, y=num_of_canceled_trains, fill=as.factor(station))) + 
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
        labs(title="Canceled trains per station") + coord_flip()
      
      plot3 <- ggplot(aggDF, aes(x=station, y=num_late_at_departure, fill=as.factor(station))) + 
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
        labs(title="Late trains at departure per station") + coord_flip()
      
      plot4 <- ggplot(aggDF, aes(x=station, y=num_arriving_late, fill=as.factor(station))) + 
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
        labs(title="Late trains on arrival per station") + coord_flip()
      
      grid.arrange(plot1, plot2, plot3, plot4, nrow=4, ncol=1)
    } 
    # Aggregation with means, per year
    else if(input$aggregation_type == "avg" & input$aggregation_by == "year") {
      
      updateSliderInput(session, inputId = "dynamic_height", value = 800)
      updateSliderInput(session, inputId = "dynamic_width", value = 820)
      
      
      aggDF <- if(!input$all_stations)
        read.csv("data/french-sncf-trains-regularities/agg_byYearStation.csv") %>% filter(station == input$departure_station)
      else read.csv("data/french-sncf-trains-regularities/agg_byYear.csv")
      
      plot1 <- ggplot(aggDF, aes(x=year, y=avg_late_at_departure, fill=as.factor(year))) +
        geom_col() +
        scale_fill_brewer(palette="Blues") +
        geom_label(aes(label=round(avg_late_at_departure, 1)), vjust=0) + 
        labs(title="Average number of delayed trains at departure per year") + 
        guides(fill="none") + 
        theme_minimal() +
        theme(axis.title.y=element_blank())
      
      plot2 <- ggplot(aggDF, aes(x=year, y=avg_arriving_late, fill=as.factor(year))) +
        geom_col() +
        scale_fill_brewer(palette="Purples") +
        geom_label(aes(label=round(avg_arriving_late, 1)), vjust=0) + 
        labs(title="Average number of delayed trains on arrival per year") + 
        guides(fill="none") + 
        theme_minimal() +
        theme(axis.title.y=element_blank())
      
      plot3 <- ggplot(aggDF, aes(x=year, y=avg_delay_all_departing, fill=as.factor(year))) +
        geom_col() +
        scale_fill_brewer(palette="Oranges") +
        geom_label(aes(label=round(avg_delay_all_departing, 1)), vjust=0) + 
        labs(title="Average delay at departure of all trains per year (min)") + 
        guides(fill="none") + 
        theme_minimal() +
        theme(axis.title.y=element_blank())
      
      plot4 <- ggplot(aggDF, aes(x=year, y=avg_delay_all_arriving, fill=as.factor(year))) +
        geom_col() +
        scale_fill_brewer(palette="Greens") +
        geom_label(aes(label=round(avg_delay_all_arriving, 1)), vjust=0) + 
        labs(title="Average delay on arrival of all trains per year (min)") + 
        guides(fill="none") + 
        theme_minimal() +
        theme(axis.title.y=element_blank())
      
      plot5 <- ggplot(aggDF, aes(x=year, y=avg_delay_late_at_departure, fill=as.factor(year))) +
        geom_col() +
        scale_fill_brewer(palette="Pastel1") +
        geom_label(aes(label=round(avg_delay_late_at_departure, 1)), vjust=0) + 
        labs(title="Average delay at departure of late trains per year (min)") + 
        guides(fill="none") + 
        theme_minimal() +
        theme(axis.title.y=element_blank())
      
      plot6 <- ggplot(aggDF, aes(x=year, y=avg_delay_late_on_arrival, fill=as.factor(year))) +
        geom_col() +
        scale_fill_brewer(palette="Pastel2") +
        geom_label(aes(label=round(avg_delay_late_on_arrival, 1)), vjust=0) + 
        labs(title="Average delay on arrival of late trains per year (min)") + 
        guides(fill="none") + 
        theme_minimal() +
        theme(axis.title.y=element_blank())
      
      grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=3, ncol=2)
      
    } 
    # Aggregations with mean, per station
    else if(input$aggregation_type == "avg" & input$aggregation_by == "station") {
      
      updateSliderInput(session, inputId = "dynamic_height", value = 2000)
      updateSliderInput(session, inputId = "dynamic_width", value = 800)
      
      
      aggDF <- read.csv("data/french-sncf-trains-regularities/agg_byYearStation.csv") %>% filter(year == input$year)
      
      plot1 <- ggplot(aggDF, aes(x=station, y=avg_late_at_departure, fill=as.factor(station))) +
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.text.x=element_text(size = 10, angle = -90, hjust = 0),
              axis.title.x=element_blank(), axis.title.y=element_blank()) +
        labs(title="Average number of delayed train at departure per station")
      
      plot2 <- ggplot(aggDF, aes(x=station, y=avg_arriving_late, fill=as.factor(station))) +
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.text.x=element_text(size = 10, angle = -90, hjust = 0),
              axis.title.x=element_blank(), axis.title.y=element_blank()) +
        labs(title="Average number of delayed train on arrival per station")
      
      plot3 <- ggplot(aggDF, aes(x=station, y=avg_delay_all_departing, fill=as.factor(station))) +
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.text.x=element_text(size = 10, angle = -90, hjust = 0),
              axis.title.x=element_blank(), axis.title.y=element_blank()) +
        labs(title="Average delay at departure of all trains per station (min)")
      
      plot4 <- ggplot(aggDF, aes(x=station, y=avg_delay_all_arriving, fill=as.factor(station))) +
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.text.x=element_text(size = 10, angle = -90, hjust = 0),
              axis.title.x=element_blank(), axis.title.y=element_blank()) +
        labs(title="Average delay on arrival of all trains per station (min)")
      
      plot5 <- ggplot(aggDF, aes(x=station, y=avg_delay_late_at_departure, fill=as.factor(station))) +
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.text.x=element_text(size = 10, angle = -90, hjust = 0),
              axis.title.x=element_blank(), axis.title.y=element_blank()) +
        labs(title="Average delay at departure of late trains per station (min)")
      
      plot6 <- ggplot(aggDF, aes(x=station, y=avg_delay_late_on_arrival, fill=as.factor(station))) +
        geom_col() + 
        guides(fill="none") + 
        theme_minimal() + 
        theme(axis.text.x=element_text(size = 10, angle = -90, hjust = 0),
              axis.title.x=element_blank(), axis.title.y=element_blank()) +
        labs(title="Average delay on arrival of late trains per station (min)")
      
      grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=6, ncol=1)
      
    }
    # Aggregations with percent, per year
    else if(input$aggregation_type == "pct" & input$aggregation_by == "year") {
      
      updateSliderInput(session, inputId = "dynamic_height", value = 800)
      updateSliderInput(session, inputId = "dynamic_width", value = 800)
      
      aggDF_melted <- if(!input$all_stations)
        read.csv("data/french-sncf-trains-regularities/agg_melted_byYearStation.csv") %>% filter(station == input$departure_station)
      else read.csv("data/french-sncf-trains-regularities/agg_melted_byYear.csv")
      
      delays_melted <- if(!input$all_stations)
        read.csv("data/french-sncf-trains-regularities/delays_melted_byYearStation.csv", check.names=FALSE, encoding="UTF-8") %>%
        filter(station == input$departure_station)
      else read.csv("data/french-sncf-trains-regularities/delays_melted_byYear.csv", check.names=FALSE, encoding="UTF-8")
      
      # aggDF <- aggDF %>% mutate(pct_canceled_trains = 100*(num_of_canceled_trains/total_num_trips)) %>%
      #   mutate(pct_carried_out_trains = 100*(num_carried_out/total_num_trips))
      
      plot1 <- ggplot(aggDF_melted,
                      aes(x=year, y=proportion, fill=train_state)) +
        geom_col(position="fill") +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=45, vjust=2.6, hjust=1.5), axis.title.x=element_blank()) + 
        guides(fill=guide_legend(title="Train state")) + 
        labs(y="Proportion of canceled trains per year (%)")
      
      plot2 <- ggplot(delays_melted,
                      aes(x=year, y=proportion, fill=delay_cause)) +
        geom_col(position="fill") + 
        theme_minimal() + 
        theme(axis.text.x=element_text(angle=45, vjust=2.6, hjust=1.5), axis.title.x=element_blank()) + 
        guides(fill=guide_legend(title="Delay causes")) + 
        labs(y="Proportion of delay causes per year (%)")
      
      grid.arrange(plot1, plot2, nrow=2, ncol=1)
    }
    # Aggregations with percent, per station
    else if(input$aggregation_type == "pct" & input$aggregation_by == "station") {
      
      updateSliderInput(session, inputId = "dynamic_height", value = 1100)
      updateSliderInput(session, inputId = "dynamic_width", value = 800)
      
      aggDF_melted <- read.csv("data/french-sncf-trains-regularities/agg_melted_byYearStation.csv", check.names=FALSE) %>%
        filter(year == input$year)
      
      delays_melted <- read.csv("data/french-sncf-trains-regularities/delays_melted_byYearStation.csv", check.names=FALSE, encoding="UTF-8")
      
      # aggregate(delays_melted %>% select(proportion), 
      #           by=list(station=delays_melted$station, delay_cause=delays_melted$delay_cause), 
      #           FUN=mean)
      
      plot1 <- ggplot(aggDF_melted,
                      aes(x=station, y=proportion, fill=train_state)) +
        geom_col(position="fill") +
        theme_minimal() + 
        theme(axis.text.x=element_text(angle=-90, hjust=0), axis.title.x=element_blank(), legend.position="top") +
        guides(fill=guide_legend(title="Train state")) +
        labs(y="Proportion of canceled trains per station (%)")
      
      
      plot2 <- ggplot(delays_melted,
                      aes(x=station, y=proportion, fill=delay_cause)) +
        geom_col(position="fill") + 
        theme_minimal() + 
        theme(axis.text.x=element_text(angle=-90, hjust=0), axis.title.x=element_blank(), legend.position="top") +
        guides(fill=guide_legend(title="Delay causes")) +
        labs(y="Proportion of delay causes per station (%)")
      
      grid.arrange(plot1, plot2, nrow=2, ncol=1)
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
  
  
  ### FLIGHTS PART ###
  
  Selected_Airline <- reactive({match(input$Selected_Airline, bd_airlines$AIRLINE)})
  Airlines_Or_Airports_React <- reactive({input$Airlines_Or_Airports})
  
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(providers$CartoDB.Positron)  %>% 
    addCircleMarkers(lng=bd_airports$LONGITUDE,
                     lat=bd_airports$LATITUDE,
                     radius = bd_airports$Flight_Count/10000+3,
                     color = "cornflowerblue",
                     stroke = FALSE, 
                     fillOpacity = 0.7,
                     popup=paste("<center>",
                                 bd_airports$IATA_CODE, "<br>",
                                 bd_airports$AIRPORT, "<br>",
                                 bd_airports$CITY, "<br>",
                                 bd_airports$STATE, bd_airports$COUNTRY, "<br>",
                                 "<br>",
                                 "Number of flights : ",  round(bd_airports$Flight_Count, digits=2), "<br>",
                                 "Number of delayed flights : ", round(bd_airports$Delayed_Count, digits=2), "<br>",
                                 "Number of diverted flights : ", round(bd_airports$Diverted_Count, digits=2), "<br>",
                                 "Number of canceled flights : ", round(bd_airports$Canceled_Count, digits=2), "<br>",
                                 "<br>",
                                 "Average flight duration : ", round(bd_airports$Average_Flight_Time, digits=2), "min <br>",
                                 "Average flight distance : ", round(bd_airports$Average_Distance, digits=2), "<br>",
                                 "The total distance covered : ", round(bd_airports$Total_Distance, digits=2), "<br>",
                                 "<br>",
                                 "Average departure delay : ", round(bd_airports$Average_Departure_Delay, digits=2), "min <br>",
                                 "Average Taxi out : ", round(bd_airports$Average_Taxi_Out, digits=2), "min <br>",
                                 "<br>",
                                 "Average air system delay : ", round(bd_airports$Average_Air_System_Delay, digits=2), "min <br>",
                                 "Average security delay : ", round(bd_airports$Average_Security_Delay, digits=2), "min <br>",
                                 "Average airline delay : ", round(bd_airports$Average_Airline_Delay, digits=2), "min <br>",
                                 "Average late aircraft delay : ", round(bd_airports$Average_Late_Aircraft_Delay, digits=2), "min <br>",
                                 "Average weather delay : ", round(bd_airports$Average_Weather_Delay, digits=2), "min <br>",
                                 "<br>",
                                 "Average arrival delay (as a depature airport): ", round(bd_airports$Average_Arrival_Delay, digits=2), "min <br>",
                                 "<center>"))
  })
  
  observe({
    print(input$Selected_Airline)
    proxy <- leafletProxy("mymap")
    id_airline <- match(input$Selected_Airline, bd_airlines$AIRLINE)
    
    proxy %>%
    clearShapes() %>%
    addPolylines(data = airlines_edges[[id_airline]],
                 color = "red",
                 fillOpacity = 0.3,
                 weight = network[[id_airline]]$edges$s.COUNT/10000+0.3)
    
    print(input$Airlines_Or_Airports)
    print(input$Type_of_agg)
    output$plot_flights <- renderPlot({
      if(input$Airlines_Or_Airports == "Airlines") {
        switch (input$Type_of_agg,
                "Number of Flights" = barplot(bd_airlines$Flight_Count, names.arg = bd_airlines$IATA_CODE, las=2),
                "Number of delayed flights" = barplot(bd_airlines$Delayed_Count, names.arg = bd_airlines$IATA_CODE, las=2),
                "Number of diverted flights" = barplot(bd_airlines$Diverted_Count, names.arg = bd_airlines$IATA_CODE, las=2),
                "Number of canceled flights" = barplot(bd_airlines$Canceled_Count, names.arg = bd_airlines$IATA_CODE, las=2),
                "Average flight duration" = barplot(bd_airlines$Average_Flight_Time, names.arg = bd_airlines$IATA_CODE, las=2),
                "Average flight distance" = barplot(bd_airlines$Average_Distance, names.arg = bd_airlines$IATA_CODE, las=2),
                "The total distance covered" = barplot(bd_airlines$Total_Distance, names.arg = bd_airlines$IATA_CODE, las=2),
                "Average departure delay" = barplot(bd_airlines$Average_Departure_Delay, names.arg = bd_airlines$IATA_CODE, las=2),
                "Average arrival delay" = barplot(bd_airlines$Average_Arrival_Delay, names.arg = bd_airlines$IATA_CODE, las=2)
        )
      } else {
        switch (input$Type_of_agg,
                "Number of Flights" = barplot(bd_airports[order(bd_airports$Flight_Count, decreasing=input$Decreasing),]$Flight_Count[1:input$Nb_Airlines_Plot],
                                              names.arg = bd_airports[order(bd_airports$Flight_Count, decreasing=input$Decreasing),]$IATA_CODE[1:input$Nb_Airlines_Plot], las=2),
                "Number of delayed flights" = barplot(bd_airports[order(bd_airports$Delayed_Count, decreasing=input$Decreasing),]$Delayed_Count[1:input$Nb_Airlines_Plot],
                                                      names.arg = bd_airports[order(bd_airports$Delayed_Count, decreasing=input$Decreasing),]$IATA_CODE[1:input$Nb_Airlines_Plot], las=2),
                "Number of diverted flights" = barplot(bd_airports[order(bd_airports$Diverted_Count, decreasing=input$Decreasing),]$Diverted_Count[1:input$Nb_Airlines_Plot],
                                                      names.arg = bd_airports[order(bd_airports$Diverted_Count, decreasing=input$Decreasing),]$IATA_CODE[1:input$Nb_Airlines_Plot], las=2),
                "Number of canceled flights" = barplot(bd_airports[order(bd_airports$Canceled_Count, decreasing=input$Decreasing),]$Canceled_Count[1:input$Nb_Airlines_Plot],
                                                      names.arg = bd_airports[order(bd_airports$Canceled_Count, decreasing=input$Decreasing),]$IATA_CODE[1:input$Nb_Airlines_Plot], las=2),
                "Average flight duration" = barplot(bd_airports[order(bd_airports$Average_Flight_Time, decreasing=input$Decreasing),]$Average_Flight_Time[1:input$Nb_Airlines_Plot],
                                                    names.arg = bd_airports[order(bd_airports$Average_Flight_Time, decreasing=input$Decreasing),]$IATA_CODE[1:input$Nb_Airlines_Plot], las=2),
                "Average flight distance" = barplot(bd_airports[order(bd_airports$Average_Distance, decreasing=input$Decreasing),]$Average_Distance[1:input$Nb_Airlines_Plot],
                                                    names.arg = bd_airports[order(bd_airports$Average_Distance, decreasing=input$Decreasing),]$IATA_CODE[1:input$Nb_Airlines_Plot], las=2),
                "The total distance covered" = barplot(bd_airports[order(bd_airports$Total_Distance, decreasing=input$Decreasing),]$Total_Distance[1:input$Nb_Airlines_Plot],
                                                       names.arg = bd_airports[order(bd_airports$Total_Distance, decreasing=input$Decreasing),]$IATA_CODE[1:input$Nb_Airlines_Plot], las=2),
                "Average departure delay" = barplot(bd_airports[order(bd_airports$Average_Departure_Delay, decreasing=input$Decreasing),]$Average_Departure_Delay[1:input$Nb_Airlines_Plot],
                                                    names.arg = bd_airports[order(bd_airports$Average_Departure_Delay, decreasing=input$Decreasing),]$IATA_CODE[1:input$Nb_Airlines_Plot], las=2),
                "Average arrival delay" = barplot(bd_airports[order(bd_airports$Average_Arrival_Delay, decreasing=input$Decreasing),]$Average_Arrival_Delay[1:input$Nb_Airlines_Plot],
                                                  names.arg = bd_airports[order(bd_airports$Average_Arrival_Delay, decreasing=input$Decreasing),]$IATA_CODE[1:input$Nb_Airlines_Plot], las=2)
        )
      }
      
      })
  })
  
  observeEvent(Airlines_Or_Airports_React(), {
    # Deactivate departure_station if checkbox "all stations" is checked
    shinyjs::toggleState("Nb_Airlines_Plot", condition = (!Airlines_Or_Airports_React() == "Airlines"))
    shinyjs::toggleState("Decreasing", condition = (!Airlines_Or_Airports_React() == "Airlines"))
  })
}