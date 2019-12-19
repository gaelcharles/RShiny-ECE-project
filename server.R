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
    
    # Aggregation with sums, per year
    if(input$aggregation_type == "tot" & input$aggregation_by == "year") {
      
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
}