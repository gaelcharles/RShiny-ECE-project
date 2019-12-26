# Read me: application documentation

### Team:
- Gaël CHARLES
- Corentin COSTE
- Ismail ZMERLI

This notebook is the documentation of the shiny app our team provided.  
This app is a dashboard that shows various information on trains and flights datasets.

### For the trains part
We used two datasets.  
**full_trains.csv**: Every line gives information on every trips between two French stations that happened for a given month and year.  
**regularite-mensuelle-tgv-aqst.csv**: Same CSV but with French column names and with delay information on those trips  
  
### For the flights part
We used three datasets.  
**airlines.csv**: Character code for a given airline  
**flights.csv**: Lots of information on many flights in North America  
**airports.csv**: Information on airports involved in those flights.  

The files are quite large and yet can't be loaded into shiny directly.  

Consequently, we wrote a script that creates lighter aggregated dataframes. These are the dataframes we load into shiny.  
  
One can run this script if the corresponding datasets are found inside their respective folders in a "data" folder, at the root of the project, like so:

- server.R
- ui.R
- **data**
    - **french-sncf-trains-regularities**
        - full_trains.csv
        - regularite-mensuelle-tgv-aqst.csv
    - **usa-flights-delays**
        - airlines.csv
        - airports.csv
        - flights.csv
        
## [!] IMPORTANT NOTE [!]
File 'data/usa-flight-delays/airports.csv' has been modified to complete unfound airport locations, you need to use this one. It can be found in the GitHub.

```{r, echo=TRUE}
# Run this cell to execute the script
source('create_aggregated_CSVs.R')
```

The app can now be used.
There are two tabs: one for controlling french trains aggregations, one for controlling USA flights aggregations.
In the trains dashboard, aggregations are grouped into three categories:
  
**Total aggregations**:  
These contain aggregations that plot the total number of something. We find here the number of trains carried out, canceled, late at departure, and late at arrival.
  
**Average aggregations**:  
These contain aggregations that plot the average number of something. We find here 6 plots: the average number of delayed trains at departure (or on arrival), and the average delay at departure (or on arrival) of all trains (or of late trains).
  
**Proportion aggregations**:  
These contain aggregations that plot some proportions (%). We find here the proportion of canceled trains, and the proportion of delay causes.

In the flights dashboard, one can select an airline company in order to plot on the USA map the different flights insured by this airline. A flight is a line between the airports of departure and arrival.  
One can also choose what type of comparison to plot in the "compare" selecter.  
For each comparison plot, we can plot choose to compare between airlines (all included) or airports (an additionnal slider filters how many airports we want to involve in the comparison).


Every aggregation just mentioned can be shown aggregated per year regarding a specific station (or all stations), or per station.
