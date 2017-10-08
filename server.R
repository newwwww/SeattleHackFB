




shinyServer(function(input, output) {

  
  v <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$run, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <- input$run
  })
  
  observeEvent(input$info, {
    v$doPlot <- FALSE
  })  
  
    output$table <- renderDataTable({
      if (v$doPlot == FALSE) return()
      isolate({
        Date <- weekdays(as.Date(input$date))
        Long <- GetLong(input$Destination)
        Lat <- GetLat(input$Destination)
        time <- as.numeric(substr(input$time2,12,13))
        Temp <- 60
        
        Input_dataframe <- data.frame(TotalParkingSpaces=integer(3),Long=numeric(3)   , Lat = numeric(3),HourlyRate = numeric(3),AirTemperature=numeric(3),Time=character(3),Weekday=character(3))
        
        
        station_location <- unique(clean_data[,c("ParkingStationId","Long","Lat", "TotalParkingSpaces", "HourlyRate")])
        station_location$Long_test<-Long
        station_location$Lat_test<-Lat
        station_location$location_distance = distHaversine(station_location[,2:3], station_location[,6:7])
        station_location<-station_location[order(location_distance),]
        Near_station <-station_location[1:3,]
        
        Input_dataframe$TotalParkingSpaces <-Near_station$TotalParkingSpaces
        Input_dataframe$Long <-Near_station$Long
        Input_dataframe$Lat <-Near_station$Lat
        Input_dataframe$HourlyRate <-Near_station$HourlyRate
        Input_dataframe$AirTemperature <-Temp
        Input_dataframe$Time <-time
        Input_dataframe$Weekday <-Date
        
        Input_dataframe[4,]<-Input_dataframe[1,]
        Input_dataframe[4,6]<-0
        Input_dataframe[4,7]<-"NA"
        
        Input_dataframe$Weekday <- as.factor(Input_dataframe$Weekday)
        Input_dataframe$Time <- as.factor(Input_dataframe$Time)
        
        Input_dataframe$AvailableSpaces<-0
        sparse_input <- sparse.model.matrix(AvailableSpaces~.-1, data = Input_dataframe)
        
        predict<-predict(bst, sparse_input)[1:3]
        predict_table <- cbind(Near_station,predict)
        dtf <-  join (x=predict_table, y= pay_station, by =c("ParkingStationId"))
        output_table<- dtf[,c("UNITDESC","HourlyRate", "location_distance","predict","ParkingStationId", "Long", "Lat")]
        
        
        names(output_table)<- c("Address", "Price","Location_distance", "prediction","ParkingStationId","Long", "Lat")
        output_table[order(-prediction),]
        })
      
    } )
  
  # output$table <- renderDataTable({
  #   if (v$doPlot == FALSE) return()
  #   
  #   isolate({
  #     outtable <- output_table[Destination==input$Destination,]
  #     sorttable<-outtable[order(-prediction),c("Destination","Address", "Long", "Lat","Price","Location_distance", "prediction")]
  #   })
  # })
  # 
  # observeEvent(input$run, {
  #   coordinate <- toString(geocodeAdddress(input$Destination))
  #   
  #   output$coordinate <- renderPrint({
  # 
  #     paste("location",coordinate)
  #   } )
  #   
  #   output$table <- renderDataTable({
  # 
  #     output_table[Destination==input$Destination,]
  #   })
  # 
  # })
  # 

  

  
}
)