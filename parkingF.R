setwd("E:\\Projects\\Hackathon\\2017fb")
library(data.table)
library(plyr)
library(geosphere)
parking_data <- fread("Annual_Parking_Study_Data.csv")
pay_station <- fread("SDOT_PayStations.csv")
clean_data <- fread("CleanData.csv")



colnames(pay_station)[4] <- "ParkingStationId"

pay_station2 <- pay_station
pay_station <- pay_station2[,list(UNITDESC = max(UNITDESC)), by ="ParkingStationId"]



GetLong <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- x$results[[1]]$geometry$location$lng
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

GetLat <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- x$results[[1]]$geometry$location$lat
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

#############################test#########################################################

date <-"2017-10-07"
time <- "12"
Address <- "Facebook Seattle"
Date <- weekdays(as.Date(date))
Long <- GetLong(Address)
Lat <- GetLat(Address)
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
dtf <-  join (x=predict_table, y= pay_station, by ="ParkingStationId")
output_table<- dtf[,c("UNITDESC","HourlyRate", "location_distance","predict","ParkingStationId", "Long", "Lat")]


names(output_table)<- c("Address", "Price","Location_distance", "prediction","ParkingStationId","Long", "Lat")
