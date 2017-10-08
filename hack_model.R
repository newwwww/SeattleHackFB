#library
library(data.table)
library(RJSONIO)
library(geosphere)

#Load data from ACE
station.info.raw <- fread("\\\\officefile\\public\\ACE\\Hackathon\\station.csv")
park.info.raw <- fread("\\\\officefile\\public\\ACE\\Hackathon\\Annual_Parking_Study_Data.csv")
weather.info.raw <- fread("\\\\officefile\\public\\ACE\\Hackathon\\Road_Weather_Information_Stations.csv")

station.info <- station.info.raw
park.info <- park.info.raw
weather.info <- weather.info.raw

#Feature Engineer - park.info.raw
park.info$Date_Time <- paste(park.info$`Study Date`,park.info$Study_Time,sep=" ")
park.info$DateTime <- substr(as.character(as.POSIXct(strptime(park.info$Date_Time,format="%m/%d/%Y %I:%M %p"))),1,19)
park.info.table <- data.table(park.info)
park.info.filter <- subset(park.info.table,year(park.info$DateTime) == 2016)
park.info.filter <- park.info.filter[,c("Elmntkey","DateTime","Parking_Spaces","Total_Vehicle_Count")]
park.info.filter$space_available <- ifelse(park.info.filter$Parking_Spaces - park.info.filter$Total_Vehicle_Count <0, 0, park.info.filter$Parking_Spaces - park.info.filter$Total_Vehicle_Count)

#Feature Engineer - pstation.info
station.info <- transform(station.info, ParkRate = pmax(WKD_RATE1,WKD_RATE2))
station.info <- station.info[,c("ELMNTKEY","SHAPE_LNG","SHAPE_LAT","ParkRate")]
station.info <- data.frame(station.info)

#weather.info.raw
weather.info$DateTime2 <- as.character(as.POSIXct(strptime(weather.info$DateTime,format="%m/%d/%Y %I:%M:%S %p")))
weather.info$DateTime2 <- paste0(substr(weather.info$DateTime2,1,14),"00",substr(weather.info$DateTime2,17,19))
weather.info$StationName <- ifelse(weather.info$StationLocation == "(47.571695, -122.370873)","HarborAveUpperNorthBridge2",weather.info$StationName)
weather.station.list <- data.frame(unique(weather.info[,c("StationName","StationLocation")]))
weather.station.list$Weather_Lat <- as.numeric(substr(weather.station.list$StationLocation,2,9))
weather.station.list$Weather_Long <- as.numeric(substr(weather.station.list$StationLocation,12,21))

#Calculate the nearest weather station to pay station
for (i in 1:nrow(weather.station.list)) {
  StationNumber <- weather.station.list[i,1]
  long_idx <- paste("long",i)
  lat_idx <- paste("lat",i)
  long <- weather.station.list[i,c("Weather_Long")]
  lat <- weather.station.list[i,c("Weather_Lat")]
  station.info[[long_idx]] <- long
  station.info[[lat_idx]] <- lat
  station.info[[StationNumber]] = distHaversine(station.info[,2:3], station.info[,(i+4):(i+5)])
  station.info <- station.info[-c((i+4),(i+5))]
  print(i)
  print(StationNumber)
}
station.info

  
station.info$weatherstation <- apply(X=station.info[,5:15], MARGIN=1, FUN=min)
station.info$stationname <- ifelse(station.info$weatherstation == station.info$`35thAveSW_SWMyrtleSt`,"35thAveSW_SWMyrtleSt",
                            ifelse(station.info$weatherstation == station.info$AlaskanWayViaduct_KingSt,"AlaskanWayViaduct_KingSt",
                            ifelse(station.info$weatherstation == station.info$AlbroPlaceAirportWay,"AlbroPlaceAirportWay",                                          
                            ifelse(station.info$weatherstation == station.info$AuroraBridge,"AuroraBridge",
                            ifelse(station.info$weatherstation == station.info$HarborAveUpperNorthBridge,"HarborAveUpperNorthBridge",
                            ifelse(station.info$weatherstation == station.info$MagnoliaBridge,"MagnoliaBridge",
                            ifelse(station.info$weatherstation == station.info$NE45StViaduct,"NE45StViaduct",
                            ifelse(station.info$weatherstation == station.info$RooseveltWay_NE80thSt,"RooseveltWay_NE80thSt",
                            ifelse(station.info$weatherstation == station.info$SpokaneSwingBridge,"SpokaneSwingBridge",
                            ifelse(station.info$weatherstation == station.info$JoseRizalBridgeNorth,"JoseRizalBridgeNorth","HarborAveUpperNorthBridge2"))))))))))
station.info2 <- station.info[,c(1:4,17)]

#Join all tables together
park.station <- data.table(merge(park.info.filter,station.info2,by.x="Elmntkey", by.y="ELMNTKEY"))
weather.station.table <-data.table(weather.info)
weather.station.nodup <- weather.station.table[,list(RoadSurfaceTemperature= max(RoadSurfaceTemperature),AirTemperature= max(AirTemperature)),by=list(StationName,DateTime2)]
park.station.weather <- merge(park.station,weather.station.nodup, by.x=c("stationname","DateTime"), by.y=c("StationName","DateTime2"))
park.station.weather[complete.cases(park.station.weather), ]
park.station.weather$Time <- as.numeric(substr(park.station.weather$DateTime,12,13))
park.station.weather$FinalDate <- weekdays(as.Date(park.station.weather$DateTime))
colnames(park.station.weather) <- c("WeatherStationName","DateTime","ParkingStationId","TotalParkingSpaces","VehicleParked","AvailableSpaces","Long","Lat","HourlyRate","RoadSurfaceTemperature","AirTemperature","Time","Weekday")

fwrite(park.station.weather,"E:/CleanData.csv")

#Model Preparation
ds.model <- park.station.weather[,c(1)]



#geo
geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

geocodeAdddress("Microsoft Building 36")
