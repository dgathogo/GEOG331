#install.packages(c("lubridate"))
library(lubridate)
 assert <- function(statement, err.message) {
   # evaluates if a statement is true or false 
   if (statement == F) {
     print(err.message)
   }
 }
 
 #evaluate a false statement
 assert(1==2, "error: unequal values")
 
 #evaluaate a true statement
 assert(2==2, "error: unequal values")
 #datW <- read.csv("/Volumes/class/Geog331/Students/dgathogo/a03/bewkes_weather.csv", na.strings = c("#N/A"), skip = 3, header = F)
 sensorInfo <- read.csv("/Volumes/class/Geog331/Students/dgathogo/a03/bewkes_weather.csv", na.strings = c("#N/A"), nrows = 2)
 
 #datW <- read.csv("q:\\Students\\dgathogo\\a03\\bewkes_weather.csv", na.strings = c("#N/A"), skip = 3, header = F)
 #print(datW[1,])
 
 sensorInfo <- read.csv("q:\\Students\\dgathogo\\a03\\bewkes_weather.csv", na.strings = c("#N/A"), nrows = 2)
 
 #print(sensorInfo)
 
 colnames(datW) <- colnames(sensorInfo)
 
 #print(datW[1,])
 
 dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
 
 datW$day <- yday(dates)
 
 datW$hour <- hour(dates) + (minute(dates)/60)
 
 datW$DD <- datW$day + (datW$hour/24)
 
# print(datW[1,])
 length(which(is.na(datW$air.temperature)))
 length(which(is.na(datW$soil.temp)))
 
 plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
      ylab="Soil moisture (cm3 water per cm3 soil)")
 
 plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
      ylab="Air temperature (degrees C)")
  
 
 #Question 4
 
 #I'm going to make a new column to work with that indicates that I am conducting QAQC
 #because overwriting values should be done cautiously and can lead to confusing issues.
 #It can be particularily confusing when you are just learning R.
 #Here I'm using the ifelse function
 #the first argument is a logical statement to be evaluated as true or false on a vector
 #the second argument is the value that my air.tempQ1 column will be given if the statement
 #is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
 #In this case it is just given the air temperature value
 datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)
 
 quantile(datW$air.tempQ1)
 
 #look at days with really low air temperature
 datW[datW$air.tempQ1 < 8,]  
 
 #look at days with really high air temperature
 datW[datW$air.tempQ1 > 33,]  
 
 
 
 #Question 5
 
 #plot precipitation and lightning strikes on the same plot
 #normalize lighting strikes to match precipitation
 lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
 
 
 #make the plot with precipitation and lightning activity marked
 #make it empty to start and add in features
 plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
      type="n")
 #plot precipitation points only when there is precipitation 
 #make the points semi-transparent
 points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
        col= rgb(95/255,158/255,160/255,.5), pch=15)        
 
 #plot lightning points only when there is lightning     
 points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
        col= "tomato3", pch=19)
 
 assert(length(lightscale) == length(datW$DD), "Unequal lengths")
 
 #Question 6
 
 #filter out storms in wind and air temperature measurements
 # filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
 #create a new air temp column
 datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                           ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
 
 
 datW$wind.speed2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                            ifelse(datW$precipitation > 5, NA, datW$wind.speed))
 
 for (i in 1 : length(datW$precipitation)) {
    assert(!(((datW$precipitation[i]  >= 2 & datW$lightning.acvitivy[i] >0) || datW$precipitation[i] > 5) & is.na(datW$wind.speed2[i])), "Unfiltered wind speed data")
 }
 
 plot(datW$DD, datW$wind.speed2, xlab = "Day of Year", ylab = "Wind speed", type = 'b')
 plot(datW$DD, datW$wind.speed, xlab = "Day of Year", ylab = "Wind speed", type = 'b')
 
 #Question 7
 
 plot(datW$DD, datW$precipitation, xlab = "Day of Year", ylab = "Precipitation")
 plot(datW$DD, datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture")
 plot(datW$DD, datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temperature")
 plot(datW$DD, datW$air.temperature, xlab = "Day of Year", ylab = "Air Temperature")
 
 
 #Question 8
 
 avgAirTemp <- mean(datW$air.temperature, na.rm = T)
 avgWindSpeed <- mean(datW$wind.speed, na.rm = T)
 avgSoilMoisture <- mean(datW$soil.moisture, na.rm = T)
 avgSoilTemp <- mean(datW$soil.temp, na.rm = T)
 totalPrecp <- sum(datW$precipitation)

 #Question 9 
 
 plot(datW$DD, datW$precipitation, xlab = "Day of Year", ylab = "Precipitation")
 plot(datW$DD, datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture")
 plot(datW$DD, datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temperature")
 plot(datW$DD, datW$air.temperature, xlab = "Day of Year", ylab = "Air Temperature")
 
 
 
 
 