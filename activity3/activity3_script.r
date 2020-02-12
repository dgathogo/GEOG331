# install.packages(c("lubridate"))
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
 
 datW <- read.csv("q:\\Students\\dgathogo\\a03\\bewkes_weather.csv", na.strings = c("#N/A"), skip = 3, header = F)
 print(datW[1,])
 
 sensorInfo <- read.csv("q:\\Students\\dgathogo\\a03\\bewkes_weather.csv", na.strings = c("#N/A"), nrows = 2)
 
 print(sensorInfo)
 
 colnames(datW) <- colnames(sensorInfo)
 
 print(datW[1,])