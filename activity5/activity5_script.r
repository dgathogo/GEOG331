library(lubridate)
library(dplyr)
library(ggplot2)
#read in streamflow data
#datH <- read.csv("q:\\Students\\dgathogo\\a05\\stream_flow_data.csv", na.strings = c("Eqp"))
#read in precipitation data
#hourly precipitation is in mm
#datP <- read.csv("q:\\Students\\dgathogo\\a05\\2049867.csv")  

datH <- read.csv("/Volumes/class/Geog331/Students/dgathogo/a05/stream_flow_data.csv", na.strings = c("Eqp"))
datP <- read.csv("/Volumes/class/Geog331/Students/dgathogo/a05/2049867.csv")  

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)

datD$month <- month(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)
month(dateP)


# Question 2
#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay -1)/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365)) 


#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
# Question 3
head(datH)
head(datP)

#Question 4

help("expression")

# Question 5

aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")
#start new plot
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",
     xaxt = "n",
     axes = F)
# get data for 2017 only
dat2017 <- datD[datD$year == "2017",]
# plot on the same axes
par(new=T)

plot(dat2017$doy, dat2017$discharge, 
     type="l",
     lwd=2,
     xlab = "",
     ylab = "",
     col = "orange",
     ylim=c(0,170), #adjust the limit plotted
     xaxs="i", yaxs ="i",
     xaxt="n",
     axes = F)

#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
doy_to_month <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)

#month_labs <- c("", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_labs <- c("","J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

axis(1, at=doy_to_month, lab=month_labs)

axis(2, seq(0,160, by=20),
     seq(0,160, by=20),
     las = 2)#show ticks at 90 degree angle

legend("topright", c("2017","mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col = c("orange", "black",rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       pch = c(NA,15), #symbols
       bty="n")#no legend border

# Question 7

datP.aggregated <- aggregate(datP, list(datP$year, datP$doy), length)

fullDay <- datP.aggregated[datP.aggregated$doy==24,]

plot(datD$decYear, datD$discharge, 
     type="l",
     lwd=2,
     xlab = "Year",
     ylab = expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     col = "black",
     ylim=c(0,400),
     xaxs="i", yaxs ="i",
     )
par(new=T)

plot(x=datP.aggregated$decYear,
     y= rep(300, length(fullDay$decYear)),
     xlab="",
     ylab="",
     type = "p",
     col = "blue",
     axes = F,
     pch=c(NA,17)
)

# Question 8
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

hydroD2 <- datD[datD$doy >= 7 & datD$doy < 9 & datD$year == 2009,]
hydroP2 <- datP[datP$doy >= 7 & datP$doy < 9 & datP$year == 2009,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl2 <- floor(min(hydroD2$discharge))-1
#celing rounds up to the integer
yh2 <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl2 <- 0
pm2 <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP2$pscale <- (((yh2-yl2)/(pm2-pl2)) * hydroP2$HPCP) + yl2

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl2,yh2), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
        polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
                  hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
                c(yl,hydroP2$pscale[i],hydroP2$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

# Question 9


#specify year as a factor
datD$yearPlot <- as.factor(datD$year)

# use 2012 because it is a leap year
getSeason <- function(DATES) {
        WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
        SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
        SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
        FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
        
        # Convert dates from any year to 2012 dates
        d <- as.Date(strftime(DATES, format="2012-%m-%d"))
        
        ifelse (d >= WS | d < SE, "Winter",
                ifelse (d >= SE & d < SS, "Spring",
                        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

datD$season <- getSeason(datesD)

#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_boxplot()

#make a violin plot
ggplot(data= subset(datD, datD$year == "2016"), aes(season,discharge)) + 
        geom_violin() + 
        ggtitle("2016")

ggplot(data= subset(datD, datD$year == "2017"), aes(season,discharge)) + 
        geom_violin() + 
        ggtitle("2017")

