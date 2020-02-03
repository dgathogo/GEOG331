
#Question 1

datW <- read.csv("/Volumes/class/Geog331/Students/dgathogo/a02/2011124.csv")
#datW <- read.csv("q:\\Students\\dgathogo\\a02\\2011124.csv")
str(datW)

# Question 2
#character vector
grades <- c('a', 'b','e', 'c', 'd')

#numeric vector
heights <- c(13.5, 9.9, 6.32, 12.0, 13.75)

#integer vector
ages <- c(19L, 6L, 38L, 9L, 13L)

#factor vector
years <- as.factor(c('1998', '2000','1994', '1998', '1995'))


# Question 3

datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))

datW$TAVE <- datW$TMIN + ((datW$TMAX - datW$TMIN)/2)

datW$siteN <- as.numeric(datW$NAME)

averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#Question 4 
# plot all histograms on the same window
par(mfrow=c(2,2))

#make a histogram for the first site in our levels, Aberdeen

hist(datW$TAVE[datW$siteN ==1], 
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "blue3",
     border = "white")

#add mean line
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean 
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


#make a histogram for the second site in our levels, Livermore

hist(datW$TAVE[datW$siteN ==2], 
     freq = FALSE,
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "cyan3",
     border = "white")

#add mean line
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#make a histogram for the third site in our levels, Mandan

hist(datW$TAVE[datW$siteN ==3], 
     freq = FALSE,
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "chartreuse3",
     border = "white")

#add mean line 
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean 
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#make a histogram for the fourth site in our levels, Mormon Flat

hist(datW$TAVE[datW$siteN ==4], 
     freq = FALSE,
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "blueviolet",
     border = "white")

#add mean line 
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean 
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

par(mfrow=c(1,1))

#Question 6
#make a histogram for the first site in our levels
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

#Question 6
temp <- qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

prob <- 1 - pnorm(temp - 4,
                  mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
                  sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))

# Question 7

hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily precipitation", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

# Question 8
annual_prcp <- aggregate(datW$PRCP, by=list('year'=datW$year, 'siteN'=datW$siteN), FUN=sum, na.rm=T)
hist(annual_prcp[annual_prcp$siteN == 3,]$x,
     freq = F,
     main = paste(levels(datW$NAME)[3]),
     xlab = "Annual precipitation",
     ylab = "Relative frequency",
     col = "grey50",
     border = "white")
     
# Question 9
mean_prcp_temp <- aggregate(annual_prcp$x, by=list('siteN'=annual_prcp$siteN), FUN=mean, na.rm=T)
colnames(mean_prcp_temp)[2] <- "prcp"
mean_prcp_temp$temp <- averageTemp$MAAT
mean_prcp_temp






