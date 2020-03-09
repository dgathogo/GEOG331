#install.packages(c("raster","sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("Y:\\Students\\dgathogo\\a06\\GNPglaciers\\GNPglaciers_1966.shp")

g1998 <- readOGR("Y:\\Students\\dgathogo\\a06\\GNPglaciers\\GNPglaciers_1998.shp")

g2005 <- readOGR("Y:\\Students\\dgathogo\\a06\\GNPglaciers\\GNPglaciers_2005.shp")

g2015 <- readOGR("Y:\\Students\\dgathogo\\a06\\GNPglaciers\\GNPglaciers_2015.shp")

str(g1966)
spplot(g1966, "GLACNAME")
g1966@proj4string
#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))


redL <- raster("Y:\\Students\\dgathogo\\a06\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("Y:\\Students\\dgathogo\\a06\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("Y:\\Students\\dgathogo\\a06\\glacier_09_05_14\\l08_blue.tif")

rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)

plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("Y:\\Students\\dgathogo\\a06\\NDVI\\NDVI_",ndviYear[i],".tif"))
  
}

str(NDVIraster[[1]])

par(mai=c(1,1,1,1))
par(mfrow=c(1,2))
plot(g1966,axes=T)
plot(NDVIraster[[1]], axes=T)
