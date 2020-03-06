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


redL <- raster("Y:\\Students\\dgathogo\\a06\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("Y:\\Students\\dgathogo\\a06\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("Y:\\Students\\dgathogo\\a06\\glacier_09_05_14\\l08_blue.tif")