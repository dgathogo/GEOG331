Mat <- matrix(c(1,2,3,4,5,6), ncol=2, byrow = TRUE)
Mat[1,2]
Mat.bycol <- matrix(c(1,2,3,4,5,6), ncol=2, byrow = FALSE)

Mat.bycol[1,2] # element in row 1 col 2
Mat.bycol[1,] # all elements in row 1
Mat.bycol[,2] # all elements in col 2






datW <- read.csv("q:\\Students\\dgathogo\\a02\\2011124.csv")
str(datW)

datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))
str(datW)

#character vector
grades <- c('a', 'b','e', 'c', 'd')

#numeric vector
heights <- c(13.5, 9.9, 6.32, 12.0, 13.75)

#integer vector
ages <- as.integer(c(19, 6, 38, 9, 13))

#factor vector
years <- as.factor(c('1998', '2000','1994', '1998', '1995'))
