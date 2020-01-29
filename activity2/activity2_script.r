datW <- read.csv("q:\\Students\\dgathogo\\a02\\2011124.csv")
str(datW)

datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))
str(datW)

#character vector
char_vector <- c('a', 'b','y', 'h', 'q')

#numeric vector
num_vector <- c(13.5, 9.9, 6.32, 12.0, 13.75)

#integer vector
int_vector <- as.integer(c(1, 6, 38, 9, 13))

#factor vector
fact_vector <- as.factor(c('1998', '2000','1994', '1998', '1995'))
