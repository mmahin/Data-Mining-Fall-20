library(spatstat)
#clear the environment
rm(list=ls())

#Import the data
mydata = read.csv("Z.csv", header = TRUE)
set.seed(001)
mydata$latitude=sample(mydata$latitude)
mydata$longitude=sample(mydata$longitude)

mydata1 = mydata

mydata1$buildingType[mydata1$buildingType == "collective_house"] <- "collective_houses&garages"
mydata1$buildingType[mydata1$buildingType == "garage"] <- "collective_houses&garages"



mydata1$buildingType <- as.factor(mydata1$buildingType)
unique(mydata1$buildingType)


# Read minimum and maximum locations
min_longitude = min(mydata1$longitude)
max_longitude = max(mydata1$longitude)
min_latitude = min(mydata1$latitude)
max_latitude = max(mydata1$latitude)
par(mfrow=c(1,1))

#----- Task c: Analysis for Different Building Patterns ------
myPattern1 <- ppp(mydata1$longitude, mydata1$latitude, marks = mydata1$buildingType, c(min_longitude,max_longitude), c(min_latitude,max_latitude))

plot1 <- plot(Kcross(myPattern1, "collective_houses&garages", "single_house"), main="A:Ranodm Colocation Collective Houses and garages as same category with sigle house",
              xlab="Distance r",
              ylab="K(r)")


#Import the data
mydata2 = mydata

mydata2$buildingType[mydata2$buildingType == "collective_house"] <- "collective_houses&single_houses"
mydata2$buildingType[mydata2$buildingType == "single_house"] <- "collective_houses&single_houses"



mydata2$buildingType <- as.factor(mydata2$buildingType)
unique(mydata2$buildingType)

myPattern2 <- ppp(mydata2$longitude, mydata2$latitude, marks = mydata2$buildingType, c(min_longitude,max_longitude), c(min_latitude,max_latitude))

plot2 <- plot(Kcross(myPattern2, "collective_houses&single_houses", "garage"), main="B:Ranodm Colocation Collective Houses and single house as same category with garage",
              xlab="Distance r",
              ylab="K(r)")


#Import the data
mydata3 = mydata

mydata3$buildingType[mydata3$buildingType == "garage"] <- "garage&single_houses"
mydata3$buildingType[mydata3$buildingType == "single_house"] <- "garage&single_houses"



mydata3$buildingType <- as.factor(mydata3$buildingType)
unique(mydata3$buildingType)

myPattern3 <- ppp(mydata3$longitude, mydata3$latitude, marks = mydata3$buildingType, c(min_longitude,max_longitude), c(min_latitude,max_latitude))

plot3 <- plot(Kcross(myPattern3, "garage&single_houses", "collective_house"), main="C:Ranodm Colocation single Houses and garages as same category with collectieve house",
              xlab="Distance r",
              ylab="K(r)")
