library(spatstat)
#clear the environment
rm(list=ls())

#Import the data
mydata = read.csv("Zone_1.csv", header = TRUE)


mydata$buildingType <- as.factor(mydata$buildingType)

# Read minimum and maximum locations
min_longitude = min(mydata$longitude)
max_longitude = max(mydata$longitude)
min_latitude = min(mydata$latitude)
max_latitude = max(mydata$latitude)


#----- Task c: Analysis for Different Building Patterns ------
myPattern <- ppp(mydata$longitude, mydata$latitude, marks = mydata$buildingType, c(min_longitude,max_longitude), c(min_latitude,max_latitude))

unique(mydata$buildingType)
plot(Kcross(myPattern, "commercial_building", "collective_house"), main="Colocation Commercial Buildings and Collective Houses",
     xlab="Distance r",
     ylab="K(r)")

plot(Kcross(myPattern, "commercial_building", "single_house"), main="Colocation Commercial Buildings and Single Houses",
     xlab="Distance r",
     ylab="K(r)")

plot(Kcross(myPattern, "commercial_building", "garage"), main="Colocation Commercial Buildings and Garages",
     xlab="Distance r",
     ylab="K(r)")

plot(Kcross(myPattern, "commercial_building", "light_building"), main="Colocation Commercial Buildings and Light Buildings",
     xlab="Distance r",
     ylab="K(r)")

plot(Kcross(myPattern, "commercial_building", "school"), main="Colocation Commercial Buildings and Schools",
     xlab="Distance r",
     ylab="K(r)")

plot(Kcross(myPattern, "collective_house", "single_house"), main="Colocation Collective Houses and Single Houses",
     xlab="Distance r",
     ylab="K(r)")

plot(Kcross(myPattern, "collective_house", "garage"), main="Colocation Collective Houses and Garages",
     xlab="Distance r",
     ylab="K(r)")

plot(Kcross(myPattern, "collective_house", "light_building"), main="Colocation Collective Houses and Light Buildings",
     xlab="Distance r",
     ylab="K(r)")

plot(Kcross(myPattern, "collective_house", "school"), main="Colocation Collective Houses and Schools",
     xlab="Distance r",
     ylab="K(r)")

