#clear the environment
rm(list=ls())

#Import the data
mydata = read.csv("Complex8_N15.csv", header = TRUE)
#names(mydata) <- c( "V0","V1","V2","V3")
mydata <- mydata[ -c(1) ]


#par(xpd = T, mar = par()$mar + c(0,0,0,7))

plot(mydata$V1, mydata$V2, 
     col=mydata$V3, main="Supervised Scatterplot",
     xlab="v1 ", ylab="v2 ", pch=19, labels=row.names( mydata$V3))

legend("topright", inset = c(- 0.12, 0),                   # Create legend outside of plot
       legend = c("Group 1","Group 2","Group 3","Group 4","Group 5","Group 6","Group 7"),
       pch = 19,
       col = c("cadetblue","chartreuse", "chartreuse4", "chocolate4", "coral1",
               "black", "blue2", "darkgoldenrod1"))

library(ggplot2)
mydata$v3 <- as.factor(mydata$v3)
ggplot(mydata, aes(x=v1, y=v2, color=v3)) + geom_point(size=2, shape=23)

mydata = read.csv("Complex8.csv", header = TRUE)
library(ggplot2)
mydata$v3 <- as.factor(mydata$v3)
ggplot(mydata, aes(x=v1, y=v2, color=v3)) + geom_point(size=2, shape=23)


