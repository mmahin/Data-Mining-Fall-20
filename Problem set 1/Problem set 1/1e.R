#clear the environment
rm(list=ls())

#Import the data
mydata = read.csv("Pid-clean.csv", header = TRUE)

library(tidyverse)
glimpse(mydata)

m = mean(mydata$X6)
s = sd(mydata$X6)
mydata$X6 <- (mydata$X6 - m)/s

m = mean(mydata$X148)
s = sd(mydata$X148)
mydata$X148 <- (mydata$X148 - m)/s

m = mean(mydata$X72)
s = sd(mydata$X72)
mydata$X72 <- (mydata$X72 - m)/s

m = mean(mydata$X35)
s = sd(mydata$X35)
mydata$X35 <- (mydata$X35 - m)/s

m = mean(mydata$X156)
s = sd(mydata$X156)
mydata$X156 <- (mydata$X156 - m)/s

m = mean(mydata$X33.6)
s = sd(mydata$X33.6)
mydata$X33.6 <- (mydata$X33.6 - m)/s

m = mean(mydata$X0.627)
s = sd(mydata$X0.627)
mydata$X0.627 <- (mydata$X0.627 - m)/s

m = mean(mydata$X50)
s = sd(mydata$X50)
mydata$X50 <- (mydata$X50 - m)/s

linearMod <- lm(mydata$X1 ~ mydata$X6 + mydata$X148 + mydata$X72 + mydata$X35 + 
                  mydata$X156 + mydata$X33.6 + mydata$X0.627 + mydata$X50, data=mydata)
coefficients(linearMod)
 
summary(linearMod)$r.squared 
summary(linearMod) 

linearMod2 <- lm(mydata$X1 ~ mydata$X6 + mydata$X148+ mydata$X33.6 + mydata$X0.627 + mydata$X50, data=mydata)
coefficients(linearMod2)

summary(linearMod2)

anova(linearMod, linearMod2) 

anova(linearMod2, linearMod) 
 

