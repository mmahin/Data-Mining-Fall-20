#clear the environment
rm(list=ls())
library(tidyverse)

#Import the data
mydata = read.csv("pima-indians-diabetes.csv", header = TRUE)

glimpse(mydata)

#Separating group 1 and cleaning 0's by replacing mean
column1 =  mydata$X6[]

#Separating group 2 and cleaning 0's by replacing mean
column2 =  mydata$X148[]

#Separating group 6 and cleaning 0's by replacing mean
column6 =  mydata$X33.6[]

#Separating group 6 and cleaning 0's by replacing mean
column8 =  mydata$X50[]

library(ggplot2)

library(hrbrthemes)
mydata$X1 <- as.factor(mydata$X1)
ggplot(data = mydata, aes(x = X6, y = X148, shape= X1, color = X1 ))+ geom_point() 

ggplot(data = mydata, aes(x = X6, y = X33.6, shape= X1, color = X1 ))+ geom_point() 

ggplot(data = mydata, aes(x = X6, y = X50, shape= X1, color = X1 ))+ geom_point() 

ggplot(data = mydata, aes(x = X148, y = X33.6, shape= X1, color = X1 ))+ geom_point() 

ggplot(data = mydata, aes(x = X148, y = X50, shape= X1, color = X1 ))+ geom_point() 
ggplot(data = mydata, aes(x = X33.6, y = X50, shape= X1, color = X1 ))+ geom_point() 
par(mfrow=c(1,1))
library("scatterplot3d")
shapes = c(16, 17) 
shapes <- shapes[as.numeric(mydata$X1)]
colors <- c("#56B4E9", "#E69F00")
colors <- colors[as.numeric(mydata$X1)]
s3d <- scatterplot3d(x=mydata$X148, y=mydata$X33.6, z=mydata$X50, pch = shapes, color=colors)
legend("right", legend = levels(mydata$X1),
       col =  c("#56B4E9", "#E69F00"), pch = shapes)
my.lm <- lm(mydata$X148 ~ mydata$X33.6 + mydata$X1)
s3d$plane3d(my.lm)

library("gg3D")
ggplot(mydata, aes(x=X148, y=X33.6, z=X50, shape= X1, color=X1)) + 
  theme_void() +
  axes_3D() +
  stat_3D()

install.packages("plotly")
library(plotly)

plot_ly(x=mydata$X148, y=mydata$X33.6, z=mydata$X50, type="scatter3d", mode="markers", color=mydata$X1)
