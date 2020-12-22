#clear the environment
rm(list=ls())
library(tidyverse)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Import the data
mydata = read.csv("pima-indians-diabetes.csv", header = FALSE)
names(mydata) <- c( "V1","V2","V3","V4","V5","V6","V7","V8","V9") 
glimpse(mydata)
#Separating group 2 and cleaning 0's by replacing mean
attribute_2 =  mydata$V2[]

#Separating group 4 and cleaning 0's by replacing mean
attribute_4 =  mydata$V4[]

#Separate data by class label for column 2
tempdata1 <- split(mydata$V2, mydata$V9==1)
attribute_2_class_1 = as.numeric(unlist(tempdata1[1]))
attribute_2_class_2 = as.numeric(unlist(tempdata1[2]))

#Separate data by class label  for column 4
tempdata2 <- split(mydata$V4, mydata$V9==1)
attribute_4_class_1 = as.numeric(unlist(tempdata2[1]))
attribute_4_class_2 = as.numeric(unlist(tempdata2[2]))

hist(attribute_2)
mean(attribute_2)
getmode(attribute_2)
median(attribute_2)
sd(attribute_2)
max(attribute_2)
min(attribute_2)
hist(attribute_2_class_1)
mean(attribute_2_class_1)
getmode(attribute_2_class_1)
median(attribute_2_class_1)
sd(attribute_2_class_1)
max(attribute_2_class_1)
min(attribute_2_class_1)
hist(attribute_2_class_2)
mean(attribute_2_class_2)
getmode(attribute_2_class_2)
median(attribute_2_class_2)
sd(attribute_2_class_2)
max(attribute_2_class_2)
min(attribute_2_class_2)

hist(attribute_4)
mean(attribute_4)
getmode(attribute_4)
median(attribute_4)
sd(attribute_4)
max(attribute_4)
min(attribute_4)
hist(attribute_4_class_1)
mean(attribute_4_class_1)
getmode(attribute_4_class_1)
median(attribute_4_class_1)
sd(attribute_4_class_1)
max(attribute_4_class_1)
min(attribute_4_class_1)
hist(attribute_4_class_2)
mean(attribute_4_class_2)
getmode(attribute_4_class_2)
median(attribute_4_class_2)
sd(attribute_4_class_2)
max(attribute_4_class_2)
min(attribute_4_class_2)

par(mfrow=c(1,3))
hist(attribute_2)
hist(attribute_2_class_1)
hist(attribute_2_class_2)
par(mfrow=c(1,3))
hist(attribute_4)
hist(attribute_4_class_1)
hist(attribute_4_class_2)

par(mfrow=c(1,2))
hist(attribute_2)
hist(attribute_4)

