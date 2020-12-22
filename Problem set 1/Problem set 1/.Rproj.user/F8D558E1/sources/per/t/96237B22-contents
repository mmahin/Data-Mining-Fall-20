#clear the environment
rm(list=ls())

#Import the data
mydata = read.csv("Pid-clean.csv", header = FALSE)
names(mydata) <- c("no_pregnent","glucose","blood_pressure","skin_thickness","insulin","body_mass","diabetes","age","class")
mydata$class = factor(mydata$class, levels = c(0, 1))


m = mean(mydata$no_pregnent)
s = sd(mydata$no_pregnent)
mydata$no_pregnent <- (mydata$no_pregnent - m)/s

m = mean(mydata$glucose)
s = sd(mydata$glucose)
mydata$glucose <- (mydata$glucose - m)/s

m = mean(mydata$blood_pressure)
s = sd(mydata$blood_pressure)
mydata$blood_pressure <- (mydata$blood_pressure - m)/s

m = mean(mydata$skin_thickness)
s = sd(mydata$skin_thickness)
mydata$skin_thickness <- (mydata$skin_thickness - m)/s

m = mean(mydata$insulin)
s = sd(mydata$insulin)
mydata$insulin <- (mydata$insulin - m)/s

m = mean(mydata$body_mass)
s = sd(mydata$body_mass)
mydata$body_mass <- (mydata$body_mass - m)/s

m = mean(mydata$diabetes)
s = sd(mydata$diabetes)
mydata$diabetes <- (mydata$diabetes - m)/s

m = mean(mydata$age)
s = sd(mydata$age)
mydata$age <- (mydata$age - m)/s


knn <- function(traindata, testdata, k) {
  trainlen = nrow(traindata)
  testlen = nrow(testdata)
  
  i = 1
  j = 1
  correct = 0
  for (i in 1:testlen)
  {
    distances <- vector()
    for (j in 1:trainlen)  
    {
      a = as.numeric(testdata[i,-9])
      b = as.numeric(traindata[j,-9])
      x = sqrt(((a[1]-b[1]))^2 + ((a[2]-b[2]))^2 + ((a[3]-b[3]))^2 + (0.5*(a[4]-b[4]))^2 + 
                 (0.5*(a[5]-b[5]))^2 + ((a[6]-b[6]))^2 + ((a[7]-b[7]))^2 + ((a[8]-b[8]))^2)
      distances[j] <- x
    }
   
    indexes = sort(distances, index.return = TRUE)
    l = 
    first = 0
    second = 0
    for (l in 1:k)
    {
      cls = traindata$class[indexes$ix[l]]
      if (cls == 0)
        first = first + 1
      else
        second = second + 1
    }
    pred = 0
    if(second > first)
      pred = 1
    
    if (pred == testdata$class[i])
      correct = correct + 1
  
  }
  return (correct/testlen*100)
}

m = matrix(, nrow = 768, ncol = 768)
i = 1
j = 1


for (i in 1:768)
{
  for (j in 1:768)  
  {
    a = as.numeric(mydata[i,-9])
    b = as.numeric(mydata[j,-9])
    x = sqrt(((a[1]-b[1]))^2 + ((a[2]-b[2]))^2 + ((a[3]-b[3]))^2 + ((a[4]-b[4]))^2 + 
        ((a[5]-b[5]))^2 + (a[6]-b[6])^2 + (a[7]-b[7])^2 + (a[8]-b[8])^2)
    m[i,j] <- x
  }
}
m

#mydata = subset(mydata, select = -c(4,5) )
#mydata
# Define training control
#set.seed(123) 
#library(tidyverse)
#library(caret)

library(e1071)


#train.control <- trainControl(method = "cv", number = 10)
# Train the model
#model <- train(Fertility ~., data = swiss, method = "knn",
#               trControl = train.control)

# Summarize the resultssvm
#print(model)



#Randomly shuffle the data
mydata<-mydata[sample(nrow(mydata)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(mydata)),breaks=10,labels=FALSE)

testIndexes <- which(folds==1,arr.ind=TRUE)
testData <- mydata[testIndexes, ]
trainData <- mydata[-testIndexes, ]

acc <- vector()
library(caret)
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testIndexes
  testData <- mydata[testIndexes, ]
  trainData <- mydata[-testIndexes, ]
  
  ac = knn(trainData,testData,23)
  acc[i] <- ac

}
mean(acc)
print("Accuracy:")
acc
