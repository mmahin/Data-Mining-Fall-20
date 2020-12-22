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
  
  #Use the test and train data partitions however you desire...
  classifier = svm(formula = class ~ ., 
                   data = trainData, 
                   type = 'C-classification',
                   scale = FALSE,
                   kernel = 'sigmoid',
                   #degree = 3,
                   gamma =.01,
                   coef0 = 1
  )
  y_pred = predict(classifier, newdata = testData[-9]) 
  cm = table(testData[, 9], y_pred)
  accuracy_Train <- sum(diag(cm)) / sum(cm)
  acc[i] <- accuracy_Train
  
}
mean(acc)
acc

  
 
