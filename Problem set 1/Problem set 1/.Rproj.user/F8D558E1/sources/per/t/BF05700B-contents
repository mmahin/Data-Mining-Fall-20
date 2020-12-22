#clear the environment
rm(list=ls())

#Import the data
mydata = read.csv("Pid-clean.csv", header = TRUE)

library(tidyverse)
glimpse(mydata)

# Split data into training (70%) and validation (30%)
dt = sort(sample(nrow(mydata), nrow(mydata)*.7))
train<-mydata[dt,]
val<-mydata[-dt,] # Check number of rows in training data set
nrow(train)

# Decision Tree Model 1
library(rpart)
mtree1 <- rpart(X1~., data = train, method="class", parms = list(split = "information"),control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 4, usesurrogate = 2, xval =10 ))


#Beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#view1
prp(mtree1, faclen = 0, cex = 0.8, extra = 1)

#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(mtree1, faclen = 0, cex = 0.8, node.fun=tot_count)
# confusion matrix (training data)
conf.matrix <- table(train$X1, predict(mtree1,type="class"))
accuracy_Train <- sum(diag(conf.matrix)) / sum(conf.matrix)
accuracy_Train


predict_unseen <-predict(mtree1, val, type = 'class')
table_mat <- table(val$X1, predict_unseen)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

############################
########Pruning#############
############################

#printcp(mtree1)
#bestcp <- mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]

# Prune the tree using the best cp.
#pruned <- prune(mtree1, cp = bestcp)

# Plot pruned tree
#prp(pruned, faclen = 0, cex = 0.8, extra = 1)

mtree2 <- rpart(X1~., data = train, method="class", parms = list(split = "gini"),control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 4, usesurrogate = 2, xval =10 ))


#Beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#view1
prp(mtree2, faclen = 0, cex = 0.8, extra = 1)

#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(mtree2, faclen = 0, cex = 0.8, node.fun=tot_count)
# confusion matrix (training data)
conf.matrix <- table(train$X1, predict(mtree2,type="class"))
accuracy_Train <- sum(diag(conf.matrix)) / sum(conf.matrix)
accuracy_Train


predict_unseen <-predict(mtree2, val, type = 'class')
table_mat <- table(val$X1, predict_unseen)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test


mtree3 <- rpart(X1~., data = train, method="class", parms = list(split = "gini"),control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 5, usesurrogate = 2, xval =10 ))


#Beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#view1
prp(mtree3, faclen = 0, cex = 0.8, extra = 1)

#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(mtree3, faclen = 0, cex = 0.8, node.fun=tot_count)


# Prune the tree using the best cp.
pruned <- prune(mtree3, cp = bestcp)

# Plot pruned tree
prp(pruned, faclen = 0, cex = 0.8, extra = 1)

# confusion matrix (training data)
conf.matrix <- table(train$X1, predict(mtree3,type="class"))
accuracy_Train <- sum(diag(conf.matrix)) / sum(conf.matrix)
accuracy_Train


predict_unseen <-predict(mtree3, val, type = 'class')
table_mat <- table(val$X1, predict_unseen)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
