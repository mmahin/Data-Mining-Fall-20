#clear the environment
rm(list=ls())

#Import the data
mydata = read.csv("Complex8_N15.csv", header = TRUE)
#names(mydata) <- c( "V0","V1","V2","V3")
mydata <- mydata[ -c(1) ]
col1 <- mydata[1]
col2 <- mydata[2]
col3 <- mydata[3]
library(Rlof)
df <- mydata[-3]
df
df.lof<-lof(df,c(50))
length(df.lof)
length(df$v1)
X <- data.frame("v1" = col1, "v2" = col2, "v3" = col3, "score" = df.lof)
attach(X)
data <- X[order(-score),]
data

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

amount <- as.integer((length(df.lof)*7)/100)

data1 = data[1:amount,]
data2 = data[-c(1:amount),]

data2$v3 <- as.factor(data2$v3)
data1$v3 <- as.factor(data1$v3)

plot1 <- ggplot(data1, aes(x=v1, y=v2, color=v3)) + geom_point(size=2, shape=23) + ggtitle("7% Outliers")
plot2 <- ggplot(data2, aes(x=v1, y=v2, color=v3)) + geom_point(size=2, shape=23) + ggtitle("93% Normal")
grid.arrange(plot1,plot2, ncol=2)


amount <- as.integer((length(df.lof)*14)/100)

data1 = data[1:amount,]
data2 = data[-c(1:amount),]

data2$v3 <- as.factor(data2$v3)
data1$v3 <- as.factor(data1$v3)

plot1 <- ggplot(data1, aes(x=v1, y=v2, color=v3)) + geom_point(size=2, shape=23) + ggtitle("14% Outliers")
plot2 <- ggplot(data2, aes(x=v1, y=v2, color=v3)) + geom_point(size=2, shape=23) + ggtitle("86% Normal")
grid.arrange(plot1,plot2, ncol=2)


amount <- as.integer((length(df.lof)*21)/100)

data1 = data[1:amount,]
data2 = data[-c(1:amount),]

data2$v3 <- as.factor(data2$v3)
data1$v3 <- as.factor(data1$v3)

plot1 <- ggplot(data1, aes(x=v1, y=v2, color=v3)) + geom_point(size=2, shape=23) + ggtitle("21% Outliers")
plot2 <- ggplot(data2, aes(x=v1, y=v2, color=v3)) + geom_point(size=2, shape=23) + ggtitle("79% Normal")
grid.arrange(plot1,plot2, ncol=2)
par(mfrow=c(1,1))
ols_21_percent <- data1$score
hist(ols_21_percent)
write.csv(data,"C:\\Users\\mdmah\\PycharmProjects\\ProfessorEick\\DM2020\\Problem set 2\\Problem set 2\\X.csv", row.names = TRUE)


wait <- mydata$v3

wait.kmeans <- kmeans(wait, 8)
wait.kmeans.cluster <- wait.kmeans$cluster

wait.df <-  data.frame(x = mydata$v1,y=mydata$v2, cluster = wait.kmeans.cluster)
ggplot(wait.df, aes(y = y, x = x, color = factor(cluster))) +
  geom_point()


attach(wait.df)
data <- wait.df[order(cluster),]

f<-function(x)
{ 
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  sum = 0
  for(i in 1:length(x[,1]))
  {
    sum = sum + euc.dist(x[i,1], x[i,2])
  }
  mean <- sum/length(x[,1])
  
  sum = 0
  for(i in 1:length(x[,1]))
  {
    sum = sum + ((euc.dist(x[i,1], x[i,2]) -mean)**2)
  }
  
  varience = sum / (length(x[,1])-1)
  
  return(c(mean, varience))
}

distance<-function(x)
{ 
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  vector <- c()
  
  for(i in 1:length(x[,1]))
  {
    vector[i] <- euc.dist(x[i,1], x[i,2])
  }

  
  return(vector)
}

data$cluster <- as.factor(data$cluster)
cluster1 = data[which(data[3]==1),]
result1 = f(cluster1[,-3])
alpha1 = length(cluster1[,1])/length(data[,1])

cluster2 = data[which(data[3]==2),]
result2 = f(cluster2[,-3])
alpha2 = length(cluster2[,1])/length(data[,1])

cluster3 = data[which(data[3]==3),]
result3 = f(cluster3[,-3])
alpha3 = length(cluster3[,1])/length(data[,1])

cluster4 = data[which(data[3]==4),]
result4 = f(cluster4[,-3])
alpha4 = length(cluster4[,1])/length(data[,1])

cluster5 = data[which(data[3]==5),]
result5 = f(cluster5[,-3])
alpha5 = length(cluster5[,1])/length(data[,1])

cluster6 = data[which(data[3]==6),]
result6 = f(cluster6[,-3])
alpha6 = length(cluster6[,1])/length(data[,1])

cluster7 = data[which(data[3]==7),]
result7 = f(cluster7[,-3])
alpha7 = length(cluster7[,1])/length(data[,1])

cluster8 = data[which(data[3]==8),]
result8 = f(cluster8[,-3])
alpha8 = length(cluster8[,1])/length(data[,1])
x <- distance( data[,-3])
library(mixtools)
gm<-normalmixEM(x,k=8,lambda=c(alpha1,alpha2,alpha3,alpha4,alpha5,alpha6,alpha7,alpha8),
mu=c(result1[1],result2[1],result3[1],result4[1],result5[1],result6[1],result7[1],result8[1])
,sigma=c(result1[2],result2[2],result3[2],result4[2],result5[2],result6[2],result7[2],result8[2]))
gm$mu
gm$sigma

x1 <- distance( cluster1[,-3])
mean(x1)
sd(x1)
mean1 <- gm$mu[1]
std1 <- gm$sigma[1]
