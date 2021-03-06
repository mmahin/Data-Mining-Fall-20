#clear the environment
rm(list=ls())

library(ggplot2)

#Import the data
mydata = read.csv("Pid-clean.csv", header = FALSE)
names(mydata) <- c( "V1","V2","V3","V4","V5","V6","V7","V8","V9") 

mydata = na.omit(mydata)
#Column headers
mydata[0,]

#separate column 6, remove 0's and calculate mean and standard deviation
column6 = mydata[,'V6']
column6 = column6[column6 != 0]
boxplot(column6, xlab ="Attribute 6")
boxplot.stats(column6)
iqr<-IQR(column6)
q <- quantile(column6)
q
mean(column6)
boxplot.stats(column6)
upper  = min(max(column6), q[4] + 1.5 * iqr)
lower  = max(min(column6), q[2] - 1.5 * iqr)

#separate column 6, remove 0's and calculate mean and standard deviation
tempdata <- split(mydata$V6, mydata$V9==1)

# Analysis attribute 6 class 1
column6group1 = tempdata[1]
boxplot(column6group1, xlab ="Attribute 6: class 1(label 0)")
iqr <- IQR(as.numeric(unlist(column6group1)))
q <- quantile(as.numeric(unlist(column6group1)))
q
mean(as.numeric(unlist(column6group1)))
boxplot.stats(as.numeric(unlist(column6group1)))
upper  = min(max(as.numeric(unlist(column6group1))), q[4] + 1.5 * iqr)
lower  = max(min(as.numeric(unlist(column6group1))), q[2] - 1.5 * iqr)


# Analysis attribute 6 class 2
data2 = tempdata[2]
column6group2 = tempdata[2]
boxplot(column6group2, xlab ="Attribute 6: class 2(label 1)")
iqr <- IQR(as.numeric(unlist(column6group2)))
q<-quantile(as.numeric(unlist(column6group2)))
q
mean(as.numeric(unlist(column6group2)))
boxplot.stats(as.numeric(unlist(column6group2)))
upper  = min(max(as.numeric(unlist(column6group2))), q[4] + 1.5 * iqr)
lower  = max(min(as.numeric(unlist(column6group2))), q[2] - 1.5 * iqr)

#separate column 8, remove 0's and calculate mean and standard deviation
column8 = mydata[,'V8']
column8 = column8[column8 != 0]
boxplot(column8, xlab = "Attribute 8")
iqr <- IQR(column8)
q <- quantile(column8)
q
mean(column8)
boxplot.stats(column8)
upper  = min(max(column8), q[4] + 1.5 * iqr)
lower  = max(min(column8), q[2] - 1.5 * iqr)
upper-lower 

#separate column 8, remove 0's and calculate mean and standard deviation
tempdata <- split(mydata$V8, mydata$V9==1)

# Analysis attribute 8 class 1
column8group1 = tempdata[1]
boxplot(column8group1,xlab = "Attribute 8: class 1(label 0)")
iqr <- IQR(as.numeric(unlist(column8group1)))
q <- quantile(as.numeric(unlist(column8group1)))
q
mean(as.numeric(unlist(column8group1)))
boxplot.stats(as.numeric(unlist(column8group1)))
upper  = min(max(as.numeric(unlist(column8group1))), q[4] + 1.5 * iqr)
lower  = max(min(as.numeric(unlist(column8group1))), q[2] - 1.5 * iqr)
upper-lower

# Analysis attribute 8 class 2
data2 = tempdata[2]
column8group2 = tempdata[2]
boxplot(column8group2, xlab  ="Attribute 8: class 2(label 1)")

iqr <- IQR(as.numeric(unlist(column8group2)))
q <- quantile(as.numeric(unlist(column8group2)))
q
mean(as.numeric(unlist(column8group2)))
boxplot.stats(as.numeric(unlist(column8group2)))
upper  = min(max(as.numeric(unlist(column8group2))), q[4] + 1.5 * iqr)
lower  = max(min(as.numeric(unlist(column8group2))), q[2] - 1.5 * iqr)
upper-lower
column8group1 = tempdata[1]

#Visualizations
boxplot(column6,as.numeric(unlist(column6group1)),as.numeric(unlist(column6group2)), names=c("Attribute 6", "Attribute 6: class 1(label 0)", "Attribute 6: class 2 (label 1)"))
boxplot(column8,as.numeric(unlist(column8group1)),as.numeric(unlist(column8group2)), names=c("Attribute 8", "Attribute 8: class 1(label 0)", "Attribute 8: class 2(label 1)"))
boxplot(column6,column8, names=c("Attribute 6", "Attribute 8"))
boxplot(as.numeric(unlist(column6group1)),as.numeric(unlist(column8group1)),names=c("Attribute 6: class 1 (label 0)", "Attribute 8: class 1(label 0)"))
boxplot(as.numeric(unlist(column6group2)),as.numeric(unlist(column8group2)),names=c("Attribute 6: class 2(label 1)", "Attribute 8: class 2(label 1)"))


