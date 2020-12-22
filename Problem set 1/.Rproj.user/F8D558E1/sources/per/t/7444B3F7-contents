#clear the environment
rm(list=ls())

#Import the data
mydata = read.csv("Pid-clean.csv", header = TRUE)

#separete column 2, remove 0's and calculate mean and standard deviation
column2 = mydata[,'X148']
column2 = column2[column2 != 0]
result.meancolumn2 <- mean(column2)
result.sdcolumn2 <- sd(column2)

#separete column 3, remove 0's and calculate mean and standard deviation
column3 = mydata[,'X72']
column3 = column3[column3 != 0]
result.meancolumn3 <- mean(column3)
result.sdcolumn3 <- sd(column3)

#separete column 6, remove 0's and calculate mean and standard deviation
column6 = mydata[,'X33.6']
column6 = column6[column6 != 0]
result.meancolumn6 <- mean(column6)
result.sdcolumn6 <- sd(column6)

#separete column 7, remove 0's and calculate mean and standard deviation
column7 = mydata[,'X0.627']
column7 = column7[column7 != 0]
result.meancolumn7 <- mean(column7)
result.sdcolumn7 <- sd(column7)

#create a matrix with the four column
m <- cbind(column2,column3,column6,column7)

#Calculate coveriance matrix
cov(m)

#Calculate correlation matrix
cor(m)

