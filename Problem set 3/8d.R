#clear the environment
rm(list=ls())


# Initialize the Purity Function

purity <- function(a,b,outliers=FALSE) {
  
  cluster_labels <- unique(a)
  class_labels <- unique(b)
  elemets_in_class_by_cluster = matrix(0, nrow = length(cluster_labels), ncol = length(class_labels))
  i = 1
  while (i <= length(a)){
    if(a[i] != 0)
      elemets_in_class_by_cluster[a[i],b[i]] = elemets_in_class_by_cluster[a[i],b[i]] + 1
    i = i + 1
  }
  sum = 0
  i = 1
  while (i <= length(cluster_labels)){
    sum = sum + max(elemets_in_class_by_cluster[i,])
    i = i + 1
  }
  
  purity <- sum/length(a)
  if (outliers==FALSE){
    return (purity)
  }else{
    outlier_number = 0
    n <- table(a)
    outlier_number = n[names(n)==0]
    outlier_percentage = outlier_number / length(a)
    data = c(purity,outlier_percentage)
    return(data)
  }
} 

#Import the data
mydata = read.csv("Pid-clean.csv", header = FALSE)
names(mydata) <- c( "V1","V2","V3","V4","V5","V6","V7","V8","V9")
mydata[9][mydata[9]==0] <- 2
clusterable_data = mydata[ -c(9) ]

#Calculate Z score of data
mean_v1 <- mean(clusterable_data$V1)
sd_v1 <- sd(clusterable_data$V1)
clusterable_data$V1  <- (clusterable_data$V1 - mean_v1) /sd_v1

mean_v2 <- mean(clusterable_data$V2)
sd_v2 <- sd(clusterable_data$V2)
clusterable_data$V2  <- (clusterable_data$V2 - mean_v2) /sd_v2

mean_v3 <- mean(clusterable_data$V3)
sd_v3 <- sd(clusterable_data$V3)
clusterable_data$V3  <- (clusterable_data$V3 - mean_v3) /sd_v3

mean_v4 <- mean(clusterable_data$V4)
sd_v4 <- sd(clusterable_data$V4)
clusterable_data$V4  <- (clusterable_data$V4 - mean_v4) /sd_v4

mean_v5 <- mean(clusterable_data$V5)
sd_v5 <- sd(clusterable_data$V5)
clusterable_data$V5  <- (clusterable_data$V5 - mean_v5) /sd_v5

mean_v6 <- mean(clusterable_data$V6)
sd_v6 <- sd(clusterable_data$V6)
clusterable_data$V6  <- (clusterable_data$V6 - mean_v6) /sd_v6

mean_v7 <- mean(clusterable_data$V7)
sd_v7 <- sd(clusterable_data$V7)
clusterable_data$V7  <- (clusterable_data$V7 - mean_v7) /sd_v7

mean_v8 <- mean(clusterable_data$V8)
sd_v8 <- sd(clusterable_data$V8)
clusterable_data$V8  <- (clusterable_data$V8 - mean_v8) /sd_v8


#Applt dbscan
install.packages("fpc")
library("fpc")
set.seed(123)
db <- fpc::dbscan(clusterable_data, eps = 2, MinPts = 5)

#Visualize the clusters
install.packages("factoextra")
library("factoextra")
fviz_cluster(db, clusterable_data)
#Print the clusters
print(db)
#Calculate purity
print(purity(db$cluster,mydata$V9,TRUE))

