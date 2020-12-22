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

# Calculate Zscores for all variables
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

# Calculate Kmeans clustering
wait.kmeans <- kmeans(clusterable_data, 3)
wait.kmeans$cluster

#visualize
install.packages("factoextra")
library(factoextra)
fviz_cluster(wait.kmeans,clusterable_data)

#Separete the clusters
one = clusterable_data[wait.kmeans$cluster == 1,]
two = clusterable_data[wait.kmeans$cluster == 2,]
three = clusterable_data[wait.kmeans$cluster == 3,]

#Calculate mean of each cluster
mean1_1 = mean(one$V1)
mean1_2 = mean(one$V2)
mean1_3 = mean(one$V3)
mean1_4 = mean(one$V4)
mean1_5 = mean(one$V5)
mean1_6 = mean(one$V6)
mean1_7 = mean(one$V7)
mean1_8 = mean(one$V8)
mean1 = c(mean1_1,mean1_2,mean1_3,mean1_4,mean1_5,mean1_6,mean1_7,mean1_8)

mean2_1 = mean(two$V1)
mean2_2 = mean(two$V2)
mean2_3 = mean(two$V3)
mean2_4 = mean(two$V4)
mean2_5 = mean(two$V5)
mean2_6 = mean(two$V6)
mean2_7 = mean(two$V7)
mean2_8 = mean(two$V8)
mean2 = c(mean2_1,mean2_2,mean2_3,mean2_4,mean2_5,mean2_6,mean2_7,mean2_8)

mean3_1 = mean(three$V1)
mean3_2 = mean(three$V2)
mean3_3 = mean(three$V3)
mean3_4 = mean(three$V4)
mean3_5 = mean(three$V5)
mean3_6 = mean(three$V6)
mean3_7 = mean(three$V7)
mean3_8 = mean(three$V8)
mean3 = c(mean3_1,mean3_2,mean3_3,mean3_4,mean3_5,mean3_6,mean3_7,mean3_8)

#Create the mean matrix
prior_means = array(c(mean1,mean2,mean3),dim = c(3,8))

#Calculate standard deviation of each cluster
sd1_1 = sd(one$V1)
sd1_2 = sd(one$V2)
sd1_3 = sd(one$V3)
sd1_4 = sd(one$V4)
sd1_5 = sd(one$V5)
sd1_6 = sd(one$V6)
sd1_7 = sd(one$V7)
sd1_8 = sd(one$V8)
sd1 = c(sd1_1,sd1_2,sd1_3,sd1_4,sd1_5,sd1_6,sd1_7,sd1_8)

sd2_1 = sd(two$V1)
sd2_2 = sd(two$V2)
sd2_3 = sd(two$V3)
sd2_4 = sd(two$V4)
sd2_5 = sd(two$V5)
sd2_6 = sd(two$V6)
sd2_7 = sd(two$V7)
sd2_8 = sd(two$V8)
sd2 = c(sd2_1,sd2_2,sd2_3,sd2_4,sd2_5,sd2_6,sd2_7,sd2_8)

sd3_1 = sd(three$V1)
sd3_2 = sd(three$V2)
sd3_3 = sd(three$V3)
sd3_4 = sd(three$V4)
sd3_5 = sd(three$V5)
sd3_6 = sd(three$V6)
sd3_7 = sd(three$V7)
sd3_8 = sd(three$V8)
sd3 = c(sd3_1,sd3_2,sd3_3,sd3_4,sd3_5,sd3_6,sd3_7,sd3_8)

#Create the standard deviation matrix
prior_sd = array(c(sd1,sd2,sd3),dim = c(3,8))

# Print Values
prior_means
prior_sd
wait.kmeans

#Calculate the purity
print(purity(wait.kmeans$cluster,mydata$V9,TRUE))

#Print the boxplots for each variable from each cluster
boxplot(one$V1)
boxplot(one$V2)
boxplot(one$V3)
boxplot(one$V4)
boxplot(one$V5)
boxplot(one$V6)
boxplot(one$V7)
boxplot(one$V8)

boxplot(two$V1)
boxplot(two$V2)
boxplot(two$V3)
boxplot(two$V4)
boxplot(two$V5)
boxplot(two$V6)
boxplot(two$V7)
boxplot(two$V8)

boxplot(three$V1)
boxplot(three$V2)
boxplot(three$V3)
boxplot(three$V4)
boxplot(three$V5)
boxplot(three$V6)
boxplot(three$V7)
boxplot(three$V8)

# Apply EM algorithm
install.packages("ClusterR")
library(ClusterR)
gmm = GMM(clusterable_data, 3, dist_mode = "eucl_dist", seed_mode = "random_subset", km_iter = 20,
          
          em_iter = 20, verbose = F) 
#Print Centroids and Coveriance Matrix
gmm$centroids
gmm$covariance_matrices

#Seperate the clusters(Hard clustering)
cluster = apply(gmm$Log_likelihood, 1,
                which.max)

one = clusterable_data[cluster == 1,]
two = clusterable_data[cluster == 2,]
three = clusterable_data[cluster == 3,]

#Calculate mean of each cluster
mean1_1 = mean(one$V1)
mean1_2 = mean(one$V2)
mean1_3 = mean(one$V3)
mean1_4 = mean(one$V4)
mean1_5 = mean(one$V5)
mean1_6 = mean(one$V6)
mean1_7 = mean(one$V7)
mean1_8 = mean(one$V8)
mean1 = c(mean1_1,mean1_2,mean1_3,mean1_4,mean1_5,mean1_6,mean1_7,mean1_8)

mean2_1 = mean(two$V1)
mean2_2 = mean(two$V2)
mean2_3 = mean(two$V3)
mean2_4 = mean(two$V4)
mean2_5 = mean(two$V5)
mean2_6 = mean(two$V6)
mean2_7 = mean(two$V7)
mean2_8 = mean(two$V8)
mean2 = c(mean2_1,mean2_2,mean2_3,mean2_4,mean2_5,mean2_6,mean2_7,mean2_8)

mean3_1 = mean(three$V1)
mean3_2 = mean(three$V2)
mean3_3 = mean(three$V3)
mean3_4 = mean(three$V4)
mean3_5 = mean(three$V5)
mean3_6 = mean(three$V6)
mean3_7 = mean(three$V7)
mean3_8 = mean(three$V8)
mean3 = c(mean3_1,mean3_2,mean3_3,mean3_4,mean3_5,mean3_6,mean3_7,mean3_8)

#Create the mean matrix
prior_means = array(c(mean1,mean2,mean3),dim = c(3,8))

#Calculate standard deviation of each cluster
sd1_1 = sd(one$V1)
sd1_2 = sd(one$V2)
sd1_3 = sd(one$V3)
sd1_4 = sd(one$V4)
sd1_5 = sd(one$V5)
sd1_6 = sd(one$V6)
sd1_7 = sd(one$V7)
sd1_8 = sd(one$V8)
sd1 = c(sd1_1,sd1_2,sd1_3,sd1_4,sd1_5,sd1_6,sd1_7,sd1_8)

sd2_1 = sd(two$V1)
sd2_2 = sd(two$V2)
sd2_3 = sd(two$V3)
sd2_4 = sd(two$V4)
sd2_5 = sd(two$V5)
sd2_6 = sd(two$V6)
sd2_7 = sd(two$V7)
sd2_8 = sd(two$V8)
sd2 = c(sd2_1,sd2_2,sd2_3,sd2_4,sd2_5,sd2_6,sd2_7,sd2_8)

sd3_1 = sd(three$V1)
sd3_2 = sd(three$V2)
sd3_3 = sd(three$V3)
sd3_4 = sd(three$V4)
sd3_5 = sd(three$V5)
sd3_6 = sd(three$V6)
sd3_7 = sd(three$V7)
sd3_8 = sd(three$V8)
sd3 = c(sd3_1,sd3_2,sd3_3,sd3_4,sd3_5,sd3_6,sd3_7,sd3_8)

#Create the standard deviation matrix
prior_sd = array(c(sd1,sd2,sd3),dim = c(3,8))

#Print Values
prior_means
prior_sd
#Calculate the puriy
print(purity(cluster,mydata$V9,TRUE))

#Print the boxplots for each variable from each cluster
boxplot(one$V1)
boxplot(one$V2)
boxplot(one$V3)
boxplot(one$V4)
boxplot(one$V5)
boxplot(one$V6)
boxplot(one$V7)
boxplot(one$V8)

boxplot(two$V1)
boxplot(two$V2)
boxplot(two$V3)
boxplot(two$V4)
boxplot(two$V5)
boxplot(two$V6)
boxplot(two$V7)
boxplot(two$V8)

boxplot(three$V1)
boxplot(three$V2)
boxplot(three$V3)
boxplot(three$V4)
boxplot(three$V5)
boxplot(three$V6)
boxplot(three$V7)
boxplot(three$V8)

#Visulaize the clusters
fviz_cluster(list(data=clusterable_data,cluster= cluster),clusterable_data)
