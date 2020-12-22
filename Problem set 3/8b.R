#clear the environment
rm(list=ls())

#Import the data
mydata = read.csv("Complex8.csv", header = FALSE)
names(mydata) <- c( "V1","V2","V3")
mydata[3][mydata[3]==0] <- 8
clusterable_data = mydata[ -c(3) ]

# Initialize the purity function
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

install.packages("factoextra")
library(factoextra)
set.seed(123)
# K = 8 once
km.res <- kmeans(clusterable_data, 8, nstart = 20)
print(km.res)
#visualize
fviz_cluster(km.res,clusterable_data) 
print("Purity")
print(purity(km.res$cluster,mydata$V3,TRUE))

# K = 8 twice
km.res <- kmeans(clusterable_data, 8, nstart = 20)
print(km.res)
#visualize
fviz_cluster(km.res,clusterable_data) 
print("Purity")
print(purity(km.res$cluster,mydata$V3,TRUE))

# K = 12 once
km.res <- kmeans(clusterable_data, 8, nstart = 20)
print(km.res)
#visualize
fviz_cluster(km.res,clusterable_data) 
print("Purity")
print(purity(km.res$cluster,mydata$V3,TRUE))

# K = 12 twice
km.res <- kmeans(clusterable_data, 8, nstart = 20)
print(km.res)
#visualize
fviz_cluster(km.res,clusterable_data) 
print("Purity")
print(purity(km.res$cluster,mydata$V3,TRUE))