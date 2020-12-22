#clear the environment
rm(list=ls())

#Initialize the purity function
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
mydata = read.csv("Complex8.csv", header = FALSE)
names(mydata) <- c( "V1","V2","V3")
mydata[3][mydata[3]==0] <- 8
clusterable_data = mydata[ -c(3) ]

#Seach procedure Genetic algorithm
#Initial Parameters
install.packages("fpc")
library("fpc")
min_eps = 0
max_eps = 10
minpts = 1
maxpts = 100

#Create initial random population
eps = runif(50,min_eps,max_eps)
pts = sample(minpts:maxpts,50)
genes <- array(c(eps,pts),dim = c(50,2))

#Initialization
values_eps = c()
values_ptr = c()
clus = c()
outs = c()
values = c()
iter <- 60
count = 0
while (count < iter){
  #Apply DBSCAN and Calculate fitness of each Chromosome
  i = 1
  while (i < 51){
    set.seed(123)
    db <- fpc::dbscan(clusterable_data, eps = genes[i,1], MinPts = genes[i,2])
    outliers = length(db$cluster[db$cluster==0])/2550
    clusters = unique(db$cluster)
    clusters = clusters[clusters != 0]
    cluster_length = length(clusters)
    fitness = c()
    if(cluster_length<2 || cluster_length> 15 || outliers > .1){
      fitness[i] <- (-1*cluster_length*outliers)
    }
    else{
      fitness[i] <- (cluster_length*(1-outliers))
    }
    if(fitness[i]>0){
      print(genes[i,1])
      print(genes[i,2])
      outs = append(outs,outliers)
      clus = append(clus,cluster_length)
      values_eps = append(values_eps,genes[i,1])
      values_ptr = append(values_ptr,genes[i,2])
      values = append(values,fitness[i])
    }
    i = i + 1
  }
  
  # Crossover, first 80% randomly chosen population 
  # Randomly chose two group of population, 40% each
  ordered <- order(fitness)
  population1 = c()
  population2 = c()
  selection = sample(1:40, 20)
  
  i = 1
  while (i <= 40){
    if (i %in%  selection)
    {
      population1<- append(population1,i)
    }
    else{
      population2<- append(population2,i)
    }
    i = i + 1
  }
  
  # Crossover
  new_genes <- array(0,dim = c(50,2))
  i = 1
  j = 1
  while (i <= 40){
    new_genes[i,1] <- genes[population1[j],1]
    new_genes[i,2] <- genes[population2[j],2]
    new_genes[i+1,1] <- genes[population2[j],1]
    new_genes[i+1,2] <- genes[population1[j],2] 
    i = i + 2
    j = j + 1
  }
  
  # remaining 20% do not breed, add them as they are
  i = 41
  while (i <= 50){
    new_genes[i,1] <- genes[i,1]
    new_genes[i,2] <- genes[i,2]
    i = i + 2
  }
  
  # Randomly mutate 20% population 
  selection = sample(1:50, 10)
  
  i = 1
  while (i <= 10){
    random_eps = runif(1,-1,1)
    random_pts = sample(-1:1, 1)
    new_genes[selection[i],1] = new_genes[selection[i],1] + random_eps
    temp = new_genes[selection[i],2] + random_pts
    #print(length(temp))
    if (temp > 0){
      new_genes[selection[i],2] = temp
    }
    i = i + 1
  }
  
  genes = new_genes
  count = count + 1
}

#Print best values
print(values_eps)
print(values_ptr)
print(clus)
print(outs)

#One sample based on value gotten earlier
library("fpc")
set.seed(123)
db <- fpc::dbscan(clusterable_data, eps = 16.57435, MinPts = 6)

#Visualiztion
library("factoextra")
fviz_cluster(db, clusterable_data)
print(db)

#Calculate purity
print(purity(db$cluster,mydata$V3,TRUE))

