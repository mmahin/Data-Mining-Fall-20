# Purity Function

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

