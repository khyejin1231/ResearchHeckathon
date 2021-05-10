#######################################################
#         Utility function: analyzing groups
# For each cluster, we create a table that shows the
# demographic characteristics of the cluster
# row: cluster
# column: attribute ratio per class, density
########################################################

#install.packages("pracma")
library(pracma)

Analysis <- function(lc,c_gender){
  
  #input: network object and attribute matrix
  #table size: m is #column, n is #row
  m <- 0
  l <- c()
  for (i in 1:ncol(c_gender)){
    
    l <- c(l, length(unique(c_gender[,i])))
    m <- m + length(unique(c_gender[,i]))
    
  }
  #This is the number of clusters
  n <- lc$numbers[3]
  
  #This will be the list that we need to reshape in the end. 
  #It contains ratio of each attribute for members in a cluster
  r <- c()
  
  #First for each cluster
  for (i in 1: n){
    index <- lc$nodeclusters[,1][which(lc$nodeclusters[,2] == i)]
    names <-c()
    #for each attribute
    for (j in 1:ncol(c_gender)){
      
      
      # then, for each level, we calculate the sum
      for (o in 1: l[j]){
        names <- c(names,o)
        r <- c(r, sum(c_gender[as.numeric(index),j] == o)/length(index))
        
      }
      
    }
  }
  #for loop: for i in k - so for all levels in an attribute
  result <- as.data.frame(Reshape(r,m))
  result <- format(result, digits = 2)
  result <- t(result)
  colnames(result) <- names
  return(result)
}


