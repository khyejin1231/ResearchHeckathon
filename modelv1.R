consulting_info <- read.csv("Cross_Parker_Consulting_info.txt", sep = " ",skip = 4)
consulting_value <- read.csv("Cross_Parker_Consulting_value.txt", sep = " ", skip = 4)
Manufacturing_aware <- read.csv("Cross_Parker_Manufacturing_aware.txt", sep = " ", skip = 4)
Manufacturing_info <- read.csv("Cross_Parker_Manufacturing_info.txt", sep = " ", skip = 4)


#install.packages("tnet")
#install.packages("linkcomm")
library(linkcomm)
library(tnet)
?getLinkCommunities
consulting_info[,1] <- as.character(consulting_info[,1])
consulting_info[,2] <- as.character(consulting_info[,2])  

Cross.Parker.Consulting.node.gender
Cross.Parker.Consulting.node.location

lcon_info <- getLinkCommunities(Manufacturing_info, directed = TRUE, hcmethod = "single")

?getLinkCommunities
?graph.feature()
?getEdgesIn
getEdgesIn(lcon_info, nodes = 1) #highlight node 1

lcon_info$igraph

ef <- graph.feature(lcon_info, type = "edges", 
      indices = getEdgesIn(lcon_info, clusterids = 1), features = 1)
nf <- graph.feature(lcon_info, type = "nodes",
      indices = getEdgesIn(lcon_info, clusterids = 1), features = 1)
plot(lcon_info, type = "graph", vsize =nf, ewidth = ef, vshape = "circle", vlabel = FALSE)
