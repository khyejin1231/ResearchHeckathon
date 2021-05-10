#install.packages("tnet")
#install.packages("linkcomm")
install.packages("RColorBrewer")
library(linkcomm)
library(tnet)
library(RColorBrewer)

c_info <- Cross.Parker.Consulting.net.info #Please indicate how often you have 
# turned to this person for information or advice on work-related topics in the past three months
c_value <- Cross.Parker.Consulting.net.value #For each person in the list below, 
# please show how strongly you agree or disagree with the following statement:
# In general, this person has expertise in areas that are important in the kind of work I do
c_gender <- Cross.Parker.Consulting.node.gender 
c_location <- Cross.Parker.Consulting.node.location
c_orglevel <- Cross.Parker.Consulting.node.orglevel
c_region <- Cross.Parker.Consulting.node.region
m_aware <- Cross.Parker.Manufacturing.net.aware #I understand this person's knowledge and skills.
#This does not necessarily mean that I have these skills or am knowledgable in these domains
# but that I understand what skills this person has and domains they are knowledgable in.
m_info <- Cross.Parker.Manufacturing.net.info #Please indicate the extent to which the people listed below
# provide you with information you use to accomplish your work
m_location <- Cross.Parker.Manufacturing.node.location
m_orglevel <- Cross.Parker.Manufacturing.node.orglevel
m_tenure <- Cross.Parker.Manufacturing.node.tenure

###############################################################################
# Research Question: Which demographic characteristics matter in how others
# ask for your expertise in workplace?
#
# a. consulting
# b. manufacturing
###############################################################################

#############################a. node based vs edge based - node based could be better
#### First analyze c_info
###### Justify the hcmethod
#### Second analyze c_value
#### compare correlation between c_info and c_value
#### Look at demographic characteristics
#### Look at weighting

?getLinkCommunities # "ward", "single",'complete','average','mcquitty','median','centroid'
?getClusterRelatedness
?plotLinkCommDend
?linkcomm2cytoscape
?LinkDensities
?plotLinkCommMembers

?vlabel.color
c_info[,3][which(c_info[,3]<2)] = 0
c_info[,3][which(c_info[,3]>0)] = c_info[,3][which(c_info[,3]>0)] - 2
lc <- getLinkCommunities(c_info, directed = TRUE, hcmethod = "single")

#getCommunityMatrix(lc)
lc$clustsizes

?graph.feature
?getEdgesIn
?set.vertex.attribute
#getEdgesIn(lc) #highlight node 1

ef <- graph.feature(lc, type = "edges",
      indices = getEdgesIn(lc, clusterids = 5), features = 0.1, default = 2, TRUE)
nf <- graph.feature(lc, type = "nodes",
      indices = getNodesIn(lc, clusterids = 5), features = 0.1, default = 1, TRUE)
plot(lc, type = "graph", vsize = 0.1, ewidth = 0.1, vshape = "circle", vlabel = TRUE,
     vlabel.color = 'black')

#structure
getClusterRelatedness(lc)

#density
LinkDensities(lc)

#correlation
corLinkcommCentrality(lc)


#important nodes: multiple membership
plotLinkCommMembers(lc, nodes = (names(lc$numclusters)))


#Analysis
m_c <- cbind(c_gender,c_location,c_orglevel, c_region)
c_result <- Analysis(lc,m_c)

#############################b.manufacturing
m_info[,3][which(m_info[,3]<2)] = 0
m_info[,3][which(m_info[,3]>0)] = m_info[,3][which(m_info[,3]>0)] - 2
lm <- getLinkCommunities(m_info, directed = TRUE, hcmethod = "single")

plot(lm, type = "graph", vsize = 0.01, ewidth = 0.1, vshape = "circle", vlabel = TRUE,
     vlabel.color = 'black')

getClusterRelatedness(lm)
LinkDensities(lm)
corLinkcommCentrality(lm)

plotLinkCommMembers(lm, nodes = (names(lm$numclusters)))

m_m <- cbind(m_location,m_orglevel, m_tenure)
m_result <- Analysis(lm,m_m)

#############################c. network, igraph, linkcomm, directedClustering