rm(list=ls())
# set working directory to Git location
setwd('/Users/jeffziegler/Documents/Git/')
# load libraries
#slibrary(tidyverse); library(igraph); library(plyr)
# load data
load("network2018_hw1/nigeria.rda")

#############################
# (1) Nigeria Data Processing
#############################

# create variable for row and column length of adjacency matrix to fill
rowLength <- length(unique(nigeria$sender))
colLength <- length(unique(nigeria$receiver))
# create adjacency matrix of sender and receiver, filled w/ zeroes
nigeriaAdjMat <- matrix(0, nrow=rowLength, ncol=colLength)
# adjust row and column names for sender and receiver
rownames(nigeriaAdjMat) <- unique(nigeria$sender)
colnames(nigeriaAdjMat) <- unique(nigeria$receiver)
# fill in adjacency matrix per year (i)
# start by sorting all unique years to iterate over
nigeriaAdjMatYearlyList  <- lapply(sort(unique(nigeria$year)), function(i){
  # find just those pairings for (1) a given year
  currentYear <- nigeria[nigeria$year == i,]
  # and (2) had a conflict (conflict == 1)
  yearlyConflicts <- currentYear[currentYear$conflict == 1,]
  # now that we know which pairings had conflicts
  # fill in a "1" based on sender and receiver
  for(i in 1:nrow(yearlyConflicts)){
    nigeriaAdjMat[as.character(yearlyConflicts[i,]$sender), 
                  as.character(yearlyConflicts[i,]$receiver)] <- 1
  }
  # return the adjacency matrix, which will be placed in a list
  return(nigeriaAdjMat)
})
# collapse all the matrices in list into one matrix
# since instructions are to "summarize the interactions 
# across all time periods into a single matrix"
nigeriaAdjMatTotalList <- Reduce('+', nigeriaAdjMatYearlyList)

##########################################
# (2) Measurements and Community Detection
##########################################

# (a) We want to discern who the most "influential" 
# actor in the network is.
# We'll create two measures of "influence": 
# (1) degree (number of connections)
# (2) eigenvector centrality (connections to high-scoring nodes 
# contribute more to the score of the node)

# create graph object from igraph package
library(igraph)
nigeriaGraph <- graph_from_adjacency_matrix(nigeriaAdjMatTotalList, 
          mode='directed', weighted=TRUE, diag=FALSE)

# (1) degree
head(sort(degree(nigeriaGraph), decreasing=T))

# interestingly, the Police (Nigeria) and the Military (Nigeria) 
# are two of the top 3 most engaged actors (Fulani Militia is #2)

# (2) eigenvector centrality (connections to high-scoring nodes 
head(sort(eigen_centrality(nigeriaGraph, directed = TRUE)$vector,
          decreasing=T))

# again, the police and military are not only more involved in conflicts
# but they engage w/ other highly conflicted actors

# (b)