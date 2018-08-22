rm(list=ls())
# set working directory to Git location
setwd('/Users/jeffziegler/Documents/Git/')
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
nigeriaAdjMatTotalMatrix <- Reduce('+', nigeriaAdjMatYearlyList)

##########################################
# (2) Measurements and Community Detection
##########################################

# (a) We want to discern who the most "influential" actor in the network is
# We'll create two measures of "influence": 
# (1) degree (number of connections)
# (2) eigenvector centrality (connections to high-scoring nodes 
# contribute more to the score of the node)

# create graph object from igraph package
library(igraph)
igeriaGraph <- graph_from_adjacency_matrix(nigeriaAdjMatTotalMatrix, 
          mode='directed', weighted=TRUE, diag=FALSE)

# (1) degree
head(sort(degree(nigeriaGraph), decreasing=T))

# interestingly, the Police (Nigeria) and the Military (Nigeria) 
# are two of the top 3 most engaged actors (Fulani Militia is #2)

# (2) eigenvector centrality 
head(sort(eigen_centrality(nigeriaGraph, directed = TRUE)$vector,
          decreasing=T))

# again, the police and military are not only more involved in conflicts
# but they engage w/ other highly conflicted actors

# (b) Instruction: Run blockmodel with varying levels of k
# Tasks/traits for blockmodel function (each run needs to):
# [1] Save the node classifications
# [2] Conduct out-of-sample CV (10 folds)
# [3] Report the AUC (ROC) and AUC (PR) statistics

# first, recreate matrices so that they are network objects
library(network)
nigeriaAdjMatNetworkList  <- lapply(sort(unique(nigeria$year)), function(i){
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
  return(as.network.matrix(nigeriaAdjMat))
})

# create function that will do tasks 1-3
# then we can run CV function for varying levels of k
# Arguments:
# (remember function takes in igraph object)
# nFolds = number of folds (default = 10)
# nClusters = number of cluster (default = 2)
library(sna); library(caret); library(networkDynamic)
library(devtools)
install_github("leifeld/btergm", dependencies=TRUE)
library(btergm)
crossValidateFunc <- function(networkData, nFolds=10, nClusters=2) {
  # set seed for reproducibility
  set.seed(5)
  # createFolds function from caret package
  # argument gives a list of the indicies in each fold
  # from the groups that comprise all possible conflicts
  # return training data
  cvFolds <- createFolds(y = unique(nigeria$sender),
                       k=nFolds, returnTrain = T)
  # create empty vectors to fill w/ goodness-of-fit stats
  # from TERGMS (AUC (ROC) and AUC (PR))
  # ROC and PR curves can be used to compare different model specifications, 
  # also for within-sample goodness-of-fit
  AUC_ROC <- NULL; AUC_PR <- NULL
  # iterate over folds
  for (i in 1:nFolds) {
    # transform input list into network list
    networkList <- networkDynamic(network.list=networkData)
    # remove the necessary observations that are exempt from each fold
    delete.vertices(networkList, (1:dim(nigeriaAdjMat)[1])[-cvFolds[[i]]])
    # create clusters from structural equivalence
    equivNetClusters <- equiv.clust(networkList)
    # perform blockmodel
    blockModel <- blockmodel(networkList, equivNetClusters, k=nClusters)
    # take info that pertains to which block actors are placed in
    groupMembership <- blockModel$block.membership[blockModel$order.vec]
    # assign the block group values from the model back in the networkList
    networkList%v%"member" <- groupMembership
    # now run the out-of-sample prediction with TERGMs
    outSampleTERGM <- btergm(as.network.networkDynamic(networkList) ~ edges + 
                  gwesp(.5, fixed = TRUE) + nodecov("member"))
    # now, simulate 100 networks from the model w/ rocpr
    # to condense the performance into a single measure, the area under
    # the curve (AUC) can be reported for both curves. 
    goodFitStats <- gof(outSampleTERGM, statistics = rocpr, nsim = 100)
    # for each iteration/fold, remove and store statistics to existing list
    AUC_ROC <- c(AUC_ROC, goodFitStats$`Tie prediction`$auc.roc)
    AUC_PR <- c(AUC_PR, goodFitStats$`Tie prediction`$auc.pr)
  }
  # return the mean of each statistic pooled over the folds
  return(list(avgAUC_ROC=mean(AUC_ROC), avgAUC_PR=mean(AUC_PR)))
}
