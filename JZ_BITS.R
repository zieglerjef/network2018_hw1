########### 
# load data
########### 

# load libraries
library(tidyverse); library(igraph); library(plyr); library(statnet)

# load data
disputesRaw <- read.csv("/Users/jeffziegler/Dropbox/Research/data/disputes.csv", sep="\t", stringsAsFactors = F)

# create variables
disputesRaw <- disputesRaw[which(disputesRaw$treaty!="CHECK!"),]
disputesRaw$startDate <- gsub("\\ BIT.*","", disputesRaw$treaty)
disputesRaw$date <- gsub("[^0-9\\-]","", disputesRaw$date)
disputesRaw$year <- gsub("\\-.*","", as.character(disputesRaw$date))
disputesRaw$countryA <- gsub("\\ - .*","", disputesRaw$startDate)
disputesRaw$countryA <- trimws(disputesRaw$countryA, "l")
disputesRaw$countryA <- revalue(disputesRaw$countryA, c("Czech and Slovak Federal Republic (Czech Republic and Slovak Republic)" = "Czechslovakia",
                                                        "Kyrgyz Republic (Kyrgyzstan)" = "Kyrgyzstan",
                                                        "USSR (Russian Federation)" = "Russian Federation"))
#disputesRaw$countryA <- gsub(" ","", disputesRaw$countryA)
disputesRaw$countryA <- revalue(disputesRaw$countryA, c("Czech Republic" = "Czechslovakia"))
disputesRaw <- disputesRaw[which(disputesRaw$countryA!=""),]
disputesRaw$countryB <- gsub(".* - ","", disputesRaw$startDate)
disputesRaw$countryB <- revalue(disputesRaw$countryB, c("Czechslovakia (Czech Republic and Slovak Republic)" = "Czechslovakia", 
                                                        "Czech and Slovak Federal Republic (Czech Republic and Slovak Republic)" = "Czechslovakia",
                                                        "USSR (Russian Federation)" = "Russian Federation"))

disputesRaw$plantiff <- gsub("\\ v. .*","", disputesRaw$case); disputesRaw$plantiff <- gsub(".* - ","", disputesRaw$plantiff)
# need to create a whole vector to fill for adjacency matrix
disputesRaw$violation <- 1

########################
# Begin network analysis
########################

# (a) Starting here after I cleaned the data
# (sorry I can't share the raw data with you)

# (b)
# create variable for row and column length of adjacency matrix to fill
# 251 unique plantiffs
# 52 violating countries

# so we'll just look at violations that occur within treaties
# actors both are countries
# undirected
# get all possible countries
countries <- sort(unique(c(unique(disputesRaw$countryA), 
                      unique(disputesRaw$countryB))))
rowLength <- length(countries); colLength <- rowLength
# create adjacency matrix of sender and receiver, filled w/ NAs
BITadjMat <- matrix(0, nrow=rowLength, ncol=colLength)
# adjust row and column names for sender and receiver
rownames(BITadjMat) <- countries; colnames(BITadjMat) <- countries
# fill in adjacency matrix per year (i)
# start by sorting all unique years to iterate over
BITadjMatYearlyList  <- lapply(sort(unique(disputesRaw$year)), function(i){
  # find just those pairings for (1) a given year
  currentYear <- disputesRaw[disputesRaw$year == i,]
  # and (2) had a violation (violation == 1)
  yearlyConflicts <- currentYear[currentYear$violation == 1,]
  # now that we know which pairings had conflicts
  # fill in a "1" based on sender and receiver
  for(i in 1:nrow(yearlyConflicts)){
    BITadjMat[as.character(yearlyConflicts[i,]$countryB), 
                  as.character(yearlyConflicts[i,]$countryA)] <- 1
  }
  # return the adjacency matrix, which will be placed in a list
  return(as.network.matrix(BITadjMat, directed=T))
})
# collapse and sum across years
#BITadjMatTotalMatrix <- Reduce('+', BITadjMatYearlyList)

# (c-e)

# show biggest "suers" (origin country of plantiff)
head(sort(rowSums(BITadjMatTotalMatrix), decreasing = T))
# show biggest violators (countries that pass potentiall expropriating policies)
head(sort(colSums(BITadjMatTotalMatrix), decreasing = T))

# create network object using igraph
yGraph = igraph::graph.adjacency(BITadjMatTotalMatrix, 
                                 mode='directed',
                                 weighted=TRUE,
                                 diag=FALSE
)

# very little violations in general
# check proportion of present edges from all possible edges in the network.
ecount(yGraph)/(vcount(yGraph)*(vcount(yGraph)-1)) #for a directed network

# doesn't seem like passing expropriating policies
# leads to other countries to sue, and then pass expropriating policies
reciprocity(yGraph)

# add democratic information
polity <- read.csv("/Users/jeffziegler/Dropbox/Research/data/polity.csv", sep="\t", stringsAsFactors = F)
countries <- data.frame("country"=countries)
polityScores <- merge(countries, polity[c("country", "polity2")], all.x=T)
polityAvgs <-aggregate(polityScores[,2], list(polityScores$country), mean,na.rm=TRUE)
names(polityAvgs) <- c("country", "polityAvg")
polityAvgs$demoGroup <- ifelse(polityAvgs$polityAvg >=6, "blue", 
                               ifelse(6 < polityAvgs$polityAvg | polityAvgs$polityAvg >=-5, "grey", "red"))
# add categories to iGraph object
V(yGraph)$demoGroup <- polityAvgs$demoGroup

# convert to network graph object using
yNet = intergraph::asNetwork(yGraph)
yNet %v% "col" <- brewer.pal(3, "Dark2")[yNet %v% "demoGroup"]

# run latent distance model
y.var<-sd(c(BITadjMatTotalMatrix), na.rm=TRUE)
if(!file.exists('bitLDM.rda')){
  lsEucl = ergmm(yNet ~ euclidean(d=2), 
                 family='normal',
                 fam.par=list(
                   prior.var=4*sd(c(BITadjMatTotalMatrix), na.rm=TRUE),
                   prior.var.df=2 # certainty of the prior, higher more certain
                 ) )
  save(lsEucl, file='bitLDM.rda') }
load('bitLDM.rda')

# evaluate convergence
mcmc.diagnostics( lsEucl )
# could probably let the chain run a little longer
# visualize results
par(mfrow=c(1,1))
# jitter the points
zPosJitter = zPos+matrix(rnorm(length(zPos),0,.03),ncol=2)
pdf("/Users/jeffziegler/Documents/Git/network2018_hw1/figure3.pdf")
plot(yGraph, 
     layout=zPosJitter,
     vertex.color=V(yGraph)$demoGroup, 
     vertex.label.color='black',
     vertex.size=V(yGraph)$size,
     vertex.label.cex =.75,
     edge.color='grey',
     edge.width=E(yGraph)$weight,
     edge.arrow.size=.2,
     asp=FALSE
)
dev.off()
     