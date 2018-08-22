# load libraries
library(tidyverse); library(igraph); library(plyr)

# load data (sorry I can't share the raw data with you)
disputesRaw <- read.csv("/Users/jeffziegler/Dropbox/Research/data/disputes.csv", sep="\t", stringsAsFactors = F)

# create variables
disputesRaw <- disputesRaw[which(disputesRaw$treaty!="CHECK!"),]
disputesRaw$startDate <- gsub("\\ BIT.*","", disputesRaw$treaty)
disputesRaw$date <- gsub("[^0-9\\-]","", disputesRaw$date)
disputesRaw$countryA <- gsub("\\ - .*","", disputesRaw$startDate)
disputesRaw$countryA <- revalue(disputesRaw$countryA, c(" Czech and Slovak Federal Republic (Czech Republic and Slovak Republic)" = "Czechslovakia",
                                                        " Kyrgyz Republic (Kyrgyzstan)" = "Kyrgyzstan"))
disputesRaw$countryA <- gsub(" ","", disputesRaw$countryA)
disputesRaw$countryA <- revalue(disputesRaw$countryA, c("CzechRepublic" = "Czech Republic"))
disputesRaw <- disputesRaw[which(disputesRaw$countryA!=""),]
disputesRaw$countryB <- gsub(".* - ","", disputesRaw$startDate)
disputesRaw$countryB <- revalue(disputesRaw$countryB, c("Czechslovakia (Czech Republic and Slovak Republic)" = "Czechslovakia", 
                                                        "Czech and Slovak Federal Republic (Czech Republic and Slovak Republic)" = "Czechslovakia"))
detach("package:plyr", unload=TRUE)

disputesRaw$plantiff <- gsub("\\ v. .*","", disputesRaw$case); disputesRaw$plantiff <- gsub(".* - ","", disputesRaw$plantiff)

country1 <- disputesRaw %>%
  distinct(countryA) %>%
  rename(label = countryA)

country2 <- disputesRaw %>%
  distinct(countryB) %>%
  rename(label = countryB)

nodes <- full_join(country1, country2, by = "label")
nodes <- nodes %>% rowid_to_column("id")

per_route <- disputesRaw %>%  
  group_by(countryA, countryB) %>%
  summarise(weight = n()) %>% 
  ungroup()

edges <- per_route %>% 
  left_join(nodes, by = c("countryA" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("countryB" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)

BITs_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)

plot(BITs_igraph,
     #vertex.color = V(nigeriaPlot)$color, # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .75, # change size of labels to 75% of original size
     edge.curved=.5, # add a 25% curve to the edges
     edge.color="grey20",
     edge.arrow.size=1,                           # Arrow size, defaults to 1
     edge.arrow.width=.25,
     vertex.shape="circle", 
     label.pos=5, layout=norm_coords(layout_with_fr(BITs_igraph), ymin=-2, ymax=2, xmin=-2, xmax=2)*5) # change edge color to grey






     