#### Description: this script produces network values (and Table3)
#### INPUT: "data.xlsx" and "values_Figure3_and_clusters.csv" 
#### OUTPUT: manuscript network values and Table 3
#### Date: 13/01/2020
#### Author: Iratxe Rubio
#######################################################################

library(igraph) #graph_from_data_frame function
library(tidyverse)
library(readxl) #read_xlsx function
#library(devtools)
#install_github("mbojan/isnar")
library(isnar) #ei index
source("function_normalize.R")

#Open data, reading sheets from excel
edges <- read_xlsx("data/data.xlsx", sheet = 2)
nodes <- read_xlsx("data/data.xlsx", sheet = 3)

data <- read.csv("data/values_Figure3_and_clusters.csv", check.names = F)[, c(1,14)] 
colnames(data)[1] <- "ID"

nodes <- left_join(nodes, data, by ="ID") #adding cluster groups to nodes

#Add AC group name
#Add node "column"/characteristic for AC group name
nodes$cluster_name <- paste("AC group", nodes$cluster)
nodes$cluster_name[14:23] <- NA

#Create igraph network object
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = F)

#Calculate network metrics
#Average degree 
round(mean(degree(g)), 2)

#Average path length
round(mean_distance(g), 2)

#EI index
round(ei(g, "cluster"), 2)
round(ei(g, "group"), 2)
  
#Node level
#Degree centrality and betweenness centrality
resuls_vertex_betw <- as.data.frame(betweenness(g)) #betwenness centrality
colnames(resuls_vertex_betw) <- "betw_centrality"
resuls_vertex_betw$ID <- rownames(resuls_vertex_betw)
results_groups <- merge(resuls_vertex_betw, nodes, by = "ID")

results_degree <- as.data.frame(degree(g, mode = "all")) #degree centrality, wchich nodes are most connected
colnames(results_degree) <- "degree_centrality"
results_degree$ID <- rownames(results_degree)
results_groups <- merge(results_degree, results_groups, by = "ID")

results_groups <- results_groups %>%
                      mutate(degree_centrality.n = normalize_fun(degree_centrality),
                             betwen_centrality.n = normalize_fun(betw_centrality))
  
#By organization type
results_groups %>% 
    group_by(group) %>% 
    summarise(mean_degree_centrality = round(mean(degree_centrality.n), 2),
              mean_betwen_centrality = round(mean(betwen_centrality.n), 2))
  
#By adaptive capacity group
results_groups %>% 
    group_by(cluster_name) %>% 
    summarise(mean_degree_centrality = round(mean(degree_centrality.n), 2),
              mean_betwen_centrality = round(mean(betwen_centrality.n), 2))