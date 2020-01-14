#### Description: this script produces manuscript Figure4 
#### INPUT: "data.xlsx" and "values_Figure3_and_clusters.csv" 
#### OUTPUT: manuscript Figure4
#### Date: 13/01/2020
#### Author: Iratxe Rubio
#######################################################################

library(igraph) #graph_from_data_frame function
library(tidyverse)
library(ggraph)
library(readxl) #read_xlsx function
source("function_normalize.R")

#Open data, reading sheets from excel
edges <- read_xlsx("data/data.xlsx", sheet = 2)
nodes <- read_xlsx("data/data.xlsx", sheet = 3)

data <- read.csv("data/values_Figure3_and_clusters.csv", check.names = F)[, c(1,14)] 
colnames(data)[1] <- "ID"

nodes <- left_join(nodes, data, by ="ID") #adding cluster groups variables to nodes

#Change names
nodes$group[nodes$group == "Governments"] <- "Government or RFMO"
nodes$group[nodes$group == "Scientists"] <- "Research"
nodes$group[nodes$group == "NGOs others"] <- "NGOs, others"

nodes$name[1] <- "Local gov"
nodes$name[5] <- "National gov"
nodes$name[14] <- "RFMO"
nodes$name[15] <- "Regional gov \n (UE)"
nodes$name[16] <- "Other regional gov \n (non-UE flag)"
nodes$name[17] <- "Other regional gov \n (public agr)"
nodes$name[18] <- "Other regional gov \n (private agr)"

#Create igraph network object
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = F)
  
my_color <- V(g)$cluster
  
names <- c(2:4,6:13,19:23)
V(g)$label <- V(g)$name # same as: get.vertex.attribute(g,"name")
V(g)[names]$label <- ""
  
shap <- c("Fishing industry", "Government or RFMO", "NGO, others", "Research")
my_shape <- shap[as.numeric(as.factor(V(g)$group))] 
  
#Network metric calculations
gs <- list()
gs[[1]] <- g

gs <- lapply(gs, function(x) {
  # Centrality measures
  V(x)$degree       <- degree(x, mode = "all") #degree centrality
  V(x)$betweenness  <- betweenness(x) #betweeness centrality
  E(x)$betweenness  <- edge.betweenness(x)
  
  # Normalized Centrality measures
  V(x)$n.degree <- normalize_fun(V(x)$degree)
  V(x)$n.betweenness <- normalize_fun(V(x)$betweenness)
  E(x)$n.edge_betweenness <- normalize_fun(E(x)$betweenness)
  return(x)
})

g <- gs[[1]]

V(g)$n.degree2[V(g)$n.degree < 0.25] <- "[0.00-0.25)"
V(g)$n.degree2[V(g)$n.degree >= 0.25 & V(g)$n.degree < 0.5] <- "[0.25-0.50)"
V(g)$n.degree2[V(g)$n.degree >= 0.5 & V(g)$n.degree < 0.75] <- "[0.50-0.75)"
V(g)$n.degree2[V(g)$n.degree >= 0.75] <- "[0.75-1.00]"

V(g)$n.betweenness2[V(g)$n.betweenness < 0.25] <- "[0.00-0.25)"
V(g)$n.betweenness2[V(g)$n.betweenness >= 0.25 & V(g)$n.betweenness < 0.5] <- "[0.25-0.50)"
V(g)$n.betweenness2[V(g)$n.betweenness >= 0.5 & V(g)$n.betweenness < 0.75] <- "[0.50-0.75)"
V(g)$n.betweenness2[V(g)$n.betweenness >= 0.75] <- "[0.75-1.00]"

#Create manuscript Figure4
png("Figure4.png", 
    width = 12, height = 7, units = 'in', res = 600)  
ggraph(g, layout = "kk") + 
  geom_edge_link0(aes(alpha = n.edge_betweenness, 
                      width = n.edge_betweenness, 
                      colour = n.edge_betweenness)) + 
  scale_edge_colour_gradient(low = "grey", high = "black") + 
  scale_edge_width(range = c(0.3, 1.2)) + 
  scale_edge_alpha(range = c(0.1, 1)) + 
  geom_node_point(aes(shape = my_shape,
                      fill = as.factor(my_color),
                      size = n.degree2), #
                  colour = "#000000", 
                  stroke = 0.3) + 
  scale_fill_manual(name = "Adaptive capacity group", 
                    values = c("grey", "deepskyblue1", "pink", "darkolivegreen3"),
                    guide=guide_legend(override.aes=list(shape = 21, 
                                                         size = 7,
                                                         color = "black"))) +
  geom_node_text(aes(label = label), colour = "#000000", 
                 size = 5, family = "Helvetica", 
                 nudge_x = 0.2, nudge_y = 0.43) + 
  theme_graph() +
  theme(legend.position = "left",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.spacing.y = unit(0, "cm")) +
  guides(
    edge_alpha = guide_legend("Edge betweenness"),
    edge_width = guide_legend("Edge betweenness"),
    edge_color = guide_legend("Edge betweenness"),
    shape = guide_legend(override.aes = list(size = 5), order = 1)) +
  scale_size_manual(name = "Node degree", values = c(2,4,10,15)) +
  scale_shape_manual(name = "Actor type", values = c(21,22,23,24))
dev.off()