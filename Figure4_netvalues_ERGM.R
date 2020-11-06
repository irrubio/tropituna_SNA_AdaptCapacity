#### Description: this script produces manuscript Figure4 and network
#### values (+ Table3)
#### INPUT: "data.xlsx" and "values_clusters.csv" 
#### OUTPUT: manuscript Figure4, network values, Table3
#### Date: 13/01/2020
#### Author: Iratxe Rubio
#######################################################################

library(igraph) #graph_from_data_frame function
library(tidyverse) #mutate, summarise, %>%... functions
library(ggraph) #ggraph function
library(readxl) #read_xlsx function

#0.Normalize function I use within the script
normalize_fun <- function(x) {
  a <- min(x)
  b <- max(x)
  (x - a)/(b - a)
}

#1.Open data####
edges <- read_xlsx("data/data.xlsx", sheet = 2)
nodes <- read_xlsx("data/data.xlsx", sheet = 3)

cluster <- read.csv("data/values_clusters.csv", check.names = F)[, c(1,14)]
colnames(cluster)[1] <- "ID"
cluster$cluster_name <- paste("AC Group", cluster$cluster)

nodes <- left_join(nodes, cluster, ID = "ID")

#2.Create igraph network object####
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)

#3.Calculate general network metrics####
#Average in-degree 
round(mean(degree(g, mode = "in")), 2)

#Average path length
round(mean_distance(g), 2)

#Density
round(edge_density(g, loops = F), 2)

#4.Calculate node level metrics, I add them to the attributes table####
#Degree centrality and betweenness centrality
resuls_vertex_betw <- as.data.frame(betweenness(g)) #betwenness centrality
colnames(resuls_vertex_betw) <- "betw_centrality"
resuls_vertex_betw$ID <- rownames(resuls_vertex_betw)
nodes <- merge(nodes, resuls_vertex_betw, by = "ID")

results_degree <- as.data.frame(degree(g, mode = "in")) #degree centrality
colnames(results_degree) <- "idegree_centrality"
results_degree$ID <- rownames(results_degree)
nodes <- merge(nodes, results_degree, by = "ID")

nodes <- nodes %>% 
  mutate(idegree_centrality.n = normalize_fun(idegree_centrality),
         betw_centrality.n = normalize_fun(betw_centrality))

##5.Calculate metrics by group (paper Table3)####
#By organization type
a <- nodes %>% 
      group_by(group) %>% 
      summarise(mean_idegree_centrality = round(mean(idegree_centrality.n), 2),
                mean_betw_centrality = round(mean(betw_centrality.n), 2),
                sd_idegree_centrality = round(sd(idegree_centrality.n), 2),
                sd_betw_centrality = round(sd(betw_centrality.n), 2))

#By adaptive capacity group
b <- nodes %>% 
      group_by(cluster_name) %>% 
      summarise(mean_idegree_centrality = round(mean(idegree_centrality.n), 2),
                mean_betw_centrality = round(mean(betw_centrality.n), 2),
                sd_idegree_centrality = round(sd(idegree_centrality.n), 2),
                sd_betw_centrality = round(sd(betw_centrality.n), 2))

rm(a, b, g, resuls_vertex_betw, results_degree)

#6.Network visualization####
#Change names
nodes$name <- NA
nodes$name[1] <- "Local gov"
nodes$name[5] <- "National gov"
nodes$name[14] <- "RFMO"
nodes$name[15] <- "Regional gov \n (UE)"
nodes$name[16] <- "Other regional gov \n (non-UE flag)"
nodes$name[17] <- "Other regional gov \n (public agr)"
nodes$name[18] <- "Other regional gov \n (private agr)"

#Network with new attributes
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)

#color, labels, shape
my_color <- V(g)$cluster
my_color[which(is.na(my_color) == T)] <- "NA"
my_color[which(my_color == 1)] <- "Group 1"
my_color[which(my_color == 2)] <- "Group 2"
my_color[which(my_color == 3)] <- "Group 3"
my_color[which(my_color == 4)] <- "Group 4"
names <- c(2:4,6:13,19:23)
V(g)$label <- V(g)$name # same as: get.vertex.attribute(g,"name")
V(g)[names]$label <- ""
shap <- c("Fishing industry", "Government or RFMO", "NGO, others", "Research")
my_shape <- shap[as.numeric(as.factor(V(g)$group))] 

#network metrics
V(g)$idegree_centrality.n2[V(g)$idegree_centrality.n < 0.25] <- "[0.00-0.25)"
V(g)$idegree_centrality.n2[V(g)$idegree_centrality.n >= 0.25 & V(g)$idegree_centrality.n < 0.5] <- "[0.25-0.50)"
V(g)$idegree_centrality.n2[V(g)$idegree_centrality.n >= 0.5 & V(g)$idegree_centrality.n < 0.75] <- "[0.50-0.75)"
V(g)$idegree_centrality.n2[V(g)$idegree_centrality.n >= 0.75] <- "[0.75-1.00]"

V(g)$betw_centrality.n2[V(g)$betw_centrality.n < 0.25] <- "[0.00-0.25)"
V(g)$betw_centrality.n2[V(g)$betw_centrality.n >= 0.25 & V(g)$betw_centrality.n < 0.5] <- "[0.25-0.50)"
V(g)$betw_centrality.n2[V(g)$betw_centrality.n >= 0.5 & V(g)$betw_centrality.n < 0.75] <- "[0.50-0.75)"
V(g)$betw_centrality.n2[V(g)$betw_centrality.n >= 0.75] <- "[0.75-1.00]"

E(g)$betweenness  <- edge.betweenness(g)
E(g)$n.edge_betweenness <- normalize_fun(E(g)$betweenness)

png("Figure4.png", 
    width = 12, height = 7, units = 'in', res = 300)  
#Plot (paper Figure4)
ggraph(g, layout = "kk") + 
  geom_node_point(aes(shape = my_shape,
                      fill = as.factor(my_color),
                      size = idegree_centrality.n2), #
                  colour = "#000000", 
                  stroke = 0.3) + 
  scale_fill_manual(name = "Adaptive capacity", 
                    values = c("grey", "deepskyblue1", "pink", "darkolivegreen3", "black"), #, "yellow"
                    guide=guide_legend(override.aes=list(shape = 21, 
                                                         size = 7,
                                                         color = "black"), order = 2)) +
  geom_node_text(aes(label = label), colour = "#000000",
                 size = 5, family = "Helvetica",
                 nudge_x = 0.2, nudge_y = 0.35) +
  geom_edge_link0(aes(alpha = n.edge_betweenness*1.2,
                      width = n.edge_betweenness*1.2), 
                  arrow = arrow(length = unit(10, "pt"), type = "closed")) + 
  scale_edge_colour_gradient(low = "grey", high = "black") + 
  scale_edge_width(range = c(0.3, 1.2)) + 
  scale_edge_alpha(range = c(0.1, 1)) + 
  theme_graph() +
  guides(
    shape = guide_legend(override.aes = list(size = 5), order = 1),
    edge_alpha = guide_legend("Edge betweenness", order = 3),
    edge_width = guide_legend("Edge betweenness", order = 3)) +
  scale_size_manual(name = "Node in-degree", values = c(2,4,10,15)) +
  scale_shape_manual(name = "Actor type", values = c(21,22,23,24)) +
  theme(legend.position = "left",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.spacing.y = unit(0, "cm")) 
dev.off()

#7.ERGM####
#data.frame for ERGM
network_attributes <- nodes
network_attributes$interview <- 1
network_attributes$interview[is.na(network_attributes$cluster) == T] <- 0
network_attributes$id <- paste("actor", 1:23, sep ="")
colnames(network_attributes)[2] <- "type"
colnames(network_attributes)[3] <- "ac.group"
network_attributes <- network_attributes[, c(2:3,10,11)]
network_attributes$id.type <- 1
network_attributes$id.type[network_attributes$type == "Government or RFMO"] <- 2
network_attributes$id.type[network_attributes$type == "NGOs, others"] <- 3
network_attributes$id.type[network_attributes$type == "Research"] <- 4

#create matrix

#Model