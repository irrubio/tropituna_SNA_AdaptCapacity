#### Description: this script produces manuscript Figure2 and data used
#### in scripts "Figure3", "Figure4" and "Network_values"
#### INPUT: "data.xlsx" 
#### OUTPUT: "values_clusters.csv" and manuscript Figure2
#### Date: 13/01/2020
#### Author: Iratxe Rubio
#######################################################################

library(readxl) #read_xlsx function
library(tidyverse)
library(magrittr) #for the %<>% operator
library(RColorBrewer) #brewer.pal function
library(ComplexHeatmap) #Heatmap function
library(dendextend) #color_branches function

#Open data
AC <- read_xlsx("data/data.xlsx", sheet = 1)
AC$AC_strategy <- as.factor(AC$AC_strategy)

#Calculate mean by organization
data <- AC %>%
  group_by(`ID`, AC_strategy) %>%
  summarise(mean = round(mean(AC_strategy_value), 1))

#Reshaping data
#R fill in a matrix with vector by column
component_num <- 12
n <- 13
component_max_value <- 1
data <- as.matrix(data)
m <- matrix(NA, nrow = component_num, ncol = n*component_max_value)
a <- 1

for(j in 1:dim(m)[2]){
  for(i in 1:dim(m)[1]){
    m[i,j] <- data[a,3]
    a <- a + 1
  }
}

m <- as.data.frame(t(m))
colnames(m) <- letters[1:12]#levels(AC$AC_strategy)

#Change class of columns
cols = c(1:component_num)
m[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
# #The %<>% operator pipes and reassigns
# #Now the list apply function is much easier to read, 
# #by only specifying the function you wish to apply.
m <- as.matrix(m)

#Create a names vector
nodes <- read_xlsx("data/data.xlsx", sheet = 3)[c(1:13),]
nam <- nodes$group
rownames(m) <- nam

#Clustering
cols <- brewer.pal(4, "PuOr")

meth <- "ward.D2"

row_dend = hclust(dist(m, "euclidean"), method = meth) # row clustering
col_dend = hclust(dist(t(m), "euclidean"), method = meth) # column clustering

cols_branches1 <- c("gray80","gray70","gray40", "gray0")#c("darkolivegreen3", "pink", "deepskyblue1", "grey")
cols_branches2 <- "black"

dend_r <- color_branches(row_dend, k = 4, col = cols_branches1)
dend_c <- color_branches(col_dend, k = 1, col = cols_branches2)

dend_r <- set(dend_r, "branches_lwd", 2)

m2 <- m
m2[m2 >= 1 & m2 <= 2] <- 1
m2[m2 > 2 & m2 <= 3] <- 2
m2[m2 > 3 & m2 <= 4] <- 3
m2[m2 > 4 & m2 <= 5] <- 4

#Create manuscript Figure2
png("Figure2.png", 
    width = 10, height = 5, units = 'in', res = 300)
Heatmap(m2, name = "mat", 
        cluster_columns = FALSE,
        cluster_rows = rev(dend_r), 
        row_split = 4, #OR 3
        col = cols,
        heatmap_legend_param = list(labels = c("Less important", 
                                               "Slightly important",
                                               "Important",
                                               "Very important"), 
                                    title = "Importance value",
                                    labels_gp = gpar(fontsize = 15),
                                    title_gp = gpar(fontsize = 15),
                                    grid_height = unit(6, "mm"),
                                    border = "white"),
        column_names_gp = gpar(fontsize = 14),
        row_names_gp = gpar(fontsize = 14),
        column_names_side = "top",
        row_names_side = "left",
        column_names_rot = 360, column_names_centered = T
)
dev.off()


#add cluster to the data and save it to posteriorly use the data in the network analysis
# Cut tree into 4 groups
sub_grp <- cutree(row_dend, k = 4)
names(sub_grp) <- c(1:n)
m2 <- as.data.frame(m)
m2 <- m2 %>%
        mutate(cluster = sub_grp)
colnames(m2)[13] <- "cluster_fig"
m2$cluster <- 1 #rename groups!!!!!!!!!! so as colors/groups are the same also for network
m2$cluster[m2$cluster_fig == 1] <- 3
m2$cluster[m2$cluster_fig == 3] <- 4
m2$cluster[m2$cluster_fig == 4] <- 2

write.csv(m2[, -13], "data/values_clusters.csv")