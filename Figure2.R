#### Description: this script produces manuscript Figure2 and data used
#### in scripts "Figure3", "Figure4" and "Network_values"
#### INPUT: "data.xlsx" 
#### OUTPUT: "values_Figure3_and_clusters.csv" and manuscript Figure2
#### Date: 13/01/2020
#### Author: Iratxe Rubio
#######################################################################

library(readxl) #read_xlsx function
library(tidyverse)
library(magrittr) #for the %<>% operator
library(dendextend) #color_branches function
library(gplots) #heatmap.2 function
library(RColorBrewer) #brewer.pal function

#Open data
AC <- read_xlsx("data/data.xlsx", sheet = 1)
AC$AC_strategy <- as.factor(AC$AC_strategy)

AC_vars <- c("1.Infrastructure", #1
             "2.Credit/savings", #2
             "3.Public funding", #3
             "4.Well connect", #4
             "5.Participation", #5
             "6.Governance", #6
             "7.Income diverse", #7
             "8.Dependence", #8
             "9.Knowledge", #9
             "10.Learning", #10
             "11.Assessment", #11
             "12.Ability react" #12
)

levels(AC$AC_strategy) <- AC_vars

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
colnames(m) <- AC_vars

#Change class of columns
cols = c(1:component_num) 
m[,cols] %<>% lapply(function(x) as.numeric(as.character(x))) 
#The %<>% operator pipes and reassigns
#Now the list apply function is much easier to read, 
#by only specifying the function you wish to apply.

m <- as.matrix(m)
m_save <- m

#Create a names vector
num <- rep(1:component_max_value, n)
name <- rep("org", dim(m)[2])
nam <- paste(name, 1:n,  sep = "")
rownames(m) <- nam

#Clustering
meth <- "ward.D2"

row_dend = hclust(dist(m, "euclidean"), method = meth) # row clustering
col_dend = hclust(dist(t(m), "euclidean"), method = meth) # column clustering

cols_branches1 <- c("darkolivegreen3", "pink", "deepskyblue1", "grey")
cols_branches2 <- "black"

dend_r <- color_branches(row_dend, k = 4, col = cols_branches1)
dend_c <- color_branches(col_dend, k = 1, col = cols_branches2)

cols <- brewer.pal(4, "PuOr")

#Create manuscript Figure2
png("Figure2.png", 
    width = 5.5, height = 5, units = 'in', res = 600)
par(oma = c(4.5, 1, 0, 0))
heatmap.2(m, scale = "none", 
          col = cols,
          trace = "none", density.info = "none",
          na.rm = TRUE,
          Rowv = dend_r,  
          Colv = dend_c,
          na.color = "white",
          key.title = NA,
          key.xlab = "Importance value",
          rowsep = c(2, 4, 10),
          cexCol = 1.3,
          adjCol = c(NA,0.5),
          cexRow = 1.3,
          key.par=list(mar=c(4,0,1,2), cex=1.0, cex.lab=1.1, cex.axis=1.0), margins=c(5,5)
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

write.csv(m2[, -13], "data/values_Figure3_and_clusters.csv")