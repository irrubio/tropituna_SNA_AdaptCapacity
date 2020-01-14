#### Description: this script produces manuscript Figure3 
#### INPUT: "values_Figure3_and_clusters.csv" 
#### OUTPUT: manuscript Figure3
#### Date: 13/01/2020
#### Author: Iratxe Rubio
#######################################################################

library(readxl) #read_xlsx function
library(ggplot2)
library(reshape2) #melt function
library(RColorBrewer) #brewer.pal function

#Open data
data <- read.csv("data/values_Figure3_and_clusters.csv", check.names = F) 
colnames(data)[1] <- "ID"

#Reshape data
df <- melt(data, id = c("ID","cluster"))
colnames(df)[3] <- "strategy"

df$domain <- NA
df$domain[df$strategy %in% c("1.Infrastructure",
                           "2.Credit/savings",
                           "3.Public funding")] <- "Assets"

df$domain[df$strategy %in% c("4.Well connect",
                             "5.Participation",
                             "6.Governance")] <- "Organization"

df$domain[df$strategy %in% c("7.Income diverse",
                             "8.Dependence")] <- "Flexibility"

df$domain[df$strategy %in% c("9.Knowledge",
                             "10.Learning",
                             "11.Assessment")] <- "Learning"

df$domain[df$strategy %in% c("12.Ability react")] <- "Agency"

#Calculate means
df <- df %>%
        group_by(domain,cluster) %>%
        summarise(value = mean(value))

colnames(df)[2] <- "Group"
for (i in 1:4) {
 df$Group[df$Group == i] <- paste("Group", i) 
}

#Conversion to categorical variable
df$brea <- cut(df$value, breaks = c(1,2,3,4,5))
df$brea2 <- NA
df$brea2[df$brea == "(1,2]"] <- "Less important"
df$brea2[df$brea == "(2,3]"] <- "Slightly important"
df$brea2[df$brea == "(3,4]"] <- "Important"
df$brea2[df$brea == "(4,5]"] <- "Very important"

col <- brewer.pal(n = 4, name = 'PuOr')

#Create manuscript Figure3
png("Figure3.png", 
    width = 11, height = 4, units = 'in', res = 600)

ggplot(df, aes(domain, value, fill = brea2))+
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("Less important" = col[1],
                               "Slightly important" = col[2],
                               "Important" = col[3],
                               "Very important" = col[4]),
                    breaks = c("Less important", "Slightly important", "Important",
                               "Very important"))+
  ylab("") +
  xlab("") +
  facet_wrap(~ Group, nrow = 1) + 
  theme_bw() +
  coord_cartesian(ylim = c(1, 5)) +
  guides(fill = guide_legend(title = "Importance value")) + 
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key.height = unit(0.7, "cm"),
        axis.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        strip.text = element_text(size = 15, face = "bold"))
dev.off()