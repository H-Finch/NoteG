library(ggplot2)
library(tabplot)
library(sqldf)
library(plyr)
library(dplyr)
library(Amelia)
library(data.table)
library(rpivotTable)
library(cluster)
library(factoextra)
library(fpc)


data=read.csv("mccR.csv",header=T)
#data=data.table(data)
summary(data)
attach(data)
head(data)

clusters <- hclust(dist(data[, 7:14]),method='average')
plot(clusters)
rect.hclust(clusters,6,border="red")
clusterCut <- cutree(clusters, 6)
table(clusterCut, data$mcc)
###########
scaled_data = as.matrix(scale(data[,7:14]))
set.seed(123)


# Compute and plot wss for k = 2 to k = 15.
k.max <-7
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 8)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

kmm = kmeans(scaled_data,6)
kmm
fviz_cluster(kmm,scaled_data)#,palette = "Set2", ggtheme = theme_minimal())
clusplot(data, kmm$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
###
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(data,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")