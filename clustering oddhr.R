#Occupation
# Farmer	0
# Housewife	1
# Others	2
# Retired	3
# Salaried	4
# Self-Employed	5
# Student	6

# Emptype
# Government	0
# Multinational	1
# Not Applicable	2
# Others	3
# Partnership	4
# Private Limited	5
# Proprietorship	6
# Public Limited	7
# PSUs	8

#Salary 
# <50,000	0
# 50,000 - 1,00,000	1
# 1,00,000 - 3,00,000	2
# 3,00,000 - 5,00,000	3
# 5,00,000 - 7,50,000	4
# 7,50,000 - 10,00,000	5
# 10,00,000 - 15,00,000	6
# >15,00,000 	7
# 0	 8

# Risk
# LOW 1
# MEDIUM 2
# HIGH 3

library(plyr)
library(dplyr)
library(dplyr)
library(ggfortify)
library(cluster)
library(klaR)
library(ggplot2)
library(tsne)
library(Rtsne)
library(factoextra)
library(fpc)
library(rpivotTable)
library(corrplot)
data=read.delim("clustercc2.txt",header=T,sep='\t')
attach(data)
head(data)
d=data[c("age","cp","ap","tc","ta")]


corrplot(cor(d),order = "hclust", tl.col='black', tl.cex=.75,method="number") 
data$emp=factor(data$emp)
data$occ=factor(data$occ)
data$sal=factor(data$sal)
data$risk=factor(data$risk)
gower_dist=daisy(data[2:10],metric='gower')
gower_mat=as.matrix(gower_dist)
data[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
           arr.ind = TRUE)[1, ], ]
data[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
           arr.ind = TRUE)[1, ], ]

#library(cluster,factoextra)
#library(fpc)
#pamkClus = pamk(gower_dist, krange = 3:8, criterion="multiasw", ns=2, critout=TRUE)
pamkClus = pamk(gower_mat, krange = 3:8, criterion="multiasw", ns=2, critout=TRUE)
pam_fit = pam(gower_dist,diss=T,k=3)#optimal no of clusters
fviz_silhouette(silhouette(pam_fit$clustering,gower_dist))
pam_fit = pam(gower_dist,diss = TRUE,k = 4)
fviz_silhouette(silhouette(pam_fit$clustering,gower_dist))
pam_fit = pam(gower_dist,diss = TRUE,k = 5)
fviz_silhouette(silhouette(pam_fit$clustering,gower_dist))
pam_fit = pam(gower_dist,diss = TRUE,k = 6)
fviz_silhouette(silhouette(pam_fit$clustering,gower_dist))
pam_fit = pam(gower_dist,diss = TRUE,k = 7)
fviz_silhouette(silhouette(pam_fit$clustering,gower_dist))
pam_fit = pam(gower_dist,diss = TRUE,k = 8)
fviz_silhouette(silhouette(pam_fit$clustering,gower_dist))

#plot(pam_fit)
#plot(silhouette(pam_fit$clustering,gower_dist))


# Silhouette width of observation
sil = pam_fit$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index = which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]
#library(ggfortify)
#autoplot(pam_fit,frame=T,frame.type='norm')

pam_fit = pam(gower_mat,diss = F,k = 3)
fviz_cluster(pam_fit,palette = "Set2", ggtheme = theme_minimal())#also sans ellipse.type
#fviz_cluster(pam_fit,palette = "Set2", ggtheme = theme_minimal(),ellipse.type = 'none')#
#clusplot(pam_fit, main = "Cluster plot", color = TRUE)
#pam_fit = pam(gower_dist,diss = T,k = 3)

out=cbind(data,cluster=pam_fit$clustering)
head(out)
write.csv(out,"out.csv")

df=read.csv("out.csv",header=T)
head(df)
df=df[-1]
rpivotTable(df)
ggplot(df,aes(x=tc,y=age,fill=factor(risk)))+ geom_point(aes(colour = factor(risk)))
ggplot(df,aes(x=tc,y=age,fill=factor(cluster)))+ geom_point(aes(colour = factor(cluster)))


# Cluster 3-most voluminous, mostly low risk, mostly between 25-40,
#ALL salaried (most salaried people are in this cluster),
# mostly in pvt or PSU cos.Mostly earning btn 10-50L
#Usually transact just once, for very less amounts typically 10k or less

# Cluster 2-least voluminous,mostly low risk,mostly between 23-50,
#mostly self-employed,with no info on employer available(obviously).
#Earning anywhere btn 10L to 75L.
#Usually txn once,very less amounts typically 10k or less

# Cluster 1-only low risk, typically less than 25 years, students, 
#with no info on employer available(obviously).Earn nothing
#Usually txn once,very less amounts typically 10k or less

