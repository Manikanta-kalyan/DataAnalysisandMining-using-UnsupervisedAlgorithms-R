
##############################
## Manikanta Kalyan Gokavarapu
##
## created : 25/March/2023
## edited : 
##############################

rm(list = ls())
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 2 work_directory")

#install.packages("ggplot")
library("ggplot2")

#Read the data
data(iris)
dataset <- iris
head(dataset)

#a)
#PCA
fit <- prcomp(dataset[,1:4], center=TRUE, scale=TRUE)

# Extracting and combining first two principal components, PC1 and PC2 and combining with species columns.
PC1 <- fit$x[,1]
PC2 <- fit$x[,2] 
PC_dats <- cbind(PC1, PC2)
PC_dats_df <- as.data.frame(PC_dats)
PC_dats_df$Species <- dataset$Species


#Creating a plot with PC1 and PC2 and coloring the iris species by class.
x11()
ggplot(PC_dats_df, aes(x = PC1, y = PC2, color = Species)) +
  geom_point() +
  labs(x = "PC1", y = "PC2", title = "PC1 and PC2 plot of Iris dataset") +
  theme_minimal()


#b)
# run k-means on the PC values
set.seed(123)
km2 <- kmeans(PC_dats, centers = 3, nstart = 10)

x11()
# create a vector of symbols corresponding to each cluster
cluster_symbols <- c(2, 3, 4) # use different integers for each cluster

# plot the groups
plot(PC_dats, col = km2$cluster,pch = cluster_symbols[km2$cluster], main = "k-means clustering plot of PC1 & PC2" )
points(km2$centers, col = 1:3, pch = 8, cex= 2)

# add legend for symbols
legend("topright", legend = c("setosa", "vercicolor", "virginicia"), pch = cluster_symbols, col = 1:3)


#c)
rand.index(km2$cluster, as.numeric(iris$Species))
adj.rand.index(km2$cluster, as.numeric(iris$Species))

#d)
# gap statistics - k-means clustering
gap_kmeans <- clusGap(PC_dats, kmeans, nstart = 20, K.max = 10, B = 100)

x11()
plot(gap_kmeans, main = "Gap Statistic: k-means plot")


# silhouette plot - k-means clustering.
sil_width <- silhouette(km2$cluster, dist(PC_dats))

x11()
plot(sil_width, main = "Silhouette : k-means plot")

#e)
# From the rand index and adjusted rand index, we can infer that the actual species labels (iris$Species) are only slightly different from the k-means clustering solution labels (km2$cluster). 
# Approximately 83.22% of data point pairs have the same cluster assignments in both the predicted and actual clusters, according to the Rand Index, which stands at 0.8322148. 
# The agreement between the predicted and actual clusters is better than would be predicted by chance, according to the Adjusted Rand Index of 0.6201352, but there is still space for growth.
# From the "Gap Statistic: k-means plot" we could see that gap static reaches it's at 3 so the optimal number of clusters for iris data set is 3.
# From the "Silhouette : k-means plot" we can interpret that the cluster quality is good. For each cluster the bar chart is pretty similar and there no negative values, implying good quality clustering.
# and there is also nearly uniform no.of data points between the 3 clusters like 50, 53, 47 this implies that the clustering and structure is good for k=3.


