##############################
## Manikanta Kalyan Gokavarapu
##
## created : 18/March/2023
## edited : 
##############################

rm(list = ls())
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 2 work_directory")


# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.16")
# BiocManager::install("multtest")
# BiocManager::install("cluster")
# install.packages("fpc")
# install.packages("bootcluster")
# library("multtest")
# library("fpc")
# library("cluster")
# library("bootcluster")
# library("fossil")
# pkgs <- c("factoextra",  "NbClust")
# install.packages(pkgs)

# Read in the  US department of Commerce, Bureau of the Census data
library(datasets)
data(state)
dats <- state.x77
head(dats)


dats_scale <- scale(dats)


# a)
d <- dist(dats_scale)
dim(as.matrix(d))
hc <- hclust(d,  method = "complete")
x11()
plot(hc, hang = -1,cex=0.6)



#b)

# calculate within-cluster sum of squares (WCSS) for different values of k
wcss <- c()
for (i in 1:25) {
  kmeans_fit <- kmeans(dats_scale, centers=i, nstart=25)
  wcss[i] <- kmeans_fit$tot.withinss
}

# plot elbow curve
x11()
plot(1:25, wcss, type="b", pch=19, frame=FALSE, 
     xlab="Number of clusters (k)", ylab="WCSS")



# Check the silhouette plots

ct2 <- cutree(hc, k = 2)
si2 <- silhouette(ct2, dist = d)
x11()
plot(si2)

ct3 <- cutree(hc, k = 3)
si3 <- silhouette(ct3, dist = d)
x11()
plot(si3)

ct4 <- cutree(hc, k = 4)
si4 <- silhouette(ct4, dist = d)
x11()
plot(si4)


ct5 <- cutree(hc, k = 5)
si5 <- silhouette(ct5, dist = d)
x11()
plot(si5)

ct6 <- cutree(hc, k = 6)
si6 <- silhouette(ct6, dist = d)
x11()
plot(si6)

ct7 <- cutree(hc, k = 7)
si7 <- silhouette(ct7, dist = d)
x11()
plot(si7)


#Considering k = 3 from the elbow plot and the silhouette plots output.

#Run k-means on the PC values
set.seed(123)
km3 <- kmeans(dats_scale, centers = 3, nstart = 10)

# plot the groups
x11()
plot(dats_scale, col = km3$cluster, main = "k-means clustering state dataset" )
points(km3$centers, col = 1:3, pch = 8, cex= 2)




# Hierarchical clustering may be more appropriate if the goal is to identify hierarchical relationships among the states
# While k-means clustering may be more appropriate if the goal is to identify distinct groups or cluster of states based on their socioeconomic characteristics.
# From the dendrogram we can clearly observe the similarities between different states and cluster formation hierarchy but it's hard to find the optimal number of clusters as there so many clusters in the dendrogram and in K-means it is very easy to identify clusters plus it is computationally inexpensive. but in sillhoutte plots we can see that k-means getting effected with outliers. we can just avoid this my removing outliers while preprocessing the data.

-------------------------------
  
  # load data
  data(state)
df <- as.data.frame(state.x77)

# select columns
df <- df[, c("Population", "Income", "Illiteracy", "Life Exp", "Murder", "HS Grad", "Frost", "Area")]

# standardize data
df_std <- scale(df)

# hierarchical clustering
hc <- hclust(dist(df_std),method="ave")

# plot dendrogram
x11()
plot(hc, hang = -1)
---------------------------------------

  