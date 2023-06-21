##############################
## Manikanta Kalyan Gokavarapu
##
## created : 22/April/2023
## edited : 
##############################

rm(list = ls())
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 3 work_directory")

#a
genedata <- read.csv("Ch12Ex13.csv",header = FALSE)

genedata

#b
d <- dist(cor(genedata))

x11()
par(mfrow=c(2,2))
hc <- hclust(d, method = "ave")
plot(hc,hang=-1)
hc1 <- hclust(d, method = "complete")
plot(hc1,hang=-1)
hc2 <- hclust(d, method = "single")
plot(hc2,hang=-1)
hc3 <- hclust(d, method = "centroid")
plot(hc3,hang=-1)

ct <- cutree(hc, k = 2)
ct
ct1 <- cutree(hc1, k = 2)
ct1
ct2 <- cutree(hc2, k = 2)
ct2
ct3<- cutree(hc3, k = 2)
ct3

#From the dendrogram plots we can see that genes separate the samples into two distinct groups . 
#From the cutree results we can see that the first 20 tissue samples are assigned  one group and second 20 tissue samples assigned into another group.
#In most of the linkages the two groups are evident in some types of linkages like centroid they are not that clear and also the dendrograms of all the linkages are not that similar. so we can say that the results depend on linkage used.


#c
#To know which genes differ the most across two groups first we need to perform PCA on the genedata.
#Then we perform k-means clustering on the first two principal components and check if we have good separation between the data points.
# If there is good separation between the groups, we need to check which genes differ the most
# across the two groups. To do so we need to get each gene's weight, In order to do that we will look at the absolute values of the total lodgings for each gene.

set.seed(123)
fit1 <- prcomp(genedata)
PC1 <- fit1$x[,1]
PC2 <- fit1$x[,2]

# run k-means on the PC values
PC_data <- cbind(PC1, PC2)
k_means <- kmeans(PC_data, centers = 2)

# plot the groups
x11()
plot(PC_data, col = k_means$cluster, main = "k-means plot of PC_data")
points(k_means$centers, col = 1:2, pch = 8, cex= 2)
#From the k-means plots we can say there is a good separation between gene data.


# Below we perform PCA on gene expression data 
pca_transpose_data <- prcomp(t(genedata))
summary(pca_transpose_data)

#We will identify the top 15 genes with the highest contribution to the first principal component.
total_load <- apply(pca_transpose_data$rotation, 1, sum)
index = order(abs(total_load), decreasing = TRUE)
index[1:15]

#These are the 15 genes that differ the most across the two groups.