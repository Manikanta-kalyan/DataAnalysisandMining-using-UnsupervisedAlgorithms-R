##############################
## Manikanta Kalyan Gokavarapu
##
## created : 22/April/2023
## edited : 
##############################

rm(list = ls())
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 3 work_directory")

library(datasets)
library("cluster")
library("multtest")
library("fpc")
library("bootcluster")
library("fossil")
library(ggplot2)

install.packages("gRbase")
install.packages("CRAN")

library(gRbase)
library(graph)

#a
set.seed(123)
data <- matrix(c(rnorm(20 * 50, mean = 1),               
                          rnorm(20 * 50, mean = 2),
                          rnorm(20 * 50, mean = 3)), ncol = 50, byrow = TRUE)
true_labels <- unlist(lapply(1:3,function(x){rep(x,20)}))

#write.csv(data, file = "mydata.csv")


#b
set.seed(123)
fit <- prcomp(data)
PC1 <- fit$x[,1]
PC2 <- fit$x[,2]
plot(PC1, PC2, col=true_labels)
#there is good separation between the three classes.

#c
set.seed(123)
km <- kmeans(data, 3, nstart = 60)
table(true_labels,km$cluster)

# Calculate ARI
adj.rand.index(true_labels,km$cluster)


# create a data frame with the PC1 and PC2 scores and cluster labels
df <- data.frame(PC1 = PC1, PC2 = PC2, Cluster = as.factor(km$cluster))

# plot the data points colored by the cluster labels
ggplot(df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(color = "Cluster") +
  theme_minimal()

#As adjusted rand index =1, we can say there is a perfect agreement between the true class labels and clustering labels we got from k-means.
#From table data we can see that the data is perfectly clustered and each cluster has 20 observations.
#you can see k-means plot when k=3 the cluster labels are perfectly assigned to data points.

#d
set.seed(123)
km2 <- kmeans(data, 2, nstart = 60)
table(true_labels,km2$cluster)

# Calculate ARI
adj.rand.index(true_labels,km2$cluster)


# create a data frame with the PC1 and PC2 scores and cluster labels
df <- data.frame(PC1 = PC1, PC2 = PC2, Cluster = as.factor(km2$cluster))

# plot the data points colored by the cluster labels
ggplot(df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(color = "Cluster") +
  theme_minimal()

#The adjusted rand index came down to 0.56 when we only take 2 clusters because 20 observation are assigned to one cluster and remaining 40 observations are assigned to other cluster in K-means.
#so there is an increase in no.of observations in a single cluster  because of having only two clusters.
#You can result of clustering in k-means plot above.

#e
set.seed(123)
km3 <- kmeans(data, 4, nstart = 60)
table(true_labels,km3$cluster)

# Calculate ARI
adj.rand.index(true_labels,km3$cluster)

# create a data frame with the PC1 and PC2 scores and cluster labels
df <- data.frame(PC1 = PC1, PC2 = PC2, Cluster = as.factor(km3$cluster))

# plot the data points colored by the cluster labels
ggplot(df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(color = "Cluster") +
  theme_minimal()

#From the table output we can observe that class2 is further divided into two clusters 1,4 in K-means.
#This is unnecessary clustering of one class which we can observe in the k-means plot above also.

#f
PC_dats <- cbind(PC1, PC2)

set.seed(123)
km4 <- kmeans(PC_dats, 3, nstart = 60)
table(true_labels,km4$cluster)

# Calculate ARI
adj.rand.index(true_labels,km4$cluster)

# create a data frame with the PC1 and PC2 scores and cluster labels
df <- data.frame(PC1 = PC1, PC2 = PC2, Cluster = as.factor(km4$cluster))

# plot the data points colored by the cluster labels
ggplot(df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(color = "Cluster") +
  theme_minimal()

# We can see a perfect clustering as same as in c which is expected. and the rand index =1 
# And from table data we can see each cluster has 20 observations.

#g
set.seed(123) 
scaled_data <- scale(data)
km5 <- kmeans(scaled_data, 3, nstart = 60)

# compare true class labels to clustering labels
table(true_labels,km5$cluster)

# Calculate ARI
adj.rand.index(true_labels,km5$cluster)

# create a data frame with the PC1 and PC2 scores and cluster labels
df <- data.frame(PC1 = scale(PC1), PC2 = scale(PC2), Cluster = as.factor(km5$cluster))

# plot the data points colored by the cluster labels when k=3 on scaled data.
ggplot(df, aes(x = scale(PC1), y = scale(PC2), color = Cluster)) +
  geom_point() +
  labs(color = "Cluster") +
  theme_minimal()



