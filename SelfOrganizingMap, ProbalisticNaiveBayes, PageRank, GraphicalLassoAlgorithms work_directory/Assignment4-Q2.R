##############################
## Manikanta Kalyan Gokavarapu
##
## created : 14/May/2023
## edited : 
##############################


library(ggplot2)
library(reshape2)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(cluster)
library(factoextra)

rm(list = ls())
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 4 work_directory")

# Load the red wine and white wine data sets
red_wine <- read.csv("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 4 work_directory/winequality-red.csv", header = TRUE, sep = ";")
white_wine <- read.csv("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 4 work_directory/winequality-white.csv", header = TRUE, sep = ";")

head(red_wine)
head(white_wine)

dim(red_wine)
dim(white_wine)


#a
##EDA
# Create a histogram of red wine w.r.t different attributes.
x11()
par(mfrow=c(2,2))
hist(red_wine[,2], breaks = 25, main = "red_wine,volatile_acidity", xlab = "Volatile acidity")
hist(red_wine[,3], breaks = 25, main = "red_wine,citric.acid", xlab = "citric.acid")
hist(red_wine[,4], breaks = 25, main = "red_wine,residual.sugar", xlab = "residual.sugar")
hist(red_wine[,7], breaks = 25, main = "red_wine,total.sulfur.dioxide", xlab = "total.sulfur.dioxide")


#Create histogram of white whine w.r.t different attributes
x11()
par(mfrow=c(2,2))
hist(white_wine[,2], breaks = 25, main = "white_wine,volatile_acidity", xlab = "Volatile acidity")
hist(white_wine[,3], breaks = 25, main = "white_wine,citric.acid", xlab = "citric.acid")
hist(white_wine[,4], breaks = 25, main = "white_wine,residual.sugar", xlab = "residual.sugar")
hist(white_wine[,7], breaks = 25, main = "white_wine,total.sulfur.dioxide", xlab = "total.sulfur.dioxide")


# Calculate the correlation  matrix of white wine
x11()
corr_matrix <- cor(red_wine)
melted_corr_matrix <- melt(corr_matrix)
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "red_wine correlation matrix")

#caluclate the coorelation matrix of red wine.
x11()
corr_matrix <- cor(white_wine)
melted_corr_matrix <- melt(corr_matrix)
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "white_wine correlation matrix")



# Create a pairplot matrix
x11()
pairs(red_wine, las=2, main= "redwine_pairplot")
x11()
pairs(white_wine, las=2, main = "whitewine_pairplot")


##Data Quality and characterstics

# Summarize the dataset
summary(red_wine)
summary(white_wine)

# Examine the structure of the dataset
str(red_wine)
str(white_wine)

# Count missing values per variable
colSums(is.na(red_wine))
colSums(is.na(white_wine))

# Count the number of duplicate rows
num_duplicates <- sum(duplicated(red_wine))
print(num_duplicates)
num_duplicates2 <- sum(duplicated(white_wine))
print(num_duplicates2)

## Outliers and associations 

#Create a box plot of wine.
x11()
boxplot(red_wine)
x11()
boxplot(white_wine)

#EDA summary of red wines
#The red wine data set contains 1599 observations (rows) and 12 variables (columns).
#The variables in the red wine data set include various chemical properties such as fixed acidity, volatile acidity, citric acid, residual sugar, chlorides, free sulfur dioxide, total sulfur dioxide, density, pH, sulphates, alcohol, and quality.
#The "quality" variable represents the sensory quality of the wines which is ranging from 3 to 8.
#From the summary and structure of red wines we could see the mean, median, minimum, maximum, and quartiles for each variable in the dataset. and we could observe there is a central tendency on the spread of the data 
#From The structure output we could see that all the variables are of numeric type.
#There are no missing values in any variable of red_wine.
#There are 240 duplicates in red_wine data set.
#From the correlation matrix and pair plots we could obviously see total and free sulfur dioxide are highly related and also denisty and citric.acid are related with fixed acidity.   
#From the box plot we could see the outliers exist for variables fixed.acidity, residual.sugar, free.sulfur dioxide, total.sulfur.dioxide

#The white wine data set contains 4898 observations (rows) and 12 variables (columns).
#The variables in the white wine data set are the same as those in the red wine data set, representing chemical properties and quality.
#From the summary and structure of white wines we could see the mean, median, minimum, maximum, and quartiles for each variable in the dataset. and we could observe there is a central tendency on the spread of the data 
#From The structure output we could see that all the variables are of numeric type.
#There are no missing values in any variable of white_wine.
#There are 937 duplicates in white_wine data set.
#From correlation matrix and pair plots we could see total and free sulfur dioxide are correlated and also density and residual sugar are linearly correlated.
#There are more number of outliers in total and free sulfur dioxide attributes and compartively less number of outliers in residual sugar.


#b
# Exclude non-numeric columns
numeric_cols <- sapply(red_wine, is.numeric)
red_wine_numeric <- red_wine[, numeric_cols]

# Perform PCA
pca <- prcomp(red_wine_numeric, scale. = TRUE)

# Create a scree plot
x11()
plot(pca, type = "lines", main = "Scree Plot of Red wines")


#create a biplot.
x11()
ggbiplot(pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

#A scree plot captures how much variation each principal component captures in the data
#From red wines scree plot we could say that first four principal components  captures most of the variance in the data, so they are enough to describe the data.
#From bi-plot between PC1 and PC2 of red wines we could observe that fixed acidity, citric acid, sulphates are in same direction of PC1 so they are positively correlated with PC1
#From bi-plot, pH has a negative association with PC1.

#c
# Exclude non-numeric columns
numeric_cols2 <- sapply(white_wine, is.numeric)
white_wine_numeric <- white_wine[, numeric_cols2]

# Perform PCA
pca2 <- prcomp(white_wine_numeric, scale. = TRUE)

# Create a scree plot
x11()
plot(pca2, type = "lines", main = "Scree Plot of white wines")


#create a biplot.
x11()
ggbiplot(pca2, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

#A scree plot captures how much variation each principal component captures in the data
#From white wines scree plot we could say that first three principal components  captures most of the variance in the data, so they are enough to describe the data.
#From bi-plot between PC1 and PC2 of white wines we could observe that alcohol and quality are in same direction of PC1 so they are positively correlated with PC1
#From bi-plot, residual sugar, total and free sulfur dioxide have negative association with PC1 values.

#d
# Add a column to indicate the wine type
red_wine$wine_type <- "red"
white_wine$wine_type <- "white"

# Combine the red wine and white wine datasets
wine <- rbind(red_wine, white_wine)

# Preview the combined data set
head(wine)


# Perform PCA on the combined data
pca3 <- prcomp(wine[,-ncol(wine)], scale. = TRUE)

x11()
# Create the biplot
biplot(pca3, choices = c(1, 2), scale = 0)
# Add color to the biplot according to wine type
ggbiplot::ggbiplot(pca3, obs.scale = 1, var.scale = 1,
                   groups = wine$wine_type,
                   ellipse = TRUE, circle = TRUE) +
  theme_minimal()

#e
#Elbow method.
x11()
# Determine the optimal number of clusters using the elbow method
wcss <- numeric(length = 10)  # Initialize vector to store WCSS values
# Iterate over different values of k
for (k in 1:10) {
  # Perform k-means clustering
  set.seed(123)
  kmeans_model <- kmeans(wine[,-ncol(wine)], centers = k, nstart = 10)
  # Store the within-cluster sum of squares (WCSS)
  wcss[k] <- kmeans_model$tot.withinss
}
# Plot the WCSS values
plot(1:10, wcss, type = "b", xlab = "Number of Clusters (k)", ylab = "WCSS",
     main = "Elbow Method for Determining k")

#Basis on the elbow plot optimal for k is 2.

# Perform k-means clustering with the chosen value of k
set.seed(123)
kmeans_model <- kmeans(wine[,-ncol(wine)], centers = 2, nstart = 10)
# Get cluster assignments for each data point
cluster_assignments <- as.factor(kmeans_model$cluster)
table(cluster_assignments)  # Frequency table of cluster assignments
x11()
fviz_cluster(kmeans_model, data = wine[, -ncol(wine)], geom = "point", ellipse.type = "norm")

#The optimal k value is 2  for the wine data as shown in elbow plot. This might be due to two different wines red and white wine data.
#From the table output we can see after clustering there is good amount of separation between data and has nearly equal number of points distributed between two clusters.

#f
# Perform PCA
pca_k <- prcomp(wine[,-ncol(wine)], scale. = TRUE)
# Extract PC scores
PC1_k <- pca_k$x[, 1]
PC2_k <- pca_k$x[, 2]
PC_dats <- cbind(PC1_k, PC2_k)
# Perform k-means clustering with the chosen value of k
set.seed(123)
kmeans_model_pc <- kmeans(PC_dats, centers = 2, nstart = 10)
# Get cluster assignments for each data point
cluster_assignments_pc <- as.factor(kmeans_model_pc$cluster)
# Plot the k-means clusters using PC1 and PC2
x11()
# Define a vector of colors for each cluster
cluster_colors <- c("red", "blue")
ggplot(wine[,-ncol(wine)], aes(x = PC1_k, y = PC2_k, color = cluster_assignments_pc)) +
  geom_point() +
  labs(x = "PC1", y = "PC2", color = "Cluster_assignments_pc") +
  scale_color_manual(values = cluster_colors) +
  theme_minimal()

#The optimal value of k is 2 for principal compenent data of wine.
#The data is clustered nicely into two groups may be indicating red and white wines.
#The no.of data points between the two group are also nearly equal.


#g
#Overall, I think E and F are nearly similar plots and also the separation nearly seems same. 
#But overall the F cluster separation is better than E as there is no overlap between clusters when plotted w.r.t principal component data.
#E clustering has better split of number of data points in each cluster. 

