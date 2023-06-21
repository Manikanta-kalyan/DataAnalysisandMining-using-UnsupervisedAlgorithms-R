##############################
## Manikanta Kalyan Gokavarapu
##
## created : 14/May/2023
## edited : 
##############################
# Install and load required packages

rm(list = ls())
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 4 work_directory")
#install.packages("kohonen")
library(kohonen)
#install.packages("readr")
#install.packages("phase_plot")


#a
# Load the red wine and white wine data sets
red_wine <- read.csv("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 4 work_directory/winequality-red.csv", header = TRUE, sep = ";")
white_wine <- read.csv("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 4 work_directory/winequality-white.csv", header = TRUE, sep = ";")

head(red_wine)
head(white_wine)

# Add a column to indicate the wine type
red_wine$wine_type <- "red"
white_wine$wine_type <- "white"

# Combine the red wine and white wine datasets
wines <- rbind(red_wine, white_wine)

#scale the wines data
wines.scaled <- scale(wines[,-ncol(wines)])



# fit an SOM
set.seed(123)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
wine.som <- som(wines.scaled, grid = som_grid, rlen = 3000)

#som codes
codes <- wine.som$codes[[1]]

x11()
plot(wine.som, main = "Wines Data")

x11()
plot(wine.som, type = "count")

d <- dist(codes)
hc <- hclust(d)

x11()
plot(hc)


som_cluster <- cutree(hc, h = 6)

# plot the SOM with the found clusters

my_pal <- c("red", "white")
my_bhcol <- my_pal[som_cluster]

graphics.off()

x11()
plot(wine.som, type = "mapping", col = "black", bgcol = my_bhcol)
add.cluster.boundaries(wine.som, som_cluster)


#b


#Phase plots for each variable in dataset
x11()
par(mfrow = c(3, 4))
for (i in 1:12){
  plot(wine.som, type = "changes", property=codes[,i], main = ifelse(is.na(colnames(codes)[i]), "", colnames(codes)[i]))
}

# phase/Component plane plots for each variable in dataset.
x11()
par(mfrow = c(3, 4))
for (i in 1:12){
  plot(wine.som, type = "property", property=codes[,i], main = ifelse(is.na(colnames(codes)[i]), "", colnames(codes)[i]))
}


#c
# Plot the prototypes on the SOM
x11()
plot(wine.som, type = "codes", bgcol = my_bhcol, main = "Prototypes on SOM")

