##############################
## Manikanta Kalyan Gokavarapu
##
## created : 15/May/2023
## edited : 
##############################

# Install and load required packages
rm(list = ls())
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 4 work_directory")


library(kohonen)
#a
data(state)
head(state.x77)
dim(state.x77)

#scale the state.x77 data.
state.x77.scaled <- scale(state.x77)
dim(state.x77.scaled)

# fit an SOM
set.seed(123)
state.x77.grid <- somgrid(xdim = 6, ydim = 6, topo = "hexagonal")
state.x77.som <- som(state.x77.scaled, grid = state.x77.grid, rlen = 3000)

#state.x77 codes
codes_x <- state.x77.som$codes[[1]]

x11()
plot(state.x77.som, main = "state.x77 Data")

x11()
plot(state.x77.som, type = "count")

#Do hierarchical clustering 
d1 <- dist(codes_x)
hc1 <- hclust(d1, method = "complete")
x11()
plot(hc1)

#Plot som Cluster
som_cluster1 <- cutree(hc1, h = 5)
# plot the SOM with the found clusters
my_pal <- c("red", "blue","orange","green","yellow")
my_bhcol <- my_pal[som_cluster1]
graphics.off()
x11()
plot(state.x77.som, type = "mapping", col = "black", bgcol = my_bhcol)
add.cluster.boundaries(state.x77.som, som_cluster1)


#b
library(gRbase) #CRAN
library(gRim) #CRAN
library(gRain) #CRAN
library(glasso) #CRAN
library(graph) #Bioconductor
library(corrplot) #CRAN

data(state.x77)


install.packages("glasso")
install.packages("Rgraphviz")
