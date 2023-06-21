##############################
## Manikanta Kalyan Gokavarapu
##
## created : 23/April/2023
## edited : 
##############################

rm(list = ls())
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 3 work_directory")

#Load the needed data.
library(igraph)
library(igraphdata)
data(karate)
?karate
data(kite)
head(kite)
?kite



#a
#Deleted 5% of edges randomly and created noisy karate data set.
set.seed(123)
deleted_edges <- sample(E(karate), size = 0.05 * ecount(karate))
#deleted_edges
noisy_karate <- delete.edges(karate, deleted_edges)

#sanity check
ecount(karate)
ecount(noisy_karate)


#performing MCMC using hierarchical random graph model on noisy_karate data.
set.seed(123)
mcmc_noisy_karate <- fit_hrg(noisy_karate)

#Plotting dendrogram
x11()
set.seed(123)
plot_dendrogram(mcmc_noisy_karate)


#Predict missing edges of noisy_karate data
set.seed(123)
pred_edges <- predict_edges(noisy_karate)
#print(pred_edges)

#List predicted edges that are same as deleted edges.
selected_pred_edges <- pred_edges$edges[c(6, 72, 124), ]
selected_pred_edges
deleted_edges

#b
data(kite)
#Deleted 5% of edges randomly and created noisy kite data set.
set.seed(123)
deleted_edges1 <- sample(E(kite), size = 0.05 * ecount(kite) + 1)
#deleted_edges
noisy_kite <- delete.edges(kite, deleted_edges1)

#sanity check
ecount(kite)
ecount(noisy_kite)


#performing MCMC using hierarchical random graph model on noisy_karate data.
set.seed(123)
mcmc_noisy_kite <- fit_hrg(noisy_kite)

#Plotting dendrogram
x11()
set.seed(123)
plot_dendrogram(mcmc_noisy_kite)

#Predict missing edges of noisy_karate data
set.seed(123)
pred_edges1 <- predict_edges(noisy_kite)
print(pred_edges1)

#List predicted edges that are same as deleted edges.
selected_pred_edges1 <- pred_edges1$edges[c(16), ]
selected_pred_edges1
deleted_edges1


#c
#Delete 10% of edges randomly and create noisy karate data set.
set.seed(123)
deleted_edges3 <- sample(E(karate), size = 0.1 * ecount(karate))
#deleted_edges
noisy_karate_10 <- delete.edges(karate, deleted_edges3)

#sanity check
ecount(karate)
ecount(noisy_karate_10)


#performing MCMC using hierarchical random graph model on noisy_karate data.
set.seed(123)
mcmc_noisy_karate_10 <- fit_hrg(noisy_karate_10)

#Plotting dendrogram
x11()
set.seed(123)
plot_dendrogram(mcmc_noisy_karate_10)


#Predict missing edges of noisy_karate data
set.seed(123)
pred_edges2 <- predict_edges(noisy_karate_10)
#pred_edges2

#List predicted edges that are same as deleted edges.
deleted_edges3
selected_pred_edges2 <- pred_edges2$edges[c(100,10,62,427,393,1,31), ]
selected_pred_edges2


#Delete 20% of edges randomly and create noisy karate data set.
set.seed(123)
deleted_edges4 <- sample(E(karate), size = 0.2 * ecount(karate))
#deleted_edges
noisy_karate_20 <- delete.edges(karate, deleted_edges4)

#sanity check
ecount(karate)
ecount(noisy_karate_20)


#performing MCMC using hierarchical random graph model on noisy_karate data.
set.seed(123)
mcmc_noisy_karate_20 <- fit_hrg(noisy_karate_20)

#Plotting dendrogram
x11()
set.seed(123)
plot_dendrogram(mcmc_noisy_karate_20)


#Predict missing edges of noisy_karate data
set.seed(123)
pred_edges3 <- predict_edges(noisy_karate_20)
pred_edges3

#List predicted edges that are same as deleted edges.
deleted_edges4
selected_pred_edges3 <- pred_edges3$edges[c(50,73,14,298,313,1,71,63,40,10,8,12,41,11,93), ]
selected_pred_edges3



#Delete 10% of edges randomly and create noisy kite data set.
set.seed(123)
deleted_edges5 <- sample(E(kite), size = 0.1 * ecount(kite))
#deleted_edges
noisy_kite_10 <- delete.edges(kite, deleted_edges5)

#sanity check
ecount(kite)
ecount(noisy_kite_10)


#performing MCMC using hierarchical random graph model on noisy_kite data.
set.seed(123)
mcmc_noisy_kite_10 <- fit_hrg(noisy_kite_10)

#Plotting dendrogram
x11()
set.seed(123)
plot_dendrogram(mcmc_noisy_kite_10)


#Predict missing edges of noisy_kite data
set.seed(123)
pred_edges4 <- predict_edges(noisy_kite_10)
pred_edges4

#List predicted edges that are same as deleted edges.
deleted_edges5
selected_pred_edges4 <- pred_edges4$edges[c(16), ]
selected_pred_edges4



#Delete 20% of edges randomly and create noisy kite data set.
set.seed(123)
deleted_edges6 <- sample(E(kite), size = 0.2 * ecount(kite))
#deleted_edges
noisy_kite_20 <- delete.edges(kite, deleted_edges6)

#sanity check
ecount(kite)
ecount(noisy_kite_20)


#performing MCMC using hierarchical random graph model on noisy_kite data.
set.seed(123)
mcmc_noisy_kite_20 <- fit_hrg(noisy_kite_20)

#Plotting dendrogram
x11()
set.seed(123)
plot_dendrogram(mcmc_noisy_kite_20)


#Predict missing edges of noisy_kite data
set.seed(123)
pred_edges5 <- predict_edges(noisy_kite_20)
pred_edges5

#List predicted edges that are same as deleted edges.
deleted_edges6
selected_pred_edges5 <- pred_edges5$edges[c(17,7,1), ]
selected_pred_edges5