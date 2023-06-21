##############################
## Manikanta Kalyan Gokavarapu
##
## created : 14/May/2023
## edited : 
##############################


rm(list = ls())
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 4 work_directory")

rm(list = ls())
graphics.off()

######################################################
## See: http://cran.r-project.org/web/views/gR.html
##################################################
install.packages("ggm")
library(ggm)
library(gRain)
#library(RHugin)
library(Rgraphviz)
library(gRbase)
library(ggm)
library(igraph)
install.packages("gRbase")
install.packages("RBGL")
library(graph)

graphics.off()
# Load required packages
install.packages("gRain")
install.packages("Rgraphviz")
install.packages("RHugin")
library(gRain)
library(RHugin)
library(Rgraphviz)
library(Rgraphviz)
library(gRbase)
library(ggm)

install.packages("MASS")
library(MASS)
# Load the "cad1" dataset
data(cad1)

install.packages("gRbase")
library(gRbase)

install.packages("graph")

# View the structure of the data
str(data)
# Part a: Inference of Bayesian Network
# Use structural learning algorithm to infer Bayesian Network
bn <- hc(cad1)

# Print the learned structure of the Bayesian Network
print(bn)

# Part b: Identifying d-separations in the graph
dseps <- allDsep(bn)

# Print the d-separations
print(dseps)

# Part c: Absorb evidence and revise probabilities
# Set evidence for a new observation (female with high cholesterol)
evidence <- data.frame(Sex = "Female", Hypercholesterolemia = "Yes")

# Absorb evidence into the Bayesian Network
bn_updated <- cpquery(bn, event = (CAD == "Yes" | HeartFailure == "Yes"), evidence = evidence)

# Print revised probabilities
print(bn_updated)

# Part d: Risk of CAD for an individual
# Set evidence for QWave = "Yes" and AMI = "Definate"
evidence2 <- data.frame(QWave = "Yes", AMI = "Definate")

# Compute probabilities for CAD
cad_prob <- cpquery(bn, event = (CAD == "Yes"), evidence = evidence2)

# Print the probability of CAD
print(cad_prob)
