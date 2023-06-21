##############################
## Manikanta Kalyan Gokavarapu
##
## created : 18/March/2023
## edited : 
##############################

rm(list = ls())
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 2 work_directory")

library(recommenderlab)


# Read in the MovieLense data
data(MovieLense)
?MovieLense
Ratings <- MovieLense
head(Ratings) # 6 x 1664
dim(Ratings) # 943 1664

## look at the first few ratings of the first user
head(as(Ratings[1,], "list")[[1]])

# Create a "realRatingMatrix (not required as MovieLense is already an object of realRatingMatrix but doesn't make a difference)
R <- as(Ratings, "realRatingMatrix")

# Get the Rating Matrix
dim(getRatingMatrix(R))
getRatingMatrix(R)[1:10, 1:10]


######################################
## Create a recommender system
######################################

recommenderRegistry$get_entries(dataType = "realRatingMatrix")

recommender_popularity <- Recommender(R[943:1], method = "POPULAR")
names(getModel(recommender_popularity))
getModel(recommender_popularity)$topN

# Create top 10 recommendations for 3 users
recom <- predict(recommender_popularity, R[1:3], n=10)
recom
as(recom, "list")

###########################################################
#### Evaluation
##### splitting the data into train and test
###########################################################
dim(R)

eval<- evaluationScheme(R,method = "split", given = 15, train=0.5, goodRating=4)
eval

##########################################################
### Building a recommender model using user based collaborative filtering
#######################################################
userbased_model<- Recommender(getData(eval,"train"), "UBCF")
userbased_model


#################################################################################
### evaluation of top-N recommender algorithm using the Given-3 protocol
###i.e, for the test users all but 3 are withheld for evaluation.
#################################################################################
scheme<- evaluationScheme(R, method="cross",k=4, given=3, goodRating=4)
scheme

###################################################################
### using the created evaluating scheme to evaluate the recommender method popular
#### evaluating the top 1, 3, 5, 10,15,20 recommendation lists
###################################################################################
results<- evaluate(scheme, method = "POPULAR", type="topNList", n=c(1,3,5,10,15,20))
results
getConfusionMatrix(results)[[1]]

