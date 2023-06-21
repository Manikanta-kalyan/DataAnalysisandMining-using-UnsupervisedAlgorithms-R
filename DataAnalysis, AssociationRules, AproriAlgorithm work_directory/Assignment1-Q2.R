##############################
## Manikanta Kalyan Gokavarapu
##
## created : 11/21/2023
## edited : 
##############################

rm(list = ls())

library(rpart)
library(arules)
#install.packages("caret")
library(caret)
library(rpart)

install.packages("rpart.plot")
library (rpart.plot)
# Set up working directory  
getwd()
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 1 work_directory")

#Imported Marketing Data through CSV into R.
Marketing <- read.csv("Marketing.csv")
head(Marketing)
dim(Marketing)

#Removing NA values
Marketing <- na.omit(Marketing)



###main part ####

# Create a reference sample
set.seed(123)
ref_sample = Marketing
for(i in 1:ncol(ref_sample)){
  ref_sample[,i] = sample(ref_sample[,i], nrow(ref_sample), replace = T)
}
dim(ref_sample)

# Assign class 0 to reference sample and class 1 to train data.
ref_sample$class <- 0
Marketing$class <- 1


data_all <- rbind(Marketing, ref_sample)


tree <- rpart(class ~ ., data = data_all, method = "class")

x11()
rpart.plot(tree)

summary(tree)

#From the plot it is evident that the highest estimated class 1 probability is 0.84.

#If Number of Household <= 8 and Number of children <=5 and Language_in_home>=2 => Ethnic classification>=7 with a value of 0.7 and confidence 5%
#If Dual_income < 2, Marital_status >= 5, Household status < 3, Ethnic classification < 3,Age < 3 => languages in home <3 with a value of 0.73   and confidence 7%
#If Number_of_Children<=5,Language_In_Home <=2, Income>=2, Marital status>=5 => Education <3 with a value of 0.70 and confidence 8%







#### Deriving Generalized association rules for marketing Dataset. #####

#categorizing the variables

# Convert the dataset into a transaction format or a Binary Incidence matrix.
Marketing_Trans <- as(Marketing, "transactions")

#Plot the item Frequency plot
x11()
itemFrequencyPlot(Marketing_Trans, support = 0.01, cex.names = 0.8)

# Apply the Apriori algorithm
#I have considered minimum support value of 0.01, so get the most of the association rules (1-5%) 
# A confidence which can be from 50% to 80% in general so I choose 0.7.


rules  <- apriori(Marketing_Trans, parameter = list(support = 0.01, confidence = 0.8))
summary(rules)

inspect(head(sort(rules, by = "lift"), n = 3))
