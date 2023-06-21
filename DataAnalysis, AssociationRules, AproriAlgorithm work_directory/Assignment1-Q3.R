##############################
## Manikanta Kalyan Gokavarapu
##
## created : 11/19/2023
## edited : 
##############################
rm(list = ls())

# Set up working directory  
getwd()
setwd("D:/University at Buffalo CSE/Spring Semester 2023/STA 546 DataM II/R/Assignment 1 work_directory")

# Install Boston package
#install.packages("ISLR2")
library(ISLR2)
ls("package:ISLR2")
head(Boston)

#install.packages("arules")
library(arules)


# Plot Histograms

Boston2<-Boston

par(mfrow=c(3,2))
#x11()
hist(Boston$crim, breaks = 4, main = "per capita crime rate by town.", xlab = "crim")

#x11()
hist(Boston$zn, breaks = 3, main = "proportion of residential land zoned for lots over 25,000 sq.ft.", xlab = "zn")

#x11()
hist(Boston$indus, breaks = 3, main = "proportion of non-retail business acres per town.", xlab = "indus")

#x11()
hist(Boston$age, breaks = 3, main = "proportion of owner-occupied units built prior to 1940.", xlab = "age")

#x11()
hist(Boston$tax, breaks = 3, main = "full-value property-tax rate per $10,000.", xlab = "tax")

# Categorize the variables using cut function
Boston$crim <- ordered(cut(Boston$crim, breaks = c(-Inf, 1, 5, Inf), labels = c("Low", "Medium", "High")))
Boston$zn <- cut(Boston$zn, breaks = c(-Inf, 10, 20, Inf), labels = c("Low", "Medium", "High"))
Boston$indus <- cut(Boston$indus, breaks = c(-Inf, 15, 25, Inf), labels = c("Low", "Medium", "High"))
Boston$chas <- factor(Boston$chas)
Boston$nox <- cut(Boston$nox, breaks = c(-Inf, 0.5, 0.7, Inf), labels = c("Low", "Medium", "High"))
Boston$rm <- cut(Boston$rm, breaks = c(-Inf, 6, 7, Inf), labels = c("Low", "Medium", "High"))
Boston$age <- ordered(cut(Boston$age, breaks = c(-Inf, 70, 85, Inf), labels = c("Low", "Medium", "High")))
Boston$dis <- cut(Boston$dis, breaks = c(-Inf, 3, 5, Inf), labels = c("short", "Medium", "Long"))
Boston$rad <- factor(Boston$rad)
Boston$tax <- cut(Boston$tax, breaks = c(-Inf, 300, 500, Inf), labels = c("Low", "Medium", "High"))
Boston$ptratio <- cut(Boston$ptratio, breaks = c(-Inf, 17, 20, Inf), labels = c("Low", "Medium", "High"))
Boston$lstat <- cut(Boston$lstat, breaks = c(-Inf, 10, 15, Inf), labels = c("Low", "Medium", "High"))
Boston$medv <- cut(Boston$medv, breaks = c(-Inf, 25, Inf), labels = c("low", "high"))

#I have used different break points for each variable, based on the distribution in histograms.
#I have also looked at the data and used relevant domain knowledge to categorize each variable like for tax, dis, age.
#As the CRIM and AGE  are ordered variables general cut function doesn't preserve the ordering So, I have used order function to preserve the order of variables.
#For the un ordered categorical variables like CHAS and RAD we leave them as un ordered factors only as they are already categorized. 


#Convert to a binary incidence matrix
Boston <- as(Boston, "transactions")
summary(Boston)


#Plot the item Frequency plot
x11()
itemFrequencyPlot(Boston, support = 0.01, cex.names = 0.8)

# Apply the Apriori algorithm
#I have considered minimum support value of 0.01, so we get the most of the association rules (1-5%) 
# and a confidence which can be from 50% to 80% in general. so I choose 0.7.
rules  <- apriori(Boston, parameter = list(support = 0.01, confidence = 0.7))
summary(rules)



rulestax_and_crime_low <- subset(rules, subset = lhs %in% c("crim=Low") & rhs %in% c("tax=Low") & lift > 1.2)
inspect(head(sort(rulestax_and_crime_low , by = "confidence"), n = 10))
inspect(head(sort(rulestax_and_crime_low , by = "lift"), n = 10))


# If we need crime rate low area which also has low tax
#Rule 1: {crim=Low, indus=High, rad=2}=>{tax=Low}, From rule 1 we can say that he need to choose a industrial area and  also an area far from radial highways
#Rule 2: {crim=Low, indus=High, ptratio=Medium} =>{tax=Low}, From rule 2 we say that he need to choose industrial area and also an area with less pupil-teacher ratio.(generally we have more taxes in the area of less pupil-teacher ratio)
#Rule 3: {crim=Low, dis=Medium, rad=2}=>{tax=Low}, From rule 3 we can say that he little bit near to Boston employment centers.
#Finally from the rules we can say that if we want to choose a area with low crime and tax he need to choose an industrial area instead of residential area and also an area with less accessibility to radial highways and area with schools having medium pupil-teacher ratio.

low_pupil_teacher_ratio <- subset(rules, subset = rhs %in% "ptratio=Low" & lift>1.2)
inspect(head(sort(low_pupil_teacher_ratio , by = "confidence"), n = 10))
inspect(head(sort(low_pupil_teacher_ratio , by = "lift"), n = 10))

#If we need low pupil-teacher ratios.
#Rule1:{zn=High, rad=6}=> {ptratio=Low} They need to choose an area with good proportion of residential land area (>25000 sq.ft).
#Rule2:{dis=Long, rad=6}=> {ptratio=Low} They need to choose an area which is far from the five Boston employment centers.
#Rule3:{nox=Low, rad=6}=>{ptratio=Low} They need to choose an area with low nitrogen oxides concentration.
#Rule4:{rad=6, lstat=Low}=> {ptratio=Low} They need to choose area which has less percent lower status population.

#Finally if they need low pupil-teacher ratio they need to choose an area with good proportion of residential land area, far from five Boston employment centers, low nitrogen oxides concentration and
#less percent of lower status population


#Extra credit

pttree <- rpart(ptratio ~ ., data = Boston2, method = "class")

model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
pttree<- rpart(ptratio~., data = Boston2, method = "class", control = model.control)

x11()
rpart.plot(pttree)