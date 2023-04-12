library(tidyverse)
dataset=read.csv("weightlifting.csv")

#Explore the data

glimpse(dataset)
head(dataset)
length(dataset)
names(dataset)
summary(dataset)

#Missing value check

colSums(is.na(dataset))

########## EXPERIMENT 1 ############

#Splitting data
library(caTools)
split=sample.split(dataset$Weight, SplitRatio= 0.8) 
training_set=subset(dataset, split=TRUE)
testing_set=subset(dataset, split=FALSE)


#Multiple linear regression training

names(dataset)
MLR=lm(formula=Weight~ ., 
       data=training_set)
summary(MLR)

#Mean square error
sum=summary(MLR)
MSE=(mean(sum$residuals^2))
paste("Mean Square Error: ", MSE)
#The mean square error is 1923.07651799418

#R- square
summary(MLR)
#The R-square is 0.8619

#testing set prediction
y_pred=predict(MLR,newdata = testing_set)
data=data.frame(testing_set$Weight,y_pred)
head(data)

################ Experiment 2 ##################

#Splitting the data into two sets
library(caTools)
split=sample.split(dataset$Weight, SplitRatio= 0.85) 
training_set=subset(dataset, split=TRUE)
testing_set=subset(dataset, split=FALSE)
#Using a split ratio of .85

#Multiple linear regression training

names(dataset)
MLR=lm(formula=Weight~ ., 
       data=training_set)
summary(MLR)


# MSE
sum=summary(MLR)
MSE=(mean(sum$residuals^2))
paste("Mean Square Error: ", MSE)
#MSE is still 1923.07651799418

#R- square
summary(MLR)
#R-square is still 0.8619

#testing set prediction
y_pred=predict(MLR, newdata=testing_set)
data=data.frame(testing_set$Weight, y_pred)
head(data)

#Changing the split ratio did not seem to change the MSE or R-Square very much

#################### Experiment 3 ######################

#Splitting the data into two sets
library(caTools)
split=sample.split(dataset$Weight, SplitRatio= 0.75) 
training_set=subset(dataset, split=TRUE)
testing_set=subset(dataset, split=FALSE)
#Using a split ratio of .75


#Multiple linear regression training

names(dataset)
MLR=lm(formula=Weight~ ., 
       data=training_set)
summary(MLR)


# MSE 
sum=summary(MLR)
MSE=(mean(sum$residuals^2))
paste("Mean Square Error: ", MSE)
#MSE is 1923.07651799418 once again 

#R- square
summary(MLR)
#R-squared is 0.8619 again.

#testing set prediction
y_pred=predict(MLR, newdata=testing_set)
data=data.frame(testing_set$Weight, y_pred)
head(data)

#Changing the split ratio did not change the MSE or R Square values.
