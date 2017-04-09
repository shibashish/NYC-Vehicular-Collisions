#Loading the necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(caTools)
library(caret)
library(randomForest)
library(flexclust)
library(rpart)
library(e1071)
library(caret)


#Setting the working directory
#ATTENTION: Change directory according to your own system
setwd("C:/Users/abhi2/Downloads")


#Loading our cleaned csv
nyc <- read.csv("NYPD_Motor_Vehicle_Collisions - Locations Cleaned.csv" , stringsAsFactors = FALSE)

# Remove X column
nyc <- nyc %>%
  select(-X)

#Saving a copy of the orignal dataset before we begin transforming the data
nyc_orignal = nyc


#Convert to date
nyc$DATE2 <- mdy(nyc$DATE)


#Separate Time and Date columns
nyc <- separate(nyc, TIME , c("Hour", "Min"), sep = ":")
nyc <- separate(nyc, DATE2 , c("YEAR", "MONTH" , "DAY"), sep = "-")


# Removing 2012 and 2017 observations
nyc <- nyc %>%
  filter(YEAR != '2012' , YEAR != '2017')


#Creating a variable that counts the number of vehicles involved in the accident
nyc$vehicle.count <- pmax(as.numeric(nyc$VEHICLE.TYPE.CODE.1 != "") + 
                            as.numeric(nyc$VEHICLE.TYPE.CODE.2 != "") +
                            as.numeric(nyc$VEHICLE.TYPE.CODE.3 != "") +
                            as.numeric(nyc$VEHICLE.TYPE.CODE.4 != "") +
                            as.numeric(nyc$VEHICLE.TYPE.CODE.5 != ""),
                          as.numeric(nyc$CONTRIBUTING.FACTOR.VEHICLE.1 != "") + 
                            as.numeric(nyc$CONTRIBUTING.FACTOR.VEHICLE.2 != "") +
                            as.numeric(nyc$CONTRIBUTING.FACTOR.VEHICLE.3 != "") +
                            as.numeric(nyc$CONTRIBUTING.FACTOR.VEHICLE.4 != "") +
                            as.numeric(nyc$CONTRIBUTING.FACTOR.VEHICLE.5 != ""))



#For that we get the list of all unique types of cars
Vehicle_list = unique(as.vector(as.matrix(nyc[,c("VEHICLE.TYPE.CODE.1","VEHICLE.TYPE.CODE.2","VEHICLE.TYPE.CODE.3","VEHICLE.TYPE.CODE.4","VEHICLE.TYPE.CODE.5")])))

#Removing the blank element from the vehicle list
Vehicle_list = Vehicle_list[-2]


#for loop going trough each car type
for(i in 1:length(Vehicle_list))
  #adding up all the 5 vehicle type codes to get a count of that type of vehicle
{
  vehicle.count <- as.numeric(nyc$VEHICLE.TYPE.CODE.1 == Vehicle_list[i]) + 
    as.numeric(nyc$VEHICLE.TYPE.CODE.2 == Vehicle_list[i]) +
    as.numeric(nyc$VEHICLE.TYPE.CODE.3 == Vehicle_list[i]) +
    as.numeric(nyc$VEHICLE.TYPE.CODE.4 == Vehicle_list[i]) +
    as.numeric(nyc$VEHICLE.TYPE.CODE.5 == Vehicle_list[i])
  
  nyc[,Vehicle_list[i]] <- vehicle.count
}


#Creating the dependent variable
nyc$injured <- rep(FALSE, nrow(nyc))
nyc$injured[nyc$NUMBER.OF.PERSONS.INJURED >0 ] <- TRUE
nyc$injured = as.factor(nyc$injured)

#Removing excessive columns from our dataset for the purpose of regression
nyc = subset(nyc,select = c(3,6,7,32,34:52))

#Converting Month and Hour into factors
nyc$MONTH = as.factor(nyc$MONTH)
nyc$Hour = as.factor(nyc$Hour)

#Legalizing column names
colnames(nyc) = make.names(colnames(nyc),unique = TRUE)

#Since our models are going to be on Longitude and Latitude locations we will presently remove all observations without valid longitude and latitude
nyc = nyc[!is.na(nyc$LATITUDE),]

#creating a training set and testing set with a 70:30
set.seed(123)
spl = sample.split(nyc$injured, SplitRatio = 0.7)
nyc_train = subset(nyc, spl ==TRUE)
nyc_test = subset(nyc,spl==FALSE)


#Baseline Model


confusionMatrix(nyc_test$vehicle.count == 1,nyc_test$injured, positive = "TRUE")


#For the baseline model we predict that someone has been injured if the number of vehicles involved in that accident are equal to exactly one


#Now that we have identified the results for our baseline model we will begin by building a logistic regression model
glm_nyc = glm(injured ~ ., data = nyc_train,family = "binomial")

#Looking at the summary of the model
summary(glm_nyc)


#Predicting 
predict_test_glm = predict(glm_nyc,newdata = nyc_test,type = "response")

#Calculating the accuracy of this model
confusionMatrix(predict_test_glm>0.5,nyc_test$injured, positive = "TRUE")

#From our simple logistic regression model we get an accuracy of 82.73 % which is a small improvement over our baseline model.

#Rpart for whole
rpart_nyc = rpart(injured~., data = nyc_train, method = "class", minbucket = 25)
predict_rpart = predict(rpart_nyc, newdata = nyc_test, type = "class")
confusionMatrix(nyc_test$injured, predict_rpart, positive = "TRUE")

#Cross Validating
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(injured~., data = nyc_train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
rpart_cv = rpart(injured~., data = nyc_train, method = "class", cp = 0.01)
predict_rpart_cv = predict(rpart_cv, newdata = nyc_test, type = "class")
confusionMatrix(predict_rpart_cv,nyc_test$injured, positive = "TRUE")
#Now we will build a random forest model 

rf_nyc = randomForest(injured ~ ., data = nyc_train, ntree = 100)

predict_test_rf = predict(rf_nyc,newdata = nyc_test)

confusionMatrix(predict_test_rf,nyc_test$injured,positive = "TRUE")

#Kmean cluster



#Scaling the dataset to prepare for k-means clustering
nyc_scaled = nyc[,-23]
nyc_scaled$Hour = as.numeric(nyc_scaled$Hour)
nyc_scaled$MONTH = as.numeric(nyc_scaled$MONTH)
nyc_scaled = as.data.frame(scale(nyc_scaled))

#Splitting the scaled dataset into train and test using the previous split vector
nyc_scaled_train = nyc_scaled[spl == TRUE,]
nyc_scaled_test = nyc_scaled[spl == FALSE,]

set.seed(123)
#Getting the trained k-means clustering
kmc = kmeans(nyc_scaled_train,centers = 8,iter.max = 1000)

#Creating a prediction model for predicting clusters on test set
kmc_kmcca = as.kcca(kmc,nyc_scaled_train)

#Predicting 
test_kmc = predict(kmc_kmcca,newdata = nyc_scaled_test)

nyc_train$cluster = kmc$cluster
nyc_test$cluster = test_kmc

#WORKING ON CLUSTERs

nyc_test$predinjured = rep(0, nrow(nyc_test))

for(i in 1:8)
{
  nyc_train_clust = nyc_train[nyc_train$cluster == i,]
  nyc_test_clust = nyc_test[nyc_test$cluster == i,]
  
  regressor = glm(injured ~ . -cluster, data = nyc_train_clust ,family = "binomial")
  
  predicted = predict(regressor, newdata = nyc_test_clust,type = "response")
  
  nyc_test$predinjured[nyc_test$cluster == i] = predicted
  
}

confusionMatrix(nyc_test$predinjured > 0.5,nyc_test$injured, positive = "TRUE")



#Classification tree implementation for clusters

nyc_test$predinjured = rep(0, nrow(nyc_test))

for(i in 1:8)
{
  nyc_train_clust = nyc_train[nyc_train$cluster == i,]
  nyc_test_clust = nyc_test[nyc_test$cluster == i,]
  
  regressor = rpart(injured~. -cluster, data = nyc_train_clust, method = "class", minbucket =25)
  
  predicted = predict(regressor, newdata = nyc_test_clust)
  
  nyc_test$predinjured[nyc_test$cluster == i] = predicted
  
}

confusionMatrix( nyc_test$predinjured == 2,nyc_test$injured,positive = "TRUE")



#Cross Validating RPART FOR CLUSTERS

nyc_test$predinjured = rep(0, nrow(nyc_test))

for(i in 1:8)
{
  nyc_train_clust = nyc_train[nyc_train$cluster == i,]
  nyc_test_clust = nyc_test[nyc_test$cluster == i,]
  
  regressor = rpart(injured~. -cluster, data = nyc_train_clust, method = "class", cp = 0.01)
  
  predicted = predict(regressor, newdata = nyc_test_clust)
  
  nyc_test$predinjured[nyc_test$cluster == i] = predicted
  
}

confusionMatrix(nyc_test$predinjured >0.5,nyc_test$injured, positive = "TRUE")


#Random Forest Implementation For clusters

nyc_test$predinjured = rep(0, nrow(nyc_test))

for(i in 1:8)
{
  nyc_train_clust = nyc_train[nyc_train$cluster == i,]
  nyc_test_clust = nyc_test[nyc_test$cluster == i,]
  set.seed(123)
  regressor = randomForest(injured ~ . -cluster , data = nyc_train_clust,mtree = 100)
  
  predicted = predict(regressor, newdata = nyc_test_clust)
  
  nyc_test$predinjured[nyc_test$cluster == i] = predicted
  
}

confusionMatrix(nyc_test$injured, nyc_test$predinjured == 2,positive = "TRUE")
