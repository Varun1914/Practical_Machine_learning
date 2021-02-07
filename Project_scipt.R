## Project Scipt document 

library(ggplot2)
library(GGally)
library(caret)
library(rpart)
library(rattle)
library(randomForest)
library(dplyr)
library(gbm)

train_input <-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                       header = TRUE)
test_input <-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                      header = TRUE)
dim(train_input)
dim(test_input)

## Cleaning of data. 
train_data <- train_input[, colSums(is.na(train_input)) == 0] # to remove col with NAs
test_data <- test_input[, colSums(is.na(test_input)) == 0]
dim(train_data)
dim(test_data)
train_set <- train_data[,-c(1:7)]
valid_set <- test_data[,-c(1:7)]


## Partitioning train_set to TrainData and TestData for model creation
set.seed(1590)
intrain <- createDataPartition(y = train_set$classe,
                               p = 0.8, 
                               list = FALSE)
trainData <- train_set[intrain,]
testData <- train_set[-intrain,]
dim(trainData)
dim(testData)
# To remove variables which are non-necessary 
cols <- nearZeroVar(x = trainData)
trainData <- trainData[,-cols]
testData <- testData[,-cols]


## Random Forest Method
rf_mdl <- train(classe ~ ., 
                data = trainData,
                method = "rf")
rf_mdl$finalModel

rf_pred <- predict(object = rf_mdl, newdata = testData)
rf_cm <- confusionMatrix(rf_pred, as.factor(testData$classe))
rf_cm


## Generalized Boosing Method
set.seed(1111)
gbm_trctrl <- trainControl(method = "repeatedcv",
                           repeats = 1, 
                           number = 3)
gbm_mdl <- train(classe ~ .,
                 data = trainData,
                 method = "gbm", 
                 trControl = gbm_trctrl)
gbm_mdl$finalModel

gbm_pred <- predict(object = gbm_mdl, newdata = testData)
gbm_cm <- confusionMatrix(gbm_pred, as.factor(testData$classe))
gbm_cm

## Decision Tree formation 
set.seed(3333)
dt_mdl <- rpart(classe ~., data = trainData, method = "class")
fancyRpartPlot(dt_mdl)

dt_pred <- predict(dt_mdl, newdata = testData, type = "class")
dt_cm <- confusionMatrix(dt_pred, as.factor(testData$classe))
dt_cm

## 1- Resubstiution error value storege in sampling accuracy calculation
acc_cm <- data.frame(rf_cm$overall[1], gbm_cm$overall[1], dt_cm$overall[1])

## Validation of the training data set. 
rf_pred_valid <- predict(rf_mdl, newdata = valid_set)

gbm_pred_valid <- predict(gbm_mdl, newdata = valid_set)


res <- data.frame(1:dim(valid_set)[1], rf_pred_valid, gbm_pred_valid)