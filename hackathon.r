#----------------------------------------------------
#Purpose: Predict Seattle Street Parking Availability
#using publicly available data set from Seattle Date Portal
#-----------------------------------------------------

setwd("~/R/Hackathon")
library(tcltk)
library(ggplot2)
library(scales)
library(bnlearn)
library(gtools)
library(glmnet)
library(data.table)
library(xgboost)
library(caret)
library(readr)
library(dplyr)
library(tidyr)
library(caTools)

#read local data set
df<- read.table("data.txt", header = TRUE, sep="\t", quote = "", stringsAsFactors=FALSE)
str(df)

#define features for modeling
features <- c("TotalParkingSpaces", "Long",  "Lat", "HourlyRate" ,"AirTemperature", "Time", "Weekday")

#select features and remove missing data
data <- data[,colnames(data) %in% c( "AvailableSpaces", features)]
data <- na.omit(data)
data$Weekday <- as.factor(data$Weekday)
data$Time <- as.factor(data$Time)

#Split into train and test 
sample <- sample.split(data$AvailableSpaces, SplitRatio = 0.70)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)
cat(nrow(train), "\n")
cat(nrow(test), "\n")
table(train$AvailableSpaces)
table(test$AvailableSpaces)

#create demo dataset
demo1 <- df[df$ParkingStationId  %in% c(11134,11133,75173) & df$DateTime == "4/6/2016 10:00",]
demo1$location <- "Space Needle"
demo2 <- df[df$ParkingStationId  %in% c(36674,59374,18166) & df$Time == 8,]
demo2$location <- "CenturyLink Seattle"
demo3 <- df[df$ParkingStationId  %in% c(1017,1018,68914) & df$Time == 8,]
demo3$location <- "Pike Market Seattle"
demo4 <- df[df$ParkingStationId  %in% c(77985,77986,53549) & df$Time == 20,]
demo4$location <- "Facebook Seattle"
demo <- unique(rbind(demo1,demo2,demo3,demo4))

demo$Weekday <- as.factor(demo$Weekday)
demo$Time <- as.factor(demo$Time)
demo <- demo[,colnames(demo) %in% c( "AvailableSpaces", features)]
demo <- na.omit(demo)
str(demo)


#all data matrix for cross validation
sparse_matrix.data <- sparse.model.matrix(AvailableSpaces~.-1, data = data)
data.label = df$AvailableSpaces

#train matrix
sparse_matrix.train <- sparse.model.matrix(AvailableSpaces~.-1, data = train)
train.label <- train$AvailableSpaces

#test matrix
sparse_matrix.test <- sparse.model.matrix(AvailableSpaces~.-1, data = test)
test.label <- test$AvailableSpaces

#demo matrix
sparse_matrix.demo <- sparse.model.matrix(AvailableSpaces~.-1, data = demo)
demo.label = demo$AvailableSpaces

#xgboost model train
bst <-xgboost(data = sparse_matrix.train, label = train.label,  objective = "reg:linear", eval_metric="rmse",
              nrounds = 1385, max_depth = 6, eta = 0.6, gamma = 0)
#, colsample_bytree = 9 , min_child_weight = 4, subsample = 0.75 )

saveRDS(bst,  "parking.xgboost.Model.RDS")

#predict test
test.bst <- test
test.bst$prediction <- predict(bst, sparse_matrix.test)
test.bst$prediction

#predict demo
demo.bst <- demo
demo.bst$prediction <- predict(bst, sparse_matrix.demo)
demo.bst$prediction


#===============================
#Cross validation  
#===============================
#Fix eta = 0.1, use xgboost cross validation to find nrounds, use default for all other parameters
xgb_params = list(
  objective = "reg:linear",
  eta = 0.1,                      # learning rate                                                           
  eval_metric = "rmse"             # evaluation/loss metric
)

xgb_cv = xgb.cv(params = xgb_params,
                data = sparse_matrix.data,
                label =  data.label,
                nfold = 10,           
                nrounds = 1385,  
                prediction = TRUE,    # return the prediction using the final model 
                showsd = TRUE,        # standard deviation of loss across folds
                #stratified = TRUE,   # sample is unbalanced; use stratified sampling
                verbose = TRUE,
                print_every_n = 1, 
                early_stopping_rounds = 10)

#======================================
#Perform grid search using caret package  
#======================================
ControlParamteres <- trainControl(method = "cv",
                                  number = 10,
                                  #verboseIter = TRUE,
                                  savePredictions = TRUE,
                                  classProbs = TRUE,
                                  allowParallel = TRUE
)

parametersGrid <-  expand.grid(
  nrounds=664,
  max_depth=c(3,6), 
  eta = c(0.2, 0.1, 0.05, 0.01, 0.001),
  gamma=0,
  colsample_bytree = c(0.6, 0.7, 0.8, 0,9),
  min_child_weight = c(1, 2, 3, 4),
  subsample = c(0.5, 0.75, 1)
)

modelxgboost <- train(AvailableSpaces~., 
                      data = sparse_matrix.data,
                      method = "xgbTree",
                      trControl = ControlParamteres,
                      tuneGrid=parametersGrid)
