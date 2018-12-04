library(nnet)
library (dplyr)
library(caret)
set.seed(123)
Mattrix_5 <- Mattrix_5[sample(nrow(Mattrix_5)),]
split <- floor(nrow(Mattrix_5)/2)
Mat_Train <- Mattrix_5[0:split,]
Mat_Test <- Mattrix_5[(split + 1):nrow(Mattrix_5),]
iots <- multinom(Type~ Length + Protocol,data = Mat_Train, maxit=500, trace = T )
topModels <- varImp(iots)
topModels$Variables <- row.names(topModels)
topModels <- topModels[order(-topModels$Overall),]
head(topModels, 5)
pred1 <- predict(iots, type = "probs", newdata = Mat_Test)
pred2 <- predict(iots, type = "class", newdata= Mat_Test)
head(pred2, 5)
head(Mat_Test, 5)
head(pred1, 5)
postResample(Mat_Test$Type, pred2)
#  Accuracy     Kappa 
# 0.7972223 0.7384605
# best accuracy without adding source to algorithm was the use of Protocol and Length at 80% accuracy
set.seed(123)
Mattrix_5 <- Mattrix_5[sample(nrow(Mattrix_5)),]
split <- floor(nrow(Mattrix_5)/2)
Mat_Train <- Mattrix_5[0:split,]
Mat_Test <- Mattrix_5[(split + 1):nrow(Mattrix_5),]
iots <- multinom(Type~ Length + Protocol + Time.to.live,data = Mat_Train, maxit=500, trace = T )
topModels <- varImp(iots)
topModels$Variables <- row.names(topModels)
topModels <- topModels[order(-topModels$Overall),]
head(topModels, 5)
pred1 <- predict(iots, type = "probs", newdata = Mat_Test)
pred2 <- predict(iots, type = "class", newdata= Mat_Test)
head(pred2, 5)
head(Mat_Test, 5)
head(pred1, 5)
postResample(Mat_Test$Type, pred2)
# Accuracy     Kappa 
# 0.7872733 0.7256075 
# addition of Time.to.live did not increase accuracy of prediction
set.seed(123)
Mattrix_5 <- Mattrix_5[sample(nrow(Mattrix_5)),]
split <- floor(nrow(Mattrix_5)/2)
Mat_Train <- Mattrix_5[0:split,]
Mat_Test <- Mattrix_5[(split + 1):nrow(Mattrix_5),]
iots <- multinom(Type ~ Length + Protocol + state, data = Mat_Train, maxit=500, trace = T )
topModels <- varImp(iots)
topModels$Variables <- row.names(topModels)
topModels <- topModels[order(-topModels$Overall),]
head(topModels, 5)
pred1 <- predict(iots, type = "probs", newdata = Mat_Test)
pred2 <- predict(iots, type = "class", newdata= Mat_Test)
head(pred2, 5)
head(Mat_Test, 5)
head(pred1, 5)
postResample(Mat_Test$Type, pred2)
# Accuracy     Kappa 
# 0.7838531 0.7214089 
# Accuracy does not increase with addition of contrived state variable
Mattrix_5a <- Mattrix_5[Mattrix_5$Time < 44000,]
Mattrix_5b <- Mattrix_5[Mattrix_5$Time > 44000,]
# In order to break down Mattrix into a small enough data frame for processing with computer we had to break 
# the day into 2 - 12 hour periods.....as was found using wireshark the data was collected int 12 hour shifts and the
# data was merged by the distributer to make a file (large) that reflected 24 hours worth of collection
set.seed(123)
Mattrix_5a <- Mattrix_5a[sample(nrow(Mattrix_5a)),]
split <- floor(nrow(Mattrix_5a)/2)
Mat_Train <- Mattrix_5a[0:split,]
Mat_Test <- Mattrix_5a[(split + 1):nrow(Mattrix_5a),]
iots <- multinom(Type ~ Length + Protocol + Source, data = Mat_Train, maxit=500, trace = T , MaxNWts = 3000)
topModels <- varImp(iots)
topModels$Variables <- row.names(topModels)
topModels <- topModels[order(-topModels$Overall),]
head(topModels, 5)
pred1 <- predict(iots, type = "probs", newdata = Mat_Test)
pred2 <- predict(iots, type = "class", newdata= Mat_Test)
head(pred2, 5)
head(Mat_Test, 5)
head(pred1, 5)
postResample(Mat_Test$Type, pred2)
# Accuracy    Kappa 
#       1        1 
# a half days of collected information gives perfect separation that implies overfitting
# as neural network analysis often does, however it is checked again with another half day
# with train and test data and the result is 99%      
set.seed(123)
Mattrix_5b <- Mattrix_5b[sample(nrow(Mattrix_5b)),]
split <- floor(nrow(Mattrix_5b)/2)
Mat_Train <- Mattrix_5b[0:split,]
Mat_Test <- Mattrix_5b[(split + 1):nrow(Mattrix_5b),]
iots <- multinom(Type ~ Length + Protocol + Source, data = Mat_Train, maxit=500, trace = T , MaxNWts = 3000)
topModels <- varImp(iots)
topModels$Variables <- row.names(topModels)
topModels <- topModels[order(-topModels$Overall),]
head(topModels, 5)
pred1 <- predict(iots, type = "probs", newdata = Mat_Test)
pred2 <- predict(iots, type = "class", newdata= Mat_Test)
head(pred2, 5)
head(Mat_Test, 5)
head(pred1, 5)
postResample(Mat_Test$Type, pred2)
postResample(Mat_Test$Type, pred2)
# Accuracy     Kappa 
# 0.9999790 0.9999722 
# Second half of the day - second 12 hours