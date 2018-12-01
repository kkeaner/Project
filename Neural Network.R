library(caret)
library(nnet)
library(RCurl)
library(Metrics)
set.seed(123)
iot <- Mattrix_5[sample(nrow(Mattrix_5)),]
split <- floor(nrow(iot)/2)
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
# Accuracy     Kappa 
# 0.5678395 0.4683052 
# just using length to predict the categorical variables dropped the accuracy
io <- multinom(Type~ Length,data = Mat_Train, maxit=500, trace = T)
topModels <- varImp(io)
topModels$Variables <- row.names(topModels)
topModels <- topModels[order(-topModels$Overall),]
head(topModels, 5)
pred1 <- predict(io, type = "probs", newdata = Mat_Test)
pred2 <- predict(io, type = "class", newdata= Mat_Test)
head(pred2, 5)
head(Mat_Test, 5)
head(pred1, 5)
postResample(Mat_Test$Type, pred2)
# Accuracy     Kappa 
# 0.3635249 0.1921299
i <- multinom(Type~ Length + Protocol + Source.1, data = Mat_Train, maxit=500, trace = T )
topModels <- varImp(i)
topModels$Variables <- row.names(topModels)
topModels <- topModels[order(-topModels$Overall),]
head(topModels, 5)
pred1 <- predict(i, type = "probs", newdata = Mat_Test)
pred2 <- predict(i, type = "class", newdata= Mat_Test)
head(pred2, 5)
head(Mat_Test, 5)
head(pred1, 5)
postResample(Mat_Test$Type, pred2)
# Accuracy     Kappa 
# 0.8142151 0.7702682 
it <- multinom(Type~ Length + Protocol + Source.1 + Time.to.live, data = Mat_Train, maxit=500, trace = T )
topModels <- varImp(it)
topModels$Variables <- row.names(topModels)
topModels <- topModels[order(-topModels$Overall),]
head(topModels, 5)
pred1 <- predict(it, type = "probs", newdata = Mat_Test)
pred2 <- predict(it, type = "class", newdata= Mat_Test)
head(pred2, 5)
head(Mat_Test, 5)
head(pred1, 5)
postResample(Mat_Test$Type, pred2)
# Accuracy     Kappa 
# 0.9922055 0.9900638
