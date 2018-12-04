# Random Forest using Caret and Ranger libraries
library (dplyr)
library(caret)
library(ranger)
model <- train(state ~ Destination.1,
tuneLength = 1,
data = Mattrix_3, method = "ranger",
trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
#
#model
#Random Forest 
#
#314513 samples
#     1 predictor
#     2 classes: '0', '1' 
#
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 251610, 251610, 251610, 251611, 251611 
# Resampling results across tuning parameters:
#
#  splitrule   Accuracy   Kappa    
#  gini        0.8788285  0.4895354
#  extratrees  0.8787173  0.4889200
#
model_2 <- train(state ~ Length,
tuneLength = 1,
data = Mattrix_3, method = "ranger",
trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
#model_2
#Random Forest 
#
#314513 samples
#     1 predictor
#     2 classes: '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 251610, 251611, 251611, 251610, 251610 
#Resampling results across tuning parameters:
#
#  splitrule   Accuracy   Kappa    
#  gini        0.9875045  0.9597284
#  extratrees  0.9875109  0.9597485
#
model_2b <- train(state ~ Length + Protocol,
 tuneLength = 1,
 data = Mattrix_5, method = "ranger",
 trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
# model_2b
#Random Forest 
#
#308169 samples
#     2 predictor
#     2 classes: '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 246535, 246535, 246536, 246535, 246535 
#Resampling results across tuning parameters:
#
#  splitrule   Accuracy  Kappa
#  gini        1         1    
#  extratrees  1         1    
#
#
model_5 <- train(state ~ Protocol,
+   tuneLength = 1,
+   data = Mattrix_5a, method = "ranger",
+ trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
#model_5
#Random Forest 
#
#117782 samples
#     1 predictor
#     2 classes: '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 94226, 94225, 94226, 94226, 94225 
#Resampling results across tuning parameters:
#
#  splitrule   Accuracy  Kappa
#  gini        1         1    
#  extratrees  1         1    
#
#
model_11 <- train(state ~ Length + Source,
    tuneLength = 1,
    data = Mattrix_5a, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
# model_11
#Random Forest 
#
#117782 samples
#     2 predictor
#     2 classes: '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 94226, 94226, 94225, 94226, 94225 
#Resampling results across tuning parameters:
#
# splitrule   Accuracy   Kappa    
#  gini        0.8827750  0.5531123
#  extratrees  0.8463599  0.4633772

