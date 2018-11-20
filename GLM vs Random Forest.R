Sept30b <- read.csv(file.choose( ))
library (dplyr)
library(caret)
library(ranger)
alpa_Sept30b <- Sept30b %>% filter(grepl("[a-z]", Source))
num_Sept30b <- Sept30b %>% filter(!grepl("[a-z]", Source))
Stateless<- subset(num_Sept30b, is.na(Source.Port))
Stateful<- subset(num_Sept30b, !is.na(Source.Port))
Stateful$state <- 1
Stateless$state <- 0
N_Sept30b <<- rbind(Stateful, Stateless)
nSept30b_go1 <- filter(N_Sept30b, Source != "::" )
nSept30b_go2 <- filter(nSept30b_go1, Source != "0.0.0.0" )
nSept30b_go3 <- filter(nSept30b_go2, Source != "8.8.8.8" )
Mattrix_2 <- subset(nSept30b_go3, Source %in% c("192.168.1.249", "192.168.1.106", "192.168.1.223", "192.168.1.193", "192.168.1.143" ,"192.168.1.240") )
Mattrix_3 <- subset(Mattrix_2, select = -c(Source.Port, Destination.Port, Window.size.value))

glmtest_c <- glm(state~ Length + Source, family = "binomial", Mattrix_3)
pred <- predict(glmtest_c, test, type = "response")
p_class <- ifelse(pred > .50, "1", "0")
table (p_class)
p_class
table(p_class, test[["state"]])
confusionMatrix(table(p_class, test [["state"]]))
# accuracy of approx. 85%

glmtest_d <- glm(state ~ Length + Destination.1, family = "binomial", Mattrix_3)
pred <- predict(glmtest_d, test, type = "response")
p_class <- ifelse(pred > .50, "1", "0")
table (p_class)
p_class
table(p_class, test[["state"]])
confusionMatrix(table(p_class, test [["state"]]))
# accuracy of approx. 88%

glmtest_f <- glm(state ~ Protocol, family = "binomial", Mattrix_3,maxit = 100)
summary(glmtest_f)

#Call:
#glm(formula = state ~ Protocol, family = "binomial", data = Mattrix_3, 
#    maxit = 100)
#
#Deviance Residuals: 
#       Min          1Q      Median          3Q         Max  
#-2.107e-08   2.107e-08   2.107e-08   2.107e-08   2.107e-08  
#
#Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)
#(Intercept)           3.157e+01  1.794e+07       0        1
#ProtocolCLASSIC-STUN -6.313e+01  1.799e+07       0        1
#ProtocolDHCP         -6.313e+01  2.456e+07       0        1
#ProtocolDIAMETER     -7.694e-05  3.496e+07       0        1
#ProtocolDISTCC       -7.606e-05  2.257e+07       0        1
#ProtocolDNS          -6.313e+01  1.798e+07       0        1
#ProtocolGearman      -7.592e-05  4.270e+07       0        1
#ProtocolHTTP         -1.004e-07  1.796e+07       0        1
#ProtocolHTTP/XML      2.362e-07  1.797e+07       0        1
#ProtocolICMP         -6.313e+01  1.794e+07       0        1
#ProtocolIGMPv2       -6.313e+01  1.805e+07       0        1
#ProtocolIGMPv3       -6.313e+01  1.919e+07       0        1
#ProtocolIPA          -7.566e-05  2.140e+07       0        1
#ProtocolMIH          -7.565e-05  2.422e+07       0        1
#ProtocolNTP          -6.313e+01  1.801e+07       0        1
#ProtocolOmni-Path    -7.674e-05  1.854e+07       0        1
#ProtocolRSIP         -2.312e-07  2.240e+07       0        1
#ProtocolSABP         -2.639e-06  2.294e+07       0        1
#ProtocolSSDP         -6.313e+01  1.794e+07       0        1
#ProtocolSTUN         -2.505e-06  1.821e+07       0        1
#ProtocolTCP          -2.860e-06  1.794e+07       0        1
#ProtocolTCPCL         6.973e-07  3.107e+07       0        1
#ProtocolTFP over TCP  2.569e-07  3.805e+07       0        1
#ProtocolTLSv1        -2.524e-06  1.794e+07       0        1
#ProtocolTLSv1.2      -7.755e-05  1.796e+07       0        1
#ProtocolUDP          -6.313e+01  1.795e+07       0        1
#ProtocolUDT          -6.313e+01  1.795e+07       0        1
#ProtocolWOW          -3.928e-22  2.640e+07       0        1
#ProtocolXMPP/XML     -3.946e-22  2.077e+07       0        1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#
#    Null deviance: 3.0873e+05  on 314512  degrees of freedom
#Residual deviance: 1.3967e-10  on 314484  degrees of freedom
#AIC: 58
#
#Number of Fisher Scoring iterations: 30
#pred <- predict(glmtest_f, test, type = "response")
#p_class <- ifelse(pred > .50, "1", "0")
#table (p_class)
#p_class
#    0     1 
#15090 63538 
# table(p_class, test[["state"]])
#       
#p_class     0     1
#      0 15090     0
#      1     0 63538
#confusionMatrix(table(p_class, test [["state"]]))
#Confusion Matrix and Statistics
#
#p_class     0     1
#      0 15090     0
#      1     0 63538
#                                   
#               Accuracy : 1        
#                 95% CI : (1, 1)   
#    No Information Rate : 0.8081   
#    P-Value [Acc > NIR] : < 2.2e-16
#                                   
#                  Kappa : 1        
# Mcnemar's Test P-Value : NA       
#                                   
#            Sensitivity : 1.0000   
#            Specificity : 1.0000   
#         Pos Pred Value : 1.0000   
#         Neg Pred Value : 1.0000   
#             Prevalence : 0.1919   
#         Detection Rate : 0.1919   
#   Detection Prevalence : 0.1919   
#      Balanced Accuracy : 1.0000   
#                                   
#       'Positive' Class : 0     
#
# this is an even more unusual result than for Length but maybe is the best
# Predictor of UDP vs TCP and allow discovery of IOT devices based strictly on protocol

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
#Tuning parameter 'mtry' was held constant at a value of 17

#Tuning parameter 'min.node.size' was held constant at a value of 1
#Accuracy was used to select the optimal model using the
# largest value.
#The final values used for the model were mtry = 17, splitrule
# = gini and min.node.size = 1.

model_2 <- train(state ~ Length,
tuneLength = 1,
data = Mattrix_3, method = "ranger",
trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
#
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
#Tuning parameter 'mtry' was held constant at a value of 1
#
#Tuning parameter 'min.node.size' was held constant at a
# value of 1
#Accuracy was used to select the optimal model using the
# largest value.
#The final values used for the model were mtry = 1, splitrule
# = extratrees and min.node.size = 1.
# excellent results for this model using (protocol) length as an explanatory variable

model_3 <- train(state ~ Length + Destination.1,
 tuneLength = 1,
 data = Mattrix_3, method = "ranger",
 trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
                                          
#model_3
#Random Forest 
#
#314513 samples
#     2 predictor
#     2 classes: '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 251610, 251611, 251610, 251611, 251610 
#Resampling results across tuning parameters:
#
#  splitrule   Accuracy   Kappa    
#  gini        0.8788858  0.4898421
#  extratrees  0.8788858  0.4898421
#
#Tuning parameter 'mtry' was held constant at
# a value of 17
#Tuning parameter
# 'min.node.size' was held constant at a value of 1
#Accuracy was used to select the optimal
# model using the largest value.
#The final values used for the model were mtry
# = 17, splitrule = gini and min.node.size = 1.

model_3 <- train(state ~ Length + Destination.1,
 tuneLength = 1,
 data = Mattrix_3, method = "ranger",
 trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))

#model_3
#Random Forest 
#
#314513 samples
#     2 predictor
#     2 classes: '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 251610, 251611, 251610, 251611, 251610 
#Resampling results across tuning parameters:
#
#  splitrule   Accuracy   Kappa    
#  gini        0.8788858  0.4898421
#  extratrees  0.8788858  0.4898421
#
#Tuning parameter 'mtry' was held constant at
# a value of 17
#Tuning parameter
# 'min.node.size' was held constant at a value of 1
#Accuracy was used to select the optimal
# model using the largest value.
#The final values used for the model were mtry
# = 17, splitrule = gini and min.node.size = 1.

model_5 <- train(state ~ Protocol,
  tuneLength = 1,
  data = Mattrix_3, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))

# Warning messages:
#1: model fit failed for Fold4: mtry=14, min.node.size=1, splitrule=gini Error in ranger::ranger(dependent.variable.name = ".outcome", data = x,  : 
#  User interrupt or internal error.
# 
#2: In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
#  There were missing values in resampled performance measures.
#
#model_5
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
#  splitrule   Accuracy  Kappa
#  gini        1         1    
#  extratrees  1         1    

#Tuning parameter 'mtry' was held constant at a value of 14
#Tuning
# parameter 'min.node.size' was held constant at a value of 1
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were mtry = 14, splitrule = gini
# and min.node.size = 1.

# this result is surprising and indicates a perfect? model.  Further investigation
# is required - there were error messages but the glm does support this output




