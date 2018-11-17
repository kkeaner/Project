# Mattrix_2 is refererred to in the top 6 talkers script
rows <- sample(nrow(Mattrix_2))
Mattrix_2 <- Mattrix_2[rows, ]
split <- round(nrow(Mattrix_2) * .75)
train <- Mattrix_2[1:split, ]
test <- Mattrix_2[(split + 1):nrow(Mattrix_2), ]
glmtest <- glm(state~ Length + Time + Time.to.live, family = "binomial", Mattrix_2)
p <- predict(glmtest, test, type = "response")
p_class <- ifelse(p > .50, "1", "0")
table (p_class)
table(p_class, test[["state"]])

# p_class     0     1
#      0  5604     0
#      1  9486 63538

# confusionMatrix(p_class, test [["state"]])
# Error: `data` and `reference` should be factors with the same levels.
# in this study we are trying to use columns with NA's to distinguish between 
# UDP and TCP......will break down further and try using confusion matrix again
# removal of rows that contain NA's....UPD protocol packets
Mattrix_2 <- subset(nSept30b_go3, Source %in% c("192.168.1.249", "192.168.1.106", "192.168.1.223", "192.168.1.193", "192.168.1.143" ,"192.168.1.240") )
Mattrix_3 <- subset(Mattrix_2, select = -c(Source.Port, Destination.Port, Window.size.value))
rowss <- sample(nrow(Mattrix_3))
Mattrix_3 <- Mattrix_3[rowss, ]
split <- round(nrow(Mattrix_3) * .75)
train <- Mattrix_3[1:split, ]
test <- Mattrix_3[(split + 1):nrow(Mattrix_3), ]
glmtest_b <- glm(state~ Length + Time + Time.to.live, family = "binomial", Mattrix_3)
pred <- predict(glmtest_b, test, type = "response")
p_class <- ifelse(pred > .50, "1", "0")
table (p_class)
# p_class
#    0     1 
# 5604 73024 
# ??
confusionMatrix(p_class, test [["state"]])
# Error in confusionMatrix(p_class, test[["state"]]) : 
# could not find function "confusionMatrix"
sum(is.na(Mattrix_3))
# [1] 0
# The error is not caused by NA's in the data set
rows <- sample(nrow(Mattrix_3))
Mattrix_3 <- Mattrix_3[rows, ]
split <- round(nrow(Mattrix_3) * .75)
train <- Mattrix_3[1:split, ]
test <- Mattrix_3[(split + 1):nrow(Mattrix_3), ]
glmtest_b <- glm(state~ Length + Time + Time.to.live, family = "binomial", Mattrix_3)
pred <- predict(glmtest_b, test, type = "response")
p_class <- ifelse(pred > .50, "1", "0")
table (p_class)
p_class
#   0     1 
# 5582 73046 
table(p_class, test[["state"]])
# p_class     0     1
#       0  5582     0
#       1  9425 63621

confusionMatrix(table(p_class, test [["state"]]))
#Confusion Matrix and Statistics
#
#      
# p_class     0     1
#     0  5604     0
#      1  9486 63538
#                                         
#              Accuracy : 0.8794          
#                95% CI : (0.8771, 0.8816)
#   No Information Rate : 0.8081          
#   P-Value [Acc > NIR] : < 2.2e-16       
#                                         
#                 Kappa : 0.4884          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                         
#           Sensitivity : 0.37137         
#           Specificity : 1.00000         
#       Pos Pred Value : 1.00000         
#        Neg Pred Value : 0.87010         
#            Prevalence : 0.19192         
#        Detection Rate : 0.07127         
#  Detection Prevalence : 0.07127         
#     Balanced Accuracy : 0.68569         
#                                         
#      'Positive' Class : 0     
# summary(glmtest_b)
# Call:
# glm(formula = state ~ Length + Time + Time.to.live, family = "binomial", 
#    data = Mattrix_3)
#
# Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.4740   0.2982   0.5489   0.5706   0.6023  
#
#Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -1.756e+01  1.137e+02  -0.154    0.877    
#Length             9.911e-04  1.757e-05  56.408  < 2e-16 ***
#Time              -1.184e-06  2.463e-07  -4.808 1.52e-06 ***
#Time.to.live2     -4.270e-01  1.171e+02  -0.004    0.997    
#Time.to.live4     -1.116e-01  1.581e+02  -0.001    0.999    
#Time.to.live5     -1.006e-02  5.502e+02   0.000    1.000    
#Time.to.live64     1.923e+01  1.137e+02   0.169    0.866    
#Time.to.live64,46 -1.141e-01  3.958e+03   0.000    1.000    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 308734  on 314512  degrees of freedom
#Residual deviance: 221517  on 314505  degrees of freedom
#AIC: 221533

#Number of Fisher Scoring iterations: 16

glmnew <- glm(state~ Source.1, family = "binomial", Mattrix_3)
pred <- predict(glmnew, test, type = "response")
p_class <- ifelse(pred > .50, "1", "0")
table (p_class)
confusionMatrix(table(p_class, test [["state"]]))
#Confusion Matrix and Statistics
#
#       
#p_class     0     1
#     0  9679  7260
#     1  5411 56278
#                                          
#               Accuracy : 0.8388          
#                 95% CI : (0.8363, 0.8414)
#    No Information Rate : 0.8081          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.5036          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                          
#            Sensitivity : 0.6414          
#           Specificity : 0.8857          
#         Pos Pred Value : 0.5714          
#         Neg Pred Value : 0.9123          
#             Prevalence : 0.1919          
#         Detection Rate : 0.1231          
#   Detection Prevalence : 0.2154          
#      Balanced Accuracy : 0.7636          
#                                          
#       'Positive' Class : 0   
#
glmnewer <- glm(state~ Destination.1, family = "binomial", Mattrix_3)
pred <- predict(glmnewer, test, type = "response")
p_class <- ifelse(pred > .50, "1", "0")
table (p_class)
#p_class
#    0     1 
# 5593 73035 
confusionMatrix(table(p_class, test [["state"]]))
#Confusion Matrix and Statistics
#
#p_class     0     1
#      0  5593     0
#     1  9497 63538
#                                          
#               Accuracy : 0.8792          
#                 95% CI : (0.8769, 0.8815)
#    No Information Rate : 0.8081          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.4877          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                          
#            Sensitivity : 0.37064         
#            Specificity : 1.00000         
#         Pos Pred Value : 1.00000         
#         Neg Pred Value : 0.86997         
#             Prevalence : 0.19192         
#         Detection Rate : 0.07113         
#   Detection Prevalence : 0.07113         
#      Balanced Accuracy : 0.68532         
#                                          
#      'Positive' Class : 0  