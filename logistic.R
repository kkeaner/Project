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