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