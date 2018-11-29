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