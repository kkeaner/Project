Sept30b <- read.csv(file.choose( ))
library(dplyr)
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
nSept30b_go3a <- filter(nSept30b_go3, Source != "NA" )
nSept30b_go3b <- filter(nSept30b_go3, Source.Port != "NA" )
# step regression model to find the best combination and selection of variables using
# non categorical variables in this modelling
library(FSA)
library(psych)
# building of the data frame after dimension reduction and feature selection
Data.num = select(nSept30b_go3b,
           Time,
           Length,
           Source.Port,
           Destination.Port,           
           Window.size.value)
# pearson pairwise correlation which seems to point to Length having the highest 
# correlation with other variables. Length and Source port are most correlated
# may not be a useful test as no probabilities are returned
corr.test(Data.num, 
           use = "pairwise",
           method="pearson",
           adjust="none",     
           alpha=.05)
model.null = lm(Length ~ 1, 
                data=Data.num)
model.full = lm(Length ~ Time + Source.Port + Destination.Port + Window.size.value, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
              data=Data.num)
# Akaike information criterion (AIC) is an estimator of the relative quality of statistical models for a given set of data. 
# Given a collection of models for the data, AIC estimates the quality of each model, relative to each of the other models. 
# Thus, AIC provides a means for model selection.  AIC values point to the model below as being 
# the best arrangement of variables
the_model <- lm(Length ~ Destination.Port + Window.size.value + Time + Source.Port, data = Data.num)
summary(the_model)
# best model where we have found our best response variable for use with categorical variables (Length)
