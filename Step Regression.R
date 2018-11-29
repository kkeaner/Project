library(FSA)
library(psych)
Data.num = select(Stateful,
           Time,
           Length,
           Source.Port,
           Destination.Port,           
           Window.size.value)
corr.test(Data.num, 
           use = "pairwise",
           method="pearson",
           adjust="none",     # Can adjust p-values; see ?p.adjust for options
           alpha=.05)
model.null = lm(Length ~ 1, 
                 data=Data.num)
model.full = lm(Length ~ Time + Source.Port + Destination.Port + Window.size.value, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
              data=Data.num)
the_model <- lm(Length ~ Source.Port + Time + Window.size.value + 
    Destination.Port, data = Data.num)
# the best model is picked to be all 4 variables having the lowest AIC.  Length is the only numeric variable 
# common to both Stateful and Stateless Data Frames so is why there is a need to move
# to logistic regression where binomial regression with categorical variables can be used
