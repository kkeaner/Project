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
the_model <- lm(Length ~ Destination.Port + Window.size.value + Time + Source.Port, data = Data.num)