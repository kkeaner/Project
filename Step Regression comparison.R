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
#
#Step:  AIC=6607768
#Length ~ Source.Port + Time + Window.size.value + Destination.Port
#
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
model.null = lm(Source.Port ~ 1, 
                 data=Data.num)
model.full = lm(Source.Port ~ Time + Length + Destination.Port + Window.size.value, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
data=Data.num)
#
#Step:  AIC=10710554
#Source.Port ~ Destination.Port + Length + Window.size.value + 
#    Time
#
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
model.null = lm(Destination.Port ~ 1, 
                 data=Data.num)
model.full = lm(Destination.Port ~ Time + Length + Source.Port + Window.size.value, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
data=Data.num)
#
#Step:  AIC=10724696
#Destination.Port ~ Source.Port + Window.size.value + Length + 
#    Time
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
model.null = lm(Time ~ 1, 
                 data=Data.num)
model.full = lm(Time ~ Destination.Port + Length + Source.Port + Window.size.value, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
data=Data.num)
#
#Step:  AIC=11260302
#Time ~ Length + Destination.Port + Window.size.value + Source.Port
#
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
model.null = lm(Window.size.value ~ 1, 
                 data=Data.num)
model.full = lm(Window.size.value ~ Destination.Port + Length + Source.Port + Time, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
data=Data.num)
#
#Step:  AIC=10219203
#Window.size.value ~ Source.Port + Length + Destination.Port + 
#    Time

