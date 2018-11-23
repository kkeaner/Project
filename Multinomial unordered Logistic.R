Mattrix_3$Type <- NA
Mattrix_3$Type[Mattrix_3$Source.1 == "30:8c:fb:2f:e4:b2"] <- "Drop Camera"
Mattrix_3$Type[Mattrix_3$Source.1 == "ec:1a:59:83:28:11"] <- "Wemo Motion Sensor"
Mattrix_3$Type[Mattrix_3$Source.1 == "f4:f2:6d:93:51:f1"] <- "TP Link camera"
Mattrix_3$Type[Mattrix_3$Source.1 == "00:16:6c:ab:6b:88"] <- "Samsung SmartCam"
Mattrix_3$Type[Mattrix_3$Source.1 == "ec:1a:59:79:f4:89"] <- "Belkin Wemo Switch"
Mattrix_3$Type[Mattrix_3$Source.1 == "44:65:0d:56:cc:d3"] <- "Amazon Echo"
Mattrix_3$state <- as.factor(Mattrix_3$state)
Mattrix_3$Type <- as.factor(Mattrix_3$Type)
Mattrix_3$Type <- relevel(Mattrix_3$Type, ref=1)
modely <- multinom(Type ~ Protocol, data = Mattrix_3)
summary(modely)$coefficients/summary(modely)$standard.errors