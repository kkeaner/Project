Sept30b <- read.csv(file.choose( ))
library (dplyr)
library(caret)
library(ggplot2)
library(mosaic)
# data description
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
Mattrix_3$Type <- NA
Mattrix_3$Type[Mattrix_3$Source.1 == "30:8c:fb:2f:e4:b2"] <- "Drop Camera"
Mattrix_3$Type[Mattrix_3$Source.1 == "ec:1a:59:83:28:11"] <- "Wemo Motion Sensor"
Mattrix_3$Type[Mattrix_3$Source.1 == "f4:f2:6d:93:51:f1"] <- "TP Link camera"
Mattrix_3$Type[Mattrix_3$Source.1 == "00:16:6c:ab:6b:88"] <- "Samsung SmartCam"
Mattrix_3$Type[Mattrix_3$Source.1 == "ec:1a:59:79:f4:89"] <- "Belkin Wemo Switch"
Mattrix_3$Type[Mattrix_3$Source.1 == "44:65:0d:56:cc:d3"] <- "Amazon Echo"  44:65:0d:56:cc:d3
Mattrix_3$state <- as.factor(Mattrix_3$state)
Mattrix_3$Type <- as.factor(Mattrix_3$Type)
Mattrix_4 <- subset(Mattrix_3, Protocol %in% c("CLASSIC-STUN", "DNS", "HTTP", "HTTP/XML", "ICMP" ,"Omni-Path" ,"SSDP","STUN","TCP","TLSv1","TLSv1.2","UDP","UDT") )
Mattrix_5 = subset(Mattrix_4, select = -c(No.,Info ))
# linear regressions and analysis
multi <- lm(Length ~ Source + Protocol + Time.to.live, data = Mattrix_5)
vif(multi)
hulti <- lm(Length ~ Type + Protocol + Time.to.live, data = Mattrix_5)
vif(hulti)
gulti <- lm(Length ~ Source.1 + Protocol + Time.to.live, data = Mattrix_5)
vif(gulti)
first <- lm(Length ~ Protocol, data = Mattrix_5)
summary(first)
second <- lm(Length ~ Type, data = Mattrix_5)
summary(second)
third <- lm(Length ~ Time.to.live, data = Mattrix_5)
summary(third)
anova(first)
anova(second)
anova(third)
# mulitple linear regression based on Multi-collinerity
first_multi <- lm(Length ~ Protocol + Source + Time.to.live, data = Mattrix_5)
summary(first_multi)
kruskal.test(x = Mattrix_5$Length, g = as.factor(Mattrix_5$Source))
aggregate(Length~Type, data = Mattrix_5,FUN=mean, na.rm=T)